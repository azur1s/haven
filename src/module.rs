//! Module loading, name mangling and merging.
//!
//! This stage sits between parsing and typechecking. Given an entry file it:
//!
//!   1. transitively loads every imported module (an `import std/...` resolves
//!      to an embedded standard-library source; any other path resolves to an
//!      `.ixc` file relative to the *importing* file's directory);
//!   2. gives each module a unique mangling prefix and renames its top-level
//!      definitions (`foo` in module `m1` becomes `m1_...$foo`), leaving `extern`
//!      link names, `@export`ed items and the entry `main` untouched;
//!   3. rewrites every reference (call targets, struct literals, struct types)
//!      according to that module's imports;
//!   4. concatenates all modules into one flat program with no imports left.
//!
//! Everything downstream (typecheck, mono, mil, ...) then sees a single flat
//! namespace exactly as it did before modules existed. The lifetime story is
//! kept trivial by allocating every module's source, token stream and freshly
//! minted (mangled/prefixed) names into a single `bumpalo` arena owned by the
//! caller; the returned AST borrows from that arena for `'a`.
//!
//! ## Import forms
//!
//! * `import std/math`            — whole module, symbols visible *unqualified*.
//! * `import std/math { sinf }`   — selective, visible only as `math::sinf`.
//!
//! The prelude is loaded as an implicit whole-module import of every user module.
//!
//! ## Known v1 limitations
//!
//! * Only call targets, struct literals and struct *types* are rewritten. A
//!   top-level function used as a first-class value (not directly called) is not
//!   rewritten across modules, matching the monomorphizer's own limitation.
//! * There is no `pub`; every top-level item is public.
//! * Types have no qualified spelling, so a *selectively* imported struct can't
//!   be named (import the whole module to use its structs).

use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};

use bumpalo::Bump;

use crate::ast::*;
use crate::parse;

/// Source of an embedded standard-library module, keyed by its `std/...` path.
fn std_source(key: &str) -> Option<&'static str> {
    match key {
        "std/math" => Some(include_str!("../crt/std/math.ixc")),
        _ => None,
    }
}

fn line_col(src: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    let clamped = offset.min(src.len());
    for (i, ch) in src.char_indices() {
        if i >= clamped { break; }
        if ch == '\n' { line += 1; col = 1; } else { col += 1; }
    }
    (line, col)
}

/// A fully loaded module: its parsed contents plus the bookkeeping the resolver
/// needs to mangle and rewrite it.
struct Module<'a> {
    /// Canonical key (an absolute path, or `std/...`, or `<prelude>`), used to
    /// de-duplicate modules reached by more than one import.
    key: String,
    /// Human-facing name used in diagnostics.
    src: &'a str,
    /// Mangling prefix, unique per module, e.g. `m2_math`.
    prefix: String,
    is_entry: bool,
    imports: Vec<Import<'a>>,
    /// Canonical key of each import (parallel to `imports`), or `None` if that
    /// import failed to resolve (an error was already recorded).
    import_keys: Vec<Option<String>>,
    items: Vec<TopLevel<'a>>,
}

/// The final (post-mangling) name a module exposes for one of its symbols, split
/// by namespace so call sites and type positions look in the right place.
#[derive(Default)]
struct SymTab<'a> {
    /// Callable names: functions and externs. `sym -> final name`.
    fns: HashMap<&'a str, &'a str>,
    /// Struct type names. `sym -> final name`.
    structs: HashMap<&'a str, &'a str>,
}

fn is_export(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|a| a.value.name == "export")
}

/// The name a top-level function/struct is emitted under: mangled with the
/// module prefix, unless it must keep a stable spelling (`extern` link names and
/// `@export`ed items keep theirs; the entry `main` stays `main`).
fn final_fn_name<'a>(m: &Module<'_>, name: &str, attrs: &[Attribute], is_extern: bool, arena: &'a Bump) -> &'a str {
    // The entry module is unique, so its names can never clash with the
    // prefixed imported modules; leaving it unmangled keeps diagnostics for
    // ordinary single-file programs exactly as they were before modules.
    if is_extern || is_export(attrs) || m.is_entry {
        arena.alloc_str(name)
    } else {
        arena.alloc_str(&format!("{}${}", m.prefix, name))
    }
}

fn final_struct_name<'a>(m: &Module<'_>, name: &str, attrs: &[Attribute], arena: &'a Bump) -> &'a str {
    if is_export(attrs) || m.is_entry {
        arena.alloc_str(name)
    } else {
        arena.alloc_str(&format!("{}${}", m.prefix, name))
    }
}

/// Resolve an import to its canonical key (no file read for the `std` case;
/// canonicalises the path for the relative case, which requires the file to
/// exist). `dir` is the importing module's directory.
fn resolve_key(imp: &Import, dir: Option<&Path>) -> Result<String, String> {
    if imp.path.first() == Some(&"std") {
        let key = imp.path.join("/");
        if std_source(&key).is_none() {
            return Err(format!("unknown std module '{}'", key));
        }
        Ok(key)
    } else {
        let dir = dir.ok_or_else(||
            "cannot resolve a relative import from this module (std/prelude modules may only import `std/...`)".to_string())?;
        let mut p = dir.to_path_buf();
        for seg in &imp.path { p.push(seg); }
        p.set_extension("ixc");
        let canon = std::fs::canonicalize(&p)
            .map_err(|_| format!("cannot find module file '{}'", p.display()))?;
        Ok(canon.to_string_lossy().into_owned())
    }
}

/// Load an import's source and the directory its own relative imports resolve
/// against. Assumes `resolve_key` already succeeded for the same import.
fn load_import<'a>(imp: &Import, key: &str, dir: Option<&Path>, arena: &'a Bump) -> Result<(&'a str, Option<PathBuf>), String> {
    if imp.path.first() == Some(&"std") {
        Ok((std_source(key).expect("std source vanished after resolve_key"), None))
    } else {
        let _ = dir; // key is already the canonical absolute path
        let canon = PathBuf::from(key);
        let src = std::fs::read_to_string(&canon)
            .map_err(|e| format!("cannot read module file '{}': {}", canon.display(), e))?;
        Ok((arena.alloc_str(&src), canon.parent().map(|d| d.to_path_buf())))
    }
}

/// Lex + parse one module's source into `(imports, items)`, printing any
/// lex/parse diagnostics. The token stream is moved into `arena` so the parsed
/// AST can borrow from it for `'a`.
fn parse_module<'a>(key: &'a str, src: &'a str, arena: &'a Bump)
    -> Result<(Vec<Import<'a>>, Vec<TopLevel<'a>>), ()>
{
    let (tokens, lex_errs) = parse::lex(key, src);
    for e in &lex_errs {
        let (l, c) = line_col(src, e.span().start);
        eprintln!("Lex error in {}:{}:{}: {}", key, l, c, e.reason());
    }
    let tokens = match tokens {
        Some(t) if lex_errs.is_empty() => t,
        _ => return Err(()),
    };
    let tokens: &'a [Metadata<Token<'a>>] = arena.alloc_slice_fill_iter(tokens);

    let (parsed, parse_errs) = parse::parse(key.to_string(), src.len(), tokens);
    for e in &parse_errs {
        let (l, c) = line_col(src, e.span().start);
        eprintln!("Parse error in {}:{}:{}: {}", key, l, c, e.reason());
    }
    match parsed {
        Some(pi) if parse_errs.is_empty() => Ok(pi),
        _ => Err(()),
    }
}

/// Name-resolution scopes for a single module.
#[derive(Default)]
struct Scopes<'a> {
    /// Unqualified callable names in scope -> final emitted name.
    calls: HashMap<&'a str, &'a str>,
    /// Unqualified struct type names in scope -> final emitted name.
    types: HashMap<&'a str, &'a str>,
    /// `qualifier -> (symbol -> final name)` for selective imports.
    quals: HashMap<&'a str, HashMap<&'a str, &'a str>>,
}

/// Carries the per-module scopes and error sink while rewriting one module's
/// AST in place.
struct Rewriter<'x, 'a> {
    scopes: &'x Scopes<'a>,
    key: &'x str,
    src: &'x str,
    errs: &'x mut Vec<String>,
}

impl<'x, 'a> Rewriter<'x, 'a> {
    fn error(&mut self, span: &Span, msg: String) {
        let (l, c) = line_col(self.src, span.start);
        self.errs.push(format!("Import error in {}:{}:{}: {}", self.key, l, c, msg));
    }

    /// Rewrite struct type names inside `ty`, skipping names bound as generic
    /// parameters of the enclosing function (those are type params, not structs).
    fn ty(&mut self, ty: &mut Type<'a>, gparams: &HashSet<&str>) {
        match ty {
            Type::Struct(n) => {
                let nm: &str = *n;
                if !gparams.contains(nm) {
                    if let Some(&f) = self.scopes.types.get(nm) {
                        *n = f;
                    }
                }
            }
            Type::Pointer(inner)
            | Type::Array(inner, _)
            | Type::Slice(inner)
            | Type::Simd(inner, _) => self.ty(inner, gparams),
            Type::Function { params, return_type } => {
                for p in params { self.ty(p, gparams); }
                self.ty(return_type, gparams);
            }
            _ => {}
        }
    }

    /// Resolve a name appearing in call position, mutating it to its final
    /// emitted name. Bare names not in scope are left untouched (they are locals,
    /// params, externs resolved globally, intrinsics, or errors caught later).
    fn call_name(&mut self, name: &mut &'a str, span: &Span) {
        let full: &str = *name;
        if let Some((qual, sym)) = full.split_once("::") {
            match self.scopes.quals.get(qual) {
                Some(map) => match map.get(sym) {
                    Some(&f) => *name = f,
                    None => self.error(span, format!(
                        "'{}' is not imported from module qualifier '{}'", sym, qual)),
                },
                None => self.error(span, format!(
                    "unknown module qualifier '{}' (did you `import .../{}`?)", qual, qual)),
            }
        } else if let Some(&f) = self.scopes.calls.get(full) {
            *name = f;
        }
    }

    fn expr(&mut self, e: &mut Expr<'a>, gparams: &HashSet<&str>) {
        match &mut e.value {
            ExprNode::Call { func, type_args, args } => {
                if let ExprNode::Var(name) = &mut func.value {
                    self.call_name(name, &func.span);
                } else {
                    self.expr(func, gparams);
                }
                for ga in type_args {
                    if let GenericArg::Type(t) = ga { self.ty(t, gparams); }
                }
                for a in args { self.expr(a, gparams); }
            }
            ExprNode::Struct { name, fields } => {
                if let Some(&f) = self.scopes.types.get(*name) { *name = f; }
                for (_, fe) in fields { self.expr(fe, gparams); }
            }
            ExprNode::Access { base, .. } => self.expr(base, gparams),
            ExprNode::Index { slice, index } => {
                self.expr(slice, gparams);
                self.expr(index, gparams);
            }
            ExprNode::Unary { operand, .. } => self.expr(operand, gparams),
            ExprNode::Binary { left, right, .. } => {
                self.expr(left, gparams);
                self.expr(right, gparams);
            }
            ExprNode::Slice(elems) => for el in elems { self.expr(el, gparams); },
            // leaves and bare (non-call) vars: nothing to rewrite
            _ => {}
        }
    }

    fn stmt(&mut self, s: &mut Stmt<'a>, gparams: &HashSet<&str>) {
        match &mut s.value {
            StmtNode::Expr(e) => self.expr(e, gparams),
            StmtNode::Block(ss) => for s in ss { self.stmt(s, gparams); },
            StmtNode::Declare { ty, value, .. } => {
                self.ty(ty, gparams);
                self.expr(value, gparams);
            }
            StmtNode::Assign { left, value } => {
                self.expr(left, gparams);
                self.expr(value, gparams);
            }
            StmtNode::If { condition, then_branch, else_branch } => {
                self.expr(condition, gparams);
                self.stmt(then_branch, gparams);
                if let Some(eb) = else_branch { self.stmt(eb, gparams); }
            }
            StmtNode::While { condition, body } => {
                self.expr(condition, gparams);
                self.stmt(body, gparams);
            }
            StmtNode::Return(e) => self.expr(e, gparams),
            StmtNode::Continue | StmtNode::Break => {}
        }
    }

    fn toplevel(&mut self, tl: &mut TopLevel<'a>) {
        match &mut tl.value {
            TopLevelNode::Function { name, generics, params, return_type, body, .. } => {
                let gparams: HashSet<&str> = generics.iter().filter_map(|g| match g {
                    GenericParam::Type(n) => Some(*n),
                    GenericParam::Const(..) => None,
                }).collect();
                *name = self.scopes.calls.get(*name).copied().unwrap_or(*name);
                for (_, ty) in params { self.ty(ty, &gparams); }
                self.ty(return_type, &gparams);
                for s in body { self.stmt(s, &gparams); }
            }
            TopLevelNode::Extern { name, generics, params, return_type, .. } => {
                let gparams: HashSet<&str> = generics.iter().filter_map(|g| match g {
                    GenericParam::Type(n) => Some(*n),
                    GenericParam::Const(..) => None,
                }).collect();
                *name = self.scopes.calls.get(*name).copied().unwrap_or(*name);
                for (_, ty) in params { self.ty(ty, &gparams); }
                self.ty(return_type, &gparams);
            }
            TopLevelNode::Struct { name, fields, .. } => {
                let empty = HashSet::new();
                *name = self.scopes.types.get(*name).copied().unwrap_or(*name);
                for (_, ty) in fields { self.ty(ty, &empty); }
            }
        }
    }
}

/// Build the symbol table a module exposes (its final emitted names).
fn build_symtab<'a>(m: &Module<'a>, arena: &'a Bump) -> SymTab<'a> {
    let mut st = SymTab::default();
    for tl in &m.items {
        match &tl.value {
            TopLevelNode::Function { name, attributes, .. } => {
                st.fns.insert(name, final_fn_name(m, name, attributes, false, arena));
            }
            TopLevelNode::Extern { name, attributes, .. } => {
                st.fns.insert(name, final_fn_name(m, name, attributes, true, arena));
            }
            TopLevelNode::Struct { name, attributes, .. } => {
                st.structs.insert(name, final_struct_name(m, name, attributes, arena));
            }
        }
    }
    st
}

pub fn load_and_merge<'a>(entry: &Path, prelude_src: Option<&'a str>, arena: &'a Bump)
    -> Result<Vec<TopLevel<'a>>, ()>
{
    // A module we've decided to load but not parsed yet.
    struct Pending<'a> {
        key: String,
        src: &'a str,
        dir: Option<PathBuf>,
        is_entry: bool,
    }

    let mut worklist: VecDeque<Pending<'a>> = VecDeque::new();

    // Prelude first (id 0, if present), so it reads nicely in dumped output and
    // becomes the implicit whole-module import of every user module.
    let has_prelude = prelude_src.is_some();
    if let Some(psrc) = prelude_src {
        worklist.push_back(Pending { key: "<prelude>".into(), src: psrc, dir: None, is_entry: false });
    }

    // Entry module.
    let entry_src = match std::fs::read_to_string(entry) {
        Ok(s) => arena.alloc_str(&s),
        Err(e) => {
            eprintln!("Error: cannot read entry file '{}': {}", entry.display(), e);
            return Err(());
        }
    };
    let entry_key = std::fs::canonicalize(entry)
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|_| entry.to_string_lossy().into_owned());
    worklist.push_back(Pending {
        key: entry_key,
        src: entry_src,
        dir: entry.parent().map(|d| d.to_path_buf()),
        is_entry: true,
    });

    let mut modules: Vec<Module<'a>> = Vec::new();
    let mut seen: HashMap<String, usize> = HashMap::new();
    let mut had_error = false;

    while let Some(p) = worklist.pop_front() {
        if seen.contains_key(&p.key) { continue; }
        let id = modules.len();
        seen.insert(p.key.clone(), id);

        let key_static = arena.alloc_str(&p.key);
        let (imports, items) = match parse_module(key_static, p.src, arena) {
            Ok(pi) => pi,
            Err(()) => { had_error = true; continue; }
        };

        let prefix = format!("m{}_{}", id, p.key
            .rsplit(['/', '\\']).next().unwrap_or("mod")
            .trim_end_matches(".ixc")
            .replace(|c: char| !c.is_alphanumeric(), "_"));

        // Resolve + enqueue each import.
        let mut import_keys = Vec::with_capacity(imports.len());
        for imp in &imports {
            match resolve_key(imp, p.dir.as_deref()) {
                Ok(key) => {
                    if !seen.contains_key(&key) {
                        match load_import(imp, &key, p.dir.as_deref(), arena) {
                            Ok((src, dir)) => worklist.push_back(Pending {
                                key: key.clone(), src, dir, is_entry: false,
                            }),
                            Err(msg) => {
                                let (l, c) = line_col(p.src, imp.span.start);
                                eprintln!("Import error in {}:{}:{}: {}", p.key, l, c, msg);
                                had_error = true;
                                import_keys.push(None);
                                continue;
                            }
                        }
                    }
                    import_keys.push(Some(key));
                }
                Err(msg) => {
                    let (l, c) = line_col(p.src, imp.span.start);
                    eprintln!("Import error in {}:{}:{}: {}", p.key, l, c, msg);
                    had_error = true;
                    import_keys.push(None);
                }
            }
        }

        modules.push(Module {
            key: p.key,
            src: p.src,
            prefix,
            is_entry: p.is_entry,
            imports,
            import_keys,
            items,
        });
    }

    if had_error {
        return Err(());
    }

    // Symbol table for every module (indexed by module id).
    let symtabs: Vec<SymTab> = modules.iter().map(|m| build_symtab(m, arena)).collect();
    let prelude_id = if has_prelude { Some(0usize) } else { None };

    let mut errs: Vec<String> = Vec::new();

    // Pass 1: build every module's name-resolution scopes (owned; the values are
    // all `&'a`, so `all_scopes` borrows nothing from `modules`/`symtabs`).
    let mut all_scopes: Vec<Scopes> = Vec::with_capacity(modules.len());
    for (id, m) in modules.iter().enumerate() {
        let mut scopes = Scopes::default();

        // 1. implicit prelude whole-module import (except for the prelude itself)
        if let Some(pid) = prelude_id {
            if id != pid {
                for (&k, &v) in &symtabs[pid].fns { scopes.calls.insert(k, v); }
                for (&k, &v) in &symtabs[pid].structs { scopes.types.insert(k, v); }
            }
        }

        // 2. explicit imports
        let mut from_import_calls: HashSet<&str> = HashSet::new();
        let mut from_import_types: HashSet<&str> = HashSet::new();
        let mut qual_owner: HashMap<&str, usize> = HashMap::new();

        for (imp, key) in m.imports.iter().zip(&m.import_keys) {
            let Some(key) = key else { continue };
            let target_id = seen[key];
            let target = &symtabs[target_id];

            match &imp.symbols {
                None => {
                    // whole module: everything visible unqualified
                    for (&k, &v) in &target.fns {
                        if from_import_calls.contains(k) && scopes.calls.get(k).copied() != Some(v) {
                            let (l, c) = line_col(m.src, imp.span.start);
                            errs.push(format!(
                                "Import error in {}:{}:{}: '{}' is imported from more than one \
                                 module; use a selective `{{ {} }}` import to disambiguate",
                                m.key, l, c, k, k));
                        }
                        scopes.calls.insert(k, v);
                        from_import_calls.insert(k);
                    }
                    for (&k, &v) in &target.structs {
                        if from_import_types.contains(k) && scopes.types.get(k).copied() != Some(v) {
                            let (l, c) = line_col(m.src, imp.span.start);
                            errs.push(format!(
                                "Import error in {}:{}:{}: struct '{}' is imported from more than one module",
                                m.key, l, c, k));
                        }
                        scopes.types.insert(k, v);
                        from_import_types.insert(k);
                    }
                }
                Some(syms) => {
                    // selective: visible only under `qualifier::sym`
                    let qualifier = *imp.path.last().unwrap();
                    if let Some(&prev) = qual_owner.get(qualifier) {
                        if prev != target_id {
                            let (l, c) = line_col(m.src, imp.span.start);
                            errs.push(format!(
                                "Import error in {}:{}:{}: qualifier '{}' already refers to a different module",
                                m.key, l, c, qualifier));
                        }
                    }
                    qual_owner.insert(qualifier, target_id);
                    let map = scopes.quals.entry(qualifier).or_default();
                    for sym in syms {
                        if let Some(&f) = target.fns.get(sym).or_else(|| target.structs.get(sym)) {
                            map.insert(sym, f);
                        } else {
                            let (l, c) = line_col(m.src, imp.span.start);
                            errs.push(format!(
                                "Import error in {}:{}:{}: module '{}' has no exported symbol '{}'",
                                m.key, l, c, imp.path.join("/"), sym));
                        }
                    }
                }
            }
        }

        // 3. this module's own definitions win over imports
        for (&k, &v) in &symtabs[id].fns { scopes.calls.insert(k, v); }
        for (&k, &v) in &symtabs[id].structs { scopes.types.insert(k, v); }

        all_scopes.push(scopes);
    }

    // Pass 2: rewrite each module's items in place using its scopes.
    for (id, m) in modules.iter_mut().enumerate() {
        let key = m.key.clone();
        let src = m.src;
        let scopes = &all_scopes[id];
        let mut rw = Rewriter { scopes, key: &key, src, errs: &mut errs };
        for tl in &mut m.items {
            rw.toplevel(tl);
        }
    }

    if !errs.is_empty() {
        for e in &errs { eprintln!("{}", e); }
        return Err(());
    }

    // Merge: concatenate all modules, de-duplicating global `extern` names.
    let mut out: Vec<TopLevel<'a>> = Vec::new();
    let mut seen_externs: HashSet<&str> = HashSet::new();
    for m in &modules {
        for tl in &m.items {
            if let TopLevelNode::Extern { name, .. } = &tl.value {
                if !seen_externs.insert(name) {
                    continue; // same global extern already emitted
                }
            }
            out.push(tl.clone());
        }
    }

    Ok(out)
}
