//! module loading, name mangling and merging.
//!
//! sits between parsing and typechecking. given an entry file it:
//!
//!   1. transitively loads every imported module (`import std/...` -> an embedded
//!      stdlib source; any other path -> an `.ixc` file relative to the
//!      *importing* file's dir);
//!   2. gives each module a unique mangling prefix and renames its top-level defs
//!      (`foo` in module `m1` -> `m1_...$foo`), leaving `extern` link names,
//!      `@export`ed items and the entry `main` alone;
//!   3. rewrites every reference (call targets, struct literals, struct types)
//!      per that module's imports;
//!   4. concatenates all modules into one flat program, no imports left.
//!
//! everything downstream (typecheck, mono, mil, ...) then sees one flat namespace
//! exactly like before modules existed. lifetimes stay trivial: every module's
//! source, tokens and freshly-minted (mangled) names go into one bumpalo arena
//! owned by the caller, and the returned AST borrows it for `'a`.
//!
//! ## import forms
//!
//! * `import std/math`            — whole module, symbols visible unqualified.
//! * `import std/math { sinf }`   — selective, visible only as `math::sinf`.
//!
//! the prelude is an implicit whole-module import into every user module.
//!
//! ## known v1 limitations
//!
//! * only call targets, struct literals and struct *types* get rewritten. a
//!   top-level function used as a first-class value (not called directly) isn't
//!   rewritten across modules — same limitation the monomorphizer has.
//! * no `pub`; every top-level item is public.
//! * types have no qualified spelling, so a *selectively* imported struct can't
//!   be named in a type position (import the whole module to use its structs).
//!   see `Rewriter::ty`: `geo::Point` in type position just falls through
//!   unrewritten and later reads as an unknown struct.

use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};

use bumpalo::Bump;

use crate::front::{ast::*, parse};
use crate::diag::{self, Sources};

/// The whole `crt/std` tree, embedded into the binary at build time. Nested
/// modules just work: `std/dsp/osc` -> `crt/std/dsp/osc.ixc`, no per-file wiring.
static STD_DIR: include_dir::Dir<'static> =
    include_dir::include_dir!("$CARGO_MANIFEST_DIR/std");

/// Source of an embedded stdlib module, by its `std/...` import path. The key is
/// `imp.path.join("/")` (always forward slashes), so `std/<rel>` maps to the
/// embedded file `<rel>.ixc`.
fn std_source(key: &str) -> Option<&'static str> {
    let rel = key.strip_prefix("std/")?;
    // include_dir keys files by forward-slash path relative to the embedded root,
    // on every host platform.
    let file = format!("{rel}.ixc");
    STD_DIR.get_file(&file)?.contents_utf8()
}

/// A loaded module: parsed contents plus the bookkeeping the resolver needs to
/// mangle and rewrite it
struct Module<'a> {
    /// canonical key (absolute path, or `std/...`, or `<prelude>`). de-dupes
    /// modules reached by more than one import.
    key: String,
    /// source text, also the name shown in diagnostics.
    src: &'a str,
    /// mangling prefix, unique per module, e.g. `m2_math`.
    prefix: String,
    is_entry: bool,
    imports: Vec<Import<'a>>,
    /// canonical key of each import (parallel to `imports`), or `None` if it
    /// failed to resolve (error already recorded).
    import_keys: Vec<Option<String>>,
    items: Vec<TopLevel<'a>>,
}

/// The final (post-mangling) name a module exposes per symbol, split by namespace
/// so call sites and type positions look in the right place.
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

/// Name a top-level fn/struct is emitted under: prefixed with the module prefix,
/// unless it must keep a stable spelling (`extern` link names and `@export`ed
/// items keep theirs; the entry `main` stays `main`).
fn final_fn_name<'a>(m: &Module<'_>, name: &str, attrs: &[Attribute], is_extern: bool, arena: &'a Bump) -> &'a str {
    // entry module is unique so its names can't clash with the prefixed imported
    // ones; leaving it unmangled keeps single-file diagnostics as they were.
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

/// Final emitted name for a module-level global. Same rule as functions: an
/// `@export`ed or entry-module global keeps its source name (a host looks the
/// symbol up by that name), everything else is prefixed to avoid cross-module
/// collisions.
fn final_global_name<'a>(m: &Module<'_>, name: &str, attrs: &[Attribute], arena: &'a Bump) -> &'a str {
    if is_export(attrs) || m.is_entry {
        arena.alloc_str(name)
    } else {
        arena.alloc_str(&format!("{}${}", m.prefix, name))
    }
}

/// Resolve an import to its canonical key. no file read for `std`; for the
/// relative case canonicalizes the path (so the file has to exist). `dir` is the
/// importing module's directory.
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

/// Load an import's source + the dir its own relative imports resolve against.
/// assumes `resolve_key` already succeeded for this import.
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
/// lex/parse diagnostics. tokens move into `arena` so the parsed AST can borrow
/// them for `'a`.
fn parse_module<'a>(key: &'a str, src: &'a str, arena: &'a Bump)
    -> Result<(Vec<Import<'a>>, Vec<TopLevel<'a>>), ()>
{
    // errors here can only point into this one module, so a single-source cache
    // is enough to quote them.
    let local: Sources = vec![(key.to_string(), src)];

    let (tokens, lex_errs) = parse::lex(key, src);
    for e in &lex_errs {
        diag::report("Lex error", &e.reason().to_string(), e.span(), &local);
    }
    let tokens = match tokens {
        Some(t) if lex_errs.is_empty() => t,
        _ => return Err(()),
    };
    let tokens: &'a [Metadata<Token<'a>>] = arena.alloc_slice_fill_iter(tokens);

    let (parsed, parse_errs) = parse::parse(key.to_string(), src.len(), tokens);
    for e in &parse_errs {
        diag::report("Parse error", &e.reason().to_string(), e.span(), &local);
    }
    match parsed {
        Some(pi) if parse_errs.is_empty() => Ok(pi),
        _ => Err(()),
    }
}

/// Name-resolution scopes for one module.
#[derive(Default)]
struct Scopes<'a> {
    /// unqualified callable names in scope -> final emitted name.
    calls: HashMap<&'a str, &'a str>,
    /// unqualified struct type names in scope -> final emitted name.
    types: HashMap<&'a str, &'a str>,
    /// `qualifier -> (symbol -> final name)` for selective imports.
    quals: HashMap<&'a str, HashMap<&'a str, &'a str>>,
}

/// Holds the per-module scopes and error sink while rewriting a module's AST in
/// place.
struct Rewriter<'x, 'a> {
    scopes: &'x Scopes<'a>,
    errs: &'x mut Vec<Error>,
    /// names bound by params/`let` in the function being walked, innermost last.
    /// used as a stack: a block records `locals.len()` on entry and truncates
    /// back to it on exit. a name in here shadows a top-level callable of the
    /// same name, so bare/call references to it are left unrewritten (it's a
    /// local value, not the top-level symbol).
    locals: Vec<&'a str>,
}

impl<'x, 'a> Rewriter<'x, 'a> {
    fn error(&mut self, span: &Span, msg: String) {
        self.errs.push(Error::new(span.clone(), msg));
    }

    fn is_local(&self, name: &str) -> bool {
        self.locals.iter().any(|n| *n == name)
    }

    /// Rewrite struct type names in `ty`, skipping names bound as generic params
    /// of the enclosing function (those are type params, not structs).
    /// NOTE: only consults `scopes.types` (unqualified). a selectively-imported
    /// `geo::Point` in type position never matches and falls through unrewritten
    /// — see the v1 limitation at the top of the file.
    fn ty(&mut self, ty: &mut Type<'a>, gparams: &HashSet<&str>) {
        match ty {
            Type::Struct { name, args } => {
                let nm: &str = *name;
                if !gparams.contains(nm) {
                    if let Some(&f) = self.scopes.types.get(nm) {
                        *name = f;
                    }
                }
                // a generic struct's arguments are themselves types to rewrite.
                for a in args { self.ty(a, gparams); }
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

    /// Resolve a name in call position to its final emitted name. bare names not
    /// in scope are left alone (locals, params, globally-resolved externs,
    /// intrinsics, or errors caught later).
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
        } else if self.is_local(full) {
            // shadowed by a param/local: this is an indirect call through a value,
            // not a reference to the top-level symbol. leave it untouched.
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
            // a bare name used as a value (fn-as-value): rewrite it to the mangled
            // top-level name, unless a param/local shadows it (then it's a local
            // read, leave it). taking a *generic* fn by value has no type args to
            // monomorphize with; that's handled (or rejected) downstream, not here.
            ExprNode::Var(name) => {
                if !self.is_local(name) {
                    if let Some(&f) = self.scopes.calls.get(*name) { *name = f; }
                }
            }
            // remaining leaves (literals): nothing to rewrite
            _ => {}
        }
    }

    fn stmt(&mut self, s: &mut Stmt<'a>, gparams: &HashSet<&str>) {
        match &mut s.value {
            StmtNode::Expr(e) => self.expr(e, gparams),
            StmtNode::Block(ss) => {
                let mark = self.locals.len();
                for s in ss { self.stmt(s, gparams); }
                self.locals.truncate(mark); // drop names bound inside the block
            }
            StmtNode::Declare { ty, value, name } => {
                self.ty(ty, gparams);
                self.expr(value, gparams); // walk the initializer BEFORE binding,
                self.locals.push(name);    // so `let f = f;` sees the outer/top f
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
                // params are locals for the whole body; body-level `let`s stack on
                // top. truncate back to 0 so the next function starts clean.
                for (pname, ty) in params {
                    self.ty(ty, &gparams);
                    self.locals.push(pname);
                }
                self.ty(return_type, &gparams);
                for s in body { self.stmt(s, &gparams); }
                self.locals.clear();
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
            TopLevelNode::Struct { name, generics, fields, .. } => {
                // the struct's own type params shadow struct names when rewriting
                // field types (a field `T` is a param, not a module type).
                let gparams: HashSet<&str> = generics.iter().filter_map(|g| match g {
                    GenericParam::Type(n) => Some(*n),
                    GenericParam::Const(..) => None,
                }).collect();
                *name = self.scopes.types.get(*name).copied().unwrap_or(*name);
                for (_, ty) in fields { self.ty(ty, &gparams); }
            }
            TopLevelNode::Global { name, ty, value, .. } => {
                let empty = HashSet::new();
                *name = self.scopes.calls.get(*name).copied().unwrap_or(*name);
                self.ty(ty, &empty);
                self.expr(value, &empty);
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
            // globals live in the callable/value namespace (referenced as vars).
            TopLevelNode::Global { name, attributes, .. } => {
                st.fns.insert(name, final_global_name(m, name, attributes, arena));
            }
        }
    }
    st
}

pub fn load_and_merge<'a>(entry: &Path, prelude_src: Option<&'a str>, arena: &'a Bump)
    -> Result<(Vec<TopLevel<'a>>, Sources<'a>), ()>
{
    // a module we've decided to load but haven't parsed yet.
    struct Pending<'a> {
        key: String,
        src: &'a str,
        dir: Option<PathBuf>,
        is_entry: bool,
    }

    let mut worklist: VecDeque<Pending<'a>> = VecDeque::new();

    // prelude first (id 0, if present) so it reads nicely in dumped output and
    // becomes the implicit whole-module import of every user module.
    let has_prelude = prelude_src.is_some();
    if let Some(psrc) = prelude_src {
        worklist.push_back(Pending { key: "<prelude>".into(), src: psrc, dir: None, is_entry: false });
    }

    // entry module. canonicalize *first* so its key matches how imports are
    // keyed (imports always canonicalize). otherwise a module that imports the
    // entry back would key it differently, miss in `seen`, and get the entry
    // parsed + merged twice (dup `main`). if canonicalize fails we can't read it
    // anyway, so bail.
    let entry_path = match std::fs::canonicalize(entry) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Error: cannot read entry file '{}': {}", entry.display(), e);
            return Err(());
        }
    };
    let entry_src = match std::fs::read_to_string(&entry_path) {
        Ok(s) => arena.alloc_str(&s),
        Err(e) => {
            eprintln!("Error: cannot read entry file '{}': {}", entry_path.display(), e);
            return Err(());
        }
    };
    worklist.push_back(Pending {
        key: entry_path.to_string_lossy().into_owned(),
        src: entry_src,
        dir: entry_path.parent().map(|d| d.to_path_buf()),
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

        // FIXME: prefix is `m{id}_{basename}` and `id` is enqueue-order, so
        // emitted symbol names shift whenever unrelated imports are added/removed.
        // fine within a single link, bad for --shared/--static-lib ABI and for
        // reproducible IR diffs. want a stable (content/path-based) key instead.
        let prefix = format!("m{}_{}", id, p.key
            .rsplit(['/', '\\']).next().unwrap_or("mod")
            .trim_end_matches(".ixc")
            .replace(|c: char| !c.is_alphanumeric(), "_"));

        // resolve + enqueue each import. errors point at the import statement in
        // this module, so a single-source cache quotes them.
        let local: Sources = vec![(p.key.clone(), p.src)];
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
                                diag::report("Import error", &msg, &imp.span, &local);
                                had_error = true;
                                import_keys.push(None);
                                continue;
                            }
                        }
                    }
                    import_keys.push(Some(key));
                }
                Err(msg) => {
                    diag::report("Import error", &msg, &imp.span, &local);
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

    // (file-key, source) for every loaded module. keys match `Span::file`, so any
    // downstream stage can quote the span's *owning* module. also the value
    // returned to the caller for its own diagnostics.
    let sources: Sources = modules.iter().map(|m| (m.key.clone(), m.src)).collect();

    // symbol table for every module (indexed by module id).
    let symtabs: Vec<SymTab> = modules.iter().map(|m| build_symtab(m, arena)).collect();
    let prelude_id = if has_prelude { Some(0usize) } else { None };

    let mut errs: Vec<Error> = Vec::new();

    // pass 1: build every module's name-resolution scopes (owned; values are all
    // `&'a`, so `all_scopes` borrows nothing from `modules`/`symtabs`).
    let mut all_scopes: Vec<Scopes> = Vec::with_capacity(modules.len());
    for (id, m) in modules.iter().enumerate() {
        let mut scopes = Scopes::default();

        // 1. implicit prelude whole-module import (except into the prelude itself)
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
                    // FIXME: the collision check below only looks at
                    // from_import_calls, which never contains prelude-origin
                    // names. so a whole-module import that redefines a prelude
                    // symbol (e.g. `print`) silently shadows it with no
                    // "imported from more than one module" error.
                    for (&k, &v) in &target.fns {
                        if from_import_calls.contains(k) && scopes.calls.get(k).copied() != Some(v) {
                            errs.push(Error::new(imp.span.clone(), format!(
                                "'{}' is imported from more than one module; use a selective \
                                 `{{ {} }}` import to disambiguate", k, k)));
                        }
                        scopes.calls.insert(k, v);
                        from_import_calls.insert(k);
                    }
                    for (&k, &v) in &target.structs {
                        if from_import_types.contains(k) && scopes.types.get(k).copied() != Some(v) {
                            errs.push(Error::new(imp.span.clone(), format!(
                                "struct '{}' is imported from more than one module", k)));
                        }
                        scopes.types.insert(k, v);
                        from_import_types.insert(k);
                    }
                }
                Some(syms) => {
                    // selective: visible only as `qualifier::sym`
                    let qualifier = *imp.path.last().unwrap();
                    if let Some(&prev) = qual_owner.get(qualifier) {
                        if prev != target_id {
                            errs.push(Error::new(imp.span.clone(), format!(
                                "qualifier '{}' already refers to a different module", qualifier)));
                        }
                    }
                    qual_owner.insert(qualifier, target_id);
                    let map = scopes.quals.entry(qualifier).or_default();
                    for sym in syms {
                        if let Some(&f) = target.fns.get(sym).or_else(|| target.structs.get(sym)) {
                            map.insert(sym, f);
                        } else {
                            errs.push(Error::new(imp.span.clone(), format!(
                                "module '{}' has no exported symbol '{}'", imp.path.join("/"), sym)));
                        }
                    }
                }
            }
        }

        // 3. this module's own defs win over imports (inserted last)
        for (&k, &v) in &symtabs[id].fns { scopes.calls.insert(k, v); }
        for (&k, &v) in &symtabs[id].structs { scopes.types.insert(k, v); }

        all_scopes.push(scopes);
    }

    // pass 2: rewrite each module's items in place using its scopes.
    for (id, m) in modules.iter_mut().enumerate() {
        let scopes = &all_scopes[id];
        let mut rw = Rewriter { scopes, errs: &mut errs, locals: Vec::new() };
        for tl in &mut m.items {
            rw.toplevel(tl);
        }
    }

    if !errs.is_empty() {
        for e in &errs { diag::report_error("Import error", e, &sources); }
        return Err(());
    }

    // merge: concatenate all modules into one flat program, reconciling the
    // global callable namespace (functions + externs, keyed by final emitted
    // name) as we go. two identical `extern` declarations of the same C symbol
    // are the legit duplicate — de-duped silently. everything else that collides
    // on a final name would become a duplicate LLVM symbol at link time, so we
    // turn it into a source diagnostic here instead:
    //   * two externs, same name, different signature -> mismatched-ABI error
    //   * any other same-name collision (two defs, or a def vs an extern) ->
    //     already-defined error. covers colliding `@export`s and a user fn
    //     shadowing a prelude extern, which the old single-file path caught too.
    // structs live in a separate namespace; they don't participate here.
    let mut out: Vec<TopLevel<'a>> = Vec::new();
    let mut merge_errs: Vec<Error> = Vec::new();
    let mut seen_callables: HashMap<&str, &TopLevel<'a>> = HashMap::new();
    // globals share the value namespace but carry no signature, so they get their
    // own dedup keyed by final name. (a global colliding with a *function* name is
    // left for the downstream duplicate-symbol check.)
    let mut seen_globals: HashSet<&str> = HashSet::new();
    for m in &modules {
        for tl in &m.items {
            let (name, is_extern, params, ret) = match &tl.value {
                TopLevelNode::Function { name, params, return_type, .. } =>
                    (*name, false, params.as_slice(), return_type),
                TopLevelNode::Extern { name, params, return_type, .. } =>
                    (*name, true, params.as_slice(), return_type),
                TopLevelNode::Struct { .. } => { out.push(tl.clone()); continue; }
                TopLevelNode::Global { name, .. } => {
                    if !seen_globals.insert(*name) {
                        merge_errs.push(Error::new(tl.span.clone(), format!(
                            "global '{}' is already defined", name)));
                    } else {
                        out.push(tl.clone());
                    }
                    continue;
                }
            };

            if let Some(prev) = seen_callables.get(name) {
                let (prev_extern, prev_params, prev_ret) = match &prev.value {
                    TopLevelNode::Function { params, return_type, .. } =>
                        (false, params.as_slice(), return_type),
                    TopLevelNode::Extern { params, return_type, .. } =>
                        (true, params.as_slice(), return_type),
                    TopLevelNode::Struct { .. } => unreachable!("only callables are recorded"),
                    TopLevelNode::Global { .. } => unreachable!("globals are deduped separately"),
                };
                // signatures match on types only (param names are irrelevant to the ABI)
                let sig_eq = params.len() == prev_params.len()
                    && params.iter().zip(prev_params).all(|((_, a), (_, b))| a == b)
                    && ret == prev_ret;

                if is_extern && prev_extern {
                    if sig_eq {
                        continue; // same extern already emitted: legit dedup
                    }
                    merge_errs.push(Error::new(tl.span.clone(), format!(
                        "extern '{}' is redeclared with a different signature", name)));
                } else {
                    merge_errs.push(Error::new(tl.span.clone(), format!(
                        "'{}' is already defined", name)));
                }
                continue;
            }

            seen_callables.insert(name, tl);
            out.push(tl.clone());
        }
    }

    if !merge_errs.is_empty() {
        for e in &merge_errs { diag::report_error("Merge error", e, &sources); }
        return Err(());
    }

    Ok((out, sources))
}
