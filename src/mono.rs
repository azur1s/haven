//! Monomorphization: turns a program containing generic (type-parameter)
//! functions into an equivalent program with only concrete functions.
//!
//! This runs as an AST -> AST pass *after* the first typecheck (which validates
//! the program and confirms which functions are generic) and *before* MIL
//! lowering. Each distinct instantiation of a generic function reachable from a
//! concrete call site is materialized into its own function with a mangled name
//! (e.g. `id::<i32>` -> `id$i32`), and every generic call site is rewritten to
//! call the mangled instance directly. The result is re-typechecked so that
//! `node_types` is fully populated for the freshly-created concrete nodes; from
//! MIL's point of view the program was never generic.
//!
//! Only type parameters are handled. Const generics on user functions are still
//! unsupported (and rejected earlier during typecheck), so they never reach here.

use std::collections::{HashMap, VecDeque};

use crate::ast::*;

/// Leaks a generated mangled name to obtain a `&'static str`, which coerces to
/// the source lifetime `'a`. The compiler is single-shot, so leaking a bounded
/// number of instantiation names is fine.
fn leak(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

/// A single requested instantiation: the generic function `base` specialized to
/// the concrete `args`, materialized under the name `mangled`. `span` is the
/// call site that first requested it, used for error reporting.
struct Instantiation<'a> {
    base: &'a str,
    args: Vec<Type<'a>>,
    mangled: &'a str,
    span: Span,
}

/// Upper bound on the number of distinct instantiations. A backstop for
/// combinatorial (rather than depth-growing) explosion; real programs stay far
/// below it.
const INSTANTIATION_LIMIT: usize = 10_000;

/// Upper bound on the nesting depth of a type argument. Polymorphic recursion
/// like `proc f<T>(...) { f::<*T>(...); }` mints an ever-deeper type
/// (`T`, `*T`, `**T`, ...); this catches it almost immediately, before the
/// growing types make each instantiation expensive. No hand-written type nests
/// anywhere near this deep.
const TYPE_DEPTH_LIMIT: usize = 128;

/// Nesting depth of a type: `i32` is 1, `*i32` is 2, `[*i32; 4]` is 3, etc.
fn type_depth(ty: &Type) -> usize {
    match ty {
        Type::Pointer(inner)
        | Type::Array(inner, _)
        | Type::Slice(inner)
        | Type::Simd(inner, _) => 1 + type_depth(inner),
        Type::Function { params, return_type } => {
            1 + params.iter().chain(std::iter::once(&**return_type))
                .map(type_depth).max().unwrap_or(0)
        }
        _ => 1,
    }
}

struct Mono<'p, 'a> {
    /// Generic function templates, keyed by name.
    templates: HashMap<&'a str, &'p TopLevel<'a>>,
    /// Instantiations still to materialize.
    queue: VecDeque<Instantiation<'a>>,
    /// Mangled name for each instantiation we've already requested, keyed by the
    /// mangled string, so a repeated call site reuses one instance (and one leak).
    seen: HashMap<String, &'a str>,
}

/// Substitutes bound type parameters in `ty` with their concrete bindings. In
/// raw (pre-typecheck) AST a type parameter appears as `Type::Struct(name)`
/// (the parser can't tell it apart from a real struct), so both `Struct` and
/// `Param` spellings are treated as a substitution target when bound.
fn subst_ty<'a>(ty: &Type<'a>, b: &HashMap<&'a str, Type<'a>>) -> Type<'a> {
    match ty {
        Type::Param(n) | Type::Struct(n) if b.contains_key(n) => b[n].clone(),
        Type::Pointer(inner)  => Type::Pointer(Box::new(subst_ty(inner, b))),
        Type::Array(inner, n) => Type::Array(Box::new(subst_ty(inner, b)), *n),
        Type::Slice(inner)    => Type::Slice(Box::new(subst_ty(inner, b))),
        Type::Simd(inner, n)  => Type::Simd(Box::new(subst_ty(inner, b)), *n),
        Type::Function { params, return_type } => Type::Function {
            params: params.iter().map(|p| subst_ty(p, b)).collect(),
            return_type: Box::new(subst_ty(return_type, b)),
        },
        other => other.clone(),
    }
}

/// Encodes a concrete type into an identifier-safe fragment for a mangled name.
/// `$`, `_` and alphanumerics are all valid in unquoted LLVM identifiers.
fn mangle_ty(ty: &Type) -> String {
    match ty {
        Type::Void => "void".into(),
        Type::Bool => "bool".into(),
        Type::Int32 => "i32".into(),
        Type::Int64 => "i64".into(),
        Type::Uint32 => "u32".into(),
        Type::Uint64 => "u64".into(),
        Type::Float32 => "f32".into(),
        Type::Float64 => "f64".into(),
        Type::Str => "str".into(),
        Type::Pointer(inner) => format!("p_{}", mangle_ty(inner)),
        Type::Array(inner, n) => format!("arr{}_{}", n, mangle_ty(inner)),
        Type::Slice(inner) => format!("slice_{}", mangle_ty(inner)),
        Type::Simd(inner, n) => format!("simd{}_{}", n, mangle_ty(inner)),
        Type::Struct(name) => (*name).into(),
        // Neither should appear in a fully-concrete instantiation; encode them
        // defensively rather than panicking so a bug surfaces as a bad symbol.
        Type::Param(name) => format!("param_{}", name),
        Type::Function { .. } => "fn".into(),
    }
}

/// `id`, `[i32]` -> `id$slice_i32`.
fn mangle_name(base: &str, args: &[Type]) -> String {
    let parts = args.iter().map(mangle_ty).collect::<Vec<_>>().join("$");
    format!("{}${}", base, parts)
}

impl<'p, 'a> Mono<'p, 'a> {
    /// Records an instantiation request, returning its (stable) mangled name.
    /// De-duplicates so each distinct instance is materialized exactly once.
    fn request(&mut self, base: &'a str, args: Vec<Type<'a>>, span: Span) -> &'a str {
        let key = mangle_name(base, &args);
        if let Some(&m) = self.seen.get(&key) {
            return m;
        }
        let mangled: &'a str = leak(key.clone());
        self.seen.insert(key, mangled);
        self.queue.push_back(Instantiation { base, args, mangled, span });
        mangled
    }

    fn rebuild_expr(&mut self, expr: &Expr<'a>, b: &HashMap<&'a str, Type<'a>>) -> Expr<'a> {
        let node = match &expr.value {
            ExprNode::Call { func, type_args, args } => {
                let new_args: Vec<Expr<'a>> =
                    args.iter().map(|a| self.rebuild_expr(a, b)).collect();
                // substitute type params inside the turbofish (covers both user
                // generic calls and intrinsics like `sizeof::<T>()`)
                let subst_targs: Vec<GenericArg<'a>> = type_args.iter().map(|ga| match ga {
                    GenericArg::Type(t) => GenericArg::Type(subst_ty(t, b)),
                    GenericArg::Const(n) => GenericArg::Const(*n),
                }).collect();

                // A call to a user-defined generic function: mangle to the
                // concrete instance and drop the turbofish.
                if let ExprNode::Var(name) = &func.value {
                    if self.templates.contains_key(name) {
                        let concrete: Vec<Type<'a>> = subst_targs.iter().map(|ga| match ga {
                            GenericArg::Type(t) => t.clone(),
                            GenericArg::Const(_) => unreachable!(
                                "const generic args on user functions are rejected during typecheck"
                            ),
                        }).collect();
                        let mangled = self.request(name, concrete, expr.span.clone());
                        let new_func = Metadata::new(ExprNode::Var(mangled), func.span.clone());
                        ExprNode::Call {
                            func: Box::new(new_func),
                            type_args: Vec::new(),
                            args: new_args,
                        }
                    } else {
                        // intrinsic or ordinary call: keep the (substituted) turbofish
                        let new_func = self.rebuild_expr(func, b);
                        ExprNode::Call { func: Box::new(new_func), type_args: subst_targs, args: new_args }
                    }
                } else {
                    let new_func = self.rebuild_expr(func, b);
                    ExprNode::Call { func: Box::new(new_func), type_args: subst_targs, args: new_args }
                }
            }
            ExprNode::Slice(elems) =>
                ExprNode::Slice(elems.iter().map(|e| self.rebuild_expr(e, b)).collect()),
            ExprNode::Struct { name, fields } => ExprNode::Struct {
                name,
                fields: fields.iter().map(|(f, e)| (*f, self.rebuild_expr(e, b))).collect(),
            },
            ExprNode::Access { base, field } =>
                ExprNode::Access { base: Box::new(self.rebuild_expr(base, b)), field },
            ExprNode::Index { slice, index } => ExprNode::Index {
                slice: Box::new(self.rebuild_expr(slice, b)),
                index: Box::new(self.rebuild_expr(index, b)),
            },
            ExprNode::Unary { op, operand } =>
                ExprNode::Unary { op: *op, operand: Box::new(self.rebuild_expr(operand, b)) },
            ExprNode::Binary { op, left, right } => ExprNode::Binary {
                op: *op,
                left: Box::new(self.rebuild_expr(left, b)),
                right: Box::new(self.rebuild_expr(right, b)),
            },
            // leaves: reproduce verbatim (under a fresh id)
            ExprNode::Bool(v) => ExprNode::Bool(*v),
            ExprNode::Int32(v) => ExprNode::Int32(*v),
            ExprNode::Int64(v) => ExprNode::Int64(*v),
            ExprNode::Uint32(v) => ExprNode::Uint32(*v),
            ExprNode::Uint64(v) => ExprNode::Uint64(*v),
            ExprNode::Float32(v) => ExprNode::Float32(*v),
            ExprNode::Float64(v) => ExprNode::Float64(*v),
            ExprNode::Str(s) => ExprNode::Str(s),
            ExprNode::Var(name) => ExprNode::Var(name),
        };
        Metadata::new(node, expr.span.clone())
    }

    fn rebuild_stmt(&mut self, stmt: &Stmt<'a>, b: &HashMap<&'a str, Type<'a>>) -> Stmt<'a> {
        let node = match &stmt.value {
            StmtNode::Expr(e) => StmtNode::Expr(self.rebuild_expr(e, b)),
            StmtNode::Block(stmts) =>
                StmtNode::Block(stmts.iter().map(|s| self.rebuild_stmt(s, b)).collect()),
            StmtNode::Declare { name, ty, value } => StmtNode::Declare {
                name,
                ty: subst_ty(ty, b),
                value: self.rebuild_expr(value, b),
            },
            StmtNode::Assign { left, value } => StmtNode::Assign {
                left: self.rebuild_expr(left, b),
                value: self.rebuild_expr(value, b),
            },
            StmtNode::If { condition, then_branch, else_branch } => StmtNode::If {
                condition: self.rebuild_expr(condition, b),
                then_branch: Box::new(self.rebuild_stmt(then_branch, b)),
                else_branch: else_branch.as_ref().map(|s| Box::new(self.rebuild_stmt(s, b))),
            },
            StmtNode::While { condition, body } => StmtNode::While {
                condition: self.rebuild_expr(condition, b),
                body: Box::new(self.rebuild_stmt(body, b)),
            },
            StmtNode::Return(e) => StmtNode::Return(self.rebuild_expr(e, b)),
            StmtNode::Continue => StmtNode::Continue,
            StmtNode::Break => StmtNode::Break,
        };
        Metadata::new(node, stmt.span.clone())
    }

    /// Rebuilds a function with type parameters substituted per `b`, an optional
    /// new (mangled) name, and no generic list. Call sites inside are rewritten
    /// and any generic calls discovered are queued.
    fn rebuild_function(
        &mut self,
        tl: &TopLevel<'a>,
        b: &HashMap<&'a str, Type<'a>>,
        name_override: Option<&'a str>,
    ) -> TopLevel<'a> {
        let TopLevelNode::Function { name, attributes, params, return_type, body, .. } = &tl.value
        else { unreachable!("rebuild_function called on a non-function") };

        let new_params: Vec<(&'a str, Type<'a>)> =
            params.iter().map(|(pn, ty)| (*pn, subst_ty(ty, b))).collect();
        let new_return = subst_ty(return_type, b);
        let new_body: Vec<Stmt<'a>> = body.iter().map(|s| self.rebuild_stmt(s, b)).collect();

        Metadata::new(
            TopLevelNode::Function {
                name: name_override.unwrap_or(name),
                attributes: attributes.clone(),
                generics: Vec::new(),
                params: new_params,
                return_type: new_return,
                body: new_body,
            },
            tl.span.clone(),
        )
    }
}

/// Expands all generic functions reachable from concrete call sites into
/// concrete instances, dropping the templates. Non-generic functions, externs
/// and structs are preserved (with call sites rewritten).
pub fn monomorphize<'a>(program: &[TopLevel<'a>]) -> Result<Vec<TopLevel<'a>>, Error> {
    let templates: HashMap<&'a str, &TopLevel<'a>> = program.iter()
        .filter_map(|tl| match &tl.value {
            TopLevelNode::Function { name, generics, .. } if !generics.is_empty() =>
                Some((*name, tl)),
            _ => None,
        })
        .collect();

    let mut m = Mono { templates, queue: VecDeque::new(), seen: HashMap::new() };
    let empty: HashMap<&'a str, Type<'a>> = HashMap::new();

    // rebuild concrete functions (seeds the queue via their call sites), keyed
    // by their original position so they can be re-emitted in source order.
    let mut concrete: HashMap<usize, TopLevel<'a>> = HashMap::new();
    for (i, tl) in program.iter().enumerate() {
        match &tl.value {
            TopLevelNode::Function { generics, .. } if !generics.is_empty() => {} // template
            TopLevelNode::Function { .. } => {
                concrete.insert(i, m.rebuild_function(tl, &empty, None));
            }
            TopLevelNode::Extern { .. } | TopLevelNode::Struct { .. } => {}
        }
    }

    // materialize each requested instantiation; instances may themselves request
    // further instantiations, so drain until the queue is empty. Instances are
    // grouped by their template so they can be emitted next to it.
    let mut instances: HashMap<&'a str, Vec<TopLevel<'a>>> = HashMap::new();
    let mut materialized = 0usize;
    while let Some(inst) = m.queue.pop_front() {
        materialized += 1;
        if let Some(deep) = inst.args.iter().find(|t| type_depth(t) > TYPE_DEPTH_LIMIT) {
            return Err(Error {
                msg: format!(
                    "monomorphization of '{}' produced a type argument nested deeper \
                     than {} (`{}`); this usually means unbounded generic recursion \
                     (a generic function calling itself at an ever-growing type)",
                    inst.base, TYPE_DEPTH_LIMIT, deep,
                ),
                span: inst.span,
            });
        }
        if materialized > INSTANTIATION_LIMIT {
            return Err(Error {
                msg: format!(
                    "monomorphization exceeded {} instantiations at call to '{}'",
                    INSTANTIATION_LIMIT, inst.base,
                ),
                span: inst.span,
            });
        }
        let tl = m.templates[inst.base];
        let TopLevelNode::Function { generics, .. } = &tl.value else { unreachable!() };
        let type_params: Vec<&'a str> = generics.iter().filter_map(|g| match g {
            GenericParam::Type(n) => Some(*n),
            GenericParam::Const(_, _) => None,
        }).collect();
        let bindings: HashMap<&'a str, Type<'a>> =
            type_params.into_iter().zip(inst.args.iter().cloned()).collect();
        let f = m.rebuild_function(tl, &bindings, Some(inst.mangled));
        instances.entry(inst.base).or_default().push(f);
    }

    // reassemble in original source order: each template is replaced in place by
    // its concrete instances (or dropped if it was never instantiated).
    let mut output: Vec<TopLevel<'a>> = Vec::with_capacity(program.len());
    for (i, tl) in program.iter().enumerate() {
        match &tl.value {
            TopLevelNode::Function { name, generics, .. } if !generics.is_empty() => {
                if let Some(insts) = instances.remove(name) {
                    output.extend(insts);
                }
            }
            TopLevelNode::Function { .. } => output.push(concrete.remove(&i).unwrap()),
            TopLevelNode::Extern { .. } | TopLevelNode::Struct { .. } => output.push(tl.clone()),
        }
    }

    Ok(output)
}
