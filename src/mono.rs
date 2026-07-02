//! monomorphization: rewrite generic (type-param) functions into concrete ones.
//!
//! runs AST -> AST after the first typecheck (which tells us which functions are
//! generic) and before MIL lowering. every distinct instantiation reachable from
//! a concrete call site becomes its own function with a mangled name
//! (`id::<i32>` -> `id$i32`), and the call site is rewritten to call it directly.
//! the result gets re-typechecked so node_types is filled in for the new nodes.
//! MIL never sees a generic.
//!
//! type params only. const generics on user functions are rejected back in
//! typecheck, so they never get here.

use std::collections::{HashMap, VecDeque};

use bumpalo::Bump;

use crate::ast::*;

/// one requested instantiation: `base` specialized to `args`, emitted as
/// `mangled`. `span` is the call site that first asked for it (for errors).
struct Instantiation<'a> {
    base: &'a str,
    args: Vec<Type<'a>>,
    mangled: &'a str,
    span: Span,
}

/// cap on distinct instantiations. backstop for combinatorial blowup (not
/// depth-growing); real programs are nowhere near this.
const INSTANTIATION_LIMIT: usize = 10_000;

/// cap on how deep a type argument can nest. polymorphic recursion like
/// `f<T>(...) { f::<*T>(...) }` grows the type forever (T, *T, **T, ...); this
/// catches it early, before the growing types make each instance expensive.
/// nothing hand-written nests this deep.
const TYPE_DEPTH_LIMIT: usize = 128;

/// how deep a type nests: i32 = 1, *i32 = 2, [*i32; 4] = 3, ...
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
    /// arena the mangled names live in. outlives the AST we produce, so the
    /// `&'a str` names it hands out are valid for `'a`.
    arena: &'a Bump,
    /// generic function templates, by name.
    templates: HashMap<&'a str, &'p TopLevel<'a>>,
    /// instantiations still to build.
    queue: VecDeque<Instantiation<'a>>,
    /// mangled name per instantiation we've already asked for, keyed by the
    /// mangled string, so repeated call sites reuse one instance (one alloc).
    seen: HashMap<String, &'a str>,
    /// mangled instance name -> its human-readable spelling (`m2_alloc$alloc$Vec2`
    /// -> `alloc::<Vec2>`). handed back to the caller so post-mono passes can
    /// report friendly names instead of mangled ones.
    display: HashMap<&'a str, String>,
}

/// Substitute bound type params in `ty` with their concrete bindings. in the raw
/// pre-typecheck AST a type param looks like `Type::Struct(name)` (parser can't
/// tell it from a real struct), so both Struct and Param get substituted when
/// bound.
///
/// FIXME: a real struct named the same as a bound type param gets wrongly
/// rewritten inside a generic body (e.g. `struct T` used inside `f<T>`). no
/// scope check distinguishes them. edge case, but there's no guard.
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

/// encode a concrete type into an identifier-safe fragment for a mangled name.
/// $, _ and alphanumerics are all fine in unquoted LLVM identifiers.
///
/// FIXME: not injective. a struct named `p_i32` mangles the same as `*i32`, and
/// all function types collapse to "fn". since `request` keys the instance cache
/// on this string, two different type args that mangle the same silently share
/// one instantiation -> wrong specialization. (fn types aren't spellable in a
/// type position yet, so that half is latent for now.)
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

/// Human-readable spelling of an instance, for diagnostics only. Strips the
/// internal module prefix off `base` (`m2_alloc$alloc` -> `alloc`, since the
/// prefix is an enqueue-order artifact, not something the user wrote) and
/// re-attaches the turbofish: `alloc`, `[Vec2]` -> `alloc::<Vec2>`. Best-effort;
/// never fed back into the compiler.
fn display_name(base: &str, args: &[Type]) -> String {
    let leaf = base.rsplit('$').next().unwrap_or(base);
    let targs = args.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
    format!("{}::<{}>", leaf, targs)
}

impl<'p, 'a> Mono<'p, 'a> {
    /// Record an instantiation request, return its (stable) mangled name.
    /// de-dupes so each distinct instance is built exactly once.
    fn request(&mut self, base: &'a str, args: Vec<Type<'a>>, span: Span) -> &'a str {
        let key = mangle_name(base, &args);
        if let Some(&m) = self.seen.get(&key) {
            return m;
        }
        let mangled: &'a str = self.arena.alloc_str(&key);
        self.seen.insert(key, mangled);
        self.display.insert(mangled, display_name(base, &args));
        self.queue.push_back(Instantiation { base, args, mangled, span });
        mangled
    }

    fn rebuild_expr(&mut self, expr: &Expr<'a>, b: &HashMap<&'a str, Type<'a>>) -> Expr<'a> {
        let node = match &expr.value {
            ExprNode::Call { func, type_args, args } => {
                let new_args: Vec<Expr<'a>> =
                    args.iter().map(|a| self.rebuild_expr(a, b)).collect();
                // sub type params inside the turbofish (user generic calls +
                // intrinsics like `sizeof::<T>()`)
                let subst_targs: Vec<GenericArg<'a>> = type_args.iter().map(|ga| match ga {
                    GenericArg::Type(t) => GenericArg::Type(subst_ty(t, b)),
                    GenericArg::Const(n) => GenericArg::Const(*n),
                }).collect();

                // user generic call: mangle to the concrete instance, drop the
                // turbofish.
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
            // leaves: copy as-is (fresh id)
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

    /// Rebuild a function with type params substituted per `b`, an optional new
    /// (mangled) name, and no generics. call sites inside get rewritten and any
    /// generic calls found get queued.
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

/// Expand every generic function reachable from a concrete call site into
/// concrete instances, drop the templates. non-generic functions, externs and
/// structs stay (with call sites rewritten).
///
/// Also returns a `mangled instance name -> friendly spelling` map so post-mono
/// passes (e.g. the alloc check) can report `alloc::<Vec2>` instead of the raw
/// `m2_alloc$alloc$Vec2`.
pub fn monomorphize<'a>(program: &[TopLevel<'a>], arena: &'a Bump)
    -> Result<(Vec<TopLevel<'a>>, HashMap<&'a str, String>), Error>
{
    let templates: HashMap<&'a str, &TopLevel<'a>> = program.iter()
        .filter_map(|tl| match &tl.value {
            TopLevelNode::Function { name, generics, .. } if !generics.is_empty() =>
                Some((*name, tl)),
            _ => None,
        })
        .collect();

    let mut m = Mono {
        arena, templates,
        queue: VecDeque::new(), seen: HashMap::new(), display: HashMap::new(),
    };
    let empty: HashMap<&'a str, Type<'a>> = HashMap::new();

    // rebuild concrete functions (their call sites seed the queue), keyed by
    // original position so we can re-emit in source order.
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

    // build each requested instantiation. instances can request more, so drain
    // till the queue is empty. group by template so they emit next to it.
    // NOTE: the depth/count guards below fire after pop, i.e. after this item was
    // already built+queued — so we overshoot the cutoff by one instance. fine as
    // a backstop, just not a tight bound.
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

    // reassemble in source order: each template -> its instances (or nothing if
    // it was never instantiated).
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

    Ok((output, m.display))
}
