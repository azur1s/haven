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

use noir_common::ast::*;

/// one requested instantiation: `base` specialized to `args`, emitted as
/// `mangled`. `span` is the call site that first asked for it (for errors).
struct Instantiation<'a> {
    base: &'a str,
    args: Vec<ConcreteArg<'a>>,
    mangled: &'a str,
    span: Span,
}

/// one requested generic-struct instantiation: `struct Buf<T, const N>` at `args`
/// (`[i32, 8]`), emitted as the concrete `struct Buf$i32$8`. args are types and/or
/// const values, matching the struct's declared params. `span` points at the
/// context that first named the type, for the depth-limit diagnostic.
struct StructInstantiation<'a> {
    base: &'a str,
    args: Vec<ConcreteArg<'a>>,
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
    /// generic struct templates, by name (`Option` -> `struct Option<T> {...}`).
    struct_templates: HashMap<&'a str, &'p TopLevel<'a>>,
    /// generic-struct instantiations still to build.
    struct_queue: VecDeque<StructInstantiation<'a>>,
    /// mangled name per struct instance already requested, keyed by the mangled
    /// string, so every concrete `Option$i32` use shares one emitted struct.
    struct_seen: HashMap<String, &'a str>,
    /// best-effort span for the context currently being rebuilt, so a struct
    /// instance requested deep inside `subst_ty` (which has no span of its own)
    /// still gets a source location for the depth-limit error.
    cur_span: Span,
    /// mangled instance name -> its human-readable spelling (`m2_alloc$alloc$Vec2`
    /// -> `alloc::<Vec2>`). handed back to the caller so post-mono passes can
    /// report friendly names instead of mangled ones.
    display: HashMap<&'a str, String>,
}

/// A bound const generic parameter: its concrete value plus declared type, so a
/// value-position use (`return N;`) can be re-emitted as a typed literal.
#[derive(Clone)]
struct ConstBind<'a> {
    val: usize,
    ty: Type<'a>,
}

/// Substitutions applied when specializing a template: type params -> concrete
/// types, const params -> concrete values.
struct Bindings<'a> {
    types: HashMap<&'a str, Type<'a>>,
    consts: HashMap<&'a str, ConstBind<'a>>,
}

impl<'a> Bindings<'a> {
    fn empty() -> Self {
        Bindings { types: HashMap::new(), consts: HashMap::new() }
    }
}

/// A fully concrete generic argument at an instantiation site: the value a type
/// or const param is specialized to. Keys the instance cache and mangled name.
enum ConcreteArg<'a> {
    Type(Type<'a>),
    Const(usize),
}

/// Substitute a bound const param with its literal value; leave literals and
/// unbound params untouched.
fn subst_cv<'a>(cv: &ConstVal<'a>, b: &Bindings<'a>) -> ConstVal<'a> {
    match cv {
        ConstVal::Param(n) => match b.consts.get(n) {
            Some(cb) => ConstVal::Lit(cb.val),
            None => cv.clone(),
        },
        ConstVal::Lit(_) => cv.clone(),
    }
}

/// Materialize a bound const param used in value position as a typed integer
/// literal (`const N: u32` bound to 4 -> `4u32`).
fn const_literal<'a>(cb: &ConstBind<'a>) -> ExprNode<'a> {
    match &cb.ty {
        Type::Int8   => ExprNode::Int8(cb.val as i8),
        Type::Int32  => ExprNode::Int32(cb.val as i32),
        Type::Int64  => ExprNode::Int64(cb.val as i64),
        Type::Uint8  => ExprNode::Uint8(cb.val as u8),
        Type::Uint32 => ExprNode::Uint32(cb.val as u32),
        Type::Uint64 => ExprNode::Uint64(cb.val as u64),
        other => panic!("const generic parameter has non-integer type {}", other),
    }
}

/// encode a concrete type into an identifier-safe fragment for a mangled name.
/// `$`, `.`, `_` and alphanumerics are all fine in unquoted LLVM identifiers.
///
/// Injectivity: every type *constructor* (pointer/array/slice/simd/param/fn/
/// generic-struct) is encoded with a leading `.tag`. A user struct name is a bare
/// identifier and can never contain `.`, and the scalar keywords are a fixed set
/// with no `.` - so a constructor fragment can never collide with a struct name
/// or a scalar. Struct arguments are wrapped in balanced `.lt`/`.gt` so their
/// boundaries stay unambiguous under nesting. (Two different types therefore
/// never mangle alike, which matters because `request*` keys its instance cache
/// on this string - a collision would silently share one instantiation.)
fn mangle_ty(ty: &Type) -> String {
    match ty {
        Type::Void => "void".into(),
        Type::Bool => "bool".into(),
        Type::Int8 => "i8".into(),
        Type::Int32 => "i32".into(),
        Type::Int64 => "i64".into(),
        Type::Uint8 => "u8".into(),
        Type::Uint32 => "u32".into(),
        Type::Uint64 => "u64".into(),
        Type::Float32 => "f32".into(),
        Type::Float64 => "f64".into(),
        Type::Str => "str".into(),
        Type::Pointer(inner) => format!(".ptr{}", mangle_ty(inner)),
        Type::Array(inner, n) => format!(".arr{}.{}", n.expect_lit(), mangle_ty(inner)),
        Type::Slice(inner) => format!(".slice{}", mangle_ty(inner)),
        Type::Simd(inner, n) => format!(".simd{}.{}", n.expect_lit(), mangle_ty(inner)),
        Type::Struct { name, args } if args.is_empty() => (*name).into(),
        // a generic struct instance is normally flattened to a bare name before it
        // reaches here (subst_ty requests it); encode defensively with balanced
        // brackets so nested args stay unambiguous.
        Type::Struct { name, args } => {
            let inner = args.iter().map(mangle_generic_arg).collect::<Vec<_>>().join(".");
            format!(".struct.{}.{}", name, inner)
        }
        // Neither should appear in a fully-concrete instantiation; encode them
        // defensively rather than panicking so a bug surfaces as a bad symbol.
        Type::Param(name) => format!(".param.{}", name),
        Type::Function { params, return_type } => {
            let ps = params.iter().map(mangle_ty).collect::<Vec<_>>().join(".");
            format!(".fn{}.{}.ret{}", params.len(), ps, mangle_ty(return_type))
        }
    }
}

/// Mangle a generic argument in a struct type's arg list (a type or a const
/// value). Only reached on the defensive not-yet-flattened path in `mangle_ty`.
fn mangle_generic_arg(ga: &GenericArg) -> String {
    match ga {
        GenericArg::Type(t) => mangle_ty(t),
        GenericArg::Const(cv) => cv.to_string(),
    }
}

/// Identifier-safe fragment for one generic arg: a mangled type, or a const's
/// decimal value (`4`). Const values are pure digits and no type mangles to bare
/// digits, so the two never collide within a single arg list.
fn mangle_arg(arg: &ConcreteArg) -> String {
    match arg {
        ConcreteArg::Type(t) => mangle_ty(t),
        ConcreteArg::Const(n) => n.to_string(),
    }
}

/// `id`, `[i32]` -> `id$slice_i32`; `splat`, `f32`, `4` -> `splat$f32$4`.
fn mangle_name(base: &str, args: &[ConcreteArg]) -> String {
    let parts = args.iter().map(mangle_arg).collect::<Vec<_>>().join("$");
    format!("{}${}", base, parts)
}

/// Human-readable spelling of an instance, for diagnostics only. Strips the
/// internal module prefix off `base` (`m2_alloc$alloc` -> `alloc`, since the
/// prefix is an enqueue-order artifact, not something the user wrote) and
/// re-attaches the turbofish: `alloc`, `[Vec2]` -> `alloc::<Vec2>`. Best-effort;
/// never fed back into the compiler.
fn display_name(base: &str, args: &[ConcreteArg]) -> String {
    let leaf = base.rsplit('$').next().unwrap_or(base);
    let targs = args.iter().map(|a| match a {
        ConcreteArg::Type(t) => t.to_string(),
        ConcreteArg::Const(n) => n.to_string(),
    }).collect::<Vec<_>>().join(", ");
    format!("{}::<{}>", leaf, targs)
}

impl<'p, 'a> Mono<'p, 'a> {
    /// Record an instantiation request, return its (stable) mangled name.
    /// de-dupes so each distinct instance is built exactly once.
    fn request(&mut self, base: &'a str, args: Vec<ConcreteArg<'a>>, span: Span) -> &'a str {
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

    /// Record a generic-struct instantiation request, return its mangled name
    /// (`Buf` + `[i32, 8]` -> `Buf$i32$8`). de-dupes so each concrete instance is
    /// built once. Enqueues onto the struct queue, drained after all functions.
    fn request_struct(&mut self, base: &'a str, args: Vec<ConcreteArg<'a>>) -> &'a str {
        let key = mangle_name(base, &args);
        if let Some(&m) = self.struct_seen.get(&key) {
            return m;
        }
        let mangled: &'a str = self.arena.alloc_str(&key);
        self.struct_seen.insert(key, mangled);
        self.display.insert(mangled, display_name(base, &args));
        self.struct_queue.push_back(StructInstantiation {
            base, args, mangled, span: self.cur_span.clone(),
        });
        mangled
    }

    /// Substitute bound type and const params in `ty` with their concrete
    /// bindings. In the raw pre-typecheck AST a type param looks like
    /// `Type::Struct(name)` (parser can't tell it from a real struct), so both
    /// Struct and Param get substituted when bound.
    ///
    /// A concrete generic-struct type (`Option<i32>`, args non-empty) is
    /// monomorphized here: its args are substituted, the instance is requested,
    /// and the mangled flat name (`Option$i32`, no args) is returned. Every
    /// downstream pass therefore only ever sees ordinary no-arg structs.
    ///
    /// FIXME: a real struct named the same as a bound type param gets wrongly
    /// rewritten inside a generic body (e.g. `struct T` used inside `f<T>`). no
    /// scope check distinguishes them. edge case, but there's no guard.
    fn subst_ty(&mut self, ty: &Type<'a>, b: &Bindings<'a>) -> Type<'a> {
        match ty {
            Type::Param(n) if b.types.contains_key(n) => b.types[n].clone(),
            // a bare struct name that's actually a bound type param; substitute it.
            Type::Struct { name, args } if args.is_empty() && b.types.contains_key(name) =>
                b.types[name].clone(),
            // a concrete generic-struct instance: substitute inside its args
            // (types and const values), then mangle + request it, collapsing to the
            // flat instance name.
            Type::Struct { name, args } if !args.is_empty() => {
                let cargs: Vec<ConcreteArg<'a>> = args.iter().map(|a| match a {
                    GenericArg::Type(t) => ConcreteArg::Type(self.subst_ty(t, b)),
                    GenericArg::Const(cv) => ConcreteArg::Const(subst_cv(cv, b).expect_lit()),
                }).collect();
                let mangled = self.request_struct(name, cargs);
                Type::plain_struct(mangled)
            }
            // an ordinary (non-generic) struct: copy through.
            Type::Struct { name, .. } => Type::plain_struct(name),
            Type::Pointer(inner)  => Type::Pointer(Box::new(self.subst_ty(inner, b))),
            Type::Array(inner, n) => Type::Array(Box::new(self.subst_ty(inner, b)), subst_cv(n, b)),
            Type::Slice(inner)    => Type::Slice(Box::new(self.subst_ty(inner, b))),
            Type::Simd(inner, n)  => Type::Simd(Box::new(self.subst_ty(inner, b)), subst_cv(n, b)),
            Type::Function { params, return_type } => Type::Function {
                params: params.iter().map(|p| self.subst_ty(p, b)).collect(),
                return_type: Box::new(self.subst_ty(return_type, b)),
            },
            other => other.clone(),
        }
    }

    /// Substitute bound params in a turbofish argument. A const generic forwarded
    /// by name (`simd_load::<f32, N>`) reaches here as a bare-ident `Type` - but
    /// the name binds in `consts`, not `types` - so resolve it to a literal
    /// `Const` argument; the specialized call then re-typechecks against a
    /// concrete value.
    fn subst_targ(&mut self, ga: &GenericArg<'a>, b: &Bindings<'a>) -> GenericArg<'a> {
        match ga {
            GenericArg::Type(Type::Param(n)) if b.consts.contains_key(n) =>
                GenericArg::Const(ConstVal::Lit(b.consts[n].val)),
            GenericArg::Type(Type::Struct { name, args }) if args.is_empty() && b.consts.contains_key(name) =>
                GenericArg::Const(ConstVal::Lit(b.consts[name].val)),
            GenericArg::Type(t) => GenericArg::Type(self.subst_ty(t, b)),
            GenericArg::Const(cv) => GenericArg::Const(subst_cv(cv, b)),
        }
    }

    fn rebuild_expr(&mut self, expr: &Expr<'a>, b: &Bindings<'a>) -> Expr<'a> {
        let node = match &expr.value {
            ExprNode::Call { func, type_args, args } => {
                let new_args: Vec<Expr<'a>> =
                    args.iter().map(|a| self.rebuild_expr(a, b)).collect();
                // sub type params inside the turbofish (user generic calls +
                // intrinsics like `sizeof::<T>()`)
                let subst_targs: Vec<GenericArg<'a>> =
                    type_args.iter().map(|ga| self.subst_targ(ga, b)).collect();

                // user generic call: mangle to the concrete instance, drop the
                // turbofish.
                if let ExprNode::Var(name) = &func.value {
                    if self.templates.contains_key(name) {
                        let concrete: Vec<ConcreteArg<'a>> = subst_targs.iter().map(|ga| match ga {
                            GenericArg::Type(t) => ConcreteArg::Type(t.clone()),
                            // subst_targ resolved every const param to a literal.
                            GenericArg::Const(cv) => ConcreteArg::Const(cv.expect_lit()),
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
            ExprNode::Struct { name, type_args, fields } => {
                let new_fields: Vec<(&'a str, Expr<'a>)> =
                    fields.iter().map(|(f, e)| (*f, self.rebuild_expr(e, b))).collect();
                if type_args.is_empty() {
                    ExprNode::Struct { name, type_args: Vec::new(), fields: new_fields }
                } else {
                    // a generic struct literal -> its concrete instance. mangle
                    // exactly like the generic *type* `Option<i32>`: substitute the
                    // args, request the instance, swap in the flat name and drop the
                    // turbofish (mil looks the fields up by this name).
                    let concrete = Type::Struct { name, args: type_args.clone() };
                    let Type::Struct { name: mangled, .. } = self.subst_ty(&concrete, b) else {
                        unreachable!("subst_ty of a Struct is always a Struct")
                    };
                    ExprNode::Struct { name: mangled, type_args: Vec::new(), fields: new_fields }
                }
            }
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
            ExprNode::Int8(v) => ExprNode::Int8(*v),
            ExprNode::Int32(v) => ExprNode::Int32(*v),
            ExprNode::Int64(v) => ExprNode::Int64(*v),
            ExprNode::Uint8(v) => ExprNode::Uint8(*v),
            ExprNode::Uint32(v) => ExprNode::Uint32(*v),
            ExprNode::Uint64(v) => ExprNode::Uint64(*v),
            ExprNode::Float32(v) => ExprNode::Float32(*v),
            ExprNode::Float64(v) => ExprNode::Float64(*v),
            ExprNode::Str(s) => ExprNode::Str(s),
            // a bound const param used as a value becomes a typed literal; any
            // other name is copied through.
            ExprNode::Var(name) => match b.consts.get(name) {
                Some(cb) => const_literal(cb),
                None => ExprNode::Var(name),
            },
        };
        Metadata::new(node, expr.span.clone())
    }

    fn rebuild_stmt(&mut self, stmt: &Stmt<'a>, b: &Bindings<'a>) -> Stmt<'a> {
        let node = match &stmt.value {
            StmtNode::Expr(e) => StmtNode::Expr(self.rebuild_expr(e, b)),
            StmtNode::Block(stmts) =>
                StmtNode::Block(stmts.iter().map(|s| self.rebuild_stmt(s, b)).collect()),
            StmtNode::Declare { name, ty, value } => StmtNode::Declare {
                name,
                ty: self.subst_ty(ty, b),
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
        b: &Bindings<'a>,
        name_override: Option<&'a str>,
    ) -> TopLevel<'a> {
        let TopLevelNode::Function { name, attributes, params, return_type, body, .. } = &tl.value
        else { unreachable!("rebuild_function called on a non-function") };

        // any struct instance requested while substituting this function's types
        // reports against the function's span.
        self.cur_span = tl.span.clone();
        let new_params: Vec<(&'a str, Type<'a>)> =
            params.iter().map(|(pn, ty)| (*pn, self.subst_ty(ty, b))).collect();
        let new_return = self.subst_ty(return_type, b);
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

    let struct_templates: HashMap<&'a str, &TopLevel<'a>> = program.iter()
        .filter_map(|tl| match &tl.value {
            TopLevelNode::Struct { name, generics, .. } if !generics.is_empty() =>
                Some((*name, tl)),
            _ => None,
        })
        .collect();

    let mut m = Mono {
        arena, templates, struct_templates,
        queue: VecDeque::new(), seen: HashMap::new(),
        struct_queue: VecDeque::new(), struct_seen: HashMap::new(),
        cur_span: Span::new(String::new(), 0, 0),
        display: HashMap::new(),
    };
    let empty = Bindings::empty();

    // rebuild concrete functions (their call sites seed the queue), keyed by
    // original position so we can re-emit in source order.
    let mut concrete: HashMap<usize, TopLevel<'a>> = HashMap::new();
    for (i, tl) in program.iter().enumerate() {
        match &tl.value {
            TopLevelNode::Function { generics, .. } if !generics.is_empty() => {} // template
            TopLevelNode::Function { .. } => {
                concrete.insert(i, m.rebuild_function(tl, &empty, None));
            }
            TopLevelNode::Extern { .. } | TopLevelNode::Struct { .. }
            | TopLevelNode::Global { .. } => {}
        }
    }

    // build each requested instantiation. instances can request more, so drain
    // till the queue is empty. group by template so they emit next to it.
    // NOTE: the depth/count guards below fire after pop, i.e. after this item was
    // already built+queued - so we overshoot the cutoff by one instance. fine as
    // a backstop, just not a tight bound.
    let mut instances: HashMap<&'a str, Vec<TopLevel<'a>>> = HashMap::new();
    let mut materialized = 0usize;
    while let Some(inst) = m.queue.pop_front() {
        materialized += 1;
        if let Some(deep) = inst.args.iter().find_map(|a| match a {
            ConcreteArg::Type(t) if type_depth(t) > TYPE_DEPTH_LIMIT => Some(t),
            _ => None,
        }) {
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
        // pair each declared generic param with its concrete arg (positional).
        let mut bindings = Bindings::empty();
        for (gp, arg) in generics.iter().zip(&inst.args) {
            match (gp, arg) {
                (GenericParam::Type(n), ConcreteArg::Type(t)) => {
                    bindings.types.insert(n, t.clone());
                }
                (GenericParam::Const(n, ty), ConcreteArg::Const(v)) => {
                    bindings.consts.insert(n, ConstBind { val: *v, ty: ty.clone() });
                }
                _ => unreachable!("generic param/arg kind mismatch - validated in typecheck"),
            }
        }
        let f = m.rebuild_function(tl, &bindings, Some(inst.mangled));
        instances.entry(inst.base).or_default().push(f);
    }

    // build each requested generic-struct instance. substituting a field type can
    // request further instances (`Box<Option<i32>>` needs `Option$i32` too), so
    // drain to fixpoint. the same depth/count guards backstop growing recursion
    // (`struct Bad<T> { p: *Bad<*T> }`). fn instances above already seeded this
    // queue via their param/return/body types.
    let mut struct_instances: HashMap<&'a str, Vec<TopLevel<'a>>> = HashMap::new();
    let mut struct_materialized = 0usize;
    while let Some(inst) = m.struct_queue.pop_front() {
        struct_materialized += 1;
        if let Some(deep) = inst.args.iter().find_map(|a| match a {
            ConcreteArg::Type(t) if type_depth(t) > TYPE_DEPTH_LIMIT => Some(t),
            _ => None,
        }) {
            return Err(Error {
                msg: format!(
                    "monomorphization of struct '{}' produced a type argument nested \
                     deeper than {} (`{}`); this usually means an unbounded generic \
                     struct (one whose field mentions itself at an ever-growing type)",
                    inst.base, TYPE_DEPTH_LIMIT, deep,
                ),
                span: inst.span,
            });
        }
        if struct_materialized > INSTANTIATION_LIMIT {
            return Err(Error {
                msg: format!(
                    "monomorphization exceeded {} struct instantiations at '{}'",
                    INSTANTIATION_LIMIT, inst.base,
                ),
                span: inst.span,
            });
        }
        let tl = m.struct_templates[inst.base];
        let TopLevelNode::Struct { generics, fields, attributes, .. } = &tl.value
        else { unreachable!() };
        // bind each declared param to its concrete arg (positional): type params to
        // types, const params to values (so `[T; N]` fields substitute both).
        let mut bindings = Bindings::empty();
        for (gp, arg) in generics.iter().zip(&inst.args) {
            match (gp, arg) {
                (GenericParam::Type(n), ConcreteArg::Type(t)) => {
                    bindings.types.insert(n, t.clone());
                }
                (GenericParam::Const(n, ty), ConcreteArg::Const(v)) => {
                    bindings.consts.insert(n, ConstBind { val: *v, ty: ty.clone() });
                }
                _ => unreachable!("struct generic param/arg kind mismatch - validated in typecheck"),
            }
        }
        // field types report against this instance's span while being substituted.
        m.cur_span = inst.span.clone();
        let new_fields: Vec<(&'a str, Type<'a>)> = fields.iter()
            .map(|(fname, fty)| (*fname, m.subst_ty(fty, &bindings)))
            .collect();
        let s = Metadata::new(
            TopLevelNode::Struct {
                name: inst.mangled,
                attributes: attributes.clone(),
                generics: Vec::new(),
                fields: new_fields,
            },
            tl.span.clone(),
        );
        struct_instances.entry(inst.base).or_default().push(s);
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
            // a generic struct template drops out (its `Param` fields never lay
            // out), replaced by its concrete instances - emitted here, next to it.
            TopLevelNode::Struct { name, generics, .. } if !generics.is_empty() => {
                if let Some(insts) = struct_instances.remove(name) {
                    output.extend(insts);
                }
            }
            TopLevelNode::Extern { .. } | TopLevelNode::Struct { .. }
            | TopLevelNode::Global { .. } => output.push(tl.clone()),
        }
    }

    Ok((output, m.display))
}
