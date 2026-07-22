use std::collections::{HashMap, HashSet};
use haven_common::ast::*;
use haven_common::layout;
use crate::intrinsics::{Intrinsic, IntrinsicSig, TyConstraint, ConstBound};

/// Field index of the discriminant tag in a data-enum aggregate's synthetic
/// struct, and of the payload byte-blob. Referenced by name in the struct table
/// and by index in MIL lowering (`FieldPtr`).
pub const ENUM_TAG_FIELD: &str = "$tag";
pub const ENUM_PAYLOAD_FIELD: &str = "$payload";

/// The synthetic payload-struct name for a data variant, e.g. `Msg::Note` ->
/// `Msg$Note`. `$` can't appear in a source identifier, so this never collides
/// with a user type. Leaked to `'static` (coerces to any `'a`), as elsewhere in
/// the pipeline - the compiler is a short-lived process.
pub fn enum_payload_struct_name(enum_name: &str, variant: &str) -> &'static str {
    Box::leak(format!("{}${}", enum_name, variant).into_boxed_str())
}

/// Leak a `String` to `'static` (coerces to any `'a`). Used for synthesized
/// tuple-payload field names (`"0"`, `"1"`, ...).
fn leak_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

/// A generic function's signature, in terms of its own type params. param/return
/// types hold `Type::Param`. Used to typecheck calls before mono materializes the
/// concrete instances
#[derive(Clone, Debug)]
pub struct GenericFnSig<'a> {
    /// Generic parameters in declaration order (type and const params
    /// interleaved). Turbofish arguments are matched against this positionally.
    pub generics: Vec<GenericParam<'a>>,
    pub params: Vec<Type<'a>>,
    pub return_type: Type<'a>,
}

#[derive(Clone, Debug)]
pub struct Context<'a> {
    /// Lexical scope stack. Each entry maps a name to its binding identity and
    /// type. The binding is `None` for module-level globals/functions (resolved
    /// via the global namespace in codegen, not a local slot) and `Some` for
    /// params and locals.
    pub scopes: Vec<HashMap<&'a str, (Option<Binding<'a>>, Type<'a>)>>,
    /// Map from Expr/Stmt/TopLevel IDs to their inferred types, for use in later codegen
    pub node_types: HashMap<usize, Type<'a>>,
    /// Name resolution: maps each `Var` use's node id to the specific param/local
    /// binding it refers to. Globals are absent (they fall back to the global
    /// namespace). Consumed by MIL lowering to key variable storage, which makes
    /// shadowing correct - two same-named locals get distinct `Binding::Local`s.
    pub resolved: HashMap<usize, Binding<'a>>,
    /// Struct definitions from name to ordered list of (field name, field type)
    pub structs: HashMap<&'a str, Vec<(&'a str, Type<'a>)>>,
    /// Structs declared with generic parameters (`struct Option<T>`,
    /// `struct Buf<T, const N: u32>`), mapped to their declared params in order
    /// (type and const interleaved). Field types are stored with `Type::Param`s
    /// and `ConstVal::Param`s; a construction/turbofish binds those to concrete
    /// args positionally (the `len()` gives the declared arity). Monomorphization
    /// rewrites a concrete use to a flat instance before codegen.
    pub generic_structs: std::collections::HashMap<&'a str, Vec<GenericParam<'a>>>,
    /// Type-param names in scope for the function being checked, e.g. `["T"]`
    /// inside `proc id<T>(...)`. used to resolve a bare `Type::Struct(name)` into
    /// a `Type::Param(name)`. empty outside generics.
    pub generics: Vec<&'a str>,
    /// Const-param names in scope for the function being checked, e.g. `["N"]`
    /// inside `proc f<const N: u32>(...)`. Used to validate that a `ConstVal::Param`
    /// in a type position names a declared const param. Empty outside generics.
    pub const_generics: Vec<&'a str>,
    /// Generic function signatures, by name. NOT callable via the ordinary
    /// `Type::Function` path; calls go through `check_generic_call`.
    pub generic_fns: HashMap<&'a str, GenericFnSig<'a>>,
    /// Names of module-level constants. Used to reject direct assignment to a
    /// `const` global (they are read-only).
    pub global_consts: std::collections::HashSet<&'a str>,
    /// Declared field-less enums, by name. Each carries the discriminant's
    /// integer repr type (from `@repr(<int>)`, default `i32`) and its variants
    /// mapped to their discriminant values. Used to resolve a `Struct(name)` type
    /// to `Type::Enum` and an `E::V` variant reference to its constant.
    pub enums: HashMap<&'a str, EnumDef<'a>>,
}

/// A declared enum's definition: the discriminant repr, variant discriminant
/// values, and (for data-carrying variants) their payload field types.
#[derive(Clone, Debug)]
pub struct EnumDef<'a> {
    pub repr: Type<'a>,
    pub variants: HashMap<&'a str, i64>,
    /// Payload field types per variant, keyed by variant name. A unit variant has
    /// an empty vec (or no entry). Resolved (enum names rewritten to `Type::Enum`).
    pub payloads: HashMap<&'a str, Vec<Type<'a>>>,
    /// `true` if any variant carries a payload - the enum is then an aggregate
    /// (`Type::Enum { has_payload: true }`) rather than a bare scalar discriminant.
    pub has_payload: bool,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()], // global scope
            node_types: HashMap::new(),
            resolved: HashMap::new(),
            structs: HashMap::new(),
            generic_structs: std::collections::HashMap::new(),
            generics: Vec::new(),
            const_generics: Vec::new(),
            generic_fns: HashMap::new(),
            global_consts: std::collections::HashSet::new(),
            enums: HashMap::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: &'a str, binding: Option<Binding<'a>>, ty: Type<'a>) {
        self.scopes.last_mut().unwrap().insert(name, (binding, ty));
    }

    /// Walk scopes from innermost to outermost, returning the first match for `name`
    pub fn lookup(&self, name: &str) -> Option<&(Option<Binding<'a>>, Type<'a>)> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }
}

/// Resolves a turbofish type argument (generic params → `Type::Param`), checks
/// any referenced structs exist, then checks it against the parameter's kind
/// constraint. Type params pass kind checks optimistically - their kind is only
/// known once a generic function is monomorphized (not yet implemented), and
/// such bodies never reach codegen.
fn check_type_arg<'a>(
    cx: &Context<'a>,
    intrinsic: Intrinsic,
    kind: TyConstraint,
    ty: &Type<'a>,
    span: &Span,
) -> Result<Type<'a>, Error> {
    let ty = resolve_type(&cx.generics, &cx.enums, ty);
    if let Err(msg) = check_type_resolves(cx, &ty) {
        return Err(Error { msg: format!("{}(): {}", intrinsic, msg), span: span.clone() });
    }
    match kind {
        TyConstraint::Any => {}
        TyConstraint::Numeric => {
            if !ty.is_numeric() && !matches!(ty, Type::Param(_)) {
                return Err(Error {
                    msg: format!("{}() expects a numeric type, got `{}`", intrinsic, ty),
                    span: span.clone(),
                });
            }
        }
        TyConstraint::Pointer => {
            if !matches!(ty, Type::Pointer(_) | Type::Param(_)) {
                return Err(Error {
                    msg: format!("{}() expects a pointer type, got `{}`", intrinsic, ty),
                    span: span.clone(),
                });
            }
        }
    }
    Ok(ty)
}

/// Checks a turbofish const argument against the parameter's bound.
fn check_const_arg(
    intrinsic: Intrinsic,
    bound: ConstBound,
    value: i64,
    span: &Span,
) -> Result<usize, Error> {
    if value >= bound.min && value <= bound.max && value % bound.multiple_of as i64 == 0 {
        Ok(value as usize)
    } else {
        let mut msg = format!(
            "{}() const argument must be an integer literal in {}..={}",
            intrinsic, bound.min, bound.max,
        );
        if bound.multiple_of != 1 {
            msg += &format!(" and a multiple of {}", bound.multiple_of);
        }
        Err(Error { msg, span: span.clone() })
    }
}

/// Validates the turbofish and value arities of an intrinsic call against its
/// signature, and binds the type/const arguments. The trailing value arguments
/// stay in `args` and are checked by the caller.
fn bind_generics<'a>(
    cx: &Context<'a>,
    intrinsic: Intrinsic,
    sig: &IntrinsicSig,
    type_args: &[GenericArg<'a>],
    args: &[Expr<'a>],
    span: &Span,
) -> Result<(Vec<Type<'a>>, Vec<ConstVal<'a>>), Error> {
    let n_type = sig.type_params.len();
    let n_const = sig.const_params.len();

    if type_args.len() != n_type + n_const {
        return Err(Error {
            msg: format!(
                "{}() expects {} type argument{} in `::<...>`, got {}",
                intrinsic, n_type + n_const,
                if n_type + n_const == 1 { "" } else { "s" }, type_args.len(),
            ),
            span: span.clone(),
        });
    }
    if args.len() != sig.value_arity {
        return Err(Error {
            msg: format!(
                "{}() takes exactly {} argument{}, got {}",
                intrinsic, sig.value_arity,
                if sig.value_arity == 1 { "" } else { "s" }, args.len(),
            ),
            span: span.clone(),
        });
    }

    let mut tys = Vec::with_capacity(n_type);
    for (i, kind) in sig.type_params.iter().enumerate() {
        match &type_args[i] {
            GenericArg::Type(ty) => tys.push(check_type_arg(cx, intrinsic, *kind, ty, span)?),
            GenericArg::Const(_) => return Err(Error {
                msg: format!("{}() expects a type for type argument {}, got a const", intrinsic, i + 1),
                span: span.clone(),
            }),
        }
    }
    let mut consts = Vec::with_capacity(n_const);
    for (j, bound) in sig.const_params.iter().enumerate() {
        let cv = match &type_args[n_type + j] {
            GenericArg::Const(ConstVal::Lit(n)) => {
                check_const_arg(intrinsic, *bound, *n as i64, span)?;
                ConstVal::Lit(*n)
            }
            // already-symbolic (not produced by the parser today, but kept total)
            GenericArg::Const(ConstVal::Param(name)) => ConstVal::Param(name),
            // a bare ident forwarded from an enclosing `const N` parses as a type;
            // accept it as a symbolic const and leave the range check to the
            // post-mono re-typecheck, when the value is a concrete literal.
            GenericArg::Type(ty) if const_param_name(ty)
                .is_some_and(|n| cx.const_generics.contains(&n)) =>
            {
                ConstVal::Param(const_param_name(ty).unwrap())
            }
            GenericArg::Type(_) => return Err(Error {
                msg: format!("{}() expects a const for type argument {}, got a type", intrinsic, n_type + j + 1),
                span: span.clone(),
            }),
        };
        consts.push(cv);
    }
    Ok((tys, consts))
}

/// If `ty` is a bare identifier (`Type::Struct` before resolution, or `Type::Param`
/// after), returns that name - the two shapes a forwarded const generic parameter
/// can take in a turbofish argument. Compound types are never const params.
fn const_param_name<'a>(ty: &Type<'a>) -> Option<&'a str> {
    match ty {
        // a forwarded const param is a bare ident: a no-arg struct name (or, once
        // resolved, a `Param`). A struct with type args is never a const param.
        Type::Struct { name, args } if args.is_empty() => Some(name),
        Type::Param(name) => Some(name),
        _ => None,
    }
}

fn typecheck_intrinsic<'a>(
    cx: &mut Context<'a>,
    intrinsic: Intrinsic,
    type_args: &[GenericArg<'a>],
    args: &[Expr<'a>],
    span: Span,
    expr_id: usize,
) -> Result<Type<'a>, Error> {
    let sig = intrinsic.signature();
    // validates turbofish/value arity and binds the type/const arguments
    // value arguments remain in `args` (indexed from 0) and are checked per-intrinsic
    let (tys, consts) = bind_generics(cx, intrinsic, &sig, type_args, args, &span)?;

    match intrinsic {
        Intrinsic::Null => {
            // null::<*T>() -> *T. turbofish validated as a pointer by bind_generics
            let target_ty = tys[0].clone();
            cx.node_types.insert(expr_id, target_ty.clone());
            Ok(target_ty)
        }

        // Intrinsic::Len => {
        //     let arg_ty = infer(cx, &args[0])?;
        //     if !matches!(arg_ty, Type::Slice(_) | Type::Array(_, _) | Type::Pointer(_) | Type::Str) {
        //         return Err(Error {
        //             msg: format!("len() expects a slice, array, pointer or str, got {}", arg_ty),
        //             span,
        //         });
        //     }
        //     cx.node_types.insert(expr_id, Type::Int32);
        //     Ok(Type::Int32)
        // }
        Intrinsic::NumericalCast => {
            // numerical_cast::<T>(value) -> T
            let target_ty = tys[0].clone();
            let value_ty = infer(cx, &args[0])?;
            // an enum casts to/from its integer repr, so it is allowed here too.
            if !value_ty.is_numeric() && !matches!(value_ty, Type::Enum { .. }) {
                return Err(Error {
                    msg: format!("numerical_cast() argument must be a numeric type, got {}", value_ty),
                    span,
                });
            }
            cx.node_types.insert(expr_id, target_ty.clone());
            Ok(target_ty)
        }
        Intrinsic::Sizeof => {
            // sizeof::<T>() -> u64. The type argument is validated by bind_generics.
            cx.node_types.insert(expr_id, Type::Uint64);
            Ok(Type::Uint64)
        }
        Intrinsic::PtrCast => {
            // ptr_cast::<*T>(p: *U) -> *T. The turbofish (validated as a pointer
            // by bind_generics) is the result type, the argument must be a pointer
            let target_ty = tys[0].clone();
            let value_ty = infer(cx, &args[0])?;
            if !matches!(value_ty, Type::Pointer(_)) {
                return Err(Error {
                    msg: format!("ptr_cast() argument must be a pointer, got {}", value_ty),
                    span,
                });
            }
            cx.node_types.insert(expr_id, target_ty.clone());
            Ok(target_ty)
        }
        Intrinsic::SimdSplat => {
            let ty = tys[0].clone();
            let size = consts[0].clone();

            let value_ty = infer(cx, &args[0])?;
            if value_ty != ty {
                return Err(Error {
                    msg: format!("simd_splat() value argument must be of the element type, got {}", value_ty),
                    span,
                });
            }

            let simd_ty = Type::Simd(Box::new(ty), size);
            cx.node_types.insert(expr_id, simd_ty.clone());
            Ok(simd_ty)
        }
        Intrinsic::SimdLoad => {
            let ty = tys[0].clone();
            let size = consts[0].clone();

            let slice_ty = infer(cx, &args[0])?;
            let offset_ty = infer(cx, &args[1])?;

            if !offset_ty.is_integer() {
                return Err(Error {
                    msg: format!("simd_load() offset argument must be an integer type, got {}", offset_ty),
                    span,
                });
            }

            match slice_ty {
                Type::Slice(inner) | Type::Pointer(inner) if *inner == ty => {
                    let simd_ty = Type::Simd(Box::new(ty), size);
                    cx.node_types.insert(expr_id, simd_ty.clone());
                    Ok(simd_ty)
                },
                _ => {
                    return Err(Error {
                        msg: format!("simd_load() first argument must be a slice or pointer to the element type, got {}", slice_ty),
                        span,
                    });
                }
            }
        }
        Intrinsic::SimdStore => {
            let ty = tys[0].clone();
            let size = consts[0].clone();

            let slice_ty = infer(cx, &args[0])?;
            let offset_ty = infer(cx, &args[1])?;
            let value_ty = infer(cx, &args[2])?;

            if !offset_ty.is_integer() {
                return Err(Error {
                    msg: format!("simd_store() offset argument must be an integer type, got {}", offset_ty),
                    span,
                });
            }

            let expected_value_ty = Type::Simd(Box::new(ty.clone()), size);
            if value_ty != expected_value_ty {
                return Err(Error {
                    msg: format!("simd_store() value argument must be a SIMD vector of the element type and size, got {}", value_ty),
                    span,
                });
            }

            match slice_ty {
                Type::Slice(inner) | Type::Pointer(inner) if *inner == ty => Ok(Type::Void),
                _ => {
                    return Err(Error {
                        msg: format!("simd_store() first argument must be a slice or pointer to the element type, got {}", slice_ty),
                        span,
                    });
                }
            }
        }
        Intrinsic::SimdConcat => {
            let ty = tys[0].clone();
            let size = consts[0].clone();

            let value1_ty = infer(cx, &args[0])?;
            let value2_ty = infer(cx, &args[1])?;

            // both operands must be half-width vectors of the element type. the
            // exact half-size relation is only statically checkable when the size
            // is a literal; for a symbolic const param we verify the shape and
            // defer the width check to the post-mono re-typecheck.
            let expected_value_ty = match &size {
                ConstVal::Lit(n) => Some(Type::Simd(Box::new(ty.clone()), ConstVal::Lit(n / 2))),
                ConstVal::Param(_) => None,
            };
            let half_ok = |v: &Type<'a>| match &expected_value_ty {
                Some(expected) => v == expected,
                None => matches!(v, Type::Simd(inner, _) if **inner == ty),
            };
            if !half_ok(&value1_ty) {
                return Err(Error {
                    msg: format!("simd_concat() first value argument must be a SIMD vector of the element type and half the size, got {}", value1_ty),
                    span,
                });
            }
            if !half_ok(&value2_ty) {
                return Err(Error {
                    msg: format!("simd_concat() second value argument must be a SIMD vector of the element type and half the size, got {}", value2_ty),
                    span,
                });
            }

            let result_ty = Type::Simd(Box::new(ty.clone()), size.clone());
            cx.node_types.insert(expr_id, result_ty.clone());
            Ok(result_ty)
        }
        Intrinsic::SimdLow | Intrinsic::SimdHigh => {
            // simd_low/high::<T, N>(value: simd[T, M]) -> simd[T, N] where N < M
            let ty = tys[0].clone();
            let size = consts[0].clone();

            let value_ty = infer(cx, &args[0])?;
            // the input must be a wider vector of the element type. `N < M` is only
            // checkable statically when both are literals; a symbolic const param
            // defers the width comparison to the post-mono re-typecheck.
            let wider = |inner_size: &ConstVal<'a>| match (inner_size, &size) {
                (ConstVal::Lit(m), ConstVal::Lit(n)) => m > n,
                _ => true,
            };
            match value_ty {
                Type::Simd(ref inner_ty, ref inner_size) if **inner_ty == ty && wider(inner_size) => {
                    let result_ty = Type::Simd(Box::new(ty.clone()), size.clone());
                    cx.node_types.insert(expr_id, result_ty.clone());
                    Ok(result_ty)
                }
                _ => {
                    return Err(Error {
                        msg: format!("{}() value argument must be a SIMD vector of the element type and larger size, got {}", intrinsic, value_ty),
                        span,
                    });
                }
            }
        }
    }
}

fn check_expr<'a>(
    cx: &mut Context<'a>,
    expected: &Type<'a>,
    expr: &Expr<'a>,
) -> Result<(), Error> {
    let metadata = expr;
    let value = &metadata.value;
    let span = metadata.span.clone();

    let actual = match value {
        ExprNode::Bool(_)    => Type::Bool,
        ExprNode::Int8(_)    => Type::Int8,
        ExprNode::Int32(_)   => Type::Int32,
        ExprNode::Int64(_)   => Type::Int64,
        ExprNode::Uint8(_)   => Type::Uint8,
        ExprNode::Uint32(_)  => Type::Uint32,
        ExprNode::Uint64(_)  => Type::Uint64,
        ExprNode::Float32(_) => Type::Float32,
        ExprNode::Float64(_) => Type::Float64,
        ExprNode::Str(_)     => Type::Str,

        // let xs: []i32 = []; so the type of [] is i32
        // else, if [...] is populated, infer it
        ExprNode::Slice(inner) if inner.len() == 0 => {
            match expected {
                Type::Slice(elem_ty) => Type::Slice(elem_ty.clone()),
                _ => {
                    let msg = format!("Expected {expected}, got []");
                    return Err(Error {
                        msg,
                        span,
                    });
                }
            }
        },

        _ => infer(cx, expr)?,
    };

    let compatible = actual == *expected ||
        matches!((&actual, expected),
            (Type::Array(inner_actual, _), Type::Slice(inner_expected))
                if inner_actual == inner_expected
        );

    if !compatible {
        let msg = format!("Expected {expected}, got {actual}");
        return Err(Error {
            msg,
            span,
        });
    }

    cx.node_types.insert(metadata.id, actual);
    Ok(())
}

fn infer<'a>(
    cx: &mut Context<'a>,
    expr: &Expr<'a>,
) -> Result<Type<'a>, Error> {
    let metadata = expr;
    let value = &metadata.value;
    let span = metadata.span.clone();

    let ty = match value {
        ExprNode::Bool(_)    => Type::Bool,
        ExprNode::Int8(_)    => Type::Int8,
        ExprNode::Int32(_)   => Type::Int32,
        ExprNode::Int64(_)   => Type::Int64,
        ExprNode::Uint8(_)   => Type::Uint8,
        ExprNode::Uint32(_)  => Type::Uint32,
        ExprNode::Uint64(_)  => Type::Uint64,
        ExprNode::Float32(_) => Type::Float32,
        ExprNode::Float64(_) => Type::Float64,
        ExprNode::Str(_)     => Type::Str,

        ExprNode::Var(name) => {
            // an `Enum::Variant` reference. A unit variant is a value: a field-less
            // enum's scalar discriminant, or (for a data enum) an aggregate with no
            // payload. A tuple/struct variant used bare is a missing constructor
            // call - `Msg::Note` needs `Msg::Note(...)`.
            if let Some((ename, _val, repr)) = enum_variant(cx, name) {
                let variant = name.split_once("::").unwrap().1;
                if cx.enums[ename].payloads.get(variant).is_some_and(|p| !p.is_empty()) {
                    return Err(Error {
                        msg: format!("variant '{}' carries a payload; construct it with `{}(...)`", name, name),
                        span,
                    });
                }
                Type::Enum { name: ename, repr: Box::new(repr), has_payload: cx.enums[ename].has_payload }
            } else {
                let Some((binding, ty)) = cx.lookup(name) else {
                    let msg = format!("Undefined variable '{}'", name);
                    return Err(Error {
                        msg,
                        span,
                    });
                };
                let binding = *binding;
                let ty = ty.clone();
                // record which param/local this use resolves to (globals -> None)
                if let Some(b) = binding {
                    cx.resolved.insert(metadata.id, b);
                }
                ty
            }
        },

        ExprNode::Slice(inner) if inner.len() == 0 => {
            return Err(Error {
                msg: "Cannot infer type of empty slice literal".to_string(),
                span,
            });
        },

        // Infer [...] as Array first, convert to slice later if needed
        ExprNode::Slice(inner) => {
            let first_ty = infer(cx, &inner[0])?;
            for elem in inner.iter().skip(1) {
                check_expr(cx, &first_ty, elem)?;
            }
            Type::Array(Box::new(first_ty), ConstVal::Lit(inner.len()))
        },
        // ExprNode::Slice(inner) => {
        //     let first_ty = infer(cx, &inner[0])?;
        //     for elem in inner.iter().skip(1) {
        //         check_expr(cx, &first_ty, elem)?;
        //     }
        //     Type::Slice(Box::new(first_ty))
        // },

        ExprNode::Index { slice, index } => {
            let slice_ty = infer(cx, slice)?;
            let index_ty = infer(cx, index)?;
            if !index_ty.is_integer() {
                let msg = format!(
                    "Expected index of type i32, got {}",
                    index_ty
                );
                return Err(Error {
                    msg,
                    span,
                });
            }
            match slice_ty {
                Type::Slice(inner)
                | Type::Array(inner, _)
                | Type::Pointer(inner) => *inner,
                _ => {
                    let msg = format!(
                        "Expected a slice or pointer type for indexing, got {}",
                        slice_ty
                    );
                    return Err(Error {
                        msg,
                        span,
                    });
                }
            }
        },

        ExprNode::Unary { op, operand } => {
            let operand_ty = infer(cx, operand)?;
            match op {
                UnaryOp::AddrOf => Type::Pointer(Box::new(operand_ty)),
                UnaryOp::Deref => match operand_ty {
                    Type::Pointer(inner) => *inner,
                    _ => {
                        let msg = format!(
                            "Expected a pointer type for dereference, got {}",
                            operand_ty
                        );
                        return Err(Error {
                            msg,
                            span,
                        });
                    }
                },
                UnaryOp::Neg => if operand_ty.is_numeric() {
                    operand_ty
                } else {
                    let msg = format!(
                        "Expected a numeric type for negation, got {}",
                        operand_ty
                    );
                    return Err(Error {
                        msg,
                        span,
                    });
                },
                UnaryOp::Not => Type::Bool,
            }
        },

        ExprNode::Binary { op, left, right } => {
            use haven_common::ast::BinaryOp::*;
            match op {
                Eq | Ne => {
                    let left_ty = infer(cx, left)?;
                    check_expr(cx, &left_ty, right)?;
                    Type::Bool
                },
                Add | Sub | Mul | Div | Mod
                | Lt | Gt | Le | Ge => {
                    let left_ty = infer(cx, left)?;
                    check_expr(cx, &left_ty, right)?;

                    // check if both are numeric (scalar or SIMD)
                    if !left_ty.is_numeric_or_numeric_simd() {
                        let msg = format!(
                            "Expected a numeric type for binary operator, got {}",
                            left_ty
                        );
                        return Err(Error { msg, span });
                    }

                    match op {
                        Add | Sub | Mul | Div | Mod => left_ty, // returns scalar or SIMD
                        Lt | Gt | Le | Ge => Type::Bool,
                        _ => unreachable!(),
                    }
                },
                And | Or => {
                    let expected = Type::Bool;
                    check_expr(cx, &expected, left)?;
                    check_expr(cx, &expected, right)?;
                    Type::Bool
                },
                // bitwise and shifts: integer operands, same type on both sides
                // (LLVM requires the shift amount to match the value type), result
                // is that integer type.
                BitAnd | BitOr | BitXor | Shl | Shr => {
                    let left_ty = infer(cx, left)?;
                    check_expr(cx, &left_ty, right)?;
                    if !left_ty.is_integer() {
                        let msg = format!(
                            "Expected an integer type for bitwise operator '{}', got {}",
                            op, left_ty
                        );
                        return Err(Error { msg, span });
                    }
                    left_ty
                },
            }
        },

        ExprNode::Struct { name, type_args, fields } => {
            let def = match cx.structs.get(name) {
                Some(d) => d.clone(),
                None => return Err(Error {
                    msg: format!("Unknown struct '{}'", name),
                    span,
                }),
            };

            // Bind the struct's params to the turbofish args so `Param` field types
            // check against a concrete type (and `[T; N]` sizes against a concrete
            // count). Non-generic structs take no args; a generic struct requires
            // them (no context inference yet). `struct_args` are the resolved args
            // in declaration order.
            let (type_subst, const_subst, struct_args) = match cx.generic_structs.get(name).cloned() {
                Some(params) => {
                    if type_args.is_empty() {
                        return Err(Error {
                            msg: format!(
                                "constructing generic struct '{}' needs type arguments, e.g. `{}::<...> {{ ... }}`",
                                name, name,
                            ),
                            span,
                        });
                    }
                    bind_struct_generics(cx, name, &params, type_args, &span)?
                }
                None => {
                    if !type_args.is_empty() {
                        return Err(Error {
                            msg: format!(
                                "struct '{}' is not generic; no type arguments expected",
                                name,
                            ),
                            span,
                        });
                    }
                    (HashMap::new(), HashMap::new(), Vec::new())
                }
            };

            if fields.len() != def.len() {
                return Err(Error {
                    msg: format!(
                        "Struct '{}' expects {} fields, got {}",
                        name, def.len(), fields.len()
                    ),
                    span,
                });
            }

            // field order must match definition; each field checks against its
            // (param-substituted) declared type.
            for ((def_name, def_ty), (lit_name, lit_value)) in def.iter().zip(fields.iter()) {
                if def_name != lit_name {
                    return Err(Error {
                        msg: format!(
                            "In struct '{}': expected field '{}', got '{}'",
                            name, def_name, lit_name
                        ),
                        span: lit_value.span.clone(),
                    });
                }
                let expected = subst_param_type(&type_subst, &const_subst, def_ty);
                check_expr(cx, &expected, lit_value)?;
            }

            // The literal's type carries its concrete args (`Option<i32>`,
            // `Buf<i32, 8>`); monomorphization rewrites both this literal and the
            // type to the flat instance before codegen. Non-generic structs get the
            // usual no-args `Struct`.
            Type::Struct { name, args: struct_args }
        },

        ExprNode::Access { base, field } => {
            let base_ty = infer(cx, base)?;
            let (struct_name, struct_args) = match &base_ty {
                Type::Struct { name, args } => (*name, args.as_slice()),
                // auto-deref one level of pointer to a struct (like C's `->`)
                Type::Pointer(inner) => match inner.as_ref() {
                    Type::Struct { name, args } => (*name, args.as_slice()),
                    _ => return Err(Error {
                        msg: format!("Cannot access field '{}' on type {}", field, base_ty),
                        span,
                    }),
                },
                _ => return Err(Error {
                    msg: format!("Cannot access field '{}' on type {}", field, base_ty),
                    span,
                }),
            };

            let def = match cx.structs.get(struct_name) {
                Some(d) => d,
                None => return Err(Error {
                    msg: format!("Unknown struct '{}'", struct_name),
                    span,
                }),
            };

            let field_ty = match def.iter().find(|(n, _)| n == field) {
                Some((_, ty)) => ty.clone(),
                None => return Err(Error {
                    msg: format!("Struct '{}' has no field '{}'", struct_name, field),
                    span,
                }),
            };
            // for a generic-struct instance (`Option<i32>`, `Buf<i32, 8>`), the
            // field type is stored with `Param`s (`value: T`, `[T; N]`); substitute
            // the struct's args so the access yields the concrete field type. mono
            // later flattens both.
            if struct_args.is_empty() {
                field_ty
            } else {
                let mut type_subst: HashMap<&'a str, Type<'a>> = HashMap::new();
                let mut const_subst: HashMap<&'a str, usize> = HashMap::new();
                if let Some(params) = cx.generic_structs.get(struct_name) {
                    for (gp, ga) in params.iter().zip(struct_args.iter()) {
                        match (gp, ga) {
                            (GenericParam::Type(n), GenericArg::Type(t)) => { type_subst.insert(*n, t.clone()); }
                            (GenericParam::Const(n, _), GenericArg::Const(ConstVal::Lit(v))) => { const_subst.insert(*n, *v); }
                            _ => {}
                        }
                    }
                }
                subst_param_type(&type_subst, &const_subst, &field_ty)
            }
        },

        ExprNode::Call { func, type_args, args }
            if matches!(&func.value, ExprNode::Var(name)
                if Intrinsic::lookup(name).is_some()) => {
            let ExprNode::Var(name) = &func.value else { unreachable!() };
            let intrinsic = Intrinsic::lookup(name).unwrap();
            typecheck_intrinsic(cx, intrinsic, type_args, args, span, metadata.id)?
        },
        ExprNode::Call { func, type_args, args } => {
            // a data-enum constructor `E::V(args...)` looks like a call but names
            // no function; check arity + each arg against the payload field types
            // and yield the aggregate enum type. Guarded before ordinary dispatch.
            if let ExprNode::Var(cname) = &func.value {
                if let Some((ename, payload_tys)) = enum_variant_ctor(cx, cname) {
                    if !type_args.is_empty() {
                        return Err(Error {
                            msg: format!("enum constructor '{}' takes no type arguments", cname),
                            span,
                        });
                    }
                    if args.len() != payload_tys.len() {
                        return Err(Error {
                            msg: format!("variant '{}' expects {} field(s), got {}",
                                cname, payload_tys.len(), args.len()),
                            span,
                        });
                    }
                    for (pty, arg) in payload_tys.iter().zip(args.iter()) {
                        check_expr(cx, pty, arg)?;
                    }
                    let repr = cx.enums[ename].repr.clone();
                    let ty = Type::Enum { name: ename, repr: Box::new(repr), has_payload: cx.enums[ename].has_payload };
                    cx.node_types.insert(expr.id, ty.clone());
                    return Ok(ty);
                }
            }
            // user generic call (`foo::<T>(...)`) check against the generic sig
            // mono emits the instance later.
            // TODO: `.cloned()` copies the whole sig on every generic call site
            // (twice, since we typecheck again after mono) just to dodge the
            // borrow of cx
            if let ExprNode::Var(name) = &func.value {
                if let Some(sig) = cx.generic_fns.get(*name).cloned() {
                    let name = *name;
                    let ty = check_generic_call(cx, name, &sig, type_args, args, &span)?;
                    cx.node_types.insert(expr.id, ty.clone());
                    return Ok(ty);
                }
            }
            if !type_args.is_empty() {
                // a turbofished name that reached here is neither a generic fn (the
                // `generic_fns` check above) nor an intrinsic (its own arm). Before
                // blaming the turbofish syntax, check whether the name resolves at
                // all: an unresolved one is almost always an undefined or, more
                // often, un-imported symbol (imports aren't re-exported), which the
                // syntax message hides
                if let ExprNode::Var(name) = &func.value {
                    if cx.lookup(name).is_none() {
                        return Err(Error {
                            msg: format!(
                                "unknown function '{}', is it defined and imported into this module?",
                                name,
                            ),
                            span,
                        });
                    }
                }
                return Err(Error {
                    msg: "type arguments (`::<...>`) are only valid on generic or intrinsic calls".into(),
                    span,
                });
            }
            let callee_ty = infer(cx, func)?;
            match callee_ty {
                Type::Function { params, return_type } => {
                    if params.len() != args.len() {
                        let msg = format!(
                            "Expected {} arguments, got {}",
                            params.len(),
                            args.len()
                        );
                        return Err(Error {
                            msg,
                            span,
                        });
                    }

                    for (param_ty, arg_expr) in params.iter().zip(args.iter()) {
                        check_expr(cx, param_ty, arg_expr)?;
                    }

                    *return_type
                }
                _ => {
                    let msg = format!("Expected a function, got {}", callee_ty);
                    return Err(Error {
                        msg,
                        span,
                    });
                }
            }
        },
    };

    cx.node_types.insert(expr.id, ty.clone());
    Ok(ty)
}

/// Whether executing `stmt` guarantees control flow never falls through to the
/// statement after it - i.e. it returns (or diverges) on every path. Used to
/// verify that a non-void function cannot reach the end of its body without
/// returning a value.
///
/// Conservative on loops: a `while` is assumed to possibly run zero times, so it
/// never guarantees a return (not even `while (true)`, since there's no
/// break analysis), and `break`/`continue` count as fall-through.
fn always_returns(stmt: &Stmt) -> bool {
    match &stmt.value {
        StmtNode::Return(_) => true,
        // a block returns if any statement in it returns (anything after the
        // first returning statement is dead, which is fine for this check)
        StmtNode::Block(stmts) => stmts.iter().any(always_returns),
        // an `if` guarantees a return only with an `else` where BOTH branches
        // return; a bare `if` falls through when the condition is false.
        StmtNode::If { then_branch, else_branch, .. } => match else_branch {
            Some(else_branch) => always_returns(then_branch) && always_returns(else_branch),
            None => false,
        },
        // a match is exhaustive (typecheck guarantees it), so it returns on every
        // path iff every arm body does.
        StmtNode::Match { arms, .. } => !arms.is_empty() && arms.iter().all(|(_, body)| always_returns(body)),
        _ => false,
    }
}

fn check_stmt<'a>(
    cx: &mut Context<'a>,
    return_ty: &Type<'a>,
    stmt: &Stmt<'a>,
) -> Result<(), Error> {
    match &stmt.value {
        StmtNode::Expr(expr) => {
            infer(cx, expr)?;
        },

        StmtNode::Block(stmts) => {
            cx.push_scope();
            for stmt in stmts {
                check_stmt(cx, return_ty, stmt)?;
            }
            cx.pop_scope();
        },

        StmtNode::Declare { name, ty, value } => {
            if let Err(msg) = check_const_scope(&cx.const_generics, ty) {
                return Err(Error { msg: format!("in declaration of '{}': {}", name, msg), span: stmt.span.clone() });
            }
            let ty = resolve_type(&cx.generics, &cx.enums, ty);
            check_expr(cx, &ty, value)?;
            // the local's binding identity is this Declare stmt's node id, which
            // is globally unique - so shadowed same-named locals stay distinct.
            cx.insert(name, Some(Binding::Local(stmt.id)), ty);
        },

        StmtNode::Assign { left, value } => {
            // a `const` global is read-only: reject a direct `GLOBAL = ...`.
            // (mutating through a pointer/field is still the pointee's business.)
            if let ExprNode::Var(name) = &left.value {
                if cx.global_consts.contains(name) {
                    return Err(Error {
                        msg: format!("cannot assign to constant global '{}'", name),
                        span: left.span.clone(),
                    });
                }
            }
            let left_ty = infer(cx, left)?;
            check_expr(cx, &left_ty, value)?;
        },

        StmtNode::If { condition, then_branch, else_branch } => {
            check_expr(cx, &Type::Bool, condition)?;

            cx.push_scope();
            check_stmt(cx, return_ty, then_branch)?;
            cx.pop_scope();

            if let Some(else_branch) = else_branch {
                cx.push_scope();
                check_stmt(cx, return_ty, else_branch)?;
                cx.pop_scope();
            }
        },

        StmtNode::While { condition, body } => {
            check_expr(cx, &Type::Bool, condition)?;

            cx.push_scope();
            check_stmt(cx, return_ty, body)?;
            cx.pop_scope();
        },

        StmtNode::Match { scrutinee, arms } => {
            let scrut_ty = infer(cx, scrutinee)?;
            // the scrutinee must be an enum or an integer.
            let enum_name: Option<&'a str> = match &scrut_ty {
                Type::Enum { name, .. } => Some(name),
                t if t.is_integer() => None,
                other => return Err(Error {
                    msg: format!("match scrutinee must be an enum or integer type, got {}", other),
                    span: scrutinee.span.clone(),
                }),
            };

            let mut has_wildcard = false;
            let mut covered_variants: HashSet<&'a str> = HashSet::new();
            let mut covered_ints: HashSet<i64> = HashSet::new();

            for (pat, body) in arms {
                if has_wildcard {
                    return Err(Error { msg: "unreachable match arm after `_`".into(), span: pat.span.clone() });
                }
                // payload bindings this arm introduces into its own scope.
                let mut arm_bindings: Vec<(&'a str, Binding<'a>, Type<'a>)> = Vec::new();
                match &pat.value {
                    PatternNode::Wildcard => has_wildcard = true,
                    PatternNode::Int(n) => {
                        if let Some(en) = enum_name {
                            return Err(Error { msg: format!("integer pattern in a match on enum '{}'", en), span: pat.span.clone() });
                        }
                        if !covered_ints.insert(*n) {
                            return Err(Error { msg: format!("duplicate match arm for `{}`", n), span: pat.span.clone() });
                        }
                    }
                    PatternNode::Path(p) => {
                        let Some(en) = enum_name else {
                            return Err(Error { msg: format!("enum-variant pattern `{}` in a match on integer type", p), span: pat.span.clone() });
                        };
                        let variant = check_variant_pattern(cx, en, *p, &pat.span)?;
                        // a bare `E::V` on a data variant would leave the payload
                        // unbound - require the destructuring form `E::V(..)`.
                        let arity = cx.enums[en].payloads.get(variant).map_or(0, |p| p.len());
                        if arity != 0 {
                            return Err(Error {
                                msg: format!("variant `{}` carries {} field(s); destructure it as `{}(..)`", p, arity, p),
                                span: pat.span.clone(),
                            });
                        }
                        if !covered_variants.insert(variant) {
                            return Err(Error { msg: format!("duplicate match arm for `{}`", p), span: pat.span.clone() });
                        }
                    }
                    PatternNode::Variant { path, fields } => {
                        let Some(en) = enum_name else {
                            return Err(Error { msg: format!("enum-variant pattern `{}` in a match on integer type", path), span: pat.span.clone() });
                        };
                        let variant = check_variant_pattern(cx, en, *path, &pat.span)?;
                        let payload = cx.enums[en].payloads.get(variant).cloned().unwrap_or_default();
                        if fields.len() != payload.len() {
                            return Err(Error {
                                msg: format!("variant `{}` has {} field(s) but the pattern binds {}",
                                    path, payload.len(), fields.len()),
                                span: pat.span.clone(),
                            });
                        }
                        if !covered_variants.insert(variant) {
                            return Err(Error { msg: format!("duplicate match arm for `{}`", path), span: pat.span.clone() });
                        }
                        for (fpat, fty) in fields.iter().zip(payload.iter()) {
                            match &fpat.value {
                                PatternNode::Wildcard => {}
                                // each `Bind` is keyed by its own node id (its
                                // binding identity), so shadowing/reuse is distinct.
                                PatternNode::Bind(bname) =>
                                    arm_bindings.push((*bname, Binding::Local(fpat.id), fty.clone())),
                                other => return Err(Error {
                                    msg: format!("unsupported payload sub-pattern `{}`", other),
                                    span: fpat.span.clone(),
                                }),
                            }
                        }
                    }
                    PatternNode::Bind(name) => return Err(Error {
                        msg: format!("bare binding `{}` is not a valid match pattern; bindings appear inside a variant destructure", name),
                        span: pat.span.clone(),
                    }),
                }
                cx.push_scope();
                for (bname, binding, bty) in &arm_bindings {
                    cx.insert(bname, Some(*binding), bty.clone());
                }
                check_stmt(cx, return_ty, body)?;
                cx.pop_scope();
            }

            // exhaustiveness: an enum needs every variant or a `_`; an integer
            // scrutinee always needs a `_` (its domain can't be enumerated).
            if !has_wildcard {
                match enum_name {
                    Some(en) => {
                        let mut missing: Vec<&str> = cx.enums[en].variants.keys()
                            .filter(|v| !covered_variants.contains(**v)).cloned().collect();
                        if !missing.is_empty() {
                            missing.sort();
                            return Err(Error {
                                msg: format!("non-exhaustive match on enum '{}': missing {}; cover them or add a `_` arm",
                                    en, missing.join(", ")),
                                span: stmt.span.clone(),
                            });
                        }
                    }
                    None => return Err(Error {
                        msg: "non-exhaustive match on an integer type: add a `_` arm".into(),
                        span: stmt.span.clone(),
                    }),
                }
            }
        },

        StmtNode::Continue | StmtNode::Break => {
            // nothing to check
        },

        StmtNode::Return(expr) => {
            check_expr(cx, return_ty, expr)?;
        },
    }

    Ok(())
}

/// Check whether a type is allowed in an @export function signature.
/// Some types are not allowed because they have unknown layout or calling convention,
/// or I just don't know how to handle it.
fn check_export_type<'a>(
    ty: &Type<'a>,
    structs: &HashMap<&'a str, Vec<(&'a str, Type<'a>)>>,
) -> Result<(), String> {
    match ty {
        // TODO check if this is correct
        Type::Array(inner, _) =>
            Err(format!("fixed-size array type '[{}; N]' is not allowed in @export functions, use a raw pointer '*{}' and an explicit length parameter instead", inner, inner)),
        Type::Slice(inner) =>
            Err(format!("slice type '{}' is not allowed in @export functions, use a raw pointer '*{}' and an explicit length parameter instead", ty, inner)),
        // `str` is a raw `*const u8` (a C string) - a single machine pointer,
        // so it is ABI-stable and maps directly to C's `const char*`.
        Type::Str => Ok(()),
        // A pointer is a single machine word regardless of what it points to, so
        // it is ABI-stable as an opaque handle even when the pointee's layout is
        // opaque to C (e.g. `*State`, `*u8`, `**u8`). We still reject pointers to
        // the genuinely fat / target-specific pointees (slice/simd/array), whose
        // *value* representation isn't a plain pointer. `*str` is fine - it is a
        // pointer to a pointer (`char**`).
        Type::Pointer(inner) => match &**inner {
            Type::Struct { .. }
            | Type::Void | Type::Bool
            | Type::Int8 | Type::Int32 | Type::Int64
            | Type::Uint8 | Type::Uint32 | Type::Uint64
            | Type::Float32 | Type::Float64
            | Type::Str
            | Type::Pointer(_) => Ok(()),
            _ => check_export_type(inner, structs), // *[]f32, *simd<...> stay banned
        },
        Type::Simd(_, _) =>
            Err(format!("SIMD type '{}' is not allowed in @export functions because its calling convention is target-specific and not guaranteed to match the expected caller, or that's what I'm told", ty)),
        Type::Function { .. } =>
            Err("function pointer types are not supported in @export functions".into()),
        // A by-value struct is now ABI-lowered (SysV eightbyte classification),
        // so it may cross the FFI boundary as long as every field is itself
        // export-safe. Recurse so a struct hiding a slice/str/array is rejected.
        Type::Struct { name, .. } => {
            let fields = structs.get(name)
                .ok_or_else(|| format!("unknown struct '{}'", name))?;
            for (fname, fty) in fields {
                check_export_type(fty, structs)
                    .map_err(|e| format!("field '{}' of struct '{}': {}", fname, name, e))?;
            }
            Ok(())
        }
        Type::Param(_) =>
            Err("generic type parameters are not allowed in @export functions".into()),
        // a field-less enum is its integer repr across FFI - a plain C enum. A
        // data-carrying enum's `@repr(C)` tagged-union ABI is deferred (Stage-3
        // Phase 4), so it can't cross `@export`/`extern` yet.
        Type::Enum { has_payload: false, .. } => Ok(()),
        Type::Enum { has_payload: true, name, .. } =>
            Err(format!("data-carrying enum '{}' cannot cross an @export/extern boundary yet", name)),
        // these are all fine across FFI
        Type::Void | Type::Bool
        | Type::Int8 | Type::Int32 | Type::Int64
        | Type::Uint8 | Type::Uint32 | Type::Uint64
        | Type::Float32 | Type::Float64 => Ok(()),
    }
}

/// Verify that a global's initializer is a compile-time constant we can emit as
/// an LLVM `constant` aggregate: literals (optionally negated), struct literals of
/// constants, and bare function names (a function's address is a link-time
/// constant). Anything that would need to run code - a call, a load of another
/// global's value, indexing - is rejected.
fn check_const_initializer<'a>(cx: &Context<'a>, expr: &Expr<'a>) -> Result<(), Error> {
    match &expr.value {
        ExprNode::Bool(_)
        | ExprNode::Int8(_) | ExprNode::Int32(_) | ExprNode::Int64(_)
        | ExprNode::Uint8(_) | ExprNode::Uint32(_) | ExprNode::Uint64(_)
        | ExprNode::Float32(_) | ExprNode::Float64(_) => Ok(()),
        // a negated numeric literal, e.g. `-1.0`, is still a constant
        ExprNode::Unary { op: UnaryOp::Neg, operand }
            if matches!(operand.value,
                ExprNode::Int8(_) | ExprNode::Int32(_) | ExprNode::Int64(_)
                | ExprNode::Uint8(_) | ExprNode::Uint32(_) | ExprNode::Uint64(_)
                | ExprNode::Float32(_) | ExprNode::Float64(_)) => Ok(()),
        // an `Enum::Variant` is a compile-time integer constant.
        ExprNode::Var(name) if enum_variant(cx, name).is_some() => Ok(()),
        // a bare top-level function name: its address is a link-time constant.
        // a *global* of function type is excluded - reading its value isn't const.
        ExprNode::Var(name)
            if matches!(cx.lookup(name), Some((_, Type::Function { .. })))
                && !cx.global_consts.contains(name) => Ok(()),
        // a struct literal is constant iff every field initializer is constant
        // (nested structs recurse). field names/types are checked by check_expr.
        ExprNode::Struct { fields, .. } => {
            for (_, fexpr) in fields {
                check_const_initializer(cx, fexpr)?;
            }
            Ok(())
        }
        // an array literal is constant iff every element is.
        ExprNode::Slice(elements) => {
            for elem in elements {
                check_const_initializer(cx, elem)?;
            }
            Ok(())
        }
        _ => Err(Error {
            msg: "global initializer must be a constant (a literal, a struct/array literal of constants, or a function name)".into(),
            span: expr.span.clone(),
        }),
    }
}

fn check_toplevel<'a>(
    cx: &mut Context<'a>,
    node: &TopLevel<'a>,
) -> Result<(), Error> {
    match &node.value {
        TopLevelNode::Function { name, attributes, generics, params, return_type, body, .. } => {
            if attributes.iter().any(|a| a.value.name == "export") {
                // monomorphization isn't implemented yet, so a generic function
                // has no single concrete ABI to export
                if !generics.is_empty() {
                    return Err(Error {
                        msg: format!("@export function '{}' cannot be generic", name),
                        span: node.span.clone(),
                    });
                }
                for (param_name, ty) in params {
                    // resolve enum names first (@export can't be generic) so an
                    // enum param is checked as its integer repr, not an unknown struct.
                    let ty = resolve_type(&[], &cx.enums, ty);
                    if let Err(msg) = check_export_type(&ty, &cx.structs) {
                        return Err(Error {
                            msg: format!("parameter '{}' in @export function '{}': {}", param_name, name, msg),
                            span: node.span.clone(),
                        });
                    }
                }
                let return_type = resolve_type(&[], &cx.enums, return_type);
                if let Err(msg) = check_export_type(&return_type, &cx.structs) {
                    return Err(Error {
                        msg: format!("return type in @export function '{}': {}", name, msg),
                        span: node.span.clone(),
                    });
                }
            }

            // bind type params for the duration of this function so that bare
            // idents in the signature/body resolve to `Type::Param` rather than
            // an (undeclared) struct
            cx.generics = generics.iter().filter_map(|g| match g {
                GenericParam::Type(n) => Some(*n),
                GenericParam::Const(_, _) => None,
            }).collect();
            cx.const_generics = generics.iter().filter_map(|g| match g {
                GenericParam::Const(n, _) => Some(*n),
                GenericParam::Type(_) => None,
            }).collect();

            // every `ConstVal::Param` in the signature must name a declared const
            // param. runs for non-generic fns too (empty scope), so a stray
            // `[f32; N]` outside a generic is rejected rather than silently
            // producing an unresolved param.
            for (pname, ty) in params {
                if let Err(msg) = check_const_scope(&cx.const_generics, ty) {
                    return Err(Error {
                        msg: format!("parameter '{}' of '{}': {}", pname, name, msg),
                        span: node.span.clone(),
                    });
                }
            }
            if let Err(msg) = check_const_scope(&cx.const_generics, return_type) {
                return Err(Error {
                    msg: format!("return type of '{}': {}", name, msg),
                    span: node.span.clone(),
                });
            }

            let return_ty = resolve_type(&cx.generics, &cx.enums, return_type);
            if let Err(msg) = check_type_resolves(cx, &return_ty) {
                return Err(Error {
                    msg: format!("return type of '{}': {}", name, msg),
                    span: node.span.clone(),
                });
            }

            cx.push_scope();

            // const generics are ordinary compile-time values, visible in the body
            for g in generics {
                if let GenericParam::Const(cname, cty) = g {
                    // const generics are substituted with literals by mono, so
                    // they never surface as `Var` uses in the lowered program.
                    cx.insert(cname, None, cty.clone());
                }
            }

            // push params into scope, resolving type-param references
            for (pname, ty) in params {
                let resolved = resolve_type(&cx.generics, &cx.enums, ty);
                if let Err(msg) = check_type_resolves(cx, &resolved) {
                    return Err(Error {
                        msg: format!("parameter '{}' of '{}': {}", pname, name, msg),
                        span: node.span.clone(),
                    });
                }
                cx.insert(pname, Some(Binding::Param(pname)), resolved);
            }

            for stmt in body {
                check_stmt(cx, &return_ty, stmt)?;
            }

            // a non-void function must return on every path, or control can fall
            // off the end with no value. structural over the body, so it also
            // covers generic templates (return_type may still hold `Param`s).
            if return_ty != Type::Void && !body.iter().any(always_returns) {
                cx.pop_scope();
                cx.generics = Vec::new();
                cx.const_generics = Vec::new();
                return Err(Error {
                    msg: format!(
                        "function '{}' has return type '{}' but not all paths return a value",
                        name, return_type
                    ),
                    span: node.span.clone(),
                });
            }

            cx.pop_scope();
            cx.generics = Vec::new();
            cx.const_generics = Vec::new();
        }

        // extern declarations have no body to check, but their signature types
        // must still resolve (so an unknown or not-yet-supported generic struct
        // type is caught at typecheck, not by a codegen panic).
        TopLevelNode::Extern { name, generics, params, return_type, .. } => {
            let type_params: Vec<&'a str> = generics.iter().filter_map(|g| match g {
                GenericParam::Type(n) => Some(*n),
                GenericParam::Const(_, _) => None,
            }).collect();
            for (pname, ty) in params {
                let resolved = resolve_type(&type_params, &cx.enums, ty);
                if let Err(msg) = check_type_resolves(cx, &resolved) {
                    return Err(Error {
                        msg: format!("parameter '{}' of extern '{}': {}", pname, name, msg),
                        span: node.span.clone(),
                    });
                }
            }
            let resolved_ret = resolve_type(&type_params, &cx.enums, return_type);
            if let Err(msg) = check_type_resolves(cx, &resolved_ret) {
                return Err(Error {
                    msg: format!("return type of extern '{}': {}", name, msg),
                    span: node.span.clone(),
                });
            }
        }

        TopLevelNode::Struct { name, generics, fields, .. } => {
            // the struct's own type and const params are in scope inside its fields:
            // `T` resolves to `Param`, and a `[T; N]` size names the const param `N`.
            let type_params: Vec<&'a str> = generics.iter().filter_map(|g| match g {
                GenericParam::Type(n) => Some(*n),
                GenericParam::Const(_, _) => None,
            }).collect();
            let const_params: Vec<&'a str> = generics.iter().filter_map(|g| match g {
                GenericParam::Const(n, _) => Some(*n),
                GenericParam::Type(_) => None,
            }).collect();
            // ensure no duplicate field names and that referenced struct types exist
            let mut seen = std::collections::HashSet::new();
            for (field_name, field_ty) in fields {
                if !seen.insert(*field_name) {
                    return Err(Error {
                        msg: format!("Duplicate field '{}' in struct '{}'", field_name, name),
                        span: node.span.clone(),
                    });
                }
                // resolve the struct's own type params to `Param` first, so they
                // aren't reported as unknown struct names.
                let resolved = resolve_type(&type_params, &cx.enums, field_ty);
                if let Err(msg) = check_type_resolves(cx, &resolved) {
                    return Err(Error {
                        msg: format!("In field '{}' of struct '{}': {}", field_name, name, msg),
                        span: node.span.clone(),
                    });
                }
                // every `ConstVal::Param` in the field (an `[T; N]` size) must name
                // one of the struct's declared const params.
                if let Err(msg) = check_const_scope(&const_params, &resolved) {
                    return Err(Error {
                        msg: format!("In field '{}' of struct '{}': {}", field_name, name, msg),
                        span: node.span.clone(),
                    });
                }
            }
        }

        TopLevelNode::Global { name, ty, value, .. } => {
            if let Err(msg) = check_type_resolves(cx, ty) {
                return Err(Error {
                    msg: format!("In global '{}': {}", name, msg),
                    span: node.span.clone(),
                });
            }
            check_const_initializer(cx, value)?;
            check_expr(cx, ty, value)?;
        }
        // field-less enums are fully validated in the forward-declaration pass
        // (duplicate variants, `@repr` value); nothing more to check here.
        TopLevelNode::Enum { .. } => {}
    }

    Ok(())
}

/// The discriminant repr type for an enum, from its `@repr(<int>)` attribute.
/// Defaults to `i32` (C `int`); `@repr(C)` is an explicit spelling of that.
/// haven has no 16-bit integer, so `u16`/`i16` are not accepted yet.
fn enum_repr<'a>(attributes: &[Attribute<'a>]) -> Result<Type<'a>, String> {
    for a in attributes {
        if a.value.name == "repr" {
            return match a.value.value.as_deref() {
                None | Some("C") | Some("i32") => Ok(Type::Int32),
                Some("i8")  => Ok(Type::Int8),
                Some("i64") => Ok(Type::Int64),
                Some("u8")  => Ok(Type::Uint8),
                Some("u32") => Ok(Type::Uint32),
                Some("u64") => Ok(Type::Uint64),
                Some(other) => Err(format!(
                    "unknown @repr('{}') on enum; expected an integer type \
                     (i8/i32/i64/u8/u32/u64) or C", other)),
            };
        }
    }
    Ok(Type::Int32)
}

/// If `name` is an `Enum::Variant` reference to a declared enum, return the
/// enum's name, the variant's discriminant value, and the discriminant repr.
/// The enum name comes from the table key so it carries the `'a` lifetime.
fn enum_variant<'a>(cx: &Context<'a>, name: &str) -> Option<(&'a str, i64, Type<'a>)> {
    let (ename, variant) = name.split_once("::")?;
    let (&ekey, def) = cx.enums.get_key_value(ename)?;
    let val = *def.variants.get(variant)?;
    Some((ekey, val, def.repr.clone()))
}

/// Validate that `path` (a joined `E::V`) names a variant of enum `en`; returns
/// the variant name. Shared by the field-less `Path` and the destructuring
/// `Variant` match-arm patterns.
fn check_variant_pattern<'a>(cx: &Context<'a>, en: &'a str, path: &'a str, span: &Span)
-> Result<&'a str, Error> {
    let (pat_enum, variant) = path.split_once("::")
        .ok_or_else(|| Error { msg: format!("invalid enum pattern `{}`", path), span: span.clone() })?;
    if pat_enum != en {
        return Err(Error { msg: format!("pattern `{}` is not a variant of enum '{}'", path, en), span: span.clone() });
    }
    if !cx.enums[en].variants.contains_key(variant) {
        return Err(Error { msg: format!("enum '{}' has no variant '{}'", en, variant), span: span.clone() });
    }
    Ok(variant)
}

/// If `name` is `E::V` naming a variant of a declared enum, returns the enum's
/// name and the variant's payload field types (empty for a unit variant). Used
/// to recognize a constructor call `E::V(...)` in `infer`/`lower_expr`.
fn enum_variant_ctor<'a>(cx: &Context<'a>, name: &str) -> Option<(&'a str, Vec<Type<'a>>)> {
    let (ename, variant) = name.split_once("::")?;
    let (&ekey, def) = cx.enums.get_key_value(ename)?;
    if !def.variants.contains_key(variant) { return None; }
    Some((ekey, def.payloads.get(variant).cloned().unwrap_or_default()))
}

/// Rewrites a bare named type into a `Type::Param` (generic param in scope),
/// `Type::Enum` (declared enum), or leaves it a `Type::Struct`, recursing
/// through compound types. The parser can't tell these apart (all bare idents),
/// so this resolution happens once the generic list and enum table are known.
fn resolve_type<'a>(generics: &[&'a str], enums: &HashMap<&'a str, EnumDef<'a>>, ty: &Type<'a>) -> Type<'a> {
    match ty {
        // a bare `T` that names an in-scope param becomes `Param`; any other named
        // type keeps its name but has its generic args resolved recursively (a
        // struct arg can itself mention `T`, e.g. `Option<T>`).
        Type::Struct { name, args } if args.is_empty() && generics.contains(name) => Type::Param(name),
        // a bare name that is a declared enum becomes `Type::Enum`, with the
        // discriminant repr baked in so later stages need no enum table.
        Type::Struct { name, args } if args.is_empty() && enums.contains_key(name) =>
            Type::Enum { name, repr: Box::new(enums[name].repr.clone()), has_payload: enums[name].has_payload },
        Type::Struct { name, args } => Type::Struct {
            name,
            // only type args can name a type param; const args pass through.
            args: args.iter().map(|a| match a {
                GenericArg::Type(t) => GenericArg::Type(resolve_type(generics, enums, t)),
                GenericArg::Const(_) => a.clone(),
            }).collect(),
        },
        Type::Pointer(inner)  => Type::Pointer(Box::new(resolve_type(generics, enums, inner))),
        Type::Array(inner, n) => Type::Array(Box::new(resolve_type(generics, enums, inner)), n.clone()),
        Type::Slice(inner)    => Type::Slice(Box::new(resolve_type(generics, enums, inner))),
        Type::Simd(inner, n)  => Type::Simd(Box::new(resolve_type(generics, enums, inner)), n.clone()),
        Type::Function { params, return_type } => Type::Function {
            params: params.iter().map(|p| resolve_type(generics, enums, p)).collect(),
            return_type: Box::new(resolve_type(generics, enums, return_type)),
        },
        other => other.clone(),
    }
}

/// Substitute `Type::Param(name)` with its concrete binding, recursing through
/// compound types. inverse of `resolve_type`: used when a generic sig (which
/// holds `Param`s) is specialized at a call site.
// TODO: this, resolve_type, and mono.rs::subst_ty are three near-identical walks
// over the same compound-type arms, so maybe in the future it could be generalized
// into a single `Type::walk_mut` or `Type::map` function that takes a closure to
// apply to each leaf type
fn subst_param_type<'a>(
    types: &HashMap<&'a str, Type<'a>>,
    consts: &HashMap<&'a str, usize>,
    ty: &Type<'a>,
) -> Type<'a> {
    let sub_cv = |cv: &ConstVal<'a>| match cv {
        ConstVal::Param(n) => consts.get(n).map(|v| ConstVal::Lit(*v)).unwrap_or_else(|| cv.clone()),
        ConstVal::Lit(_) => cv.clone(),
    };
    match ty {
        Type::Param(name) => types.get(name).cloned().unwrap_or_else(|| ty.clone()),
        // a generic struct instance can mention params in its args (`Option<T>`,
        // `Buf<T, N>`); recurse so both type and const args get substituted.
        Type::Struct { name, args } => Type::Struct {
            name,
            args: args.iter().map(|a| match a {
                GenericArg::Type(t) => GenericArg::Type(subst_param_type(types, consts, t)),
                GenericArg::Const(cv) => GenericArg::Const(sub_cv(cv)),
            }).collect(),
        },
        Type::Pointer(inner)  => Type::Pointer(Box::new(subst_param_type(types, consts, inner))),
        Type::Array(inner, n) => Type::Array(Box::new(subst_param_type(types, consts, inner)), sub_cv(n)),
        Type::Slice(inner)    => Type::Slice(Box::new(subst_param_type(types, consts, inner))),
        Type::Simd(inner, n)  => Type::Simd(Box::new(subst_param_type(types, consts, inner)), sub_cv(n)),
        Type::Function { params, return_type } => Type::Function {
            params: params.iter().map(|p| subst_param_type(types, consts, p)).collect(),
            return_type: Box::new(subst_param_type(types, consts, return_type)),
        },
        other => other.clone(),
    }
}

/// Typecheck a call to a user generic function: bind the turbofish type args to
/// the callee's type params, substitute them into the sig, check the value args,
/// and return the substituted result type. mono materializes the instance later.
fn check_generic_call<'a>(
    cx: &mut Context<'a>,
    name: &'a str,
    sig: &GenericFnSig<'a>,
    type_args: &[GenericArg<'a>],
    args: &[Expr<'a>],
    span: &Span,
) -> Result<Type<'a>, Error> {
    if type_args.len() != sig.generics.len() {
        return Err(Error {
            msg: format!(
                "{}() expects {} generic argument{} in `::<...>`, got {}",
                name, sig.generics.len(),
                if sig.generics.len() == 1 { "" } else { "s" }, type_args.len(),
            ),
            span: span.clone(),
        });
    }

    // bind each turbofish arg to its generic param, matched positionally.
    let mut type_bindings: HashMap<&'a str, Type<'a>> = HashMap::new();
    let mut const_bindings: HashMap<&'a str, usize> = HashMap::new();
    for (gp, ta) in sig.generics.iter().zip(type_args) {
        match (gp, ta) {
            (GenericParam::Type(pname), GenericArg::Type(ty)) => {
                // resolve against the caller's own type params (a generic body
                // can forward its `T`), then check any structs exist.
                let ty = resolve_type(&cx.generics, &cx.enums, ty);
                if let Err(msg) = check_type_resolves(cx, &ty) {
                    return Err(Error { msg: format!("{}(): {}", name, msg), span: span.clone() });
                }
                type_bindings.insert(pname, ty);
            }
            (GenericParam::Const(pname, _), GenericArg::Const(ConstVal::Lit(v))) => {
                const_bindings.insert(pname, *v);
            }
            // forwarding a const generic by name into another generic function
            // isn't supported yet: const bindings must reduce to a concrete value
            // here (they're substituted, not kept symbolic like intrinsic args).
            (GenericParam::Const(pname, _), GenericArg::Const(ConstVal::Param(fwd))) => return Err(Error {
                msg: format!("{}(): forwarding const parameter '{}' to '{}' is not supported yet", name, fwd, pname),
                span: span.clone(),
            }),
            (GenericParam::Type(pname), GenericArg::Const(_)) => return Err(Error {
                msg: format!("{}(): expected a type argument for '{}', got a const value", name, pname),
                span: span.clone(),
            }),
            (GenericParam::Const(pname, _), GenericArg::Type(ty)) => {
                // a bare-ident turbofish arg that names an in-scope const param is
                // a (currently unsupported) forward; otherwise it's a real type in
                // a const slot.
                if const_param_name(ty).is_some_and(|n| cx.const_generics.contains(&n)) {
                    return Err(Error {
                        msg: format!("{}(): forwarding const parameter '{}' to '{}' is not supported yet",
                            name, const_param_name(ty).unwrap(), pname),
                        span: span.clone(),
                    });
                }
                return Err(Error {
                    msg: format!("{}(): expected a const argument for '{}', got a type", name, pname),
                    span: span.clone(),
                });
            }
        }
    }

    let params: Vec<Type<'a>> = sig.params.iter()
        .map(|p| subst_param_type(&type_bindings, &const_bindings, p)).collect();
    let return_type = subst_param_type(&type_bindings, &const_bindings, &sig.return_type);

    if args.len() != params.len() {
        return Err(Error {
            msg: format!("{}() expects {} argument{}, got {}",
                name, params.len(), if params.len() == 1 { "" } else { "s" }, args.len()),
            span: span.clone(),
        });
    }
    for (param_ty, arg) in params.iter().zip(args) {
        check_expr(cx, param_ty, arg)?;
    }

    Ok(return_type)
}

/// Bind a generic struct's applied args to its declared params (`Buf<i32, 8>` ->
/// `{T: i32}`, `{N: 8}`), validating arity and each arg's kind. Type args are
/// resolved against the enclosing function's own generics and checked to exist;
/// const args must be concrete literals. Returns the type/const substitutions
/// plus the resolved args (for the resulting `Type::Struct`). Mirrors
/// `check_generic_call`'s binding, for the struct case.
fn bind_struct_generics<'a>(
    cx: &Context<'a>,
    name: &str,
    params: &[GenericParam<'a>],
    args: &[GenericArg<'a>],
    span: &Span,
) -> Result<(HashMap<&'a str, Type<'a>>, HashMap<&'a str, usize>, Vec<GenericArg<'a>>), Error> {
    if args.len() != params.len() {
        return Err(Error {
            msg: format!(
                "struct '{}' expects {} type argument{}, got {}",
                name, params.len(),
                if params.len() == 1 { "" } else { "s" }, args.len(),
            ),
            span: span.clone(),
        });
    }
    let mut type_subst: HashMap<&'a str, Type<'a>> = HashMap::new();
    let mut const_subst: HashMap<&'a str, usize> = HashMap::new();
    let mut resolved: Vec<GenericArg<'a>> = Vec::with_capacity(params.len());
    for (gp, ga) in params.iter().zip(args) {
        match (gp, ga) {
            (GenericParam::Type(pname), GenericArg::Type(ty)) => {
                let ty = resolve_type(&cx.generics, &cx.enums, ty);
                if let Err(msg) = check_type_resolves(cx, &ty) {
                    return Err(Error { msg: format!("struct '{}': {}", name, msg), span: span.clone() });
                }
                type_subst.insert(pname, ty.clone());
                resolved.push(GenericArg::Type(ty));
            }
            (GenericParam::Const(pname, _), GenericArg::Const(ConstVal::Lit(v))) => {
                const_subst.insert(pname, *v);
                resolved.push(GenericArg::Const(ConstVal::Lit(*v)));
            }
            (GenericParam::Const(pname, _), GenericArg::Const(ConstVal::Param(fwd))) => return Err(Error {
                msg: format!("struct '{}': forwarding const parameter '{}' to '{}' is not supported yet", name, fwd, pname),
                span: span.clone(),
            }),
            (GenericParam::Type(pname), GenericArg::Const(_)) => return Err(Error {
                msg: format!("struct '{}': expected a type argument for '{}', got a const value", name, pname),
                span: span.clone(),
            }),
            (GenericParam::Const(pname, _), GenericArg::Type(ty)) => {
                // a bare-ident arg (`N`) parses as a type; if it names an in-scope
                // const param it's a (currently unsupported) forward, else it's a
                // real type wrongly placed in a const slot.
                if const_param_name(ty).is_some_and(|n| cx.const_generics.contains(&n)) {
                    return Err(Error {
                        msg: format!("struct '{}': forwarding const parameter '{}' to '{}' is not supported yet",
                            name, const_param_name(ty).unwrap(), pname),
                        span: span.clone(),
                    });
                }
                return Err(Error {
                    msg: format!("struct '{}': expected a const argument for '{}', got a type", name, pname),
                    span: span.clone(),
                });
            }
        }
    }
    Ok((type_subst, const_subst, resolved))
}

/// Verifies that every `ConstVal::Param` in `ty` names a const generic parameter
/// in `in_scope`. Ignores type params/structs entirely - those are handled by
/// `resolve_type`/`check_type_resolves` - so it can run on raw (unresolved) types.
fn check_const_scope<'a>(in_scope: &[&'a str], ty: &Type<'a>) -> Result<(), String> {
    fn check_cv<'a>(in_scope: &[&'a str], cv: &ConstVal<'a>) -> Result<(), String> {
        match cv {
            ConstVal::Param(n) if !in_scope.contains(n) =>
                Err(format!("unknown const parameter '{}'", n)),
            _ => Ok(()),
        }
    }
    match ty {
        Type::Array(inner, n) | Type::Simd(inner, n) => {
            check_cv(in_scope, n)?;
            check_const_scope(in_scope, inner)
        }
        Type::Pointer(inner) | Type::Slice(inner) => check_const_scope(in_scope, inner),
        Type::Function { params, return_type } => {
            for p in params { check_const_scope(in_scope, p)?; }
            check_const_scope(in_scope, return_type)
        }
        _ => Ok(()),
    }
}

/// Recursively verifies that every Type::Struct referenced in each Type exists
/// in cx.structs.
fn check_type_resolves<'a>(cx: &Context<'a>, ty: &Type<'a>) -> Result<(), String> {
    match ty {
        Type::Struct { name, args } => {
            if !cx.structs.contains_key(name) {
                return Err(format!("unknown type '{}'", name));
            }
            // recurse into type arguments (`Option<Unknown>` must still error).
            for a in args {
                if let GenericArg::Type(t) = a { check_type_resolves(cx, t)?; }
            }
            // validate the applied argument count against the struct's declared
            // arity: 0 for a non-generic struct, `params.len()` for a generic one.
            let params = cx.generic_structs.get(name);
            let arity = params.map_or(0, |p| p.len());
            if args.len() != arity {
                return Err(if arity == 0 {
                    format!("struct '{}' is not generic; no type arguments expected", name)
                } else {
                    format!(
                        "struct '{}' expects {} type argument{}, got {}",
                        name, arity,
                        if arity == 1 { "" } else { "s" }, args.len(),
                    )
                });
            }
            // each applied arg's kind must match its param (type vs const);
            // forwarding a const param into a struct type isn't supported yet.
            if let Some(params) = params {
                for (gp, ga) in params.iter().zip(args) {
                    match (gp, ga) {
                        (GenericParam::Type(_), GenericArg::Type(_)) => {}
                        (GenericParam::Const(_, _), GenericArg::Const(ConstVal::Lit(_))) => {}
                        (GenericParam::Const(pn, _), GenericArg::Const(ConstVal::Param(f))) =>
                            return Err(format!("struct '{}': forwarding const parameter '{}' to '{}' is not supported yet", name, f, pn)),
                        (GenericParam::Type(pn), GenericArg::Const(_)) =>
                            return Err(format!("struct '{}': expected a type argument for '{}', got a const value", name, pn)),
                        (GenericParam::Const(pn, _), GenericArg::Type(t)) => {
                            if const_param_name(t).is_some_and(|n| cx.const_generics.contains(&n)) {
                                return Err(format!("struct '{}': forwarding const parameter '{}' to '{}' is not supported yet", name, const_param_name(t).unwrap(), pn));
                            }
                            return Err(format!("struct '{}': expected a const argument for '{}', got a type", name, pn));
                        }
                    }
                }
            }
            // A concrete generic-struct use (`Option<i32>`, `Buf<i32, 8>`) is now
            // valid: monomorphization rewrites it to a flat instance before any
            // later stage.
            Ok(())
        }
        Type::Pointer(inner)
        | Type::Array(inner, _)
        | Type::Slice(inner)
        | Type::Simd(inner, _) => check_type_resolves(cx, inner),
        Type::Function { params, return_type } => {
            for p in params { check_type_resolves(cx, p)?; }
            check_type_resolves(cx, return_type)
        }
        _ => Ok(()),
    }
}

pub fn typecheck_program<'a>(cx: &mut Context<'a>, program: &[TopLevel<'a>]) -> Vec<Error> {
    let mut errors = Vec::new();

    // --- forward declaration pass

    // enums come first: a struct field or function parameter may name an enum
    // type, and `resolve_type` needs the enum table to rewrite `Struct(name)`
    // into `Type::Enum`.
    for node in program {
        if let TopLevelNode::Enum { name, attributes, variants, .. } = &node.value {
            if cx.enums.contains_key(name) || cx.structs.contains_key(name) {
                errors.push(Error {
                    msg: format!("Duplicate type definition '{}'", name),
                    span: node.span.clone(),
                });
                continue;
            }
            let repr = match enum_repr(attributes) {
                Ok(r) => r,
                Err(msg) => { errors.push(Error { msg, span: node.span.clone() }); continue; }
            };
            // C-style discriminants: an unspecified variant is the previous one
            // plus one, starting at 0. Payload field types are collected here but
            // resolved against the enum table in a later pass (below), once every
            // enum is known.
            let mut vmap = HashMap::new();
            let mut payloads: HashMap<&str, Vec<Type>> = HashMap::new();
            let mut next: i64 = 0;
            let mut dup = false;
            let mut has_payload = false;
            for (vname, explicit, payload) in variants {
                let val = explicit.unwrap_or(next);
                if vmap.insert(*vname, val).is_some() {
                    errors.push(Error {
                        msg: format!("Duplicate variant '{}' in enum '{}'", vname, name),
                        span: node.span.clone(),
                    });
                    dup = true;
                    break;
                }
                if !payload.is_empty() {
                    has_payload = true;
                    payloads.insert(*vname, payload.iter().map(|(_, ty)| ty.clone()).collect());
                }
                next = val + 1;
            }
            if dup { continue; }
            cx.enums.insert(name, EnumDef { repr, variants: vmap, payloads, has_payload });
        }
    }

    // forward declare structs so that they can be referenced in function signatures
    for node in program {
        if let TopLevelNode::Struct { name, generics, fields, .. } = &node.value {
            if cx.structs.contains_key(name) || cx.enums.contains_key(name) {
                errors.push(Error {
                    msg: format!("Duplicate type definition '{}'", name),
                    span: node.span.clone(),
                });
                continue;
            }
            // resolve field types against the struct's own type params so a field
            // referencing `T` is stored as `Type::Param(T)`, not a bogus struct
            // name. (const params already arrive as `ConstVal::Param` from the
            // parser. Body-level validation happens in the main pass below.)
            let type_params: Vec<&'a str> = generics.iter().filter_map(|g| match g {
                GenericParam::Type(n) => Some(*n),
                GenericParam::Const(_, _) => None,
            }).collect();
            let resolved_fields = fields.iter()
                .map(|(fname, fty)| (*fname, resolve_type(&type_params, &cx.enums, fty)))
                .collect();
            cx.structs.insert(name, resolved_fields);
            if !generics.is_empty() {
                cx.generic_structs.insert(name, generics.clone());
            }
        }
    }

    // data enums: now that every enum and struct is declared, resolve payload
    // field types and register the synthetic structs that back the aggregate.
    // For `enum Msg { Note(u8, f32) }` this registers a payload struct
    // `Msg$Note = { "0": u8, "1": f32 }` and the aggregate `Msg = { $tag: repr,
    // $payload: [P x i8] }` where P is the largest variant payload. These reuse
    // the whole struct machinery (layout, FieldPtr, AllocaStruct, copy_struct,
    // sret) so the backend needs almost no new aggregate code.
    let data_enums: Vec<&'a str> = cx.enums.iter()
        .filter(|(_, d)| d.has_payload).map(|(n, _)| *n).collect();
    // pass 1: register every payload struct and stash the resolved payloads.
    for ename in &data_enums {
        let raw: Vec<(&'a str, Vec<Type<'a>>)> = cx.enums[ename].payloads.iter()
            .map(|(v, tys)| (*v, tys.clone())).collect();
        let mut resolved: HashMap<&'a str, Vec<Type<'a>>> = HashMap::new();
        for (vname, tys) in raw {
            let rtys: Vec<Type<'a>> = tys.iter().map(|t| resolve_type(&[], &cx.enums, t)).collect();
            let fields: Vec<(&'a str, Type<'a>)> = rtys.iter().enumerate()
                .map(|(i, t)| (leak_str(i.to_string()), t.clone()))
                .collect();
            cx.structs.insert(enum_payload_struct_name(ename, vname), fields);
            resolved.insert(vname, rtys);
        }
        if let Some(def) = cx.enums.get_mut(ename) { def.payloads = resolved; }
    }
    // pass 2: register each aggregate, sizing its byte blob from the (now present)
    // payload structs. Separate pass so a payload struct is always in the table.
    for ename in &data_enums {
        let repr = cx.enums[ename].repr.clone();
        let payload_bytes = cx.enums[ename].payloads.keys()
            .map(|v| layout::size_of(&Type::plain_struct(enum_payload_struct_name(ename, v)), &cx.structs))
            .max().unwrap_or(0);
        let agg_fields = vec![
            (ENUM_TAG_FIELD, repr),
            (ENUM_PAYLOAD_FIELD, Type::Array(Box::new(Type::Int8), ConstVal::Lit(payload_bytes))),
        ];
        cx.structs.insert(ename, agg_fields);
    }

    // forward declare functions so that they can be called before their definition
    for node in program {
        match &node.value {
            // generic functions go into a separate table; not callable via the
            // ordinary function-type path, only through turbofish.
            TopLevelNode::Function { name, generics, params, return_type, .. }
                if !generics.is_empty() => {
                let type_params: Vec<&'a str> = generics.iter().filter_map(|g| match g {
                    GenericParam::Type(n) => Some(*n),
                    GenericParam::Const(_, _) => None,
                }).collect();
                // only type params get reclassified `Struct`->`Param`; const params
                // already arrive as `ConstVal::Param` from the parser.
                let resolved_params = params.iter()
                    .map(|(_, ty)| resolve_type(&type_params, &cx.enums, ty))
                    .collect();
                let resolved_return = resolve_type(&type_params, &cx.enums, return_type);
                cx.generic_fns.insert(name, GenericFnSig {
                    generics: generics.clone(),
                    params: resolved_params,
                    return_type: resolved_return,
                });
            }
            // generic externs (`extern printf<T>(...)`) are the same story as
            // generic functions: they live in `generic_fns` and are only reachable
            // through turbofish, never via the ordinary function-type path. codegen
            // still emits the single underlying C symbol (mono keeps the name, drops
            // the turbofish), so there's nothing to monomorphize.
            TopLevelNode::Extern { name, generics, params, return_type, .. }
                if !generics.is_empty() => {
                let type_params: Vec<&'a str> = generics.iter().filter_map(|g| match g {
                    GenericParam::Type(n) => Some(*n),
                    GenericParam::Const(_, _) => None,
                }).collect();
                let resolved_params = params.iter()
                    .map(|(_, ty)| resolve_type(&type_params, &cx.enums, ty))
                    .collect();
                let resolved_return = resolve_type(&type_params, &cx.enums, return_type);
                cx.generic_fns.insert(name, GenericFnSig {
                    generics: generics.clone(),
                    params: resolved_params,
                    return_type: resolved_return,
                });
            }
            TopLevelNode::Function { name, params, return_type, .. }
            | TopLevelNode::Extern { name, params, return_type, .. } => {
                // resolve enum-typed params/return (no generics here) so the stored
                // signature uses `Type::Enum`, matching how a call's args infer;
                // otherwise an enum param stays `Struct` and mismatches the arg.
                cx.insert(name, None, Type::Function {
                    params: params.iter().map(|(_, ty)| resolve_type(&[], &cx.enums, ty)).collect(),
                    return_type: Box::new(resolve_type(&[], &cx.enums, return_type)),
                });
            }
            // globals share the value namespace with functions; register the name
            // so references resolve as ordinary variables.
            TopLevelNode::Global { name, ty, .. } => {
                cx.insert(name, None, ty.clone());
                cx.global_consts.insert(name);
            }
            // enums were collected in their own pass above; nothing to register
            // in the value namespace (variant refs resolve directly).
            TopLevelNode::Struct { .. } | TopLevelNode::Enum { .. } => {}
        }
    }

    // --- typecheck pass
    for node in program {
        if let Err(err) = check_toplevel(cx, node) {
            errors.push(err);
        }
    }

    errors
}