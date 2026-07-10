use std::collections::HashMap;
use crate::{front::ast::*, intrinsics::{Intrinsic, IntrinsicSig, TyConstraint, ConstBound}};

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
    /// shadowing correct — two same-named locals get distinct `Binding::Local`s.
    pub resolved: HashMap<usize, Binding<'a>>,
    /// Struct definitions from name to ordered list of (field name, field type)
    pub structs: HashMap<&'a str, Vec<(&'a str, Type<'a>)>>,
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
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()], // global scope
            node_types: HashMap::new(),
            resolved: HashMap::new(),
            structs: HashMap::new(),
            generics: Vec::new(),
            const_generics: Vec::new(),
            generic_fns: HashMap::new(),
            global_consts: std::collections::HashSet::new(),
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
/// constraint. Type params pass kind checks optimistically — their kind is only
/// known once a generic function is monomorphized (not yet implemented), and
/// such bodies never reach codegen.
fn check_type_arg<'a>(
    cx: &Context<'a>,
    intrinsic: Intrinsic,
    kind: TyConstraint,
    ty: &Type<'a>,
    span: &Span,
) -> Result<Type<'a>, Error> {
    let ty = resolve_type(&cx.generics, ty);
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
/// after), returns that name — the two shapes a forwarded const generic parameter
/// can take in a turbofish argument. Compound types are never const params.
fn const_param_name<'a>(ty: &Type<'a>) -> Option<&'a str> {
    match ty {
        Type::Struct(name) | Type::Param(name) => Some(name),
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
        Intrinsic::Len => {
            let arg_ty = infer(cx, &args[0])?;
            if !matches!(arg_ty, Type::Slice(_) | Type::Array(_, _) | Type::Pointer(_) | Type::Str) {
                return Err(Error {
                    msg: format!("len() expects a slice, array, pointer or str, got {}", arg_ty),
                    span,
                });
            }
            cx.node_types.insert(expr_id, Type::Int32);
            Ok(Type::Int32)
        }
        Intrinsic::NumericalCast => {
            // numerical_cast::<T>(value) -> T
            let target_ty = tys[0].clone();
            let value_ty = infer(cx, &args[0])?;
            if !value_ty.is_numeric() {
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
            use crate::front::ast::BinaryOp::*;
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
                Xor => {
                    let expected = Type::Int32;
                    check_expr(cx, &expected, left)?;
                    check_expr(cx, &expected, right)?;
                    Type::Int32
                },
            }
        },

        ExprNode::Struct { name, fields } => {
            let def = match cx.structs.get(name) {
                Some(d) => d.clone(),
                None => return Err(Error {
                    msg: format!("Unknown struct '{}'", name),
                    span,
                }),
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

            // field order must match definition
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
                check_expr(cx, def_ty, lit_value)?;
            }

            Type::Struct(name)
        },

        ExprNode::Access { base, field } => {
            let base_ty = infer(cx, base)?;
            let struct_name = match &base_ty {
                Type::Struct(n) => *n,
                // auto-deref one level of pointer to a struct (like C's `->`)
                Type::Pointer(inner) => match inner.as_ref() {
                    Type::Struct(n) => *n,
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

            match def.iter().find(|(n, _)| n == field) {
                Some((_, ty)) => ty.clone(),
                None => return Err(Error {
                    msg: format!("Struct '{}' has no field '{}'", struct_name, field),
                    span,
                }),
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
/// statement after it — i.e. it returns (or diverges) on every path. Used to
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
            let ty = resolve_type(&cx.generics, ty);
            check_expr(cx, &ty, value)?;
            // the local's binding identity is this Declare stmt's node id, which
            // is globally unique — so shadowed same-named locals stay distinct.
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
        Type::Str =>
            Err("str type is not allowed in @export functions (its fat-pointer layout is not stable across FFI), pass a raw '*u8' pointer and an explicit length parameter instead".into()),
        // A pointer is a single machine word regardless of what it points to, so
        // it is ABI-stable as an opaque handle even when the pointee's layout is
        // opaque to C (e.g. `*State`, `*u8`, `**u8`). We still reject pointers to
        // the genuinely fat / target-specific pointees (slice/str/simd/array),
        // whose *value* representation isn't a plain pointer.
        Type::Pointer(inner) => match &**inner {
            Type::Struct(_)
            | Type::Void | Type::Bool
            | Type::Int8 | Type::Int32 | Type::Int64
            | Type::Uint8 | Type::Uint32 | Type::Uint64
            | Type::Float32 | Type::Float64
            | Type::Pointer(_) => Ok(()),
            _ => check_export_type(inner, structs), // *[]f32, *str, *simd<...> stay banned
        },
        Type::Simd(_, _) =>
            Err(format!("SIMD type '{}' is not allowed in @export functions because its calling convention is target-specific and not guaranteed to match the expected caller, or that's what I'm told", ty)),
        Type::Function { .. } =>
            Err("function pointer types are not supported in @export functions".into()),
        // A by-value struct is now ABI-lowered (SysV eightbyte classification),
        // so it may cross the FFI boundary as long as every field is itself
        // export-safe. Recurse so a struct hiding a slice/str/array is rejected.
        Type::Struct(name) => {
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
/// constant). Anything that would need to run code — a call, a load of another
/// global's value, indexing — is rejected.
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
        // a bare top-level function name: its address is a link-time constant.
        // a *global* of function type is excluded — reading its value isn't const.
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
        _ => Err(Error {
            msg: "global initializer must be a constant (a literal, a struct literal of constants, or a function name)".into(),
            span: expr.span.clone(),
        }),
    }
}

fn check_toplevel<'a>(
    cx: &mut Context<'a>,
    node: &TopLevel<'a>,
) -> Result<(), Error> {
    match &node.value {
        TopLevelNode::Function { name, attributes, generics, params, return_type, body } => {
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
                    if let Err(msg) = check_export_type(ty, &cx.structs) {
                        return Err(Error {
                            msg: format!("parameter '{}' in @export function '{}': {}", param_name, name, msg),
                            span: node.span.clone(),
                        });
                    }
                }
                if let Err(msg) = check_export_type(return_type, &cx.structs) {
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

            let return_ty = resolve_type(&cx.generics, return_type);

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
                let resolved = resolve_type(&cx.generics, ty);
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

        // extern declarations have no body to check
        TopLevelNode::Extern { .. } => {}

        TopLevelNode::Struct { name, fields, .. } => {
            // ensure no duplicate field names and that referenced struct types exist
            let mut seen = std::collections::HashSet::new();
            for (field_name, field_ty) in fields {
                if !seen.insert(*field_name) {
                    return Err(Error {
                        msg: format!("Duplicate field '{}' in struct '{}'", field_name, name),
                        span: node.span.clone(),
                    });
                }
                if let Err(msg) = check_type_resolves(cx, field_ty) {
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
    }

    Ok(())
}

/// Rewrites every `Type::Struct(name)` whose name is a generic type parameter
/// in scope into a `Type::Param(name)`, recursing through compound types. The
/// parser can't tell a struct name from a type param (both are bare idents), so
/// this resolution happens once the function's generic list is known.
fn resolve_type<'a>(generics: &[&'a str], ty: &Type<'a>) -> Type<'a> {
    match ty {
        Type::Struct(name) if generics.contains(name) => Type::Param(name),
        Type::Pointer(inner)  => Type::Pointer(Box::new(resolve_type(generics, inner))),
        Type::Array(inner, n) => Type::Array(Box::new(resolve_type(generics, inner)), n.clone()),
        Type::Slice(inner)    => Type::Slice(Box::new(resolve_type(generics, inner))),
        Type::Simd(inner, n)  => Type::Simd(Box::new(resolve_type(generics, inner)), n.clone()),
        Type::Function { params, return_type } => Type::Function {
            params: params.iter().map(|p| resolve_type(generics, p)).collect(),
            return_type: Box::new(resolve_type(generics, return_type)),
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
                let ty = resolve_type(&cx.generics, ty);
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

/// Verifies that every `ConstVal::Param` in `ty` names a const generic parameter
/// in `in_scope`. Ignores type params/structs entirely — those are handled by
/// `resolve_type`/`check_type_resolves` — so it can run on raw (unresolved) types.
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
        Type::Struct(name) => {
            if cx.structs.contains_key(name) {
                Ok(())
            } else {
                Err(format!("unknown type '{}'", name))
            }
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

    // forward declare structs so that they can be referenced in function signatures
    for node in program {
        if let TopLevelNode::Struct { name, fields, .. } = &node.value {
            if cx.structs.contains_key(name) {
                errors.push(Error {
                    msg: format!("Duplicate struct definition '{}'", name),
                    span: node.span.clone(),
                });
                continue;
            }
            cx.structs.insert(name, fields.clone());
        }
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
                    .map(|(_, ty)| resolve_type(&type_params, ty))
                    .collect();
                let resolved_return = resolve_type(&type_params, return_type);
                cx.generic_fns.insert(name, GenericFnSig {
                    generics: generics.clone(),
                    params: resolved_params,
                    return_type: resolved_return,
                });
            }
            TopLevelNode::Function { name, params, return_type, .. }
            | TopLevelNode::Extern { name, params, return_type, .. } => {
                cx.insert(name, None, Type::Function {
                    params: params.iter().map(|(_, ty)| ty.clone()).collect(),
                    return_type: Box::new(return_type.clone()),
                });
            }
            // globals share the value namespace with functions; register the name
            // so references resolve as ordinary variables.
            TopLevelNode::Global { name, ty, .. } => {
                cx.insert(name, None, ty.clone());
                cx.global_consts.insert(name);
            }
            TopLevelNode::Struct { .. } => {}
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