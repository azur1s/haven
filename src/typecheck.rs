use std::collections::HashMap;
use crate::{ast::*, intrinsics::{Intrinsic, IntrinsicSig, TyConstraint, ConstBound}};

#[derive(Clone, Debug)]
pub struct Context<'a> {
    pub scopes: Vec<HashMap<&'a str, Type<'a>>>,
    /// Map from Expr/Stmt/TopLevel IDs to their inferred types, for use in later codegen
    pub node_types: HashMap<usize, Type<'a>>,
    /// Struct definitions from name to ordered list of (field name, field type)
    pub structs: HashMap<&'a str, Vec<(&'a str, Type<'a>)>>,
    /// Type-parameter names in scope for the function currently being checked,
    /// e.g. `["T"]` while inside `proc id<T>(...)`. Used to resolve a bare
    /// `Type::Struct(name)` into a `Type::Param(name)`. Empty outside generics.
    pub generics: Vec<&'a str>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()], // global scope
            node_types: HashMap::new(),
            structs: HashMap::new(),
            generics: Vec::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: &'a str, ty: Type<'a>) {
        self.scopes.last_mut().unwrap().insert(name, ty);
    }

    /// Walk scopes from innermost to outermost, returning the first match for `name`
    pub fn lookup(&self, name: &str) -> Option<&Type<'a>> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }
}

/// Resolves a type-argument expression (a bare type name) into a `Type`, then
/// checks it against the parameter's kind constraint. Accepts scalars, declared
/// structs, and in-scope generic type params (`Type::Param`). Type params are
/// passed through optimistically — their kind is only known once a generic
/// function is monomorphized (not yet implemented), and such bodies never reach
/// codegen.
fn parse_type_arg<'a>(
    cx: &Context<'a>,
    intrinsic: Intrinsic,
    kind: TyConstraint,
    arg: &Expr<'a>,
    span: &Span,
) -> Result<Type<'a>, Error> {
    let name = match &arg.value {
        ExprNode::Var(n) => *n,
        _ => return Err(Error {
            msg: format!("{}() expects a type name as a type argument", intrinsic),
            span: span.clone(),
        }),
    };
    let ty = match name {
        "bool" => Type::Bool,
        "i32" => Type::Int32,
        "i64" => Type::Int64,
        "u32" => Type::Uint32,
        "u64" => Type::Uint64,
        "f32" => Type::Float32,
        "f64" => Type::Float64,
        _ if cx.generics.contains(&name) => Type::Param(name),
        _ if cx.structs.contains_key(&name) => Type::Struct(name),
        _ => return Err(Error {
            msg: format!("{}() got unknown type `{}`", intrinsic, name),
            span: span.clone(),
        }),
    };
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
    }
    Ok(ty)
}

/// Reads a const-argument expression as a compile-time integer and checks it
/// against the parameter's bound. Stage 1 const generics are literal-only.
fn parse_const_arg(
    intrinsic: Intrinsic,
    bound: ConstBound,
    arg: &Expr<'_>,
    span: &Span,
) -> Result<usize, Error> {
    let valid = |x: i64| {
        x >= bound.min && x <= bound.max && x % bound.multiple_of as i64 == 0
    };
    match &arg.value {
        ExprNode::Int32(x) if valid(*x as i64) => Ok(*x as usize),
        _ => {
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
}

/// Validates the call's total arity against the intrinsic's signature and binds
/// its leading type and const arguments. Returns the bound types and consts; the
/// trailing value arguments stay in `args` and are checked by the caller.
fn bind_leading_generics<'a>(
    cx: &Context<'a>,
    intrinsic: Intrinsic,
    sig: &IntrinsicSig,
    args: &[Expr<'a>],
    span: &Span,
) -> Result<(Vec<Type<'a>>, Vec<usize>), Error> {
    let header = sig.type_params.len() + sig.const_params.len();
    let expected = header + sig.value_arity;
    if args.len() != expected {
        return Err(Error {
            msg: format!(
                "{}() takes exactly {} argument{}, got {}",
                intrinsic, expected, if expected == 1 { "" } else { "s" }, args.len(),
            ),
            span: span.clone(),
        });
    }

    let mut tys = Vec::with_capacity(sig.type_params.len());
    for (i, kind) in sig.type_params.iter().enumerate() {
        tys.push(parse_type_arg(cx, intrinsic, *kind, &args[i], span)?);
    }
    let mut consts = Vec::with_capacity(sig.const_params.len());
    for (j, bound) in sig.const_params.iter().enumerate() {
        consts.push(parse_const_arg(intrinsic, *bound, &args[sig.type_params.len() + j], span)?);
    }
    Ok((tys, consts))
}

fn typecheck_intrinsic<'a>(
    cx: &mut Context<'a>,
    intrinsic: Intrinsic,
    args: &[Expr<'a>],
    span: Span,
    expr_id: usize,
) -> Result<Type<'a>, Error> {
    let sig = intrinsic.signature();

    match intrinsic {
        Intrinsic::Len => {
            bind_leading_generics(cx, intrinsic, &sig, args, &span)?;
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
            // Irregular: the type argument is trailing (`numerical_cast(value, T)`),
            // so it doesn't fit the leading-generics driver. Arity comes from the
            // signature's value_arity; the target type is parsed directly.
            if args.len() != sig.value_arity {
                return Err(Error { msg: "numerical_cast() takes exactly two arguments".into(), span });
            }
            let value_ty = infer(cx, &args[0])?;
            let target_ty = parse_type_arg(cx, intrinsic, TyConstraint::Numeric, &args[1], &span)?;

            if !value_ty.is_numeric() {
                return Err(Error {
                    msg: format!("numerical_cast() first argument must be a numeric type, got {}", value_ty),
                    span,
                });
            }

            cx.node_types.insert(expr_id, target_ty.clone());
            Ok(target_ty)
        }
        Intrinsic::Sizeof => {
            // A single type argument (scalar, struct, or in-scope type param).
            bind_leading_generics(cx, intrinsic, &sig, args, &span)?;
            cx.node_types.insert(expr_id, Type::Uint64);
            Ok(Type::Uint64)
        }
        Intrinsic::SimdSplat => {
            let (tys, consts) = bind_leading_generics(cx, intrinsic, &sig, args, &span)?;
            let ty = tys[0].clone();
            let size = consts[0];

            let value_ty = infer(cx, &args[2])?;
            if value_ty != ty {
                return Err(Error {
                    msg: format!("simd_splat() third argument must be of the element type, got {}", value_ty),
                    span,
                });
            }

            let simd_ty = Type::Simd(Box::new(ty), size);
            cx.node_types.insert(expr_id, simd_ty.clone());
            Ok(simd_ty)
        }
        Intrinsic::SimdLoad => {
            let (tys, consts) = bind_leading_generics(cx, intrinsic, &sig, args, &span)?;
            let ty = tys[0].clone();
            let size = consts[0];

            let slice_ty = infer(cx, &args[2])?;
            let offset_ty = infer(cx, &args[3])?;

            if !offset_ty.is_integer() {
                return Err(Error {
                    msg: format!("simd_load() fourth argument must be an integer type, got {}", offset_ty),
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
                        msg: format!("simd_load() third argument must be a slice or pointer to the element type, got {}", slice_ty),
                        span,
                    });
                }
            }
        }
        Intrinsic::SimdStore => {
            let (tys, consts) = bind_leading_generics(cx, intrinsic, &sig, args, &span)?;
            let ty = tys[0].clone();
            let size = consts[0];

            let slice_ty = infer(cx, &args[2])?;
            let offset_ty = infer(cx, &args[3])?;
            let value_ty = infer(cx, &args[4])?;

            if !offset_ty.is_integer() {
                return Err(Error {
                    msg: format!("simd_store() fourth argument must be an integer type, got {}", offset_ty),
                    span,
                });
            }

            let expected_value_ty = Type::Simd(Box::new(ty.clone()), size);
            if value_ty != expected_value_ty {
                return Err(Error {
                    msg: format!("simd_store() fifth argument must be a SIMD vector of the element type and size, got {}", value_ty),
                    span,
                });
            }

            match slice_ty {
                Type::Slice(inner) | Type::Pointer(inner) if *inner == ty => Ok(Type::Void),
                _ => {
                    return Err(Error {
                        msg: format!("simd_store() third argument must be a slice or pointer to the element type, got {}", slice_ty),
                        span,
                    });
                }
            }
        }
        Intrinsic::SimdConcat => {
            let (tys, consts) = bind_leading_generics(cx, intrinsic, &sig, args, &span)?;
            let ty = tys[0].clone();
            let size = consts[0];

            let value1_ty = infer(cx, &args[2])?;
            let value2_ty = infer(cx, &args[3])?;

            let expected_value_ty = Type::Simd(Box::new(ty.clone()), size / 2);
            if value1_ty != expected_value_ty {
                return Err(Error {
                    msg: format!("simd_concat() third argument must be a SIMD vector of the element type and half the size, got {}", value1_ty),
                    span,
                });
            }
            if value2_ty != expected_value_ty {
                return Err(Error {
                    msg: format!("simd_concat() fourth argument must be a SIMD vector of the element type and half the size, got {}", value2_ty),
                    span,
                });
            }

            let result_ty = Type::Simd(Box::new(ty), size);
            cx.node_types.insert(expr_id, result_ty.clone());
            Ok(result_ty)
        }
        Intrinsic::SimdLow | Intrinsic::SimdHigh => {
            // simd_low/high(f32, N, value: simd[f32, M]) -> simd[f32, N] where N < M
            let (tys, consts) = bind_leading_generics(cx, intrinsic, &sig, args, &span)?;
            let ty = tys[0].clone();
            let size = consts[0];

            let value_ty = infer(cx, &args[2])?;
            // Only check if inner type is the same, because size can be different
            match value_ty {
                Type::Simd(inner_ty, inner_size) if *inner_ty == ty && inner_size > size => {
                    let result_ty = Type::Simd(Box::new(ty), size);
                    cx.node_types.insert(expr_id, result_ty.clone());
                    Ok(result_ty)
                }
                _ => {
                    return Err(Error {
                        msg: format!("{}() third argument must be a SIMD vector of the element type and larger size, got {}", intrinsic, value_ty),
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
        ExprNode::Int32(_)   => Type::Int32,
        ExprNode::Int64(_)   => Type::Int64,
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
        ExprNode::Int32(_)   => Type::Int32,
        ExprNode::Int64(_)   => Type::Int64,
        ExprNode::Uint32(_)  => Type::Uint32,
        ExprNode::Uint64(_)  => Type::Uint64,
        ExprNode::Float32(_) => Type::Float32,
        ExprNode::Float64(_) => Type::Float64,
        ExprNode::Str(_)     => Type::Str,

        ExprNode::Var(name) => {
            match cx.lookup(name) {
                Some(ty) => ty.clone(),
                None => {
                    let msg = format!("Undefined variable '{}'", name);
                    return Err(Error {
                        msg,
                        span,
                    });
                }
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
            Type::Array(Box::new(first_ty), inner.len())
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
            use crate::ast::BinaryOp::*;
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

        ExprNode::Call { func, args }
            if matches!(&func.value, ExprNode::Var(name)
                if Intrinsic::lookup(name).is_some()) => {
            let ExprNode::Var(name) = &func.value else { unreachable!() };
            let intrinsic = Intrinsic::lookup(name).unwrap();
            typecheck_intrinsic(cx, intrinsic, args, span, metadata.id)?
        },
        ExprNode::Call { func, args } => {
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
            let ty = resolve_type(&cx.generics, ty);
            check_expr(cx, &ty, value)?;
            cx.insert(name, ty);
        },

        StmtNode::Assign { left, value } => {
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
fn check_export_type<'a>(ty: &Type<'a>) -> Result<(), String> {
    match ty {
        // TODO check if this is correct
        Type::Array(inner, _) =>
            Err(format!("fixed-size array type '[{}; N]' is not allowed in @export functions, use a raw pointer '*{}' and an explicit length parameter instead", inner, inner)),
        Type::Slice(inner) =>
            Err(format!("slice type '{}' is not allowed in @export functions, use a raw pointer '*{}' and an explicit length parameter instead", ty, inner)),
        Type::Str =>
            Err("str type is not allowed in @export functions (its fat-pointer layout is not stable across FFI), pass a raw '*u8' pointer and an explicit length parameter instead".into()),
        Type::Pointer(inner) => check_export_type(inner), // recurse: *[]f32 is also banned
        Type::Simd(_, _) =>
            Err(format!("SIMD type '{}' is not allowed in @export functions because its calling convention is target-specific and not guaranteed to match the expected caller, or that's what I'm told", ty)),
        Type::Function { .. } =>
            Err("function pointer types are not supported in @export functions".into()),
        // TODO something like @repr(C)
        Type::Struct(_) =>
            Err(format!("struct type '{}' has unknown layout and cannot be used in @export functions", ty)),
        Type::Param(_) =>
            Err("generic type parameters are not allowed in @export functions".into()),
        // these are all fine across FFI
        Type::Void | Type::Bool
        | Type::Int32 | Type::Int64
        | Type::Uint32 | Type::Uint64
        | Type::Float32 | Type::Float64 => Ok(()),
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
                    if let Err(msg) = check_export_type(ty) {
                        return Err(Error {
                            msg: format!("parameter '{}' in @export function '{}': {}", param_name, name, msg),
                            span: node.span.clone(),
                        });
                    }
                }
                if let Err(msg) = check_export_type(return_type) {
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

            let return_ty = resolve_type(&cx.generics, return_type);

            cx.push_scope();

            // const generics are ordinary compile-time values, visible in the body
            for g in generics {
                if let GenericParam::Const(cname, cty) = g {
                    cx.insert(cname, cty.clone());
                }
            }

            // push params into scope, resolving type-param references
            for (pname, ty) in params {
                let resolved = resolve_type(&cx.generics, ty);
                cx.insert(pname, resolved);
            }

            for stmt in body {
                check_stmt(cx, &return_ty, stmt)?;
            }

            cx.pop_scope();
            cx.generics = Vec::new();
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
        Type::Array(inner, n) => Type::Array(Box::new(resolve_type(generics, inner)), *n),
        Type::Slice(inner)    => Type::Slice(Box::new(resolve_type(generics, inner))),
        Type::Simd(inner, n)  => Type::Simd(Box::new(resolve_type(generics, inner)), *n),
        Type::Function { params, return_type } => Type::Function {
            params: params.iter().map(|p| resolve_type(generics, p)).collect(),
            return_type: Box::new(resolve_type(generics, return_type)),
        },
        other => other.clone(),
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
            TopLevelNode::Function { name, params, return_type, .. }
            | TopLevelNode::Extern { name, params, return_type, .. } => {
                cx.insert(name, Type::Function {
                    params: params.iter().map(|(_, ty)| ty.clone()).collect(),
                    return_type: Box::new(return_type.clone()),
                });
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