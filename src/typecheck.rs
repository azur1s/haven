use std::collections::HashMap;
use crate::{ast::*, intrinsics::Intrinsic};

#[derive(Clone, Debug)]
pub struct Context<'a> {
    pub scopes: Vec<HashMap<&'a str, Type<'a>>>,
    /// Map from Expr/Stmt/TopLevel IDs to their inferred types, for use in later codegen
    pub node_types: HashMap<usize, Type<'a>>,
    /// Struct definitions from name to ordered list of (field name, field type)
    pub structs: HashMap<&'a str, Vec<(&'a str, Type<'a>)>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()], // global scope
            node_types: HashMap::new(),
            structs: HashMap::new(),
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

fn typecheck_intrinsic<'a>(
    cx: &mut Context<'a>,
    intrinsic: Intrinsic,
    args: &[Expr<'a>],
    span: Span,
    expr_id: usize,
) -> Result<Type<'a>, Error> {
    // let is_simd_compatible = |expr: &Expr<'a>| -> Result<Type<'a>, Error> {
    macro_rules! is_simd_compatible {
        ($expr:expr) => {
            match &($expr).value {
                ExprNode::Var(name) => match *name {
                    "i32" => Ok(Type::Int32),
                    "i64" => Ok(Type::Int64),
                    "u32" => Ok(Type::Uint32),
                    "u64" => Ok(Type::Uint64),
                    "f32" => Ok(Type::Float32),
                    "f64" => Ok(Type::Float64),
                    _ => {
                        Err(Error {
                            msg: format!("{}() first argument must be a SIMD compatible type`", intrinsic),
                            span: span.clone(),
                        })
                    }
                }
                _ => {
                    Err(Error {
                        msg: format!("{}() first argument must be a SIMD compatible type`", intrinsic),
                        span: span.clone(),
                    })
                }
            }
        }
    }

    match intrinsic {
        Intrinsic::Len => {
            if args.len() != 1 {
                return Err(Error { msg: "len() takes exactly one argument".into(), span });
            }
            let arg_ty = infer(cx, &args[0])?;
            if !matches!(arg_ty, Type::Slice(_) | Type::Array(_, _) | Type::Pointer(_)) {
                return Err(Error {
                    msg: format!("len() expects a slice, array or pointer, got {}", arg_ty),
                    span,
                });
            }
            cx.node_types.insert(expr_id, Type::Int32);
            Ok(Type::Int32)
        }
        Intrinsic::NumericalCast => {
            if args.len() != 2 {
                return Err(Error { msg: "numerical_cast() takes exactly two arguments".into(), span });
            }
            let value_ty = infer(cx, &args[0])?;
            let target_ty = match &args[1].value {
                ExprNode::Var(name) => match *name {
                    "i32" => Type::Int32,
                    "i64" => Type::Int64,
                    "u32" => Type::Uint32,
                    "u64" => Type::Uint64,
                    "f32" => Type::Float32,
                    "f64" => Type::Float64,
                    _ => {
                        return Err(Error {
                            msg: "numerical_cast() second argument must be `iN`, `uN` or `fN`".into(),
                            span,
                        });
                    }
                }
                _ => {
                    return Err(Error {
                        msg: "numerical_cast() second argument must be `iN`, `uN` or `fN`".into(),
                        span,
                    });
                }
            };

            if !value_ty.is_numeric() {
                println!("{value_ty:?}");
                return Err(Error {
                    msg: format!("numerical_cast() first argument must be a numeric type, got {}", value_ty),
                    span,
                });
            }

            cx.node_types.insert(expr_id, target_ty.clone());
            Ok(target_ty)
        }
        Intrinsic::SimdSplat => {
            if args.len() != 3 {
                return Err(Error { msg: "simd_splat() takes exactly three arguments".into(), span });
            }

            let ty = is_simd_compatible!(&args[0])?;
            let size = match &args[1].value {
                ExprNode::Int32(x) if *x > 0 && *x <= 64 => *x as usize,
                _ => {
                    return Err(Error {
                        msg: "simd_splat() second argument must be a positive integer literal between 1 and 64".into(),
                        span,
                    });
                }
             };

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
            if args.len() != 4 {
                return Err(Error { msg: "simd_load() takes exactly four arguments".into(), span });
            }

            let ty = is_simd_compatible!(&args[0])?;
            let size = match &args[1].value {
                ExprNode::Int32(x) if *x > 0 && *x <= 64 => *x as usize,
                _ => {
                    return Err(Error {
                        msg: "simd_load() second argument must be a positive integer literal between 1 and 64".into(),
                        span,
                    });
                }
            };

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
            if args.len() != 5 {
                return Err(Error { msg: "simd_store() takes exactly five arguments".into(), span });
            }

            let ty = is_simd_compatible!(&args[0])?;
            let size = match &args[1].value {
                ExprNode::Int32(x) if *x > 0 && *x <= 64 => *x as usize,
                _ => {
                    return Err(Error {
                        msg: "simd_store() second argument must be a positive integer literal between 1 and 64".into(),
                        span,
                    });
                }
            };

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
            if args.len() != 4 {
                return Err(Error { msg: "simd_concat() takes exactly four arguments".into(), span });
            }

            let ty = is_simd_compatible!(&args[0])?;
            let size = match &args[1].value {
                ExprNode::Int32(x) if *x > 0 && *x <= 64 && *x % 2 == 0 => *x as usize,
                _ => {
                    return Err(Error {
                        msg: "simd_concat() second argument must be an even positive integer literal between 1 and 64".into(),
                        span,
                    });
                }
            };

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
            let fn_str = match intrinsic {
                Intrinsic::SimdLow => "simd_low",
                Intrinsic::SimdHigh => "simd_high",
                _ => unreachable!(),
            };
            if args.len() != 3 {
                return Err(Error { msg: format!("{}() takes exactly three arguments", fn_str), span });
            }

            let ty = is_simd_compatible!(&args[0])?;

            let size = match &args[1].value {
                ExprNode::Int32(x) if *x > 0 && *x <= 64 && *x % 2 == 0 => *x as usize,
                _ => {
                    return Err(Error {
                        msg: format!("{}() second argument must be an even positive integer literal between 1 and 64", fn_str),
                        span,
                    });
                }
            };

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
                        msg: format!("{}() third argument must be a SIMD vector of the element type and larger size, got {}", fn_str, value_ty),
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
        // ExprNode::Str(_)     => Type::Str,

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
        // ExprNode::Str(_)     => Type::Str,

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
            check_expr(cx, ty, value)?;
            cx.insert(name, ty.clone());
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
        Type::Pointer(inner) => check_export_type(inner), // recurse: *[]f32 is also banned
        Type::Simd(_, _) =>
            Err(format!("SIMD type '{}' is not allowed in @export functions because its calling convention is target-specific and not guaranteed to match the expected caller, or that's what I'm told", ty)),
        Type::Function { .. } =>
            Err("function pointer types are not supported in @export functions".into()),
        // TODO something like @repr(C)
        Type::Struct(_) =>
            Err(format!("struct type '{}' has unknown layout and cannot be used in @export functions", ty)),
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
        TopLevelNode::Function { name, attributes, params, return_type, body, .. } => {
            if attributes.iter().any(|a| a.value.name == "export") {
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

            cx.push_scope();

            // Push params into scope
            for (name, ty) in params {
                cx.insert(name, ty.clone());
            }

            for stmt in body {
                check_stmt(cx, return_type, stmt)?;
            }

            cx.pop_scope();
        }

        // Extern declarations have no body to check
        TopLevelNode::Extern { .. } => {}

        TopLevelNode::Struct { name, fields, .. } => {
            // Ensure no duplicate field names and that referenced struct types exist
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