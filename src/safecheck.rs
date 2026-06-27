use crate::{ast::*, intrinsics::Intrinsic};
use std::collections::HashMap;

struct RtSafetyCtx<'a> {
    // function name -> is it known-clean
    clean: HashMap<&'a str, bool>,
}

/// Returns the list of dirty callees found in this expression.
fn alloc_check_expr<'a>(ctx: &RtSafetyCtx<'a>, e: &Expr<'a>) -> Vec<(&'a str, Span)> {
    match &e.value {
        ExprNode::Call { func, args } => {
            // collect any dirty calls inside the arguments
            let mut dirty: Vec<(&'a str, Span)> = args.iter()
                .flat_map(|a| alloc_check_expr(ctx, a))
                .collect();

            // then check the callee
            if let ExprNode::Var(name) = func.value {
                if Intrinsic::lookup(name).is_none() {
                    let is_clean = ctx.clean.get(name).copied().unwrap_or(false);
                    if !is_clean {
                        dirty.push((name, func.span.clone()));
                    }
                }
            }
            dirty
        }
        ExprNode::Binary { left, right, .. } => {
            let mut dirty = alloc_check_expr(ctx, left);
            dirty.extend(alloc_check_expr(ctx, right));
            dirty
        }
        ExprNode::Unary { operand, .. } => alloc_check_expr(ctx, operand),
        ExprNode::Index { slice, index } => {
            let mut dirty = alloc_check_expr(ctx, slice);
            dirty.extend(alloc_check_expr(ctx, index));
            dirty
        }
        // literals & variable reads should always be clean
        _ => vec![],
    }
}

fn alloc_check_stmt<'a>(ctx: &RtSafetyCtx<'a>, s: &Stmt<'a>) -> Vec<(&'a str, Span)> {
    match &s.value {
        StmtNode::Expr(e) => alloc_check_expr(ctx, e),
        StmtNode::Block(block) => block.iter()
            .flat_map(|s| alloc_check_stmt(ctx, s))
            .collect(),

        StmtNode::Declare { value, .. } => alloc_check_expr(ctx, value),
        StmtNode::Assign { value, .. } => alloc_check_expr(ctx, value),

        StmtNode::If { condition, then_branch, else_branch, .. } => {
            let mut dirty = alloc_check_expr(ctx, condition);
            dirty.extend(alloc_check_stmt(ctx, then_branch));
            if let Some(b) = else_branch {
                dirty.extend(alloc_check_stmt(ctx, b));
            }
            dirty
        }

        StmtNode::While { condition, body } => {
            let mut dirty = alloc_check_expr(ctx, condition);
            dirty.extend(alloc_check_stmt(ctx, body));
            dirty
        }

        StmtNode::Break | StmtNode::Continue => vec![],
        StmtNode::Return(e) => alloc_check_expr(ctx, e),
    }
}


fn alloc_check_toplevel<'a>(ctx: &mut RtSafetyCtx<'a>, node: &TopLevel<'a>) -> Result<(), Vec<Error>> {
    match &node.value {
        // if function are marked with @alloc(false), then that function must
        // not have any allocation or call any function that allocates.
        TopLevelNode::Function { name, attributes, body, .. } => {
            let dirty_calls: Vec<(&'a str, Span)> = body.iter()
                .flat_map(|s| alloc_check_stmt(ctx, s))
                .collect();

            let is_clean = dirty_calls.is_empty();
            ctx.clean.insert(name, is_clean);

            if attributes.iter().any(|attr| attr.value.is_false("alloc")) && !is_clean {
                let errors: Vec<Error> = dirty_calls.into_iter()
                    .map(|(callee, span)| {
                        let msg = format!(
                            "Function '{}' is marked as @alloc(false) but calls '{}', which may allocate.",
                            name,
                            callee,
                        );
                        Error::new(span, msg)
                    })
                    .collect();

                return Err(errors);
            }
            Ok(())
        },
        TopLevelNode::Extern { name, attributes, .. } => {
            if attributes.iter().any(|attr| attr.value.is_false("alloc")) {
                // trust the user that the extern function is clean
                ctx.clean.insert(name, true);
            }
            Ok(())
        },
    }
}

pub fn alloc_check_program<'a>(program: &[TopLevel<'a>]) -> Result<(), Vec<Error>> {
    let mut ctx = RtSafetyCtx {
        clean: HashMap::new(),
    };

    for node in program {
        alloc_check_toplevel(&mut ctx, node)?;
    }

    Ok(())
}