use crate::{ast::*, intrinsics::Intrinsic};
use std::collections::{HashMap, HashSet};

// TODO collect_calls_* and alloc_check_* probably could just be merged into
// something like a AST walker with callbacks

struct RtSafetyCtx<'a> {
    // function name -> is it known-clean
    clean: HashMap<&'a str, bool>,
    // call graph: caller -> set of callees
    calls: HashMap<&'a str, HashSet<&'a str>>,
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
        TopLevelNode::Struct { .. } => Ok(()),
    }
}

fn collect_calls_expr<'a>(calls: &mut HashSet<&'a str>, e: &Expr<'a>) {
    match &e.value {
        ExprNode::Call { func, args } => {
            if let ExprNode::Var(name) = func.value {
                if Intrinsic::lookup(name).is_none() {
                    calls.insert(name);
                }
            }
            for arg in args {
                collect_calls_expr(calls, arg);
            }
        }
        ExprNode::Binary { left, right, .. } => {
            collect_calls_expr(calls, left);
            collect_calls_expr(calls, right);
        }
        ExprNode::Unary { operand, .. } => collect_calls_expr(calls, operand),
        ExprNode::Index { slice, index } => {
            collect_calls_expr(calls, slice);
            collect_calls_expr(calls, index);
        }
        _ => {}
    }
}

fn collect_calls_stmt<'a>(calls: &mut HashSet<&'a str>, s: &Stmt<'a>) {
    match &s.value {
        StmtNode::Expr(e) => collect_calls_expr(calls, e),
        StmtNode::Block(stmts) => stmts.iter().for_each(|s| collect_calls_stmt(calls, s)),
        StmtNode::Declare { value, .. } => collect_calls_expr(calls, value),
        StmtNode::Assign { value, .. } => collect_calls_expr(calls, value),
        StmtNode::If { condition, then_branch, else_branch, .. } => {
            collect_calls_expr(calls, condition);
            collect_calls_stmt(calls, then_branch);
            if let Some(b) = else_branch { collect_calls_stmt(calls, b); }
        }
        StmtNode::While { condition, body } => {
            collect_calls_expr(calls, condition);
            collect_calls_stmt(calls, body);
        }
        StmtNode::Return(e) => collect_calls_expr(calls, e),
        StmtNode::Break | StmtNode::Continue => {}
    }
}

fn populate_call_graph<'a>(ctx: &mut RtSafetyCtx<'a>, program: &[TopLevel<'a>]) {
    for node in program {
        if let TopLevelNode::Function { name, body, .. } = &node.value {
            let mut callees = HashSet::new();
            for stmt in body {
                collect_calls_stmt(&mut callees, stmt);
            }
            ctx.calls.insert(name, callees);
        }
    }
}

pub fn alloc_check_program<'a>(program: &[TopLevel<'a>]) -> Result<(), Vec<Error>> {
    let mut ctx = RtSafetyCtx {
        clean: HashMap::new(),
        calls: HashMap::new(),
    };

    populate_call_graph(&mut ctx, program);

    for node in program {
        alloc_check_toplevel(&mut ctx, node)?;
    }

    Ok(())
}