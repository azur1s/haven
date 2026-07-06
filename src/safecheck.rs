use crate::{ast::*, intrinsics::Intrinsic};
use std::collections::{HashMap, HashSet};

// Runtime-safety (`@alloc(false)`) checking.
//
// "clean" means a function performs no heap allocation, transitively. The
// analysis is a greatest fixpoint over the call graph:
//
//   * extern    -> clean iff annotated `@alloc(false)` (we trust the user);
//                  an unannotated extern is an allocating leaf.
//   * intrinsic -> always clean (dropped during call collection).
//   * function  -> clean iff every callee is clean.
//
// We can't do this in a single definition-order pass, modules are flattened with
// imports appearing *after* the module that imports them, so a callee can be
// defined later in the program than its caller. The fixpoint starts every function
// optimistically clean and propagates dirtiness until it stabilizes, which is
// order-independent and handles (mutual) recursion correctly.

/// Map from a callable's final (post-mono) name to whether it is known clean.
type CleanMap<'a> = HashMap<&'a str, bool>;

/// Sentinel "callee" for an indirect call through a function pointer. Its target
/// isn't statically known, so we can't prove it clean — it never appears in the
/// clean map, so any function that makes one is forced dirty. Not a valid
/// identifier, so it can't collide with a real callable's name.
const INDIRECT_CALLEE: &str = "<indirect call>";

/// Returns the immediate calls in this expression whose callee is dirty.
fn dirty_calls_expr<'a>(clean: &CleanMap<'a>, e: &Expr<'a>) -> Vec<(&'a str, Span)> {
    match &e.value {
        ExprNode::Call { func, args, .. } => {
            // dirty calls nested in the arguments, first
            let mut dirty: Vec<(&'a str, Span)> = args.iter()
                .flat_map(|a| dirty_calls_expr(clean, a))
                .collect();

            // then the callee itself (intrinsics are always clean)
            match &func.value {
                ExprNode::Var(name) => {
                    if Intrinsic::lookup(name).is_none()
                        && !clean.get(name).copied().unwrap_or(false)
                    {
                        dirty.push((*name, func.span.clone()));
                    }
                }
                // indirect call: target unknown, conservatively dirty
                _ => dirty.push((INDIRECT_CALLEE, func.span.clone())),
            }
            dirty
        }
        ExprNode::Binary { left, right, .. } => {
            let mut dirty = dirty_calls_expr(clean, left);
            dirty.extend(dirty_calls_expr(clean, right));
            dirty
        }
        ExprNode::Unary { operand, .. } => dirty_calls_expr(clean, operand),
        ExprNode::Index { slice, index } => {
            let mut dirty = dirty_calls_expr(clean, slice);
            dirty.extend(dirty_calls_expr(clean, index));
            dirty
        }
        // literals & variable reads are always clean
        _ => vec![],
    }
}

fn dirty_calls_stmt<'a>(clean: &CleanMap<'a>, s: &Stmt<'a>) -> Vec<(&'a str, Span)> {
    match &s.value {
        StmtNode::Expr(e) => dirty_calls_expr(clean, e),
        StmtNode::Block(block) => block.iter()
            .flat_map(|s| dirty_calls_stmt(clean, s))
            .collect(),

        StmtNode::Declare { value, .. } => dirty_calls_expr(clean, value),
        StmtNode::Assign { value, .. } => dirty_calls_expr(clean, value),

        StmtNode::If { condition, then_branch, else_branch, .. } => {
            let mut dirty = dirty_calls_expr(clean, condition);
            dirty.extend(dirty_calls_stmt(clean, then_branch));
            if let Some(b) = else_branch {
                dirty.extend(dirty_calls_stmt(clean, b));
            }
            dirty
        }

        StmtNode::While { condition, body } => {
            let mut dirty = dirty_calls_expr(clean, condition);
            dirty.extend(dirty_calls_stmt(clean, body));
            dirty
        }

        StmtNode::Break | StmtNode::Continue => vec![],
        StmtNode::Return(e) => dirty_calls_expr(clean, e),
    }
}

fn collect_calls_expr<'a>(calls: &mut HashSet<&'a str>, e: &Expr<'a>) {
    match &e.value {
        ExprNode::Call { func, args, .. } => {
            match &func.value {
                ExprNode::Var(name) => {
                    if Intrinsic::lookup(name).is_none() {
                        calls.insert(*name);
                    }
                }
                // indirect call through a fn pointer: record the sentinel so the
                // enclosing function is forced dirty (target can't be proven clean)
                _ => { calls.insert(INDIRECT_CALLEE); }
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

/// Compute the clean/dirty status of every callable via a fixpoint.
fn compute_clean<'a>(program: &[TopLevel<'a>]) -> CleanMap<'a> {
    let mut clean: CleanMap<'a> = HashMap::new();

    // leaves: externs are clean iff annotated, functions start optimistically
    // clean and the call graph records who they call
    let mut calls: HashMap<&'a str, HashSet<&'a str>> = HashMap::new();
    for node in program {
        match &node.value {
            TopLevelNode::Extern { name, attributes, .. } => {
                let is_clean = attributes.iter().any(|a| a.value.is_false("alloc"));
                clean.insert(name, is_clean);
            }
            TopLevelNode::Function { name, body, .. } => {
                let mut callees = HashSet::new();
                for s in body { collect_calls_stmt(&mut callees, s); }
                calls.insert(name, callees);
                clean.insert(name, true);
            }
            TopLevelNode::Struct { .. } | TopLevelNode::Global { .. } => {}
        }
    }

    // propagate dirtiness until stable. a function goes dirty as soon as any of
    // its callees is dirty (or unknown, which we treat conservatively as dirty)
    loop {
        let mut changed = false;
        for (&fname, callees) in &calls {
            if clean.get(fname).copied() == Some(false) { continue; }
            let is_dirty = callees.iter()
                .any(|c| !clean.get(c).copied().unwrap_or(false));
            if is_dirty {
                clean.insert(fname, false);
                changed = true;
            }
        }
        if !changed { break; }
    }

    clean
}

pub fn alloc_check_program<'a>(
    program: &[TopLevel<'a>],
    display: &HashMap<&'a str, String>,
) -> Result<(), Vec<Error>> {
    let clean = compute_clean(program);

    // mono rewrites generic calls to their mangled instance name; prefer the
    // friendly spelling it recorded (`alloc::<Vec2>`) over `m2_alloc$alloc$Vec2`
    let show = |n: &'a str| display.get(n).map(String::as_str).unwrap_or(n);

    // report each `@alloc(false)` function that came out dirty, pointing at the
    // offending immediate calls in its body. dirtiness always propagates through
    // at least one immediate callee, so a dirty function has >=1 span to blame
    let mut errors: Vec<Error> = Vec::new();
    for node in program {
        let TopLevelNode::Function { name, attributes, body, .. } = &node.value else { continue };
        let marked = attributes.iter().any(|attr| attr.value.is_false("alloc"));
        if !marked || clean.get(name).copied().unwrap_or(false) {
            continue;
        }

        for (callee, span) in body.iter().flat_map(|s| dirty_calls_stmt(&clean, s)) {
            errors.push(Error::new(span, format!(
                "Function '{}' is marked as @alloc(false) but calls '{}', which may allocate.",
                show(name),
                show(callee),
            )));
        }
    }

    if errors.is_empty() { Ok(()) } else { Err(errors) }
}
