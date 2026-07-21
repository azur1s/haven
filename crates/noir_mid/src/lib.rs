//! Middle end: typecheck, safety-check, monomorphize, and lower the AST to MIL.
pub mod intrinsics;
pub mod typecheck;
pub mod safecheck;
pub mod mono;
pub mod mil;
