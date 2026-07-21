//! Shared foundations for the noir compiler: the AST (with spans) and the
//! diagnostic reporter. Everything downstream (front/mid/back) builds on these.
pub mod ast;
pub mod diag;
