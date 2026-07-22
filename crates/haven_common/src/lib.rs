//! Shared foundations for the haven compiler: the AST (with spans) and the
//! diagnostic reporter. Everything downstream (front/mid/back) builds on these.
pub mod ast;
pub mod diag;
pub mod layout;
