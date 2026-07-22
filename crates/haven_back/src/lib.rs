//! Back end: ABI/layout computation and LLVM IR emission from MIL.
//! `layout` now lives in `haven_common` (so the mid end can size data-enum
//! aggregates too); re-exported here so `crate::layout` keeps resolving.
pub use haven_common::layout;
pub mod abi;
pub mod llvm;
