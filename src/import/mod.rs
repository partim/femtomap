//! The feature definition language.
//!
//! This module defines a small domain-specific language that can be used to
//! describe the set of features for a map.

pub use self::eval::Failed;

pub mod ast;
pub mod eval;
pub mod path;
pub mod watch;

