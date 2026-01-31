#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]

//! Test runner types and utilities
//!
//! This module provides types for representing test execution results
//! using functional patterns and Railway-Oriented Programming.

mod assertion;
mod assertion_kind;
pub mod assertions;
mod test_result;
mod test_runner;

pub use assertion::Assertion;
pub use assertion_kind::AssertionKind;
pub use test_result::TestResult;
pub use test_runner::TestRunner;

// Re-export assertion functions
pub use assertions::assert_json_path;
