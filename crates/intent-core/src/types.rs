//! Domain types for Intent CLI
//!
//! All types use newtype pattern for type safety and validation.
//! No public constructors - use try_new() for validated construction.
//!
//! # Philosophy
//!
//! - **Type-driven design**: Make invalid states unrepresentable
//! - **Validated construction**: All types validate their invariants on creation
//! - **Zero panics**: Use `Result` for fallible construction
//! - **Newtype pattern**: Wrap primitives for type safety

use crate::error::{IntentError, IntentResult};
use std::fmt;

// Module will contain: HttpMethod, SpecName, Url, HeaderName,
// HeaderValue, StatusCode, Duration

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn types_module_compiles() {
        // Smoke test - module exists and compiles
    }
}
