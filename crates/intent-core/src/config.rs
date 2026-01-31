//! Configuration management for Intent CLI
//!
//! Handles loading, validation, and merging of configuration from
//! multiple sources: intent.toml, environment variables, CLI flags.
//!
//! # Philosophy
//!
//! - **Layered configuration**: Files < Env vars < CLI flags
//! - **Validated construction**: All config validated on load
//! - **Zero panics**: Use `Result` for all fallible operations
//! - **Immutable by default**: Config is read-only after construction

use crate::error::{IntentError, IntentResult};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Main configuration struct
///
/// Configuration is loaded and merged from multiple sources in order:
/// 1. Default values
/// 2. intent.toml file
/// 3. Environment variables (INTENT_*)
/// 4. CLI flags
///
/// Later sources override earlier ones.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    // Will be filled in by subsequent beads
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn config_module_compiles() {
        // Smoke test - module exists and compiles
    }
}
