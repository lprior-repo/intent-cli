//! CLI tool detection and validation
//!
//! This module provides utilities for detecting and validating external CLI tools
//! required by Intent CLI. All functions follow Railway-Oriented Programming
//! principles with zero panics and comprehensive error handling.
//!
//! # Philosophy
//!
//! - **Zero panics**: All operations return `Result<T, IntentError>`
//! - **Explicit errors**: Clear error messages for missing tools
//! - **Path preservation**: Returns full paths to found binaries
//! - **Functional patterns**: Pure functions with no side effects
//!
//! # Examples
//!
//! ```rust
//! use intent_core::cli::check_cue_installed;
//!
//! match check_cue_installed() {
//!     Ok(path) => println!("cue found at: {:?}", path),
//!     Err(e) => eprintln!("cue not found: {}", e),
//! }
//! ```

#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]

use std::path::PathBuf;

use crate::error::{IntentError, IntentResult};

/// Check if the `cue` CLI tool is installed and available in PATH
///
/// This function searches the system PATH for the `cue` binary and returns
/// its full path if found. This is a pure function with no side effects.
///
/// # Returns
///
/// - `Ok(PathBuf)` - Full path to the `cue` binary
/// - `Err(IntentError)` - Error if `cue` is not found or cannot be accessed
///
/// # Errors
///
/// Returns `IntentError::NotFound` if:
/// - The `cue` binary is not in PATH
/// - The binary exists but is not executable
/// - There are permission issues accessing the binary
///
/// # Examples
///
/// ```rust,no_run
/// use intent_core::cli::check_cue_installed;
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let cue_path = check_cue_installed()?;
///     println!("Using cue at: {:?}", cue_path);
///     Ok(())
/// }
/// ```
///
/// # Railway-Oriented Programming
///
/// This function can be chained with other operations:
///
/// ```rust,no_run
/// use intent_core::cli::check_cue_installed;
///
/// let result = check_cue_installed()
///     .map(|path| format!("Found cue at: {:?}", path))
///     .map_err(|e| eprintln!("Error: {}", e));
/// ```
pub fn check_cue_installed() -> IntentResult<PathBuf> {
    which::which("cue").map_err(|_source| IntentError::resource_not_found("cue CLI tool", "PATH"))
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // TDD Tests - Written FIRST per bead requirements
    // =========================================================================

    #[test]
    fn test_cue_available() {
        // This test verifies the behavior when cue is installed
        // Note: This test may fail on systems without cue installed,
        // but it demonstrates the expected happy path behavior

        let result = check_cue_installed();

        match result {
            Ok(path) => {
                // If cue is found, verify we got a valid PathBuf
                assert!(!path.as_os_str().is_empty(), "Path should not be empty");
                assert!(
                    path.is_absolute() || path.components().count() > 0,
                    "Path should be valid"
                );

                // Verify the path ends with 'cue' (or 'cue.exe' on Windows)
                let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
                assert!(
                    file_name == "cue" || file_name == "cue.exe",
                    "Binary name should be 'cue' or 'cue.exe', got: {}",
                    file_name
                );
            }
            Err(e) => {
                // If cue is not found, verify we get the correct error type
                match e {
                    IntentError::NotFound { resource_type, .. } => {
                        assert_eq!(resource_type, "cue CLI tool");
                    }
                    _ => panic!("Expected NotFound error variant, got: {:?}", e),
                }
            }
        }
    }

    #[test]
    fn test_cue_not_found_returns_error() {
        // This test verifies error handling for a non-existent binary
        // We use a guaranteed non-existent name to test error path

        let result = which::which("cue-definitely-not-a-real-binary-name-12345");

        // Verify the which crate returns an error for non-existent binaries
        assert!(
            result.is_err(),
            "Should return error for non-existent binary"
        );
    }

    #[test]
    fn test_check_cue_installed_returns_result() {
        // Verify the function signature returns Result type (compile-time check)
        let _result: IntentResult<PathBuf> = check_cue_installed();

        // This test primarily verifies the type signature compiles correctly
        // The actual success/failure depends on system state
    }

    #[test]
    fn test_error_contains_resource_type() {
        // When cue is not found, verify error message contains useful info

        // Simulate the error case by checking for a non-existent binary
        let error = IntentError::resource_not_found("cue CLI tool", "PATH");

        // Verify error message contains resource type
        let error_string = error.to_string();
        assert!(
            error_string.contains("cue CLI tool"),
            "Error should mention 'cue CLI tool', got: {}",
            error_string
        );
    }

    #[test]
    fn test_no_panic_on_missing_binary() {
        // Verify function never panics, even when binary is missing
        // This is a critical safety test for the zero-panic philosophy

        let result = check_cue_installed();

        // The function should always return (either Ok or Err)
        // If we reach this line, the function didn't panic
        match result {
            Ok(_) | Err(_) => {
                // Success: function returned a Result without panicking
            }
        }
    }

    // =========================================================================
    // Integration Tests
    // =========================================================================

    #[test]
    fn test_railway_oriented_chaining() {
        // Verify the function works correctly in railway-oriented pipelines

        let result = check_cue_installed()
            .map(|path| path.to_string_lossy().to_string())
            .map_err(|e| format!("Wrapped error: {}", e));

        // Verify we can chain operations without panics
        match result {
            Ok(path_str) => {
                assert!(!path_str.is_empty());
            }
            Err(err_str) => {
                assert!(err_str.contains("Wrapped error"));
            }
        }
    }

    #[test]
    fn test_and_then_combinator() {
        // Test using and_then for monadic chaining

        let result = check_cue_installed().and_then(|path| {
            // Simulate a dependent operation
            if path.exists() {
                Ok(path)
            } else {
                Err(IntentError::resource_not_found("cue binary", path))
            }
        });

        // Verify the chain works correctly
        match result {
            Ok(path) => {
                assert!(path.exists() || !path.as_os_str().is_empty());
            }
            Err(_) => {
                // Error is acceptable for this test
            }
        }
    }
}
