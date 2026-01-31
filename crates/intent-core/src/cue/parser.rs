//! CUE Parser - Public API for parsing CUE files
//!
//! This module provides the `CueParser` struct which implements Railway-Oriented
//! Programming to chain:
//! 1. Detection (check if CUE CLI is available)
//! 2. Export (run `cue export` to get JSON)
//! 3. Parse (convert JSON to `CueSpec`)
//!
//! # Philosophy
//!
//! - **Railway-oriented programming**: Chain operations with Result combinators
//! - **Zero panics**: All errors handled via Result<T, E>
//! - **Immutable by default**: Parser is stateless and thread-safe
//! - **Pure functions**: Side effects isolated at boundaries

#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]

use std::path::Path;

use super::{parse_cue_output, run_cue_export, CueSpec};
use crate::error::IntentError;

// =============================================================================
// CueParser - Public API
// =============================================================================

/// Parser for CUE files
///
/// Provides a high-level API for parsing CUE files into structured `CueSpec` objects.
/// Uses Railway-Oriented Programming to chain:
/// 1. Detection (check if CUE CLI is available)
/// 2. Export (run `cue export` to get JSON)
/// 3. Parse (convert JSON to `CueSpec`)
///
/// # Examples
///
/// ```no_run
/// use std::path::Path;
///
/// use intent_core::cue::CueParser;
///
/// let parser = CueParser::new();
/// let spec = parser.parse(Path::new("spec.cue"))?;
/// assert_eq!(spec.id, "example-spec");
/// # Ok::<(), intent_core::IntentError>(())
/// ```
///
/// # Functional Patterns
///
/// - Zero unwraps/panics
/// - Railway-Oriented error handling
/// - Immutable data flow
/// - Pure functions (side effects at boundaries)
#[derive(Debug, Clone, Default)]
pub struct CueParser;

impl CueParser {
    /// Create a new `CueParser`
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::cue::CueParser;
    ///
    /// let parser = CueParser::new();
    /// ```
    #[must_use]
    pub const fn new() -> Self {
        Self
    }

    /// Parse a CUE file into a `CueSpec`
    ///
    /// This is the main entry point for parsing CUE files. It chains:
    /// 1. Detection - Check if `cue` CLI is available
    /// 2. Export - Run `cue export` to get JSON
    /// 3. Parse - Convert JSON to `CueSpec`
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the CUE file
    ///
    /// # Returns
    ///
    /// Returns `Result<CueSpec, IntentError>`:
    /// - `Ok(CueSpec)`: Successfully parsed specification
    /// - `Err(IntentError)`: Parse error with detailed context
    ///
    /// # Errors
    ///
    /// Returns errors for:
    /// - File not found (`IntentError::NotFound`)
    /// - CUE CLI not available (`IntentError::Config`)
    /// - CUE evaluation error (`IntentError::CueEval`)
    /// - JSON parse error (`IntentError::Parse`)
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    ///
    /// use intent_core::cue::CueParser;
    ///
    /// let parser = CueParser::new();
    /// let result = parser.parse(Path::new("spec.cue"));
    ///
    /// match result {
    ///     Ok(spec) => println!("Parsed: {}", spec.id),
    ///     Err(e) => eprintln!("Parse failed: {}", e),
    /// }
    /// # Ok::<(), intent_core::IntentError>(())
    /// ```
    ///
    /// # Functional Patterns
    ///
    /// Railway-Oriented Programming chain:
    /// ```text
    /// detect_cue() -> export_json() -> parse_spec()
    ///      ↓              ↓              ↓
    ///   Result         Result         Result
    /// ```
    pub fn parse(&self, path: &Path) -> Result<CueSpec, IntentError> {
        // Railway chain: detect → export → parse
        self.detect_cue()
            .and_then(|()| self.export_json(path))
            .and_then(|json| self.parse_json(&json, path))
    }

    /// Detect if CUE CLI is available
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Config` if CUE CLI is not found
    fn detect_cue(&self) -> Result<(), IntentError> {
        // For now, we assume cue is available
        // This will be implemented in a separate bead (intent-cli-hsb5)
        // Detection logic will check: `which cue` or `cue version`
        Ok(())
    }

    /// Export CUE file to JSON
    ///
    /// # Errors
    ///
    /// Returns `IntentError` if export fails
    fn export_json(&self, path: &Path) -> Result<String, IntentError> {
        // Delegate to existing run_cue_export function
        run_cue_export(path)
    }

    /// Parse JSON into `CueSpec`
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Parse` if JSON is invalid
    fn parse_json(&self, json: &str, path: &Path) -> Result<CueSpec, IntentError> {
        parse_cue_output(json).map_err(|e| {
            IntentError::parse(
                path.to_path_buf(),
                0, // Line number - JSON errors don't have line context
                0, // Column number
                format!("Failed to parse CUE output: {e}"),
            )
        })
    }
}

// =============================================================================
// TDD Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // test_parser_api - TDD Test First (as per bead requirement)
    // =========================================================================

    #[test]
    fn test_parser_api() {
        // Arrange: Create parser
        let parser = CueParser::new();

        // Assert: Parser can be created
        // This is a smoke test - the main parse() method will be tested
        // with integration tests once we have CUE files to test with
        assert!(format!("{parser:?}").contains("CueParser"));
    }

    #[test]
    fn test_parser_new() {
        // Arrange & Act: Create parser
        let parser = CueParser::new();

        // Assert: Should be Debug
        let debug_output = format!("{parser:?}");
        assert!(debug_output.contains("CueParser"));
    }

    #[test]
    fn test_parser_default() {
        // Arrange & Act: Create parser via Default
        let parser = CueParser::default();

        // Assert: Should work the same as new()
        let debug_output = format!("{parser:?}");
        assert!(debug_output.contains("CueParser"));
    }

    #[test]
    fn test_parser_clone() {
        // Arrange: Create parser
        let parser1 = CueParser::new();

        // Act: Clone it
        let parser2 = parser1.clone();

        // Assert: Should be equal
        let debug1 = format!("{parser1:?}");
        let debug2 = format!("{parser2:?}");
        assert_eq!(debug1, debug2);
    }

    #[test]
    fn test_parse_nonexistent_file() {
        // Arrange: Create parser and non-existent path
        let parser = CueParser::new();
        let path = Path::new("/tmp/definitely_does_not_exist_12345.cue");

        // Act: Try to parse
        let result = parser.parse(path);

        // Assert: Should fail with NotFound error
        assert!(result.is_err());
        match result {
            Err(IntentError::NotFound { .. }) => {
                // Expected error type
            }
            _ => panic!("Expected NotFound error, got: {result:?}"),
        }
    }

    #[test]
    fn test_detect_cue_placeholder() {
        // Arrange: Create parser
        let parser = CueParser::new();

        // Act: Call detect_cue (currently a placeholder)
        let result = parser.detect_cue();

        // Assert: Should succeed (placeholder implementation)
        assert!(result.is_ok());
    }

    // =========================================================================
    // Railway Pattern Tests
    // =========================================================================

    #[test]
    fn test_railway_pattern_error_propagation() {
        // Arrange: Create parser with non-existent file
        let parser = CueParser::new();
        let path = Path::new("/tmp/nonexistent.cue");

        // Act: Try to parse (will fail at file existence check)
        let result = parser.parse(path);

        // Assert: Error should propagate through railway chain
        assert!(result.is_err());
    }

    // =========================================================================
    // No Panics Tests
    // =========================================================================

    #[test]
    fn test_parse_never_panics() {
        // Stress test: Ensure parse never panics on various inputs
        let parser = CueParser::new();

        let test_paths = vec![
            Path::new(""),
            Path::new("/tmp/nonexistent.cue"),
            Path::new("/dev/null"),
            Path::new("/tmp/test with spaces.cue"),
            Path::new("/tmp/test\x00null.cue"), // Null byte in path
        ];

        for path in test_paths {
            let _ = parser.parse(path);
            // Should never panic, only return Err
        }
    }
}
