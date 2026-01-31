//! CUE error parsing module
//!
//! Parses CUE CLI stderr output to extract line, column, and error messages,
//! mapping them to structured `IntentError::Parse` errors with location information.
//!
//! # Philosophy
//!
//! - **Railway-oriented programming**: Chain operations with Result combinators
//! - **Zero panics**: All regex operations handled via Result<T, E>
//! - **Functional patterns**: Use `.map()`, `.and_then()`, `.map_err()`
//! - **No unwraps**: Fallback to default values using combinators
//!
//! # CUE Error Format
//!
//! CUE error messages typically follow this format:
//! ```text
//! path/to/file.cue:42:10: error message here
//! ```
//!
//! Where:
//! - `path/to/file.cue` is the file path
//! - `42` is the line number (1-indexed)
//! - `10` is the column number (1-indexed)
//! - `error message here` is the descriptive error text
//!
//! # Examples
//!
//! ```
//! use std::path::PathBuf;
//!
//! use intent_core::{cue::error_parser::parse_cue_stderr, error::IntentError};
//!
//! let stderr = "test.cue:5:12: unexpected token ';'";
//! let error = parse_cue_stderr(&PathBuf::from("test.cue"), stderr);
//!
//! match error {
//!     IntentError::Parse {
//!         line,
//!         column,
//!         message,
//!         ..
//!     } => {
//!         assert_eq!(line, 5);
//!         assert_eq!(column, 12);
//!         assert!(message.contains("unexpected token"));
//!     }
//!     _ => panic!("Expected Parse error"),
//! }
//! ```

#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]

use std::path::Path;

use regex::Regex;

use crate::error::IntentError;

// =============================================================================
// Types
// =============================================================================

/// Parsed location information from CUE error
#[derive(Debug, Clone, PartialEq, Eq)]
struct ErrorLocation {
    /// Line number (1-indexed)
    line: usize,
    /// Column number (1-indexed)
    column: usize,
    /// Error message
    message: String,
}

// =============================================================================
// Public API
// =============================================================================

/// Parse CUE CLI stderr output into structured `IntentError::Parse`
///
/// Attempts to extract line, column, and error message from CUE error format.
/// If parsing fails, returns a Parse error with line 0, column 0 and the full stderr.
///
/// # Arguments
///
/// * `file` - Path to the CUE file being parsed
/// * `stderr` - Raw stderr output from CUE CLI
///
/// # Returns
///
/// `IntentError::Parse` with extracted location or default location if parsing fails
///
/// # Examples
///
/// ```
/// use std::path::PathBuf;
///
/// use intent_core::cue::error_parser::parse_cue_stderr;
///
/// let stderr = "spec.cue:42:10: conflicting values";
/// let error = parse_cue_stderr(&PathBuf::from("spec.cue"), stderr);
/// # match error {
/// #     intent_core::error::IntentError::Parse { line, column, .. } => {
/// #         assert_eq!(line, 42);
/// #         assert_eq!(column, 10);
/// #     }
/// #     _ => panic!("Expected Parse error"),
/// # }
/// ```
pub fn parse_cue_stderr(file: &Path, stderr: &str) -> IntentError {
    extract_error_location(stderr)
        .map(|loc| IntentError::parse(file, loc.line, loc.column, loc.message))
        .unwrap_or_else(|| {
            // Fallback: Create error with line 0, column 0 and full stderr
            IntentError::parse(file, 0, 0, stderr)
        })
}

// =============================================================================
// Internal Implementation
// =============================================================================

/// Extract error location from CUE stderr
///
/// Parses CUE error format: `file.cue:line:column: message`
///
/// Returns `None` if parsing fails (regex doesn't match or numbers invalid).
fn extract_error_location(stderr: &str) -> Option<ErrorLocation> {
    // Pattern matches: `:42:10: error message`
    // Captures: (line)(column)(message)
    create_location_regex().and_then(|re| extract_with_regex(&re, stderr))
}

/// Create regex for CUE error location
///
/// Returns `None` if regex compilation fails (should never happen with static pattern).
fn create_location_regex() -> Option<Regex> {
    // Pattern: :digits:digits: message (multiline mode to match $ at line end)
    Regex::new(r"(?m):(\d+):(\d+):\s*(.+)$").ok()
}

/// Extract location using regex
///
/// Uses regex captures to extract line, column, and message.
/// Returns `None` if no match or if parsing numbers fails.
fn extract_with_regex(re: &Regex, stderr: &str) -> Option<ErrorLocation> {
    re.captures(stderr).and_then(parse_captures)
}

/// Parse regex captures into ErrorLocation
///
/// Extracts line, column, and message from capture groups.
/// Returns `None` if any number parsing fails.
fn parse_captures(caps: regex::Captures) -> Option<ErrorLocation> {
    let line = caps.get(1).and_then(|m| m.as_str().parse::<usize>().ok())?;

    let column = caps.get(2).and_then(|m| m.as_str().parse::<usize>().ok())?;

    let message = caps.get(3).map(|m| m.as_str().to_string())?;

    Some(ErrorLocation {
        line,
        column,
        message,
    })
}

// =============================================================================
// TDD Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    // =========================================================================
    // Test: CUE Error Mapping (TDD - Test First)
    // =========================================================================

    #[test]
    fn test_cue_error_mapping_with_valid_location() {
        // Arrange: CUE error with line:column format
        let stderr = "test.cue:42:10: unexpected token ';'";
        let file = PathBuf::from("test.cue");

        // Act: Parse stderr
        let error = parse_cue_stderr(&file, stderr);

        // Assert: Verify Parse error with correct location
        match error {
            IntentError::Parse {
                file: error_file,
                line,
                column,
                message,
                ..
            } => {
                assert_eq!(error_file, file);
                assert_eq!(line, 42);
                assert_eq!(column, 10);
                assert_eq!(message, "unexpected token ';'");
            }
            _ => panic!("Expected Parse variant, got: {:?}", error),
        }
    }

    #[test]
    fn test_cue_error_mapping_with_multiline_stderr() {
        // Arrange: Multi-line stderr (common with CUE)
        let stderr = "some context\ntest.cue:5:1: conflicting values\n  more details";
        let file = PathBuf::from("test.cue");

        // Act: Parse stderr
        let error = parse_cue_stderr(&file, stderr);

        // Assert: Should extract from the line containing location
        match error {
            IntentError::Parse {
                line,
                column,
                message,
                ..
            } => {
                assert_eq!(line, 5);
                assert_eq!(column, 1);
                assert!(message.contains("conflicting values"));
            }
            _ => panic!("Expected Parse variant"),
        }
    }

    #[test]
    fn test_cue_error_mapping_without_location() {
        // Arrange: Generic error without location format
        let stderr = "generic error message";
        let file = PathBuf::from("test.cue");

        // Act: Parse stderr
        let error = parse_cue_stderr(&file, stderr);

        // Assert: Should fall back to line 0, column 0 with full message
        match error {
            IntentError::Parse {
                line,
                column,
                message,
                ..
            } => {
                assert_eq!(line, 0);
                assert_eq!(column, 0);
                assert_eq!(message, "generic error message");
            }
            _ => panic!("Expected Parse variant"),
        }
    }

    #[test]
    fn test_cue_error_mapping_with_large_line_numbers() {
        // Arrange: Error with large line/column numbers
        let stderr = "spec.cue:9999:123: error at end of file";
        let file = PathBuf::from("spec.cue");

        // Act: Parse stderr
        let error = parse_cue_stderr(&file, stderr);

        // Assert: Should handle large numbers correctly
        match error {
            IntentError::Parse { line, column, .. } => {
                assert_eq!(line, 9999);
                assert_eq!(column, 123);
            }
            _ => panic!("Expected Parse variant"),
        }
    }

    #[test]
    fn test_cue_error_mapping_with_empty_stderr() {
        // Arrange: Empty stderr
        let stderr = "";
        let file = PathBuf::from("test.cue");

        // Act: Parse stderr
        let error = parse_cue_stderr(&file, stderr);

        // Assert: Should create error with empty message
        match error {
            IntentError::Parse {
                line,
                column,
                message,
                ..
            } => {
                assert_eq!(line, 0);
                assert_eq!(column, 0);
                assert_eq!(message, "");
            }
            _ => panic!("Expected Parse variant"),
        }
    }

    #[test]
    fn test_cue_error_mapping_with_whitespace_in_message() {
        // Arrange: Error with whitespace in message
        let stderr = "test.cue:1:1:   leading whitespace preserved";
        let file = PathBuf::from("test.cue");

        // Act: Parse stderr
        let error = parse_cue_stderr(&file, stderr);

        // Assert: Regex should trim leading whitespace after colon
        match error {
            IntentError::Parse { message, .. } => {
                assert_eq!(message, "leading whitespace preserved");
            }
            _ => panic!("Expected Parse variant"),
        }
    }

    #[test]
    fn test_cue_error_mapping_with_special_characters() {
        // Arrange: Error message with special characters
        let stderr = "test.cue:10:5: expected '}' got '{'";
        let file = PathBuf::from("test.cue");

        // Act: Parse stderr
        let error = parse_cue_stderr(&file, stderr);

        // Assert: Should preserve special characters
        match error {
            IntentError::Parse { message, .. } => {
                assert_eq!(message, "expected '}' got '{'");
            }
            _ => panic!("Expected Parse variant"),
        }
    }

    // =========================================================================
    // Test: Internal Functions (Unit Tests)
    // =========================================================================

    #[test]
    fn test_create_location_regex_succeeds() {
        // Act: Create regex
        let result = create_location_regex();

        // Assert: Should succeed with valid pattern
        assert!(result.is_some());
    }

    #[test]
    fn test_extract_error_location_with_valid_input() {
        // Arrange: Valid CUE error format
        let stderr = ":42:10: test error";

        // Act: Extract location
        let result = extract_error_location(stderr);

        // Assert: Should extract successfully
        assert!(result.is_some());
        let loc = result.unwrap_or_else(|| panic!("Expected Some"));
        assert_eq!(loc.line, 42);
        assert_eq!(loc.column, 10);
        assert_eq!(loc.message, "test error");
    }

    #[test]
    fn test_extract_error_location_with_invalid_input() {
        // Arrange: Invalid format (no colons)
        let stderr = "invalid error format";

        // Act: Extract location
        let result = extract_error_location(stderr);

        // Assert: Should return None
        assert!(result.is_none());
    }

    #[test]
    fn test_extract_error_location_with_non_numeric_line() {
        // Arrange: Non-numeric line number
        let stderr = ":abc:10: error";

        // Act: Extract location
        let result = extract_error_location(stderr);

        // Assert: Should return None (parsing fails)
        assert!(result.is_none());
    }

    #[test]
    fn test_extract_error_location_with_zero_line() {
        // Arrange: Zero line number (edge case)
        let stderr = ":0:0: error at unknown location";

        // Act: Extract location
        let result = extract_error_location(stderr);

        // Assert: Should extract (0 is valid usize)
        assert!(result.is_some());
        let loc = result.unwrap_or_else(|| panic!("Expected Some"));
        assert_eq!(loc.line, 0);
        assert_eq!(loc.column, 0);
    }

    // =========================================================================
    // Test: Railway Pattern (Functional Composition)
    // =========================================================================

    #[test]
    fn test_railway_pattern_success_path() {
        // Arrange: Valid input for entire pipeline
        let stderr = ":100:50: test message";

        // Act: Run through extraction pipeline
        let result = create_location_regex().and_then(|re| extract_with_regex(&re, stderr));

        // Assert: Should succeed
        assert!(result.is_some());
    }

    #[test]
    fn test_railway_pattern_failure_path() {
        // Arrange: Invalid input (no match)
        let stderr = "no location here";

        // Act: Run through extraction pipeline
        let result = create_location_regex().and_then(|re| extract_with_regex(&re, stderr));

        // Assert: Should fail gracefully
        assert!(result.is_none());
    }

    // =========================================================================
    // Test: Edge Cases (Stress Testing)
    // =========================================================================

    #[test]
    fn test_no_panics_on_various_inputs() {
        // Stress test - ensure no panics on edge cases
        let test_cases = vec![
            "",
            ":1:1:",
            ":::",
            ":abc:def: error",
            ":999999999999999999999:1: overflow",
            "test.cue:1:1: normal",
            "\n\n:5:10: multiline\n\n",
            ":1:1: 🚀 unicode",
        ];

        for stderr in test_cases {
            let file = PathBuf::from("test.cue");
            let _ = parse_cue_stderr(&file, stderr);
            // Should never panic
        }
    }

    #[test]
    fn test_parser_never_panics_on_invalid_regex() {
        // Test that even if regex fails, we handle gracefully
        let stderr = ":1:1: test";
        let result = extract_error_location(stderr);
        // Should not panic, just return Some or None
        assert!(result.is_some());
    }
}
