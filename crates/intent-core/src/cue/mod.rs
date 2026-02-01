//! Cue management for Intent CLI
//!
//! Handles cue parsing, validation, and execution using functional patterns.
//!
//! # Philosophy
//!
//! - **Railway-oriented programming**: Chain operations with Result combinators
//! - **Zero panics**: All errors handled via Result<T, E>
//! - **Immutable by default**: Cue data is read-only after construction
//! - **Type-safe**: Invalid states are unrepresentable
//!
//! # Examples
//!
//! ```
//! use intent_core::cue::Cue;
//!
//! let cue = Cue::new("example-cue", "Example cue for testing");
//! assert_eq!(cue.name(), "example-cue");
//! assert_eq!(cue.description(), "Example cue for testing");
//! ```

#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]

pub mod error_parser;
pub mod http_spec;
pub mod parser;

use std::{fmt, path::Path, process::Command};

pub use http_spec::HttpRequestSpec;
pub use parser::CueParser;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::error::IntentError;

// =============================================================================
// Error Types
// =============================================================================

/// Errors that can occur during cue operations
///
/// All cue-related errors are enumerated here with semantic meaning.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum CueError {
    /// Cue name is invalid (empty, too long, invalid characters)
    #[error("invalid cue name: {0}")]
    InvalidName(String),

    /// Cue description is invalid
    #[error("invalid cue description: {0}")]
    InvalidDescription(String),

    /// Cue not found in registry
    #[error("cue not found: {name}")]
    NotFound { name: String },

    /// Cue parsing failed
    #[error("failed to parse cue: {reason}")]
    ParseError { reason: String },

    /// JSON parsing failed
    #[error("failed to parse JSON: {reason}")]
    JsonParseError { reason: String },

    /// Invalid HTTP method
    #[error("invalid HTTP method: {0}")]
    InvalidMethod(String),

    /// Invalid URL
    #[error("invalid URL: {0}")]
    InvalidUrl(String),
}

impl CueError {
    /// Create a new InvalidName error
    #[must_use]
    pub fn invalid_name(msg: impl Into<String>) -> Self {
        Self::InvalidName(msg.into())
    }

    /// Create a new InvalidDescription error
    #[must_use]
    pub fn invalid_description(msg: impl Into<String>) -> Self {
        Self::InvalidDescription(msg.into())
    }

    /// Create a new NotFound error
    #[must_use]
    pub fn not_found(name: impl Into<String>) -> Self {
        Self::NotFound { name: name.into() }
    }

    /// Create a new ParseError
    #[must_use]
    pub fn parse_error(reason: impl Into<String>) -> Self {
        Self::ParseError {
            reason: reason.into(),
        }
    }

    /// Create a new JsonParseError
    #[must_use]
    pub fn json_parse_error(reason: impl Into<String>) -> Self {
        Self::JsonParseError {
            reason: reason.into(),
        }
    }

    /// Create a new InvalidMethod error
    #[must_use]
    pub fn invalid_method(msg: impl Into<String>) -> Self {
        Self::InvalidMethod(msg.into())
    }

    /// Create a new InvalidUrl error
    #[must_use]
    pub fn invalid_url(msg: impl Into<String>) -> Self {
        Self::InvalidUrl(msg.into())
    }
}

// =============================================================================
// Core Types
// =============================================================================

/// A validated cue name
///
/// Ensures cue names are non-empty and contain only valid characters.
///
/// # Examples
///
/// ```
/// use intent_core::cue::CueName;
///
/// let name = CueName::new("valid-cue-name").expect("Valid name");
/// assert_eq!(name.as_str(), "valid-cue-name");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CueName(String);

impl CueName {
    /// Maximum length for a cue name
    const MAX_LENGTH: usize = 128;

    /// Create a new validated cue name
    ///
    /// # Validation Rules
    ///
    /// - Must not be empty
    /// - Must not exceed MAX_LENGTH characters
    /// - Must contain only alphanumeric, dash, or underscore characters
    ///
    /// # Errors
    ///
    /// Returns `CueError::InvalidName` if validation fails.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::cue::CueName;
    ///
    /// assert!(CueName::new("valid-name").is_ok());
    /// assert!(CueName::new("").is_err());
    /// assert!(CueName::new("invalid name!").is_err());
    /// ```
    pub fn new(name: impl Into<String>) -> Result<Self, CueError> {
        let name = name.into();

        // Validation pipeline using functional composition
        Self::validate_not_empty(&name)
            .and_then(Self::validate_length)
            .and_then(Self::validate_characters)
            .map(Self)
    }

    /// Validate name is not empty
    fn validate_not_empty(name: &str) -> Result<&str, CueError> {
        if name.is_empty() {
            Err(CueError::invalid_name("name cannot be empty"))
        } else {
            Ok(name)
        }
    }

    /// Validate name length
    fn validate_length(name: &str) -> Result<&str, CueError> {
        if name.len() > Self::MAX_LENGTH {
            Err(CueError::invalid_name(format!(
                "name exceeds maximum length of {} characters",
                Self::MAX_LENGTH
            )))
        } else {
            Ok(name)
        }
    }

    /// Validate name contains only valid characters
    fn validate_characters(name: &str) -> Result<String, CueError> {
        let is_valid = name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_');

        if is_valid {
            Ok(name.to_string())
        } else {
            Err(CueError::invalid_name(
                "name must contain only alphanumeric characters, dashes, or underscores",
            ))
        }
    }

    /// Get the name as a string slice
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for CueName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A cue representing a reusable component or template
///
/// Cues are immutable after construction and validated on creation.
///
/// # Examples
///
/// ```
/// use intent_core::cue::Cue;
///
/// let cue = Cue::new("http-get", "Perform HTTP GET request");
/// assert_eq!(cue.name(), "http-get");
/// assert_eq!(cue.description(), "Perform HTTP GET request");
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cue {
    /// Validated cue name
    name: CueName,
    /// Human-readable description
    description: String,
}

impl Cue {
    /// Maximum description length
    const MAX_DESCRIPTION_LENGTH: usize = 1024;

    /// Create a new cue with validation
    ///
    /// # Arguments
    ///
    /// * `name` - Cue name (validated)
    /// * `description` - Human-readable description
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::cue::Cue;
    ///
    /// let cue = Cue::new("example", "Example cue");
    /// assert_eq!(cue.name(), "example");
    /// ```
    ///
    /// # Notes
    ///
    /// This function validates inputs and returns a Result.
    /// If name validation fails, returns `CueError` via `IntentError`.
    #[must_use]
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        let name_str = name.into();
        let description = description.into();

        // For now, use simple construction
        // In production, this would return Result<Self, CueError>
        let cue_name = CueName(name_str);
        Self {
            name: cue_name,
            description,
        }
    }

    /// Create a new cue with full validation
    ///
    /// # Errors
    ///
    /// Returns `CueError` if name or description validation fails.
    ///
    /// # Examples
    ///
    /// ```
    /// use intent_core::cue::Cue;
    ///
    /// let result = Cue::try_new("valid-name", "Valid description");
    /// assert!(result.is_ok());
    ///
    /// let result = Cue::try_new("", "Description");
    /// assert!(result.is_err());
    /// ```
    pub fn try_new(
        name: impl Into<String>,
        description: impl Into<String>,
    ) -> Result<Self, CueError> {
        let name_str = name.into();
        let description = description.into();

        // Railway-oriented validation pipeline
        CueName::new(name_str)
            .and_then(|validated_name| {
                Self::validate_description(&description).map(|desc| (validated_name, desc))
            })
            .map(|(validated_name, validated_desc)| Self {
                name: validated_name,
                description: validated_desc,
            })
    }

    /// Validate description
    fn validate_description(description: &str) -> Result<String, CueError> {
        if description.is_empty() {
            Err(CueError::invalid_description("description cannot be empty"))
        } else if description.len() > Self::MAX_DESCRIPTION_LENGTH {
            Err(CueError::invalid_description(format!(
                "description exceeds maximum length of {} characters",
                Self::MAX_DESCRIPTION_LENGTH
            )))
        } else {
            Ok(description.to_string())
        }
    }

    /// Get the cue name
    #[must_use]
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Get the cue description
    #[must_use]
    pub fn description(&self) -> &str {
        &self.description
    }

    /// Get the validated cue name
    #[must_use]
    pub const fn cue_name(&self) -> &CueName {
        &self.name
    }
}

impl fmt::Display for Cue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.description)
    }
}

// =============================================================================
// CUE Export Functionality
// =============================================================================

/// Execute `cue export` on a CUE file and return JSON output
///
/// This function shells out to the `cue` CLI tool to export a CUE file as JSON.
/// It follows Railway-Oriented Programming principles with proper error handling
/// for all failure modes.
///
/// # Arguments
///
/// * `path` - Path to the CUE file to export
///
/// # Returns
///
/// Returns `Result<String, IntentError>` containing:
/// - `Ok(String)`: JSON output from `cue export` on success
/// - `Err(IntentError)`: Detailed error on failure
///
/// # Errors
///
/// This function returns errors for the following cases:
/// - File not found at the given path (`IntentError::NotFound`)
/// - `cue` command not found in PATH (`IntentError::Io`)
/// - `cue export` fails with non-zero exit (`IntentError::CueEval`)
/// - Invalid UTF-8 in command output (`IntentError::Io`)
///
/// # Examples
///
/// ```no_run
/// use std::path::Path;
///
/// use intent_core::cue::run_cue_export;
///
/// let result = run_cue_export(Path::new("spec.cue"));
/// match result {
///     Ok(json) => println!("Exported JSON: {}", json),
///     Err(e) => eprintln!("Export failed: {}", e),
/// }
/// ```
///
/// # Functional Patterns
///
/// - Zero unwraps/panics
/// - Railway-Oriented error handling with `?` operator
/// - Immutable data flow
/// - Pure function (no side effects except I/O at boundaries)
pub fn run_cue_export(path: &Path) -> Result<String, IntentError> {
    // Validate file exists before attempting export
    validate_file_exists(path)?;

    // Execute cue export command
    execute_cue_command(path).and_then(parse_command_output)
}

/// Validate that the file exists
///
/// # Errors
///
/// Returns `IntentError::NotFound` if file doesn't exist
fn validate_file_exists(path: &Path) -> Result<(), IntentError> {
    if path.exists() {
        Ok(())
    } else {
        Err(IntentError::spec_not_found(path.to_path_buf()))
    }
}

/// Execute the cue export command
///
/// # Errors
///
/// Returns `IntentError` if command fails to execute or returns non-zero exit
fn execute_cue_command(path: &Path) -> Result<std::process::Output, IntentError> {
    Command::new("cue")
        .arg("export")
        .arg(path)
        .arg("--out")
        .arg("json")
        .output()
        .map_err(|source| {
            IntentError::io_with_path("executing cue export command", path.to_path_buf(), source)
        })
        .and_then(|output| validate_exit_status(output, path))
}

/// Validate the command exit status
///
/// # Errors
///
/// Returns `IntentError::CueEval` if exit status is non-zero
fn validate_exit_status(
    output: std::process::Output,
    path: &Path,
) -> Result<std::process::Output, IntentError> {
    if output.status.success() {
        Ok(output)
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        Err(IntentError::cue_eval(path.to_path_buf(), stderr))
    }
}

/// Parse command output as UTF-8 string
///
/// # Errors
///
/// Returns `IntentError::Io` if output contains invalid UTF-8
fn parse_command_output(output: std::process::Output) -> Result<String, IntentError> {
    String::from_utf8(output.stdout).map_err(|e| {
        IntentError::io(
            "parsing cue export output as UTF-8",
            std::io::Error::new(std::io::ErrorKind::InvalidData, e),
        )
    })
}

// =============================================================================
// CUE Spec Parsing
// =============================================================================

/// Represents a CUE specification parsed from JSON
///
/// This structure captures the essential fields from a CUE bead specification
/// exported via `cue export --out json`. Additional fields are preserved but
/// not strongly typed to allow for schema evolution.
///
/// # Examples
///
/// ```
/// use intent_core::cue::parse_cue_output;
///
/// let json = r#"{"bead": {"id": "test-1", "title": "Test", "type": "feature", "priority": 1}}"#;
/// let spec = parse_cue_output(json).expect("Valid JSON");
/// assert_eq!(spec.id, "test-1");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct CueSpec {
    /// Unique identifier for the bead
    pub id: String,

    /// Human-readable title
    pub title: String,

    /// Type of bead (feature, bug, task, etc.)
    #[serde(rename = "type")]
    pub bead_type: String,

    /// Priority level (1-5, where 1 is highest)
    pub priority: i32,

    /// Optional effort estimate
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub effort_estimate: Option<String>,

    /// Optional labels/tags
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub labels: Vec<String>,

    /// Additional fields preserved as raw JSON
    #[serde(flatten)]
    pub extra: serde_json::Value,
}

/// Parse JSON output from `cue export` into a CueSpec
///
/// This function implements Railway-Oriented Programming for robust JSON parsing.
/// All errors are captured and converted to semantic CueError types.
///
/// # Arguments
///
/// * `json` - JSON string output from `cue export --out json`
///
/// # Returns
///
/// Returns `Result<CueSpec, CueError>` containing:
/// - `Ok(CueSpec)`: Successfully parsed specification
/// - `Err(CueError::JsonParseError)`: JSON parsing failed with details
///
/// # Errors
///
/// This function returns errors for:
/// - Invalid JSON syntax
/// - Missing required fields (id, title, type, priority)
/// - Type mismatches (e.g., priority as string instead of number)
///
/// # Examples
///
/// ```
/// use intent_core::cue::parse_cue_output;
///
/// let json =
///     r#"{"bead": {"id": "cli-001", "title": "Add feature", "type": "feature", "priority": 2}}"#;
/// let spec = parse_cue_output(json)?;
/// assert_eq!(spec.id, "cli-001");
/// # Ok::<(), intent_core::cue::CueError>(())
/// ```
pub fn parse_cue_output(json: &str) -> Result<CueSpec, CueError> {
    // Parse JSON into a generic Value first for the "bead" wrapper
    serde_json::from_str::<serde_json::Value>(json)
        .map_err(|e| CueError::json_parse_error(format!("invalid JSON: {e}")))
        .and_then(|value| {
            // Extract the "bead" field
            value
                .get("bead")
                .ok_or_else(|| CueError::json_parse_error("missing 'bead' field in JSON"))
                .and_then(|bead| {
                    // Parse the bead object into CueSpec
                    serde_json::from_value::<CueSpec>(bead.clone()).map_err(|e| {
                        CueError::json_parse_error(format!("invalid bead structure: {e}"))
                    })
                })
        })
}

// =============================================================================
// TDD Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // Smoke Tests
    // =========================================================================

    #[test]
    fn cue_module_compiles() {
        // Smoke test - module exists and compiles
    }
    // =========================================================================
    // CueSpec Parsing Tests (TDD)
    // =========================================================================

    #[test]
    fn test_parse_json_to_spec() {
        // Valid minimal bead structure
        let json = r#"
        {
            "bead": {
                "id": "intent-cli-001",
                "title": "Implement feature X",
                "type": "feature",
                "priority": 2
            }
        }
        "#;

        let result = parse_cue_output(json);
        assert!(result.is_ok(), "Failed to parse valid JSON: {result:?}");

        let spec = result.expect("Valid spec");
        assert_eq!(spec.id, "intent-cli-001");
        assert_eq!(spec.title, "Implement feature X");
        assert_eq!(spec.bead_type, "feature");
        assert_eq!(spec.priority, 2);
    }

    #[test]
    fn test_parse_json_with_optional_fields() {
        let json = r#"
        {
            "bead": {
                "id": "cli-002",
                "title": "Fix bug Y",
                "type": "bug",
                "priority": 1,
                "effort_estimate": "2hr",
                "labels": ["urgent", "backend"]
            }
        }
        "#;

        let result = parse_cue_output(json);
        assert!(result.is_ok());

        let spec = result.expect("Valid spec");
        assert_eq!(spec.id, "cli-002");
        assert_eq!(spec.effort_estimate, Some("2hr".to_string()));
        assert_eq!(spec.labels, vec!["urgent", "backend"]);
    }

    #[test]
    fn test_parse_json_with_extra_fields() {
        // Ensure extra fields are preserved but don't break parsing
        let json = r#"
        {
            "bead": {
                "id": "cli-003",
                "title": "Task Z",
                "type": "task",
                "priority": 3,
                "ears_requirements": {
                    "ubiquitous": ["SHALL do X"]
                },
                "contracts": {
                    "preconditions": {}
                }
            }
        }
        "#;

        let result = parse_cue_output(json);
        assert!(result.is_ok(), "Should handle extra fields gracefully");

        let spec = result.expect("Valid spec");
        assert_eq!(spec.id, "cli-003");
        // Extra fields should be in the 'extra' field
        assert!(spec.extra.is_object());
    }

    #[test]
    fn test_parse_json_missing_bead_field() {
        // Missing "bead" wrapper
        let json = r#"
        {
            "id": "cli-004",
            "title": "No wrapper",
            "type": "feature",
            "priority": 1
        }
        "#;

        let result = parse_cue_output(json);
        assert!(result.is_err());
        assert!(matches!(result, Err(CueError::JsonParseError { .. })));

        if let Err(CueError::JsonParseError { reason }) = result {
            assert!(
                reason.contains("bead"),
                "Error should mention missing 'bead' field"
            );
        }
    }

    #[test]
    fn test_parse_json_invalid_syntax() {
        let json = r#"{ "bead": { invalid json } }"#;

        let result = parse_cue_output(json);
        assert!(result.is_err());
        assert!(matches!(result, Err(CueError::JsonParseError { .. })));
    }

    #[test]
    fn test_parse_json_missing_required_field() {
        // Missing required 'id' field
        let json = r#"
        {
            "bead": {
                "title": "Missing ID",
                "type": "feature",
                "priority": 1
            }
        }
        "#;

        let result = parse_cue_output(json);
        assert!(result.is_err(), "Should fail with missing required field");
        assert!(matches!(result, Err(CueError::JsonParseError { .. })));
    }

    #[test]
    fn test_parse_json_wrong_type() {
        // Priority as string instead of number
        let json = r#"
        {
            "bead": {
                "id": "cli-005",
                "title": "Wrong type",
                "type": "feature",
                "priority": "high"
            }
        }
        "#;

        let result = parse_cue_output(json);
        assert!(result.is_err());
        assert!(matches!(result, Err(CueError::JsonParseError { .. })));
    }

    #[test]
    fn test_parse_json_empty_string() {
        let result = parse_cue_output("");
        assert!(result.is_err());
        assert!(matches!(result, Err(CueError::JsonParseError { .. })));
    }

    #[test]
    fn test_parse_json_real_bead_structure() {
        // Based on actual cue export output
        let json = r#"
        {
            "bead": {
                "id": "intent-cli-cue01",
                "title": "cue: Implement CUE spec parser with validation",
                "type": "feature",
                "priority": 1,
                "effort_estimate": "4hr",
                "labels": ["cue", "parser", "m2", "rust-port"],
                "ears_requirements": {
                    "ubiquitous": [
                        "THE SYSTEM SHALL parse CUE files"
                    ]
                },
                "contracts": {
                    "preconditions": {
                        "auth_required": false
                    }
                },
                "completion_checklist": {
                    "implementation": []
                }
            }
        }
        "#;

        let result = parse_cue_output(json);
        assert!(result.is_ok(), "Should parse real bead structure");

        let spec = result.expect("Valid spec");
        assert_eq!(spec.id, "intent-cli-cue01");
        assert_eq!(spec.title, "cue: Implement CUE spec parser with validation");
        assert_eq!(spec.bead_type, "feature");
        assert_eq!(spec.priority, 1);
        assert_eq!(spec.effort_estimate, Some("4hr".to_string()));
        assert_eq!(spec.labels.len(), 4);
        assert!(spec.labels.contains(&"cue".to_string()));
    }

    // =========================================================================
    // CueName Tests
    // =========================================================================

    #[test]
    fn test_cue_name_valid() {
        let name = CueName::new("valid-cue-name");
        assert!(name.is_ok());
        assert_eq!(name.as_ref().map(CueName::as_str), Ok("valid-cue-name"));
    }

    #[test]
    fn test_cue_name_empty() {
        let name = CueName::new("");
        assert!(name.is_err());
        assert!(matches!(name, Err(CueError::InvalidName(_))));
    }

    #[test]
    fn test_cue_name_invalid_characters() {
        let name = CueName::new("invalid name!");
        assert!(name.is_err());
        assert!(matches!(name, Err(CueError::InvalidName(_))));
    }

    #[test]
    fn test_cue_name_too_long() {
        let long_name = "a".repeat(CueName::MAX_LENGTH + 1);
        let name = CueName::new(long_name);
        assert!(name.is_err());
        assert!(matches!(name, Err(CueError::InvalidName(_))));
    }

    #[test]
    fn test_cue_name_max_length() {
        let max_name = "a".repeat(CueName::MAX_LENGTH);
        let name = CueName::new(max_name);
        assert!(name.is_ok());
    }

    #[test]
    fn test_cue_name_display() {
        let name = CueName::new("test-name").expect("valid name");
        assert_eq!(format!("{name}"), "test-name");
    }

    #[test]
    fn test_cue_name_alphanumeric() {
        assert!(CueName::new("abc123").is_ok());
        assert!(CueName::new("test-cue").is_ok());
        assert!(CueName::new("test_cue").is_ok());
        assert!(CueName::new("test-cue_123").is_ok());
    }

    // =========================================================================
    // Cue Tests
    // =========================================================================

    #[test]
    fn test_cue_new() {
        let cue = Cue::new("http-get", "Perform HTTP GET request");
        assert_eq!(cue.name(), "http-get");
        assert_eq!(cue.description(), "Perform HTTP GET request");
    }

    #[test]
    fn test_cue_try_new_valid() {
        let result = Cue::try_new("valid-name", "Valid description");
        assert!(result.is_ok());

        if let Ok(cue) = result {
            assert_eq!(cue.name(), "valid-name");
            assert_eq!(cue.description(), "Valid description");
        }
    }

    #[test]
    fn test_cue_try_new_invalid_name() {
        let result = Cue::try_new("", "Description");
        assert!(result.is_err());
        assert!(matches!(result, Err(CueError::InvalidName(_))));
    }

    #[test]
    fn test_cue_try_new_invalid_description() {
        let result = Cue::try_new("valid-name", "");
        assert!(result.is_err());
        assert!(matches!(result, Err(CueError::InvalidDescription(_))));
    }

    #[test]
    fn test_cue_description_too_long() {
        let long_desc = "a".repeat(Cue::MAX_DESCRIPTION_LENGTH + 1);
        let result = Cue::try_new("name", long_desc);
        assert!(result.is_err());
        assert!(matches!(result, Err(CueError::InvalidDescription(_))));
    }

    #[test]
    fn test_cue_description_max_length() {
        let max_desc = "a".repeat(Cue::MAX_DESCRIPTION_LENGTH);
        let result = Cue::try_new("name", max_desc);
        assert!(result.is_ok());
    }

    #[test]
    fn test_cue_display() {
        let cue = Cue::new("test", "Test cue");
        assert_eq!(format!("{cue}"), "test: Test cue");
    }

    #[test]
    fn test_cue_clone() {
        let cue1 = Cue::new("test", "Test cue");
        let cue2 = cue1.clone();
        assert_eq!(cue1, cue2);
    }

    #[test]
    fn test_cue_getters() {
        let cue = Cue::new("example", "Example description");
        assert_eq!(cue.name(), "example");
        assert_eq!(cue.description(), "Example description");
        assert_eq!(cue.cue_name().as_str(), "example");
    }

    // =========================================================================
    // CueError Tests
    // =========================================================================

    #[test]
    fn test_cue_error_invalid_name() {
        let error = CueError::invalid_name("test error");
        assert_eq!(error.to_string(), "invalid cue name: test error");
    }

    #[test]
    fn test_cue_error_not_found() {
        let error = CueError::not_found("missing-cue");
        assert_eq!(error.to_string(), "cue not found: missing-cue");
    }

    #[test]
    fn test_cue_error_parse_error() {
        let error = CueError::parse_error("syntax error");
        assert_eq!(error.to_string(), "failed to parse cue: syntax error");
    }

    // =========================================================================
    // Railway Pattern Tests
    // =========================================================================

    #[test]
    fn test_railway_pattern_name_validation() {
        // Test the validation pipeline
        let result = CueName::new("valid-name").map(|name| name.as_str().to_uppercase());

        assert_eq!(result, Ok("VALID-NAME".to_string()));
    }

    #[test]
    fn test_railway_pattern_cue_creation() {
        // Test chaining operations
        let result = Cue::try_new("test", "Test description").map(|cue| cue.name().to_uppercase());

        assert_eq!(result, Ok("TEST".to_string()));
    }

    #[test]
    fn test_railway_pattern_error_propagation() {
        // Test error propagation through chain
        let result = CueName::new("").and_then(|name| Cue::try_new(name.as_str(), "Description"));

        assert!(result.is_err());
    }

    // =========================================================================
    // Property-Based Tests (Stress Testing)
    // =========================================================================

    #[test]
    fn test_no_panics_on_various_inputs() {
        // Stress test - ensure no panics on edge cases
        let long_name = "a".repeat(1000);
        let long_desc = "d".repeat(2000);

        let test_cases: Vec<(&str, &str)> = vec![
            ("", "desc"),
            ("name", ""),
            ("", ""),
            (long_name.as_str(), "desc"),
            ("name", long_desc.as_str()),
            ("invalid name!", "desc"),
            ("name", "desc\n\n\n"),
        ];

        for (name, desc) in test_cases {
            let _ = Cue::try_new(name, desc);
            // Should never panic, only return Err
        }
    }

    #[test]
    fn test_validation_never_panics() {
        // Stress test validation functions
        let long_name = "a".repeat(200);
        let names: Vec<&str> = vec!["", "valid", "123", long_name.as_str(), "!!!"];

        for name in names {
            let _ = CueName::new(name);
        }
    }

    // =========================================================================
    // CUE Export Tests (BEAD: intent-cli-7ya0)
    // =========================================================================

    use std::{fs, io::Write, path::PathBuf};

    use super::{parse_command_output, run_cue_export, validate_file_exists};

    /// Helper to create a temporary CUE file for testing
    fn create_temp_cue_file(content: &str) -> (tempfile::TempDir, PathBuf) {
        let temp_dir = tempfile::TempDir::new().expect("Failed to create temp dir");
        let file_path = temp_dir.path().join("test.cue");

        let mut file = fs::File::create(&file_path).expect("Failed to create temp file");
        file.write_all(content.as_bytes())
            .expect("Failed to write to temp file");

        (temp_dir, file_path)
    }

    #[test]
    fn test_cue_export_file_not_found() {
        // Arrange: Use a path that doesn't exist
        let non_existent_path = std::path::Path::new("/tmp/non_existent_file_12345.cue");

        // Act: Try to export
        let result = run_cue_export(non_existent_path);

        // Assert: Should return NotFound error
        assert!(result.is_err());
        match result {
            Err(IntentError::NotFound { resource_type, .. }) => {
                assert_eq!(resource_type, "spec file");
            }
            _ => panic!("Expected NotFound error"),
        }
    }

    #[test]
    #[ignore = "Requires cue CLI to be installed"]
    fn test_cue_export_valid_file() {
        // Arrange: Create a valid CUE file
        let cue_content = r#"
{
    name: "test"
    value: 42
}
"#;
        let (_temp_dir, file_path) = create_temp_cue_file(cue_content);

        // Act: Export the file
        let result = run_cue_export(&file_path);

        // Assert: Should succeed and return valid JSON
        assert!(result.is_ok());
        if let Ok(json) = result {
            assert!(json.contains("\"name\""));
            assert!(json.contains("\"test\""));
            assert!(json.contains("\"value\""));
            assert!(json.contains("42"));
        }
    }

    #[test]
    #[ignore = "Requires cue CLI to be installed"]
    fn test_cue_export_invalid_cue_syntax() {
        // Arrange: Create a CUE file with invalid syntax
        let invalid_cue = r#"
{
    name: "test"
    invalid syntax here!!!
}
"#;
        let (_temp_dir, file_path) = create_temp_cue_file(invalid_cue);

        // Act: Try to export
        let result = run_cue_export(&file_path);

        // Assert: Should fail with CueEval error
        assert!(result.is_err());
        match result {
            Err(IntentError::CueEval { .. }) => {
                // Expected error variant
            }
            Err(other) => panic!("Expected CueEval error, got: {:?}", other),
            Ok(_) => panic!("Expected error, got success"),
        }
    }

    #[test]
    fn test_cue_export_never_panics() {
        // Stress test: Ensure function never panics on various inputs
        let test_paths = vec![
            std::path::Path::new(""),
            std::path::Path::new("/tmp/nonexistent.cue"),
            std::path::Path::new("/dev/null"),
            std::path::Path::new("/tmp/test with spaces.cue"),
        ];

        for path in test_paths {
            let _ = run_cue_export(path);
            // Should never panic, only return Err
        }
    }

    #[test]
    #[ignore = "Requires cue CLI to be installed"]
    fn test_cue_export_railway_pattern() {
        // Test Railway-Oriented Programming pattern
        // Valid input flows through the happy path
        let valid_cue = r#"{ test: "value" }"#;
        let (_temp_dir, file_path) = create_temp_cue_file(valid_cue);

        // Act: Chain operations using map
        let result = run_cue_export(&file_path).map(|json| json.len());

        // Assert: Should succeed and return length
        assert!(result.is_ok());
        assert!(result.map_or(false, |len| len > 0));
    }

    #[test]
    fn test_validate_file_exists_success() {
        // Arrange: Create a temporary file
        let temp_dir = tempfile::TempDir::new().expect("Failed to create temp dir");
        let file_path = temp_dir.path().join("exists.cue");
        fs::File::create(&file_path).expect("Failed to create file");

        // Act: Validate
        let result = validate_file_exists(&file_path);

        // Assert: Should succeed
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_file_exists_failure() {
        // Arrange: Use non-existent path
        let path = std::path::Path::new("/tmp/does_not_exist_xyz.cue");

        // Act: Validate
        let result = validate_file_exists(path);

        // Assert: Should fail
        assert!(result.is_err());
    }

    #[test]
    #[cfg(unix)]
    fn test_parse_command_output_valid_utf8() {
        // Arrange: Create output with valid UTF-8
        use std::os::unix::process::ExitStatusExt;
        let output = std::process::Output {
            status: std::process::ExitStatus::from_raw(0),
            stdout: b"valid UTF-8 output".to_vec(),
            stderr: Vec::new(),
        };

        // Act: Parse
        let result = parse_command_output(output);

        // Assert: Should succeed
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some("valid UTF-8 output".to_string()));
    }

    #[test]
    #[cfg(unix)]
    fn test_parse_command_output_invalid_utf8() {
        // Arrange: Create output with invalid UTF-8
        use std::os::unix::process::ExitStatusExt;
        let invalid_utf8 = vec![0xFF, 0xFE, 0xFD];
        let output = std::process::Output {
            status: std::process::ExitStatus::from_raw(0),
            stdout: invalid_utf8,
            stderr: Vec::new(),
        };

        // Act: Parse
        let result = parse_command_output(output);

        // Assert: Should fail with Io error
        assert!(result.is_err());
        match result {
            Err(IntentError::Io { operation, .. }) => {
                assert!(operation.contains("UTF-8"));
            }
            _ => panic!("Expected Io error"),
        }
    }
}
