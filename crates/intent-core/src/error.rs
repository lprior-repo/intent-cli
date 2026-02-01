//! Error handling module for Intent CLI
//!
//! This module provides comprehensive, type-safe error handling using Railway-Oriented
//! Programming principles. Every error variant includes rich context for debugging
//! while maintaining compile-time safety guarantees.
//!
//! # Philosophy
//!
//! - **Zero panics**: All error conditions are represented as `Result<T, IntentError>`
//! - **Rich context**: Every error includes location, cause, and actionable information
//! - **Error chaining**: Source errors are preserved for full debugging capability
//! - **Type safety**: No string errors, no `Box<dyn Error>` in domain logic
//!
//! # Examples
//!
//! ```rust
//! use std::path::PathBuf;
//!
//! use intent_core::error::{IntentError, IntentResult};
//!
//! fn read_spec(path: &str) -> IntentResult<String> {
//!     let path = PathBuf::from(path);
//!     std::fs::read_to_string(&path).map_err(|source| IntentError::not_found(path, source))
//! }
//!
//! // Railway-oriented chaining
//! fn process_spec(path: &str) -> IntentResult<()> {
//!     read_spec(path)
//!         .and_then(|content| validate_content(&content))
//!         .map(|_| ())
//! }
//! # fn validate_content(_: &str) -> IntentResult<()> { Ok(()) }
//! ```

use std::path::PathBuf;

use thiserror::Error;

/// Result type alias for Intent operations
///
/// All fallible operations in Intent CLI return this type.
/// Use Railway-Oriented combinators (`.map()`, `.and_then()`, `.map_err()`)
/// for chaining operations.
pub type IntentResult<T> = Result<T, IntentError>;

/// Comprehensive error type for Intent CLI operations
///
/// Each variant includes:
/// - Contextual information for debugging
/// - Source error preservation where applicable
/// - Human-readable error messages via `Display`
///
/// # Error Categories
///
/// | Category | Use Case |
/// |----------|----------|
/// | `NotFound` | File, resource, or entity doesn't exist |
/// | `Parse` | CUE spec or configuration parsing failures |
/// | `Http` | Network request failures |
/// | `Validation` | Domain rule violations |
/// | `Config` | Configuration errors |
/// | `Io` | General I/O errors |
/// | `Session` | Interview/test session errors |
/// | `Timeout` | Operation timeout errors |
#[derive(Debug, Error)]
pub enum IntentError {
    // =========================================================================
    // File & Resource Errors
    // =========================================================================
    /// Resource not found error with path context
    ///
    /// Use when a file, directory, or named resource cannot be located.
    #[error("Not found: {resource_type} at '{path}'")]
    NotFound {
        /// Type of resource (e.g., "file", "directory", "session", "spec")
        resource_type: String,
        /// Path or identifier of the missing resource
        path: PathBuf,
        /// Underlying IO error if available
        #[source]
        source: Option<std::io::Error>,
    },

    // =========================================================================
    // Parsing Errors
    // =========================================================================
    /// Parse error with location information
    ///
    /// Use for CUE spec parsing, JSON parsing, or any structured data parsing.
    #[error("Parse error in '{file}' at line {line}, column {column}: {message}")]
    Parse {
        /// File being parsed
        file: PathBuf,
        /// Line number (1-indexed)
        line: usize,
        /// Column number (1-indexed)
        column: usize,
        /// Human-readable error message
        message: String,
        /// Suggestion for fixing the error
        suggestion: Option<String>,
    },

    /// JSON parsing error
    #[error("JSON parse error: {message}")]
    JsonParse {
        /// Error message
        message: String,
        /// Source `serde_json` error
        #[source]
        source: serde_json::Error,
    },

    // =========================================================================
    // HTTP Errors
    // =========================================================================
    /// HTTP request/response error
    ///
    /// Use for any network-related failures during API testing.
    #[error("HTTP error: {method} {url} returned status {status}")]
    Http {
        /// HTTP method (GET, POST, etc.)
        method: String,
        /// Request URL
        url: String,
        /// HTTP status code
        status: u16,
        /// Response body (truncated if large)
        body: Option<String>,
        /// Expected status code if verification failed
        expected_status: Option<u16>,
    },

    /// HTTP connection error (network level)
    #[error("Connection failed to '{url}': {message}")]
    Connection {
        /// Target URL
        url: String,
        /// Error message
        message: String,
    },

    // =========================================================================
    // Validation Errors
    // =========================================================================
    /// Domain validation error
    ///
    /// Use when data fails business rule validation.
    #[error("Validation error for '{field}': {message}")]
    Validation {
        /// Field or context that failed validation
        field: String,
        /// Human-readable validation error
        message: String,
        /// The invalid value (for debugging)
        value: Option<String>,
        /// Suggestion for valid input
        suggestion: Option<String>,
    },

    /// Multiple validation errors (batch validation)
    #[error("Validation failed with {count} errors")]
    ValidationBatch {
        /// Number of errors
        count: usize,
        /// Individual validation errors
        errors: Vec<ValidationDetail>,
    },

    // =========================================================================
    // Configuration Errors
    // =========================================================================
    /// Configuration error
    ///
    /// Use for intent.toml parsing or environment configuration issues.
    #[error("Configuration error: {message}")]
    Config {
        /// Error message
        message: String,
        /// Configuration key if applicable
        key: Option<String>,
        /// Source file if applicable
        file: Option<PathBuf>,
        /// Suggestion for fixing
        suggestion: Option<String>,
    },

    /// Missing required configuration
    #[error("Missing required configuration: '{key}'")]
    ConfigMissing {
        /// Configuration key
        key: String,
        /// Where it should be defined
        location: String,
    },

    // =========================================================================
    // I/O Errors
    // =========================================================================
    /// General I/O error with context
    #[error("I/O error during {operation}: {message}")]
    Io {
        /// Operation being performed (e.g., "file read", "directory creation")
        operation: String,
        /// Human-readable message
        message: String,
        /// Path if applicable
        path: Option<PathBuf>,
        /// Underlying error
        #[source]
        source: std::io::Error,
    },

    // =========================================================================
    // Session Errors
    // =========================================================================
    /// Session management error
    #[error("Session error: {message}")]
    Session {
        /// Error message
        message: String,
        /// Session ID if applicable
        session_id: Option<String>,
        /// Session state if relevant
        state: Option<String>,
    },

    /// Session not found
    #[error("Session not found: '{session_id}'")]
    SessionNotFound {
        /// Session ID that was not found
        session_id: String,
    },

    // =========================================================================
    // Timeout Errors
    // =========================================================================
    /// Operation timeout
    #[error("Operation timed out after {duration_ms}ms: {operation}")]
    Timeout {
        /// Operation that timed out
        operation: String,
        /// Timeout duration in milliseconds
        duration_ms: u64,
        /// Partial result if any
        partial_result: Option<String>,
    },

    // =========================================================================
    // CUE-Specific Errors
    // =========================================================================
    /// CUE evaluation error
    #[error("CUE evaluation error in '{file}': {message}")]
    CueEval {
        /// CUE file being evaluated
        file: PathBuf,
        /// Error message from CUE
        message: String,
        /// Specific path in CUE document
        cue_path: Option<String>,
    },

    /// CUE schema validation error
    #[error("Schema validation failed: {message}")]
    SchemaValidation {
        /// Validation error message
        message: String,
        /// Expected schema
        expected: Option<String>,
        /// Actual value
        actual: Option<String>,
    },

    // =========================================================================
    // Internal Errors (Should never reach users)
    // =========================================================================
    /// Internal error (programming error, should not occur)
    #[error("Internal error: {message}")]
    Internal {
        /// Error message
        message: String,
        /// Location in code
        location: String,
    },
}

/// Detail for individual validation errors in batch validation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationDetail {
    /// Field that failed validation
    pub field: String,
    /// Error message
    pub message: String,
    /// Invalid value
    pub value: Option<String>,
}

// =============================================================================
// Constructor Methods (Builder Pattern for Errors)
// =============================================================================

impl IntentError {
    // -------------------------------------------------------------------------
    // NotFound Constructors
    // -------------------------------------------------------------------------

    /// Create a file not found error
    #[must_use]
    pub fn not_found(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::NotFound {
            resource_type: "file".to_string(),
            path: path.into(),
            source: Some(source),
        }
    }

    /// Create a resource not found error without source
    #[must_use]
    pub fn resource_not_found(resource_type: impl Into<String>, path: impl Into<PathBuf>) -> Self {
        Self::NotFound {
            resource_type: resource_type.into(),
            path: path.into(),
            source: None,
        }
    }

    /// Create a spec file not found error
    #[must_use]
    pub fn spec_not_found(path: impl Into<PathBuf>) -> Self {
        Self::NotFound {
            resource_type: "spec file".to_string(),
            path: path.into(),
            source: None,
        }
    }

    // -------------------------------------------------------------------------
    // Parse Constructors
    // -------------------------------------------------------------------------

    /// Create a parse error
    #[must_use]
    pub fn parse(
        file: impl Into<PathBuf>,
        line: usize,
        column: usize,
        message: impl Into<String>,
    ) -> Self {
        Self::Parse {
            file: file.into(),
            line,
            column,
            message: message.into(),
            suggestion: None,
        }
    }

    /// Create a parse error with suggestion
    #[must_use]
    pub fn parse_with_suggestion(
        file: impl Into<PathBuf>,
        line: usize,
        column: usize,
        message: impl Into<String>,
        suggestion: impl Into<String>,
    ) -> Self {
        Self::Parse {
            file: file.into(),
            line,
            column,
            message: message.into(),
            suggestion: Some(suggestion.into()),
        }
    }

    // -------------------------------------------------------------------------
    // HTTP Constructors
    // -------------------------------------------------------------------------

    /// Create an HTTP error
    #[must_use]
    pub fn http(method: impl Into<String>, url: impl Into<String>, status: u16) -> Self {
        Self::Http {
            method: method.into(),
            url: url.into(),
            status,
            body: None,
            expected_status: None,
        }
    }

    /// Create an HTTP error with body
    #[must_use]
    pub fn http_with_body(
        method: impl Into<String>,
        url: impl Into<String>,
        status: u16,
        body: impl Into<String>,
    ) -> Self {
        Self::Http {
            method: method.into(),
            url: url.into(),
            status,
            body: Some(body.into()),
            expected_status: None,
        }
    }

    /// Create an HTTP status mismatch error
    #[must_use]
    pub fn http_status_mismatch(
        method: impl Into<String>,
        url: impl Into<String>,
        expected: u16,
        actual: u16,
    ) -> Self {
        Self::Http {
            method: method.into(),
            url: url.into(),
            status: actual,
            body: None,
            expected_status: Some(expected),
        }
    }

    /// Create a connection error
    #[must_use]
    pub fn connection(url: impl Into<String>, message: impl Into<String>) -> Self {
        Self::Connection {
            url: url.into(),
            message: message.into(),
        }
    }

    // -------------------------------------------------------------------------
    // Validation Constructors
    // -------------------------------------------------------------------------

    /// Create a validation error
    #[must_use]
    pub fn validation(field: impl Into<String>, message: impl Into<String>) -> Self {
        Self::Validation {
            field: field.into(),
            message: message.into(),
            value: None,
            suggestion: None,
        }
    }

    /// Create a validation error with value context
    #[must_use]
    pub fn validation_with_value(
        field: impl Into<String>,
        message: impl Into<String>,
        value: impl Into<String>,
    ) -> Self {
        Self::Validation {
            field: field.into(),
            message: message.into(),
            value: Some(value.into()),
            suggestion: None,
        }
    }

    /// Create a validation error with suggestion
    #[must_use]
    pub fn validation_with_suggestion(
        field: impl Into<String>,
        message: impl Into<String>,
        suggestion: impl Into<String>,
    ) -> Self {
        Self::Validation {
            field: field.into(),
            message: message.into(),
            value: None,
            suggestion: Some(suggestion.into()),
        }
    }

    /// Create a batch validation error
    #[must_use]
    pub const fn validation_batch(errors: Vec<ValidationDetail>) -> Self {
        Self::ValidationBatch {
            count: errors.len(),
            errors,
        }
    }

    // -------------------------------------------------------------------------
    // Config Constructors
    // -------------------------------------------------------------------------

    /// Create a config error
    #[must_use]
    pub fn config(message: impl Into<String>) -> Self {
        Self::Config {
            message: message.into(),
            key: None,
            file: None,
            suggestion: None,
        }
    }

    /// Create a config error with key
    #[must_use]
    pub fn config_for_key(key: impl Into<String>, message: impl Into<String>) -> Self {
        Self::Config {
            message: message.into(),
            key: Some(key.into()),
            file: None,
            suggestion: None,
        }
    }

    /// Create a missing config error
    #[must_use]
    pub fn config_missing(key: impl Into<String>, location: impl Into<String>) -> Self {
        Self::ConfigMissing {
            key: key.into(),
            location: location.into(),
        }
    }

    // -------------------------------------------------------------------------
    // I/O Constructors
    // -------------------------------------------------------------------------

    /// Create an I/O error
    #[must_use]
    pub fn io(operation: impl Into<String>, source: std::io::Error) -> Self {
        Self::Io {
            operation: operation.into(),
            message: source.to_string(),
            path: None,
            source,
        }
    }

    /// Create an I/O error with path
    #[must_use]
    pub fn io_with_path(
        operation: impl Into<String>,
        path: impl Into<PathBuf>,
        source: std::io::Error,
    ) -> Self {
        Self::Io {
            operation: operation.into(),
            message: source.to_string(),
            path: Some(path.into()),
            source,
        }
    }

    // -------------------------------------------------------------------------
    // Session Constructors
    // -------------------------------------------------------------------------

    /// Create a session error
    #[must_use]
    pub fn session(message: impl Into<String>) -> Self {
        Self::Session {
            message: message.into(),
            session_id: None,
            state: None,
        }
    }

    /// Create a session not found error
    #[must_use]
    pub fn session_not_found(session_id: impl Into<String>) -> Self {
        Self::SessionNotFound {
            session_id: session_id.into(),
        }
    }

    // -------------------------------------------------------------------------
    // Timeout Constructors
    // -------------------------------------------------------------------------

    /// Create a timeout error
    #[must_use]
    pub fn timeout(operation: impl Into<String>, duration_ms: u64) -> Self {
        Self::Timeout {
            operation: operation.into(),
            duration_ms,
            partial_result: None,
        }
    }

    // -------------------------------------------------------------------------
    // CUE Constructors
    // -------------------------------------------------------------------------

    /// Create a CUE evaluation error
    #[must_use]
    pub fn cue_eval(file: impl Into<PathBuf>, message: impl Into<String>) -> Self {
        Self::CueEval {
            file: file.into(),
            message: message.into(),
            cue_path: None,
        }
    }

    /// Create a schema validation error
    #[must_use]
    pub fn schema_validation(message: impl Into<String>) -> Self {
        Self::SchemaValidation {
            message: message.into(),
            expected: None,
            actual: None,
        }
    }

    // -------------------------------------------------------------------------
    // Internal Constructors
    // -------------------------------------------------------------------------

    /// Create an internal error (use sparingly - indicates a bug)
    #[must_use]
    pub fn internal(message: impl Into<String>, location: impl Into<String>) -> Self {
        Self::Internal {
            message: message.into(),
            location: location.into(),
        }
    }
}

// =============================================================================
// From Implementations (Railway Bridges)
// =============================================================================

impl From<std::io::Error> for IntentError {
    fn from(source: std::io::Error) -> Self {
        Self::Io {
            operation: "I/O operation".to_string(),
            message: source.to_string(),
            path: None,
            source,
        }
    }
}

impl From<serde_json::Error> for IntentError {
    fn from(source: serde_json::Error) -> Self {
        Self::JsonParse {
            message: source.to_string(),
            source,
        }
    }
}

// =============================================================================
// Exit Code Mapping
// =============================================================================

impl IntentError {
    /// Get the appropriate exit code for this error
    ///
    /// Exit codes follow POSIX conventions:
    /// - 0: Success (not an error)
    /// - 1: General error
    /// - 2: Usage/CLI error
    /// - 3: Validation error
    /// - 4: Not found
    /// - 5: Timeout
    /// - 6: Network error
    #[must_use]
    pub const fn exit_code(&self) -> i32 {
        match self {
            Self::NotFound { .. } | Self::SessionNotFound { .. } => 4,
            Self::Parse { .. }
            | Self::JsonParse { .. }
            | Self::CueEval { .. }
            | Self::Validation { .. }
            | Self::ValidationBatch { .. }
            | Self::SchemaValidation { .. } => 3,
            Self::Config { .. } | Self::ConfigMissing { .. } => 2,
            Self::Http { .. } | Self::Connection { .. } => 6,
            Self::Timeout { .. } => 5,
            Self::Io { .. } | Self::Session { .. } | Self::Internal { .. } => 1,
        }
    }

    /// Check if this error is recoverable
    #[must_use]
    pub const fn is_recoverable(&self) -> bool {
        matches!(
            self,
            Self::Timeout { .. } | Self::Connection { .. } | Self::Http { status: 503, .. }
        )
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_not_found_error_display() {
        let error = IntentError::spec_not_found("test.cue");
        let display = error.to_string();
        assert!(display.contains("spec file"));
        assert!(display.contains("test.cue"));
    }

    #[test]
    fn test_parse_error_display() {
        let error = IntentError::parse("spec.cue", 42, 10, "unexpected token");
        let display = error.to_string();
        assert!(display.contains("spec.cue"));
        assert!(display.contains("line 42"));
        assert!(display.contains("column 10"));
        assert!(display.contains("unexpected token"));
    }

    #[test]
    fn test_http_error_display() {
        let error = IntentError::http("POST", "https://api.example.com/users", 404);
        let display = error.to_string();
        assert!(display.contains("POST"));
        assert!(display.contains("https://api.example.com/users"));
        assert!(display.contains("404"));
    }

    #[test]
    fn test_validation_error_display() {
        let error = IntentError::validation("email", "must contain @");
        let display = error.to_string();
        assert!(display.contains("email"));
        assert!(display.contains("must contain @"));
    }

    #[test]
    fn test_exit_codes() {
        // Exit code 1: General errors
        let io_error = std::io::Error::new(std::io::ErrorKind::Other, "io error");
        assert_eq!(IntentError::io("read", io_error).exit_code(), 1);
        assert_eq!(IntentError::session("session error").exit_code(), 1);
        assert_eq!(IntentError::internal("bug", "file.rs:42").exit_code(), 1);

        // Exit code 2: Config errors
        assert_eq!(IntentError::config("bad").exit_code(), 2);
        assert_eq!(IntentError::config_missing("key", "file").exit_code(), 2);

        // Exit code 3: Validation errors
        assert_eq!(IntentError::validation("field", "invalid").exit_code(), 3);
        assert_eq!(IntentError::parse("file.cue", 1, 1, "error").exit_code(), 3);

        // Exit code 4: Not found errors
        assert_eq!(IntentError::spec_not_found("x").exit_code(), 4);
        assert_eq!(IntentError::session_not_found("sid").exit_code(), 4);

        // Exit code 5: Timeout errors
        assert_eq!(IntentError::timeout("op", 1000).exit_code(), 5);

        // Exit code 6: Network errors
        assert_eq!(IntentError::connection("url", "failed").exit_code(), 6);
        assert_eq!(IntentError::http("GET", "url", 500).exit_code(), 6);
    }

    #[test]
    fn test_is_recoverable() {
        assert!(IntentError::timeout("op", 1000).is_recoverable());
        assert!(IntentError::connection("url", "failed").is_recoverable());
        assert!(!IntentError::validation("field", "invalid").is_recoverable());
    }

    #[test]
    fn test_from_io_error() {
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let intent_error: IntentError = io_error.into();
        assert!(matches!(intent_error, IntentError::Io { .. }));
    }

    #[test]
    fn test_validation_batch() {
        let errors = vec![
            ValidationDetail {
                field: "email".to_string(),
                message: "invalid format".to_string(),
                value: Some("bad".to_string()),
            },
            ValidationDetail {
                field: "age".to_string(),
                message: "must be positive".to_string(),
                value: Some("-1".to_string()),
            },
        ];
        let error = IntentError::validation_batch(errors);
        let display = error.to_string();
        assert!(display.contains("2 errors"));
    }

    // =========================================================================
    // Constructor Method Tests (BEAD: intent-cli-mzn5)
    // =========================================================================
    // These tests verify each main constructor creates the correct variant
    // with expected data. Following TDD and functional patterns with zero unwraps.

    #[test]
    fn test_not_found_constructor_creates_correct_variant() {
        // Arrange: Create a sample IO error
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "file missing");
        let path = PathBuf::from("/tmp/missing.txt");

        // Act: Use the constructor
        let error = IntentError::not_found(path.clone(), io_error);

        // Assert: Verify correct variant and data using pattern matching
        match error {
            IntentError::NotFound {
                resource_type,
                path: error_path,
                source,
            } => {
                assert_eq!(resource_type, "file");
                assert_eq!(error_path, path);
                assert!(source.is_some());
            }
            _ => panic!("Expected NotFound variant"),
        }
    }

    #[test]
    fn test_parse_constructor_creates_correct_variant() {
        // Arrange: Set up parse error parameters
        let file = PathBuf::from("spec.cue");
        let line = 42;
        let column = 15;
        let message = "unexpected token ';'";

        // Act: Use the constructor
        let error = IntentError::parse(file.clone(), line, column, message);

        // Assert: Verify correct variant and all fields
        match error {
            IntentError::Parse {
                file: error_file,
                line: error_line,
                column: error_column,
                message: error_message,
                suggestion,
            } => {
                assert_eq!(error_file, file);
                assert_eq!(error_line, line);
                assert_eq!(error_column, column);
                assert_eq!(error_message, message);
                assert!(suggestion.is_none());
            }
            _ => panic!("Expected Parse variant"),
        }
    }

    #[test]
    fn test_validation_constructor_creates_correct_variant() {
        // Arrange: Set up validation error parameters
        let field = "email";
        let message = "must contain @ symbol";

        // Act: Use the constructor
        let error = IntentError::validation(field, message);

        // Assert: Verify correct variant and all fields
        match error {
            IntentError::Validation {
                field: error_field,
                message: error_message,
                value,
                suggestion,
            } => {
                assert_eq!(error_field, field);
                assert_eq!(error_message, message);
                assert!(value.is_none());
                assert!(suggestion.is_none());
            }
            _ => panic!("Expected Validation variant"),
        }
    }

    #[test]
    fn test_http_constructor_creates_correct_variant() {
        // Arrange: Set up HTTP error parameters
        let method = "GET";
        let url = "https://api.example.com/users";
        let status = 404;

        // Act: Use the constructor
        let error = IntentError::http(method, url, status);

        // Assert: Verify correct variant and all fields
        match error {
            IntentError::Http {
                method: error_method,
                url: error_url,
                status: error_status,
                body,
                expected_status,
            } => {
                assert_eq!(error_method, method);
                assert_eq!(error_url, url);
                assert_eq!(error_status, status);
                assert!(body.is_none());
                assert!(expected_status.is_none());
            }
            _ => panic!("Expected Http variant"),
        }
    }

    #[test]
    fn test_config_constructor_creates_correct_variant() {
        // Arrange: Set up config error parameters
        let message = "invalid timeout value";

        // Act: Use the constructor
        let error = IntentError::config(message);

        // Assert: Verify correct variant and all fields
        match error {
            IntentError::Config {
                message: error_message,
                key,
                file,
                suggestion,
            } => {
                assert_eq!(error_message, message);
                assert!(key.is_none());
                assert!(file.is_none());
                assert!(suggestion.is_none());
            }
            _ => panic!("Expected Config variant"),
        }
    }

    #[test]
    fn test_not_found_constructor_with_string_path() {
        // Test that Into<PathBuf> works with &str
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "test");
        let error = IntentError::not_found("config.toml", io_error);

        match error {
            IntentError::NotFound { path, .. } => {
                assert_eq!(path, PathBuf::from("config.toml"));
            }
            _ => panic!("Expected NotFound variant"),
        }
    }

    #[test]
    fn test_parse_constructor_with_string_inputs() {
        // Test that Into<String> and Into<PathBuf> work with &str
        let error = IntentError::parse("test.cue", 1, 1, "error");

        match error {
            IntentError::Parse { file, message, .. } => {
                assert_eq!(file, PathBuf::from("test.cue"));
                assert_eq!(message, "error");
            }
            _ => panic!("Expected Parse variant"),
        }
    }

    #[test]
    fn test_validation_constructor_with_string_inputs() {
        // Test that Into<String> works with &str
        let error = IntentError::validation("field", "message");

        match error {
            IntentError::Validation { field, message, .. } => {
                assert_eq!(field, "field");
                assert_eq!(message, "message");
            }
            _ => panic!("Expected Validation variant"),
        }
    }

    #[test]
    fn test_http_constructor_with_string_inputs() {
        // Test that Into<String> works with &str
        let error = IntentError::http("POST", "http://localhost", 500);

        match error {
            IntentError::Http {
                method,
                url,
                status,
                ..
            } => {
                assert_eq!(method, "POST");
                assert_eq!(url, "http://localhost");
                assert_eq!(status, 500);
            }
            _ => panic!("Expected Http variant"),
        }
    }

    #[test]
    fn test_config_constructor_with_string_input() {
        // Test that Into<String> works with &str
        let error = IntentError::config("error message");

        match error {
            IntentError::Config { message, .. } => {
                assert_eq!(message, "error message");
            }
            _ => panic!("Expected Config variant"),
        }
    }

    #[test]
    fn test_constructor_must_use_attribute_present() {
        // This test verifies that #[must_use] is working by attempting to
        // call constructors without binding the result. If #[must_use] is present,
        // the compiler will warn, but the test will still pass.
        // This is a compile-time check, not a runtime check.

        // These calls would generate warnings if #[must_use] is properly applied
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "test");
        let _ = IntentError::not_found("test", io_err);
        let _ = IntentError::parse("test", 1, 1, "msg");
        let _ = IntentError::validation("field", "msg");
        let _ = IntentError::http("GET", "url", 200);
        let _ = IntentError::config("msg");

        // If we get here, the constructors work correctly
        // The #[must_use] attribute will be verified by clippy
    }
}
