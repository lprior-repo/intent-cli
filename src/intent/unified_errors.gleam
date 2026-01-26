/// Unified error handling system for Intent CLI
///
/// Provides a comprehensive error framework with:
/// - Standardized error codes and exit codes
/// - Severity levels (error, warning, fatal)
/// - Structured context and recovery suggestions
/// - JSON serialization for machine consumption
/// - Human-friendly text formatting

import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// =============================================================================
// EXIT CODE CONSTANTS
// =============================================================================

/// Exit code for successful completion
pub const exit_success = 0

/// Exit code for user input errors (missing args, invalid flags, etc.)
pub const exit_user_input_error = 2

/// Exit code for validation errors (spec errors, check failures, etc.)
pub const exit_validation_error = 3

/// Exit code for runtime errors (file I/O, network, permissions, etc.)
pub const exit_runtime_error = 4

/// Exit code for internal errors (panic, invariant violation, etc.)
pub const exit_internal_error = 5

// =============================================================================
// ERROR CODE ENUM
// =============================================================================

/// Error codes representing different failure categories
///
/// Each variant carries semantic meaning for error routing and recovery.
pub type ErrorCode {
  /// Missing required input (arguments, files, etc.)
  MissingInput
  /// Validation failed (spec, format, business rules)
  ValidationFailed
  /// File not found
  FileNotFound
  /// File permission denied
  FilePermissionDenied
  /// Invalid input format or value
  InvalidInput
  /// CUE spec parsing error
  SpecParseError
  /// General file/resource loading error
  LoadError
  /// Session not found
  SessionNotFound
  /// Conflicting or mutually exclusive flags
  ConflictingFlags
  /// Internal error (should not happen)
  InternalError
}

// =============================================================================
// SEVERITY LEVEL ENUM
// =============================================================================

/// Error severity level
pub type Severity {
  /// Non-blocking warning (may continue)
  Warning
  /// Error (should stop execution)
  Error
  /// Fatal error (must stop immediately)
  Fatal
}

// =============================================================================
// UNIFIED ERROR TYPE
// =============================================================================

/// Unified error with complete context for handling and recovery
pub type UnifiedError {
  UnifiedError(
    /// Error code for categorization and routing
    code: ErrorCode,
    /// Human-readable error message
    message: String,
    /// Severity level
    severity: Severity,
    /// Additional context as key-value pairs
    context: Dict(String, String),
    /// Recovery/remediation suggestion for user
    suggestion: String,
    /// Exact command to fix the issue (if applicable)
    fix_command: Option(String),
    /// Standard exit code
    exit_code: Int,
  )
}

// =============================================================================
// ERROR CODE UTILITIES
// =============================================================================

/// Convert ErrorCode to string representation
pub fn error_code_to_string(code: ErrorCode) -> String {
  case code {
    MissingInput -> "missing_input"
    ValidationFailed -> "validation_failed"
    FileNotFound -> "file_not_found"
    FilePermissionDenied -> "file_permission_denied"
    InvalidInput -> "invalid_input"
    SpecParseError -> "spec_parse_error"
    LoadError -> "load_error"
    SessionNotFound -> "session_not_found"
    ConflictingFlags -> "conflicting_flags"
    InternalError -> "internal_error"
  }
}

/// Get standard exit code for error code
pub fn get_exit_code_for_error(code: ErrorCode) -> Int {
  case code {
    MissingInput -> exit_user_input_error
    ConflictingFlags -> exit_user_input_error
    FileNotFound -> exit_user_input_error
    SessionNotFound -> exit_user_input_error
    ValidationFailed -> exit_validation_error
    InvalidInput -> exit_validation_error
    SpecParseError -> exit_validation_error
    FilePermissionDenied -> exit_runtime_error
    LoadError -> exit_runtime_error
    InternalError -> exit_internal_error
  }
}

/// Convert Severity to string representation
pub fn severity_to_string(sev: Severity) -> String {
  case sev {
    Warning -> "warning"
    Error -> "error"
    Fatal -> "fatal"
  }
}

// =============================================================================
// FACTORY FUNCTION
// =============================================================================

/// Create a UnifiedError with all required fields
///
/// This is the primary way to construct errors. It automatically determines
/// the exit code from the error code and sets severity to Error.
pub fn unified_error(
  code code: ErrorCode,
  message message: String,
  suggestion suggestion: String,
  fix_command fix_command: Option(String),
) -> UnifiedError {
  UnifiedError(
    code: code,
    message: message,
    severity: Error,
    context: dict.new(),
    suggestion: suggestion,
    fix_command: fix_command,
    exit_code: get_exit_code_for_error(code),
  )
}

/// Create a UnifiedError with full control over all fields
pub fn unified_error_full(
  code code: ErrorCode,
  message message: String,
  severity severity: Severity,
  context context: Dict(String, String),
  suggestion suggestion: String,
  fix_command fix_command: Option(String),
) -> UnifiedError {
  UnifiedError(
    code: code,
    message: message,
    severity: severity,
    context: context,
    suggestion: suggestion,
    fix_command: fix_command,
    exit_code: get_exit_code_for_error(code),
  )
}

/// Add context to an existing error
pub fn with_context(
  error error: UnifiedError,
  key key: String,
  value value: String,
) -> UnifiedError {
  UnifiedError(
    ..error,
    context: dict.insert(error.context, key, value),
  )
}

/// Add multiple context entries to an error
pub fn with_context_list(
  error error: UnifiedError,
  entries entries: List(#(String, String)),
) -> UnifiedError {
  let new_context =
    list.fold(entries, error.context, fn(acc, entry) {
      let #(k, v) = entry
      dict.insert(acc, k, v)
    })
  UnifiedError(..error, context: new_context)
}

/// Change the severity level of an error
pub fn with_severity(error error: UnifiedError, sev sev: Severity) -> UnifiedError {
  UnifiedError(..error, severity: sev)
}

// =============================================================================
// JSON SERIALIZATION
// =============================================================================

/// Convert UnifiedError to JSON for machine consumption
pub fn unified_error_to_json(error: UnifiedError) -> Json {
  let context_items =
    dict.to_list(error.context)
    |> list.map(fn(pair) {
      let #(k, v) = pair
      #(k, json.string(v))
    })

  json.object([
    #("action", json.string("error")),
    #(
      "error",
      json.object([
        #("code", json.string(error_code_to_string(error.code))),
        #("message", json.string(error.message)),
        #("severity", json.string(severity_to_string(error.severity))),
        #("context", json.object(context_items)),
        #("suggestion", json.string(error.suggestion)),
        #(
          "fix_command",
          case error.fix_command {
            Some(cmd) -> json.string(cmd)
            None -> json.null()
          },
        ),
        #("exit_code", json.int(error.exit_code)),
      ]),
    ),
  ])
}

// =============================================================================
// TEXT FORMATTING
// =============================================================================

/// Format error as human-readable text
pub fn format_error_text(error: UnifiedError) -> String {
  let severity_label = severity_to_string(error.severity)
  let code_label = error_code_to_string(error.code)

  let header =
    "Error (code="
    <> code_label
    <> ", severity="
    <> severity_label
    <> ", exit="
    <> int.to_string(error.exit_code)
    <> ")"

  let message_text = "\n\nMessage:\n  " <> error.message

  let context_text = case dict.is_empty(error.context) {
    True -> ""
    False -> {
      "\n\nContext:"
      <> {
        dict.to_list(error.context)
        |> list.map(fn(pair) {
          let #(k, v) = pair
          "\n  " <> k <> ": " <> v
        })
        |> string.join("")
      }
    }
  }

  let suggestion_text = "\n\nSuggestion:\n  " <> error.suggestion

  let fix_text = case error.fix_command {
    Some(cmd) -> "\n\nFix Command:\n  " <> cmd
    None -> ""
  }

  header <> message_text <> context_text <> suggestion_text <> fix_text
}

/// Format error concisely for inline display
pub fn format_error_brief(error: UnifiedError) -> String {
  error_code_to_string(error.code)
  <> ": "
  <> error.message
  <> " (exit "
  <> int.to_string(error.exit_code)
  <> ")"
}

// =============================================================================
// OUTPUT AND EXIT
// =============================================================================

/// Output error to stderr and halt with appropriate exit code
pub fn output_and_halt(error error: UnifiedError, is_json is_json: Bool) -> Nil {
  case is_json {
    True -> {
      error
      |> unified_error_to_json
      |> json.to_string
      |> io.println
      halt(error.exit_code)
    }
    False -> {
      error
      |> format_error_text
      |> io.println_error
      halt(error.exit_code)
    }
  }
}

/// Output error to stderr without halting (for logging)
pub fn output_error(error error: UnifiedError, is_json is_json: Bool) -> Nil {
  case is_json {
    True -> {
      error
      |> unified_error_to_json
      |> json.to_string
      |> io.println
    }
    False -> {
      error
      |> format_error_text
      |> io.println_error
    }
  }
}

// =============================================================================
// COMMON ERROR BUILDERS
// =============================================================================

/// Create a missing input error (file, argument, etc.)
pub fn missing_input(
  what what: String,
  suggestion suggestion: String,
) -> UnifiedError {
  unified_error(
    code: MissingInput,
    message: "Missing required input: " <> what,
    suggestion: suggestion,
    fix_command: None,
  )
}

/// Create a validation failed error
pub fn validation_failed(
  what what: String,
  reason reason: String,
  suggestion suggestion: String,
) -> UnifiedError {
  unified_error(
    code: ValidationFailed,
    message: "Validation failed for " <> what <> ": " <> reason,
    suggestion: suggestion,
    fix_command: None,
  )
}

/// Create a file not found error
pub fn file_not_found(path path: String) -> UnifiedError {
  unified_error(
    code: FileNotFound,
    message: "File not found: " <> path,
    suggestion: "Check that the file exists at the specified path",
    fix_command: Some("ls -l " <> path),
  )
  |> with_context(key: "path", value: path)
}

/// Create a file permission denied error
pub fn file_permission_denied(path path: String, operation operation: String) -> UnifiedError {
  unified_error(
    code: FilePermissionDenied,
    message: "Permission denied: cannot " <> operation <> " " <> path,
    suggestion: "Check file permissions or run with appropriate privileges",
    fix_command: Some("ls -la " <> path),
  )
  |> with_context(key: "path", value: path)
  |> with_context(key: "operation", value: operation)
}

/// Create an invalid input error
pub fn invalid_input(
  input input: String,
  reason reason: String,
  suggestion suggestion: String,
) -> UnifiedError {
  unified_error(
    code: InvalidInput,
    message: "Invalid input: " <> input <> " (" <> reason <> ")",
    suggestion: suggestion,
    fix_command: None,
  )
  |> with_context(key: "input", value: input)
  |> with_context(key: "reason", value: reason)
}

/// Create a spec parse error
pub fn spec_parse_error(
  path path: String,
  reason reason: String,
) -> UnifiedError {
  unified_error(
    code: SpecParseError,
    message: "Failed to parse spec from " <> path <> ": " <> reason,
    suggestion: "Verify the spec file is valid CUE or JSON",
    fix_command: Some("intent validate " <> path),
  )
  |> with_context(key: "path", value: path)
  |> with_context(key: "reason", value: reason)
}

/// Create a load error
pub fn load_error(
  resource resource: String,
  reason reason: String,
) -> UnifiedError {
  unified_error(
    code: LoadError,
    message: "Failed to load " <> resource <> ": " <> reason,
    suggestion: "Check that the resource is accessible and properly formatted",
    fix_command: None,
  )
  |> with_context(key: "resource", value: resource)
  |> with_context(key: "reason", value: reason)
}

/// Create a session not found error
pub fn session_not_found(session_id session_id: String) -> UnifiedError {
  unified_error(
    code: SessionNotFound,
    message: "Session not found: " <> session_id,
    suggestion: "List available sessions or start a new interview session",
    fix_command: Some("intent sessions"),
  )
  |> with_context(key: "session_id", value: session_id)
}

/// Create a conflicting flags error
pub fn conflicting_flags(flag1 flag1: String, flag2 flag2: String) -> UnifiedError {
  unified_error(
    code: ConflictingFlags,
    message: "Conflicting flags: --" <> flag1 <> " and --" <> flag2,
    suggestion: "Use only one of these flags",
    fix_command: None,
  )
  |> with_context(key: "flag1", value: flag1)
  |> with_context(key: "flag2", value: flag2)
}

/// Create an internal error (should not happen)
pub fn internal_error(
  operation operation: String,
  reason reason: String,
) -> UnifiedError {
  let error =
    unified_error(
      code: InternalError,
      message: "Internal error during " <> operation <> ": " <> reason,
      suggestion: "This is a bug. Please report it with the details below.",
      fix_command: None,
    )
    |> with_severity(sev: Fatal)

  with_context(error, key: "operation", value: operation)
  |> with_context(key: "reason", value: reason)
}

// =============================================================================
// FFI AND INTERNAL
// =============================================================================

/// Halt execution with exit code
@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil
