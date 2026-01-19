//// Structured error types for bead-to-prompt pipeline operations.
////
//// This module provides error types with full context preservation for:
//// - Loading beads from session files
//// - Parsing bead data from CUE/JSON
//// - Resolving bead references
////
//// Error Context Philosophy:
//// - Preserve all available information from underlying operations
//// - Include session context for traceability
//// - Provide structured data for programmatic handling
//// - Support both human-readable and JSON error output

import gleam/dynamic.{type DecodeError}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// =============================================================================
// CORE ERROR TYPE
// =============================================================================

/// Errors that can occur in the bead-to-prompt pipeline.
/// Each variant preserves full context from the underlying operation.
pub type PromptError {
  /// Failed to load bead data from a session file.
  /// Preserves the shellout exit code and stderr for debugging.
  BeadLoadError(
    /// The session ID being loaded
    session_id: String,
    /// Path to the session file that failed to load
    path: String,
    /// Exit code from the shell command (e.g., cue export)
    exit_code: Int,
    /// Standard error output from the command
    stderr: String,
    /// Extracted file path from stderr (if parseable)
    error_file: Option(String),
    /// Extracted line number from stderr (if parseable)
    error_line: Option(Int),
  )

  /// Failed to parse bead data from JSON/CUE content.
  /// Preserves structured decode errors for precise error reporting.
  BeadParseError(
    /// The session ID being parsed
    session_id: String,
    /// Source of the content (file path or "stdin")
    source: String,
    /// List of decode errors with path information
    decode_errors: List(DecodeError),
    /// Raw content snippet that failed to parse (first 200 chars)
    content_preview: Option(String),
  )

  /// Bead not found in the session.
  /// Includes session context and available beads for suggestions.
  BeadNotFound(
    /// The bead ID that was not found
    bead_id: String,
    /// The session ID where the bead was expected
    session_id: String,
    /// Path to the session file
    session_path: String,
    /// List of available bead IDs in the session (for suggestions)
    available_beads: List(String),
  )

  /// Session file does not exist.
  /// Includes context about where sessions are expected.
  SessionNotFound(
    /// The session ID that was not found
    session_id: String,
    /// Expected path where the session file should be
    expected_path: String,
    /// Available session IDs (if known)
    available_sessions: List(String),
  )

  /// CUE validation failed before export.
  /// Preserves CUE error details for debugging schema issues.
  CueValidationError(
    /// The session ID being validated
    session_id: String,
    /// Path to the CUE file
    path: String,
    /// Exit code from cue vet
    exit_code: Int,
    /// CUE validation error message
    error_message: String,
    /// Line number if parseable from error
    line_number: Option(Int),
  )

  /// Template rendering failed.
  /// Occurs when converting beads to prompt format.
  TemplateError(
    /// The bead ID being rendered
    bead_id: String,
    /// The template being used
    template_name: String,
    /// Description of what went wrong
    reason: String,
  )

  /// Security violation detected during bead loading.
  /// Path traversal, shell injection, or other security issues.
  SecurityError(
    /// The operation that was blocked
    operation: String,
    /// The input that triggered the violation
    input: String,
    /// Specific security concern
    reason: String,
  )
}

// =============================================================================
// CONSTRUCTORS: Convenient error creation
// =============================================================================

/// Create a BeadLoadError from shellout result.
/// Use this when `shellout.command` fails.
pub fn bead_load_error(
  session_id: String,
  path: String,
  shellout_result: #(Int, String),
) -> PromptError {
  let #(exit_code, stderr) = shellout_result
  let error_file = extract_file_path(stderr)
  let error_line = extract_line_number(stderr)

  BeadLoadError(
    session_id: session_id,
    path: path,
    exit_code: exit_code,
    stderr: stderr,
    error_file: error_file,
    error_line: error_line,
  )
}

/// Create a BeadParseError from decode errors.
/// Use this when JSON/dynamic decoding fails.
pub fn bead_parse_error(
  session_id: String,
  source: String,
  errors: List(DecodeError),
  content: Option(String),
) -> PromptError {
  let preview = case content {
    None -> None
    Some(c) ->
      case string.length(c) > 500 {
        True -> Some(string.slice(c, 0, 500) <> "...")
        False -> Some(c)
      }
  }

  BeadParseError(
    session_id: session_id,
    source: source,
    decode_errors: errors,
    content_preview: preview,
  )
}

/// Create a BeadNotFound error with available bead suggestions.
pub fn bead_not_found(
  bead_id: String,
  session_id: String,
  session_path: String,
  available: List(String),
) -> PromptError {
  BeadNotFound(
    bead_id: bead_id,
    session_id: session_id,
    session_path: session_path,
    available_beads: available,
  )
}

/// Create a SessionNotFound error.
pub fn session_not_found(
  session_id: String,
  expected_path: String,
  available: List(String),
) -> PromptError {
  SessionNotFound(
    session_id: session_id,
    expected_path: expected_path,
    available_sessions: available,
  )
}

/// Create a CueValidationError from shellout result.
pub fn cue_validation_error(
  session_id: String,
  path: String,
  shellout_result: #(Int, String),
) -> PromptError {
  let #(exit_code, stderr) = shellout_result
  let line_number = extract_line_number(stderr)

  CueValidationError(
    session_id: session_id,
    path: path,
    exit_code: exit_code,
    error_message: stderr,
    line_number: line_number,
  )
}

// =============================================================================
// FORMATTING: Human-readable error messages
// =============================================================================

/// Format a PromptError as a human-readable string.
pub fn format_error(error: PromptError) -> String {
  case error {
    BeadLoadError(session_id, path, exit_code, stderr, error_file, error_line) -> {
      let file_info = case error_file {
        None -> ""
        Some(f) -> "File: " <> f <> "\n"
      }
      let line_info = case error_line {
        None -> ""
        Some(n) -> "  Line: " <> int.to_string(n) <> "\n"
      }

      "Failed to load beads from session '"
      <> session_id
      <> "'\n"
      <> "  File: "
      <> path
      <> "\n"
      <> file_info
      <> line_info
      <> "  Exit code: "
      <> int.to_string(exit_code)
      <> "\n"
      <> "  Error:\n"
      <> indent_lines(stderr, "    ")
    }

    BeadParseError(session_id, source, decode_errors, content_preview) -> {
      let errors_str = format_decode_errors(decode_errors)
      let preview_str = case content_preview {
        None -> ""
        Some(preview) ->
          "\n  Content preview:\n" <> indent_lines(preview, "    ")
      }

      "Failed to parse beads for session '"
      <> session_id
      <> "'\n"
      <> "  Source: "
      <> source
      <> "\n"
      <> "  Parse errors:\n"
      <> indent_lines(errors_str, "    ")
      <> preview_str
    }

    BeadNotFound(bead_id, session_id, session_path, available_beads) -> {
      let suggestions = case available_beads {
        [] -> ""
        beads ->
          "\n  Available beads:\n"
          <> list.map(beads, fn(b) { "    - " <> b })
          |> string.join("\n")
      }

      "Bead '"
      <> bead_id
      <> "' not found in session '"
      <> session_id
      <> "'\n"
      <> "  Session file: "
      <> session_path
      <> suggestions
    }

    SessionNotFound(session_id, expected_path, available_sessions) -> {
      let suggestions = case available_sessions {
        [] -> "\n  No sessions found. Run 'intent interview' to create one."
        sessions ->
          "\n  Available sessions:\n"
          <> list.map(sessions, fn(s) { "    - " <> s })
          |> string.join("\n")
      }

      "Session '"
      <> session_id
      <> "' not found\n"
      <> "  Expected at: "
      <> expected_path
      <> suggestions
    }

    CueValidationError(session_id, path, exit_code, error_message, line_number) -> {
      let line_info = case line_number {
        None -> ""
        Some(n) -> " (line " <> int.to_string(n) <> ")"
      }

      "CUE validation failed for session '"
      <> session_id
      <> "'"
      <> line_info
      <> "\n"
      <> "  File: "
      <> path
      <> "\n"
      <> "  Exit code: "
      <> int.to_string(exit_code)
      <> "\n"
      <> "  Error:\n"
      <> indent_lines(error_message, "    ")
    }

    TemplateError(bead_id, template_name, reason) ->
      "Template rendering failed for bead '"
      <> bead_id
      <> "'\n"
      <> "  Template: "
      <> template_name
      <> "\n"
      <> "  Reason: "
      <> reason

    SecurityError(operation, input, reason) ->
      "Security violation in '"
      <> operation
      <> "'\n"
      <> "  Input: "
      <> truncate(input, 50)
      <> "\n"
      <> "  Reason: "
      <> reason
  }
}

// =============================================================================
// JSON OUTPUT: Structured error for programmatic handling
// =============================================================================

/// Convert a PromptError to JSON for machine consumption.
pub fn to_json(error: PromptError) -> Json {
  case error {
    BeadLoadError(session_id, path, exit_code, stderr, error_file, error_line) ->
      json.object([
        #("error_type", json.string("bead_load_error")),
        #("session_id", json.string(session_id)),
        #("path", json.string(path)),
        #("exit_code", json.int(exit_code)),
        #("stderr", json.string(stderr)),
        #("error_file", case error_file {
          None -> json.null()
          Some(f) -> json.string(f)
        }),
        #("error_line", case error_line {
          None -> json.null()
          Some(n) -> json.int(n)
        }),
      ])

    BeadParseError(session_id, source, decode_errors, content_preview) ->
      json.object([
        #("error_type", json.string("bead_parse_error")),
        #("session_id", json.string(session_id)),
        #("source", json.string(source)),
        #(
          "decode_errors",
          json.array(decode_errors, fn(e) {
            json.object([
              #("expected", json.string(e.expected)),
              #("found", json.string(e.found)),
              #("path", json.array(e.path, json.string)),
            ])
          }),
        ),
        #("content_preview", case content_preview {
          None -> json.null()
          Some(p) -> json.string(p)
        }),
      ])

    BeadNotFound(bead_id, session_id, session_path, available_beads) ->
      json.object([
        #("error_type", json.string("bead_not_found")),
        #("bead_id", json.string(bead_id)),
        #("session_id", json.string(session_id)),
        #("session_path", json.string(session_path)),
        #("available_beads", json.array(available_beads, json.string)),
      ])

    SessionNotFound(session_id, expected_path, available_sessions) ->
      json.object([
        #("error_type", json.string("session_not_found")),
        #("session_id", json.string(session_id)),
        #("expected_path", json.string(expected_path)),
        #("available_sessions", json.array(available_sessions, json.string)),
      ])

    CueValidationError(session_id, path, exit_code, error_message, line_number) ->
      json.object([
        #("error_type", json.string("cue_validation_error")),
        #("session_id", json.string(session_id)),
        #("path", json.string(path)),
        #("exit_code", json.int(exit_code)),
        #("error_message", json.string(error_message)),
        #("line_number", case line_number {
          None -> json.null()
          Some(n) -> json.int(n)
        }),
      ])

    TemplateError(bead_id, template_name, reason) ->
      json.object([
        #("error_type", json.string("template_error")),
        #("bead_id", json.string(bead_id)),
        #("template_name", json.string(template_name)),
        #("reason", json.string(reason)),
      ])

    SecurityError(operation, input, reason) ->
      json.object([
        #("error_type", json.string("security_error")),
        #("operation", json.string(operation)),
        #("input", json.string(input)),
        #("reason", json.string(reason)),
      ])
  }
}

// =============================================================================
// ERROR CATEGORIZATION: For routing and handling
// =============================================================================

/// Check if error is recoverable (can be retried).
pub fn is_recoverable(error: PromptError) -> Bool {
  case error {
    BeadLoadError(_, _, _, _, _, _) -> True
    BeadParseError(_, _, _, _) -> True
    BeadNotFound(_, _, _, _) -> False
    SessionNotFound(_, _, _) -> False
    CueValidationError(_, _, _, _, _) -> True
    TemplateError(_, _, _) -> False
    SecurityError(_, _, _) -> False
  }
}

/// Get the session ID from an error, if applicable.
pub fn get_session_id(error: PromptError) -> Option(String) {
  case error {
    BeadLoadError(session_id, _, _, _, _, _) -> Some(session_id)
    BeadParseError(session_id, _, _, _) -> Some(session_id)
    BeadNotFound(_, session_id, _, _) -> Some(session_id)
    SessionNotFound(session_id, _, _) -> Some(session_id)
    CueValidationError(session_id, _, _, _, _) -> Some(session_id)
    TemplateError(_, _, _) -> None
    SecurityError(_, _, _) -> None
  }
}

/// Get a short error code for logging/metrics.
pub fn error_code(error: PromptError) -> String {
  case error {
    BeadLoadError(_, _, _, _, _, _) -> "PROMPT_LOAD_ERR"
    BeadParseError(_, _, _, _) -> "PROMPT_PARSE_ERR"
    BeadNotFound(_, _, _, _) -> "PROMPT_BEAD_404"
    SessionNotFound(_, _, _) -> "PROMPT_SESSION_404"
    CueValidationError(_, _, _, _, _) -> "PROMPT_CUE_ERR"
    TemplateError(_, _, _) -> "PROMPT_TEMPLATE_ERR"
    SecurityError(_, _, _) -> "PROMPT_SECURITY_ERR"
  }
}

// =============================================================================
// PRIVATE HELPERS
// =============================================================================

/// Format a list of decode errors into a readable string.
fn format_decode_errors(errors: List(DecodeError)) -> String {
  errors
  |> list.map(fn(e) {
    let path_str = case e.path {
      [] -> "root"
      parts -> string.join(parts, ".")
    }
    "At '" <> path_str <> "': expected " <> e.expected <> ", found " <> e.found
  })
  |> string.join("\n")
}

/// Indent all lines of a string with the given prefix.
fn indent_lines(text: String, prefix: String) -> String {
  text
  |> string.split("\n")
  |> list.map(fn(line) { prefix <> line })
  |> string.join("\n")
}

/// Truncate a string to max length with ellipsis.
fn truncate(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len - 3) <> "..."
    False -> s
  }
}

/// Extract line number from CUE error message.
/// CUE errors typically have format: "file.cue:42:10: error message"
fn extract_line_number(stderr: String) -> Option(Int) {
  case string.split(stderr, ":") {
    [_, line_str, ..] ->
      case int.parse(string.trim(line_str)) {
        Ok(n) -> Some(n)
        Error(_) -> None
      }
    _ -> None
  }
}

/// Extract file path from CUE error message.
/// CUE errors typically have format: "file.cue:42:10: error message"
fn extract_file_path(stderr: String) -> Option(String) {
  case string.split(stderr, ":") {
    [file_path, ..] -> Some(string.trim(file_path))
    _ -> None
  }
}
