/// AI-Agent Friendly Error Messages
///
/// This module provides structured error formatting that helps AI agents
/// understand what went wrong and what actions to take to fix it.
///
/// Error Format:
/// {
///   action: "error_category",
///   error: {
///     type: "specific_error_type",
///     message: "what went wrong",
///     context: {...}
///   },
///   suggestion: "what to do next",
///   recovery: ["step 1", "step 2", ...]
/// }
import gleam/dict.{type Dict}
import gleam/json
import gleam/list
import gleam/result
import gleam/string

/// Structured error for AI consumption
pub type AiError {
  AiError(
    action: String,
    error_type: String,
    message: String,
    context: Dict(String, String),
    suggestion: String,
    recovery_steps: List(String),
  )
}

/// Format an AiError as CUE-compatible output
pub fn format_cue(error: AiError) -> String {
  let context_lines =
    dict.to_list(error.context)
    |> list.map(fn(pair) {
      "\t\t" <> pair.0 <> ": \"" <> escape_cue_string(pair.1) <> "\""
    })
    |> string.join("\n")

  let recovery_lines =
    error.recovery_steps
    |> list.map(fn(step) { "\t\t\"" <> escape_cue_string(step) <> "\"" })
    |> string.join(",\n")

  "{\n"
  <> "\taction: \""
  <> error.action
  <> "\"\n"
  <> "\terror: {\n"
  <> "\t\ttype: \""
  <> error.error_type
  <> "\"\n"
  <> "\t\tmessage: \""
  <> escape_cue_string(error.message)
  <> "\"\n"
  <> "\t\tcontext: {\n"
  <> context_lines
  <> "\n\t\t}\n"
  <> "\t}\n"
  <> "\tsuggestion: \""
  <> escape_cue_string(error.suggestion)
  <> "\"\n"
  <> "\trecovery: [\n"
  <> recovery_lines
  <> "\n\t]\n"
  <> "}\n"
}

/// Format an AiError as JSON
pub fn format_json(error: AiError) -> String {
  json.object([
    #("action", json.string(error.action)),
    #(
      "error",
      json.object([
        #("type", json.string(error.error_type)),
        #("message", json.string(error.message)),
        #(
          "context",
          json.object(
            dict.to_list(error.context)
            |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) }),
          ),
        ),
      ]),
    ),
    #("suggestion", json.string(error.suggestion)),
    #("recovery", json.array(error.recovery_steps, json.string)),
  ])
  |> json.to_string
}

/// Format an AiError as human-readable text
pub fn format_text(error: AiError) -> String {
  let context_text = case dict.size(error.context) {
    0 -> ""
    _ -> {
      "\n\nContext:\n"
      <> string.join(
        dict.to_list(error.context)
          |> list.map(fn(pair) { "  " <> pair.0 <> ": " <> pair.1 }),
        "\n",
      )
    }
  }

  let recovery_text = case list.length(error.recovery_steps) {
    0 -> ""
    _ -> {
      "\n\nRecovery Steps:\n"
      <> string.join(
        error.recovery_steps
          |> list.index_map(fn(step, i) {
            "  " <> string.inspect(i + 1) <> ". " <> step
          }),
        "\n",
      )
    }
  }

  "Error: "
  <> error.message
  <> context_text
  <> "\n\nSuggestion: "
  <> error.suggestion
  <> recovery_text
}

fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

// =============================================================================
// Common Error Builders
// =============================================================================

/// File not found error with directory creation suggestion
pub fn file_not_found(path: String, expected_location: String) -> AiError {
  AiError(
    action: "file_error",
    error_type: "file_not_found",
    message: "File not found: " <> path,
    context: dict.from_list([
      #("path", path),
      #("expected_location", expected_location),
    ]),
    suggestion: "Create the missing file or directory",
    recovery_steps: [
      "Check if the parent directory exists",
      "Create directory: mkdir -p " <> extract_directory(path),
      "Create the file with appropriate content",
      "Verify file permissions allow read/write access",
    ],
  )
}

/// Directory not found with creation command
pub fn directory_not_found(path: String) -> AiError {
  AiError(
    action: "file_error",
    error_type: "directory_not_found",
    message: "Directory not found: " <> path,
    context: dict.from_list([#("path", path)]),
    suggestion: "Create the missing directory",
    recovery_steps: [
      "Create directory: mkdir -p " <> path,
      "Verify directory was created: ls -la " <> extract_parent(path),
    ],
  )
}

/// CUE validation error with installation check
pub fn cue_validation_error(message: String, file_path: String) -> AiError {
  AiError(
    action: "cue_error",
    error_type: "validation_error",
    message: "CUE validation failed: " <> message,
    context: dict.from_list([
      #("file", file_path),
      #("validation_output", message),
    ]),
    suggestion: "Fix CUE syntax errors in the specification file",
    recovery_steps: [
      "Check if CUE is installed: cue version",
      "If not installed: Visit https://cuelang.org/docs/install/",
      "Validate CUE syntax: cue vet " <> file_path,
      "Review error message for specific line numbers and fix syntax",
      "Ensure all required fields are present in the spec",
    ],
  )
}

/// CUE export error with schema suggestion
pub fn cue_export_error(message: String, file_path: String) -> AiError {
  AiError(
    action: "cue_error",
    error_type: "export_error",
    message: "CUE export failed: " <> message,
    context: dict.from_list([#("file", file_path), #("export_output", message)]),
    suggestion: "Ensure the CUE file has a valid 'spec' definition",
    recovery_steps: [
      "Check that your CUE file defines a 'spec' object",
      "Verify the spec matches the required schema",
      "Test export manually: cue export " <> file_path <> " -e spec",
      "Compare your spec with examples in examples/ directory",
      "Ensure all required fields are present (name, description, features, etc.)",
    ],
  )
}

/// Session not found with list suggestion
pub fn session_not_found(session_id: String, sessions_path: String) -> AiError {
  AiError(
    action: "session_error",
    error_type: "session_not_found",
    message: "Interview session not found: " <> session_id,
    context: dict.from_list([
      #("session_id", session_id),
      #("sessions_file", sessions_path),
    ]),
    suggestion: "Use a valid session ID from an existing interview",
    recovery_steps: [
      "List all sessions: intent interview --list",
      "Check if .interview directory exists: ls -la .interview",
      "If no sessions exist, start a new interview: intent interview --profile api",
      "Verify session ID format (should be like: interview-20240101-120000)",
    ],
  )
}

/// Invalid session ID format
pub fn invalid_session_id(session_id: String) -> AiError {
  AiError(
    action: "validation_error",
    error_type: "invalid_session_id",
    message: "Invalid session ID format: " <> session_id,
    context: dict.from_list([#("session_id", session_id)]),
    suggestion: "Use a valid session ID format",
    recovery_steps: [
      "Session IDs should contain only alphanumeric characters, hyphens, and underscores",
      "Example valid format: interview-20240101-120000",
      "List available sessions: intent interview --list",
    ],
  )
}

/// Bead not found with generation suggestion
pub fn bead_not_found(bead_id: String, session_id: String) -> AiError {
  AiError(
    action: "bead_error",
    error_type: "bead_not_found",
    message: "Bead not found: " <> bead_id,
    context: dict.from_list([#("bead_id", bead_id), #("session_id", session_id)]),
    suggestion: "Generate beads from the interview session first",
    recovery_steps: [
      "Generate beads: intent beads " <> session_id,
      "List generated beads: cat .intent/beads-" <> session_id <> ".cue",
      "Verify bead ID format (e.g., AUTH-001, API-042)",
    ],
  )
}

/// Write permission error
pub fn write_permission_error(path: String) -> AiError {
  AiError(
    action: "file_error",
    error_type: "permission_denied",
    message: "Permission denied writing to: " <> path,
    context: dict.from_list([#("path", path)]),
    suggestion: "Check file/directory permissions",
    recovery_steps: [
      "Check permissions: ls -la " <> extract_directory(path),
      "Ensure you have write access to the directory",
      "If needed, adjust permissions: chmod u+w " <> extract_directory(path),
      "Check if disk is full: df -h",
    ],
  )
}

/// Invalid requirement format (EARS interview)
pub fn invalid_requirement(reason: String, requirement: String) -> AiError {
  AiError(
    action: "validation_error",
    error_type: "invalid_requirement",
    message: "Invalid requirement format: " <> reason,
    context: dict.from_list([#("requirement", requirement), #("reason", reason)]),
    suggestion: "Ensure requirements follow EARS pattern",
    recovery_steps: [
      "Requirements should start with: WHEN/WHILE/IF/WHERE",
      "Format: WHEN <trigger> THEN <system response>",
      "Example: WHEN user clicks login THEN system validates credentials",
      "Use intent ears-check to validate requirements before saving",
    ],
  )
}

/// HTTP connection error with debug suggestions
pub fn http_connection_error(
  error_message: String,
  target_url: String,
) -> AiError {
  AiError(
    action: "http_error",
    error_type: "connection_failed",
    message: "HTTP connection failed: " <> error_message,
    context: dict.from_list([
      #("target_url", target_url),
      #("error", error_message),
    ]),
    suggestion: "Verify the target API is running and accessible",
    recovery_steps: [
      "Check if server is running: curl " <> target_url,
      "Verify base_url in your spec is correct",
      "Test network connectivity: ping " <> extract_hostname(target_url),
      "Check for firewall rules blocking the connection",
      "If using HTTPS, verify SSL certificate is valid",
    ],
  )
}

/// Interpolation variable not found
pub fn interpolation_error(
  variable: String,
  available_vars: List(String),
) -> AiError {
  let available = case list.length(available_vars) {
    0 -> "none available"
    _ -> string.join(available_vars, ", ")
  }

  AiError(
    action: "interpolation_error",
    error_type: "variable_not_found",
    message: "Variable not found: " <> variable,
    context: dict.from_list([#("variable", variable), #("available", available)]),
    suggestion: "Ensure the variable is captured in a previous behavior",
    recovery_steps: [
      "Check that a previous behavior captures this variable using 'captures'",
      "Verify the behavior order - variables must be captured before use",
      "Review spec for typos in variable names",
      "Use 'intent validate' to check for undefined variables",
    ],
  )
}

/// Session directory creation error
pub fn session_directory_error(path: String, reason: String) -> AiError {
  AiError(
    action: "session_error",
    error_type: "directory_creation_failed",
    message: "Failed to create session directory: " <> reason,
    context: dict.from_list([#("path", path), #("reason", reason)]),
    suggestion: "Ensure you have write permissions in the current directory",
    recovery_steps: [
      "Check directory permissions: ls -la .",
      "Verify disk space: df -h",
      "Try creating directory manually: mkdir -p " <> path,
      "Check if parent directory exists and is writable",
    ],
  )
}

/// Session file write error
pub fn session_file_write_error(path: String, reason: String) -> AiError {
  AiError(
    action: "session_error",
    error_type: "file_write_failed",
    message: "Failed to write session file: " <> reason,
    context: dict.from_list([#("path", path), #("reason", reason)]),
    suggestion: "Check file permissions and disk space",
    recovery_steps: [
      "Verify file permissions: ls -la " <> extract_directory(path),
      "Check disk space: df -h",
      "Ensure directory exists: mkdir -p " <> extract_directory(path),
      "Try writing manually: echo 'test' > " <> path,
    ],
  )
}

/// Session file read error
pub fn session_file_read_error(path: String, reason: String) -> AiError {
  AiError(
    action: "session_error",
    error_type: "file_read_failed",
    message: "Failed to read session file: " <> reason,
    context: dict.from_list([#("path", path), #("reason", reason)]),
    suggestion: "Verify the session file exists and is readable",
    recovery_steps: [
      "Check if file exists: ls -la " <> path,
      "Verify file permissions: ls -la " <> path,
      "Check file is not corrupted: cat " <> path,
      "List all sessions: intent interview --list",
    ],
  )
}

/// Invalid JSONL format error
pub fn invalid_jsonl_error(path: String, line_number: Int) -> AiError {
  AiError(
    action: "session_error",
    error_type: "invalid_jsonl",
    message: "Invalid JSONL format at line " <> string.inspect(line_number),
    context: dict.from_list([
      #("path", path),
      #("line", string.inspect(line_number)),
    ]),
    suggestion: "Fix the JSONL syntax error in the session file",
    recovery_steps: [
      "View the problematic line: sed -n '"
        <> string.inspect(line_number)
        <> "p' "
        <> path,
      "Validate JSON syntax for that line",
      "Backup and recreate the file if corrupted",
      "Start a new interview session if recovery fails",
    ],
  )
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Extract directory from file path
fn extract_directory(path: String) -> String {
  case string.split(path, "/") |> list.reverse {
    [_file, ..rest] -> string.join(list.reverse(rest), "/")
    _ -> "."
  }
}

/// Extract parent directory
fn extract_parent(path: String) -> String {
  case string.split(path, "/") |> list.reverse {
    [_, ..rest] -> string.join(list.reverse(rest), "/")
    _ -> "."
  }
}

/// Extract hostname from URL
fn extract_hostname(url: String) -> String {
  url
  |> string.replace("http://", "")
  |> string.replace("https://", "")
  |> string.split("/")
  |> list.first
  |> result.unwrap("unknown-host")
}
