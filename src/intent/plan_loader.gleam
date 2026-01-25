/// CUE Plan loader - loads and validates Plan files using the cue command
///
/// FUNCTIONAL CORE / IMPERATIVE SHELL (FC/IS) Architecture
/// ========================================================
/// This module separates pure business logic from I/O operations:
///
/// FUNCTIONAL CORE (Pure functions):
/// - parse_cue_validation_result: Interprets command output
/// - parse_cue_export_result: Converts export output to JSON
/// - parse_json_to_plan: Decodes JSON to Plan type
///
/// IMPERATIVE SHELL (I/O wrappers with dependency injection):
/// - validate_plan_cue_with_executor: Runs CUE validation via injected executor
/// - export_plan_cue_with_executor: Exports CUE via injected executor
/// - load_plan_with_executor: Orchestrates full load with injected I/O
///
/// PUBLIC API (Convenience functions using default implementations):
/// - validate_plan_cue: Validates using shellout
/// - export_plan_json: Exports using shellout
/// - load_plan: Loads using shellout
///
/// Pattern follows intent/loader.gleam for consistency
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/string
import intent/parser
import intent/planning_types.{type Plan}
import intent/security
import shellout

// ============================================================================
// Error Types (Railway-Oriented Programming)
// ============================================================================

/// Error types for loading Plans
/// Railway-Oriented Programming: Preserve all context for programmatic handling
pub type LoadPlanError {
  FileNotFound(path: String)
  CueValidationFailed(path: String, exit_code: Int, stderr: String)
  CueExportFailed(path: String, exit_code: Int, stderr: String)
  JsonDecodeFailed(errors: List(dynamic.DecodeError))
  PlanParseFailed(errors: List(dynamic.DecodeError))
  SecurityError(message: String)
}

// ============================================================================
// Type Definitions for Dependency Injection
// ============================================================================

/// Command executor type for dependency injection
/// Takes: command name, arguments, working directory
/// Returns: Result of stdout or (exit_code, stderr)
pub type CommandExecutor =
  fn(String, List(String), String) -> Result(String, #(Int, String))

/// Default executor using shellout.command
fn default_executor(
  cmd: String,
  args: List(String),
  dir: String,
) -> Result(String, #(Int, String)) {
  shellout.command(run: cmd, with: args, in: dir, opt: [])
}

// ============================================================================
// FUNCTIONAL CORE - Pure Business Logic (No I/O)
// ============================================================================

/// Parse CUE validation result (PURE - no I/O)
/// Interprets command output to determine validation success/failure
pub fn parse_cue_validation_result(
  path: String,
  result: Result(String, #(Int, String)),
) -> Result(Nil, LoadPlanError) {
  case result {
    Ok(_stdout) -> Ok(Nil)
    Error(#(exit_code, stderr)) ->
      Error(CueValidationFailed(path, exit_code, stderr))
  }
}

/// Parse CUE export result (PURE - no I/O)
/// Converts CUE export output to JSON string or error
pub fn parse_cue_export_result(
  path: String,
  result: Result(String, #(Int, String)),
) -> Result(String, LoadPlanError) {
  case result {
    Ok(json_str) -> Ok(json_str)
    Error(#(exit_code, stderr)) ->
      Error(CueExportFailed(path, exit_code, stderr))
  }
}

/// Parse JSON string to Plan (PURE - no I/O)
/// Decodes JSON and validates against Plan schema
pub fn parse_json_to_plan(json_str: String) -> Result(Plan, LoadPlanError) {
  case json.decode(json_str, dynamic.dynamic) {
    Ok(data) ->
      case parser.parse_plan(data) {
        Ok(plan) -> Ok(plan)
        Error(errors) -> Error(PlanParseFailed(errors))
      }
    Error(json_error) -> {
      // Convert json.DecodeError to List(dynamic.DecodeError) for consistency
      let decode_errors = json_error_to_decode_errors(json_error)
      Error(JsonDecodeFailed(decode_errors))
    }
  }
}

// ============================================================================
// IMPERATIVE SHELL - I/O Wrappers with Dependency Injection
// ============================================================================

/// Validate Plan CUE file using injected command executor (Imperative Shell)
/// This function performs I/O but delegates business logic to pure functions
pub fn validate_plan_cue_with_executor(
  path: String,
  executor: CommandExecutor,
) -> Result(Nil, LoadPlanError) {
  case security.validate_file_path(path) {
    Ok(validated_path) -> {
      let result = executor("cue", ["vet", validated_path], ".")
      parse_cue_validation_result(validated_path, result)
    }
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

/// Export Plan CUE file to JSON using injected command executor (Imperative Shell)
/// This function performs I/O but delegates business logic to pure functions
pub fn export_plan_cue_with_executor(
  path: String,
  executor: CommandExecutor,
) -> Result(String, LoadPlanError) {
  case security.validate_file_path(path) {
    Ok(validated_path) -> {
      let result =
        executor("cue", ["export", validated_path, "-e", "plan"], ".")
      parse_cue_export_result(validated_path, result)
    }
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

/// Load plan with injected command executor (Imperative Shell)
/// This orchestrates the entire loading process with dependency-injected I/O
pub fn load_plan_with_executor(
  path: String,
  executor: CommandExecutor,
) -> Result(Plan, LoadPlanError) {
  case security.validate_file_path(path) {
    Ok(validated_path) -> {
      // Execute validation via injected executor
      let validation_result = executor("cue", ["vet", validated_path], ".")

      // Parse validation result using pure function
      case parse_cue_validation_result(validated_path, validation_result) {
        Ok(_) -> {
          // Execute export via injected executor
          let export_result =
            executor("cue", ["export", validated_path, "-e", "plan"], ".")

          // Parse export result and convert to Plan using pure functions
          case parse_cue_export_result(validated_path, export_result) {
            Ok(json_str) -> parse_json_to_plan(json_str)
            Error(e) -> Error(e)
          }
        }
        Error(e) -> Error(e)
      }
    }
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

// ============================================================================
// PUBLIC API - Default Implementations
// ============================================================================

/// Load a Plan from a CUE file
/// Uses default shellout executor
pub fn load_plan(path: String) -> Result(Plan, LoadPlanError) {
  load_plan_with_executor(path, default_executor)
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Convert json.DecodeError to List(dynamic.DecodeError) for structured error handling
fn json_error_to_decode_errors(
  error: json.DecodeError,
) -> List(dynamic.DecodeError) {
  case error {
    json.UnexpectedFormat(errs) -> errs
    json.UnexpectedEndOfInput -> [
      dynamic.DecodeError(
        expected: "complete JSON",
        found: "unexpected end of input",
        path: [],
      ),
    ]
    json.UnexpectedByte(b) -> [
      dynamic.DecodeError(
        expected: "valid JSON character",
        found: "unexpected byte '" <> b <> "'",
        path: [],
      ),
    ]
    json.UnexpectedSequence(s) -> [
      dynamic.DecodeError(
        expected: "valid JSON syntax",
        found: "unexpected sequence '" <> s <> "'",
        path: [],
      ),
    ]
  }
}

fn format_decode_errors(errors: List(dynamic.DecodeError)) -> String {
  case errors {
    [] -> "Unknown decode error"
    [error] -> format_single_decode_error(error)
    multiple -> {
      "Multiple decode errors:\n"
      <> string.join(
        list.map(multiple, fn(e) { "  • " <> format_single_decode_error(e) }),
        "\n",
      )
    }
  }
}

fn format_single_decode_error(error: dynamic.DecodeError) -> String {
  let path_str = case error.path {
    [] -> "at root"
    path_parts ->
      "at "
      <> string.join(path_parts, ".")
      <> " (path: ."
      <> string.join(path_parts, ".")
      <> ")"
  }

  "Expected "
  <> error.expected
  <> " but found "
  <> error.found
  <> " "
  <> path_str
}

/// Format a LoadPlanError as a human-readable string
pub fn format_error(error: LoadPlanError) -> String {
  case error {
    FileNotFound(path) -> "File not found: " <> path
    CueValidationFailed(path, exit_code, stderr) ->
      "CUE validation failed for '"
      <> path
      <> "' (exit code "
      <> string.inspect(exit_code)
      <> "):\n"
      <> stderr
    CueExportFailed(path, exit_code, stderr) ->
      "CUE export failed for '"
      <> path
      <> "' (exit code "
      <> string.inspect(exit_code)
      <> "):\n"
      <> stderr
    JsonDecodeFailed(errors) ->
      "JSON decode error:\n" <> format_decode_errors(errors)
    PlanParseFailed(errors) ->
      "Plan parse error:\n" <> format_decode_errors(errors)
    SecurityError(msg) -> msg
  }
}
