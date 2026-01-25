/// CUE spec loader - loads and validates CUE files using the cue command
///
/// FUNCTIONAL CORE / IMPERATIVE SHELL (FC/IS) Architecture
/// ========================================================
/// This module separates pure business logic from I/O operations:
///
/// FUNCTIONAL CORE (Pure functions):
/// - parse_cue_validation_result: Interprets command output
/// - parse_cue_export_result: Converts export output to JSON
/// - parse_json_to_spec: Decodes JSON to Spec type
///
/// IMPERATIVE SHELL (I/O wrappers with dependency injection):
/// - validate_cue_with_executor: Runs CUE validation via injected executor
/// - export_cue_with_executor: Exports CUE via injected executor
/// - load_spec_with_executor: Orchestrates full load with injected I/O
///
/// PUBLIC API (Convenience functions using default implementations):
/// - validate_cue: Validates using shellout
/// - export_spec_json: Exports using shellout
/// - load_spec: Loads using shellout
/// - load_spec_quiet: Loads using shellout (alias for load_spec)
///
/// Refactored to address beads: intent-cli-3lom, intent-cli-27i7, intent-cli-qc44
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/string
import intent/parser
import intent/security
import intent/types.{type Spec}
import shellout

// ============================================================================
// Error Types (Railway-Oriented Programming)
// ============================================================================

/// Error types for loading specs
/// Railway-Oriented Programming: Preserve all context for programmatic handling
pub type LoadError {
  FileNotFound(path: String)
  CueValidationFailed(path: String, exit_code: Int, stderr: String)
  CueExportFailed(path: String, exit_code: Int, stderr: String)
  JsonDecodeFailed(errors: List(dynamic.DecodeError))
  SpecParseFailed(errors: List(dynamic.DecodeError))
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

/// High-level CUE validator function
pub type CueValidator =
  fn(String) -> Result(Nil, LoadError)

/// High-level CUE exporter function
pub type CueExporter =
  fn(String) -> Result(String, LoadError)

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
) -> Result(Nil, LoadError) {
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
) -> Result(String, LoadError) {
  case result {
    Ok(json_str) -> Ok(json_str)
    Error(#(exit_code, stderr)) ->
      Error(CueExportFailed(path, exit_code, stderr))
  }
}

/// Parse JSON string to Spec (PURE - no I/O)
/// Decodes JSON and validates against Spec schema
pub fn parse_json_to_spec(json_str: String) -> Result(Spec, LoadError) {
  case json.decode(json_str, dynamic.dynamic) {
    Ok(data) ->
      case parser.parse_spec(data) {
        Ok(spec) -> Ok(spec)
        Error(errors) -> Error(SpecParseFailed(errors))
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

/// Validate CUE file using injected command executor (Imperative Shell)
/// This function performs I/O but delegates business logic to pure functions
pub fn validate_cue_with_executor(
  path: String,
  executor: CommandExecutor,
) -> Result(Nil, LoadError) {
  case security.validate_file_path(path) {
    Ok(validated_path) -> {
      let result = executor("cue", ["vet", validated_path], ".")
      parse_cue_validation_result(validated_path, result)
    }
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

/// Default implementation of CUE validation
pub fn default_cue_validator(path: String) -> Result(Nil, LoadError) {
  validate_cue_with_executor(path, default_executor)
}

/// Export CUE file to JSON using injected command executor (Imperative Shell)
/// This function performs I/O but delegates business logic to pure functions
pub fn export_cue_with_executor(
  path: String,
  executor: CommandExecutor,
) -> Result(String, LoadError) {
  case security.validate_file_path(path) {
    Ok(validated_path) -> {
      let result =
        executor("cue", ["export", validated_path, "-e", "spec"], ".")
      parse_cue_export_result(validated_path, result)
    }
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

/// Default implementation of CUE export
pub fn default_cue_exporter(path: String) -> Result(String, LoadError) {
  export_cue_with_executor(path, default_executor)
}

/// Load spec with injected command executor (Imperative Shell)
/// This orchestrates the entire loading process with dependency-injected I/O
pub fn load_spec_with_executor(
  path: String,
  executor: CommandExecutor,
) -> Result(Spec, LoadError) {
  case security.validate_file_path(path) {
    Ok(validated_path) -> {
      // Execute validation via injected executor
      let validation_result = executor("cue", ["vet", validated_path], ".")

      // Parse validation result using pure function
      case parse_cue_validation_result(validated_path, validation_result) {
        Ok(_) -> {
          // Execute export via injected executor
          let export_result =
            executor("cue", ["export", validated_path, "-e", "spec"], ".")

          // Parse export result and convert to Spec using pure functions
          case parse_cue_export_result(validated_path, export_result) {
            Ok(json_str) -> parse_json_to_spec(json_str)
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

/// Validate a CUE file without exporting (CUE syntax only)
/// Uses dependency injection for validation logic
pub fn validate_cue(
  path: String,
  validator: CueValidator,
) -> Result(Nil, LoadError) {
  validator(path)
}

/// Export a spec to JSON format (for AI consumption)
/// Uses dependency injection for export logic
pub fn export_spec_json(
  path: String,
  exporter: CueExporter,
) -> Result(String, LoadError) {
  exporter(path)
}

/// Load a spec from a CUE file
/// Uses default shellout executor
pub fn load_spec(path: String) -> Result(Spec, LoadError) {
  load_spec_with_executor(path, default_executor)
}

/// Load a spec from a CUE file (quiet mode alias)
/// Use this for testing and automation where no UI output is desired
pub fn load_spec_quiet(path: String) -> Result(Spec, LoadError) {
  load_spec_with_executor(path, default_executor)
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

/// Format a LoadError as a human-readable string
pub fn format_error(error: LoadError) -> String {
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
    SpecParseFailed(errors) ->
      "Spec parse error:\n" <> format_decode_errors(errors)
    SecurityError(msg) -> msg
  }
}
