//// Output validator - validates JSON responses against CUE schemas
////
//// FUNCTIONAL CORE / IMPERATIVE SHELL (FC/IS) Architecture
//// ========================================================
//// This module separates pure business logic from I/O operations:
////
//// FUNCTIONAL CORE (Pure functions):
//// - parse_validation_result: Interprets CUE command output
//// - format_error: Formats ValidationError for display
////
//// IMPERATIVE SHELL (I/O wrappers with dependency injection):
//// - validate_with_executor: Runs CUE validation via injected executor
////
//// PUBLIC API (Convenience functions using default implementations):
//// - validate_against_schema: Validates using shellout

import gleam/int
import gleam/result
import gleam/string
import shellout
import simplifile

// ============================================================================
// Error Types (Railway-Oriented Programming)
// ============================================================================

/// Error types for validation
pub type ValidationError {
  SchemaValidationFailed(errors: String)
  InvalidSchema(message: String)
  InvalidJson(message: String)
  CueCommandFailed(exit_code: Int, stderr: String)
  TempFileError(message: String)
}

// ============================================================================
// Type Definitions for Dependency Injection
// ============================================================================

/// Command executor type for dependency injection
pub type CommandExecutor =
  fn(String, List(String), String) -> Result(String, #(Int, String))

// ============================================================================
// FUNCTIONAL CORE - Pure Business Logic (No I/O)
// ============================================================================

/// Parse CUE validation result (PURE - no I/O)
pub fn parse_validation_result(
  result: Result(String, #(Int, String)),
) -> Result(Nil, ValidationError) {
  case result {
    Ok(_stdout) -> Ok(Nil)
    Error(#(_exit_code, stderr)) -> Error(SchemaValidationFailed(stderr))
  }
}

/// Format a ValidationError as a human-readable string
pub fn format_error(error: ValidationError) -> String {
  case error {
    SchemaValidationFailed(errors) -> "Schema validation failed:\n  " <> errors
    InvalidSchema(message) -> "Invalid schema: " <> message
    InvalidJson(message) -> "Invalid JSON: " <> message
    CueCommandFailed(exit_code, stderr) ->
      "CUE command failed (exit code "
      <> string.inspect(exit_code)
      <> "):\n  "
      <> stderr
    TempFileError(message) -> "Temporary file error: " <> message
  }
}

// ============================================================================
// IMPERATIVE SHELL - I/O Wrappers with Dependency Injection
// ============================================================================

/// Validate JSON data against CUE schema using injected command executor
pub fn validate_with_executor(
  schema_cue: String,
  data_json: String,
  executor: CommandExecutor,
) -> Result(Nil, ValidationError) {
  // Generate unique temp file names
  let timestamp = int.to_string(erlang_system_time())
  let schema_path = "/tmp/intent_schema_" <> timestamp <> ".cue"
  let data_path = "/tmp/intent_data_" <> timestamp <> ".json"

  // Write schema to file
  use _ <- result.try(
    simplifile.write(schema_path, schema_cue)
    |> result.map_error(fn(_) {
      TempFileError("Failed to write schema to temp file")
    }),
  )

  // Write data to file
  use _ <- result.try(
    simplifile.write(data_path, data_json)
    |> result.map_error(fn(_) {
      let _ = simplifile.delete(schema_path)
      TempFileError("Failed to write data to temp file")
    }),
  )

  // Run CUE validation
  let validation_result = executor("cue", ["vet", schema_path, data_path], ".")
  let parsed_result = parse_validation_result(validation_result)

  // Clean up temp files (always)
  let _ = simplifile.delete(schema_path)
  let _ = simplifile.delete(data_path)

  parsed_result
}

// Helper function to get system time for unique filenames
@external(erlang, "erlang", "system_time")
fn erlang_system_time() -> Int

// ============================================================================
// PUBLIC API - Default Implementations
// ============================================================================

/// Validate JSON data against CUE schema
pub fn validate_against_schema(
  schema_cue: String,
  data_json: String,
) -> Result(Nil, ValidationError) {
  validate_with_executor(schema_cue, data_json, default_executor)
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Default executor using shellout.command
fn default_executor(
  cmd: String,
  args: List(String),
  dir: String,
) -> Result(String, #(Int, String)) {
  shellout.command(run: cmd, with: args, in: dir, opt: [])
}
