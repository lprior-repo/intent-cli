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
  // Validate inputs
  use _ <- result.try(case string.is_empty(string.trim(schema_cue)) {
    True -> Error(InvalidSchema("Schema cannot be empty"))
    False -> Ok(Nil)
  })

  use _ <- result.try(case string.is_empty(string.trim(data_json)) {
    True -> Error(InvalidJson("Data cannot be empty"))
    False -> Ok(Nil)
  })

  // Generate unique temp file name
  let timestamp = int.to_string(erlang_system_time())
  let validation_file = "/tmp/intent_validation_" <> timestamp <> ".cue"

  // Create a complete CUE file that includes both schema and data
  // Check if schema already has package declaration
  let has_package = string.contains(schema_cue, "package ")
  let data_cue = json_to_cue(data_json)

  use _ <- result.try(case data_cue {
    Ok(cue_data) -> {
      let combined_cue = case has_package {
        True ->
          // Schema already has package, just append data
          schema_cue <> "\n\n// Instance data\n" <> cue_data
        False ->
          // Need to add package declaration
          "package test\n\n"
          <> schema_cue
          <> "\n\n// Instance data\n"
          <> cue_data
      }

      simplifile.write(validation_file, combined_cue)
      |> result.map_error(fn(_) {
        TempFileError("Failed to write validation file")
      })
    }
    Error(_) -> Error(InvalidJson("Failed to convert JSON to CUE format"))
  })

  // Run CUE validation (vet without second argument validates the single file)
  let validation_result = executor("cue", ["vet", validation_file], ".")
  let parsed_result = parse_validation_result(validation_result)

  // Clean up temp file (always)
  let _ = simplifile.delete(validation_file)

  parsed_result
}

/// Convert JSON string to CUE format for validation
fn json_to_cue(json_str: String) -> Result(String, Nil) {
  // Simple JSON to CUE converter for basic validation
  // This is a simplified implementation - in production would need full parser
  let trimmed = string.trim(json_str)

  case trimmed {
    "" -> Error(Nil)
    _ ->
      // For now, wrap the JSON in a 'data' field as-is
      // CUE can parse JSON in this context
      Ok("data: " <> trimmed)
  }
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
