/// CUE schema loader - loads schema files for command validation
///
/// FUNCTIONAL CORE / IMPERATIVE SHELL (FC/IS) Architecture
/// ========================================================
/// This module separates pure business logic from I/O operations:
///
/// FUNCTIONAL CORE (Pure functions):
/// - build_schema_path: Constructs schema file path from command and type
/// - parse_schema_load_result: Interprets file read output
/// - parse_schema_validation_result: Interprets CUE validation output
///
/// IMPERATIVE SHELL (I/O wrappers with dependency injection):
/// - load_schema_with_executor: Loads schema via injected executor
/// - validate_schema_with_executor: Validates schema syntax via executor
///
/// PUBLIC API (Convenience functions using default implementations):
/// - get_schema: Loads schema using default executor
/// - validate_schema: Validates schema using default executor
///
import gleam/string
import intent/security
import shellout

// ============================================================================
// Error Types (Railway-Oriented Programming)
// ============================================================================

/// Error types for schema loading
/// Railway-Oriented Programming: Preserve all context for programmatic handling
pub type SchemaError {
  SchemaNotFound(command: String, schema_type: SchemaType)
  SchemaLoadFailed(path: String, exit_code: Int, stderr: String)
  SchemaValidationFailed(path: String, exit_code: Int, stderr: String)
  InvalidSchemaPath(path: String, reason: String)
  SecurityError(message: String)
}

/// Type of schema to load (input or output validation)
pub type SchemaType {
  Input
  Output
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

/// Convert SchemaType to string suffix
fn schema_type_to_string(schema_type: SchemaType) -> String {
  case schema_type {
    Input -> "input"
    Output -> "output"
  }
}

/// Construct schema file path from command name and type (PURE - no I/O)
/// Example: "vision.start" + Input -> "schema/commands/vision/start.input.cue"
pub fn build_schema_path(command: String, schema_type: SchemaType) -> String {
  let parts = string.split(command, ".")
  case parts {
    [domain, action] -> {
      let suffix = schema_type_to_string(schema_type)
      "schema/commands/" <> domain <> "/" <> action <> "." <> suffix <> ".cue"
    }
    _ -> {
      // Invalid command format - return a path that will fail validation
      "invalid/" <> command
    }
  }
}

/// Parse schema load result (PURE - no I/O)
/// Converts file read output to schema content or error
pub fn parse_schema_load_result(
  path: String,
  result: Result(String, #(Int, String)),
) -> Result(String, SchemaError) {
  case result {
    Ok(content) -> Ok(content)
    Error(#(exit_code, stderr)) ->
      Error(SchemaLoadFailed(path, exit_code, stderr))
  }
}

/// Parse CUE validation result (PURE - no I/O)
/// Interprets command output to determine validation success/failure
pub fn parse_schema_validation_result(
  path: String,
  result: Result(String, #(Int, String)),
) -> Result(Nil, SchemaError) {
  case result {
    Ok(_stdout) -> Ok(Nil)
    Error(#(exit_code, stderr)) ->
      Error(SchemaValidationFailed(path, exit_code, stderr))
  }
}

// ============================================================================
// IMPERATIVE SHELL - I/O Wrappers with Dependency Injection
// ============================================================================

/// Load schema file using injected command executor (Imperative Shell)
/// This function performs I/O but delegates business logic to pure functions
pub fn load_schema_with_executor(
  command: String,
  schema_type: SchemaType,
  executor: CommandExecutor,
) -> Result(String, SchemaError) {
  let path = build_schema_path(command, schema_type)

  case security.validate_file_path(path) {
    Ok(validated_path) -> {
      let result = executor("cat", [validated_path], ".")
      parse_schema_load_result(path, result)
    }
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

/// Validate CUE schema syntax using injected executor (Imperative Shell)
/// This function performs I/O but delegates business logic to pure functions
pub fn validate_schema_with_executor(
  path: String,
  executor: CommandExecutor,
) -> Result(Nil, SchemaError) {
  case security.validate_file_path(path) {
    Ok(validated_path) -> {
      let result = executor("cue", ["vet", validated_path], ".")
      parse_schema_validation_result(path, result)
    }
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

// ============================================================================
// PUBLIC API - Default Implementations
// ============================================================================

/// Load a schema file for the given command (uses default executor)
/// Example: get_schema("vision.start", Input) loads schema/commands/vision/start.input.cue
pub fn get_schema(
  command: String,
  schema_type: SchemaType,
) -> Result(String, SchemaError) {
  load_schema_with_executor(command, schema_type, default_executor)
}

/// Validate a schema file's CUE syntax (uses default executor)
pub fn validate_schema(
  command: String,
  schema_type: SchemaType,
) -> Result(Nil, SchemaError) {
  let path = build_schema_path(command, schema_type)
  validate_schema_with_executor(path, default_executor)
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Format a SchemaError as a human-readable string
pub fn format_error(error: SchemaError) -> String {
  case error {
    SchemaNotFound(command, schema_type) -> {
      let type_str = schema_type_to_string(schema_type)
      "Schema not found: " <> command <> " (" <> type_str <> ")"
    }
    SchemaLoadFailed(path, exit_code, stderr) ->
      "Failed to load schema '"
      <> path
      <> "' (exit code "
      <> string.inspect(exit_code)
      <> "):\n"
      <> stderr
    SchemaValidationFailed(path, exit_code, stderr) ->
      "Schema validation failed for '"
      <> path
      <> "' (exit code "
      <> string.inspect(exit_code)
      <> "):\n"
      <> stderr
    InvalidSchemaPath(path, reason) ->
      "Invalid schema path '" <> path <> "': " <> reason
    SecurityError(message) -> message
  }
}
