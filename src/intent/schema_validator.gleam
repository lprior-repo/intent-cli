/// BEAM-native JSON Schema validation using jesse (Erlang)
/// Replaces subprocess-based CUE validation with ~1ms in-process validation
///
/// Pipeline: CUE schemas (source of truth) → JSON Schema files (build-time) → jesse (runtime)
import gleam/result

/// Validation error from schema checking
pub type ValidationError {
  SchemaLoadError(message: String)
  SchemaValidationError(message: String)
  JsonParseError(message: String)
}

/// Validate a JSON string against a JSON Schema string
/// Both must be valid JSON. Returns Ok(Nil) on success.
pub fn validate_json(
  schema_json: String,
  data_json: String,
) -> Result(Nil, ValidationError) {
  case do_validate_json(schema_json, data_json) {
    Ok(Nil) -> Ok(Nil)
    Error(msg) -> Error(SchemaValidationError(msg))
  }
}

@external(erlang, "schema_validator_ffi", "validate_json")
fn do_validate_json(
  schema_json: String,
  data_json: String,
) -> Result(Nil, String)

/// Load a JSON Schema file from the schema/json-schema/ directory
pub fn load_schema(command: String) -> Result(String, ValidationError) {
  let path = schema_path(command)
  case do_load_schema_file(path) {
    Ok(content) -> Ok(content)
    Error(msg) -> Error(SchemaLoadError(msg))
  }
}

@external(erlang, "schema_validator_ffi", "load_schema_file")
fn do_load_schema_file(path: String) -> Result(String, String)

/// Get the schema file path for a command
fn schema_path(command: String) -> String {
  "schema/json-schema/" <> command <> "-response.json"
}

/// Validate command output against its JSON Schema
/// Convenience function: loads schema then validates
pub fn validate_command_output(
  command: String,
  json_str: String,
) -> Result(Nil, ValidationError) {
  use schema_json <- result.try(load_schema(command))
  validate_json(schema_json, json_str)
}

/// Format a validation error for display
pub fn format_error(error: ValidationError) -> String {
  case error {
    SchemaLoadError(msg) -> "Schema load error: " <> msg
    SchemaValidationError(msg) -> "Schema validation failed: " <> msg
    JsonParseError(msg) -> "JSON parse error: " <> msg
  }
}

/// Check if a schema file exists for a given command
pub fn has_schema(command: String) -> Bool {
  case do_load_schema_file(schema_path(command)) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// List of all commands that should have schemas
pub fn all_commands() -> List(String) {
  [
    "validate", "show", "export", "lint", "check", "analyze", "improve",
    "doctor", "quality", "coverage", "gaps", "invert", "effects", "ears",
    "parse", "interview", "sessions", "history", "diff", "beads",
    "beads-regenerate", "bead-status", "plan", "plan-approve", "prompt",
    "feedback", "help",
  ]
}

/// Normalize command name to schema file name
/// Some commands use different action names than their command names
pub fn command_to_schema_name(command: String) -> String {
  case command {
    "analyze" -> "analyze"
    _ -> command
  }
}
