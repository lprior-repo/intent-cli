/// Unified JSON output module for AI-friendly responses
///
/// This module provides a consistent action-based JSON schema for ALL commands.
/// When --json flag is used, commands output structured JSON with:
/// - success: Boolean indicating if command achieved its goal
/// - action: What kind of result this is (e.g., "check_result", "error")
/// - command: Which command produced this (e.g., "check", "quality")
/// - data: Command-specific output
/// - errors: Array of structured errors (empty if success)
/// - next_actions: Array of suggested follow-up commands
/// - metadata: Timestamp, version, exit code
/// - spec_path: Optional path to spec file
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}
import intent/ffi
import intent/schema_validator

/// Unified JSON response structure for all commands
pub type JsonResponse {
  JsonResponse(
    success: Bool,
    action: String,
    command: String,
    data: Json,
    errors: List(JsonError),
    next_actions: List(NextAction),
    metadata: JsonMetadata,
    spec_path: Option(String),
  )
}

/// Structured error for AI consumption
pub type JsonError {
  JsonError(
    code: String,
    message: String,
    location: Option(String),
    fix_hint: Option(String),
    fix_command: Option(String),
  )
}

/// Suggested follow-up command
pub type NextAction {
  NextAction(command: String, reason: String)
}

/// Metadata included in all JSON responses
pub type JsonMetadata {
  JsonMetadata(
    timestamp: String,
    version: String,
    exit_code: Int,
    correlation_id: String,
    duration_ms: Int,
  )
}

/// Create a successful JSON response
pub fn success(
  action: String,
  command: String,
  data: Json,
  spec_path: Option(String),
  next_actions: List(NextAction),
) -> JsonResponse {
  JsonResponse(
    success: True,
    action: action,
    command: command,
    data: data,
    errors: [],
    next_actions: next_actions,
    metadata: create_metadata(0),
    spec_path: spec_path,
  )
}

/// Create a failure JSON response
pub fn failure(
  action: String,
  command: String,
  data: Json,
  errors: List(JsonError),
  spec_path: Option(String),
  next_actions: List(NextAction),
  exit_code: Int,
) -> JsonResponse {
  JsonResponse(
    success: False,
    action: action,
    command: command,
    data: data,
    errors: errors,
    next_actions: next_actions,
    metadata: create_metadata(exit_code),
    spec_path: spec_path,
  )
}

/// Create a JSON response with explicit success flag (for backwards compatibility)
pub fn create_response(
  action: String,
  command: String,
  data: Json,
  spec_path: Option(String),
  exit_code: Int,
) -> JsonResponse {
  JsonResponse(
    success: exit_code == 0,
    action: action,
    command: command,
    data: data,
    errors: [],
    next_actions: [],
    metadata: create_metadata(exit_code),
    spec_path: spec_path,
  )
}

/// Create a JSON response with all fields
pub fn create_full_response(
  success: Bool,
  action: String,
  command: String,
  data: Json,
  errors: List(JsonError),
  next_actions: List(NextAction),
  spec_path: Option(String),
  exit_code: Int,
) -> JsonResponse {
  JsonResponse(
    success: success,
    action: action,
    command: command,
    data: data,
    errors: errors,
    next_actions: next_actions,
    metadata: create_metadata(exit_code),
    spec_path: spec_path,
  )
}

/// Add next_actions to an existing response
pub fn with_next_actions(
  response: JsonResponse,
  next_actions: List(NextAction),
) -> JsonResponse {
  JsonResponse(..response, next_actions: next_actions)
}

/// Add errors to an existing response
pub fn with_errors(
  response: JsonResponse,
  errors: List(JsonError),
) -> JsonResponse {
  JsonResponse(..response, errors: errors, success: False)
}

/// Create a simple error with fix_command based on error code
pub fn error(code: String, message: String) -> JsonError {
  let fix_command = case code {
    "usage_error" -> "intent validate <spec-file>"
    "validation_error" -> "intent doctor <spec-file> --json"
    "load_error" -> "intent validate <spec-file>"
    "missing_session_id" -> "intent sessions [--profile=api|cli]"
    "parse_error" -> "intent doctor <spec-file> --json"
    _ -> ""
  }
  JsonError(
    code: code,
    message: message,
    location: None,
    fix_hint: None,
    fix_command: Some(fix_command),
  )
}

/// Create a detailed error with all fields
pub fn detailed_error(
  code: String,
  message: String,
  location: String,
  fix_hint: String,
  fix_command: String,
) -> JsonError {
  JsonError(
    code: code,
    message: message,
    location: Some(location),
    fix_hint: Some(fix_hint),
    fix_command: Some(fix_command),
  )
}

/// Create a next action suggestion
pub fn next_action(command: String, reason: String) -> NextAction {
  NextAction(command: command, reason: reason)
}

/// Convert JsonResponse to JSON for output
pub fn to_json(response: JsonResponse) -> Json {
  json.object([
    #("success", json.bool(response.success)),
    #("action", json.string(response.action)),
    #("command", json.string(response.command)),
    #("data", response.data),
    #("errors", errors_to_json(response.errors)),
    #("next_actions", next_actions_to_json(response.next_actions)),
    #("metadata", metadata_to_json(response.metadata)),
    #("spec_path", spec_path_to_json(response.spec_path)),
  ])
}

/// Convert errors list to JSON
fn errors_to_json(errors: List(JsonError)) -> Json {
  json.array(errors, error_to_json)
}

/// Convert single error to JSON
pub fn error_to_json(err: JsonError) -> Json {
  json.object([
    #("code", json.string(err.code)),
    #("message", json.string(err.message)),
    #("location", optional_string_to_json(err.location)),
    #("fix_hint", optional_string_to_json(err.fix_hint)),
    #("fix_command", optional_string_to_json(err.fix_command)),
  ])
}

/// Convert next_actions list to JSON
fn next_actions_to_json(actions: List(NextAction)) -> Json {
  json.array(actions, next_action_to_json)
}

/// Convert single next_action to JSON
fn next_action_to_json(action: NextAction) -> Json {
  json.object([
    #("command", json.string(action.command)),
    #("reason", json.string(action.reason)),
  ])
}

/// Convert metadata to JSON
fn metadata_to_json(metadata: JsonMetadata) -> Json {
  json.object([
    #("timestamp", json.string(metadata.timestamp)),
    #("version", json.string(metadata.version)),
    #("exit_code", json.int(metadata.exit_code)),
    #("correlation_id", json.string(metadata.correlation_id)),
    #("duration_ms", json.int(metadata.duration_ms)),
  ])
}

/// Convert optional spec path to JSON
fn spec_path_to_json(spec_path: Option(String)) -> Json {
  case spec_path {
    Some(path) -> json.string(path)
    None -> json.null()
  }
}

/// Convert optional string to JSON
fn optional_string_to_json(value: Option(String)) -> Json {
  case value {
    Some(s) -> json.string(s)
    None -> json.null()
  }
}

/// Output JSON response to stdout with automatic schema validation.
/// Validates against the command's JSON Schema before printing.
/// AI-native: outputs pure JSON with no warnings - validation issues are silent
/// (schema validation is for development, not production output).
pub fn output(response: JsonResponse) -> Nil {
  let json_str =
    response
    |> to_json
    |> json.to_string
  // Silent validation - don't pollute output with warnings
  // Schema validation is for development-time catching of issues
  let _ = schema_validator.validate_command_output(response.command, json_str)
  io.println(json_str)
}

/// Get current timestamp in ISO 8601 format
/// Uses FFI via the centralized ffi module
fn current_timestamp() -> String {
  ffi.current_timestamp()
}

/// Generate a UUID v4 for correlation tracking
/// Uses FFI via the centralized ffi module
fn generate_uuid() -> String {
  ffi.generate_uuid()
}

/// Create metadata with the given exit code
fn create_metadata(exit_code: Int) -> JsonMetadata {
  JsonMetadata(
    timestamp: current_timestamp(),
    version: "0.1.0",
    exit_code: exit_code,
    correlation_id: generate_uuid(),
    duration_ms: 0,
  )
}

// Required imports
import gleam/io
