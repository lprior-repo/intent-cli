/// Unified JSON output module for AI-friendly responses
///
/// This module provides a consistent action-based JSON schema for ALL commands.
/// When --json flag is used, commands output structured JSON with:
/// - action: What kind of result this is (e.g., "quality_result", "error")
/// - command: Which command produced this (e.g., "quality", "check")
/// - data: Command-specific output
/// - metadata: Timestamp, version, exit code
/// - spec_path: Optional path to spec file
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}

/// Unified JSON response structure for all commands
pub type JsonResponse {
  JsonResponse(
    action: String,
    command: String,
    data: Json,
    metadata: JsonMetadata,
    spec_path: Option(String),
  )
}

/// Metadata included in all JSON responses
pub type JsonMetadata {
  JsonMetadata(timestamp: String, version: String, exit_code: Int)
}

/// Create a JSON response with standard metadata
pub fn create_response(
  action: String,
  command: String,
  data: Json,
  spec_path: Option(String),
  exit_code: Int,
) -> JsonResponse {
  JsonResponse(
    action: action,
    command: command,
    data: data,
    metadata: JsonMetadata(
      timestamp: current_timestamp(),
      version: "0.1.0",
      exit_code: exit_code,
    ),
    spec_path: spec_path,
  )
}

/// Convert JsonResponse to JSON for output
pub fn to_json(response: JsonResponse) -> Json {
  json.object([
    #("action", json.string(response.action)),
    #("command", json.string(response.command)),
    #("data", response.data),
    #("metadata", metadata_to_json(response.metadata)),
    #("spec_path", spec_path_to_json(response.spec_path)),
  ])
}

/// Convert metadata to JSON
fn metadata_to_json(metadata: JsonMetadata) -> Json {
  json.object([
    #("timestamp", json.string(metadata.timestamp)),
    #("version", json.string(metadata.version)),
    #("exit_code", json.int(metadata.exit_code)),
  ])
}

/// Convert optional spec path to JSON
fn spec_path_to_json(spec_path: Option(String)) -> Json {
  case spec_path {
    Some(path) -> json.string(path)
    None -> json.null()
  }
}

/// Output JSON response to stdout
pub fn output(response: JsonResponse) -> Nil {
  response
  |> to_json
  |> json.to_string
  |> io.println
}

/// Get current timestamp in ISO 8601 format
/// Uses FFI to get actual timestamp
@external(erlang, "intent_ffi", "current_timestamp")
fn current_timestamp() -> String

// Required imports
import gleam/io
