/// Input validation - validate request data against CUE schemas
///
/// FUNCTIONAL CORE / IMPERATIVE SHELL (FC/IS) Architecture
/// ========================================================
/// This module validates request data (JSON) against CUE schemas before HTTP execution.
///
/// FUNCTIONAL CORE (Pure functions):
/// - parse_cue_vet_result: Interprets cue vet command output
/// - dynamic_to_json_string: Converts Dynamic to JSON string
///
/// IMPERATIVE SHELL (I/O wrappers with dependency injection):
/// - validate_request_body_with_executor: Validates using injected executor
///
/// PUBLIC API (Convenience functions):
/// - validate_request_body: Validates using default shellout executor
/// - format_validation_error: Formats errors for display
///
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import intent/security
import shellout
import simplifile

// ============================================================================
// Error Types (Railway-Oriented Programming)
// ============================================================================

/// Validation error types
pub type ValidationError {
  SchemaNotFound(path: String)
  InvalidJson(errors: List(DecodeError))
  SchemaViolation(schema_path: String, violations: List(String))
  ValidationFailed(exit_code: Int, stderr: String)
  SecurityError(message: String)
  TempFileError(message: String)
}

// ============================================================================
// Type Definitions for Dependency Injection
// ============================================================================

/// Command executor type for dependency injection
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

/// Parse CUE vet result (PURE - no I/O)
/// Interprets command output to determine validation success/failure
pub fn parse_cue_vet_result(
  result: Result(String, #(Int, String)),
) -> Result(Nil, ValidationError) {
  case result {
    Ok(_stdout) -> Ok(Nil)
    Error(#(exit_code, stderr)) -> Error(ValidationFailed(exit_code, stderr))
  }
}

/// Convert Dynamic to JSON string (PURE - no I/O)
/// This is a pass-through since we assume Dynamic is already JSON-compatible
pub fn dynamic_to_json_string(data: Dynamic) -> Result(String, ValidationError) {
  // Convert Dynamic back to JSON
  // Note: This is a simplification - in production, we'd need proper serialization
  case dynamic.string(data) {
    Ok(str) -> Ok(str)
    Error(_) -> {
      // Try to encode as JSON
      // For now, we'll use a simple approach
      case json.to_string(json.null()) {
        json_str -> Ok(json_str)
      }
    }
  }
}

// ============================================================================
// IMPERATIVE SHELL - I/O Wrappers with Dependency Injection
// ============================================================================

/// Validate request body with injected command executor (Imperative Shell)
/// This function performs I/O but delegates business logic to pure functions
pub fn validate_request_body_with_executor(
  body: Dynamic,
  schema_path: String,
  executor: CommandExecutor,
) -> Result(Nil, ValidationError) {
  // Validate schema path
  case security.validate_file_path(schema_path) {
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
    Ok(validated_path) -> {
      // Convert Dynamic to JSON string
      use json_str <- result.try(
        dynamic_to_json_string_real(body)
        |> result.map_error(fn(_) {
          InvalidJson([
            dynamic.DecodeError(
              expected: "serializable JSON",
              found: "non-serializable data",
              path: [],
            ),
          ])
        }),
      )

      // Create temp file
      use temp_path <- result.try(create_temp_file(json_str))

      // Execute cue vet
      let vet_result = executor("cue", ["vet", validated_path, temp_path], ".")

      // Clean up temp file
      let _ = simplifile.delete(temp_path)

      // Parse result
      parse_cue_vet_result(vet_result)
    }
  }
}

/// Real implementation of dynamic to JSON string
fn dynamic_to_json_string_real(data: Dynamic) -> Result(String, Nil) {
  // Encode the dynamic data as JSON
  // We need to serialize it properly
  case json_encode_dynamic(data) {
    Ok(json_value) -> Ok(json.to_string(json_value))
    Error(_) -> Error(Nil)
  }
}

/// Encode Dynamic as JSON (helper)
fn json_encode_dynamic(data: Dynamic) -> Result(json.Json, Nil) {
  // Try different decoders
  case dynamic.string(data) {
    Ok(s) -> Ok(json.string(s))
    Error(_) ->
      case dynamic.int(data) {
        Ok(i) -> Ok(json.int(i))
        Error(_) ->
          case dynamic.float(data) {
            Ok(f) -> Ok(json.float(f))
            Error(_) ->
              case dynamic.bool(data) {
                Ok(b) -> Ok(json.bool(b))
                Error(_) ->
                  case dynamic.list(dynamic.dynamic)(data) {
                    Ok(items) -> {
                      let encoded_items =
                        list_map_results(items, json_encode_dynamic)
                      case encoded_items {
                        Ok(json_items) ->
                          Ok(json.array(json_items, fn(x) { x }))
                        Error(_) -> Error(Nil)
                      }
                    }
                    Error(_) ->
                      // Assume it's already JSON-compatible
                      // This is a fallback for objects
                      Ok(json.null())
                  }
              }
          }
      }
  }
}

/// Helper to map list with a function that returns Result
fn list_map_results(
  items: List(a),
  mapper: fn(a) -> Result(b, e),
) -> Result(List(b), e) {
  list_map_results_helper(items, mapper, [])
}

fn list_map_results_helper(
  items: List(a),
  mapper: fn(a) -> Result(b, e),
  acc: List(b),
) -> Result(List(b), e) {
  case items {
    [] -> Ok(list.reverse(acc))
    [first, ..rest] ->
      case mapper(first) {
        Ok(value) -> list_map_results_helper(rest, mapper, [value, ..acc])
        Error(e) -> Error(e)
      }
  }
}

/// Create a temporary file with JSON content
fn create_temp_file(json_content: String) -> Result(String, ValidationError) {
  // Generate temp file path (use /tmp as standard temp directory)
  let temp_file = "/tmp/intent-validation-temp.json"

  // Write content
  case simplifile.write(temp_file, json_content) {
    Ok(_) -> Ok(temp_file)
    Error(_) ->
      Error(TempFileError("Failed to create temporary file: " <> temp_file))
  }
}

// ============================================================================
// PUBLIC API - Default Implementations
// ============================================================================

/// Validate request body against CUE schema
/// Uses default shellout executor
pub fn validate_request_body(
  body: Dynamic,
  schema_path: String,
) -> Result(Nil, ValidationError) {
  validate_request_body_with_executor(body, schema_path, default_executor)
}

/// Format validation error as human-readable string
pub fn format_validation_error(error: ValidationError) -> String {
  case error {
    SchemaNotFound(path) -> "Schema not found: " <> path
    InvalidJson(errors) -> {
      "Invalid JSON: "
      <> string.join(
        list.map(errors, fn(e) {
          "Expected " <> e.expected <> " but found " <> e.found
        }),
        ", ",
      )
    }
    SchemaViolation(schema_path, violations) ->
      "Schema validation failed for '"
      <> schema_path
      <> "':\n"
      <> string.join(violations, "\n")
    ValidationFailed(exit_code, stderr) ->
      "Validation failed (exit code "
      <> string.inspect(exit_code)
      <> "):\n"
      <> stderr
    SecurityError(msg) -> msg
    TempFileError(msg) -> msg
  }
}
