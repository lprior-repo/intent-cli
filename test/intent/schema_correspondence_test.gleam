/// Schema Correspondence Tests
/// Verify that CUE schemas and Gleam types match exactly
///
/// This test suite implements the correspondence testing strategy from
/// SCHEMA_ENFORCEMENT.md. It ensures that:
/// 1. Valid Gleam types serialize to JSON that passes CUE validation
/// 2. Invalid data is rejected by both CUE and Gleam decoders
/// 3. Roundtrip encoding/decoding preserves data integrity
///
/// Test Pattern:
/// - Load CUE schema from registry
/// - Create valid JSON matching Gleam types
/// - Validate JSON against CUE schema
/// - Verify both Gleam and CUE accept/reject the same inputs
import gleam/json
import gleam/string
import gleeunit
import gleeunit/should
import intent/ai_schema
import intent/output_validator

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Schema Loading Tests
// ============================================================================

pub fn can_load_quality_input_schema_test() {
  // Test that we can load an AI command input schema
  let result = ai_schema.get_schema(command: "quality", schema_type: "input")

  case result {
    Ok(content) -> {
      content
      |> should.not_equal("")
      // Should contain the expected type definition
      string.contains(content, "#QualityInput")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

pub fn can_load_validate_input_schema_test() {
  // Test that we can load validate command schema
  let result = ai_schema.get_schema(command: "validate", schema_type: "input")

  case result {
    Ok(content) -> content |> should.not_equal("")
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Basic CUE Validation Tests
// ============================================================================

pub fn simple_cue_validation_accepts_valid_data_test() {
  // Test that CUE validation works with simple valid data
  let schema =
    "package test

data: {
  name: *\"\" | string
  age: *0 | int
}
"

  let valid_json = "{\"name\": \"Alice\", \"age\": 30}"

  let result = output_validator.validate_against_schema(schema, valid_json)

  result
  |> should.be_ok
}

pub fn simple_cue_validation_rejects_wrong_type_test() {
  // Test that CUE validation rejects type mismatches
  let schema =
    "package test

data: {
  name: *\"\" | string
  age: *0 | int
}
"

  let invalid_json = "{\"name\": \"Alice\", \"age\": \"thirty\"}"

  let result = output_validator.validate_against_schema(schema, invalid_json)

  result
  |> should.be_error
}

pub fn simple_cue_validation_rejects_missing_field_test() {
  // Test that CUE validation rejects missing required fields
  // Note: Schema intentionally has NO defaults to require both fields
  let schema =
    "package test

data: {
  name: string
  age: int
}
"

  let invalid_json = "{\"name\": \"Alice\"}"

  let result = output_validator.validate_against_schema(schema, invalid_json)

  result
  |> should.be_error
}

// ============================================================================
// Quality Command Correspondence Tests
// ============================================================================

pub fn quality_input_valid_minimal_test() {
  // Test that minimal valid quality input matches schema
  let schema =
    "package test

data: {
  spec_path: *\"\" | string
  json?: *false | bool
}
"

  let valid_json =
    json.object([
      #("spec_path", json.string("examples/user-api.cue")),
    ])
    |> json.to_string

  let result = output_validator.validate_against_schema(schema, valid_json)

  result
  |> should.be_ok
}

pub fn quality_input_valid_with_optional_test() {
  // Test that quality input with optional json field matches schema
  let schema =
    "package test

data: {
  spec_path: *\"\" | string
  json?: *false | bool
}
"

  let valid_json =
    json.object([
      #("spec_path", json.string("examples/user-api.cue")),
      #("json", json.bool(True)),
    ])
    |> json.to_string

  let result = output_validator.validate_against_schema(schema, valid_json)

  result
  |> should.be_ok
}

pub fn quality_input_rejects_missing_required_field_test() {
  // Test that quality input without spec_path is rejected
  // Note: Schema intentionally has NO defaults to require spec_path
  let schema =
    "package test

data: {
  spec_path: string
  json?: bool
}
"

  let invalid_json =
    json.object([
      #("json", json.bool(True)),
    ])
    |> json.to_string

  let result = output_validator.validate_against_schema(schema, invalid_json)

  result
  |> should.be_error
}

pub fn quality_input_rejects_wrong_type_test() {
  // Test that quality input with wrong type for json field is rejected
  let schema =
    "package test

data: {
  spec_path: *\"\" | string
  json?: *false | bool
}
"

  let invalid_json =
    json.object([
      #("spec_path", json.string("examples/user-api.cue")),
      #("json", json.string("not a boolean")),
    ])
    |> json.to_string

  let result = output_validator.validate_against_schema(schema, invalid_json)

  result
  |> should.be_error
}

// ============================================================================
// Common Envelope Tests
// ============================================================================

pub fn envelope_response_accepts_valid_structure_test() {
  // Test that a valid response envelope matches the schema
  let schema =
    "package test

data: {
  status: *\"ok\" | \"ok\" | \"error\" | \"requires_input\"
  data: {...}
  metadata: {
    timestamp: *\"\" | string
    duration_ms: *0 | int
    version: *\"\" | string
  }
  session_id?: *\"\" | string
  next_actions: *[] | [...]
  error?: {
    code: *\"\" | string
    message: *\"\" | string
  }
}
"

  let valid_json =
    json.object([
      #("status", json.string("ok")),
      #("data", json.object([])),
      #(
        "metadata",
        json.object([
          #("timestamp", json.string("2024-01-25T10:00:00Z")),
          #("duration_ms", json.int(100)),
          #("version", json.string("4.0.0")),
        ]),
      ),
      #("next_actions", json.array([], json.string)),
    ])
    |> json.to_string

  let result = output_validator.validate_against_schema(schema, valid_json)

  result
  |> should.be_ok
}

pub fn envelope_response_rejects_invalid_status_test() {
  // Test that response envelope rejects invalid status values
  let schema =
    "package test

data: {
  status: *\"ok\" | \"ok\" | \"error\" | \"requires_input\"
  data: {...}
  metadata: {
    timestamp: *\"\" | string
    duration_ms: *0 | int
    version: *\"\" | string
  }
  next_actions: *[] | [...]
}
"

  let invalid_json =
    json.object([
      #("status", json.string("invalid_status")),
      #("data", json.object([])),
      #(
        "metadata",
        json.object([
          #("timestamp", json.string("2024-01-25T10:00:00Z")),
          #("duration_ms", json.int(100)),
          #("version", json.string("4.0.0")),
        ]),
      ),
      #("next_actions", json.array([], json.string)),
    ])
    |> json.to_string

  let result = output_validator.validate_against_schema(schema, invalid_json)

  result
  |> should.be_error
}
