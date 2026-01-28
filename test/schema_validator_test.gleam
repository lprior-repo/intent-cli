import gleam/json
import gleam/option.{Some}
import gleeunit/should
import intent/json_output
import intent/schema_validator

// ============================================================================
// Schema Loading Tests
// ============================================================================

pub fn load_existing_schema_test() {
  schema_validator.load_schema("quality")
  |> should.be_ok
}

pub fn load_missing_schema_returns_error_test() {
  schema_validator.load_schema("nonexistent_command")
  |> should.be_error
}

pub fn has_schema_for_existing_command_test() {
  schema_validator.has_schema("quality")
  |> should.be_true
}

pub fn has_schema_for_missing_command_test() {
  schema_validator.has_schema("nonexistent")
  |> should.be_false
}

// ============================================================================
// Validation Tests
// ============================================================================

pub fn validate_valid_json_against_base_schema_test() {
  let schema_result = schema_validator.load_schema("base")
  let assert Ok(schema_json) = schema_result

  let valid_response =
    json.object([
      #("success", json.bool(True)),
      #("action", json.string("test_result")),
      #("command", json.string("test")),
      #("data", json.object([])),
      #("errors", json.array([], fn(x) { x })),
      #("next_actions", json.array([], fn(x) { x })),
      #(
        "metadata",
        json.object([
          #("timestamp", json.string("2026-01-28T00:00:00Z")),
          #("version", json.string("0.1.0")),
          #("exit_code", json.int(0)),
          #("correlation_id", json.string("test-uuid")),
          #("duration_ms", json.int(0)),
        ]),
      ),
      #("spec_path", json.null()),
    ])
    |> json.to_string

  schema_validator.validate_json(schema_json, valid_response)
  |> should.be_ok
}

pub fn validate_quality_response_test() {
  let response =
    json_output.success(
      "quality_report",
      "quality",
      json.object([
        #("overall_score", json.int(85)),
        #("coverage_score", json.int(80)),
        #("clarity_score", json.int(90)),
        #("testability_score", json.int(85)),
        #("ai_readiness_score", json.int(82)),
        #("issues", json.array([], fn(x) { x })),
        #("suggestions", json.array([], fn(x) { x })),
      ]),
      Some("test.cue"),
      [],
    )

  let json_str = response |> json_output.to_json |> json.to_string
  schema_validator.validate_command_output("quality", json_str)
  |> should.be_ok
}

pub fn validate_command_output_graceful_missing_schema_test() {
  // Commands without schemas should return SchemaLoadError
  schema_validator.validate_command_output("nonexistent", "{}")
  |> should.be_error
}

// ============================================================================
// Error Formatting Tests
// ============================================================================

pub fn format_schema_load_error_test() {
  let err = schema_validator.SchemaLoadError("file not found")
  schema_validator.format_error(err)
  |> should.equal("Schema load error: file not found")
}

pub fn format_validation_error_test() {
  let err = schema_validator.SchemaValidationError("missing field: success")
  schema_validator.format_error(err)
  |> should.equal("Schema validation failed: missing field: success")
}

// ============================================================================
// All Commands List Tests
// ============================================================================

pub fn all_commands_contains_quality_test() {
  schema_validator.all_commands()
  |> list.contains("quality")
  |> should.be_true
}

pub fn all_commands_contains_check_test() {
  schema_validator.all_commands()
  |> list.contains("check")
  |> should.be_true
}

import gleam/list

pub fn all_commands_has_expected_count_test() {
  schema_validator.all_commands()
  |> list.length
  |> should.equal(27)
}
