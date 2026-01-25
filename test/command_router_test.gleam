//// Tests for command_router module - JSONL command parsing and routing
//// Phase 4 (RED) -> Phase 5 (GREEN): Tests for command router
//// TDD15 bead: intent-cli-dt9

import gleam/dynamic
import gleam/json
import gleam/string
import gleeunit/should
import intent/command_router

// =============================================================================
// parse_request Tests
// =============================================================================

/// Test: parse_request parses valid complete request with all fields
pub fn parse_request_valid_complete_test() {
  let line =
    "{\"id\":\"req-123\",\"command\":\"quality\",\"args\":{\"spec_path\":\"test.cue\"}}"
  let result = command_router.parse_request(line)

  result |> should.be_ok()
  let assert Ok(request) = result
  request.id |> should.equal("req-123")
  request.command |> should.equal("quality")
}

/// Test: parse_request generates UUID when id is missing
pub fn parse_request_missing_id_generates_uuid_test() {
  let line = "{\"command\":\"coverage\",\"args\":{\"spec_path\":\"test.cue\"}}"
  let result = command_router.parse_request(line)

  result |> should.be_ok()
  let assert Ok(request) = result
  request.command |> should.equal("coverage")
  // ID should be auto-generated (non-empty)
  request.id |> string.length() |> should.not_equal(0)
}

/// Test: parse_request handles missing args with default empty object
pub fn parse_request_missing_args_defaults_empty_test() {
  let line = "{\"id\":\"req-456\",\"command\":\"gaps\"}"
  let result = command_router.parse_request(line)

  result |> should.be_ok()
  let assert Ok(request) = result
  request.command |> should.equal("gaps")
  request.id |> should.equal("req-456")
}

/// Test: parse_request returns MissingCommandField error when command missing
pub fn parse_request_missing_command_returns_error_test() {
  let line = "{\"id\":\"req-789\",\"args\":{\"spec_path\":\"test.cue\"}}"
  let result = command_router.parse_request(line)

  result |> should.be_error()
  case result {
    Error(command_router.MissingCommandField) -> Nil
    _ -> should.fail()
  }
}

/// Test: parse_request returns InvalidJson error for malformed JSON
pub fn parse_request_invalid_json_returns_error_test() {
  let line = "{invalid json"
  let result = command_router.parse_request(line)

  result |> should.be_error()
  case result {
    Error(command_router.InvalidJson(_, _)) -> Nil
    _ -> should.fail()
  }
}

/// Test: parse_request handles empty string
pub fn parse_request_empty_string_returns_error_test() {
  let result = command_router.parse_request("")

  result |> should.be_error()
  case result {
    Error(command_router.InvalidJson(_, _)) -> Nil
    _ -> should.fail()
  }
}

// =============================================================================
// route_request Tests - Known Commands
// =============================================================================

/// Test: route_request dispatches quality command correctly
pub fn route_request_quality_dispatches_correctly_test() {
  let request =
    command_router.CommandRequest(
      id: "test-q",
      command: "quality",
      args: dynamic.from(json.object([#("spec_path", json.string("test.cue"))])),
    )

  let response = command_router.route_request(request)
  response.command |> should.equal("quality")
  response.action |> should.equal("quality_result")
}

/// Test: route_request dispatches coverage command correctly
pub fn route_request_coverage_dispatches_correctly_test() {
  let request =
    command_router.CommandRequest(
      id: "test-c",
      command: "coverage",
      args: dynamic.from(json.object([#("spec_path", json.string("test.cue"))])),
    )

  let response = command_router.route_request(request)
  response.command |> should.equal("coverage")
  response.action |> should.equal("coverage_result")
}

/// Test: route_request dispatches gaps command correctly
pub fn route_request_gaps_dispatches_correctly_test() {
  let request =
    command_router.CommandRequest(
      id: "test-g",
      command: "gaps",
      args: dynamic.from(json.object([#("spec_path", json.string("test.cue"))])),
    )

  let response = command_router.route_request(request)
  response.command |> should.equal("gaps")
  response.action |> should.equal("gaps_result")
}

/// Test: route_request dispatches invert command correctly
pub fn route_request_invert_dispatches_correctly_test() {
  let request =
    command_router.CommandRequest(
      id: "test-i",
      command: "invert",
      args: dynamic.from(json.object([#("spec_path", json.string("test.cue"))])),
    )

  let response = command_router.route_request(request)
  response.command |> should.equal("invert")
  response.action |> should.equal("invert_result")
}

/// Test: route_request dispatches effects command correctly
pub fn route_request_effects_dispatches_correctly_test() {
  let request =
    command_router.CommandRequest(
      id: "test-e",
      command: "effects",
      args: dynamic.from(json.object([#("spec_path", json.string("test.cue"))])),
    )

  let response = command_router.route_request(request)
  response.command |> should.equal("effects")
  response.action |> should.equal("effects_result")
}

// =============================================================================
// route_request Tests - Unknown Commands
// =============================================================================

/// Test: route_request returns UnknownCommand error for nonexistent command
pub fn route_request_unknown_command_returns_error_test() {
  let request =
    command_router.CommandRequest(
      id: "test-unknown",
      command: "nonexistent",
      args: dynamic.from(json.object([])),
    )

  let response = command_router.route_request(request)
  response.success |> should.be_false()
  response.command |> should.equal("nonexistent")
}

/// Test: route_request provides helpful error for unknown command
pub fn route_request_unknown_command_includes_available_commands_test() {
  let request =
    command_router.CommandRequest(
      id: "test-help",
      command: "badcmd",
      args: dynamic.from(json.object([])),
    )

  let response = command_router.route_request(request)
  response.success |> should.be_false()
  // Errors should mention available commands
  response.errors |> should.not_equal([])
}

// =============================================================================
// error_response Tests
// =============================================================================

/// Test: error_response for InvalidJson creates proper error response
pub fn error_response_invalid_json_test() {
  let error = command_router.InvalidJson(line: "{bad}", error: "syntax error")
  let response = command_router.error_response("test-id", error)

  response.success |> should.be_false()
  response.action |> should.equal("error")
  response.errors |> should.not_equal([])
}

/// Test: error_response for MissingCommandField creates proper error response
pub fn error_response_missing_command_field_test() {
  let error = command_router.MissingCommandField
  let response = command_router.error_response("test-id", error)

  response.success |> should.be_false()
  response.action |> should.equal("error")
  response.errors |> should.not_equal([])
}

/// Test: error_response for UnknownCommand includes command name
pub fn error_response_unknown_command_includes_name_test() {
  let error = command_router.UnknownCommand("badcmd")
  let response = command_router.error_response("test-id", error)

  response.success |> should.be_false()
  response.command |> should.equal("badcmd")
  response.errors |> should.not_equal([])
}

/// Test: error_response for HandlerError includes command and error message
pub fn error_response_handler_error_includes_details_test() {
  let error = command_router.HandlerError("quality", "spec file not found")
  let response = command_router.error_response("test-id", error)

  response.success |> should.be_false()
  response.command |> should.equal("quality")
  response.errors |> should.not_equal([])
}

// =============================================================================
// process_single_request Tests - End to End
// =============================================================================

/// Test: process_single_request handles valid quality command
pub fn process_single_request_valid_quality_test() {
  let line = "{\"command\":\"quality\",\"args\":{\"spec_path\":\"test.cue\"}}"
  let response_json = command_router.process_single_request(line)

  // Response should be valid JSON with expected fields
  response_json |> string.contains("\"success\"") |> should.be_true()
  response_json |> string.contains("\"action\"") |> should.be_true()
  response_json |> string.contains("\"command\"") |> should.be_true()
}

/// Test: process_single_request handles invalid JSON input
pub fn process_single_request_invalid_json_test() {
  let line = "{bad json}"
  let response_json = command_router.process_single_request(line)

  // Should return error response as JSON
  response_json |> string.contains("\"success\":false") |> should.be_true()
  response_json |> string.contains("INVALID_JSON") |> should.be_true()
}

/// Test: process_single_request handles missing command field
pub fn process_single_request_missing_command_test() {
  let line = "{\"args\":{\"spec_path\":\"test.cue\"}}"
  let response_json = command_router.process_single_request(line)

  // Should return error response as JSON
  response_json |> string.contains("\"success\":false") |> should.be_true()
  response_json |> string.contains("MISSING_COMMAND_FIELD") |> should.be_true()
}

/// Test: process_single_request handles unknown command
pub fn process_single_request_unknown_command_test() {
  let line = "{\"command\":\"nonexistent\"}"
  let response_json = command_router.process_single_request(line)

  // Should return error response as JSON
  response_json |> string.contains("\"success\":false") |> should.be_true()
  response_json |> string.contains("UNKNOWN_COMMAND") |> should.be_true()
}

// =============================================================================
// get_available_commands Tests
// =============================================================================

/// Test: get_available_commands returns non-empty list
pub fn get_available_commands_returns_list_test() {
  let commands = command_router.get_available_commands()

  commands |> should.not_equal([])
}

/// Test: get_available_commands includes core KIRK commands
pub fn get_available_commands_includes_kirk_commands_test() {
  let commands = command_router.get_available_commands()

  commands |> should.equal(["quality", "coverage", "gaps", "invert", "effects"])
}
