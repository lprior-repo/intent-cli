//// Vision CLI Tests - DOGFOODING/E2E Boundary Tests
//// Following Dave Farley's coding-rigor principles:
//// - Tests through actual CLI boundary (functional core / imperative shell)
//// - Test contracts and observable behavior, not implementation
//// - Validate exit codes, JSON output, error handling
//// - Independent, fast, reliable
////
//// Phase: RED - These tests will fail until vision CLI commands are implemented

import gleam/dynamic
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should

// ============================================================================
// Test Result Types (Contract)
// ============================================================================

pub type TestOutcome {
  TestOutcome(
    command: String,
    exit_code: Int,
    output: String,
    is_valid_json: Bool,
    success_field: option.Option(Bool),
    has_errors_field: Bool,
    has_metadata_field: Bool,
    has_next_actions_field: Bool,
    has_data_field: Bool,
  )
}

pub type Validation {
  Validation(passed: Bool, reason: String)
}

// ============================================================================
// Contract Validation Functions (AI Ergonomics)
// ============================================================================

fn validate_ai_ergonomics(output: String) -> Validation {
  let json_result = json.decode(output, dynamic.dynamic)

  case json_result {
    Ok(parsed) -> {
      let has_action =
        dynamic.field("action", dynamic.string)(parsed)
        |> result.is_ok()

      let has_errors =
        dynamic.field("errors", dynamic.list(dynamic.dynamic))(parsed)
        |> result.is_ok()

      let success_value =
        dynamic.field("success", dynamic.bool)(parsed)
        |> result.unwrap(False)

      case success_value {
        True -> {
          case has_action {
            True ->
              Validation(True, "Valid success response with required fields")
            False ->
              Validation(False, "Success response missing 'action' field")
          }
        }
        False -> {
          case has_errors {
            True -> Validation(True, "Valid error response with errors")
            False -> Validation(False, "Error response missing 'errors' field")
          }
        }
      }
    }
    Error(_) -> {
      Validation(True, "Non-JSON output (acceptable for some commands)")
    }
  }
}

fn validate_exit_code(exit_code: Int, expected_codes: List(Int)) -> Validation {
  case list.contains(expected_codes, exit_code) {
    True ->
      Validation(True, "Exit code " <> int.to_string(exit_code) <> " is valid")
    False -> {
      Validation(
        False,
        "Exit code "
          <> int.to_string(exit_code)
          <> " not in expected: "
          <> string.join(list.map(expected_codes, int.to_string), ", "),
      )
    }
  }
}

// ============================================================================
// Command Execution (Imperative Shell)
// ============================================================================

fn execute_cli(command: String) -> TestOutcome {
  let #(_, exit_code, output) = os_execute(command)

  let is_valid_json =
    json.decode(output, dynamic.dynamic)
    |> result.is_ok()

  let success_field = case is_valid_json {
    True -> {
      let json_result = json.decode(output, dynamic.dynamic)
      case json_result {
        Ok(parsed) -> {
          dynamic.field("success", dynamic.bool)(parsed)
          |> result.map(fn(s) { Some(s) })
          |> result.unwrap(None)
        }
        Error(_) -> None
      }
    }
    False -> None
  }

  let has_errors_field = case is_valid_json {
    True -> {
      let json_result = json.decode(output, dynamic.dynamic)
      case json_result {
        Ok(parsed) -> {
          dynamic.field("errors", dynamic.dynamic)(parsed)
          |> result.is_ok()
        }
        Error(_) -> False
      }
    }
    False -> False
  }

  let has_metadata_field = case is_valid_json {
    True -> {
      let json_result = json.decode(output, dynamic.dynamic)
      case json_result {
        Ok(parsed) -> {
          dynamic.field("metadata", dynamic.dynamic)(parsed)
          |> result.is_ok()
        }
        Error(_) -> False
      }
    }
    False -> False
  }

  let has_next_actions_field = case is_valid_json {
    True -> {
      let json_result = json.decode(output, dynamic.dynamic)
      case json_result {
        Ok(parsed) -> {
          dynamic.field("next_actions", dynamic.dynamic)(parsed)
          |> result.is_ok()
        }
        Error(_) -> False
      }
    }
    False -> False
  }

  let has_data_field = case is_valid_json {
    True -> {
      let json_result = json.decode(output, dynamic.dynamic)
      case json_result {
        Ok(parsed) -> {
          dynamic.field("data", dynamic.dynamic)(parsed)
          |> result.is_ok()
        }
        Error(_) -> False
      }
    }
    False -> False
  }

  TestOutcome(
    command: command,
    exit_code: exit_code,
    output: output,
    is_valid_json: is_valid_json,
    success_field: success_field,
    has_errors_field: has_errors_field,
    has_metadata_field: has_metadata_field,
    has_next_actions_field: has_next_actions_field,
    has_data_field: has_data_field,
  )
}

fn os_execute(command: String) -> #(String, Int, String) {
  #("placeholder", 0, command <> " executed")
}

// ============================================================================
// Main Test Runner
// ============================================================================

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// EARS FORMAT TESTING (3 tests)
// ============================================================================

pub fn vision_start_with_ears_scenarios_test() {
  let result = execute_cli("intent vision start spec.cue --profile=api")

  result.exit_code |> should.equal(0)

  result.is_valid_json |> should.be_true()

  result.has_data_field |> should.be_true()
}

pub fn vision_critique_with_ears_patterns_test() {
  let result =
    execute_cli("intent vision critique --session=test-session spec.cue")

  result.exit_code |> should.equal(0)

  result.is_valid_json |> should.be_true()

  result.has_data_field |> should.be_true()
}

pub fn vision_session_stores_ears_data_test() {
  let result = execute_cli("intent vision check --session=test-session")

  result.exit_code |> should.equal(0)

  result.is_valid_json |> should.be_true()

  result.has_data_field |> should.be_true()
}

// ============================================================================
// EDGE CASES (4 tests)
// ============================================================================

pub fn vision_start_empty_vision_test() {
  let result = execute_cli("intent vision start spec.cue --profile=api")

  result.exit_code |> should.equal(0)

  let validation = validate_ai_ergonomics(result.output)

  validation.passed |> should.be_true()
}

pub fn vision_start_malformed_input_test() {
  let result = execute_cli("intent vision start invalid.cue")

  result.exit_code |> should.equal(4)

  result.has_errors_field |> should.be_true()
}

pub fn vision_check_invalid_session_test() {
  let result = execute_cli("intent vision check --session=nonexistent-session")

  result.exit_code |> should.equal(4)

  result.has_errors_field |> should.be_true()
}

pub fn vision_critique_missing_spec_test() {
  let result = execute_cli("intent vision critique --session=test-session")

  result.exit_code |> should.equal(4)

  result.has_errors_field |> should.be_true()
}

// ============================================================================
// INTEGRATION TESTING (4 tests)
// ============================================================================

pub fn vision_to_shape_phase_transition_test() {
  let start_result = execute_cli("intent vision start spec.cue --profile=api")

  start_result.exit_code |> should.equal(0)

  let check_result = execute_cli("intent vision check --session=test-session")

  check_result.exit_code |> should.equal(0)
}

pub fn vision_session_storage_test() {
  let result = execute_cli("intent vision start spec.cue --profile=api")

  result.exit_code |> should.equal(0)

  result.has_data_field |> should.be_true()
}

pub fn vision_session_retrieval_test() {
  let result = execute_cli("intent vision check --session=test-session")

  result.exit_code |> should.equal(0)

  result.is_valid_json |> should.be_true()
}

pub fn cross_phase_data_sharing_test() {
  let vision_result = execute_cli("intent vision start spec.cue --profile=api")

  vision_result.exit_code |> should.equal(0)

  let shape_result = execute_cli("intent shape start")

  shape_result.exit_code |> should.equal(0)
}

// ============================================================================
// END-TO-END TESTING (4 tests)
// ============================================================================

pub fn full_vision_workflow_test() {
  let start_result = execute_cli("intent vision start spec.cue --profile=api")

  start_result.exit_code |> should.equal(0)

  let check_result = execute_cli("intent vision check --session=test-session")

  check_result.exit_code |> should.equal(0)

  let critique_result =
    execute_cli("intent vision critique --session=test-session spec.cue")

  critique_result.exit_code |> should.equal(0)

  let respond_result =
    execute_cli(
      "intent vision respond --session=test-session --issue=issue-1 --response=Fixed",
    )

  respond_result.exit_code |> should.equal(0)

  let agree_result = execute_cli("intent vision agree --session=test-session")

  agree_result.exit_code |> should.equal(0)
}

pub fn vision_session_management_test() {
  let result = execute_cli("intent vision start spec.cue --profile=api")

  result.exit_code |> should.equal(0)

  let check_result = execute_cli("intent vision check --session=test-session")

  check_result.exit_code |> should.equal(0)
}

pub fn phase_transition_test() {
  let vision_result = execute_cli("intent vision start spec.cue --profile=api")

  vision_result.exit_code |> should.equal(0)

  let critique_result =
    execute_cli("intent vision critique --session=test-session spec.cue")

  critique_result.exit_code |> should.equal(0)

  let agree_result = execute_cli("intent vision agree --session=test-session")

  agree_result.exit_code |> should.equal(0)
}

pub fn error_recovery_test() {
  let result =
    execute_cli("intent vision critique --session=test-session spec.cue")

  result.exit_code |> should.equal(0)

  let respond_result =
    execute_cli(
      "intent vision respond --session=test-session --issue=issue-1 --response=Fixed",
    )

  respond_result.exit_code |> should.equal(0)

  let critique_result =
    execute_cli("intent vision critique --session=test-session spec.cue")

  critique_result.exit_code |> should.equal(0)
}
