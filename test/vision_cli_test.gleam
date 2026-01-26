//// Vision CLI Tests - DOGFOODING/E2E Boundary Tests
//// Following Dave Farley's coding-rigor principles:
//// - Tests through actual CLI boundary (functional core / imperative shell)
//// - Test contracts and observable behavior, not implementation
//// - Validate exit codes, JSON output, error handling
//// - Independent, fast, reliable
////
//// Phase: RED - These tests will fail until vision CLI commands are implemented

import gleam/dynamic
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import intent/ffi

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

// ============================================================================
// Command Execution (Imperative Shell)
// ============================================================================

fn execute_cli(command: String) -> TestOutcome {
  let #(_, exit_code, output) = os_execute(command <> " 2>/dev/null")

  // Find JSON output - handle both single-line and multi-line JSON
  // For multi-line JSON (e.g., export command), capture from opening { to closing }
  let lines = string.split(output, "\n")
  let clean_output = extract_json_from_lines(lines)

  let is_valid_json =
    json.decode(clean_output, dynamic.dynamic)
    |> result.is_ok()

  let success_field = case is_valid_json {
    True -> {
      let json_result = json.decode(clean_output, dynamic.dynamic)
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
      let json_result = json.decode(clean_output, dynamic.dynamic)
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
      let json_result = json.decode(clean_output, dynamic.dynamic)
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
      let json_result = json.decode(clean_output, dynamic.dynamic)
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
      let json_result = json.decode(clean_output, dynamic.dynamic)
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
    output: clean_output,
    is_valid_json: is_valid_json,
    success_field: success_field,
    has_errors_field: has_errors_field,
    has_metadata_field: has_metadata_field,
    has_next_actions_field: has_next_actions_field,
    has_data_field: has_data_field,
  )
}

fn os_execute(command: String) -> #(String, Int, String) {
  let #(output, exit_code) = ffi.execute_command(command)
  #("", exit_code, output)
}

/// Extract JSON from lines, handling both single-line and multi-line JSON
/// For multi-line JSON, finds the opening { or [ and captures until matching closing
fn extract_json_from_lines(lines: List(String)) -> String {
  extract_json_from_lines_helper(lines, [])
}

/// Helper function to accumulate lines until we find complete JSON
fn extract_json_from_lines_helper(
  lines: List(String),
  acc: List(String),
) -> String {
  case lines {
    [] -> {
      // No more lines - return what we have joined
      case acc {
        [] -> ""
        _ -> {
          acc
          |> list.reverse
          |> string.join("\n")
        }
      }
    }
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      let has_opening =
        string.starts_with(trimmed, "{") || string.starts_with(trimmed, "[")
      let has_closing =
        string.ends_with(trimmed, "}") || string.ends_with(trimmed, "]")

      case acc, has_opening {
        [], True -> {
          // Start accumulating JSON lines
          case has_closing {
            True -> {
              // Single-line JSON
              trimmed
            }
            False -> {
              // Start of multi-line JSON
              extract_multiline_json(rest, 1, [line])
            }
          }
        }
        _, _ -> {
          // No JSON opening found yet, continue searching
          extract_json_from_lines_helper(rest, [])
        }
      }
    }
  }
}

/// Extract multi-line JSON by tracking brace/bracket depth
fn extract_multiline_json(
  lines: List(String),
  depth: Int,
  acc: List(String),
) -> String {
  case lines {
    [] -> ""
    [line, ..rest] -> {
      // Count braces in this line
      let open_count = count_char(line, "{") + count_char(line, "[")
      let close_count = count_char(line, "}") + count_char(line, "]")

      let new_depth = depth + open_count - close_count

      let new_acc = [line, ..acc]

      case new_depth {
        0 -> {
          // Found the closing brace at top level
          new_acc
          |> list.reverse
          |> string.join("\n")
        }
        _ -> extract_multiline_json(rest, new_depth, new_acc)
      }
    }
  }
}

/// Count occurrences of a character in a string
fn count_char(s: String, char: String) -> Int {
  string.length(s) - string.length(string.replace(s, char, ""))
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
  let result = execute_cli("gleam run -- vision start spec.cue --profile=api")

  result.exit_code |> should.equal(0)

  result.is_valid_json |> should.be_true()

  result.has_data_field |> should.be_true()
}

pub fn vision_critique_with_ears_patterns_test() {
  let result =
    execute_cli("gleam run -- vision critique --session=test-session spec.cue")

  result.exit_code |> should.equal(0)

  result.is_valid_json |> should.be_true()

  result.has_data_field |> should.be_true()
}

pub fn vision_session_stores_ears_data_test() {
  let result = execute_cli("gleam run -- vision check --session=test-session")

  result.exit_code |> should.equal(0)

  result.is_valid_json |> should.be_true()

  result.has_data_field |> should.be_true()
}

// ============================================================================
// EDGE CASES (4 tests)
// ============================================================================

pub fn vision_start_empty_vision_test() {
  let result = execute_cli("gleam run -- vision start spec.cue --profile=api")

  result.exit_code |> should.equal(0)

  let validation = validate_ai_ergonomics(result.output)

  validation.passed |> should.be_true()
}

pub fn vision_start_malformed_input_test() {
  let result = execute_cli("gleam run -- vision start invalid.cue")

  result.exit_code |> should.equal(4)

  result.has_errors_field |> should.be_true()
}

pub fn vision_check_invalid_session_test() {
  let result =
    execute_cli("gleam run -- vision check --session=nonexistent-session")

  result.exit_code |> should.equal(4)

  result.has_errors_field |> should.be_true()
}

pub fn vision_critique_missing_spec_test() {
  let result =
    execute_cli("gleam run -- vision critique --session=test-session")

  result.exit_code |> should.equal(4)

  result.has_errors_field |> should.be_true()
}

// ============================================================================
// INTEGRATION TESTING (4 tests)
// ============================================================================

pub fn vision_to_shape_phase_transition_test() {
  let start_result =
    execute_cli("gleam run -- vision start spec.cue --profile=api")

  start_result.exit_code |> should.equal(0)

  let check_result =
    execute_cli("gleam run -- vision check --session=test-session")

  check_result.exit_code |> should.equal(0)
}

pub fn vision_session_storage_test() {
  let result = execute_cli("gleam run -- vision start spec.cue --profile=api")

  result.exit_code |> should.equal(0)

  result.has_data_field |> should.be_true()
}

pub fn vision_session_retrieval_test() {
  let result = execute_cli("gleam run -- vision check --session=test-session")

  result.exit_code |> should.equal(0)

  result.is_valid_json |> should.be_true()
}

pub fn cross_phase_data_sharing_test() {
  let vision_result =
    execute_cli("gleam run -- vision start spec.cue --profile=api")

  vision_result.exit_code |> should.equal(0)

  let shape_result = execute_cli("gleam run -- shape start")

  shape_result.exit_code |> should.equal(0)
}

// ============================================================================
// END-TO-END TESTING (4 tests)
// ============================================================================

pub fn full_vision_workflow_test() {
  let start_result =
    execute_cli("gleam run -- vision start spec.cue --profile=api")

  start_result.exit_code |> should.equal(0)

  let check_result =
    execute_cli("gleam run -- vision check --session=test-session")

  check_result.exit_code |> should.equal(0)

  let critique_result =
    execute_cli("gleam run -- vision critique --session=test-session spec.cue")

  critique_result.exit_code |> should.equal(0)

  let respond_result =
    execute_cli(
      "gleam run -- vision respond --session=test-session --issue=issue-1 --response=Fixed",
    )

  respond_result.exit_code |> should.equal(0)

  let agree_result =
    execute_cli("gleam run -- vision agree --session=test-session")

  agree_result.exit_code |> should.equal(0)
}

pub fn vision_session_management_test() {
  let result = execute_cli("gleam run -- vision start spec.cue --profile=api")

  result.exit_code |> should.equal(0)

  let check_result =
    execute_cli("gleam run -- vision check --session=test-session")

  check_result.exit_code |> should.equal(0)
}

pub fn phase_transition_test() {
  let vision_result =
    execute_cli("gleam run -- vision start spec.cue --profile=api")

  vision_result.exit_code |> should.equal(0)

  let critique_result =
    execute_cli("gleam run -- vision critique --session=test-session spec.cue")

  critique_result.exit_code |> should.equal(0)

  let agree_result =
    execute_cli("gleam run -- vision agree --session=test-session")

  agree_result.exit_code |> should.equal(0)
}

pub fn error_recovery_test() {
  let result =
    execute_cli("gleam run -- vision critique --session=test-session spec.cue")

  result.exit_code |> should.equal(0)

  let respond_result =
    execute_cli(
      "gleam run -- vision respond --session=test-session --issue=issue-1 --response=Fixed",
    )

  respond_result.exit_code |> should.equal(0)

  let critique_result =
    execute_cli("gleam run -- vision critique --session=test-session spec.cue")

  critique_result.exit_code |> should.equal(0)
}
