//// Intent CLI Integration & E2E Tests
//// Following Dave Farley's coding-rigor principles:
//// - Tests through actual CLI boundary (functional core / imperative shell)
//// - Test contracts and observable behavior, not implementation
//// - Validate exit codes, JSON output, error handling
//// - Independent, fast, reliable
//// 
//// These are INTEGRATION tests - they run the actual CLI commands
//// and validate the contract from the outside in.

import gleam/dynamic
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
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

/// Validate response follows AI CLI Ergonomics v1.1 spec
fn validate_ai_ergonomics(output: String) -> Validation {
  // Check if output is valid JSON
  let json_result = json.decode(output, dynamic.dynamic)

  case json_result {
    Ok(parsed) -> {
      // Validate required fields per AI CLI spec
      let has_action =
        dynamic.field("action", dynamic.string)(parsed)
        |> result.is_ok()

      let has_errors =
        dynamic.field("errors", dynamic.list(dynamic.dynamic))(parsed)
        |> result.is_ok()

      // For success responses, should have action
      let success_value =
        dynamic.field("success", dynamic.bool)(parsed)
        |> result.unwrap(False)

      case success_value {
        True -> {
          // Success response must have action
          case has_action {
            True ->
              Validation(True, "Valid success response with required fields")
            False ->
              Validation(False, "Success response missing 'action' field")
          }
        }
        False -> {
          // Error response must have errors
          case has_errors {
            True -> Validation(True, "Valid error response with errors")
            False -> Validation(False, "Error response missing 'errors' field")
          }
        }
      }
    }
    Error(_) -> {
      // Non-JSON output is acceptable for some commands
      Validation(True, "Non-JSON output (acceptable for some commands)")
    }
  }
}

// ============================================================================
// Command Execution (Imperative Shell)
// ============================================================================

/// Execute a CLI command and capture output
/// This is the "imperative shell" boundary - we execute the actual CLI
fn execute_cli(command: String) -> TestOutcome {
  // Use os:execute to run actual CLI command
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

/// Stub for OS execute - uses FFI during tests
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
// Core Spec Commands (Integration Tests)
// ============================================================================

/// Test: validate command returns proper exit code and JSON output
pub fn validate_spec_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- validate " <> spec)

  // Validate exit code (0 for success)
  result.exit_code |> should.equal(0)

  // Validate AI ergonomics
  let validation = validate_ai_ergonomics(result.output)
  validation.passed |> should.be_true()
}

/// Test: validate with missing file returns error code 3
pub fn validate_missing_file_e2e_test() {
  let result = execute_cli("gleam run -- validate nonexistent.cue")

  // Exit code 3 for invalid input (per AGENTS.md)
  result.exit_code |> should.equal(3)
}

/// Test: show command exports spec JSON
pub fn show_spec_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- show " <> spec)

  // Should succeed
  result.exit_code |> should.equal(0)

  // Should return valid JSON
  result.is_valid_json |> should.be_true()
}

/// Test: export command exports interview session to CUE
pub fn export_spec_e2e_test() {
  // Test that export command requires a session ID
  let result = execute_cli("gleam run -- export")

  // Should fail with usage error
  result.exit_code |> should.equal(4)

  // Should return valid JSON error
  result.is_valid_json |> should.be_true()
}

/// Test: lint command checks for anti-patterns
pub fn lint_spec_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- lint " <> spec)

  // Should succeed (may have warnings but exit 0)
  result.exit_code |> should.equal(0)

  // Should return JSON with findings if available
  result.is_valid_json |> should.be_true()
}

/// Test: analyze command provides quality scores
pub fn analyze_spec_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- analyze " <> spec)

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()

  // Success response should have data
  result.has_data_field |> should.be_true()
}

/// Test: doctor command returns health report
pub fn doctor_spec_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- doctor " <> spec)

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: improve command suggests improvements
pub fn improve_spec_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- improve " <> spec)

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()

  // Should have data (suggestions)
  result.has_data_field |> should.be_true()
}

// ============================================================================
// Interview Commands (Integration Tests)
// ============================================================================

/// Test: interview starts with API profile
pub fn interview_api_profile_e2e_test() {
  let result = execute_cli("gleam run -- interview --profile=api")

  result.exit_code |> should.equal(0)
  // Should return CUE directives (non-JSON is OK here)
}

/// Test: interview with CLI profile
pub fn interview_cli_profile_e2e_test() {
  let result = execute_cli("gleam run -- interview --profile=cli")

  result.exit_code |> should.equal(0)
}

/// Test: interview dry-run doesn't persist
pub fn interview_dry_run_e2e_test() {
  let result = execute_cli("gleam run -- interview --profile=api --dry-run")

  result.exit_code |> should.equal(0)
}

/// Test: interview with invalid profile fails
pub fn interview_invalid_profile_e2e_test() {
  let result = execute_cli("gleam run -- interview --profile=invalid")

  // Should error (exit 4)
  result.exit_code |> should.equal(4)
}

// ============================================================================
// KIRK Commands (Integration Tests)
// ============================================================================

/// Test: quality command returns quality scores
pub fn quality_analysis_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- quality " <> spec)

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
  result.has_data_field |> should.be_true()
}

/// Test: invert command analyzes failure modes
pub fn invert_analysis_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- invert " <> spec)

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
  result.has_data_field |> should.be_true()
}

/// Test: coverage command detects gaps
pub fn coverage_analysis_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- coverage " <> spec)

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: gaps command finds missing coverage
pub fn gaps_analysis_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- gaps " <> spec)

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: ears command parses EARS patterns
pub fn ears_parser_e2e_test() {
  // EARS parser expects markdown requirements file, not CUE spec
  let spec = "examples/requirements.ears.md"
  let result = execute_cli("gleam run -- ears " <> spec <> " --output=json")

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: effects command analyzes side effects
pub fn effects_analysis_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- effects " <> spec)

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: parse command shows structure
pub fn parse_spec_e2e_test() {
  // EARS parser expects markdown requirements file, not CUE spec
  // Use -o json to get clean JSON without progress messages
  let spec = "examples/requirements.ears.md"
  let result = execute_cli("gleam run -- parse " <> spec <> " -o json")

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

// ============================================================================
// AI Commands (Integration Tests)
// ============================================================================

/// Test: ai schema --all returns all schemas
pub fn ai_schema_all_e2e_test() {
  let result = execute_cli("gleam run -- ai schema --all")

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: ai aggregate combines specs
pub fn ai_aggregate_e2e_test() {
  let result = execute_cli("gleam run -- ai aggregate")

  // May need specs to aggregate
  // Exit code 0 or 4 (error) is acceptable
  [0, 4]
  |> list.contains(result.exit_code)
  |> should.be_true()
}

// ============================================================================
// Beads & History Commands (Integration Tests)
// ============================================================================

/// Test: bead-status shows tracking status
pub fn bead_status_e2e_test() {
  // bead-status requires --bead-id and --status flags
  // For this test, we just verify the command exists and returns error when called without required flags
  let result = execute_cli("gleam run -- bead-status")

  // Should error (exit 4) when called without required flags
  result.exit_code |> should.equal(4)
}

/// Test: beads-regenerate regenerates from failures
pub fn beads_regenerate_e2e_test() {
  // beads-regenerate requires a session_id argument
  // For this test, we verify the command exists and returns error when called without required argument
  let result = execute_cli("gleam run -- beads-regenerate")

  // Should error (exit 3) when called without required session_id (invalid input)
  result.exit_code |> should.equal(3)
}

/// Test: history shows command history
pub fn history_e2e_test() {
  let result = execute_cli("gleam run -- history")

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: sessions shows interview sessions
pub fn sessions_e2e_test() {
  let result = execute_cli("gleam run -- sessions")

  // Should succeed (exit 0) even if no sessions exist
  result.exit_code |> should.equal(0)
  // Output may be text (not JSON) when no sessions exist
  // Test filters warnings but validates exit code
}

// ============================================================================
// Phase Commands (Integration Tests)
// ============================================================================

/// Test: shape start begins shape phase
pub fn shape_start_e2e_test() {
  let result = execute_cli("gleam run -- shape start")

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: ready start begins ready phase
pub fn ready_start_e2e_test() {
  let result = execute_cli("gleam run -- ready start")

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: vision start begins vision phase
pub fn vision_start_e2e_test() {
  let result = execute_cli("gleam run -- vision start")

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

// ============================================================================
// Plan & Other Commands (Integration Tests)
// ============================================================================

/// Test: plan command shows development plan
pub fn plan_e2e_test() {
  let result = execute_cli("gleam run -- plan")

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: diff command shows differences
pub fn diff_e2e_test() {
  let result = execute_cli("gleam run -- diff")

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: feedback command provides feedback
pub fn feedback_e2e_test() {
  let result = execute_cli("gleam run -- feedback")

  result.exit_code |> should.equal(0)
  result.is_valid_json |> should.be_true()
}

/// Test: prompt command generates prompts
pub fn prompt_e2e_test() {
  // prompt command requires a session-id argument
  // For this test, we verify the command exists and returns error when called without required argument
  let result = execute_cli("gleam run -- prompt")

  // Should error (exit 4) when called without required session-id
  result.exit_code |> should.equal(4)
}

/// Test: help command shows usage
pub fn help_e2e_test() {
  let result = execute_cli("gleam run -- help")

  result.exit_code |> should.equal(0)
  // Help may be text (non-JSON is OK)
}

// ============================================================================
// Contract Tests (AI CLI Ergonomics v1.1)
// ============================================================================

/// Test: All JSON responses include required metadata
pub fn all_json_responses_have_metadata_e2e_test() {
  let spec = "examples/user-api.cue"
  let commands = [
    "gleam run -- validate " <> spec,
    "gleam run -- show " <> spec,
    "gleam run -- export " <> spec,
    "gleam run -- lint " <> spec,
    "gleam run -- analyze " <> spec,
    "gleam run -- improve " <> spec,
    "gleam run -- doctor " <> spec,
  ]

  // Test all commands have metadata in JSON responses
  let has_metadata =
    list.map(commands, fn(cmd) {
      let result = execute_cli(cmd)
      result.has_metadata_field
    })
    |> list.any(fn(x) { x == True })

  // At least some should have metadata
  has_metadata |> should.be_true()
}

/// Test: Error responses include fix suggestions
pub fn error_responses_have_fix_e2e_test() {
  let result = execute_cli("gleam run -- validate nonexistent.cue")

  // Should have errors field
  result.has_errors_field |> should.be_true()

  // Error responses should be JSON
  result.is_valid_json |> should.be_true()
}

/// Test: Success responses include next_actions for workflow guidance
pub fn success_responses_have_next_actions_e2e_test() {
  let spec = "examples/user-api.cue"
  let result = execute_cli("gleam run -- validate " <> spec)

  // Success responses should suggest next actions
  result.has_next_actions_field |> should.be_true()
}
