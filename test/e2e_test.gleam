//// End-to-end tests for Intent CLI
////
//// These tests execute the actual CLI binary via subprocess and verify:
//// - Exit codes for success/failure scenarios
//// - JSON output format and structure
//// - Error handling and messages
////
//// TDD15 bead: intent-cli-avoz

import gleam/dynamic
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import gleeunit/should
import shellout

// ============================================================================
// Constants
// ============================================================================

const exit_pass = 0

const exit_error = 4

const examples_dir = "examples"

// ============================================================================
// Test Infrastructure
// ============================================================================

fn run_intent(args: List(String)) -> #(Int, String, String) {
  case
    shellout.command(
      run: "gleam",
      with: ["run", "--", ..args],
      in: ".",
      opt: [],
    )
  {
    Ok(stdout) -> #(0, stdout, "")
    Error(#(code, output)) -> #(code, "", output)
  }
}

fn get_exit_code(args: List(String)) -> Int {
  let #(code, _, _) = run_intent(args)
  code
}

fn get_stdout(args: List(String)) -> String {
  let #(_, stdout, _) = run_intent(args)
  stdout
}

fn extract_json(output: String) -> String {
  output
  |> string.split("\n")
  |> list.filter(fn(line) { string.starts_with(line, "{") })
  |> string.join("\n")
}

fn parse_json_output(output: String) -> Result(dynamic.Dynamic, Nil) {
  json.decode(output, dynamic.dynamic)
  |> result.map_error(fn(_) { Nil })
}

fn is_valid_json_response(output: String) -> Bool {
  let json_output = extract_json(output)
  case parse_json_output(json_output) {
    Ok(data) -> {
      let has_success =
        data
        |> dynamic.field("success", dynamic.bool)
        |> result.is_ok()
      let has_action =
        data
        |> dynamic.field("action", dynamic.string)
        |> result.is_ok()
      has_success && has_action
    }
    Error(_) -> False
  }
}

// ============================================================================
// Validate Command Tests
// ============================================================================

pub fn e2e_validate_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["validate", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_validate_no_args_returns_exit_4_test() {
  let code = get_exit_code(["validate"])
  code |> should.equal(exit_error)
}

pub fn e2e_validate_outputs_valid_json_test() {
  let output = get_stdout(["validate", examples_dir <> "/user-api.cue"])
  output |> is_valid_json_response() |> should.be_true()
}

// ============================================================================
// Lint Command Tests
// ============================================================================

pub fn e2e_lint_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["lint", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_lint_no_args_returns_exit_4_test() {
  let code = get_exit_code(["lint"])
  code |> should.equal(exit_error)
}

// ============================================================================
// Quality Command Tests (KIRK)
// ============================================================================

pub fn e2e_quality_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["quality", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_quality_json_output_is_valid_test() {
  let output =
    get_stdout(["quality", examples_dir <> "/user-api.cue", "--json=true"])
  output |> is_valid_json_response() |> should.be_true()
}

// ============================================================================
// Coverage Command Tests (KIRK)
// ============================================================================

pub fn e2e_coverage_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["coverage", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_coverage_json_output_is_valid_test() {
  let output =
    get_stdout(["coverage", examples_dir <> "/user-api.cue", "--json=true"])
  output |> is_valid_json_response() |> should.be_true()
}

// ============================================================================
// Gaps Command Tests (KIRK)
// ============================================================================

pub fn e2e_gaps_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["gaps", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_gaps_json_output_is_valid_test() {
  let output =
    get_stdout(["gaps", examples_dir <> "/user-api.cue", "--json=true"])
  output |> is_valid_json_response() |> should.be_true()
}

// ============================================================================
// Invert Command Tests (KIRK)
// ============================================================================

pub fn e2e_invert_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["invert", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_invert_json_output_is_valid_test() {
  let output =
    get_stdout(["invert", examples_dir <> "/user-api.cue", "--json=true"])
  output |> is_valid_json_response() |> should.be_true()
}

// ============================================================================
// Effects Command Tests (KIRK)
// ============================================================================

pub fn e2e_effects_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["effects", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_effects_json_output_is_valid_test() {
  let output =
    get_stdout(["effects", examples_dir <> "/user-api.cue", "--json=true"])
  output |> is_valid_json_response() |> should.be_true()
}

// ============================================================================
// Show Command Tests
// ============================================================================

pub fn e2e_show_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["show", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_show_json_output_is_valid_test() {
  let output = get_stdout(["show", examples_dir <> "/user-api.cue"])
  output |> is_valid_json_response() |> should.be_true()
}

pub fn e2e_show_missing_file_returns_error_test() {
  let code = get_exit_code(["show", "nonexistent.cue"])
  code |> should.equal(exit_error)
}

// ============================================================================
// Doctor Command Tests
// ============================================================================

pub fn e2e_doctor_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["doctor", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_doctor_json_output_is_valid_test() {
  let output =
    get_stdout(["doctor", examples_dir <> "/user-api.cue", "--json=true"])
  output |> is_valid_json_response() |> should.be_true()
}

// ============================================================================
// Analyze/Improve Command Tests
// ============================================================================

pub fn e2e_analyze_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["analyze", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_improve_valid_spec_returns_exit_0_test() {
  let code = get_exit_code(["improve", examples_dir <> "/user-api.cue"])
  code |> should.equal(exit_pass)
}

// ============================================================================
// Help Command Tests
// ============================================================================

pub fn e2e_help_returns_exit_0_test() {
  let code = get_exit_code(["help"])
  code |> should.equal(exit_pass)
}

pub fn e2e_unknown_command_returns_error_test() {
  let code = get_exit_code(["nonexistent-command"])
  code |> should.not_equal(exit_pass)
}

// ============================================================================
// Sessions Command Tests
// ============================================================================

pub fn e2e_sessions_returns_exit_0_test() {
  let code = get_exit_code(["sessions"])
  code |> should.equal(exit_pass)
}

// ============================================================================
// Multiple Specs Tests
// ============================================================================

pub fn e2e_validate_array_validation_spec_test() {
  let code =
    get_exit_code(["validate", examples_dir <> "/array-validation.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_validate_regex_rules_spec_test() {
  let code = get_exit_code(["validate", examples_dir <> "/regex-rules.cue"])
  code |> should.equal(exit_pass)
}

pub fn e2e_validate_nested_paths_spec_test() {
  let code = get_exit_code(["validate", examples_dir <> "/nested-paths.cue"])
  code |> should.equal(exit_pass)
}

// ============================================================================
// Exit Code Consistency Tests
// ============================================================================

pub fn e2e_all_kirk_commands_same_success_exit_code_test() {
  let commands = ["quality", "coverage", "gaps", "invert", "effects"]
  let spec = examples_dir <> "/user-api.cue"

  let results = list.map(commands, fn(cmd) { get_exit_code([cmd, spec]) })

  list.all(results, fn(code) { code == exit_pass }) |> should.be_true()
}

pub fn e2e_commands_with_missing_args_return_exit_error_test() {
  let commands = ["validate", "lint", "quality", "show", "doctor"]

  let results = list.map(commands, fn(cmd) { get_exit_code([cmd]) })

  list.all(results, fn(code) { code == exit_error }) |> should.be_true()
}

// ============================================================================
// JSON Output Structure Tests
// ============================================================================

pub fn e2e_json_output_has_metadata_test() {
  let output =
    get_stdout(["quality", examples_dir <> "/user-api.cue", "--json=true"])
  let json_output = extract_json(output)

  case parse_json_output(json_output) {
    Ok(data) -> {
      let has_metadata =
        data
        |> dynamic.field("metadata", dynamic.dynamic)
        |> result.is_ok()
      has_metadata |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn e2e_json_output_has_next_actions_test() {
  let output =
    get_stdout(["quality", examples_dir <> "/user-api.cue", "--json=true"])
  let json_output = extract_json(output)

  case parse_json_output(json_output) {
    Ok(data) -> {
      let has_next_actions =
        data
        |> dynamic.field("next_actions", dynamic.list(dynamic.dynamic))
        |> result.is_ok()
      has_next_actions |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}
