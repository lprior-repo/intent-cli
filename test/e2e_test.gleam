//// End-to-end tests for Intent CLI
//// TDD15 bead: intent-cli-avoz

import gleam/dynamic
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import gleeunit/should
import shellout

const exit_pass = 0
const exit_error = 4
const examples_dir = "examples"

fn run_intent(args: List(String)) -> #(Int, String, String) {
  case shellout.command(run: "gleam", with: ["run", "--", ..args], in: ".", opt: []) {
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
  output |> string.split("\n") |> list.filter(fn(line) { string.starts_with(line, "{") }) |> string.join("\n")
}

fn parse_json_output(output: String) -> Result(dynamic.Dynamic, Nil) {
  json.decode(output, dynamic.dynamic) |> result.map_error(fn(_) { Nil })
}

fn is_valid_json_response(output: String) -> Bool {
  let json_output = extract_json(output)
  case parse_json_output(json_output) {
    Ok(data) -> {
      let has_success = data |> dynamic.field("success", dynamic.bool) |> result.is_ok()
      let has_action = data |> dynamic.field("action", dynamic.string) |> result.is_ok()
      has_success && has_action
    }
    Error(_) -> False
  }
}

pub fn e2e_validate_valid_spec_test() {
  get_exit_code(["validate", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_validate_no_args_test() {
  get_exit_code(["validate"]) |> should.equal(exit_error)
}

pub fn e2e_validate_json_output_test() {
  get_stdout(["validate", examples_dir <> "/user-api.cue"]) |> is_valid_json_response() |> should.be_true()
}

pub fn e2e_lint_valid_spec_test() {
  get_exit_code(["lint", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_quality_valid_spec_test() {
  get_exit_code(["quality", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_quality_json_output_test() {
  get_stdout(["quality", examples_dir <> "/user-api.cue", "--json=true"]) |> is_valid_json_response() |> should.be_true()
}

pub fn e2e_coverage_valid_spec_test() {
  get_exit_code(["coverage", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_gaps_valid_spec_test() {
  get_exit_code(["gaps", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_invert_valid_spec_test() {
  get_exit_code(["invert", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_effects_valid_spec_test() {
  get_exit_code(["effects", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_show_valid_spec_test() {
  get_exit_code(["show", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_show_missing_file_test() {
  get_exit_code(["show", "nonexistent.cue"]) |> should.equal(exit_error)
}

pub fn e2e_doctor_valid_spec_test() {
  get_exit_code(["doctor", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_analyze_valid_spec_test() {
  get_exit_code(["analyze", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_improve_valid_spec_test() {
  get_exit_code(["improve", examples_dir <> "/user-api.cue"]) |> should.equal(exit_pass)
}

pub fn e2e_help_test() {
  get_exit_code(["help"]) |> should.equal(exit_pass)
}

pub fn e2e_sessions_test() {
  get_exit_code(["sessions"]) |> should.equal(exit_pass)
}

pub fn e2e_all_kirk_commands_success_test() {
  let commands = ["quality", "coverage", "gaps", "invert", "effects"]
  let spec = examples_dir <> "/user-api.cue"
  let results = list.map(commands, fn(cmd) { get_exit_code([cmd, spec]) })
  list.all(results, fn(code) { code == exit_pass }) |> should.be_true()
}
