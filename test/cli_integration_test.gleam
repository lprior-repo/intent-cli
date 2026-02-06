import gleam/list
import gleeunit
import gleeunit/should
import intent

pub fn main() {
  gleeunit.main()
}

pub fn given_bare_bool_flag_when_normalizing_then_true_is_assumed_test() {
  let args = ["check", "examples/user-api.cue", "--json"]

  intent.normalize_cli_args(args)
  |> should.equal(["check", "examples/user-api.cue", "--json=true"])
}

pub fn given_bool_literal_value_when_normalizing_then_equals_syntax_is_used_test() {
  let args = ["sessions", "--json", "false"]

  intent.normalize_cli_args(args)
  |> should.equal(["sessions", "--json=false"])
}

pub fn given_non_bool_flag_when_normalizing_then_arguments_are_preserved_test() {
  let args = [
    "check", "examples/user-api.cue", "--feature", "Users", "--only", "login",
  ]

  intent.normalize_cli_args(args)
  |> should.equal([
    "check", "examples/user-api.cue", "--feature=Users", "--only=login",
  ])
}

pub fn given_mixed_flags_when_normalizing_then_only_known_bool_flags_change_test() {
  let args = [
    "plan-approve", "session-123", "--yes", "--notes", "ship it", "--json",
  ]

  intent.normalize_cli_args(args)
  |> should.equal([
    "plan-approve", "session-123", "--yes=true", "--notes=ship it",
    "--json=true",
  ])
}

pub fn given_draft_bool_flag_when_normalizing_then_true_is_assumed_test() {
  let args = ["validate-bead", "sample.cue", "--draft"]

  intent.normalize_cli_args(args)
  |> should.equal(["validate-bead", "sample.cue", "--draft=true"])
}

pub fn given_value_flag_missing_value_when_normalizing_then_flag_is_preserved_test() {
  let args = ["check", "examples/user-api.cue", "--target", "--json"]

  intent.normalize_cli_args(args)
  |> should.equal(["check", "examples/user-api.cue", "--target", "--json=true"])
}

pub fn given_export_template_value_flag_when_normalizing_then_equals_syntax_is_used_test() {
  let args = [
    "interview", "--profile", "api", "--export-answers-template",
    "/tmp/answers.json",
  ]

  intent.normalize_cli_args(args)
  |> should.equal([
    "interview", "--profile=api", "--export-answers-template=/tmp/answers.json",
  ])
}

pub fn given_interview_session_and_answer_flags_when_normalizing_then_values_are_encoded_test() {
  let args = [
    "interview", "--session", "interview-123", "--answer",
    "THE SYSTEM SHALL authenticate users",
  ]

  intent.normalize_cli_args(args)
  |> should.equal([
    "interview", "--session=interview-123",
    "--answer=THE SYSTEM SHALL authenticate users",
  ])
}

pub fn command_inventory_count_is_current_test() {
  let commands = [
    "analyze", "bead-status", "beads", "beads-regenerate", "check", "compact",
    "coverage", "diff", "ears", "effects", "export", "gaps", "history",
    "improve", "interview", "invert", "lint", "plan", "plan-approve",
    "prototext", "quality", "sessions", "show", "validate", "validate-bead",
  ]

  commands
  |> list.length
  |> should.equal(25)
}
