/// Vision Commands CLI Tests
/// Tests for the Vision phase state management module
import gleam/dict
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import intent/vision_commands.{
  CommandComplete, CommandInProgress, CommandReadyForCritique,
  create_command_session, get_status_string, record_response, session_to_json,
  set_status_complete, set_status_ready_for_critique,
}

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Vision Command Session Tests
// =============================================================================

pub fn create_session_test() {
  let session =
    create_command_session("test-vision-001", "api", "2026-01-25T14:00:00Z")

  session.id |> should.equal("test-vision-001")
  session.profile |> should.equal("api")
  session.status |> should.equal(CommandInProgress)
  session.critique_score |> should.equal(0)
  session.responses |> should.equal(dict.new())
  session.issues_addressed |> should.equal(0)
}

pub fn session_status_strings_test() {
  get_status_string(CommandInProgress) |> should.equal("in_progress")
  get_status_string(CommandReadyForCritique)
  |> should.equal("ready_for_critique")
  get_status_string(CommandComplete) |> should.equal("complete")
}

pub fn record_response_test() {
  let session =
    create_command_session("test-vision-002", "api", "2026-01-25T14:00:00Z")

  let updated =
    record_response(
      session,
      "issue-001",
      "We have clarified the persona",
      "2026-01-25T14:05:00Z",
    )

  dict.size(updated.responses) |> should.equal(1)
  dict.has_key(updated.responses, "issue-001") |> should.be_true
}

pub fn set_status_ready_for_critique_test() {
  let session =
    create_command_session("test-vision-003", "api", "2026-01-25T14:00:00Z")

  let updated = set_status_ready_for_critique(session, "2026-01-25T14:30:00Z")

  updated.status |> should.equal(CommandReadyForCritique)
  updated.updated_at |> should.equal("2026-01-25T14:30:00Z")
}

pub fn set_status_complete_test() {
  let session =
    create_command_session("test-vision-004", "api", "2026-01-25T14:00:00Z")
    |> set_status_ready_for_critique("2026-01-25T14:30:00Z")

  let updated = set_status_complete(session, 85, 3, "2026-01-25T15:00:00Z")

  updated.status |> should.equal(CommandComplete)
  updated.critique_score |> should.equal(85)
  updated.issues_addressed |> should.equal(3)
}

// =============================================================================
// Vision Command Session Agree Tests
// =============================================================================

pub fn agree_session_passes_when_complete_test() {
  let session =
    create_command_session("test-vision-005", "api", "2026-01-25T14:00:00Z")
    |> set_status_ready_for_critique("2026-01-25T14:30:00Z")
    |> set_status_complete(85, 0, "2026-01-25T15:00:00Z")

  let result =
    vision_commands.agree_session(
      session,
      "Vision approved",
      "2026-01-25T15:30:00Z",
    )

  result |> should.be_ok
  let agreed = case result {
    Ok(s) -> s
    Error(_) -> session
  }
  agreed.finalized_at |> should.equal(Some("2026-01-25T15:30:00Z"))
}

pub fn agree_session_fails_when_not_complete_test() {
  let session =
    create_command_session("test-vision-006", "api", "2026-01-25T14:00:00Z")

  let result =
    vision_commands.agree_session(
      session,
      "Vision approved",
      "2026-01-25T15:30:00Z",
    )

  result |> should.be_error
}

pub fn agree_session_fails_with_low_score_test() {
  let session =
    create_command_session("test-vision-007", "api", "2026-01-25T14:00:00Z")
    |> set_status_ready_for_critique("2026-01-25T14:30:00Z")
    |> set_status_complete(50, 0, "2026-01-25T15:00:00Z")

  let result =
    vision_commands.agree_session(
      session,
      "Vision approved",
      "2026-01-25T15:30:00Z",
    )

  result |> should.be_error
}

// =============================================================================
// Vision Critique Integration Tests
// =============================================================================

pub fn empty_vision_has_issues_test() {
  let vision = vision_commands.empty_vision_section()
  let result = vision_commands.run_critique(vision)
  result.passed |> should.be_false
}

pub fn complete_vision_has_better_score_test() {
  let vision = vision_commands.sample_complete_vision()
  let result = vision_commands.run_critique(vision)
  { result.score > 0 } |> should.be_true
}
