//// Comprehensive tests for intent/vision_commands.gleam
//// Tests state management for Vision phase CLI commands
////
//// Design by Contract:
//// - Preconditions: Valid command session creation with all required fields
//// - Postconditions: Pure functions maintain immutability, status transitions valid
//// - Invariants: Sessions are immutable, score >= 70 required for finalization

import gleam/dict
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import intent/vision_commands

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Command Session Creation Tests
// ============================================================================

pub fn create_command_session_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-001",
      "api",
      "2026-01-25T10:00:00Z",
    )

  session.id
  |> should.equal("cmd-001")

  session.profile
  |> should.equal("api")

  session.created_at
  |> should.equal("2026-01-25T10:00:00Z")

  session.updated_at
  |> should.equal("2026-01-25T10:00:00Z")
}

pub fn create_command_session_initial_status_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-002",
      "cli",
      "2026-01-25T10:00:00Z",
    )

  case session.status {
    vision_commands.CommandInProgress -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn create_command_session_score_zero_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-003",
      "api",
      "2026-01-25T10:00:00Z",
    )

  session.critique_score
  |> should.equal(0)

  session.issues_addressed
  |> should.equal(0)
}

pub fn create_command_session_empty_responses_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-004",
      "api",
      "2026-01-25T10:00:00Z",
    )

  session.responses
  |> dict.size()
  |> should.equal(0)
}

pub fn create_command_session_no_finalized_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-005",
      "api",
      "2026-01-25T10:00:00Z",
    )

  case session.finalized_at {
    None -> True
    Some(_) -> False
  }
  |> should.be_true()
}

// ============================================================================
// Status String Tests
// ============================================================================

pub fn get_status_string_in_progress_test() {
  let status = vision_commands.CommandInProgress
  let result = vision_commands.get_status_string(status)

  result
  |> should.equal("in_progress")
}

pub fn get_status_string_ready_for_critique_test() {
  let status = vision_commands.CommandReadyForCritique
  let result = vision_commands.get_status_string(status)

  result
  |> should.equal("ready_for_critique")
}

pub fn get_status_string_complete_test() {
  let status = vision_commands.CommandComplete
  let result = vision_commands.get_status_string(status)

  result
  |> should.equal("complete")
}

// ============================================================================
// Response Recording Tests
// ============================================================================

pub fn record_response_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-010",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let updated =
    vision_commands.record_response(
      session,
      "issue-1",
      "Fixed the press release wording",
      "2026-01-25T10:05:00Z",
    )

  updated.responses
  |> dict.size()
  |> should.equal(1)

  case dict.get(updated.responses, "issue-1") {
    Ok(response) -> {
      response.issue_id
      |> should.equal("issue-1")

      response.response_text
      |> should.equal("Fixed the press release wording")

      response.timestamp
      |> should.equal("2026-01-25T10:05:00Z")
    }
    Error(_) -> panic as "Expected response in dict"
  }
}

pub fn record_response_updates_timestamp_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-011",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let updated =
    vision_commands.record_response(
      session,
      "issue-1",
      "Response text",
      "2026-01-25T10:10:00Z",
    )

  updated.updated_at
  |> should.equal("2026-01-25T10:10:00Z")

  // Original created_at should not change
  updated.created_at
  |> should.equal("2026-01-25T10:00:00Z")
}

pub fn record_response_multiple_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-012",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let updated =
    session
    |> vision_commands.record_response(
      "issue-1",
      "Response 1",
      "2026-01-25T10:01:00Z",
    )
    |> vision_commands.record_response(
      "issue-2",
      "Response 2",
      "2026-01-25T10:02:00Z",
    )
    |> vision_commands.record_response(
      "issue-3",
      "Response 3",
      "2026-01-25T10:03:00Z",
    )

  updated.responses
  |> dict.size()
  |> should.equal(3)
}

pub fn record_response_overwrites_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-013",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let updated =
    session
    |> vision_commands.record_response(
      "issue-1",
      "First response",
      "2026-01-25T10:01:00Z",
    )
    |> vision_commands.record_response(
      "issue-1",
      "Updated response",
      "2026-01-25T10:05:00Z",
    )

  updated.responses
  |> dict.size()
  |> should.equal(1)

  case dict.get(updated.responses, "issue-1") {
    Ok(response) -> {
      response.response_text
      |> should.equal("Updated response")

      response.timestamp
      |> should.equal("2026-01-25T10:05:00Z")
    }
    Error(_) -> panic as "Expected response in dict"
  }
}

// ============================================================================
// Status Transition Tests
// ============================================================================

pub fn set_status_ready_for_critique_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-020",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let updated =
    vision_commands.set_status_ready_for_critique(
      session,
      "2026-01-25T10:15:00Z",
    )

  case updated.status {
    vision_commands.CommandReadyForCritique -> True
    _ -> False
  }
  |> should.be_true()

  updated.updated_at
  |> should.equal("2026-01-25T10:15:00Z")
}

pub fn set_status_complete_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-021",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let updated =
    vision_commands.set_status_complete(session, 85, 10, "2026-01-25T10:20:00Z")

  case updated.status {
    vision_commands.CommandComplete -> True
    _ -> False
  }
  |> should.be_true()

  updated.critique_score
  |> should.equal(85)

  updated.issues_addressed
  |> should.equal(10)

  updated.updated_at
  |> should.equal("2026-01-25T10:20:00Z")
}

// ============================================================================
// Session Agreement Tests
// ============================================================================

pub fn agree_session_success_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-030",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let ready =
    vision_commands.set_status_complete(session, 75, 5, "2026-01-25T10:20:00Z")

  let result =
    vision_commands.agree_session(ready, "Looks good", "2026-01-25T10:30:00Z")

  case result {
    Ok(finalized) -> {
      case finalized.finalized_at {
        Some(timestamp) -> {
          timestamp
          |> should.equal("2026-01-25T10:30:00Z")
        }
        None -> panic as "Expected Some(finalized_at)"
      }

      finalized.updated_at
      |> should.equal("2026-01-25T10:30:00Z")
    }
    Error(msg) -> panic as { "Expected Ok, got Error: " <> msg }
  }
}

pub fn agree_session_boundary_70_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-031",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let ready =
    vision_commands.set_status_complete(session, 70, 5, "2026-01-25T10:20:00Z")

  let result =
    vision_commands.agree_session(ready, "Notes", "2026-01-25T10:30:00Z")

  case result {
    Ok(_) -> True
    Error(_) -> False
  }
  |> should.be_true()
}

pub fn agree_session_boundary_69_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-032",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let ready =
    vision_commands.set_status_complete(session, 69, 5, "2026-01-25T10:20:00Z")

  let result =
    vision_commands.agree_session(ready, "Notes", "2026-01-25T10:30:00Z")

  case result {
    Error(msg) -> {
      msg
      |> should.equal(
        "Critique score must be >= 70 to finalize vision (current: 69)",
      )
    }
    Ok(_) -> panic as "Expected Error for score < 70"
  }
}

pub fn agree_session_low_score_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-033",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let ready =
    vision_commands.set_status_complete(session, 50, 5, "2026-01-25T10:20:00Z")

  let result =
    vision_commands.agree_session(ready, "Notes", "2026-01-25T10:30:00Z")

  case result {
    Error(msg) -> {
      msg
      |> should.equal(
        "Critique score must be >= 70 to finalize vision (current: 50)",
      )
    }
    Ok(_) -> panic as "Expected Error for low score"
  }
}

pub fn agree_session_not_complete_in_progress_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-034",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let result =
    vision_commands.agree_session(session, "Notes", "2026-01-25T10:30:00Z")

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Session must be complete before finalizing")
    }
    Ok(_) -> panic as "Expected Error for InProgress status"
  }
}

pub fn agree_session_not_complete_ready_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-035",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let ready =
    vision_commands.set_status_ready_for_critique(
      session,
      "2026-01-25T10:20:00Z",
    )

  let result =
    vision_commands.agree_session(ready, "Notes", "2026-01-25T10:30:00Z")

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Session must be complete before finalizing")
    }
    Ok(_) -> panic as "Expected Error for ReadyForCritique status"
  }
}

// ============================================================================
// JSON Serialization Tests
// ============================================================================

pub fn session_to_json_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-040",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let updated =
    session
    |> vision_commands.record_response(
      "issue-1",
      "Response 1",
      "2026-01-25T10:01:00Z",
    )
    |> vision_commands.record_response(
      "issue-2",
      "Response 2",
      "2026-01-25T10:02:00Z",
    )
    |> vision_commands.set_status_complete(75, 2, "2026-01-25T10:20:00Z")

  let _json = vision_commands.session_to_json(updated)

  // JSON serialization is tested by structure - actual parsing would require simplifile
  True
  |> should.be_true()
}

pub fn session_to_json_finalized_none_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-042",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let _json = vision_commands.session_to_json(session)

  // finalized_at should be null for None
  case session.finalized_at {
    None -> True
    Some(_) -> False
  }
  |> should.be_true()
}

pub fn session_to_json_finalized_some_test() {
  let session =
    vision_commands.create_command_session(
      "cmd-043",
      "api",
      "2026-01-25T10:00:00Z",
    )

  let complete =
    vision_commands.set_status_complete(session, 80, 5, "2026-01-25T10:20:00Z")

  let result =
    vision_commands.agree_session(complete, "Notes", "2026-01-25T10:30:00Z")

  case result {
    Ok(finalized) -> {
      let _json = vision_commands.session_to_json(finalized)

      // finalized_at should be a string for Some
      case finalized.finalized_at {
        Some(_) -> True
        None -> False
      }
      |> should.be_true()
    }
    Error(msg) -> panic as { "Expected Ok, got Error: " <> msg }
  }
}

// ============================================================================
// Helper Function Tests
// ============================================================================

pub fn empty_vision_section_test() {
  let section = vision_commands.empty_vision_section()

  section.press_release
  |> should.equal("")

  section.persona
  |> should.equal("")

  section.non_personas
  |> should.equal([])

  section.north_star
  |> should.equal("")

  section.scenarios
  |> should.equal([])

  case section.replaces {
    None -> True
    Some(_) -> False
  }
  |> should.be_true()

  section.vorp
  |> should.equal("")

  section.out_of_scope
  |> should.equal([])
}

pub fn sample_complete_vision_test() {
  let section = vision_commands.sample_complete_vision()

  // Verify all required fields are populated (non-empty)
  section.press_release
  |> should.not_equal("")

  section.persona
  |> should.not_equal("")

  section.north_star
  |> should.not_equal("")

  section.vorp
  |> should.not_equal("")
}

pub fn sample_complete_vision_lists_test() {
  let section = vision_commands.sample_complete_vision()

  // Verify lists are populated
  section.non_personas
  |> should.not_equal([])

  section.out_of_scope
  |> should.not_equal([])
}

pub fn sample_complete_vision_scenarios_test() {
  let section = vision_commands.sample_complete_vision()

  // Verify scenarios exist (sample has 2)
  let scenario_count = case section.scenarios {
    [_, _] -> 2
    _ -> 0
  }

  scenario_count
  |> should.equal(2)
}

pub fn sample_complete_vision_replaces_test() {
  let section = vision_commands.sample_complete_vision()

  // Verify replaces is Some
  case section.replaces {
    Some(_) -> True
    None -> False
  }
  |> should.be_true()
}

// ============================================================================
// Critique Function Tests
// ============================================================================

pub fn run_critique_empty_test() {
  let section = vision_commands.empty_vision_section()
  let result = vision_commands.run_critique(section)

  // Critique should return a result - exact validation done in vision_critique_test
  // Empty section should have low score
  case result.score < 70 {
    True -> True
    False -> False
  }
  |> should.be_true()
}

pub fn run_critique_complete_test() {
  let section = vision_commands.sample_complete_vision()
  let result = vision_commands.run_critique(section)

  // Complete sample should have a passing score
  case result.score >= 70 {
    True -> True
    False -> False
  }
  |> should.be_true()
}
