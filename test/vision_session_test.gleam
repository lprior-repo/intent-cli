//// Comprehensive tests for intent/vision_session.gleam
//// Tests state management for Vision phase (Phase 1 of INTENT_4_PLAN.md)
////
//// Design by Contract:
//// - Preconditions: Valid session creation with all required fields
//// - Postconditions: Pure functions maintain immutability, state updates correct
//// - Invariants: Sessions are immutable, answers maintain order, status transitions valid

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import intent/vision_session.{InProgress}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Session Creation Tests
// ============================================================================

pub fn create_session_test() {
  let session =
    vision_session.create_session("sess-001", "api", "2026-01-25T10:00:00Z")

  session.id
  |> should.equal("sess-001")

  session.profile
  |> should.equal("api")

  session.created_at
  |> should.equal("2026-01-25T10:00:00Z")

  session.updated_at
  |> should.equal("2026-01-25T10:00:00Z")
}

pub fn create_session_initial_status_test() {
  let session =
    vision_session.create_session("sess-002", "cli", "2026-01-25T10:00:00Z")

  case session.status {
    InProgress -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn create_session_empty_collections_test() {
  let session =
    vision_session.create_session("sess-003", "api", "2026-01-25T10:00:00Z")

  session.answers
  |> should.equal([])

  session.gaps
  |> should.equal([])

  session.conflicts
  |> should.equal([])
}

// ============================================================================
// Answer Recording Tests
// ============================================================================

pub fn record_answer_test() {
  let session =
    vision_session.create_session("sess-010", "api", "2026-01-25T10:00:00Z")

  let extracted = dict.from_list([#("press_release", "Test press release")])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "User response text",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  updated.answers
  |> list.length()
  |> should.equal(1)

  let first_answer = case updated.answers {
    [answer] -> answer
    _ -> panic as "Expected exactly one answer"
  }

  first_answer.question_id
  |> should.equal("q1")

  first_answer.response
  |> should.equal("User response text")
}

pub fn record_answer_updates_timestamp_test() {
  let session =
    vision_session.create_session("sess-011", "api", "2026-01-25T10:00:00Z")

  let extracted = dict.new()

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Response",
      extracted,
      "2026-01-25T10:05:00Z",
    )

  updated.updated_at
  |> should.equal("2026-01-25T10:05:00Z")

  // Original created_at should not change
  updated.created_at
  |> should.equal("2026-01-25T10:00:00Z")
}

pub fn record_answer_multiple_test() {
  let session =
    vision_session.create_session("sess-012", "api", "2026-01-25T10:00:00Z")

  let extracted1 = dict.from_list([#("field1", "value1")])
  let extracted2 = dict.from_list([#("field2", "value2")])

  let updated =
    session
    |> vision_session.record_answer(
      "q1",
      "Response 1",
      extracted1,
      "2026-01-25T10:01:00Z",
    )
    |> vision_session.record_answer(
      "q2",
      "Response 2",
      extracted2,
      "2026-01-25T10:02:00Z",
    )

  updated.answers
  |> list.length()
  |> should.equal(2)
}

pub fn record_answer_preserves_order_test() {
  let session =
    vision_session.create_session("sess-013", "api", "2026-01-25T10:00:00Z")

  let extracted = dict.new()

  let updated =
    session
    |> vision_session.record_answer(
      "q1",
      "First",
      extracted,
      "2026-01-25T10:01:00Z",
    )
    |> vision_session.record_answer(
      "q2",
      "Second",
      extracted,
      "2026-01-25T10:02:00Z",
    )
    |> vision_session.record_answer(
      "q3",
      "Third",
      extracted,
      "2026-01-25T10:03:00Z",
    )

  let ids =
    updated.answers
    |> list.map(fn(a) { a.question_id })

  ids
  |> should.equal(["q1", "q2", "q3"])
}

// ============================================================================
// Answer Lookup Tests
// ============================================================================

pub fn find_answer_existing_test() {
  let session =
    vision_session.create_session("sess-020", "api", "2026-01-25T10:00:00Z")

  let extracted = dict.from_list([#("key", "value")])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Test response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.find_answer(updated, "q1")

  case result {
    Some(answer) -> {
      answer.question_id
      |> should.equal("q1")
      answer.response
      |> should.equal("Test response")
    }
    None -> panic as "Expected Some(answer)"
  }
}

pub fn find_answer_missing_test() {
  let session =
    vision_session.create_session("sess-021", "api", "2026-01-25T10:00:00Z")

  let result = vision_session.find_answer(session, "nonexistent")

  case result {
    None -> True
    Some(_) -> False
  }
  |> should.be_true()
}

pub fn find_answer_multiple_test() {
  let session =
    vision_session.create_session("sess-022", "api", "2026-01-25T10:00:00Z")

  let extracted = dict.new()

  let updated =
    session
    |> vision_session.record_answer(
      "q1",
      "First",
      extracted,
      "2026-01-25T10:01:00Z",
    )
    |> vision_session.record_answer(
      "q2",
      "Second",
      extracted,
      "2026-01-25T10:02:00Z",
    )
    |> vision_session.record_answer(
      "q3",
      "Third",
      extracted,
      "2026-01-25T10:03:00Z",
    )

  let result = vision_session.find_answer(updated, "q2")

  case result {
    Some(answer) -> {
      answer.question_id
      |> should.equal("q2")
      answer.response
      |> should.equal("Second")
    }
    None -> panic as "Expected Some(answer)"
  }
}

// ============================================================================
// Answer Count Tests
// ============================================================================

pub fn get_answered_count_empty_test() {
  let session =
    vision_session.create_session("sess-030", "api", "2026-01-25T10:00:00Z")

  vision_session.get_answered_count(session)
  |> should.equal(0)
}

pub fn get_answered_count_one_test() {
  let session =
    vision_session.create_session("sess-031", "api", "2026-01-25T10:00:00Z")

  let extracted = dict.new()

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  vision_session.get_answered_count(updated)
  |> should.equal(1)
}

pub fn get_answered_count_multiple_test() {
  let session =
    vision_session.create_session("sess-032", "api", "2026-01-25T10:00:00Z")

  let extracted = dict.new()

  let updated =
    session
    |> vision_session.record_answer(
      "q1",
      "R1",
      extracted,
      "2026-01-25T10:01:00Z",
    )
    |> vision_session.record_answer(
      "q2",
      "R2",
      extracted,
      "2026-01-25T10:02:00Z",
    )
    |> vision_session.record_answer(
      "q3",
      "R3",
      extracted,
      "2026-01-25T10:03:00Z",
    )
    |> vision_session.record_answer(
      "q4",
      "R4",
      extracted,
      "2026-01-25T10:04:00Z",
    )

  vision_session.get_answered_count(updated)
  |> should.equal(4)
}

// ============================================================================
// Vision Section Building Tests
// ============================================================================

pub fn build_vision_section_complete_test() {
  let session =
    vision_session.create_session("sess-040", "api", "2026-01-25T10:00:00Z")

  let extracted =
    dict.from_list([
      #("press_release", "Press release text"),
      #("persona", "Backend engineer"),
      #("non_personas", "Frontend dev, DBA"),
      #("north_star", "Fast API validation"),
      #("replaces", "Manual testing"),
      #("vorp", "10x faster"),
      #("scenarios", "User tests API"),
      #("out_of_scope", "UI testing, Load testing"),
    ])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Complete response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.build_vision_section(updated)

  case result {
    Ok(section) -> {
      section.press_release
      |> should.equal("Press release text")

      section.persona
      |> should.equal("Backend engineer")

      section.north_star
      |> should.equal("Fast API validation")

      section.vorp
      |> should.equal("10x faster")

      // Check lists are parsed
      section.non_personas
      |> list.length()
      |> should.equal(2)

      section.out_of_scope
      |> list.length()
      |> should.equal(2)

      // Check optional field
      case section.replaces {
        Some(text) -> text |> should.equal("Manual testing")
        None -> panic as "Expected Some(replaces)"
      }
    }
    Error(msg) -> panic as { "Expected Ok, got Error: " <> msg }
  }
}

pub fn build_vision_section_missing_press_release_test() {
  let session =
    vision_session.create_session("sess-041", "api", "2026-01-25T10:00:00Z")

  let extracted =
    dict.from_list([
      #("persona", "Backend engineer"),
      #("non_personas", "Frontend dev"),
      #("north_star", "Fast validation"),
      #("vorp", "10x faster"),
      #("scenarios", "User tests API"),
      #("out_of_scope", "UI testing"),
    ])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Incomplete response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.build_vision_section(updated)

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing required field: press_release")
    }
    Ok(_) -> panic as "Expected Error for missing press_release"
  }
}

pub fn build_vision_section_missing_persona_test() {
  let session =
    vision_session.create_session("sess-042", "api", "2026-01-25T10:00:00Z")

  let extracted =
    dict.from_list([
      #("press_release", "Press release"),
      #("non_personas", "Frontend dev"),
      #("north_star", "Fast validation"),
      #("vorp", "10x faster"),
      #("scenarios", "User tests API"),
      #("out_of_scope", "UI testing"),
    ])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.build_vision_section(updated)

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing required field: persona")
    }
    Ok(_) -> panic as "Expected Error for missing persona"
  }
}

pub fn build_vision_section_missing_non_personas_test() {
  let session =
    vision_session.create_session("sess-043", "api", "2026-01-25T10:00:00Z")

  let extracted =
    dict.from_list([
      #("press_release", "Press release"),
      #("persona", "Backend engineer"),
      #("north_star", "Fast validation"),
      #("vorp", "10x faster"),
      #("scenarios", "User tests API"),
      #("out_of_scope", "UI testing"),
    ])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.build_vision_section(updated)

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing required field: non_personas")
    }
    Ok(_) -> panic as "Expected Error for missing non_personas"
  }
}

pub fn build_vision_section_missing_north_star_test() {
  let session =
    vision_session.create_session("sess-044", "api", "2026-01-25T10:00:00Z")

  let extracted =
    dict.from_list([
      #("press_release", "Press release"),
      #("persona", "Backend engineer"),
      #("non_personas", "Frontend dev"),
      #("vorp", "10x faster"),
      #("scenarios", "User tests API"),
      #("out_of_scope", "UI testing"),
    ])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.build_vision_section(updated)

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing required field: north_star")
    }
    Ok(_) -> panic as "Expected Error for missing north_star"
  }
}

pub fn build_vision_section_missing_vorp_test() {
  let session =
    vision_session.create_session("sess-045", "api", "2026-01-25T10:00:00Z")

  let extracted =
    dict.from_list([
      #("press_release", "Press release"),
      #("persona", "Backend engineer"),
      #("non_personas", "Frontend dev"),
      #("north_star", "Fast validation"),
      #("scenarios", "User tests API"),
      #("out_of_scope", "UI testing"),
    ])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.build_vision_section(updated)

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing required field: vorp")
    }
    Ok(_) -> panic as "Expected Error for missing vorp"
  }
}

pub fn build_vision_section_missing_scenarios_test() {
  let session =
    vision_session.create_session("sess-046", "api", "2026-01-25T10:00:00Z")

  let extracted =
    dict.from_list([
      #("press_release", "Press release"),
      #("persona", "Backend engineer"),
      #("non_personas", "Frontend dev"),
      #("north_star", "Fast validation"),
      #("vorp", "10x faster"),
      #("out_of_scope", "UI testing"),
    ])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.build_vision_section(updated)

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing required field: scenarios")
    }
    Ok(_) -> panic as "Expected Error for missing scenarios"
  }
}

pub fn build_vision_section_missing_out_of_scope_test() {
  let session =
    vision_session.create_session("sess-047", "api", "2026-01-25T10:00:00Z")

  let extracted =
    dict.from_list([
      #("press_release", "Press release"),
      #("persona", "Backend engineer"),
      #("non_personas", "Frontend dev"),
      #("north_star", "Fast validation"),
      #("vorp", "10x faster"),
      #("scenarios", "User tests API"),
    ])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.build_vision_section(updated)

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing required field: out_of_scope")
    }
    Ok(_) -> panic as "Expected Error for missing out_of_scope"
  }
}

pub fn build_vision_section_optional_replaces_test() {
  let session =
    vision_session.create_session("sess-048", "api", "2026-01-25T10:00:00Z")

  let extracted =
    dict.from_list([
      #("press_release", "Press release"),
      #("persona", "Backend engineer"),
      #("non_personas", "Frontend dev"),
      #("north_star", "Fast validation"),
      #("vorp", "10x faster"),
      #("scenarios", "User tests API"),
      #("out_of_scope", "UI testing"),
      // Note: replaces is NOT included
    ])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.build_vision_section(updated)

  case result {
    Ok(section) -> {
      case section.replaces {
        None -> True
        Some(_) -> False
      }
      |> should.be_true()
    }
    Error(msg) -> panic as { "Expected Ok, got Error: " <> msg }
  }
}

pub fn build_vision_section_list_parsing_test() {
  let session =
    vision_session.create_session("sess-049", "api", "2026-01-25T10:00:00Z")

  // Test comma and newline parsing
  let extracted =
    dict.from_list([
      #("press_release", "Press release"),
      #("persona", "Backend engineer"),
      #("non_personas", "Frontend dev,DBA,QA engineer"),
      #("north_star", "Fast validation"),
      #("vorp", "10x faster"),
      #("scenarios", "User tests API"),
      #("out_of_scope", "UI testing\nLoad testing\nDatabase testing"),
    ])

  let updated =
    vision_session.record_answer(
      session,
      "q1",
      "Response",
      extracted,
      "2026-01-25T10:01:00Z",
    )

  let result = vision_session.build_vision_section(updated)

  case result {
    Ok(section) -> {
      section.non_personas
      |> list.length()
      |> should.equal(3)

      section.out_of_scope
      |> list.length()
      |> should.equal(3)

      section.non_personas
      |> should.equal(["Frontend dev", "DBA", "QA engineer"])

      section.out_of_scope
      |> should.equal(["UI testing", "Load testing", "Database testing"])
    }
    Error(msg) -> panic as { "Expected Ok, got Error: " <> msg }
  }
}
