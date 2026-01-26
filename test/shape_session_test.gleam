//// Comprehensive tests for intent/shape_session.gleam
//// Tests Shape session management following Functional Core pattern
////
//// Design by Contract:
//// - Preconditions: Valid session IDs, timestamps, and question IDs
//// - Postconditions: Session state transitions are pure and immutable
//// - Invariants: Sessions never mutate, only return new instances

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import intent/planning_types.{FeatureShape, MVPSlice, ShapeSection}
import intent/shape_session.{
  type ShapeAnswer, type ShapeConflict, type ShapeGap, type ShapeSession,
  type ShapeStatus, Complete, InProgress, ReadyForCritique, ShapeAnswer,
  ShapeConflict, ShapeGap, ShapeSession, build_shape_section, create_session,
  find_answer, get_answered_count, record_answer,
}

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Session Creation Tests
// =============================================================================

pub fn create_session_test() {
  let session = create_session("session-001", "api", "2026-01-25T10:00:00Z")

  session.id
  |> should.equal("session-001")

  session.profile
  |> should.equal("api")

  session.created_at
  |> should.equal("2026-01-25T10:00:00Z")

  session.updated_at
  |> should.equal("2026-01-25T10:00:00Z")

  session.status
  |> should.equal(InProgress)

  session.answers
  |> should.equal([])

  session.gaps
  |> should.equal([])

  session.conflicts
  |> should.equal([])
}

pub fn create_session_initializes_empty_lists_test() {
  let session = create_session("session-empty", "cli", "2026-01-25T10:00:00Z")

  list.length(session.answers)
  |> should.equal(0)

  list.length(session.gaps)
  |> should.equal(0)

  list.length(session.conflicts)
  |> should.equal(0)
}

// =============================================================================
// Answer Recording Tests
// =============================================================================

pub fn record_answer_test() {
  let session = create_session("session-002", "api", "2026-01-25T10:00:00Z")

  let extracted = dict.from_list([#("features", "Auth, Payments")])

  let updated =
    record_answer(
      session,
      "q1",
      "We need auth and payments",
      extracted,
      "2026-01-25T10:05:00Z",
    )

  list.length(updated.answers)
  |> should.equal(1)

  updated.updated_at
  |> should.equal("2026-01-25T10:05:00Z")
}

pub fn record_answer_preserves_immutability_test() {
  let original = create_session("session-003", "api", "2026-01-25T10:00:00Z")

  let extracted = dict.from_list([#("key", "value")])

  let _updated =
    record_answer(original, "q1", "answer", extracted, "2026-01-25T10:05:00Z")

  // Original session should be unchanged
  list.length(original.answers)
  |> should.equal(0)
}

pub fn record_multiple_answers_test() {
  let session = create_session("session-004", "api", "2026-01-25T10:00:00Z")

  let extracted1 = dict.from_list([#("features", "Feature 1")])
  let extracted2 = dict.from_list([#("critical_path", "Step 1, Step 2")])

  let session =
    record_answer(
      session,
      "q1",
      "First answer",
      extracted1,
      "2026-01-25T10:05:00Z",
    )

  let session =
    record_answer(
      session,
      "q2",
      "Second answer",
      extracted2,
      "2026-01-25T10:10:00Z",
    )

  list.length(session.answers)
  |> should.equal(2)

  session.updated_at
  |> should.equal("2026-01-25T10:10:00Z")
}

// =============================================================================
// Find Answer Tests
// =============================================================================

pub fn find_answer_found_test() {
  let session = create_session("session-005", "api", "2026-01-25T10:00:00Z")

  let extracted = dict.from_list([#("features", "Feature A")])

  let session =
    record_answer(
      session,
      "q_features",
      "Feature A is needed",
      extracted,
      "2026-01-25T10:05:00Z",
    )

  let result = find_answer(session, "q_features")

  case result {
    Some(answer) -> {
      answer.question_id
      |> should.equal("q_features")

      answer.response
      |> should.equal("Feature A is needed")
    }
    None -> panic as "Expected Some(answer)"
  }
}

pub fn find_answer_not_found_test() {
  let session = create_session("session-006", "api", "2026-01-25T10:00:00Z")

  let result = find_answer(session, "q_missing")

  result
  |> should.equal(None)
}

pub fn find_answer_multiple_answers_test() {
  let session = create_session("session-007", "api", "2026-01-25T10:00:00Z")

  let extracted1 = dict.from_list([#("features", "Feature 1")])
  let extracted2 = dict.from_list([#("critical_path", "Path")])

  let session =
    record_answer(session, "q1", "Answer 1", extracted1, "2026-01-25T10:05:00Z")

  let session =
    record_answer(session, "q2", "Answer 2", extracted2, "2026-01-25T10:10:00Z")

  let result = find_answer(session, "q2")

  case result {
    Some(answer) -> {
      answer.question_id
      |> should.equal("q2")
    }
    None -> panic as "Expected Some(answer)"
  }
}

// =============================================================================
// Get Answered Count Tests
// =============================================================================

pub fn get_answered_count_empty_test() {
  let session = create_session("session-008", "api", "2026-01-25T10:00:00Z")

  get_answered_count(session)
  |> should.equal(0)
}

pub fn get_answered_count_with_answers_test() {
  let session = create_session("session-009", "api", "2026-01-25T10:00:00Z")

  let extracted1 = dict.from_list([#("key1", "val1")])
  let extracted2 = dict.from_list([#("key2", "val2")])
  let extracted3 = dict.from_list([#("key3", "val3")])

  let session =
    record_answer(session, "q1", "a1", extracted1, "2026-01-25T10:05:00Z")

  let session =
    record_answer(session, "q2", "a2", extracted2, "2026-01-25T10:10:00Z")

  let session =
    record_answer(session, "q3", "a3", extracted3, "2026-01-25T10:15:00Z")

  get_answered_count(session)
  |> should.equal(3)
}

// =============================================================================
// Build Shape Section Tests
// =============================================================================

pub fn build_shape_section_success_test() {
  let session = create_session("session-010", "api", "2026-01-25T10:00:00Z")

  // Add all required answers
  let session =
    record_answer(
      session,
      "q_features",
      "Auth, Payments",
      dict.from_list([#("features", "Auth, Payments")]),
      "2026-01-25T10:05:00Z",
    )

  let session =
    record_answer(
      session,
      "q_critical_path",
      "Design, Implement, Test",
      dict.from_list([#("critical_path", "Design, Implement, Test")]),
      "2026-01-25T10:10:00Z",
    )

  let session =
    record_answer(
      session,
      "q_mvp_description",
      "Minimal auth system",
      dict.from_list([#("mvp_description", "Minimal auth system")]),
      "2026-01-25T10:15:00Z",
    )

  let session =
    record_answer(
      session,
      "q_shortcuts",
      "Hardcode admin, Skip MFA",
      dict.from_list([#("shortcuts", "Hardcode admin, Skip MFA")]),
      "2026-01-25T10:20:00Z",
    )

  let session =
    record_answer(
      session,
      "q_post_mvp",
      "OAuth, SSO",
      dict.from_list([#("post_mvp", "OAuth, SSO")]),
      "2026-01-25T10:25:00Z",
    )

  let session =
    record_answer(
      session,
      "q_validation_moment",
      "User logs in successfully with hardcoded credentials",
      dict.from_list([
        #(
          "validation_moment",
          "User logs in successfully with hardcoded credentials",
        ),
      ]),
      "2026-01-25T10:30:00Z",
    )

  let result = build_shape_section(session)

  result
  |> should.be_ok()

  case result {
    Ok(shape) -> {
      list.length(shape.features)
      |> should.equal(2)

      list.length(shape.critical_path)
      |> should.equal(3)

      shape.mvp_slice.description
      |> should.equal("Minimal auth system")

      list.length(shape.mvp_slice.shortcuts)
      |> should.equal(2)

      list.length(shape.post_mvp)
      |> should.equal(2)

      shape.validation_moment
      |> should.equal("User logs in successfully with hardcoded credentials")
    }
    Error(_) -> panic as "Expected Ok"
  }
}

pub fn build_shape_section_missing_features_test() {
  let session = create_session("session-011", "api", "2026-01-25T10:00:00Z")

  // Add only some required fields (missing features)
  let session =
    record_answer(
      session,
      "q_critical_path",
      "Step 1",
      dict.from_list([#("critical_path", "Step 1")]),
      "2026-01-25T10:05:00Z",
    )

  let result = build_shape_section(session)

  result
  |> should.be_error()

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing required field: features")
    }
    Ok(_) -> panic as "Expected Error"
  }
}

pub fn build_shape_section_missing_critical_path_test() {
  let session = create_session("session-012", "api", "2026-01-25T10:00:00Z")

  let session =
    record_answer(
      session,
      "q_features",
      "Feature 1",
      dict.from_list([#("features", "Feature 1")]),
      "2026-01-25T10:05:00Z",
    )

  let result = build_shape_section(session)

  result
  |> should.be_error()

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing required field: critical_path")
    }
    Ok(_) -> panic as "Expected Error"
  }
}

pub fn build_shape_section_missing_mvp_description_test() {
  let session = create_session("session-013", "api", "2026-01-25T10:00:00Z")

  let session =
    record_answer(
      session,
      "q_features",
      "Feature 1",
      dict.from_list([#("features", "Feature 1")]),
      "2026-01-25T10:05:00Z",
    )

  let session =
    record_answer(
      session,
      "q_critical_path",
      "Step 1",
      dict.from_list([#("critical_path", "Step 1")]),
      "2026-01-25T10:10:00Z",
    )

  let result = build_shape_section(session)

  result
  |> should.be_error()

  case result {
    Error(msg) -> {
      msg
      |> should.equal("Missing required field: mvp_description")
    }
    Ok(_) -> panic as "Expected Error"
  }
}

pub fn build_shape_section_parse_lists_test() {
  let session = create_session("session-014", "api", "2026-01-25T10:00:00Z")

  // Test newline-separated lists
  let session =
    record_answer(
      session,
      "q_features",
      "Features",
      dict.from_list([#("features", "Feature A\nFeature B\nFeature C")]),
      "2026-01-25T10:05:00Z",
    )

  let session =
    record_answer(
      session,
      "q_critical_path",
      "Path",
      dict.from_list([#("critical_path", "Step 1\nStep 2")]),
      "2026-01-25T10:10:00Z",
    )

  let session =
    record_answer(
      session,
      "q_mvp_description",
      "MVP",
      dict.from_list([#("mvp_description", "MVP description")]),
      "2026-01-25T10:15:00Z",
    )

  let session =
    record_answer(
      session,
      "q_shortcuts",
      "Shortcuts",
      dict.from_list([#("shortcuts", "Shortcut 1\nShortcut 2")]),
      "2026-01-25T10:20:00Z",
    )

  let session =
    record_answer(
      session,
      "q_post_mvp",
      "Post",
      dict.from_list([#("post_mvp", "Item 1\nItem 2\nItem 3")]),
      "2026-01-25T10:25:00Z",
    )

  let session =
    record_answer(
      session,
      "q_validation_moment",
      "Validation",
      dict.from_list([#("validation_moment", "Validation moment")]),
      "2026-01-25T10:30:00Z",
    )

  let result = build_shape_section(session)

  case result {
    Ok(shape) -> {
      list.length(shape.features)
      |> should.equal(3)

      list.length(shape.critical_path)
      |> should.equal(2)

      list.length(shape.post_mvp)
      |> should.equal(3)
    }
    Error(_) -> panic as "Expected Ok"
  }
}

pub fn build_shape_section_empty_strings_filtered_test() {
  let session = create_session("session-015", "api", "2026-01-25T10:00:00Z")

  // Test that empty strings are filtered out
  let session =
    record_answer(
      session,
      "q_features",
      "Features",
      dict.from_list([
        #("features", "Feature A, , Feature B,  ,Feature C"),
      ]),
      "2026-01-25T10:05:00Z",
    )

  let session =
    record_answer(
      session,
      "q_critical_path",
      "Path",
      dict.from_list([#("critical_path", "Step 1")]),
      "2026-01-25T10:10:00Z",
    )

  let session =
    record_answer(
      session,
      "q_mvp_description",
      "MVP",
      dict.from_list([#("mvp_description", "MVP")]),
      "2026-01-25T10:15:00Z",
    )

  let session =
    record_answer(
      session,
      "q_shortcuts",
      "Shortcuts",
      dict.from_list([#("shortcuts", "S1")]),
      "2026-01-25T10:20:00Z",
    )

  let session =
    record_answer(
      session,
      "q_post_mvp",
      "Post",
      dict.from_list([#("post_mvp", "P1")]),
      "2026-01-25T10:25:00Z",
    )

  let session =
    record_answer(
      session,
      "q_validation_moment",
      "Val",
      dict.from_list([#("validation_moment", "Val")]),
      "2026-01-25T10:30:00Z",
    )

  let result = build_shape_section(session)

  case result {
    Ok(shape) -> {
      // Should have 3 features (empty strings filtered out)
      list.length(shape.features)
      |> should.equal(3)
    }
    Error(_) -> panic as "Expected Ok"
  }
}

// =============================================================================
// ShapeAnswer Type Tests
// =============================================================================

pub fn shape_answer_construction_test() {
  let extracted = dict.from_list([#("field", "value")])

  let answer =
    ShapeAnswer(
      question_id: "q1",
      response: "Response text",
      extracted: extracted,
      timestamp: "2026-01-25T10:00:00Z",
    )

  answer.question_id
  |> should.equal("q1")

  answer.response
  |> should.equal("Response text")

  answer.timestamp
  |> should.equal("2026-01-25T10:00:00Z")
}

// =============================================================================
// ShapeGap Type Tests
// =============================================================================

pub fn shape_gap_construction_test() {
  let gap =
    ShapeGap(
      field: "critical_path",
      description: "Missing critical path steps",
      blocking: True,
    )

  gap.field
  |> should.equal("critical_path")

  gap.blocking
  |> should.be_true()
}

pub fn shape_gap_non_blocking_test() {
  let gap =
    ShapeGap(
      field: "post_mvp",
      description: "Could add more post-MVP items",
      blocking: False,
    )

  gap.blocking
  |> should.be_false()
}

// =============================================================================
// ShapeConflict Type Tests
// =============================================================================

pub fn shape_conflict_construction_test() {
  let conflict =
    ShapeConflict(
      between: #("q1", "q2"),
      description: "Conflicting feature priorities",
    )

  conflict.between
  |> should.equal(#("q1", "q2"))

  conflict.description
  |> should.equal("Conflicting feature priorities")
}

// =============================================================================
// ShapeStatus Type Tests
// =============================================================================

pub fn shape_status_in_progress_test() {
  let status = InProgress

  case status {
    InProgress -> True
  }
  |> should.be_true()
}

pub fn shape_status_ready_for_critique_test() {
  let status = ReadyForCritique

  case status {
    ReadyForCritique -> True
  }
  |> should.be_true()
}

pub fn shape_status_complete_test() {
  let status = Complete

  case status {
    Complete -> True
  }
  |> should.be_true()
}
