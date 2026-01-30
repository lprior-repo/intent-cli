/// Integration test demonstrating export validation (intent-cli-pn1w)
/// This test validates that corrupted session data causes export to fail
import gleam/dict
import gleeunit
import gleeunit/should
import intent/interview.{Answer, Complete, InterviewSession}
import intent/question_types.{User}
import intent/session_validation

pub fn main() {
  gleeunit.main()
}

/// Test that export validation rejects session with TODO placeholder
pub fn export_rejects_todo_placeholder_test() {
  let corrupted_answer =
    Answer(
      question_id: "q1",
      question_text: "What is the API endpoint?",
      perspective: User,
      round: 1,
      response: "TODO: define the endpoint path",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let session =
    InterviewSession(
      id: "corrupted-session",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T01:00:00Z",
      completed_at: "2024-01-01T02:00:00Z",
      stage: Complete,
      rounds_completed: 1,
      answers: [corrupted_answer],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  // This should fail validation
  case session_validation.validate_session_for_export(session) {
    Error(_errors) -> {
      // Expected - corruption detected
      should.be_true(True)
    }
    Ok(_) -> {
      // Should NOT succeed with corrupted data
      should.fail()
    }
  }
}

/// Test that export validation accepts valid session
pub fn export_accepts_valid_session_test() {
  let valid_answer =
    Answer(
      question_id: "q1",
      question_text: "What is the API endpoint?",
      perspective: User,
      round: 1,
      response: "GET /api/users - returns a list of all users in the system",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let session =
    InterviewSession(
      id: "valid-session",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T01:00:00Z",
      completed_at: "2024-01-01T02:00:00Z",
      stage: Complete,
      rounds_completed: 1,
      answers: [valid_answer],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  // This should pass validation
  case session_validation.validate_session_for_export(session) {
    Ok(validated) -> {
      // Expected - valid session passes
      validated.id
      |> should.equal("valid-session")
    }
    Error(_) -> {
      // Should NOT fail with valid data
      should.fail()
    }
  }
}
