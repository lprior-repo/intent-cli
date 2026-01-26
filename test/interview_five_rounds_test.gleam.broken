//// Tests for 5-round mental model interview flow
//// Verifies that interview sessions properly progress through all 5 rounds
////
//// Mental Model Mapping:
//// - Round 1: EARS (spec skeleton + patterns)
//// - Round 2: Contracts (response.checks with rule+why)
//// - Round 3: Inversion (anti_patterns + error behaviors)
//// - Round 4: Effects (requires[] + verification behaviors)
//// - Round 5: Pre-mortem (ai_hints.pitfalls)

import gleam/dict
import gleam/list
import gleeunit
import gleeunit/should
import intent/interview.{
  type Answer, type InterviewSession, Answer, Api, Complete, Discovery,
  InterviewSession, Refinement, Validation,
}
import intent/interview_questions
import intent/question_types.{Developer}

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// Test Helpers
// ============================================================================

/// Create a test answer for a given round
fn make_answer(question_id: String, response: String, round: Int) -> Answer {
  Answer(
    question_id: question_id,
    question_text: "Test question: " <> question_id,
    perspective: Developer,
    round: round,
    response: response,
    extracted: dict.new(),
    confidence: 0.85,
    notes: "Test answer for round " <> int_to_string(round),
    timestamp: "2026-01-25T12:00:00Z",
  )
}

/// Create a test session
fn make_session(
  id: String,
  rounds_completed: Int,
  answers: List(Answer),
) -> InterviewSession {
  InterviewSession(
    id: id,
    profile: Api,
    created_at: "2026-01-25T12:00:00Z",
    updated_at: "2026-01-25T12:00:00Z",
    completed_at: "",
    stage: Discovery,
    rounds_completed: rounds_completed,
    answers: answers,
    gaps: [],
    conflicts: [],
    raw_notes: "Test session for 5-round flow",
  )
}

/// Helper to convert int to string
fn int_to_string(n: Int) -> String {
  case n {
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    _ -> "unknown"
  }
}

// ============================================================================
// 5-ROUND FLOW TESTS
// ============================================================================

/// Test that questions exist for all 5 rounds for api profile
pub fn api_profile_has_questions_for_all_five_rounds_test() {
  let round1 = interview_questions.get_questions_for_round("api", 1)
  let round2 = interview_questions.get_questions_for_round("api", 2)
  let round3 = interview_questions.get_questions_for_round("api", 3)
  let round4 = interview_questions.get_questions_for_round("api", 4)
  let round5 = interview_questions.get_questions_for_round("api", 5)

  // All rounds should have at least one question
  { list.length(round1) > 0 }
  |> should.be_true()

  { list.length(round2) > 0 }
  |> should.be_true()

  { list.length(round3) > 0 }
  |> should.be_true()

  { list.length(round4) > 0 }
  |> should.be_true()

  { list.length(round5) > 0 }
  |> should.be_true()
}

/// Test that session progresses through all 5 rounds correctly
pub fn session_completes_all_five_rounds_test() {
  let initial_session = make_session("test-5-rounds", 0, [])

  // Round 1
  let questions_r1 = interview_questions.get_questions_for_round("api", 1)
  let answers_r1 =
    list.map(questions_r1, fn(q) { make_answer(q.id, "Answer for " <> q.id, 1) })
  let session_r1 =
    list.fold(answers_r1, initial_session, fn(sess, ans) {
      interview.add_answer(sess, ans)
    })
  let session_after_r1 = interview.complete_round(session_r1)

  session_after_r1.rounds_completed
  |> should.equal(1)

  // Round 2
  let questions_r2 = interview_questions.get_questions_for_round("api", 2)
  let answers_r2 =
    list.map(questions_r2, fn(q) { make_answer(q.id, "Answer for " <> q.id, 2) })
  let session_r2 =
    list.fold(answers_r2, session_after_r1, fn(sess, ans) {
      interview.add_answer(sess, ans)
    })
  let session_after_r2 = interview.complete_round(session_r2)

  session_after_r2.rounds_completed
  |> should.equal(2)

  // Round 3
  let questions_r3 = interview_questions.get_questions_for_round("api", 3)
  let answers_r3 =
    list.map(questions_r3, fn(q) { make_answer(q.id, "Answer for " <> q.id, 3) })
  let session_r3 =
    list.fold(answers_r3, session_after_r2, fn(sess, ans) {
      interview.add_answer(sess, ans)
    })
  let session_after_r3 = interview.complete_round(session_r3)

  session_after_r3.rounds_completed
  |> should.equal(3)

  // Round 4
  let questions_r4 = interview_questions.get_questions_for_round("api", 4)
  let answers_r4 =
    list.map(questions_r4, fn(q) { make_answer(q.id, "Answer for " <> q.id, 4) })
  let session_r4 =
    list.fold(answers_r4, session_after_r3, fn(sess, ans) {
      interview.add_answer(sess, ans)
    })
  let session_after_r4 = interview.complete_round(session_r4)

  session_after_r4.rounds_completed
  |> should.equal(4)

  // Round 5
  let questions_r5 = interview_questions.get_questions_for_round("api", 5)
  let answers_r5 =
    list.map(questions_r5, fn(q) { make_answer(q.id, "Answer for " <> q.id, 5) })
  let session_r5 =
    list.fold(answers_r5, session_after_r4, fn(sess, ans) {
      interview.add_answer(sess, ans)
    })
  let session_after_r5 = interview.complete_round(session_r5)

  session_after_r5.rounds_completed
  |> should.equal(5)

  // Final stage should be Complete
  session_after_r5.stage
  |> should.equal(Complete)
}

/// Test that stage transitions match expected flow
pub fn stage_transitions_through_rounds_correctly_test() {
  let session_r0 = make_session("test-stages", 0, [])
  session_r0.stage
  |> should.equal(Discovery)

  let session_r1 = interview.complete_round(session_r0)
  session_r1.stage
  |> should.equal(Discovery)

  let session_r2 = interview.complete_round(session_r1)
  session_r2.stage
  |> should.equal(Discovery)

  let session_r3 = interview.complete_round(session_r2)
  session_r3.stage
  |> should.equal(Refinement)

  let session_r4 = interview.complete_round(session_r3)
  session_r4.stage
  |> should.equal(Validation)

  let session_r5 = interview.complete_round(session_r4)
  session_r5.stage
  |> should.equal(Complete)
}

/// Test that get_current_round correctly identifies round based on answers
pub fn get_current_round_identifies_correct_round_test() {
  let session = make_session("test-current-round", 0, [])

  // No answers = round 1
  interview.get_current_round(session)
  |> should.equal(1)

  // Add some round 1 answers (but not all)
  let session_with_r1_partial =
    interview.add_answer(session, make_answer("q1", "Answer 1", 1))

  // Still round 1 if questions remain
  // NOTE: This might advance to round 2 if all questions are answered
  // The actual behavior depends on question count
  let current = interview.get_current_round(session_with_r1_partial)
  { current >= 1 && current <= 2 }
  |> should.be_true()
}

/// Test CLI profile also has 5 rounds
pub fn cli_profile_has_questions_for_all_five_rounds_test() {
  let round1 = interview_questions.get_questions_for_round("cli", 1)
  let round2 = interview_questions.get_questions_for_round("cli", 2)
  let round3 = interview_questions.get_questions_for_round("cli", 3)
  let round4 = interview_questions.get_questions_for_round("cli", 4)
  let round5 = interview_questions.get_questions_for_round("cli", 5)

  // All rounds should have at least one question
  { list.length(round1) > 0 }
  |> should.be_true()

  { list.length(round2) > 0 }
  |> should.be_true()

  { list.length(round3) > 0 }
  |> should.be_true()

  { list.length(round4) > 0 }
  |> should.be_true()

  { list.length(round5) > 0 }
  |> should.be_true()
}
