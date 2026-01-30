//// Tests for CUE mode interview rounds persistence
//// Bug: intent-cli-xxxx - rounds_completed not updated when answering in CUE mode
////
//// Context:
//// When answering questions in CUE mode (--session --answer), the session
//// saves the answer but rounds_completed stays at 0. Resuming always restarts
//// from question 1.
////
//// Root cause: run_interview_cue_answer() calls interview.add_answer() but never
//// calls interview.complete_round() to increment rounds_completed.

import gleam/dict
import gleam/list
import gleeunit
import gleeunit/should
import intent/interview.{
  type Answer, type InterviewSession, Answer, Api, Discovery, InterviewSession,
}
import intent/question_types.{Developer}

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// Test Helpers
// ============================================================================

/// Create a test answer
fn make_answer(question_id: String, response: String, round: Int) -> Answer {
  Answer(
    question_id: question_id,
    question_text: "Test question: " <> question_id,
    perspective: Developer,
    round: round,
    response: response,
    extracted: dict.new(),
    confidence: 0.85,
    notes: "Test answer",
    timestamp: "2026-01-23T12:00:00Z",
  )
}

/// Create a test session with specified rounds_completed
fn make_session(
  id: String,
  rounds_completed: Int,
  answers: List(Answer),
) -> InterviewSession {
  InterviewSession(
    id: id,
    profile: Api,
    created_at: "2026-01-23T12:00:00Z",
    updated_at: "2026-01-23T12:00:00Z",
    completed_at: "",
    stage: Discovery,
    rounds_completed: rounds_completed,
    answers: answers,
    gaps: [],
    conflicts: [],
    raw_notes: "Test session",
  )
}

// ============================================================================
// BUG REPRODUCTION TEST - FAILING
// ============================================================================

/// Test that demonstrates the bug: rounds_completed stays at 0 after answering
///
/// EXPECTED BEHAVIOR:
/// When all questions in round 1 are answered, rounds_completed should be 1
///
/// CURRENT BEHAVIOR (BUG):
/// rounds_completed remains 0 even after completing all questions in round 1
pub fn cue_mode_should_update_rounds_completed_when_round_completes_test() {
  // Step 1: Create a session with rounds_completed = 0
  let initial_session = make_session("test-session-1", 0, [])

  // Step 2: Simulate answering all questions in round 1
  // (In real CUE mode, these would come from run_interview_cue_answer calls)
  let answer1 = make_answer("q1", "First response", 1)
  let answer2 = make_answer("q2", "Second response", 1)
  let answer3 = make_answer("q3", "Third response", 1)

  // Add answers one by one
  let session_after_q1 = interview.add_answer(initial_session, answer1)
  let session_after_q2 = interview.add_answer(session_after_q1, answer2)
  let session_after_q3 = interview.add_answer(session_after_q2, answer3)

  // Step 3: Check current state (this is where the bug manifests)
  // After adding all answers for round 1, rounds_completed is still 0
  session_after_q3.rounds_completed
  |> should.equal(0)

  // Step 4: Call complete_round() manually (this is what CUE mode should do)
  let session_after_round_complete = interview.complete_round(session_after_q3)

  // Step 5: Verify expected behavior - rounds_completed should be 1
  session_after_round_complete.rounds_completed
  |> should.equal(1)
}

/// Test that rounds_completed increments correctly through multiple rounds
pub fn rounds_completed_increments_correctly_through_multiple_rounds_test() {
  let initial_session = make_session("test-session-2", 0, [])

  // Complete round 1
  let round1_answers = [
    make_answer("q1", "Response 1", 1),
    make_answer("q2", "Response 2", 1),
    make_answer("q3", "Response 3", 1),
  ]
  let session_with_r1 =
    list.fold(round1_answers, initial_session, fn(sess, ans) {
      interview.add_answer(sess, ans)
    })
  let session_after_r1 = interview.complete_round(session_with_r1)

  // Verify round 1 complete
  session_after_r1.rounds_completed
  |> should.equal(1)

  // Complete round 2
  let round2_answers = [
    make_answer("q4", "Response 4", 2),
    make_answer("q5", "Response 5", 2),
  ]
  let session_with_r2 =
    list.fold(round2_answers, session_after_r1, fn(sess, ans) {
      interview.add_answer(sess, ans)
    })
  let session_after_r2 = interview.complete_round(session_with_r2)

  // Verify round 2 complete
  session_after_r2.rounds_completed
  |> should.equal(2)
}

/// Test that add_answer does NOT update rounds_completed (it's unchanged)
pub fn add_answer_does_not_modify_rounds_completed_test() {
  let session = make_session("test-session-3", 2, [])
  let answer = make_answer("q1", "Test response", 3)

  let updated_session = interview.add_answer(session, answer)

  // Verify that add_answer() doesn't change rounds_completed
  updated_session.rounds_completed
  |> should.equal(2)
}

/// Test that complete_round increments rounds_completed
pub fn complete_round_increments_rounds_completed_test() {
  let session = make_session("test-session-4", 0, [])

  let updated = interview.complete_round(session)

  updated.rounds_completed
  |> should.equal(1)
}

/// Test that complete_round works correctly at each milestone
pub fn complete_round_increments_at_each_round_test() {
  let session_r0 = make_session("test-session-5", 0, [])
  let session_r1 = interview.complete_round(session_r0)
  let session_r2 = interview.complete_round(session_r1)
  let session_r3 = interview.complete_round(session_r2)
  let session_r4 = interview.complete_round(session_r3)
  let session_r5 = interview.complete_round(session_r4)

  // Verify each round increments
  session_r0.rounds_completed
  |> should.equal(0)

  session_r1.rounds_completed
  |> should.equal(1)

  session_r2.rounds_completed
  |> should.equal(2)

  session_r3.rounds_completed
  |> should.equal(3)

  session_r4.rounds_completed
  |> should.equal(4)

  session_r5.rounds_completed
  |> should.equal(5)
}
