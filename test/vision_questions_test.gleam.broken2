//// Comprehensive tests for intent/vision_questions.gleam
//// Tests cover the 8 core Vision phase questions
////
//// Design by Contract:
//// - Preconditions: Valid question IDs and answers
//// - Postconditions: Questions map correctly to VisionSection fields
//// - Invariants: All 8 questions are required and ordered correctly

import gleam/dict
import gleam/list
import gleeunit
import gleeunit/should
import intent/vision_questions

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Question Definition Tests
// ============================================================================

pub fn all_questions_returns_eight_test() {
  let questions = vision_questions.all_questions()

  questions
  |> list.length()
  |> should.equal(8)
}

pub fn questions_have_correct_ids_test() {
  let questions = vision_questions.all_questions()

  let ids =
    questions
    |> list.map(fn(q) { q.id })

  ids
  |> should.equal([
    "q1_problem",
    "q2_persona",
    "q3_non_personas",
    "q4_replaces",
    "q5_vorp",
    "q6_north_star",
    "q7_scenarios",
    "q8_out_of_scope",
  ])
}

pub fn questions_have_required_fields_test() {
  let questions = vision_questions.all_questions()

  // First question should have id, text, field_name
  let first = case list.first(questions) {
    Ok(q) -> q
    Error(_) -> panic as "Expected at least one question"
  }

  first.id
  |> should.equal("q1_problem")

  // Text should be non-empty
  first.text
  |> should.not_equal("")

  // Should map to press_release field
  first.field_name
  |> should.equal("press_release")
}

pub fn all_questions_are_required_test() {
  let questions = vision_questions.all_questions()

  // All 8 questions should be required
  questions
  |> list.all(fn(q) { q.required })
  |> should.be_true()
}

// ============================================================================
// Question Lookup Tests
// ============================================================================

pub fn get_question_by_id_success_test() {
  let result = vision_questions.get_question("q1_problem")

  case result {
    Ok(q) -> {
      q.id
      |> should.equal("q1_problem")

      q.field_name
      |> should.equal("press_release")
    }
    Error(_) -> panic as "Expected Ok result"
  }
}

pub fn get_question_by_id_not_found_test() {
  let result = vision_questions.get_question("invalid_id")

  result
  |> should.be_error()
}

// ============================================================================
// Answer Validation Tests
// ============================================================================

pub fn validate_answer_success_test() {
  let result = vision_questions.validate_answer("q1_problem", "We are solving X")

  result
  |> should.be_ok()
}

pub fn validate_answer_empty_fails_test() {
  let result = vision_questions.validate_answer("q1_problem", "")

  result
  |> should.be_error()
}

pub fn validate_answer_whitespace_only_fails_test() {
  let result = vision_questions.validate_answer("q1_problem", "   ")

  result
  |> should.be_error()
}

pub fn validate_answer_invalid_question_id_test() {
  let result = vision_questions.validate_answer("invalid_id", "Some answer")

  result
  |> should.be_error()
}

// ============================================================================
// Progress Tracking Tests
// ============================================================================

pub fn calculate_progress_all_answered_test() {
  let answers =
    dict.from_list([
      #("q1_problem", "Answer 1"),
      #("q2_persona", "Answer 2"),
      #("q3_non_personas", "Answer 3"),
      #("q4_replaces", "Answer 4"),
      #("q5_vorp", "Answer 5"),
      #("q6_north_star", "Answer 6"),
      #("q7_scenarios", "Answer 7"),
      #("q8_out_of_scope", "Answer 8"),
    ])

  let progress = vision_questions.calculate_progress(answers)

  progress.total
  |> should.equal(8)

  progress.answered
  |> should.equal(8)

  progress.remaining
  |> should.equal(0)

  progress.is_complete
  |> should.be_true()
}

pub fn calculate_progress_partially_answered_test() {
  let answers =
    dict.from_list([
      #("q1_problem", "Answer 1"),
      #("q2_persona", "Answer 2"),
      #("q3_non_personas", "Answer 3"),
    ])

  let progress = vision_questions.calculate_progress(answers)

  progress.total
  |> should.equal(8)

  progress.answered
  |> should.equal(3)

  progress.remaining
  |> should.equal(5)

  progress.is_complete
  |> should.be_false()
}

pub fn calculate_progress_no_answers_test() {
  let answers = dict.new()

  let progress = vision_questions.calculate_progress(answers)

  progress.total
  |> should.equal(8)

  progress.answered
  |> should.equal(0)

  progress.remaining
  |> should.equal(8)

  progress.is_complete
  |> should.be_false()
}

pub fn next_unanswered_question_test() {
  let answers =
    dict.from_list([
      #("q1_problem", "Answer 1"),
      #("q2_persona", "Answer 2"),
    ])

  let next = vision_questions.next_unanswered(answers)

  case next {
    Ok(q) -> {
      q.id
      |> should.equal("q3_non_personas")
    }
    Error(_) -> panic as "Expected Ok result"
  }
}

pub fn next_unanswered_all_answered_test() {
  let answers =
    dict.from_list([
      #("q1_problem", "Answer 1"),
      #("q2_persona", "Answer 2"),
      #("q3_non_personas", "Answer 3"),
      #("q4_replaces", "Answer 4"),
      #("q5_vorp", "Answer 5"),
      #("q6_north_star", "Answer 6"),
      #("q7_scenarios", "Answer 7"),
      #("q8_out_of_scope", "Answer 8"),
    ])

  let next = vision_questions.next_unanswered(answers)

  next
  |> should.be_error()
}
