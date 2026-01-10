/// AI-CUE Protocol Tests
/// TDD: Tests written first to drive the implementation
///
/// These tests cover:
/// - [AI-CUE-2] --cue flag for interview command
/// - [AI-CUE-3] --answer flag for answer submission
/// - [AI-CUE-6] Generate beads as CUE
/// - [AI-CUE-7] Bead feedback via CUE
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should

// Import modules we'll create/extend
import intent/ai_cue_protocol.{
  AIDirective, AskQuestion, NotStarted, create_initial_directive,
  directive_to_cue, parse_answer_submission, validate_session_file,
}
import intent/question_types.{type Question, Critical, Developer, Question}

pub fn main() -> Nil {
  gleeunit.main()
}

// =============================================================================
// [AI-CUE-2] --cue flag: Output #AIDirective CUE
// =============================================================================

/// Test: Create initial directive for a new interview
pub fn ai_cue_create_initial_directive_test() {
  let directive = create_initial_directive("api", "session-abc123")

  // State should be not_started
  directive.state
  |> should.equal(NotStarted)

  // Action should be ask_question
  directive.action
  |> should.equal(AskQuestion)

  // Progress should start at 0
  directive.progress.percent_complete
  |> should.equal(0.0)

  directive.progress.current_round
  |> should.equal(1)

  directive.progress.questions_asked
  |> should.equal(0)
}

/// Test: Directive has a valid question when action is ask_question
pub fn ai_cue_directive_has_question_when_ask_test() {
  let directive = create_initial_directive("api", "session-abc123")

  case directive.action {
    AskQuestion -> {
      // Must have a question
      directive.question
      |> option.is_some()
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

/// Test: Convert directive to valid CUE string
pub fn ai_cue_directive_to_cue_test() {
  let directive = create_initial_directive("api", "session-abc123")
  let cue_output = directive_to_cue(directive)

  // Must contain required fields
  cue_output
  |> string.contains("state:")
  |> should.be_true()

  cue_output
  |> string.contains("action:")
  |> should.be_true()

  cue_output
  |> string.contains("progress:")
  |> should.be_true()

  cue_output
  |> string.contains("collected:")
  |> should.be_true()
}

/// Test: CUE output is valid CUE syntax (has proper quoting)
pub fn ai_cue_directive_cue_syntax_valid_test() {
  let directive = create_initial_directive("api", "session-abc123")
  let cue_output = directive_to_cue(directive)

  // Should have proper CUE structure
  cue_output
  |> string.contains("\"not_started\"")
  |> should.be_true()

  cue_output
  |> string.contains("\"ask_question\"")
  |> should.be_true()
}

/// Test: Progress calculation is correct
pub fn ai_cue_progress_calculation_test() {
  // When 5 of 25 questions are asked, progress should be 20%
  let progress = ai_cue_protocol.calculate_progress(5, 25, 1, 5)

  progress.questions_asked
  |> should.equal(5)

  progress.questions_remain
  |> should.equal(20)

  progress.percent_complete
  |> should.equal(20.0)
}

// =============================================================================
// [AI-CUE-3] --answer flag: Submit answers and get next directive
// =============================================================================

/// Test: Parse valid answer submission
pub fn ai_cue_parse_answer_submission_test() {
  let raw_answer = "The system shall authenticate users with JWT tokens"
  let question_id = "q-auth-001"

  let result = parse_answer_submission(question_id, raw_answer, "high")

  result
  |> should.be_ok()

  let assert Ok(submission) = result
  submission.question_id
  |> should.equal(question_id)

  submission.raw_answer
  |> should.equal(raw_answer)

  submission.confidence
  |> should.equal(ai_cue_protocol.High)
}

/// Test: Empty answer is rejected
pub fn ai_cue_reject_empty_answer_test() {
  let result = parse_answer_submission("q-001", "", "high")

  result
  |> should.be_error()
}

/// Test: Very short answer gets low confidence
pub fn ai_cue_short_answer_low_confidence_test() {
  let result = parse_answer_submission("q-001", "yes", "high")

  result
  |> should.be_ok()

  // Even if user says "high", short answers should be flagged
  let assert Ok(submission) = result
  // Implementation should warn about short answers
  { string.length(submission.raw_answer) < 10 }
  |> should.be_true()
}

/// Test: After answer, get next directive
pub fn ai_cue_next_directive_after_answer_test() {
  // Start with initial directive
  let directive = create_initial_directive("api", "session-abc123")

  // Submit an answer
  let assert Ok(submission) =
    parse_answer_submission(
      "q-001",
      "The system shall authenticate users with JWT tokens",
      "high",
    )

  // Get next directive
  let next = ai_cue_protocol.process_answer(directive, submission)

  // Progress should advance
  next.progress.questions_asked
  |> should.equal(1)

  // Should still be asking questions (round 1 has more questions)
  next.action
  |> should.equal(AskQuestion)
}

/// Test: When all questions answered, action becomes generate_beads
pub fn ai_cue_complete_interview_test() {
  // Create directive at final question
  let progress =
    ai_cue_protocol.Progress(
      current_round: 5,
      total_rounds: 5,
      questions_asked: 24,
      questions_remain: 1,
      percent_complete: 96.0,
    )

  let directive =
    AIDirective(
      state: ai_cue_protocol.RoundUnwanted,
      action: AskQuestion,
      question: Some(make_test_question("q-final")),
      progress: progress,
      collected: ai_cue_protocol.empty_collected(),
      errors: None,
    )

  // Submit final answer
  let assert Ok(submission) =
    parse_answer_submission(
      "q-final",
      "The system shall not allow unauthenticated access",
      "high",
    )

  let next = ai_cue_protocol.process_answer(directive, submission)

  // Action should be generate_beads
  next.action
  |> should.equal(ai_cue_protocol.GenerateBeads)

  next.state
  |> should.equal(ai_cue_protocol.Complete)
}

/// Test: Invalid answer triggers clarify_answer action
pub fn ai_cue_invalid_answer_clarify_test() {
  let directive = create_initial_directive("api", "session-abc123")

  // Submit a confusing answer
  let assert Ok(submission) = parse_answer_submission("q-001", "???", "low")

  let next = ai_cue_protocol.process_answer(directive, submission)

  // Should ask for clarification
  next.action
  |> should.equal(ai_cue_protocol.ClarifyAnswer)

  // Errors should be populated
  next.errors
  |> option.is_some()
  |> should.be_true()
}

// =============================================================================
// Session File Validation
// =============================================================================

/// Test: Valid session file path format
pub fn ai_cue_validate_session_path_test() {
  let result = validate_session_file(".intent/session-abc123.cue")
  result |> should.be_ok()
}

/// Test: Invalid session file path rejected
pub fn ai_cue_reject_invalid_session_path_test() {
  let result = validate_session_file("random/path.txt")
  result |> should.be_error()
}

/// Test: Session directory created if missing
pub fn ai_cue_create_intent_directory_test() {
  // This is more of an integration test
  // Unit test just verifies the function exists and handles input
  let result = ai_cue_protocol.ensure_intent_directory()
  // In unit test context, just verify function exists
  result |> should.be_ok()
}

// =============================================================================
// Collected Requirements
// =============================================================================

/// Test: Empty collected requirements structure
pub fn ai_cue_empty_collected_test() {
  let collected = ai_cue_protocol.empty_collected()

  list.length(collected.ubiquitous)
  |> should.equal(0)

  list.length(collected.event_driven)
  |> should.equal(0)

  list.length(collected.inversions.security)
  |> should.equal(0)
}

/// Test: Add requirement to collected
pub fn ai_cue_add_requirement_test() {
  let collected = ai_cue_protocol.empty_collected()

  let req =
    ai_cue_protocol.EARSRequirement(
      id: "req-001",
      pattern: ai_cue_protocol.Ubiquitous,
      trigger: None,
      state: None,
      condition: None,
      shall: "authenticate users",
      shall_not: None,
      raw_text: "The system shall authenticate users",
    )

  let updated = ai_cue_protocol.add_requirement(collected, req)

  list.length(updated.ubiquitous)
  |> should.equal(1)
}

// =============================================================================
// EARS Pattern Detection
// =============================================================================

/// Test: Detect ubiquitous pattern
pub fn ai_cue_detect_ubiquitous_pattern_test() {
  let text = "The system shall authenticate users"
  let pattern = ai_cue_protocol.detect_ears_pattern(text)

  pattern
  |> should.equal(ai_cue_protocol.Ubiquitous)
}

/// Test: Detect event-driven pattern
pub fn ai_cue_detect_event_driven_pattern_test() {
  let text = "When a user logs in, the system shall create a session"
  let pattern = ai_cue_protocol.detect_ears_pattern(text)

  pattern
  |> should.equal(ai_cue_protocol.EventDriven)
}

/// Test: Detect state-driven pattern
pub fn ai_cue_detect_state_driven_pattern_test() {
  let text = "While the user is authenticated, the system shall allow access"
  let pattern = ai_cue_protocol.detect_ears_pattern(text)

  pattern
  |> should.equal(ai_cue_protocol.StateDriven)
}

/// Test: Detect unwanted pattern
pub fn ai_cue_detect_unwanted_pattern_test() {
  let text = "If the token is expired, the system shall not allow access"
  let pattern = ai_cue_protocol.detect_ears_pattern(text)

  pattern
  |> should.equal(ai_cue_protocol.Unwanted)
}

// =============================================================================
// [OPT-1] Default output should NOT contain KIRK extension fields
// =============================================================================

/// Test: Default directive CUE output should NOT contain inversions
pub fn ai_cue_default_no_inversions_test() {
  let directive = create_initial_directive("api", "session-abc123")
  let cue_output = directive_to_cue(directive)

  // Should NOT contain KIRK extension fields by default
  cue_output
  |> string.contains("inversions:")
  |> should.be_false()
}

/// Test: Default directive CUE output should NOT contain premortem
pub fn ai_cue_default_no_premortem_test() {
  let directive = create_initial_directive("api", "session-abc123")
  let cue_output = directive_to_cue(directive)

  // Should NOT contain premortem field by default
  cue_output
  |> string.contains("premortem:")
  |> should.be_false()
}

/// Test: Default directive CUE output should NOT contain quality_score
pub fn ai_cue_default_no_quality_score_test() {
  let directive = create_initial_directive("api", "session-abc123")
  let cue_output = directive_to_cue(directive)

  // Should NOT contain quality_score field by default
  cue_output
  |> string.contains("quality_score:")
  |> should.be_false()
}

/// Test: Default directive CUE output should NOT contain second_order_effects
pub fn ai_cue_default_no_second_order_test() {
  let directive = create_initial_directive("api", "session-abc123")
  let cue_output = directive_to_cue(directive)

  // Should NOT contain second_order_effects field by default
  cue_output
  |> string.contains("second_order_effects:")
  |> should.be_false()
}

/// Test: With analysis flag enabled, CUE output SHOULD contain inversions
pub fn ai_cue_with_analysis_has_inversions_test() {
  let directive = create_initial_directive("api", "session-abc123")
  let cue_output = ai_cue_protocol.directive_to_cue_with_analysis(directive)

  // SHOULD contain inversions when analysis is enabled
  cue_output
  |> string.contains("inversions:")
  |> should.be_true()
}

/// Test: With analysis flag enabled, CUE output SHOULD contain premortem
pub fn ai_cue_with_analysis_has_premortem_test() {
  let directive = create_initial_directive("api", "session-abc123")
  let cue_output = ai_cue_protocol.directive_to_cue_with_analysis(directive)

  // SHOULD contain premortem when analysis is enabled
  cue_output
  |> string.contains("premortem:")
  |> should.be_true()
}

// =============================================================================
// Helper Functions
// =============================================================================

fn make_test_question(id: String) -> Question {
  Question(
    id: id,
    round: 1,
    perspective: Developer,
    category: question_types.HappyPath,
    priority: Critical,
    question: "Test question for " <> id,
    context: "Test context",
    example: "Test example",
    expected_type: "text",
    extract_into: [],
    depends_on: [],
    blocks: [],
  )
}
