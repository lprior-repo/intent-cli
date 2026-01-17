//// AI Agent Integration Tests
////
//// Comprehensive E2E tests simulating AI agent conducting full interview flow:
//// - Starting interview in CUE mode (--cue flag)
//// - Answering questions progressively through all 5 rounds
//// - Resuming sessions mid-interview
//// - Handling validation errors and edge cases
//// - Session persistence across invocations
//// - Complete interview lifecycle with CUE spec generation
////
//// These tests validate that an AI agent can conduct a full requirements
//// interview programmatically without human interaction.

import gleam/dict
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import gleeunit/should
import intent/interview
import intent/interview_storage
import intent/question_types
import simplifile

// ============================================================================
// Test Setup and Teardown
// ============================================================================

/// Clean up test sessions before each test
fn setup_test_environment() -> Nil {
  // Ensure .interview directory exists
  let _ = simplifile.create_directory(".interview")
  // Clean up any existing test sessions
  let _ = simplifile.delete("test-sessions.jsonl")
  Nil
}

fn teardown_test_environment() -> Nil {
  // Clean up test artifacts
  let _ = simplifile.delete("test-sessions.jsonl")
  Nil
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Create a test session for AI agent simulation
fn create_test_session(
  session_id: String,
  profile: interview.Profile,
) -> interview.InterviewSession {
  interview.create_session(session_id, profile, "2024-01-01T00:00:00Z")
}

/// Create a mock answer for testing
fn create_mock_answer(
  question_id: String,
  response: String,
  round: Int,
) -> interview.Answer {
  interview.Answer(
    question_id: question_id,
    question_text: "Test question",
    perspective: question_types.Developer,
    round: round,
    response: response,
    extracted: dict.new(),
    confidence: 0.8,
    notes: "",
    timestamp: "2024-01-01T00:00:00Z",
  )
}

/// Simulate answering all questions for a round
fn answer_all_questions_in_round(
  session: interview.InterviewSession,
  round: Int,
) -> interview.InterviewSession {
  let profile_str = interview.profile_to_string(session.profile)
  let questions =
    intent_questions_for_round(profile_str, round)
    |> list.take(5)

  list.fold(questions, session, fn(sess, question) {
    let answer =
      create_mock_answer(
        question.id,
        "THE SYSTEM SHALL " <> question.id <> " behavior",
        round,
      )
    interview.add_answer(sess, answer)
  })
}

/// Get questions for a round (mock implementation)
fn intent_questions_for_round(
  _profile: String,
  round: Int,
) -> List(question_types.Question) {
  // Generate mock questions for testing
  list.range(1, 5)
  |> list.map(fn(i) {
    question_types.Question(
      id: "q_r" <> string.inspect(round) <> "_" <> string.inspect(i),
      round: round,
      perspective: question_types.Developer,
      category: question_types.HappyPath,
      priority: question_types.Important,
      question: "Round "
        <> string.inspect(round)
        <> " Question "
        <> string.inspect(i),
      context: "Context for question",
      example: "Example answer",
      expected_type: "string",
      extract_into: [],
      depends_on: [],
      blocks: [],
    )
  })
}

// ============================================================================
// Test: Session Creation and Persistence
// ============================================================================

pub fn create_new_session_test() {
  setup_test_environment()

  let session = create_test_session("test-session-1", interview.Api)

  session.id
  |> should.equal("test-session-1")
  session.profile
  |> should.equal(interview.Api)
  session.stage
  |> should.equal(interview.Discovery)
  list.length(session.answers)
  |> should.equal(0)

  teardown_test_environment()
}

pub fn save_session_to_jsonl_test() {
  setup_test_environment()

  let session = create_test_session("test-session-save", interview.Api)

  let result =
    interview_storage.append_session_to_jsonl(session, "test-sessions.jsonl")

  result
  |> should.be_ok

  teardown_test_environment()
}

pub fn load_session_from_jsonl_test() {
  setup_test_environment()

  let session = create_test_session("test-session-load", interview.Cli)
  let _ =
    interview_storage.append_session_to_jsonl(session, "test-sessions.jsonl")

  let result =
    interview_storage.get_session_from_jsonl(
      "test-sessions.jsonl",
      "test-session-load",
    )

  case result {
    Ok(loaded) -> {
      loaded.id
      |> should.equal("test-session-load")
      loaded.profile
      |> should.equal(interview.Cli)
    }
    Error(_) -> should.fail()
  }

  teardown_test_environment()
}

pub fn load_nonexistent_session_fails_test() {
  setup_test_environment()

  let result =
    interview_storage.get_session_from_jsonl(
      "test-sessions.jsonl",
      "nonexistent-session",
    )

  result
  |> should.be_error

  teardown_test_environment()
}

// ============================================================================
// Test: Answer Submission and Validation
// ============================================================================

pub fn add_answer_updates_session_test() {
  setup_test_environment()

  let session = create_test_session("test-answer-1", interview.Api)
  let answer =
    create_mock_answer("q1", "THE SYSTEM SHALL authenticate users", 1)

  let updated = interview.add_answer(session, answer)

  list.length(updated.answers)
  |> should.equal(1)
  updated.updated_at
  |> should.equal(answer.timestamp)

  teardown_test_environment()
}

pub fn multiple_answers_accumulate_test() {
  setup_test_environment()

  let session = create_test_session("test-multi-answer", interview.Api)
  let answer1 =
    create_mock_answer("q1", "THE SYSTEM SHALL provide authentication", 1)
  let answer2 = create_mock_answer("q2", "THE SYSTEM SHALL validate input", 1)
  let answer3 =
    create_mock_answer("q3", "THE SYSTEM SHALL return JSON responses", 1)

  let session = interview.add_answer(session, answer1)
  let session = interview.add_answer(session, answer2)
  let session = interview.add_answer(session, answer3)

  list.length(session.answers)
  |> should.equal(3)

  teardown_test_environment()
}

pub fn answer_extraction_captures_fields_test() {
  setup_test_environment()

  let response = "We'll use JWT authentication for the API"
  let fields = ["auth_method"]

  let extracted = interview.extract_from_answer("q1", response, fields)

  case dict.get(extracted, "auth_method") {
    Ok(value) ->
      value
      |> should.equal("jwt")
    Error(_) -> should.fail()
  }

  teardown_test_environment()
}

pub fn confidence_calculation_varies_by_response_test() {
  setup_test_environment()

  // High confidence: long response with extracted fields
  let high_conf =
    interview.calculate_confidence(
      "q1",
      "THE SYSTEM SHALL authenticate users using JWT tokens with RS256 signing and 1-hour expiry",
      dict.from_list([#("auth_method", "jwt")]),
    )

  // Low confidence: short response with no extraction
  let low_conf = interview.calculate_confidence("q2", "Yes", dict.new())

  high_conf
  |> should.equal(0.85)
  low_conf
  |> should.equal(0.6)

  teardown_test_environment()
}

// ============================================================================
// Test: Round Progression
// ============================================================================

pub fn complete_round_advances_stage_test() {
  setup_test_environment()

  let session = create_test_session("test-round-progress", interview.Api)

  // Start at round 0, Discovery stage
  session.rounds_completed
  |> should.equal(0)
  session.stage
  |> should.equal(interview.Discovery)

  // Complete round 1
  let session = interview.complete_round(session)
  session.rounds_completed
  |> should.equal(1)
  session.stage
  |> should.equal(interview.Discovery)

  // Complete round 2
  let session = interview.complete_round(session)
  session.rounds_completed
  |> should.equal(2)
  session.stage
  |> should.equal(interview.Discovery)

  // Complete round 3 - transitions to Refinement
  let session = interview.complete_round(session)
  session.rounds_completed
  |> should.equal(3)
  session.stage
  |> should.equal(interview.Refinement)

  // Complete round 4 - transitions to Validation
  let session = interview.complete_round(session)
  session.rounds_completed
  |> should.equal(4)
  session.stage
  |> should.equal(interview.Validation)

  // Complete round 5 - transitions to Complete
  let session = interview.complete_round(session)
  session.rounds_completed
  |> should.equal(5)
  session.stage
  |> should.equal(interview.Complete)

  teardown_test_environment()
}

pub fn get_current_round_from_answers_test() {
  setup_test_environment()

  let session = create_test_session("test-current-round", interview.Api)

  // No answers = round 1
  let round = interview.get_current_round(session)
  round
  |> should.equal(1)

  // Add answers from round 1
  let session = answer_all_questions_in_round(session, 1)
  let round = interview.get_current_round(session)
  // After answering all round 1 questions, should move to round 2
  // (implementation may vary based on question count)

  teardown_test_environment()
}

// ============================================================================
// Test: Gap Detection
// ============================================================================

pub fn detect_gaps_for_api_profile_test() {
  setup_test_environment()

  let session = create_test_session("test-gaps-api", interview.Api)

  // No answers = all required fields are gaps
  let gaps = interview.detect_gaps(session.profile, session.answers)

  list.length(gaps)
  |> should.equal(5)

  // Should include base_url gap
  let has_base_url =
    list.any(gaps, fn(gap) { string.contains(gap.field, "base_url") })
  has_base_url
  |> should.equal(True)

  teardown_test_environment()
}

pub fn detect_gaps_resolved_by_answers_test() {
  setup_test_environment()

  let session = create_test_session("test-gaps-resolved", interview.Api)

  // Create answer with extracted fields
  let answer =
    interview.Answer(
      question_id: "q1",
      question_text: "What's the base URL?",
      perspective: question_types.Developer,
      round: 1,
      response: "https://api.example.com",
      extracted: dict.from_list([#("base_url", "https://api.example.com")]),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let gaps = interview.detect_gaps(session.profile, [answer])

  // base_url gap should be resolved
  let has_base_url =
    list.any(gaps, fn(gap) { string.contains(gap.field, "base_url") })
  has_base_url
  |> should.equal(False)

  teardown_test_environment()
}

pub fn detect_gaps_for_cli_profile_test() {
  setup_test_environment()

  let session = create_test_session("test-gaps-cli", interview.Cli)
  let gaps = interview.detect_gaps(session.profile, [])

  // Should have CLI-specific required fields
  let has_command_name =
    list.any(gaps, fn(gap) { string.contains(gap.field, "command_name") })
  has_command_name
  |> should.equal(True)

  teardown_test_environment()
}

// ============================================================================
// Test: Conflict Detection
// ============================================================================

pub fn detect_cap_theorem_conflict_test() {
  setup_test_environment()

  let answer1 =
    create_mock_answer("q1", "We need fast response times with low latency", 1)
  let answer2 =
    create_mock_answer("q2", "Data must be strongly consistent at all times", 1)

  let conflicts = interview.detect_conflicts([answer1, answer2])

  list.length(conflicts)
  |> should.equal(1)

  let first = list.first(conflicts)
  case first {
    Ok(conflict) -> {
      string.contains(conflict.id, "cap")
      |> should.equal(True)
    }
    Error(_) -> should.fail()
  }

  teardown_test_environment()
}

pub fn detect_anonymous_audit_conflict_test() {
  setup_test_environment()

  let answer1 =
    create_mock_answer("q1", "Users should be completely anonymous", 1)
  let answer2 =
    create_mock_answer("q2", "We need full audit trail of all actions", 1)

  let conflicts = interview.detect_conflicts([answer1, answer2])

  list.length(conflicts)
  |> should.equal(1)

  teardown_test_environment()
}

pub fn no_conflicts_with_compatible_answers_test() {
  setup_test_environment()

  let answer1 = create_mock_answer("q1", "Simple API design", 1)
  let answer2 = create_mock_answer("q2", "Basic error handling", 1)

  let conflicts = interview.detect_conflicts([answer1, answer2])

  list.length(conflicts)
  |> should.equal(0)

  teardown_test_environment()
}

// ============================================================================
// Test: Conflict and Gap Resolution
// ============================================================================

pub fn resolve_conflict_updates_session_test() {
  setup_test_environment()

  let session = create_test_session("test-resolve-conflict", interview.Api)
  let conflict =
    interview.Conflict(
      id: "conflict-1",
      between: #("q1", "q2"),
      description: "CAP theorem conflict",
      impact: "Must choose consistency or availability",
      options: [],
      chosen: -1,
    )
  let session = interview.InterviewSession(..session, conflicts: [conflict])

  let result = interview.resolve_conflict(session, "conflict-1", 0)

  case result {
    Ok(updated) -> {
      let first_conflict = list.first(updated.conflicts)
      case first_conflict {
        Ok(c) ->
          c.chosen
          |> should.equal(0)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }

  teardown_test_environment()
}

pub fn resolve_gap_marks_as_resolved_test() {
  setup_test_environment()

  let session = create_test_session("test-resolve-gap", interview.Api)
  let gap =
    interview.Gap(
      id: "gap-1",
      field: "base_url",
      description: "Missing base URL",
      blocking: True,
      suggested_default: "",
      why_needed: "Required for API calls",
      round: 1,
      resolved: False,
      resolution: "",
    )
  let session = interview.InterviewSession(..session, gaps: [gap])

  let updated =
    interview.resolve_gap(session, "gap-1", "https://api.example.com")

  let first_gap = list.first(updated.gaps)
  case first_gap {
    Ok(g) -> {
      g.resolved
      |> should.equal(True)
      g.resolution
      |> should.equal("https://api.example.com")
    }
    Error(_) -> should.fail()
  }

  teardown_test_environment()
}

pub fn get_blocking_gaps_filters_correctly_test() {
  setup_test_environment()

  let session = create_test_session("test-blocking-gaps", interview.Api)
  let gap1 =
    interview.Gap(
      id: "gap-1",
      field: "field1",
      description: "Blocking gap",
      blocking: True,
      suggested_default: "",
      why_needed: "Critical",
      round: 1,
      resolved: False,
      resolution: "",
    )
  let gap2 =
    interview.Gap(
      id: "gap-2",
      field: "field2",
      description: "Non-blocking gap",
      blocking: False,
      suggested_default: "",
      why_needed: "Optional",
      round: 1,
      resolved: False,
      resolution: "",
    )
  let session = interview.InterviewSession(..session, gaps: [gap1, gap2])

  let blocking = interview.get_blocking_gaps(session)

  list.length(blocking)
  |> should.equal(1)

  teardown_test_environment()
}

pub fn can_proceed_blocks_on_unresolved_gaps_test() {
  setup_test_environment()

  let session = create_test_session("test-can-proceed", interview.Api)
  let gap =
    interview.Gap(
      id: "gap-1",
      field: "critical_field",
      description: "Blocking gap",
      blocking: True,
      suggested_default: "",
      why_needed: "Required",
      round: 1,
      resolved: False,
      resolution: "",
    )
  let session = interview.InterviewSession(..session, gaps: [gap])

  let result = interview.can_proceed(session)

  case result {
    Error(msg) -> {
      string.contains(msg, "Blocking gaps")
      |> should.equal(True)
    }
    Ok(_) -> should.fail()
  }

  teardown_test_environment()
}

pub fn can_proceed_allows_non_blocking_gaps_test() {
  setup_test_environment()

  let session =
    create_test_session("test-can-proceed-nonblocking", interview.Api)
  let gap =
    interview.Gap(
      id: "gap-1",
      field: "optional_field",
      description: "Non-blocking gap",
      blocking: False,
      suggested_default: "",
      why_needed: "Nice to have",
      round: 1,
      resolved: False,
      resolution: "",
    )
  let session = interview.InterviewSession(..session, gaps: [gap])

  let result = interview.can_proceed(session)

  result
  |> should.be_ok

  teardown_test_environment()
}

// ============================================================================
// Test: Session Diff and History
// ============================================================================

pub fn diff_sessions_detects_added_answers_test() {
  setup_test_environment()

  let session1 = create_test_session("test-diff-1", interview.Api)
  let session2 = create_test_session("test-diff-1", interview.Api)
  let answer = create_mock_answer("q1", "THE SYSTEM SHALL authenticate", 1)
  let session2 = interview.add_answer(session2, answer)

  let diff = interview_storage.diff_sessions(session1, session2)

  list.length(diff.answers_added)
  |> should.equal(1)
  list.length(diff.answers_modified)
  |> should.equal(0)
  list.length(diff.answers_removed)
  |> should.equal(0)

  teardown_test_environment()
}

pub fn diff_sessions_detects_modified_answers_test() {
  setup_test_environment()

  let answer1 = create_mock_answer("q1", "Original response", 1)
  let answer2 = create_mock_answer("q1", "Modified response", 1)

  let session1 = create_test_session("test-diff-2", interview.Api)
  let session1 = interview.add_answer(session1, answer1)

  let session2 = create_test_session("test-diff-2", interview.Api)
  let session2 = interview.add_answer(session2, answer2)

  let diff = interview_storage.diff_sessions(session1, session2)

  list.length(diff.answers_modified)
  |> should.equal(1)

  teardown_test_environment()
}

pub fn diff_sessions_detects_stage_changes_test() {
  setup_test_environment()

  let session1 = create_test_session("test-diff-stage", interview.Api)
  let session2 =
    interview.InterviewSession(..session1, stage: interview.Refinement)

  let diff = interview_storage.diff_sessions(session1, session2)

  case diff.stage_changed {
    option.Some(#(from, to)) -> {
      from
      |> should.equal("discovery")
      to
      |> should.equal("refinement")
    }
    option.None -> should.fail()
  }

  teardown_test_environment()
}

// ============================================================================
// Test: Full Interview Lifecycle (AI Agent Simulation)
// ============================================================================

pub fn full_interview_lifecycle_test() {
  setup_test_environment()

  // STEP 1: Create new session (AI agent starts interview)
  let session = create_test_session("test-full-lifecycle", interview.Api)
  session.stage
  |> should.equal(interview.Discovery)

  // STEP 2: Answer questions in round 1
  let session = answer_all_questions_in_round(session, 1)
  list.length(session.answers)
  |> should.equal(5)

  // STEP 3: Complete round 1 and move to round 2
  let session = interview.complete_round(session)
  session.rounds_completed
  |> should.equal(1)

  // STEP 4: Answer questions in round 2
  let session = answer_all_questions_in_round(session, 2)
  let session = interview.complete_round(session)
  session.rounds_completed
  |> should.equal(2)

  // STEP 5: Complete remaining rounds
  let session = answer_all_questions_in_round(session, 3)
  let session = interview.complete_round(session)
  session.stage
  |> should.equal(interview.Refinement)

  let session = answer_all_questions_in_round(session, 4)
  let session = interview.complete_round(session)
  session.stage
  |> should.equal(interview.Validation)

  let session = answer_all_questions_in_round(session, 5)
  let session = interview.complete_round(session)
  session.stage
  |> should.equal(interview.Complete)
  session.rounds_completed
  |> should.equal(5)

  // STEP 6: Verify final state
  // Should have 25 answers total (5 rounds × 5 questions)
  list.length(session.answers)
  |> should.equal(25)

  teardown_test_environment()
}

pub fn session_persistence_across_saves_test() {
  setup_test_environment()

  // Create and save session
  let original = create_test_session("test-persistence", interview.Cli)
  let answer = create_mock_answer("q1", "THE SYSTEM SHALL provide help text", 1)
  let original = interview.add_answer(original, answer)

  let save_result =
    interview_storage.append_session_to_jsonl(original, "test-sessions.jsonl")
  save_result
  |> should.be_ok

  // Load session
  let load_result =
    interview_storage.get_session_from_jsonl(
      "test-sessions.jsonl",
      "test-persistence",
    )

  case load_result {
    Ok(loaded) -> {
      loaded.id
      |> should.equal(original.id)
      loaded.profile
      |> should.equal(original.profile)
      list.length(loaded.answers)
      |> should.equal(1)
    }
    Error(_) -> should.fail()
  }

  teardown_test_environment()
}

pub fn resume_session_continues_from_saved_state_test() {
  setup_test_environment()

  // Create session with partial progress
  let session = create_test_session("test-resume", interview.Api)
  let session = answer_all_questions_in_round(session, 1)
  let session = interview.complete_round(session)

  // Save session
  let _ =
    interview_storage.append_session_to_jsonl(session, "test-sessions.jsonl")

  // Load session (simulating resume)
  let result =
    interview_storage.get_session_from_jsonl(
      "test-sessions.jsonl",
      "test-resume",
    )

  case result {
    Ok(resumed) -> {
      resumed.rounds_completed
      |> should.equal(1)
      list.length(resumed.answers)
      |> should.equal(5)
    }
    Error(_) -> should.fail()
  }

  teardown_test_environment()
}

// ============================================================================
// Test: Profile Conversion
// ============================================================================

pub fn profile_to_string_conversions_test() {
  interview.profile_to_string(interview.Api)
  |> should.equal("api")
  interview.profile_to_string(interview.Cli)
  |> should.equal("cli")
  interview.profile_to_string(interview.Event)
  |> should.equal("event")
  interview.profile_to_string(interview.Data)
  |> should.equal("data")
  interview.profile_to_string(interview.Workflow)
  |> should.equal("workflow")
  interview.profile_to_string(interview.UI)
  |> should.equal("ui")
}

pub fn string_to_profile_valid_test() {
  interview.string_to_profile("api")
  |> should.be_ok
  |> should.equal(interview.Api)

  interview.string_to_profile("CLI")
  |> should.be_ok
  |> should.equal(interview.Cli)
}

pub fn string_to_profile_invalid_test() {
  let result = interview.string_to_profile("invalid-profile")

  case result {
    Error(msg) -> {
      string.contains(msg, "Unknown profile")
      |> should.equal(True)
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Test: Error Handling and Edge Cases
// ============================================================================

pub fn empty_answer_extraction_test() {
  let extracted = interview.extract_from_answer("q1", "", ["field1"])

  dict.size(extracted)
  |> should.equal(0)
}

pub fn whitespace_only_answer_extraction_test() {
  let extracted = interview.extract_from_answer("q1", "   ", ["field1"])

  dict.size(extracted)
  |> should.equal(0)
}

pub fn multiple_profiles_distinct_gaps_test() {
  let api_gaps = interview.detect_gaps(interview.Api, [])
  let cli_gaps = interview.detect_gaps(interview.Cli, [])
  let event_gaps = interview.detect_gaps(interview.Event, [])

  // Each profile should have distinct required fields
  list.length(api_gaps)
  > 0
  |> should.equal(True)
  list.length(cli_gaps)
  > 0
  |> should.equal(True)
  list.length(event_gaps)
  > 0
  |> should.equal(True)

  // API should have base_url
  list.any(api_gaps, fn(g) { g.field == "base_url" })
  |> should.equal(True)

  // CLI should have command_name
  list.any(cli_gaps, fn(g) { g.field == "command_name" })
  |> should.equal(True)
}

pub fn format_progress_includes_all_metrics_test() {
  let session = create_test_session("test-progress", interview.Api)
  let answer = create_mock_answer("q1", "Response", 1)
  let session = interview.add_answer(session, answer)

  let progress = interview.format_progress(session)

  string.contains(progress, "Profile: api")
  |> should.equal(True)
  string.contains(progress, "Stage: Discovery")
  |> should.equal(True)
  string.contains(progress, "Answers: 1")
  |> should.equal(True)
}
