//// Comprehensive tests for the interview module
//// Tests structured interrogation system for discovering and refining specifications:
//// - Profile conversion (string <-> Profile type)
//// - Session creation and management
//// - Answer extraction (auth methods, entities, audience)
//// - Gap detection and resolution
//// - Conflict detection and resolution
//// - Confidence calculation
//// - Question formatting
//// - Round management
//// - Progress tracking

import gleam/dict
import gleam/list
import gleam/string
import gleeunit/should
import intent/interview
import intent/question_types

// ============================================================================
// Test Fixtures
// ============================================================================

fn make_test_answer(
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

fn make_test_question(
  id: String,
  priority: question_types.QuestionPriority,
) -> question_types.Question {
  question_types.Question(
    id: id,
    round: 1,
    perspective: question_types.Developer,
    category: question_types.HappyPath,
    priority: priority,
    question: "Test question?",
    context: "Context",
    example: "Example",
    expected_type: "string",
    extract_into: [],
    depends_on: [],
    blocks: [],
  )
}

// ============================================================================
// Profile Conversion Tests
// ============================================================================

pub fn profile_to_string_api_test() {
  interview.profile_to_string(interview.Api)
  |> should.equal("api")
}

pub fn profile_to_string_cli_test() {
  interview.profile_to_string(interview.Cli)
  |> should.equal("cli")
}

pub fn profile_to_string_event_test() {
  interview.profile_to_string(interview.Event)
  |> should.equal("event")
}

pub fn profile_to_string_data_test() {
  interview.profile_to_string(interview.Data)
  |> should.equal("data")
}

pub fn profile_to_string_workflow_test() {
  interview.profile_to_string(interview.Workflow)
  |> should.equal("workflow")
}

pub fn profile_to_string_ui_test() {
  interview.profile_to_string(interview.UI)
  |> should.equal("ui")
}

pub fn string_to_profile_api_test() {
  interview.string_to_profile("api")
  |> should.be_ok
  |> should.equal(interview.Api)
}

pub fn string_to_profile_cli_test() {
  interview.string_to_profile("cli")
  |> should.be_ok
  |> should.equal(interview.Cli)
}

pub fn string_to_profile_event_test() {
  interview.string_to_profile("event")
  |> should.be_ok
  |> should.equal(interview.Event)
}

pub fn string_to_profile_data_test() {
  interview.string_to_profile("data")
  |> should.be_ok
  |> should.equal(interview.Data)
}

pub fn string_to_profile_workflow_test() {
  interview.string_to_profile("workflow")
  |> should.be_ok
  |> should.equal(interview.Workflow)
}

pub fn string_to_profile_ui_test() {
  interview.string_to_profile("ui")
  |> should.be_ok
  |> should.equal(interview.UI)
}

pub fn string_to_profile_case_insensitive_test() {
  interview.string_to_profile("API")
  |> should.be_ok
  |> should.equal(interview.Api)
}

pub fn string_to_profile_unknown_test() {
  case interview.string_to_profile("unknown") {
    Error(msg) -> {
      string.contains(msg, "Unknown profile")
      |> should.equal(True)
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Session Creation Tests
// ============================================================================

pub fn create_session_test() {
  let session = interview.create_session("test-id", interview.Api, "2024-01-01")

  session.id
  |> should.equal("test-id")
  session.profile
  |> should.equal(interview.Api)
  session.created_at
  |> should.equal("2024-01-01")
  session.stage
  |> should.equal(interview.Discovery)
  list.length(session.answers)
  |> should.equal(0)
}

pub fn create_session_cli_profile_test() {
  let session =
    interview.create_session("test-cli", interview.Cli, "2024-01-01")

  session.profile
  |> should.equal(interview.Cli)
  session.stage
  |> should.equal(interview.Discovery)
}

pub fn create_session_event_profile_test() {
  let session =
    interview.create_session("test-event", interview.Event, "2024-01-01")

  session.profile
  |> should.equal(interview.Event)
  session.stage
  |> should.equal(interview.Discovery)
}

pub fn create_session_data_profile_test() {
  let session =
    interview.create_session("test-data", interview.Data, "2024-01-01")

  session.profile
  |> should.equal(interview.Data)
  session.stage
  |> should.equal(interview.Discovery)
}

pub fn create_session_workflow_profile_test() {
  let session =
    interview.create_session("test-workflow", interview.Workflow, "2024-01-01")

  session.profile
  |> should.equal(interview.Workflow)
  session.stage
  |> should.equal(interview.Discovery)
}

pub fn create_session_ui_profile_test() {
  let session = interview.create_session("test-ui", interview.UI, "2024-01-01")

  session.profile
  |> should.equal(interview.UI)
  session.stage
  |> should.equal(interview.Discovery)
}

pub fn add_answer_test() {
  let session = interview.create_session("test-id", interview.Api, "2024-01-01")
  let answer = make_test_answer("q1", "Test response", 1)

  let updated = interview.add_answer(session, answer)

  list.length(updated.answers)
  |> should.equal(1)
  updated.updated_at
  |> should.equal(answer.timestamp)
}

// ============================================================================
// Extract Auth Method Tests
// ============================================================================

pub fn extract_from_answer_auth_jwt_test() {
  let result =
    interview.extract_from_answer("q1", "We'll use JWT tokens", ["auth_method"])

  case dict.get(result, "auth_method") {
    Ok(value) ->
      value
      |> should.equal("jwt")
    Error(_) -> should.fail()
  }
}

pub fn extract_from_answer_auth_oauth_test() {
  let result =
    interview.extract_from_answer("q1", "OAuth 2.0 authentication", [
      "auth_method",
    ])

  case dict.get(result, "auth_method") {
    Ok(value) ->
      value
      |> should.equal("oauth")
    Error(_) -> should.fail()
  }
}

pub fn extract_from_answer_auth_session_test() {
  let result =
    interview.extract_from_answer("q1", "Session-based auth", ["auth_method"])

  case dict.get(result, "auth_method") {
    Ok(value) ->
      value
      |> should.equal("session")
    Error(_) -> should.fail()
  }
}

pub fn extract_from_answer_auth_api_key_test() {
  let result =
    interview.extract_from_answer("q1", "API key in header", ["auth_method"])

  case dict.get(result, "auth_method") {
    Ok(value) ->
      value
      |> should.equal("api_key")
    Error(_) -> should.fail()
  }
}

pub fn extract_from_answer_auth_none_test() {
  let result =
    interview.extract_from_answer("q1", "Authentication: none", ["auth_method"])

  case dict.get(result, "auth_method") {
    Ok(value) ->
      value
      |> should.equal("none")
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Extract Entities Tests
// ============================================================================

pub fn extract_from_answer_entities_test() {
  let result =
    interview.extract_from_answer(
      "q1",
      "We have User, Order, Product entities",
      ["entities"],
    )

  case dict.get(result, "entities") {
    Ok(value) -> {
      string.contains(value, "User")
      |> should.equal(True)
      string.contains(value, "Order")
      |> should.equal(True)
      string.contains(value, "Product")
      |> should.equal(True)
    }
    Error(_) -> should.fail()
  }
}

pub fn extract_from_answer_entities_with_comma_test() {
  let result =
    interview.extract_from_answer("q1", "Main entities: Customer, Invoice", [
      "entities",
    ])

  case dict.get(result, "entities") {
    Ok(value) -> {
      string.contains(value, "Customer")
      |> should.equal(True)
      string.contains(value, "Invoice")
      |> should.equal(True)
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Extract Audience Tests
// ============================================================================

pub fn extract_from_answer_audience_mobile_test() {
  let result =
    interview.extract_from_answer("q1", "Mobile app users", ["audience"])

  case dict.get(result, "audience") {
    Ok(value) ->
      value
      |> should.equal("mobile")
    Error(_) -> should.fail()
  }
}

pub fn extract_from_answer_audience_web_test() {
  let result =
    interview.extract_from_answer("q1", "Web interface", ["audience"])

  case dict.get(result, "audience") {
    Ok(value) ->
      value
      |> should.equal("web")
    Error(_) -> should.fail()
  }
}

pub fn extract_from_answer_audience_api_test() {
  let result =
    interview.extract_from_answer("q1", "API consumers", ["audience"])

  case dict.get(result, "audience") {
    Ok(value) ->
      value
      |> should.equal("api")
    Error(_) -> should.fail()
  }
}

pub fn extract_from_answer_audience_cli_test() {
  let result =
    interview.extract_from_answer("q1", "CLI tool users", ["audience"])

  case dict.get(result, "audience") {
    Ok(value) ->
      value
      |> should.equal("cli")
    Error(_) -> should.fail()
  }
}

pub fn extract_from_answer_audience_internal_test() {
  let result =
    interview.extract_from_answer("q1", "Internal teams only", ["audience"])

  case dict.get(result, "audience") {
    Ok(value) ->
      value
      |> should.equal("internal")
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Generic Extraction Tests
// ============================================================================

pub fn extract_from_answer_generic_field_test() {
  let result =
    interview.extract_from_answer("q1", "Some response text", ["unknown_field"])

  case dict.get(result, "unknown_field") {
    Ok(value) ->
      value
      |> should.equal("Some response text")
    Error(_) -> should.fail()
  }
}

pub fn extract_from_answer_empty_response_test() {
  let result = interview.extract_from_answer("q1", "", ["unknown_field"])

  dict.get(result, "unknown_field")
  |> should.be_error
}

// ============================================================================
// Gap Detection Tests
// ============================================================================

pub fn detect_gaps_api_profile_missing_base_url_test() {
  let gaps = interview.detect_gaps(interview.Api, [])

  let has_base_url_gap =
    list.any(gaps, fn(gap) { string.contains(gap.field, "base_url") })

  has_base_url_gap
  |> should.equal(True)
}

pub fn detect_gaps_api_profile_with_answers_test() {
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
      timestamp: "2024-01-01",
    )

  let gaps = interview.detect_gaps(interview.Api, [answer])

  let has_base_url_gap =
    list.any(gaps, fn(gap) { string.contains(gap.field, "base_url") })

  has_base_url_gap
  |> should.equal(False)
}

pub fn detect_gaps_cli_profile_test() {
  let gaps = interview.detect_gaps(interview.Cli, [])

  let has_command_name_gap =
    list.any(gaps, fn(gap) { string.contains(gap.field, "command_name") })

  has_command_name_gap
  |> should.equal(True)
}

// ============================================================================
// Conflict Detection Tests
// ============================================================================

pub fn detect_conflicts_cap_theorem_test() {
  let answers = [
    make_test_answer("q1", "We need fast response times with low latency", 1),
    make_test_answer("q2", "Data must be strongly consistent at all times", 1),
  ]

  let conflicts = interview.detect_conflicts(answers)

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
}

pub fn detect_conflicts_anonymous_audit_test() {
  let answers = [
    make_test_answer("q1", "Users should be completely anonymous", 1),
    make_test_answer("q2", "We need full audit trail of all actions", 1),
  ]

  let conflicts = interview.detect_conflicts(answers)

  list.length(conflicts)
  |> should.equal(1)

  let first = list.first(conflicts)
  case first {
    Ok(conflict) -> {
      string.contains(conflict.id, "anon-audit")
      |> should.equal(True)
    }
    Error(_) -> should.fail()
  }
}

pub fn detect_conflicts_no_conflicts_test() {
  let answers = [
    make_test_answer("q1", "Simple API design", 1),
    make_test_answer("q2", "Basic error handling", 1),
  ]

  let conflicts = interview.detect_conflicts(answers)

  list.length(conflicts)
  |> should.equal(0)
}

// ============================================================================
// Confidence Calculation Tests
// ============================================================================

pub fn calculate_confidence_high_test() {
  let response =
    "This is a detailed response with substantial information about the requirements"
  let extracted = dict.from_list([#("field1", "value1"), #("field2", "value2")])

  let confidence = interview.calculate_confidence("q1", response, extracted)

  confidence
  |> should.equal(0.85)
}

pub fn calculate_confidence_low_test() {
  let response = "Short"
  let extracted = dict.new()

  let confidence = interview.calculate_confidence("q1", response, extracted)

  confidence
  |> should.equal(0.6)
}

// ============================================================================
// Question Formatting Tests
// ============================================================================

pub fn format_question_critical_test() {
  let question = make_test_question("q1", question_types.Critical)

  let formatted = interview.format_question(question)

  string.contains(formatted, "[CRITICAL]")
  |> should.equal(True)
}

pub fn format_question_important_test() {
  let question = make_test_question("q1", question_types.Important)

  let formatted = interview.format_question(question)

  string.contains(formatted, "[IMPORTANT]")
  |> should.equal(True)
}

pub fn format_question_nice_to_have_test() {
  let question = make_test_question("q1", question_types.NiceTohave)

  let formatted = interview.format_question(question)

  string.contains(formatted, "[CRITICAL]")
  |> should.equal(False)
  string.contains(formatted, "[IMPORTANT]")
  |> should.equal(False)
}

pub fn format_question_includes_question_text_test() {
  let question = make_test_question("q1", question_types.Important)

  let formatted = interview.format_question(question)

  string.contains(formatted, "Test question?")
  |> should.equal(True)
}

// ============================================================================
// Round Management Tests
// ============================================================================

pub fn complete_round_first_round_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")

  let updated = interview.complete_round(session)

  updated.rounds_completed
  |> should.equal(1)
  updated.stage
  |> should.equal(interview.Discovery)
}

pub fn complete_round_third_round_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let session = interview.InterviewSession(..session, rounds_completed: 2)

  let updated = interview.complete_round(session)

  updated.rounds_completed
  |> should.equal(3)
  updated.stage
  |> should.equal(interview.Refinement)
}

pub fn complete_round_fourth_round_validation_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let session = interview.InterviewSession(..session, rounds_completed: 3)

  let updated = interview.complete_round(session)

  updated.rounds_completed
  |> should.equal(4)
  updated.stage
  |> should.equal(interview.Validation)
}

pub fn complete_round_fifth_round_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let session = interview.InterviewSession(..session, rounds_completed: 4)

  let updated = interview.complete_round(session)

  updated.rounds_completed
  |> should.equal(5)
  updated.stage
  |> should.equal(interview.Complete)
}

pub fn stage_paused_manually_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let paused = interview.InterviewSession(..session, stage: interview.Paused)

  paused.stage
  |> should.equal(interview.Paused)
}

pub fn get_current_round_no_answers_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")

  let current = interview.get_current_round(session)

  current
  |> should.equal(1)
}

pub fn get_current_round_with_answers_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let answer1 = make_test_answer("q1", "Response 1", 1)
  let answer2 = make_test_answer("q2", "Response 2", 2)
  let session = interview.add_answer(session, answer1)
  let session = interview.add_answer(session, answer2)

  let current = interview.get_current_round(session)

  // Should return 2 since we have answers in round 2
  current
  |> should.equal(2)
}

// ============================================================================
// Conflict Resolution Tests
// ============================================================================

pub fn resolve_conflict_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let conflict =
    interview.Conflict(
      id: "conflict-1",
      between: #("q1", "q2"),
      description: "Test conflict",
      impact: "Test impact",
      options: [],
      chosen: -1,
    )
  let session = interview.InterviewSession(..session, conflicts: [conflict])

  let result = interview.resolve_conflict(session, "conflict-1", 0)

  case result {
    Ok(updated) -> {
      let first = list.first(updated.conflicts)
      case first {
        Ok(c) ->
          c.chosen
          |> should.equal(0)
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Gap Resolution Tests
// ============================================================================

pub fn resolve_gap_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let gap =
    interview.Gap(
      id: "gap-1",
      field: "test_field",
      description: "Test gap",
      blocking: True,
      suggested_default: "",
      why_needed: "Required",
      round: 1,
      resolved: False,
      resolution: "",
    )
  let session = interview.InterviewSession(..session, gaps: [gap])

  let updated = interview.resolve_gap(session, "gap-1", "Resolved!")

  let first = list.first(updated.gaps)
  case first {
    Ok(g) -> {
      g.resolved
      |> should.equal(True)
      g.resolution
      |> should.equal("Resolved!")
    }
    Error(_) -> should.fail()
  }
}

pub fn get_blocking_gaps_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let gap1 =
    interview.Gap(
      id: "gap-1",
      field: "field1",
      description: "Blocking gap",
      blocking: True,
      suggested_default: "",
      why_needed: "Required",
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
}

pub fn get_unresolved_conflicts_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let conflict1 =
    interview.Conflict(
      id: "c1",
      between: #("q1", "q2"),
      description: "Unresolved",
      impact: "High",
      options: [],
      chosen: -1,
    )
  let conflict2 =
    interview.Conflict(
      id: "c2",
      between: #("q3", "q4"),
      description: "Resolved",
      impact: "Low",
      options: [],
      chosen: 0,
    )
  let session =
    interview.InterviewSession(..session, conflicts: [conflict1, conflict2])

  let unresolved = interview.get_unresolved_conflicts(session)

  list.length(unresolved)
  |> should.equal(1)
}

// ============================================================================
// Can Proceed Tests
// ============================================================================

pub fn can_proceed_no_gaps_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")

  interview.can_proceed(session)
  |> should.be_ok
}

pub fn can_proceed_with_blocking_gaps_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let gap =
    interview.Gap(
      id: "gap-1",
      field: "field1",
      description: "Blocking gap",
      blocking: True,
      suggested_default: "",
      why_needed: "Required",
      round: 1,
      resolved: False,
      resolution: "",
    )
  let session = interview.InterviewSession(..session, gaps: [gap])

  case interview.can_proceed(session) {
    Error(msg) -> {
      string.contains(msg, "Blocking gaps")
      |> should.equal(True)
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Progress Formatting Tests
// ============================================================================

pub fn format_progress_test() {
  let session = interview.create_session("test", interview.Api, "2024-01-01")
  let answer = make_test_answer("q1", "Response", 1)
  let session = interview.add_answer(session, answer)

  let progress = interview.format_progress(session)

  string.contains(progress, "Profile: api")
  |> should.equal(True)
  string.contains(progress, "Stage: Discovery")
  |> should.equal(True)
  string.contains(progress, "Answers: 1")
  |> should.equal(True)
}
