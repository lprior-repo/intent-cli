/// Tests for session_handoff module
/// Comprehensive tests covering handoff creation, serialization, and restoration
import gleam/dict
import gleam/list
import gleam/string
import gleeunit/should
import intent/interview.{
  type Answer, type Gap, type InterviewSession, Answer, Api, Discovery, Gap,
  InterviewSession, Paused,
}
import intent/question_types.{Developer}
import intent/session_handoff.{
  calculate_mental_model_indicators, create_handoff, extract_key_assumptions,
  handoff_to_jsonl_line, parse_handoff_lines,
}

// Test helpers

fn test_answer(question_id: String, response: String) -> Answer {
  Answer(
    question_id: question_id,
    question_text: "Test question: " <> question_id,
    perspective: Developer,
    round: 1,
    response: response,
    extracted: dict.new(),
    confidence: 0.85,
    notes: "Test note",
    timestamp: "2026-01-18T10:00:00Z",
  )
}

fn test_gap(id: String, resolved: Bool) -> Gap {
  Gap(
    id: id,
    field: "field_" <> id,
    description: "Test gap " <> id,
    blocking: True,
    suggested_default: "default_" <> id,
    why_needed: "For testing",
    round: 1,
    resolved: resolved,
    resolution: case resolved {
      True -> "Resolved by test"
      False -> ""
    },
  )
}

fn test_session(
  id: String,
  answers: List(Answer),
  gaps: List(Gap),
  rounds_completed: Int,
) -> InterviewSession {
  InterviewSession(
    id: id,
    profile: Api,
    created_at: "2026-01-18T08:00:00Z",
    updated_at: "2026-01-18T10:00:00Z",
    completed_at: "",
    stage: Discovery,
    rounds_completed: rounds_completed,
    answers: answers,
    gaps: gaps,
    conflicts: [],
    raw_notes: "Raw notes for testing",
  )
}

// =============================================================================
// Mental Model Indicator Tests
// =============================================================================

pub fn calculate_mental_model_indicators_shows_pending_for_new_session_test() {
  let session = test_session("sess1", [], [], 0)
  let indicators = calculate_mental_model_indicators(session)

  dict.get(indicators, "round_1_status")
  |> should.equal(Ok("Pending"))

  dict.get(indicators, "total_progress")
  |> should.equal(Ok("0/5"))
}

pub fn calculate_mental_model_indicators_shows_complete_for_finished_rounds_test() {
  let session = test_session("sess1", [], [], 2)
  let indicators = calculate_mental_model_indicators(session)

  dict.get(indicators, "round_1_status")
  |> should.equal(Ok("Complete"))

  dict.get(indicators, "round_2_status")
  |> should.equal(Ok("Complete"))

  dict.get(indicators, "round_3_status")
  |> should.equal(Ok("Pending"))

  dict.get(indicators, "total_progress")
  |> should.equal(Ok("2/5"))
}

pub fn calculate_mental_model_indicators_tracks_all_rounds_test() {
  let session = test_session("sess1", [], [], 5)
  let indicators = calculate_mental_model_indicators(session)

  dict.get(indicators, "round_1_status")
  |> should.equal(Ok("Complete"))

  dict.get(indicators, "round_5_status")
  |> should.equal(Ok("Complete"))

  dict.get(indicators, "total_progress")
  |> should.equal(Ok("5/5"))
}

// =============================================================================
// Key Assumptions Extraction Tests
// =============================================================================

pub fn extract_key_assumptions_includes_gaps_test() {
  let gaps = [test_gap("gap1", False)]
  let session = test_session("sess1", [], gaps, 0)

  let assumptions = extract_key_assumptions(session)

  assumptions
  |> list.any(fn(a) { string.contains(a, "gap1") })
  |> should.be_true()
}

pub fn extract_key_assumptions_includes_stage_test() {
  let session = test_session("sess1", [], [], 0)
  let assumptions = extract_key_assumptions(session)

  assumptions
  |> list.any(fn(a) { string.contains(a, "Discovery") })
  |> should.be_true()
}

pub fn extract_key_assumptions_for_paused_session_test() {
  let session =
    InterviewSession(..test_session("sess1", [], [], 2), stage: Paused)

  let assumptions = extract_key_assumptions(session)

  assumptions
  |> list.any(fn(a) { string.contains(a, "paused") })
  |> should.be_true()
}

// =============================================================================
// Handoff Creation Tests
// =============================================================================

pub fn create_handoff_captures_session_metadata_test() {
  let session = test_session("sess1", [test_answer("q1", "Answer")], [], 1)
  let handoff =
    create_handoff(
      session,
      "user_requested",
      "next_q_id",
      "2026-01-18T10:30:00Z",
      "{\"id\":\"sess1\"}",
    )

  handoff.session_id
  |> should.equal("sess1")

  handoff.pause_reason
  |> should.equal("user_requested")

  handoff.next_question_id
  |> should.equal("next_q_id")

  handoff.profile
  |> should.equal("api")
}

pub fn create_handoff_calculates_round_progress_test() {
  let session = test_session("sess1", [], [], 2)
  let handoff =
    create_handoff(session, "break", "q1", "2026-01-18T10:30:00Z", "{}")

  handoff.rounds_completed
  |> should.equal(2)

  handoff.mental_model_indicators
  |> dict.get("total_progress")
  |> should.equal(Ok("2/5"))
}

pub fn create_handoff_counts_unresolved_gaps_and_conflicts_test() {
  let gaps = [
    test_gap("gap1", False),
    test_gap("gap2", True),
    test_gap("gap3", False),
  ]
  let session = test_session("sess1", [], gaps, 1)

  let handoff =
    create_handoff(session, "pause", "q1", "2026-01-18T10:30:00Z", "{}")

  handoff.gaps_unresolved
  |> should.equal(2)
}

pub fn create_handoff_includes_session_jsonl_test() {
  let session = test_session("sess1", [], [], 0)
  let session_line = "{\"id\":\"sess1\",\"profile\":\"api\"}"
  let handoff =
    create_handoff(session, "pause", "q1", "2026-01-18T10:30:00Z", session_line)

  handoff.full_session_jsonl
  |> should.equal(session_line)
}

// =============================================================================
// Serialization Tests
// =============================================================================

pub fn handoff_to_jsonl_line_produces_valid_json_test() {
  let session = test_session("sess1", [], [], 1)
  let handoff =
    create_handoff(session, "test", "q1", "2026-01-18T10:30:00Z", "{}")

  let line = handoff_to_jsonl_line(handoff)

  line
  |> string.contains("session_id")
  |> should.be_true()

  line
  |> string.contains("sess1")
  |> should.be_true()
}

pub fn parse_handoff_lines_reconstructs_handoff_test() {
  let session = test_session("sess1", [], [], 1)
  let original_handoff =
    create_handoff(session, "test", "q1", "2026-01-18T10:30:00Z", "{}")

  let line = handoff_to_jsonl_line(original_handoff)
  let parsed = parse_handoff_lines([line])

  list.length(parsed)
  |> should.equal(1)

  case list.first(parsed) {
    Ok(h) ->
      h.session_id
      |> should.equal(original_handoff.session_id)
    Error(_) -> should.fail()
  }
}

pub fn parse_handoff_lines_handles_empty_input_test() {
  let parsed = parse_handoff_lines([])

  list.length(parsed)
  |> should.equal(0)
}

pub fn parse_handoff_lines_handles_blank_lines_test() {
  let parsed = parse_handoff_lines(["", "  ", "\n"])

  list.length(parsed)
  |> should.equal(0)
}

// =============================================================================
// Multiple Handoff Tests
// =============================================================================

pub fn parse_handoff_lines_handles_multiple_handoffs_test() {
  let session1 = test_session("sess1", [], [], 1)
  let session2 = test_session("sess2", [], [], 2)

  let handoff1 =
    create_handoff(session1, "test1", "q1", "2026-01-18T10:00:00Z", "{}")
  let handoff2 =
    create_handoff(session2, "test2", "q2", "2026-01-18T10:30:00Z", "{}")

  let lines = [handoff_to_jsonl_line(handoff1), handoff_to_jsonl_line(handoff2)]

  let parsed = parse_handoff_lines(lines)

  list.length(parsed)
  |> should.equal(2)
}

pub fn handoff_format_enables_efficient_lookup_test() {
  let session = test_session("sess1", [], [], 1)
  let handoff =
    create_handoff(session, "test", "next_q", "2026-01-18T10:30:00Z", "{}")

  // The format should include session_id for filtering
  let line = handoff_to_jsonl_line(handoff)

  line
  |> string.contains("\"session_id\":\"sess1\"")
  |> should.be_true()

  // The format should include handoff_id for uniqueness
  line
  |> string.contains("\"handoff_id\":\"sess1-handoff-")
  |> should.be_true()
}

// =============================================================================
// Context Preservation Tests
// =============================================================================

pub fn handoff_preserves_mental_model_state_test() {
  let session = test_session("sess1", [], [], 3)
  let handoff =
    create_handoff(session, "pause", "q1", "2026-01-18T10:30:00Z", "{}")

  // Mental model indicators should show progress
  dict.get(handoff.mental_model_indicators, "round_1_status")
  |> should.be_ok()

  dict.get(handoff.mental_model_indicators, "round_3_status")
  |> should.be_ok()

  // Total progress should be accurate
  dict.get(handoff.mental_model_indicators, "total_progress")
  |> should.equal(Ok("3/5"))
}

pub fn handoff_captures_questions_answered_count_test() {
  let answers = [
    test_answer("q1", "Answer 1"),
    test_answer("q2", "Answer 2"),
    test_answer("q3", "Answer 3"),
  ]
  let session = test_session("sess1", answers, [], 1)

  let handoff =
    create_handoff(session, "pause", "q4", "2026-01-18T10:30:00Z", "{}")

  handoff.questions_answered
  |> should.equal(3)

  handoff.next_question_id
  |> should.equal("q4")
}

pub fn handoff_includes_session_for_restoration_test() {
  let session = test_session("sess1", [test_answer("q1", "Response")], [], 1)
  let session_jsonl =
    "{\"id\":\"sess1\",\"answers\":[{\"question_id\":\"q1\"}]}"

  let handoff =
    create_handoff(
      session,
      "pause",
      "q2",
      "2026-01-18T10:30:00Z",
      session_jsonl,
    )

  // Full session should be embedded for seamless restoration
  handoff.full_session_jsonl
  |> string.contains("sess1")
  |> should.be_true()

  handoff.full_session_jsonl
  |> string.contains("q1")
  |> should.be_true()
}
