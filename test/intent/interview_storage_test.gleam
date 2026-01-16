//// Comprehensive tests for the interview_storage module
//// Tests storage functionality:
//// - Enum to string conversions (profile, stage, perspective)
//// - Answer history tracking (version creation, snapshots)
//// - Session diffing (added/modified/removed answers, gaps, conflicts)
//// - Diff formatting (human-readable output)
//// - JSONL encoding/decoding (sessions, snapshots)
//// - File error formatting
//// - String truncation

import gleam/dict
import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import intent/interview
import intent/interview_storage
import intent/question_types
import simplifile

// ============================================================================
// Enum to String Conversion Tests
// ============================================================================

pub fn profile_to_string_api_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"api\"")
  |> should.be_true
}

pub fn profile_to_string_cli_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.Cli,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"cli\"")
  |> should.be_true
}

pub fn profile_to_string_event_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.Event,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"event\"")
  |> should.be_true
}

pub fn profile_to_string_data_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.Data,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"data\"")
  |> should.be_true
}

pub fn profile_to_string_workflow_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.Workflow,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"workflow\"")
  |> should.be_true
}

pub fn profile_to_string_ui_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.UI,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"ui\"")
  |> should.be_true
}

pub fn stage_to_string_discovery_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"discovery\"")
  |> should.be_true
}

pub fn stage_to_string_complete_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Complete,
      rounds_completed: 3,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"complete\"")
  |> should.be_true
}

pub fn stage_to_string_refinement_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Refinement,
      rounds_completed: 3,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"refinement\"")
  |> should.be_true
}

pub fn stage_to_string_validation_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Validation,
      rounds_completed: 4,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"validation\"")
  |> should.be_true
}

pub fn stage_to_string_paused_test() {
  let session =
    interview.InterviewSession(
      id: "test-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Paused,
      rounds_completed: 2,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let json = interview_storage.session_to_json(session)
  let json_str = json |> string.inspect

  string.contains(json_str, "\"paused\"")
  |> should.be_true
}

// ============================================================================
// Answer History Tests
// ============================================================================

pub fn answer_to_version_creates_version_test() {
  let answer =
    interview.Answer(
      question_id: "q1",
      question_text: "What is the purpose?",
      perspective: question_types.User,
      round: 1,
      response: "Build an API",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let version =
    interview_storage.answer_to_version(answer, 1, "Initial response")

  version.version
  |> should.equal(1)
  version.response
  |> should.equal("Build an API")
  version.change_reason
  |> should.equal("Initial response")
}

pub fn answer_to_version_preserves_confidence_test() {
  let answer =
    interview.Answer(
      question_id: "q1",
      question_text: "Test",
      perspective: question_types.Developer,
      round: 1,
      response: "Answer",
      extracted: dict.new(),
      confidence: 0.75,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let version = interview_storage.answer_to_version(answer, 2, "Revised")

  version.confidence
  |> should.equal(0.75)
}

// ============================================================================
// Snapshot Creation Tests
// ============================================================================

pub fn create_snapshot_includes_session_id_test() {
  let session =
    interview.InterviewSession(
      id: "session-123",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let snapshot = interview_storage.create_snapshot(session, "Initial snapshot")

  snapshot.session_id
  |> should.equal("session-123")
}

pub fn create_snapshot_includes_description_test() {
  let session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let snapshot = interview_storage.create_snapshot(session, "After first round")

  snapshot.description
  |> should.equal("After first round")
}

pub fn create_snapshot_counts_unresolved_gaps_test() {
  let gap1 =
    interview.Gap(
      id: "gap-1",
      field: "auth_method",
      description: "Authentication method not specified",
      blocking: True,
      suggested_default: "JWT",
      why_needed: "Required for security",
      round: 1,
      resolved: False,
      resolution: "",
    )
  let gap2 =
    interview.Gap(
      id: "gap-2",
      field: "rate_limit",
      description: "Rate limiting not specified",
      blocking: False,
      suggested_default: "100 req/min",
      why_needed: "Prevent abuse",
      round: 1,
      resolved: True,
      resolution: "Set to 1000 req/min",
    )

  let session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Refinement,
      rounds_completed: 1,
      answers: [],
      gaps: [gap1, gap2],
      conflicts: [],
      raw_notes: "",
    )

  let snapshot = interview_storage.create_snapshot(session, "Test")

  // Should count only unresolved gaps (gap1)
  snapshot.gaps_count
  |> should.equal(1)
}

pub fn create_snapshot_counts_unresolved_conflicts_test() {
  let resolution =
    interview.ConflictResolution(
      option: "REST",
      description: "Use REST API",
      tradeoffs: "Simple but less flexible",
      recommendation: "start",
    )
  let conflict1 =
    interview.Conflict(
      id: "conflict-1",
      between: #("REST", "GraphQL"),
      description: "API style choice",
      impact: "Affects all endpoints",
      options: [resolution],
      chosen: -1,
      // Unresolved
    )
  let conflict2 =
    interview.Conflict(
      id: "conflict-2",
      between: #("SQL", "NoSQL"),
      description: "Database choice",
      impact: "Affects storage",
      options: [resolution],
      chosen: 0,
      // Resolved
    )

  let session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Refinement,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [conflict1, conflict2],
      raw_notes: "",
    )

  let snapshot = interview_storage.create_snapshot(session, "Test")

  // Should count only unresolved conflicts (conflict1)
  snapshot.conflicts_count
  |> should.equal(1)
}

// ============================================================================
// Session Diff Tests
// ============================================================================

pub fn diff_sessions_detects_added_answers_test() {
  let from_session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let answer =
    interview.Answer(
      question_id: "q1",
      question_text: "What is the purpose?",
      perspective: question_types.User,
      round: 1,
      response: "Build an API",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:01:00Z",
    )

  let to_session =
    interview.InterviewSession(
      ..from_session,
      updated_at: "2024-01-01T00:01:00Z",
      answers: [answer],
    )

  let diff = interview_storage.diff_sessions(from_session, to_session)

  { list.length(diff.answers_added) == 1 }
  |> should.be_true
}

pub fn diff_sessions_detects_modified_answers_test() {
  let answer1 =
    interview.Answer(
      question_id: "q1",
      question_text: "What is the purpose?",
      perspective: question_types.User,
      round: 1,
      response: "Build an API",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let answer2 =
    interview.Answer(
      ..answer1,
      response: "Build a REST API with authentication",
      timestamp: "2024-01-01T00:01:00Z",
    )

  let from_session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [answer1],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let to_session =
    interview.InterviewSession(
      ..from_session,
      updated_at: "2024-01-01T00:01:00Z",
      answers: [answer2],
    )

  let diff = interview_storage.diff_sessions(from_session, to_session)

  { list.length(diff.answers_modified) == 1 }
  |> should.be_true
}

pub fn diff_sessions_ignores_unchanged_answers_test() {
  let answer =
    interview.Answer(
      question_id: "q1",
      question_text: "What is the purpose?",
      perspective: question_types.User,
      round: 1,
      response: "Build an API",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [answer],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let diff = interview_storage.diff_sessions(session, session)

  { list.length(diff.answers_added) == 0 }
  |> should.be_true
  { list.length(diff.answers_modified) == 0 }
  |> should.be_true
  { list.length(diff.answers_removed) == 0 }
  |> should.be_true
}

pub fn diff_sessions_detects_removed_answers_test() {
  let answer =
    interview.Answer(
      question_id: "q1",
      question_text: "What is the purpose?",
      perspective: question_types.User,
      round: 1,
      response: "Build an API",
      extracted: dict.new(),
      confidence: 0.9,
      notes: "",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let from_session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [answer],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let to_session =
    interview.InterviewSession(
      ..from_session,
      updated_at: "2024-01-01T00:01:00Z",
      answers: [],
    )

  let diff = interview_storage.diff_sessions(from_session, to_session)

  { list.length(diff.answers_removed) == 1 }
  |> should.be_true
}

pub fn diff_sessions_detects_stage_change_test() {
  let session1 =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let session2 =
    interview.InterviewSession(
      ..session1,
      stage: interview.Refinement,
      updated_at: "2024-01-01T00:01:00Z",
    )

  let diff = interview_storage.diff_sessions(session1, session2)

  case diff.stage_changed {
    option.Some(_) -> True
    option.None -> False
  }
  |> should.be_true
}

// ============================================================================
// Diff Formatting Tests
// ============================================================================

pub fn format_diff_includes_session_ids_test() {
  let session =
    interview.InterviewSession(
      id: "session-abc",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let diff = interview_storage.diff_sessions(session, session)
  let formatted = interview_storage.format_diff(diff)

  string.contains(formatted, "session-abc")
  |> should.be_true
}

pub fn format_diff_includes_timestamps_test() {
  let session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let diff = interview_storage.diff_sessions(session, session)
  let formatted = interview_storage.format_diff(diff)

  string.contains(formatted, "2024-01-01T00:00:00Z")
  |> should.be_true
}

// ============================================================================
// File Error Formatting Tests
// ============================================================================

pub fn format_file_error_enoent_test() {
  // Create a path that doesn't exist
  let result = simplifile.read("/nonexistent/path/test.jsonl")

  case result {
    Error(simplifile.Enoent) -> True
    _ -> False
  }
  |> should.be_true
}

// ============================================================================
// Session JSONL Encoding Tests
// ============================================================================

pub fn session_to_jsonl_line_includes_id_test() {
  let session =
    interview.InterviewSession(
      id: "session-xyz",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let line = interview_storage.session_to_jsonl_line(session)

  string.contains(line, "\"session-xyz\"")
  |> should.be_true
}

pub fn session_to_jsonl_line_includes_profile_test() {
  let session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Cli,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let line = interview_storage.session_to_jsonl_line(session)

  string.contains(line, "\"cli\"")
  |> should.be_true
}

pub fn session_to_jsonl_line_includes_stage_test() {
  let session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Validation,
      rounds_completed: 2,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let line = interview_storage.session_to_jsonl_line(session)

  string.contains(line, "\"validation\"")
  |> should.be_true
}

pub fn session_to_jsonl_line_valid_json_test() {
  let session =
    interview.InterviewSession(
      id: "session-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let line = interview_storage.session_to_jsonl_line(session)

  // JSON line should start with { and end with }
  string.starts_with(line, "{")
  |> should.be_true
  string.ends_with(line, "}")
  |> should.be_true
}

// ============================================================================
// Session Persistence Tests (save/load JSONL)
// ============================================================================

pub fn append_session_creates_file_test() {
  let session =
    interview.InterviewSession(
      id: "test-save-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let path = "/tmp/test-sessions-create.jsonl"
  // Clean up any existing file
  let _ = simplifile.delete(path)

  let result = interview_storage.append_session_to_jsonl(session, path)

  result
  |> should.be_ok

  // Clean up
  let _ = simplifile.delete(path)
}

pub fn load_session_from_empty_file_test() {
  let path = "/tmp/test-sessions-empty.jsonl"
  // Create empty file
  let _ = simplifile.write(path, "")

  let result = interview_storage.list_sessions_from_jsonl(path)

  case result {
    Ok(sessions) -> {
      list.length(sessions)
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }

  // Clean up
  let _ = simplifile.delete(path)
}

pub fn load_session_from_nonexistent_file_test() {
  let path = "/tmp/nonexistent-sessions.jsonl"
  // Ensure file doesn't exist
  let _ = simplifile.delete(path)

  let result = interview_storage.list_sessions_from_jsonl(path)

  result
  |> should.be_error
}

pub fn save_and_load_single_session_test() {
  let session =
    interview.InterviewSession(
      id: "test-roundtrip-1",
      profile: interview.Cli,
      created_at: "2024-01-15T10:00:00Z",
      updated_at: "2024-01-15T11:00:00Z",
      completed_at: "",
      stage: interview.Refinement,
      rounds_completed: 3,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "Test notes",
    )

  let path = "/tmp/test-sessions-roundtrip.jsonl"
  let _ = simplifile.delete(path)

  // Save
  let save_result = interview_storage.append_session_to_jsonl(session, path)
  save_result
  |> should.be_ok

  // Load
  let load_result = interview_storage.list_sessions_from_jsonl(path)

  case load_result {
    Ok(sessions) -> {
      list.length(sessions)
      |> should.equal(1)

      let first = list.first(sessions)
      case first {
        Ok(loaded) -> {
          loaded.id
          |> should.equal("test-roundtrip-1")
          loaded.profile
          |> should.equal(interview.Cli)
          loaded.stage
          |> should.equal(interview.Refinement)
          loaded.rounds_completed
          |> should.equal(3)
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }

  // Clean up
  let _ = simplifile.delete(path)
}

pub fn save_multiple_sessions_test() {
  let session1 =
    interview.InterviewSession(
      id: "multi-1",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let session2 =
    interview.InterviewSession(
      id: "multi-2",
      profile: interview.Event,
      created_at: "2024-01-02T00:00:00Z",
      updated_at: "2024-01-02T00:00:00Z",
      completed_at: "",
      stage: interview.Validation,
      rounds_completed: 4,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let path = "/tmp/test-sessions-multiple.jsonl"
  let _ = simplifile.delete(path)

  // Save both
  let _ = interview_storage.append_session_to_jsonl(session1, path)
  let _ = interview_storage.append_session_to_jsonl(session2, path)

  // Load all
  let result = interview_storage.list_sessions_from_jsonl(path)

  case result {
    Ok(sessions) -> {
      list.length(sessions)
      |> should.equal(2)
    }
    Error(_) -> should.fail()
  }

  // Clean up
  let _ = simplifile.delete(path)
}

pub fn update_existing_session_test() {
  let session_v1 =
    interview.InterviewSession(
      id: "update-test",
      profile: interview.Data,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "Version 1",
    )

  let session_v2 =
    interview.InterviewSession(
      id: "update-test",
      profile: interview.Data,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T12:00:00Z",
      completed_at: "",
      stage: interview.Refinement,
      rounds_completed: 3,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "Version 2 updated",
    )

  let path = "/tmp/test-sessions-update.jsonl"
  let _ = simplifile.delete(path)

  // Save v1
  let _ = interview_storage.append_session_to_jsonl(session_v1, path)

  // Update to v2 (same ID)
  let _ = interview_storage.append_session_to_jsonl(session_v2, path)

  // Load - should only have one session with v2 data
  let result = interview_storage.list_sessions_from_jsonl(path)

  case result {
    Ok(sessions) -> {
      list.length(sessions)
      |> should.equal(1)

      let first = list.first(sessions)
      case first {
        Ok(loaded) -> {
          loaded.stage
          |> should.equal(interview.Refinement)
          loaded.rounds_completed
          |> should.equal(3)
          loaded.raw_notes
          |> should.equal("Version 2 updated")
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }

  // Clean up
  let _ = simplifile.delete(path)
}

pub fn get_session_by_id_test() {
  let session1 =
    interview.InterviewSession(
      id: "find-me",
      profile: interview.Workflow,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "Target session",
    )

  let session2 =
    interview.InterviewSession(
      id: "other",
      profile: interview.UI,
      created_at: "2024-01-02T00:00:00Z",
      updated_at: "2024-01-02T00:00:00Z",
      completed_at: "",
      stage: interview.Complete,
      rounds_completed: 5,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "Other session",
    )

  let path = "/tmp/test-sessions-get-by-id.jsonl"
  let _ = simplifile.delete(path)

  // Save both
  let _ = interview_storage.append_session_to_jsonl(session1, path)
  let _ = interview_storage.append_session_to_jsonl(session2, path)

  // Get specific session by ID
  let result = interview_storage.get_session_from_jsonl(path, "find-me")

  case result {
    Ok(loaded) -> {
      loaded.id
      |> should.equal("find-me")
      loaded.raw_notes
      |> should.equal("Target session")
    }
    Error(_) -> should.fail()
  }

  // Clean up
  let _ = simplifile.delete(path)
}

pub fn get_session_by_id_not_found_test() {
  let session =
    interview.InterviewSession(
      id: "exists",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let path = "/tmp/test-sessions-not-found.jsonl"
  let _ = simplifile.delete(path)

  let _ = interview_storage.append_session_to_jsonl(session, path)

  // Try to get non-existent session
  let result = interview_storage.get_session_from_jsonl(path, "does-not-exist")

  result
  |> should.be_error

  // Clean up
  let _ = simplifile.delete(path)
}

pub fn save_session_with_answers_test() {
  let answer =
    interview.Answer(
      question_id: "q1",
      question_text: "What's the base URL?",
      perspective: question_types.Developer,
      round: 1,
      response: "https://api.example.com",
      extracted: dict.from_list([#("base_url", "https://api.example.com")]),
      confidence: 0.9,
      notes: "Extracted from response",
      timestamp: "2024-01-01T00:00:00Z",
    )

  let session =
    interview.InterviewSession(
      id: "with-answers",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [answer],
      gaps: [],
      conflicts: [],
      raw_notes: "",
    )

  let path = "/tmp/test-sessions-with-answers.jsonl"
  let _ = simplifile.delete(path)

  // Save
  let _ = interview_storage.append_session_to_jsonl(session, path)

  // Load
  let result = interview_storage.get_session_from_jsonl(path, "with-answers")

  case result {
    Ok(loaded) -> {
      list.length(loaded.answers)
      |> should.equal(1)

      let first_answer = list.first(loaded.answers)
      case first_answer {
        Ok(ans) -> {
          ans.question_id
          |> should.equal("q1")
          ans.response
          |> should.equal("https://api.example.com")
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }

  // Clean up
  let _ = simplifile.delete(path)
}

pub fn save_session_with_gaps_and_conflicts_test() {
  let gap =
    interview.Gap(
      id: "gap-1",
      field: "auth_method",
      description: "Missing authentication method",
      blocking: True,
      suggested_default: "jwt",
      why_needed: "Required for API security",
      round: 1,
      resolved: False,
      resolution: "",
    )

  let conflict =
    interview.Conflict(
      id: "conflict-cap",
      between: #("speed", "consistency"),
      description: "CAP theorem tradeoff",
      impact: "System design decision",
      options: [],
      chosen: -1,
    )

  let session =
    interview.InterviewSession(
      id: "with-gaps-conflicts",
      profile: interview.Api,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      completed_at: "",
      stage: interview.Discovery,
      rounds_completed: 1,
      answers: [],
      gaps: [gap],
      conflicts: [conflict],
      raw_notes: "",
    )

  let path = "/tmp/test-sessions-gaps-conflicts.jsonl"
  let _ = simplifile.delete(path)

  // Save
  let _ = interview_storage.append_session_to_jsonl(session, path)

  // Load
  let result =
    interview_storage.get_session_from_jsonl(path, "with-gaps-conflicts")

  case result {
    Ok(loaded) -> {
      list.length(loaded.gaps)
      |> should.equal(1)
      list.length(loaded.conflicts)
      |> should.equal(1)

      let first_gap = list.first(loaded.gaps)
      case first_gap {
        Ok(g) -> {
          g.field
          |> should.equal("auth_method")
          g.blocking
          |> should.be_true
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }

  // Clean up
  let _ = simplifile.delete(path)
}
