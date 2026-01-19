//// Tests for bead_feedback.gleam
//// Verifies pure JSON parsing logic can operate without file I/O
////
//// Key verification: parse_feedback_json and parse_feedback_content are 
//// pure functions that accept content as String, enabling testing without filesystem.

import gleam/option
import gleeunit/should
import intent/bead_feedback.{
  BeadError, BeadFeedback, Blocked, BlockedReason, Failed, Skipped, Success,
}

// =============================================================================
// TEST: parse_feedback_json - Pure function tests (no file I/O)
// =============================================================================

pub fn parse_feedback_json_empty_array_test() {
  // Empty array should return empty list
  let result = bead_feedback.parse_feedback_json("[]")

  should.be_ok(result)

  let feedback = case result {
    Ok(f) -> f
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(feedback, [])
}

pub fn parse_feedback_json_single_success_test() {
  // Single success feedback entry
  let json =
    "[{
    \"bead_id\": \"AUTH-001\",
    \"result\": \"success\",
    \"reason\": \"All checks passed\",
    \"executed_at\": \"2026-01-17T10:30:00Z\",
    \"duration_ms\": 150
  }]"

  let result = bead_feedback.parse_feedback_json(json)

  should.be_ok(result)

  let feedback = case result {
    Ok(f) -> f
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(feedback, [
    BeadFeedback(
      bead_id: "AUTH-001",
      result: Success,
      reason: "All checks passed",
      executed_at: "2026-01-17T10:30:00Z",
      duration_ms: 150,
      error: option.None,
      blocked_by: option.None,
    ),
  ])
}

pub fn parse_feedback_json_failed_with_error_test() {
  // Failed feedback with error details
  let json =
    "[{
    \"bead_id\": \"API-042\",
    \"result\": \"failed\",
    \"reason\": \"HTTP request failed\",
    \"executed_at\": \"2026-01-17T10:35:00Z\",
    \"duration_ms\": 2500,
    \"error\": {
      \"type\": \"http_error\",
      \"message\": \"Connection refused\",
      \"trace\": \"at connect:42\\nat request:108\"
    }
  }]"

  let result = bead_feedback.parse_feedback_json(json)

  should.be_ok(result)

  let feedback = case result {
    Ok(f) -> f
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(feedback, [
    BeadFeedback(
      bead_id: "API-042",
      result: Failed,
      reason: "HTTP request failed",
      executed_at: "2026-01-17T10:35:00Z",
      duration_ms: 2500,
      error: option.Some(BeadError(
        error_type: "http_error",
        message: "Connection refused",
        trace: option.Some("at connect:42\nat request:108"),
      )),
      blocked_by: option.None,
    ),
  ])
}

pub fn parse_feedback_json_blocked_with_reason_test() {
  // Blocked feedback with blocker details
  let json =
    "[{
    \"bead_id\": \"DATA-015\",
    \"result\": \"blocked\",
    \"reason\": \"Dependency not met\",
    \"executed_at\": \"2026-01-17T10:40:00Z\",
    \"duration_ms\": 0,
    \"blocked_by\": {
      \"type\": \"dependency\",
      \"details\": \"AUTH-001 must complete first\",
      \"unblocks_when\": \"AUTH-001 reaches success state\"
    }
  }]"

  let result = bead_feedback.parse_feedback_json(json)

  should.be_ok(result)

  let feedback = case result {
    Ok(f) -> f
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(feedback, [
    BeadFeedback(
      bead_id: "DATA-015",
      result: Blocked,
      reason: "Dependency not met",
      executed_at: "2026-01-17T10:40:00Z",
      duration_ms: 0,
      error: option.None,
      blocked_by: option.Some(BlockedReason(
        blocker_type: "dependency",
        details: "AUTH-001 must complete first",
        unblocks_when: "AUTH-001 reaches success state",
      )),
    ),
  ])
}

pub fn parse_feedback_json_multiple_entries_test() {
  // Multiple feedback entries of different types
  let json =
    "[
    {\"bead_id\": \"AUTH-001\", \"result\": \"success\", \"reason\": \"OK\", \"executed_at\": \"2026-01-17T10:30:00Z\", \"duration_ms\": 100},
    {\"bead_id\": \"AUTH-002\", \"result\": \"failed\", \"reason\": \"Error\", \"executed_at\": \"2026-01-17T10:31:00Z\", \"duration_ms\": 200},
    {\"bead_id\": \"AUTH-003\", \"result\": \"skipped\", \"reason\": \"N/A\", \"executed_at\": \"2026-01-17T10:32:00Z\", \"duration_ms\": 0}
  ]"

  let result = bead_feedback.parse_feedback_json(json)

  should.be_ok(result)

  let feedback = case result {
    Ok(f) -> f
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(3, case feedback {
    [_, _, _] -> 3
    _ -> 0
  })

  // Verify each result type
  let results = case feedback {
    [f1, f2, f3] -> [f1.result, f2.result, f3.result]
    _ -> []
  }

  should.equal(results, [Success, Failed, Skipped])
}

pub fn parse_feedback_json_invalid_json_test() {
  // Invalid JSON should return error
  let result = bead_feedback.parse_feedback_json("not valid json")

  should.be_error(result)
}

pub fn parse_feedback_json_missing_required_field_test() {
  // Missing required field should return error
  let json =
    "[{
    \"bead_id\": \"AUTH-001\",
    \"result\": \"success\"
  }]"
  // Missing reason, executed_at, duration_ms

  let result = bead_feedback.parse_feedback_json(json)

  should.be_error(result)
}

pub fn parse_feedback_json_invalid_result_value_test() {
  // Invalid result value should return error
  let json =
    "[{
    \"bead_id\": \"AUTH-001\",
    \"result\": \"invalid_status\",
    \"reason\": \"Test\",
    \"executed_at\": \"2026-01-17T10:30:00Z\",
    \"duration_ms\": 100
  }]"

  let result = bead_feedback.parse_feedback_json(json)

  should.be_error(result)
}

// =============================================================================
// TEST: parse_feedback_content - Auto-detection of JSON vs CUE
// =============================================================================

pub fn parse_feedback_content_json_array_test() {
  // Content starting with [ should be parsed as JSON
  let json =
    "[{\"bead_id\": \"TEST-001\", \"result\": \"success\", \"reason\": \"OK\", \"executed_at\": \"2026-01-17T10:30:00Z\", \"duration_ms\": 100}]"

  let result = bead_feedback.parse_feedback_content(json)

  should.be_ok(result)

  let feedback = case result {
    Ok(f) -> f
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(1, case feedback {
    [_] -> 1
    _ -> 0
  })
}

pub fn parse_feedback_content_cue_returns_empty_test() {
  // Raw CUE content should return empty list (needs cue export first)
  let cue_content =
    "{
  bead_id: \"TEST-001\"
  result: \"success\"
}"

  let result = bead_feedback.parse_feedback_content(cue_content)

  should.be_ok(result)

  let feedback = case result {
    Ok(f) -> f
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(feedback, [])
}

pub fn parse_feedback_content_empty_string_test() {
  // Empty string returns empty list
  let result = bead_feedback.parse_feedback_content("")

  should.be_ok(result)

  let feedback = case result {
    Ok(f) -> f
    Error(_) -> panic as "Expected Ok"
  }

  should.equal(feedback, [])
}

// =============================================================================
// TEST: feedback_file_path - Path generation
// =============================================================================

pub fn feedback_file_path_test() {
  let path = bead_feedback.feedback_file_path("session-abc123")
  should.equal(path, ".intent/feedback-session-abc123.cue")
}

pub fn feedback_file_path_with_hyphens_test() {
  let path = bead_feedback.feedback_file_path("my-session-id")
  should.equal(path, ".intent/feedback-my-session-id.cue")
}
