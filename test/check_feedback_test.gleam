//// Tests for check_feedback.gleam
//// Verifies conversion of check results to bead feedback for AI consumption.
////
//// All functions tested are pure (no I/O), making tests deterministic.

import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import intent/bead_feedback.{Failed, Success}
import intent/check_feedback
import intent/checker.{CheckFailed, CheckPassed, ResponseCheckResult}

// =============================================================================
// TEST: from_response_check_result - Main conversion function
// =============================================================================

pub fn from_response_check_result_all_passed_test() {
  // All checks pass, status matches
  let check_result =
    ResponseCheckResult(
      passed: [
        CheckPassed("id", "present"),
        CheckPassed("name", "string"),
      ],
      failed: [],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let feedback =
    check_feedback.from_response_check_result(
      "AUTH-001",
      check_result,
      "2026-01-17T10:30:00Z",
      150,
    )

  should.equal(feedback.bead_id, "AUTH-001")
  should.equal(feedback.result, Success)
  should.equal(feedback.executed_at, "2026-01-17T10:30:00Z")
  should.equal(feedback.duration_ms, 150)
  should.be_none(feedback.error)
  should.be_none(feedback.blocked_by)
}

pub fn from_response_check_result_status_mismatch_test() {
  // Status doesn't match (404 instead of 200)
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [],
      status_ok: False,
      status_expected: 200,
      status_actual: 404,
    )

  let feedback =
    check_feedback.from_response_check_result(
      "API-042",
      check_result,
      "2026-01-17T10:35:00Z",
      250,
    )

  should.equal(feedback.bead_id, "API-042")
  should.equal(feedback.result, Failed)
  should.be_some(feedback.error)

  // Verify error contains status mismatch info
  case feedback.error {
    option.Some(err) -> {
      should.equal(err.error_type, "status_mismatch")
      should.be_true(
        err.message
        |> has_substring("expected 200"),
      )
    }
    option.None -> panic as "Expected error to be Some"
  }
}

pub fn from_response_check_result_field_check_failed_test() {
  // Status matches but field check fails
  let check_result =
    ResponseCheckResult(
      passed: [CheckPassed("id", "present")],
      failed: [
        CheckFailed(
          "email",
          "email",
          "valid email format",
          "not-an-email",
          "Field must be a valid email address",
        ),
      ],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let feedback =
    check_feedback.from_response_check_result(
      "USER-003",
      check_result,
      "2026-01-17T10:40:00Z",
      100,
    )

  should.equal(feedback.result, Failed)
  should.be_some(feedback.error)

  case feedback.error {
    option.Some(err) -> {
      should.equal(err.error_type, "assertion_failure")
      should.be_true(
        err.message
        |> has_substring("email"),
      )
    }
    option.None -> panic as "Expected error to be Some"
  }
}

pub fn from_response_check_result_multiple_failures_test() {
  // Multiple field checks fail
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [
        CheckFailed(
          "id",
          "present",
          "field present",
          "missing",
          "Field required",
        ),
        CheckFailed("name", "string", "string type", "null", "Expected string"),
        CheckFailed("age", ">=18", "18 or greater", "15", "Must be adult"),
      ],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let feedback =
    check_feedback.from_response_check_result(
      "MULTI-001",
      check_result,
      "2026-01-17T10:45:00Z",
      200,
    )

  should.equal(feedback.result, Failed)

  // Verify error mentions multiple failures
  case feedback.error {
    option.Some(err) -> {
      should.be_true(
        err.message
        |> has_substring("more"),
      )
      // Should have trace with all failures
      should.be_some(err.trace)
    }
    option.None -> panic as "Expected error to be Some"
  }
}

// =============================================================================
// TEST: from_response_check_result_with_context - Context-aware conversion
// =============================================================================

pub fn from_response_check_result_with_context_success_test() {
  let check_result =
    ResponseCheckResult(
      passed: [CheckPassed("token", "present")],
      failed: [],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let feedback =
    check_feedback.from_response_check_result_with_context(
      "AUTH-001",
      check_result,
      "2026-01-17T10:30:00Z",
      150,
      "login_with_valid_credentials",
      "User can authenticate with correct username and password",
    )

  should.equal(feedback.result, Success)
  should.be_true(
    feedback.reason
    |> has_substring("login_with_valid_credentials"),
  )
}

pub fn from_response_check_result_with_context_failure_test() {
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [
        CheckFailed("token", "present", "field present", "missing", "Required"),
      ],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let feedback =
    check_feedback.from_response_check_result_with_context(
      "AUTH-002",
      check_result,
      "2026-01-17T10:35:00Z",
      200,
      "login_with_invalid_credentials",
      "Invalid credentials should return error",
    )

  should.equal(feedback.result, Failed)
  should.be_true(
    feedback.reason
    |> has_substring("login_with_invalid_credentials"),
  )
}

// =============================================================================
// TEST: summarize_check_result - Summary generation
// =============================================================================

pub fn summarize_check_result_all_passed_test() {
  let check_result =
    ResponseCheckResult(
      passed: [
        CheckPassed("id", "present"),
        CheckPassed("name", "string"),
        CheckPassed("email", "email"),
      ],
      failed: [],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let summary = check_feedback.summarize_check_result(check_result)

  should.equal(summary.total_checks, 3)
  should.equal(summary.passed_count, 3)
  should.equal(summary.failed_count, 0)
  should.be_true(summary.status_matched)
  should.equal(summary.failed_fields, [])
  should.equal(summary.failed_rules, [])
}

pub fn summarize_check_result_mixed_test() {
  let check_result =
    ResponseCheckResult(
      passed: [CheckPassed("id", "present")],
      failed: [
        CheckFailed("email", "email", "valid", "invalid", "Bad email"),
        CheckFailed("age", ">=18", "18+", "15", "Too young"),
      ],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let summary = check_feedback.summarize_check_result(check_result)

  should.equal(summary.total_checks, 3)
  should.equal(summary.passed_count, 1)
  should.equal(summary.failed_count, 2)
  should.be_true(summary.status_matched)
  should.equal(list.length(summary.failed_fields), 2)
  should.be_true(list.contains(summary.failed_fields, "email"))
  should.be_true(list.contains(summary.failed_fields, "age"))
}

pub fn summarize_check_result_status_mismatch_test() {
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [],
      status_ok: False,
      status_expected: 201,
      status_actual: 400,
    )

  let summary = check_feedback.summarize_check_result(check_result)

  should.be_false(summary.status_matched)
}

// =============================================================================
// TEST: extract_failure_details - Detailed failure extraction
// =============================================================================

pub fn extract_failure_details_no_failures_test() {
  let check_result =
    ResponseCheckResult(
      passed: [CheckPassed("id", "present")],
      failed: [],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let details = check_feedback.extract_failure_details(check_result)

  should.equal(details, [])
}

pub fn extract_failure_details_field_failures_test() {
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [
        CheckFailed(
          "email",
          "email",
          "valid email",
          "not-valid",
          "Must be valid email format",
        ),
      ],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let details = check_feedback.extract_failure_details(check_result)

  should.equal(list.length(details), 1)

  case details {
    [detail] -> {
      should.equal(detail.field, "email")
      should.equal(detail.rule, "email")
      should.equal(detail.expected, "valid email")
      should.equal(detail.actual, "not-valid")
      should.equal(detail.explanation, "Must be valid email format")
    }
    _ -> panic as "Expected exactly one detail"
  }
}

pub fn extract_failure_details_includes_status_mismatch_test() {
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [
        CheckFailed("name", "present", "present", "missing", "Required"),
      ],
      status_ok: False,
      status_expected: 200,
      status_actual: 404,
    )

  let details = check_feedback.extract_failure_details(check_result)

  // Should include both status mismatch and field failure
  should.equal(list.length(details), 2)

  // First should be status
  case details {
    [first, ..] -> {
      should.equal(first.field, "status")
      should.equal(first.expected, "200")
      should.equal(first.actual, "404")
    }
    _ -> panic as "Expected at least one detail"
  }
}

// =============================================================================
// TEST: determine_result - Result determination
// =============================================================================

pub fn determine_result_success_test() {
  let check_result =
    ResponseCheckResult(
      passed: [CheckPassed("id", "present")],
      failed: [],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let result = check_feedback.determine_result(check_result)

  should.equal(result, Success)
}

pub fn determine_result_failed_status_test() {
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [],
      status_ok: False,
      status_expected: 200,
      status_actual: 500,
    )

  let result = check_feedback.determine_result(check_result)

  should.equal(result, Failed)
}

pub fn determine_result_failed_checks_test() {
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [
        CheckFailed("id", "present", "present", "missing", "Required"),
      ],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let result = check_feedback.determine_result(check_result)

  should.equal(result, Failed)
}

// =============================================================================
// TEST: is_passing / has_failures - Boolean checks
// =============================================================================

pub fn is_passing_true_test() {
  let check_result =
    ResponseCheckResult(
      passed: [CheckPassed("id", "present")],
      failed: [],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  should.be_true(check_feedback.is_passing(check_result))
  should.be_false(check_feedback.has_failures(check_result))
}

pub fn is_passing_false_with_failures_test() {
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [
        CheckFailed("id", "present", "present", "missing", "Required"),
      ],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  should.be_false(check_feedback.is_passing(check_result))
  should.be_true(check_feedback.has_failures(check_result))
}

pub fn is_passing_false_with_status_mismatch_test() {
  let check_result =
    ResponseCheckResult(
      passed: [CheckPassed("id", "present")],
      failed: [],
      status_ok: False,
      status_expected: 200,
      status_actual: 404,
    )

  should.be_false(check_feedback.is_passing(check_result))
  should.be_true(check_feedback.has_failures(check_result))
}

// =============================================================================
// TEST: build_success_reason / build_failure_reason - Reason generation
// =============================================================================

pub fn build_success_reason_no_checks_test() {
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let reason = check_feedback.build_success_reason(check_result)

  should.be_true(
    reason
    |> has_substring("passed"),
  )
  should.be_true(
    reason
    |> has_substring("200"),
  )
}

pub fn build_success_reason_with_checks_test() {
  let check_result =
    ResponseCheckResult(
      passed: [
        CheckPassed("id", "present"),
        CheckPassed("name", "string"),
      ],
      failed: [],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let reason = check_feedback.build_success_reason(check_result)

  should.be_true(
    reason
    |> has_substring("2"),
  )
  should.be_true(
    reason
    |> has_substring("assertions"),
  )
}

pub fn build_failure_reason_status_mismatch_test() {
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [],
      status_ok: False,
      status_expected: 200,
      status_actual: 404,
    )

  let reason = check_feedback.build_failure_reason(check_result)

  should.be_true(
    reason
    |> has_substring("Status"),
  )
  should.be_true(
    reason
    |> has_substring("200"),
  )
  should.be_true(
    reason
    |> has_substring("404"),
  )
}

pub fn build_failure_reason_field_failures_test() {
  let check_result =
    ResponseCheckResult(
      passed: [],
      failed: [
        CheckFailed("email", "email", "valid", "invalid", "Bad"),
        CheckFailed("age", ">=18", "18+", "15", "Too young"),
      ],
      status_ok: True,
      status_expected: 200,
      status_actual: 200,
    )

  let reason = check_feedback.build_failure_reason(check_result)

  should.be_true(
    reason
    |> has_substring("2"),
  )
  should.be_true(
    reason
    |> has_substring("failed"),
  )
}

// =============================================================================
// HELPER: String contains check
// =============================================================================

fn has_substring(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}
