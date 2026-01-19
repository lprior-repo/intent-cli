//// Check-to-Bead Feedback Generator
////
//// Converts checker results (pass/fail) into bead feedback for AI consumption.
//// This bridges the gap between Intent's check system and the bead tracking system.
////
//// Architecture: Functional Core / Imperative Shell
//// - Pure functions: from_check_result(), from_response_check_result(), summarize_*
//// - No I/O in this module - pure transformation only
////
//// Usage:
//// ```gleam
//// let check_result = checker.check_response(expected, actual, ctx)
//// let feedback = check_feedback.from_response_check_result(
////   bead_id: "AUTH-001",
////   check_result: check_result,
////   executed_at: timestamp,
////   duration_ms: duration,
//// )
//// // Now persist feedback via bead_feedback module
//// ```

import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/string
import intent/bead_feedback.{
  type BeadError, type BeadFeedback, type BeadResult, BeadError, BeadFeedback,
  Failed, Success,
}
import intent/checker.{type ResponseCheckResult}

/// Summary of check results for AI consumption
pub type CheckSummary {
  CheckSummary(
    total_checks: Int,
    passed_count: Int,
    failed_count: Int,
    status_matched: Bool,
    failed_fields: List(String),
    failed_rules: List(String),
  )
}

/// Detailed failure information for AI debugging
pub type FailureDetail {
  FailureDetail(
    field: String,
    rule: String,
    expected: String,
    actual: String,
    explanation: String,
  )
}

// =============================================================================
// PURE: Main Conversion Functions (Functional Core)
// =============================================================================

/// Convert a ResponseCheckResult to BeadFeedback.
///
/// This is the primary entry point for converting check results to bead feedback.
/// The function determines success/failure based on:
/// - Status code match
/// - All field checks passing
///
/// ## Examples
///
/// ```gleam
/// let check_result = checker.check_response(expected, actual, ctx)
/// let feedback = from_response_check_result(
///   "AUTH-001",
///   check_result,
///   "2026-01-17T10:30:00Z",
///   150,
/// )
/// ```
pub fn from_response_check_result(
  bead_id: String,
  check_result: ResponseCheckResult,
  executed_at: String,
  duration_ms: Int,
) -> BeadFeedback {
  let all_passed = check_result.status_ok && list.is_empty(check_result.failed)

  case all_passed {
    True ->
      create_success_feedback(bead_id, check_result, executed_at, duration_ms)
    False ->
      create_failure_feedback(bead_id, check_result, executed_at, duration_ms)
  }
}

/// Convert a ResponseCheckResult to BeadFeedback with optional behavior context.
///
/// This variant allows including additional context about the behavior being tested,
/// which provides richer feedback for AI consumers.
pub fn from_response_check_result_with_context(
  bead_id: String,
  check_result: ResponseCheckResult,
  executed_at: String,
  duration_ms: Int,
  behavior_name: String,
  behavior_intent: String,
) -> BeadFeedback {
  let all_passed = check_result.status_ok && list.is_empty(check_result.failed)

  case all_passed {
    True -> {
      let reason =
        "Behavior '" <> behavior_name <> "' passed: " <> behavior_intent
      bead_feedback.create_success_feedback(
        bead_id,
        reason,
        executed_at,
        duration_ms,
      )
    }
    False -> {
      let summary = summarize_check_result(check_result)
      let reason = build_failure_reason_with_context(behavior_name, summary)
      let error = build_error_from_check_result(check_result, behavior_intent)
      BeadFeedback(
        bead_id: bead_id,
        result: Failed,
        reason: reason,
        executed_at: executed_at,
        duration_ms: duration_ms,
        error: option.Some(error),
        blocked_by: option.None,
      )
    }
  }
}

// =============================================================================
// PURE: Summary Functions (Functional Core)
// =============================================================================

/// Create a summary of check results for analysis.
///
/// Useful for understanding the overall state of a check without
/// iterating through all individual results.
pub fn summarize_check_result(check_result: ResponseCheckResult) -> CheckSummary {
  let passed_count = list.length(check_result.passed)
  let failed_count = list.length(check_result.failed)

  let failed_fields =
    check_result.failed
    |> list.map(fn(check) {
      case check {
        checker.CheckFailed(field, _, _, _, _) -> field
        checker.CheckPassed(field, _) -> field
      }
    })
    |> list.unique

  let failed_rules =
    check_result.failed
    |> list.map(fn(check) {
      case check {
        checker.CheckFailed(_, rule, _, _, _) -> rule
        checker.CheckPassed(_, rule) -> rule
      }
    })
    |> list.unique

  CheckSummary(
    total_checks: passed_count + failed_count,
    passed_count: passed_count,
    failed_count: failed_count,
    status_matched: check_result.status_ok,
    failed_fields: failed_fields,
    failed_rules: failed_rules,
  )
}

/// Extract detailed failure information from check results.
///
/// Returns a list of FailureDetail records that can be used
/// for detailed error reporting or AI analysis.
pub fn extract_failure_details(
  check_result: ResponseCheckResult,
) -> List(FailureDetail) {
  let field_failures =
    check_result.failed
    |> list.filter_map(fn(check) {
      case check {
        checker.CheckFailed(field, rule, expected, actual, explanation) ->
          Ok(FailureDetail(
            field: field,
            rule: rule,
            expected: expected,
            actual: actual,
            explanation: explanation,
          ))
        checker.CheckPassed(_, _) -> Error(Nil)
      }
    })

  // Add status mismatch as a failure detail if applicable
  case check_result.status_ok {
    True -> field_failures
    False -> [
      FailureDetail(
        field: "status",
        rule: "equals " <> int.to_string(check_result.status_expected),
        expected: int.to_string(check_result.status_expected),
        actual: int.to_string(check_result.status_actual),
        explanation: "HTTP status code mismatch",
      ),
      ..field_failures
    ]
  }
}

/// Determine the BeadResult from a ResponseCheckResult.
///
/// Useful when you only need to know pass/fail without creating full feedback.
pub fn determine_result(check_result: ResponseCheckResult) -> BeadResult {
  let all_passed = check_result.status_ok && list.is_empty(check_result.failed)
  case all_passed {
    True -> Success
    False -> Failed
  }
}

/// Check if a ResponseCheckResult represents a passing state.
pub fn is_passing(check_result: ResponseCheckResult) -> Bool {
  check_result.status_ok && list.is_empty(check_result.failed)
}

/// Check if a ResponseCheckResult has any failures.
pub fn has_failures(check_result: ResponseCheckResult) -> Bool {
  !check_result.status_ok || !list.is_empty(check_result.failed)
}

// =============================================================================
// PURE: Reason Generation (Functional Core)
// =============================================================================

/// Generate a human-readable success reason from check results.
pub fn build_success_reason(check_result: ResponseCheckResult) -> String {
  let check_count = list.length(check_result.passed)
  case check_count {
    0 ->
      "All checks passed (status "
      <> int.to_string(check_result.status_actual)
      <> ")"
    1 -> "All checks passed: 1 assertion verified"
    n -> "All checks passed: " <> int.to_string(n) <> " assertions verified"
  }
}

/// Generate a human-readable failure reason from check results.
pub fn build_failure_reason(check_result: ResponseCheckResult) -> String {
  let summary = summarize_check_result(check_result)
  build_failure_reason_from_summary(summary, check_result)
}

// =============================================================================
// PRIVATE: Helper Functions
// =============================================================================

fn create_success_feedback(
  bead_id: String,
  check_result: ResponseCheckResult,
  executed_at: String,
  duration_ms: Int,
) -> BeadFeedback {
  let reason = build_success_reason(check_result)
  bead_feedback.create_success_feedback(
    bead_id,
    reason,
    executed_at,
    duration_ms,
  )
}

fn create_failure_feedback(
  bead_id: String,
  check_result: ResponseCheckResult,
  executed_at: String,
  duration_ms: Int,
) -> BeadFeedback {
  let reason = build_failure_reason(check_result)
  let error = build_error_from_check_result(check_result, "")

  BeadFeedback(
    bead_id: bead_id,
    result: Failed,
    reason: reason,
    executed_at: executed_at,
    duration_ms: duration_ms,
    error: option.Some(error),
    blocked_by: option.None,
  )
}

fn build_failure_reason_from_summary(
  summary: CheckSummary,
  check_result: ResponseCheckResult,
) -> String {
  case summary.status_matched {
    False -> {
      "Status mismatch: expected "
      <> int.to_string(check_result.status_expected)
      <> ", got "
      <> int.to_string(check_result.status_actual)
      <> case summary.failed_count > 0 {
        True ->
          " + "
          <> int.to_string(summary.failed_count)
          <> " field check(s) failed"
        False -> ""
      }
    }
    True -> {
      case summary.failed_count {
        0 -> "Unknown failure"
        1 -> "1 check failed: " <> string.join(summary.failed_fields, ", ")
        n ->
          int.to_string(n)
          <> " checks failed: "
          <> string.join(summary.failed_fields, ", ")
      }
    }
  }
}

fn build_failure_reason_with_context(
  behavior_name: String,
  summary: CheckSummary,
) -> String {
  let base_reason = case summary.status_matched {
    False -> "status mismatch"
    True ->
      case summary.failed_count {
        1 -> "1 check failed"
        n -> int.to_string(n) <> " checks failed"
      }
  }

  "Behavior '" <> behavior_name <> "' failed: " <> base_reason
}

fn build_error_from_check_result(
  check_result: ResponseCheckResult,
  context: String,
) -> BeadError {
  let failures = extract_failure_details(check_result)

  let error_type = case check_result.status_ok {
    False -> "status_mismatch"
    True -> "assertion_failure"
  }

  let message = case failures {
    [] -> "Check failed"
    [first, ..rest] -> {
      let first_msg =
        first.field
        <> ": expected "
        <> first.expected
        <> ", got "
        <> first.actual
      case rest {
        [] -> first_msg
        _ ->
          first_msg <> " (and " <> int.to_string(list.length(rest)) <> " more)"
      }
    }
  }

  let trace = build_error_trace(failures, context)

  BeadError(error_type: error_type, message: message, trace: trace)
}

fn build_error_trace(
  failures: List(FailureDetail),
  context: String,
) -> Option(String) {
  case failures {
    [] -> option.None
    _ -> {
      let trace_lines =
        failures
        |> list.map(fn(f) {
          "- "
          <> f.field
          <> " ("
          <> f.rule
          <> "): "
          <> f.explanation
          <> "\n  expected: "
          <> f.expected
          <> "\n  actual: "
          <> f.actual
        })

      let trace = string.join(trace_lines, "\n\n")
      let full_trace = case context {
        "" -> trace
        _ -> "Context: " <> context <> "\n\n" <> trace
      }
      option.Some(full_trace)
    }
  }
}
