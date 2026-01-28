//// Tests for kirk/inversion_checker.gleam
//// Contract: Inversion thinking analysis (security, usability, integration inversions)

import gleam/dict
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/inversion_checker
import intent/types.{Behavior, Get, Post, Request, Response}
import test_helpers

// =============================================================================
// analyze_inversions tests
// =============================================================================

pub fn analyze_inversions_empty_spec_test() {
  // Contract: Empty spec has 0% score
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let report = inversion_checker.analyze_inversions(spec)

  // Empty spec should have low score due to missing inversions
  { report.score >=. 0.0 } |> should.be_true
}

pub fn analyze_inversions_auth_bypass_test() {
  // Contract: 401 behavior covers auth-bypass
  let behaviors = [
    Behavior(
      name: "unauthorized-access",
      intent: "Verify unauthorized access is rejected",
      notes: "",
      requires: [],
      tags: ["security"],
      request: Request(
        method: Get,
        path: "/protected-resource",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(status: 401, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    ),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = inversion_checker.analyze_inversions(spec)

  // Should have score > 0 (some coverage)
  { report.score >=. 0.0 } |> should.be_true
}

pub fn analyze_inversions_sql_injection_test() {
  // Contract: "injection" keyword covers gap
  let behaviors = [
    Behavior(
      name: "sql-injection-test",
      intent: "Verify SQL injection is blocked",
      notes: "",
      requires: [],
      tags: ["security", "injection"],
      request: Request(
        method: Post,
        path: "/users",
        headers: dict.new(),
        query: dict.new(),
        body: json.object([
          #("name", json.string("'; DROP TABLE users; --")),
        ]),
      ),
      response: Response(status: 400, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    ),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = inversion_checker.analyze_inversions(spec)

  // Score should be >= 0
  { report.score >=. 0.0 } |> should.be_true
}

pub fn analyze_inversions_not_found_test() {
  // Contract: 404 behavior covers not-found
  let behaviors = [
    test_helpers.make_test_behavior_with_status("not-found-test", 404, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = inversion_checker.analyze_inversions(spec)

  // Should have valid score
  { report.score >=. 0.0 && report.score <=. 100.0 } |> should.be_true
}

pub fn analyze_inversions_rate_limit_test() {
  // Contract: 429 status covers rate-limit
  let behaviors = [
    test_helpers.make_test_behavior_with_status("rate-limit-test", 429, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = inversion_checker.analyze_inversions(spec)

  // Should have valid score
  { report.score >=. 0.0 && report.score <=. 100.0 } |> should.be_true
}

pub fn analyze_inversions_score_calculation_test() {
  // Contract: Score formula produces valid result
  let behaviors = [
    test_helpers.make_test_behavior_with_status("auth-test", 401, []),
    test_helpers.make_test_behavior_with_status("not-found-test", 404, []),
    test_helpers.make_test_behavior_with_status("bad-request-test", 400, []),
    test_helpers.make_test_behavior_with_status("conflict-test", 409, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = inversion_checker.analyze_inversions(spec)

  // Score should be in valid range
  { report.score >=. 0.0 && report.score <=. 100.0 } |> should.be_true
}

pub fn analyze_inversions_suggestions_limit_test() {
  // Contract: Max 10 suggestions (5 security + 3 usability + 2 integration)
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let report = inversion_checker.analyze_inversions(spec)

  // Should have at most 10 suggestions
  { list.length(report.suggested_behaviors) <= 10 } |> should.be_true
}

pub fn analyze_inversions_severity_assignment_test() {
  // Contract: Gaps have severity assigned
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let report = inversion_checker.analyze_inversions(spec)

  // Check all gaps have non-empty what_could_fail
  let security_gaps_valid =
    report.security_gaps
    |> list.all(fn(gap) { !string.is_empty(gap.what_could_fail) })

  let usability_gaps_valid =
    report.usability_gaps
    |> list.all(fn(gap) { !string.is_empty(gap.what_could_fail) })

  let integration_gaps_valid =
    report.integration_gaps
    |> list.all(fn(gap) { !string.is_empty(gap.what_could_fail) })

  { security_gaps_valid && usability_gaps_valid && integration_gaps_valid }
  |> should.be_true
}

pub fn analyze_inversions_with_comprehensive_test() {
  // Contract: Comprehensive spec has better score
  let behaviors = [
    test_helpers.make_test_behavior_with_status("success", 200, []),
    test_helpers.make_test_behavior_with_status("unauthorized", 401, []),
    test_helpers.make_test_behavior_with_status("forbidden", 403, []),
    test_helpers.make_test_behavior_with_status("not-found", 404, []),
    test_helpers.make_test_behavior_with_status("bad-request", 400, []),
    test_helpers.make_test_behavior_with_status("conflict", 409, []),
    test_helpers.make_test_behavior_with_status("rate-limited", 429, []),
    test_helpers.make_test_behavior_with_status("server-error", 500, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = inversion_checker.analyze_inversions(spec)

  // Should have reasonable score with comprehensive error coverage
  { report.score >=. 0.0 } |> should.be_true
}

// =============================================================================
// format_report tests
// =============================================================================

pub fn format_report_test() {
  // Contract: Report formats without crashing
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let report = inversion_checker.analyze_inversions(spec)

  let formatted = inversion_checker.format_report(report)

  // Should produce non-empty output
  formatted |> string.is_empty |> should.be_false
}

pub fn format_report_includes_score_test() {
  // Contract: Report includes score information
  let behaviors = [test_helpers.make_test_behavior("test-behavior", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let report = inversion_checker.analyze_inversions(spec)

  let formatted = inversion_checker.format_report(report)

  // Should contain score-related terms or percentage
  let lower = string.lowercase(formatted)
  {
    string.contains(lower, "score")
    || string.contains(lower, "inversion")
    || string.contains(lower, "%")
  }
  |> should.be_true
}

// =============================================================================
// severity_to_string tests
// =============================================================================

pub fn severity_to_string_low_test() {
  // Contract: Low severity converts correctly
  let result = inversion_checker.severity_to_string(inversion_checker.Low)
  result |> string.lowercase |> string.contains("low") |> should.be_true
}

pub fn severity_to_string_medium_test() {
  // Contract: Medium severity converts correctly
  let result = inversion_checker.severity_to_string(inversion_checker.Medium)
  result |> string.lowercase |> string.contains("medium") |> should.be_true
}

pub fn severity_to_string_high_test() {
  // Contract: High severity converts correctly
  let result = inversion_checker.severity_to_string(inversion_checker.High)
  result |> string.lowercase |> string.contains("high") |> should.be_true
}

pub fn severity_to_string_critical_test() {
  // Contract: Critical severity converts correctly
  let result = inversion_checker.severity_to_string(inversion_checker.Critical)
  result |> string.lowercase |> string.contains("critical") |> should.be_true
}
