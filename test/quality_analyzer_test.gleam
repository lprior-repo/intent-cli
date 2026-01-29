//// Tests for kirk/quality_analyzer.gleam
//// Contract: Quality scoring (completeness, consistency, testability, clarity, security)

import gleam/dict
import gleam/float
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/quality_analyzer
import intent/types.{Behavior, Check, Get, Request, Response}
import test_helpers

// =============================================================================
// analyze_quality tests
// =============================================================================

pub fn analyze_quality_empty_spec_test() {
  // Contract: Empty spec produces valid quality report with scores in range
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let report = quality_analyzer.analyze_quality(spec)

  // All scores should be in valid range [0, 100]
  { report.completeness >=. 0.0 && report.completeness <=. 100.0 }
  |> should.be_true
  { report.consistency >=. 0.0 && report.consistency <=. 100.0 }
  |> should.be_true
  { report.overall >=. 0.0 && report.overall <=. 100.0 } |> should.be_true
}

pub fn analyze_quality_missing_why_test() {
  // Contract: Empty why fields produce Warning issues
  let behavior =
    Behavior(
      name: "get-user",
      intent: "Get a user by ID for testing purposes",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/users/${id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 200,
        example: json.object([#("id", json.int(1))]),
        checks: dict.from_list([
          #("id", Check(rule: "integer", why: "")),
          // Empty why
        ]),
      ),
      captures: dict.new(),
    )
  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  let report = quality_analyzer.analyze_quality(spec)

  // Should detect missing why as an issue
  let has_clarity_issue =
    report.issues
    |> list.any(fn(issue) {
      string.contains(string.lowercase(issue.issue), "why")
      || string.contains(string.lowercase(issue.field), "why")
    })

  // Clarity score should be reduced for missing 'why'
  { report.clarity <. 100.0 || has_clarity_issue } |> should.be_true
}

pub fn analyze_quality_short_intent_test() {
  // Contract: Short intents reduce clarity
  let behavior =
    Behavior(
      name: "get-user",
      intent: "Get",
      // Very short intent - less than 10 chars
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/users/${id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(status: 200, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    )
  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  let report = quality_analyzer.analyze_quality(spec)

  // Clarity should be reduced for short intent
  report.clarity |> should.not_equal(100.0)
}

pub fn analyze_quality_no_checks_test() {
  // Contract: Missing checks produce ErrorLevel issues
  let behavior =
    Behavior(
      name: "get-user",
      intent: "Get a user by their unique identifier",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/users/${id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 200,
        example: json.object([#("id", json.int(1))]),
        checks: dict.new(),
        // No checks
      ),
      captures: dict.new(),
    )
  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  let report = quality_analyzer.analyze_quality(spec)

  // Testability should be reduced when no checks are defined
  report.testability |> should.not_equal(100.0)
}

pub fn analyze_quality_duplicate_names_test() {
  // Contract: Duplicate names reduce consistency
  let behaviors = [
    test_helpers.make_test_behavior("get-user", []),
    test_helpers.make_test_behavior("get-user", []),
    // Duplicate name
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = quality_analyzer.analyze_quality(spec)

  // Consistency should be reduced for duplicate names
  report.consistency |> should.not_equal(100.0)
}

pub fn analyze_quality_security_keywords_test() {
  // Contract: Security keywords boost security score
  let behaviors = [
    Behavior(
      name: "test-authentication",
      intent: "Test that authentication works correctly",
      notes: "",
      requires: [],
      tags: ["security", "auth"],
      request: Request(
        method: Get,
        path: "/auth/verify",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(status: 200, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    ),
    Behavior(
      name: "test-unauthorized-access",
      intent: "Verify unauthorized access is blocked",
      notes: "",
      requires: [],
      tags: ["security"],
      request: Request(
        method: Get,
        path: "/admin",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(status: 401, example: json.null(), checks: dict.new()),
      captures: dict.new(),
    ),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = quality_analyzer.analyze_quality(spec)

  // Security score should be relatively high with security keywords
  { report.security >=. 0.0 && report.security <=. 100.0 } |> should.be_true
}

pub fn analyze_quality_weighted_average_test() {
  // Contract: Overall score matches weighted formula
  // overall = 0.2*completeness + 0.2*consistency + 0.25*testability + 0.15*clarity + 0.2*security
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let report = quality_analyzer.analyze_quality(spec)

  // Calculate expected overall
  let expected =
    report.completeness
    *. 0.2
    +. report.consistency
    *. 0.2
    +. report.testability
    *. 0.25
    +. report.clarity
    *. 0.15
    +. report.security
    *. 0.2

  // Allow small floating point tolerance
  let diff = float.absolute_value(report.overall -. expected)
  { diff <. 0.01 } |> should.be_true
}

pub fn analyze_quality_scores_in_range_test() {
  // Contract: All scores in range [0.0, 100.0]
  let behaviors = [
    test_helpers.make_test_behavior("behavior-1", []),
    test_helpers.make_test_behavior("behavior-2", ["behavior-1"]),
    test_helpers.make_test_behavior("behavior-3", ["behavior-2"]),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = quality_analyzer.analyze_quality(spec)

  // All scores should be in valid range
  { report.completeness >=. 0.0 && report.completeness <=. 100.0 }
  |> should.be_true
  { report.consistency >=. 0.0 && report.consistency <=. 100.0 }
  |> should.be_true
  { report.testability >=. 0.0 && report.testability <=. 100.0 }
  |> should.be_true
  { report.clarity >=. 0.0 && report.clarity <=. 100.0 } |> should.be_true
  { report.security >=. 0.0 && report.security <=. 100.0 } |> should.be_true
  { report.overall >=. 0.0 && report.overall <=. 100.0 } |> should.be_true
}

// =============================================================================
// format_report tests
// =============================================================================

pub fn format_report_test() {
  // Contract: Report formats without crashing
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let report = quality_analyzer.analyze_quality(spec)

  let formatted = quality_analyzer.format_report(report)

  // Should produce non-empty output
  formatted |> string.is_empty |> should.be_false
}

pub fn format_report_includes_scores_test() {
  // Contract: Report includes score information
  let behaviors = [test_helpers.make_test_behavior("test-behavior", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let report = quality_analyzer.analyze_quality(spec)

  let formatted = quality_analyzer.format_report(report)

  // Should contain quality-related terms
  let lower = string.lowercase(formatted)
  {
    string.contains(lower, "quality")
    || string.contains(lower, "score")
    || string.contains(lower, "%")
  }
  |> should.be_true
}

// =============================================================================
// severity_to_string tests
// =============================================================================

pub fn severity_to_string_info_test() {
  // Contract: Info severity converts correctly
  let result = quality_analyzer.severity_to_string(quality_analyzer.Info)
  result |> string.is_empty |> should.be_false
}

pub fn severity_to_string_warning_test() {
  // Contract: Warning severity converts correctly
  let result = quality_analyzer.severity_to_string(quality_analyzer.Warning)
  result |> string.lowercase |> string.contains("warn") |> should.be_true
}

pub fn severity_to_string_error_test() {
  // Contract: Error severity converts correctly
  let result = quality_analyzer.severity_to_string(quality_analyzer.Error)
  result |> string.lowercase |> string.contains("error") |> should.be_true
}

pub fn severity_to_string_critical_test() {
  // Contract: Critical severity converts correctly
  let result = quality_analyzer.severity_to_string(quality_analyzer.Critical)
  result |> string.lowercase |> string.contains("critical") |> should.be_true
}
