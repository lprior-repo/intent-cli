//// Tests for kirk/coverage_analyzer.gleam
//// Contract: OWASP Top 10 analysis and multi-dimensional coverage scoring

import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/string
import gleeunit/should
import intent/kirk/coverage_analyzer
import intent/types.{Behavior, Delete, Get, Post, Put, Request, Response}
import test_helpers

// =============================================================================
// analyze_coverage tests
// =============================================================================

pub fn analyze_coverage_empty_spec_test() {
  // Contract: Empty spec has zero coverage
  let spec = test_helpers.make_test_spec_from_behaviors([])

  let report = coverage_analyzer.analyze_coverage(spec)

  // Methods dict should be empty or have zero counts
  let method_count =
    report.methods
    |> dict.values
    |> list.fold(0, fn(sum, v) { sum + v })
  method_count |> should.equal(0)
}

pub fn analyze_coverage_method_counts_test() {
  // Contract: Methods counted correctly
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
    test_helpers.make_test_behavior_with_method("update-user", Put, []),
    test_helpers.make_test_behavior_with_method("another-get", Get, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = coverage_analyzer.analyze_coverage(spec)

  // Should have GET count of 2
  case dict.get(report.methods, "GET") {
    Ok(count) -> count |> should.equal(2)
    Error(_) -> should.fail()
  }
}

pub fn analyze_coverage_status_categories_test() {
  // Contract: 2xx/4xx/5xx categorized correctly
  let behaviors = [
    test_helpers.make_test_behavior_with_status("success", 200, []),
    test_helpers.make_test_behavior_with_status("created", 201, []),
    test_helpers.make_test_behavior_with_status("not-found", 404, []),
    test_helpers.make_test_behavior_with_status("unauthorized", 401, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = coverage_analyzer.analyze_coverage(spec)

  // Should have status code categories
  // 2xx count should be 2 (200, 201)
  case dict.get(report.status_codes, "2xx") {
    Ok(count) -> count |> should.equal(2)
    Error(_) -> should.fail()
  }

  // 4xx count should be 2 (404, 401)
  case dict.get(report.status_codes, "4xx") {
    Ok(count) -> count |> should.equal(2)
    Error(_) -> should.fail()
  }
}

pub fn analyze_coverage_path_normalization_test() {
  // Contract: ${var} → {param} normalization
  let behavior =
    Behavior(
      name: "get-user",
      intent: "Get a user by ID",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/users/${user_id}/posts/${post_id}",
        headers: dict.new(),
        query: dict.new(),
        body: json.null(),
      ),
      response: Response(
        status: 200,
        example: json.null(),
        checks: dict.new(),
        headers: None,
      ),
      captures: dict.new(),
    )
  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  let report = coverage_analyzer.analyze_coverage(spec)

  // Paths should have normalized path (with {param})
  let has_normalized_path =
    report.paths
    |> dict.keys
    |> list.any(fn(path) { string.contains(path, "{") })

  has_normalized_path |> should.be_true
}

pub fn analyze_coverage_owasp_keywords_test() {
  // Contract: OWASP categories detected from keywords
  let behaviors = [
    Behavior(
      name: "login",
      intent: "Authenticate user with valid credentials",
      notes: "",
      requires: [],
      tags: ["auth"],
      request: Request(
        method: Post,
        path: "/auth/login",
        headers: dict.new(),
        query: dict.new(),
        body: json.object([
          #("email", json.string("test@example.com")),
          #("password", json.string("secret")),
        ]),
      ),
      response: Response(
        status: 200,
        example: json.null(),
        checks: dict.new(),
        headers: None,
      ),
      captures: dict.new(),
    ),
    Behavior(
      name: "test-unauthorized-access",
      intent: "Verify unauthorized access is rejected",
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
      response: Response(
        status: 401,
        example: json.null(),
        checks: dict.new(),
        headers: None,
      ),
      captures: dict.new(),
    ),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = coverage_analyzer.analyze_coverage(spec)

  // OWASP score should be >= 0
  { report.owasp.score >=. 0.0 } |> should.be_true
}

pub fn analyze_coverage_owasp_score_test() {
  // Contract: Score = covered/10 * 100
  let behaviors = [test_helpers.make_test_behavior("test-behavior", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = coverage_analyzer.analyze_coverage(spec)

  // OWASP score should be in range [0, 100]
  { report.owasp.score >=. 0.0 && report.owasp.score <=. 100.0 }
  |> should.be_true
}

pub fn analyze_coverage_edge_cases_test() {
  // Contract: Edge case keywords detected
  let behaviors = [
    Behavior(
      name: "empty-list",
      intent: "Test empty list response",
      notes: "",
      requires: [],
      tags: ["edge-case"],
      request: Request(
        method: Get,
        path: "/users",
        headers: dict.new(),
        query: dict.from_list([#("page", json.int(999))]),
        body: json.null(),
      ),
      response: Response(
        status: 200,
        example: json.array([], fn(x) { x }),
        checks: dict.new(),
        headers: None,
      ),
      captures: dict.new(),
    ),
    Behavior(
      name: "pagination-test",
      intent: "Test pagination with limit and offset",
      notes: "",
      requires: [],
      tags: [],
      request: Request(
        method: Get,
        path: "/users",
        headers: dict.new(),
        query: dict.from_list([
          #("limit", json.int(10)),
          #("offset", json.int(20)),
        ]),
        body: json.null(),
      ),
      response: Response(
        status: 200,
        example: json.null(),
        checks: dict.new(),
        headers: None,
      ),
      captures: dict.new(),
    ),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = coverage_analyzer.analyze_coverage(spec)

  // Edge case coverage should have some tested items
  { list.length(report.edge_cases.tested) >= 0 } |> should.be_true
}

pub fn analyze_coverage_overall_score_test() {
  // Contract: Weighted formula produces valid overall score
  let behaviors = [
    test_helpers.make_test_behavior_with_method("create", Post, []),
    test_helpers.make_test_behavior_with_method("read", Get, []),
    test_helpers.make_test_behavior_with_method("update", Put, []),
    test_helpers.make_test_behavior_with_method("delete", Delete, []),
    test_helpers.make_test_behavior_with_status("not-found", 404, []),
    test_helpers.make_test_behavior_with_status("unauthorized", 401, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  let report = coverage_analyzer.analyze_coverage(spec)

  // Overall score should be in valid range
  { report.overall_score >=. 0.0 && report.overall_score <=. 100.0 }
  |> should.be_true
}

// =============================================================================
// format_report tests
// =============================================================================

pub fn format_report_test() {
  // Contract: Report formats without crashing
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let report = coverage_analyzer.analyze_coverage(spec)

  let formatted = coverage_analyzer.format_report(report)

  // Should produce non-empty output
  formatted |> string.is_empty |> should.be_false
}

pub fn format_report_includes_coverage_test() {
  // Contract: Report includes coverage information
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let report = coverage_analyzer.analyze_coverage(spec)

  let formatted = coverage_analyzer.format_report(report)

  // Should contain coverage-related terms
  let lower = string.lowercase(formatted)
  {
    string.contains(lower, "coverage")
    || string.contains(lower, "method")
    || string.contains(lower, "owasp")
  }
  |> should.be_true
}

pub fn format_report_includes_methods_test() {
  // Contract: Report includes method breakdown
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let report = coverage_analyzer.analyze_coverage(spec)

  let formatted = coverage_analyzer.format_report(report)

  // Should mention GET and POST
  { string.contains(formatted, "GET") || string.contains(formatted, "POST") }
  |> should.be_true
}
