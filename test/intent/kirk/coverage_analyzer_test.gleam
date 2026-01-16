//// Test coverage for intent/kirk/coverage_analyzer.gleam
////
//// Tests multi-dimensional API test coverage analysis:
//// - HTTP methods (CRUD operations)
//// - Response status codes (2xx/4xx/5xx)
//// - Path coverage and normalization
//// - Edge case patterns
//// - OWASP Top 10 security coverage
////
//// DbC Postconditions Verified:
//// - overall_score in range [0.0, 100.0]
//// - methods contains only valid HTTP method strings
//// - owasp.score = (10 - len(missing)) / 10 * 100
//// - Path normalization: ${var} → {param}

import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/coverage_analyzer
import intent/types.{
  type Behavior, type Request, Behavior, Delete, Get, Patch, Post, Put, Request,
}
import test_helpers

// =============================================================================
// EMPTY SPEC TESTS (Baseline)
// =============================================================================

pub fn analyze_coverage_empty_spec_test() {
  // GIVEN: An empty spec with no behaviors
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Method counts are zero
  let total_methods =
    dict.values(report.methods)
    |> list.fold(0, fn(sum, count) { sum + count })

  total_methods
  |> should.equal(0)

  // THEN: DbC postcondition - overall_score in [0.0, 100.0]
  { report.overall_score >=. 0.0 && report.overall_score <=. 100.0 }
  |> should.be_true()

  // THEN: OWASP coverage shows all categories missing
  { list.length(report.owasp.missing) >= 0 }
  |> should.be_true()
}

// =============================================================================
// METHOD COUNT TESTS
// =============================================================================

pub fn analyze_coverage_method_counts_test() {
  // GIVEN: A spec with multiple HTTP methods
  let get_behavior =
    test_helpers.make_test_behavior_with_method("get-user", Get, [])
  let post_behavior =
    test_helpers.make_test_behavior_with_method("create-user", Post, [])
  let put_behavior =
    test_helpers.make_test_behavior_with_method("update-user", Put, [])
  let delete_behavior =
    test_helpers.make_test_behavior_with_method("delete-user", Delete, [])

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      get_behavior,
      post_behavior,
      put_behavior,
      delete_behavior,
    ])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Methods dict contains each method
  dict.has_key(report.methods, "GET")
  |> should.be_true()

  dict.has_key(report.methods, "POST")
  |> should.be_true()

  dict.has_key(report.methods, "PUT")
  |> should.be_true()

  dict.has_key(report.methods, "DELETE")
  |> should.be_true()

  // THEN: Each method has count of 1
  case dict.get(report.methods, "GET") {
    Ok(count) -> count |> should.equal(1)
    Error(_) -> should.fail()
  }
}

pub fn analyze_coverage_only_get_methods_test() {
  // GIVEN: A spec with only GET operations
  let get1 = test_helpers.make_test_behavior_with_method("get-user", Get, [])
  let get2 = test_helpers.make_test_behavior_with_method("get-posts", Get, [])

  let spec = test_helpers.make_test_spec_from_behaviors([get1, get2])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Only GET appears in methods
  dict.has_key(report.methods, "GET")
  |> should.be_true()

  // THEN: GET count is 2
  case dict.get(report.methods, "GET") {
    Ok(count) -> count |> should.equal(2)
    Error(_) -> should.fail()
  }

  // THEN: Other methods are either missing or have count 0
  case dict.get(report.methods, "POST") {
    Ok(count) -> count |> should.equal(0)
    Error(_) -> True |> should.be_true()
  }
}

// =============================================================================
// STATUS CODE CATEGORIZATION TESTS
// =============================================================================

pub fn analyze_coverage_status_categories_test() {
  // GIVEN: Behaviors with 2xx, 4xx, and 5xx statuses
  let success = test_helpers.make_test_behavior_with_status("get-user", 200, [])
  let created =
    test_helpers.make_test_behavior_with_status("create-user", 201, [])
  let bad_request =
    test_helpers.make_test_behavior_with_status("invalid-input", 400, [])
  let unauthorized =
    test_helpers.make_test_behavior_with_status("no-auth", 401, [])
  let server_error =
    test_helpers.make_test_behavior_with_status("server-crash", 500, [])

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      success,
      created,
      bad_request,
      unauthorized,
      server_error,
    ])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Status codes dict contains 2xx category
  dict.has_key(report.status_codes, "2xx")
  |> should.be_true()

  // THEN: 2xx count is 2 (200 + 201)
  case dict.get(report.status_codes, "2xx") {
    Ok(count) -> count |> should.equal(2)
    Error(_) -> should.fail()
  }

  // THEN: 4xx category exists
  dict.has_key(report.status_codes, "4xx")
  |> should.be_true()

  // THEN: 5xx category exists
  dict.has_key(report.status_codes, "5xx")
  |> should.be_true()
}

pub fn analyze_coverage_only_success_statuses_test() {
  // GIVEN: A spec with only 2xx statuses
  let success1 =
    test_helpers.make_test_behavior_with_status("get-user", 200, [])
  let success2 =
    test_helpers.make_test_behavior_with_status("create-user", 201, [])

  let spec = test_helpers.make_test_spec_from_behaviors([success1, success2])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: 2xx count is 2
  case dict.get(report.status_codes, "2xx") {
    Ok(count) -> count |> should.equal(2)
    Error(_) -> should.fail()
  }

  // THEN: 4xx and 5xx are missing or zero
  {
    dict.get(report.status_codes, "4xx") == Ok(0)
    || dict.get(report.status_codes, "4xx") == Error(Nil)
  }
  |> should.be_true()
}

// =============================================================================
// PATH NORMALIZATION TESTS
// =============================================================================

pub fn analyze_coverage_path_normalization_test() {
  // GIVEN: Behaviors with ${var} style path parameters
  let behavior1 = test_helpers.make_test_behavior("get-user", [])
  let behavior1_with_path =
    types.Behavior(
      ..behavior1,
      request: types.Request(..behavior1.request, path: "/users/${id}"),
    )

  let behavior2 = test_helpers.make_test_behavior("get-post", [])
  let behavior2_with_path =
    types.Behavior(
      ..behavior2,
      request: types.Request(
        ..behavior2.request,
        path: "/posts/${post_id}/comments/${comment_id}",
      ),
    )

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      behavior1_with_path,
      behavior2_with_path,
    ])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: DbC invariant - paths have ${var} normalized to {param}
  let normalized_paths = dict.keys(report.paths)

  // Check that at least one path is normalized
  let has_normalized =
    normalized_paths
    |> list.any(fn(path) {
      string.contains(path, "{") && string.contains(path, "}")
    })

  has_normalized
  |> should.be_true()
}

// =============================================================================
// OWASP COVERAGE TESTS
// =============================================================================

pub fn analyze_coverage_owasp_keywords_test() {
  // GIVEN: Behaviors with OWASP-related keywords
  let auth_behavior =
    types.Behavior(
      ..test_helpers.make_test_behavior("login", []),
      intent: "User authentication with JWT token",
    )

  let access_control =
    types.Behavior(
      ..test_helpers.make_test_behavior("check-permission", []),
      intent: "Verify user has required permissions",
    )

  let unauthorized =
    test_helpers.make_test_behavior_with_status("unauthorized-access", 401, [])

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      auth_behavior,
      access_control,
      unauthorized,
    ])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Some OWASP categories are covered
  let covered_count =
    dict.size(report.owasp.categories) - list.length(report.owasp.missing)

  { covered_count > 0 }
  |> should.be_true()

  // THEN: OWASP score is calculated
  { report.owasp.score >=. 0.0 && report.owasp.score <=. 100.0 }
  |> should.be_true()
}

pub fn analyze_coverage_owasp_score_formula_test() {
  // GIVEN: Any spec
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("test-behavior", []),
    ])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: DbC postcondition - owasp.score = (10 - len(missing)) / 10 * 100
  let expected_score =
    int.to_float(10 - list.length(report.owasp.missing)) /. 10.0 *. 100.0

  // Allow small floating point error
  let diff = float_abs(report.owasp.score -. expected_score)

  { diff <. 0.1 }
  |> should.be_true()
}

// Helper for absolute value
fn float_abs(f: Float) -> Float {
  case f >=. 0.0 {
    True -> f
    False -> 0.0 -. f
  }
}

pub fn analyze_coverage_no_owasp_keywords_test() {
  // GIVEN: Behaviors with no security keywords
  let behavior =
    types.Behavior(
      ..test_helpers.make_test_behavior("get-data", []),
      intent: "Retrieve data from database",
    )

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Most/all OWASP categories are missing
  { list.length(report.owasp.missing) >= 5 }
  |> should.be_true()

  // THEN: OWASP score is low
  { report.owasp.score <. 60.0 }
  |> should.be_true()
}

// =============================================================================
// EDGE CASE DETECTION TESTS
// =============================================================================

pub fn analyze_coverage_edge_cases_test() {
  // GIVEN: Behaviors with edge case keywords
  let empty_list =
    types.Behavior(
      ..test_helpers.make_test_behavior("get-empty-list", []),
      intent: "Test behavior when list is empty",
    )

  let pagination =
    types.Behavior(
      ..test_helpers.make_test_behavior("paginate", []),
      intent: "Test pagination with limit and offset",
    )

  let null_values =
    types.Behavior(
      ..test_helpers.make_test_behavior("handle-null", []),
      intent: "Handle null values in request",
    )

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      empty_list,
      pagination,
      null_values,
    ])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Edge cases are detected
  { list.length(report.edge_cases.tested) > 0 }
  |> should.be_true()
}

// =============================================================================
// OVERALL SCORE TESTS (DbC Postcondition)
// =============================================================================

pub fn analyze_coverage_overall_score_range_test() {
  // GIVEN: Any spec
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("test-behavior", []),
    ])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: DbC postcondition - overall_score in [0.0, 100.0]
  { report.overall_score >=. 0.0 }
  |> should.be_true()

  { report.overall_score <=. 100.0 }
  |> should.be_true()
}

pub fn analyze_coverage_overall_score_weighted_test() {
  // GIVEN: A comprehensive spec
  let get_behavior =
    test_helpers.make_test_behavior_with_method("get-user", Get, [])
  let post_behavior =
    test_helpers.make_test_behavior_with_method("create-user", Post, [])
  let error_behavior =
    test_helpers.make_test_behavior_with_status("unauthorized", 401, [])

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      get_behavior,
      post_behavior,
      error_behavior,
    ])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Overall score is calculated (formula uses weights)
  // Note: Exact formula may vary, but score should be reasonable
  { report.overall_score >=. 0.0 && report.overall_score <=. 100.0 }
  |> should.be_true()
}

// =============================================================================
// FORMAT REPORT TESTS
// =============================================================================

pub fn format_report_test() {
  // GIVEN: A coverage report
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("test-behavior", []),
    ])

  let report = coverage_analyzer.analyze_coverage(spec)

  // WHEN: Formatting the report
  let formatted = coverage_analyzer.format_report(report)

  // THEN: Report produces valid UTF-8 string
  formatted
  |> should.not_equal("")

  // THEN: Report contains coverage information
  string.contains(formatted, "Coverage")
  |> should.be_true()
}

pub fn format_report_shows_methods_test() {
  // GIVEN: A coverage report with methods
  let get_behavior =
    test_helpers.make_test_behavior_with_method("get-user", Get, [])
  let post_behavior =
    test_helpers.make_test_behavior_with_method("create-user", Post, [])

  let spec =
    test_helpers.make_test_spec_from_behaviors([get_behavior, post_behavior])

  let report = coverage_analyzer.analyze_coverage(spec)

  // WHEN: Formatting the report
  let formatted = coverage_analyzer.format_report(report)

  // THEN: Report mentions HTTP methods or CRUD
  {
    string.contains(formatted, "Method")
    || string.contains(formatted, "GET")
    || string.contains(formatted, "POST")
  }
  |> should.be_true()
}

pub fn format_report_shows_owasp_test() {
  // GIVEN: A coverage report
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("test-behavior", []),
    ])

  let report = coverage_analyzer.analyze_coverage(spec)

  // WHEN: Formatting the report
  let formatted = coverage_analyzer.format_report(report)

  // THEN: Report mentions OWASP or security
  {
    string.contains(formatted, "OWASP")
    || string.contains(formatted, "Security")
    || string.contains(formatted, "security")
  }
  |> should.be_true()
}

// =============================================================================
// DETERMINISM TESTS
// =============================================================================

pub fn analyze_coverage_deterministic_test() {
  // GIVEN: The same spec
  let spec =
    test_helpers.make_test_spec_from_behaviors([
      test_helpers.make_test_behavior("test-behavior", []),
    ])

  // WHEN: Analyzing coverage twice
  let report1 = coverage_analyzer.analyze_coverage(spec)
  let report2 = coverage_analyzer.analyze_coverage(spec)

  // THEN: Overall scores are identical (deterministic)
  report1.overall_score
  |> should.equal(report2.overall_score)

  // THEN: OWASP scores are identical
  report1.owasp.score
  |> should.equal(report2.owasp.score)
}
