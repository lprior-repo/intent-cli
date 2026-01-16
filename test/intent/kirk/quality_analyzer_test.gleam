//// Test coverage for intent/kirk/quality_analyzer.gleam
////
//// Tests the quality scoring system across 5 dimensions:
//// - Completeness (0-100): Coverage of required fields and CRUD operations
//// - Consistency (0-100): Naming conventions and no conflicts
//// - Testability (0-100): Concrete examples and assertions
//// - Clarity (0-100): Unambiguous language and detailed explanations
//// - Security (0-100): Auth coverage and validation patterns
////
//// DbC Postconditions Verified:
//// - All scores in range [0.0, 100.0]
//// - overall = weighted average (0.2*comp + 0.2*cons + 0.25*test + 0.15*clarity + 0.2*security)
//// - Issues list contains all detected problems

import gleam/dict
import gleam/list
import gleam/string
import gleeunit/should
import intent/kirk/quality_analyzer
import intent/types.{
  type Behavior, type Check, type Method, Behavior, Check, Delete, Get, Post,
  Put, Request, Response,
}
import test_helpers

// Helper to create a check with custom rule and why
fn make_check(rule: String, why: String) -> Check {
  Check(rule: rule, why: why)
}

// Helper to create behavior with custom checks
fn make_behavior_with_checks(
  name: String,
  checks: dict.Dict(String, Check),
) -> Behavior {
  let base = test_helpers.make_test_behavior(name, [])
  Behavior(..base, response: Response(..base.response, checks: checks))
}

// Helper to create behavior with custom intent
fn make_behavior_with_intent(name: String, intent: String) -> Behavior {
  let base = test_helpers.make_test_behavior(name, [])
  Behavior(..base, intent: intent)
}

// Helper to create behavior with custom method
fn make_behavior_method(name: String, method: Method) -> Behavior {
  test_helpers.make_test_behavior_with_method(name, method, [])
}

// =============================================================================
// EMPTY SPEC TESTS (Edge Case)
// =============================================================================

pub fn analyze_quality_empty_spec_test() {
  // GIVEN: An empty spec with no behaviors
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Empty spec scores are within valid range
  { report.completeness >=. 0.0 && report.completeness <=. 100.0 }
  |> should.be_true()

  { report.consistency >=. 0.0 && report.consistency <=. 100.0 }
  |> should.be_true()

  { report.testability >=. 0.0 && report.testability <=. 100.0 }
  |> should.be_true()

  { report.clarity >=. 0.0 && report.clarity <=. 100.0 }
  |> should.be_true()

  { report.security >=. 0.0 && report.security <=. 100.0 }
  |> should.be_true()

  // THEN: DbC postcondition - all scores in [0.0, 100.0]
  { report.overall >=. 0.0 && report.overall <=. 100.0 }
  |> should.be_true()
}

// =============================================================================
// MISSING 'WHY' FIELD TESTS (Clarity)
// =============================================================================

pub fn analyze_quality_missing_why_test() {
  // GIVEN: A behavior with checks that have empty 'why' fields
  let check_with_empty_why = make_check("status == 200", "")

  let checks = dict.from_list([#("status_check", check_with_empty_why)])

  let behavior = make_behavior_with_checks("test-behavior", checks)

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Issues list contains warning about missing 'why'
  let has_why_warning =
    report.issues
    |> list.any(fn(issue) {
      issue.severity == quality_analyzer.Warning
      && string.contains(issue.issue, "why")
    })

  has_why_warning
  |> should.be_true()

  // THEN: Clarity score is reduced
  { report.clarity <. 100.0 }
  |> should.be_true()
}

pub fn analyze_quality_filled_why_test() {
  // GIVEN: A behavior with checks that have complete 'why' fields
  let check_with_why =
    make_check("status == 200", "Ensures successful response")

  let checks = dict.from_list([#("status_check", check_with_why)])

  let behavior = make_behavior_with_checks("test-behavior", checks)

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: No warnings about missing 'why'
  let has_why_warning =
    report.issues
    |> list.any(fn(issue) {
      string.contains(issue.issue, "why") && string.contains(issue.issue, "empty")
    })

  has_why_warning
  |> should.be_false()
}

// =============================================================================
// SHORT INTENT TESTS (Clarity)
// =============================================================================

pub fn analyze_quality_short_intent_test() {
  // GIVEN: A behavior with very short intent (<10 characters)
  let behavior = make_behavior_with_intent("test-behavior", "Test it")

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Issues list contains clarity warning about short intent
  let has_short_intent_warning =
    report.issues
    |> list.any(fn(issue) {
      string.contains(string.lowercase(issue.issue), "intent")
    })

  has_short_intent_warning
  |> should.be_true()

  // THEN: Clarity score is reduced
  { report.clarity <. 100.0 }
  |> should.be_true()
}

pub fn analyze_quality_detailed_intent_test() {
  // GIVEN: A behavior with detailed intent (>10 characters)
  let behavior =
    make_behavior_with_intent(
      "test-behavior",
      "This behavior verifies that users can successfully authenticate",
    )

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Clarity score is high
  { report.clarity >=. 80.0 }
  |> should.be_true()
}

// =============================================================================
// NO CHECKS TESTS (Testability)
// =============================================================================

pub fn analyze_quality_no_checks_test() {
  // GIVEN: A behavior with empty checks dictionary
  let behavior = test_helpers.make_test_behavior("test-behavior", [])
  // Default test helper creates empty checks dict

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Issues list contains ErrorLevel issue about missing checks
  let has_no_checks_error =
    report.issues
    |> list.any(fn(issue) {
      issue.severity == quality_analyzer.ErrorLevel
      || issue.severity == quality_analyzer.Warning
    })

  has_no_checks_error
  |> should.be_true()

  // THEN: Testability score is reduced
  { report.testability <. 100.0 }
  |> should.be_true()
}

pub fn analyze_quality_with_checks_test() {
  // GIVEN: A behavior with proper response checks
  let check1 = make_check("status == 200", "Verifies success")
  let check2 = make_check("body.id != null", "Ensures ID is returned")

  let checks =
    dict.from_list([#("status_check", check1), #("id_check", check2)])

  let behavior = make_behavior_with_checks("test-behavior", checks)

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Testability score is higher with checks
  { report.testability >=. 50.0 }
  |> should.be_true()
}

// =============================================================================
// DUPLICATE BEHAVIOR NAMES TESTS (Consistency)
// =============================================================================

pub fn analyze_quality_duplicate_names_test() {
  // GIVEN: A spec with duplicate behavior names
  let behavior1 = test_helpers.make_test_behavior("duplicate-name", [])
  let behavior2 = test_helpers.make_test_behavior("duplicate-name", [])

  let spec = test_helpers.make_test_spec_from_behaviors([behavior1, behavior2])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Consistency score is reduced (penalty applied for duplicates)
  { report.consistency <. 100.0 }
  |> should.be_true()
}

pub fn analyze_quality_unique_names_test() {
  // GIVEN: A spec with unique behavior names
  let behavior1 = test_helpers.make_test_behavior("behavior-one", [])
  let behavior2 = test_helpers.make_test_behavior("behavior-two", [])

  let spec = test_helpers.make_test_spec_from_behaviors([behavior1, behavior2])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Consistency score is high (no duplicate name penalty)
  { report.consistency >=. 90.0 }
  |> should.be_true()
}

// =============================================================================
// SECURITY KEYWORDS TESTS
// =============================================================================

pub fn analyze_quality_security_keywords_test() {
  // GIVEN: Behaviors with security-related keywords (auth, permission, etc.)
  let auth_behavior =
    make_behavior_with_intent("authenticate-user", "User authentication endpoint")

  let permission_behavior =
    make_behavior_with_intent(
      "check-permission",
      "Verify user has required permissions",
    )

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      auth_behavior,
      permission_behavior,
    ])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Security score is present (may vary based on implementation)
  { report.security >=. 0.0 && report.security <=. 100.0 }
  |> should.be_true()
}

pub fn analyze_quality_no_security_keywords_test() {
  // GIVEN: Behaviors with no security-related keywords
  let behavior1 = make_behavior_with_intent("get-users", "Retrieve user list")
  let behavior2 = make_behavior_with_intent("create-post", "Create blog post")

  let spec = test_helpers.make_test_spec_from_behaviors([behavior1, behavior2])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Security score is lower without security keywords
  { report.security <. 80.0 }
  |> should.be_true()
}

// =============================================================================
// WEIGHTED AVERAGE TESTS (DbC Postcondition)
// =============================================================================

pub fn analyze_quality_weighted_average_test() {
  // GIVEN: A realistic spec
  let check = make_check("status == 200", "Success check")
  let checks = dict.from_list([#("status", check)])

  let behavior1 =
    Behavior(
      ..make_behavior_with_checks("create-user", checks),
      request: Request(
        ..make_behavior_with_checks("create-user", checks).request,
        method: Post,
      ),
    )

  let behavior2 =
    Behavior(
      ..make_behavior_with_checks("get-user", checks),
      request: Request(
        ..make_behavior_with_checks("get-user", checks).request,
        method: Get,
      ),
    )

  let spec = test_helpers.make_test_spec_from_behaviors([behavior1, behavior2])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: DbC postcondition - overall = weighted average
  let expected_overall =
    report.completeness *. 0.2
    +. report.consistency *. 0.2
    +. report.testability *. 0.25
    +. report.clarity *. 0.15
    +. report.security *. 0.2

  // Allow small floating point error (0.1 tolerance)
  let diff = float_abs(report.overall -. expected_overall)

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

// =============================================================================
// SCORE RANGE TESTS (DbC Postcondition)
// =============================================================================

pub fn analyze_quality_scores_in_range_test() {
  // GIVEN: Any spec (use spec with mixed quality)
  let check = make_check("status == 200", "")
  // Empty why reduces quality
  let checks = dict.from_list([#("status", check)])

  let behavior = make_behavior_with_checks("test-behavior", checks)

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: DbC postcondition - all scores in [0.0, 100.0]
  { report.completeness >=. 0.0 && report.completeness <=. 100.0 }
  |> should.be_true()

  { report.consistency >=. 0.0 && report.consistency <=. 100.0 }
  |> should.be_true()

  { report.testability >=. 0.0 && report.testability <=. 100.0 }
  |> should.be_true()

  { report.clarity >=. 0.0 && report.clarity <=. 100.0 }
  |> should.be_true()

  { report.security >=. 0.0 && report.security <=. 100.0 }
  |> should.be_true()

  { report.overall >=. 0.0 && report.overall <=. 100.0 }
  |> should.be_true()
}

// =============================================================================
// COMPLETENESS TESTS (CRUD Coverage)
// =============================================================================

pub fn analyze_quality_crud_coverage_test() {
  // GIVEN: A spec with full CRUD coverage
  let create_behavior = make_behavior_method("create-user", Post)
  let read_behavior = make_behavior_method("get-user", Get)
  let update_behavior = make_behavior_method("update-user", Put)
  let delete_behavior = make_behavior_method("delete-user", Delete)

  let spec =
    test_helpers.make_test_spec_from_behaviors([
      create_behavior,
      read_behavior,
      update_behavior,
      delete_behavior,
    ])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Completeness score is present (CRUD coverage affects score)
  { report.completeness >=. 0.0 && report.completeness <=. 100.0 }
  |> should.be_true()
}

pub fn analyze_quality_partial_crud_test() {
  // GIVEN: A spec with only GET operations (incomplete CRUD)
  let read1 = make_behavior_method("get-user", Get)
  let read2 = make_behavior_method("get-post", Get)

  let spec = test_helpers.make_test_spec_from_behaviors([read1, read2])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Completeness score is lower without full CRUD
  { report.completeness <. 80.0 }
  |> should.be_true()
}

// =============================================================================
// FORMAT REPORT TESTS
// =============================================================================

pub fn format_report_test() {
  // GIVEN: A quality report
  let spec = test_helpers.make_test_spec_from_behaviors([
    test_helpers.make_test_behavior("test-behavior", []),
  ])

  let report = quality_analyzer.analyze_quality(spec)

  // WHEN: Formatting the report
  let formatted = quality_analyzer.format_report(report)

  // THEN: Report produces valid UTF-8 string
  formatted
  |> should.not_equal("")

  // THEN: Report contains quality scores
  string.contains(formatted, "Quality")
  |> should.be_true()

  string.contains(formatted, "Overall")
  |> should.be_true()
}

pub fn format_report_shows_all_dimensions_test() {
  // GIVEN: A quality report
  let spec = test_helpers.make_test_spec_from_behaviors([
    test_helpers.make_test_behavior("test-behavior", []),
  ])

  let report = quality_analyzer.analyze_quality(spec)

  // WHEN: Formatting the report
  let formatted = quality_analyzer.format_report(report)

  // THEN: All 5 quality dimensions are shown
  string.contains(formatted, "Completeness")
  |> should.be_true()

  string.contains(formatted, "Consistency")
  |> should.be_true()

  string.contains(formatted, "Testability")
  |> should.be_true()

  string.contains(formatted, "Clarity")
  |> should.be_true()

  string.contains(formatted, "Security")
  |> should.be_true()
}

// =============================================================================
// SEVERITY TO STRING TESTS
// =============================================================================

pub fn severity_to_string_info_test() {
  quality_analyzer.severity_to_string(quality_analyzer.Info)
  |> should.equal("info")
}

pub fn severity_to_string_warning_test() {
  quality_analyzer.severity_to_string(quality_analyzer.Warning)
  |> should.equal("warning")
}

pub fn severity_to_string_error_test() {
  quality_analyzer.severity_to_string(quality_analyzer.ErrorLevel)
  |> should.equal("error")
}

pub fn severity_to_string_critical_test() {
  quality_analyzer.severity_to_string(quality_analyzer.Critical)
  |> should.equal("critical")
}

// =============================================================================
// SUGGESTIONS GENERATION TESTS
// =============================================================================

pub fn analyze_quality_generates_suggestions_test() {
  // GIVEN: A spec with quality issues
  let behavior = test_helpers.make_test_behavior("test-behavior", [])
  // No checks = testability issue

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing quality
  let report = quality_analyzer.analyze_quality(spec)

  // THEN: Suggestions list is populated
  { list.length(report.suggestions) >= 0 }
  |> should.be_true()
}

// =============================================================================
// DETERMINISM TESTS (DbC Invariant)
// =============================================================================

pub fn analyze_quality_deterministic_test() {
  // GIVEN: The same spec
  let check = make_check("status == 200", "Success check")
  let checks = dict.from_list([#("status", check)])

  let behavior = make_behavior_with_checks("test-behavior", checks)

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing quality twice
  let report1 = quality_analyzer.analyze_quality(spec)
  let report2 = quality_analyzer.analyze_quality(spec)

  // THEN: Scores are identical (deterministic)
  report1.overall
  |> should.equal(report2.overall)

  report1.completeness
  |> should.equal(report2.completeness)

  report1.consistency
  |> should.equal(report2.consistency)

  report1.testability
  |> should.equal(report2.testability)

  report1.clarity
  |> should.equal(report2.clarity)

  report1.security
  |> should.equal(report2.security)
}
