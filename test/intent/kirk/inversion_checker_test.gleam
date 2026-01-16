//// Test coverage for intent/kirk/inversion_checker.gleam
////
//// Tests inversion thinking analysis (security, usability, integration):
//// - Detection of missing security inversions (auth, SQL injection, XSS)
//// - Detection of missing usability inversions (404, validation)
//// - Detection of missing integration inversions (idempotency, timeouts)
//// - Coverage detection via behavior names, intents, statuses, anti-patterns, rules
//// - Score calculation: (total - gaps) / total * 100
//// - Suggestion generation (max 10: 5 security + 3 usability + 2 integration)
////
//// DbC Postconditions Verified:
//// - score in range [0.0, 100.0]
//// - Each gap has non-empty what_could_fail explanation
//// - suggested_behaviors limited to top 10
//// - Severity assignments match spec (sql-injection = Critical, etc)

import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/string
import gleeunit/should
import intent/kirk/inversion_checker
import intent/types.{
  type AntiPattern, type Rule, type RuleCheck, AntiPattern, Rule, RuleCheck,
  Spec,
}
import test_helpers

// =============================================================================
// EMPTY SPEC TESTS (All inversions missing)
// =============================================================================

pub fn analyze_inversions_empty_spec_test() {
  // GIVEN: An empty spec with no behaviors
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Score is 0% (no coverage)
  report.score
  |> should.equal(0.0)

  // THEN: DbC postcondition - score in [0.0, 100.0]
  { report.score >=. 0.0 && report.score <=. 100.0 }
  |> should.be_true()

  // THEN: All three categories have gaps
  list.is_empty(report.security_gaps)
  |> should.be_false()

  list.is_empty(report.usability_gaps)
  |> should.be_false()

  list.is_empty(report.integration_gaps)
  |> should.be_false()
}

pub fn analyze_inversions_empty_spec_has_suggestions_test() {
  // GIVEN: An empty spec
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Suggested behaviors are generated (max 10)
  { list.length(report.suggested_behaviors) <= 10 }
  |> should.be_true()

  // THEN: Suggestions are not empty
  list.is_empty(report.suggested_behaviors)
  |> should.be_false()
}

// =============================================================================
// SECURITY INVERSION TESTS
// =============================================================================

pub fn analyze_inversions_auth_bypass_test() {
  // GIVEN: A spec with a 401 behavior (tests auth failure)
  let auth_behavior =
    test_helpers.make_test_behavior_with_status("test-auth", 401, [])

  let spec = test_helpers.make_test_spec_from_behaviors([auth_behavior])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Score is higher than empty spec (some coverage)
  { report.score >. 0.0 }
  |> should.be_true()

  // THEN: Auth-related security gaps should be reduced
  // (auth-bypass, expired-token, invalid-token all use 401)
  let security_gap_count = list.length(report.security_gaps)
  { security_gap_count < 10 }
  |> should.be_true()
}

pub fn analyze_inversions_sql_injection_test() {
  // GIVEN: A spec with behavior intent mentioning "injection"
  let behaviors = [
    test_helpers.make_test_behavior("test-injection", []),
    test_helpers.make_test_behavior("normal-behavior", []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: sql-injection gap should NOT appear (covered by keyword match)
  let has_sql_injection =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "SQL injection in query parameters"
    })

  has_sql_injection
  |> should.be_false()
}

pub fn analyze_inversions_sql_injection_missing_test() {
  // GIVEN: A spec without any injection-related behaviors
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: sql-injection gap should appear
  let has_sql_injection =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "SQL injection in query parameters"
    })

  has_sql_injection
  |> should.be_true()
}

pub fn analyze_inversions_xss_payload_test() {
  // GIVEN: A spec with behavior mentioning "xss" in name
  let behaviors = [test_helpers.make_test_behavior("test-xss-attack", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: xss-payload gap should NOT appear (covered by keyword match)
  let has_xss =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "XSS payloads in user-controlled fields"
    })

  has_xss
  |> should.be_false()
}

pub fn analyze_inversions_rate_limit_test() {
  // GIVEN: A spec with 429 status behavior
  let behaviors = [
    test_helpers.make_test_behavior_with_status("rate-limited", 429, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: rate-limit-exceeded gap should NOT appear
  let has_rate_limit =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "Exceeding rate limits"
    })

  has_rate_limit
  |> should.be_false()
}

// =============================================================================
// USABILITY INVERSION TESTS
// =============================================================================

pub fn analyze_inversions_not_found_test() {
  // GIVEN: A spec with 404 behavior
  let behaviors = [
    test_helpers.make_test_behavior_with_status("resource-not-found", 404, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: not-found gap should NOT appear
  let has_not_found =
    list.any(report.usability_gaps, fn(gap) {
      gap.description == "Requesting non-existent resources"
    })

  has_not_found
  |> should.be_false()
}

pub fn analyze_inversions_not_found_missing_test() {
  // GIVEN: A spec without 404 behaviors
  let behaviors = [
    test_helpers.make_test_behavior_with_status("success", 200, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: not-found gap should appear
  let has_not_found =
    list.any(report.usability_gaps, fn(gap) {
      gap.description == "Requesting non-existent resources"
    })

  has_not_found
  |> should.be_true()
}

pub fn analyze_inversions_invalid_format_test() {
  // GIVEN: A spec with behavior containing "invalid" in name
  let behaviors = [test_helpers.make_test_behavior("test-invalid-data", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: invalid-format gap should NOT appear (partial keyword match)
  let has_invalid_format =
    list.any(report.usability_gaps, fn(gap) {
      gap.description == "Sending malformed request data"
    })

  has_invalid_format
  |> should.be_false()
}

pub fn analyze_inversions_duplicate_create_test() {
  // GIVEN: A spec with 409 conflict status
  let behaviors = [
    test_helpers.make_test_behavior_with_status("duplicate-user", 409, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: duplicate-create and concurrent-modify gaps should NOT appear
  let has_duplicate =
    list.any(report.usability_gaps, fn(gap) {
      gap.description == "Creating duplicate resources"
    })

  has_duplicate
  |> should.be_false()
}

// =============================================================================
// INTEGRATION INVERSION TESTS
// =============================================================================

pub fn analyze_inversions_timeout_handling_test() {
  // GIVEN: A spec with 504 timeout status
  let behaviors = [
    test_helpers.make_test_behavior_with_status("slow-operation", 504, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: timeout-handling gap should NOT appear
  let has_timeout =
    list.any(report.integration_gaps, fn(gap) {
      gap.description == "Long-running operation timeout"
    })

  has_timeout
  |> should.be_false()
}

pub fn analyze_inversions_method_not_allowed_test() {
  // GIVEN: A spec with 405 method not allowed status
  let behaviors = [
    test_helpers.make_test_behavior_with_status("wrong-method", 405, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: method-not-allowed gap should NOT appear
  let has_method_not_allowed =
    list.any(report.integration_gaps, fn(gap) {
      gap.description == "Using wrong HTTP method"
    })

  has_method_not_allowed
  |> should.be_false()
}

// =============================================================================
// ANTI-PATTERN COVERAGE TESTS
// =============================================================================

pub fn analyze_inversions_anti_pattern_coverage_test() {
  // GIVEN: A spec with anti-pattern mentioning "sql"
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let anti_patterns = [
    AntiPattern(
      name: "sql-injection-vulnerable",
      description: "Don't concatenate SQL strings",
      bad_example: json.null(),
      good_example: json.null(),
      why: "Prevents SQL injection attacks",
    ),
  ]

  let spec =
    Spec(
      ..test_helpers.make_test_spec_from_behaviors(behaviors),
      anti_patterns: anti_patterns,
    )

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: sql-injection gap should NOT appear (covered by anti-pattern)
  let has_sql_injection =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "SQL injection in query parameters"
    })

  has_sql_injection
  |> should.be_false()
}

pub fn analyze_inversions_rule_coverage_test() {
  // GIVEN: A spec with rule mentioning "xss"
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let rules = [
    Rule(
      name: "xss-protection",
      description: "Prevent XSS attacks",
      when: None,
      check: RuleCheck(
        body_must_not_contain: [],
        body_must_contain: [],
        fields_must_exist: [],
        fields_must_not_exist: [],
        header_must_exist: "",
        header_must_not_exist: "",
      ),
      example: json.null(),
    ),
  ]

  let spec =
    Spec(..test_helpers.make_test_spec_from_behaviors(behaviors), rules: rules)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: xss-payload gap should NOT appear (covered by rule)
  let has_xss =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "XSS payloads in user-controlled fields"
    })

  has_xss
  |> should.be_false()
}

// =============================================================================
// SCORE CALCULATION TESTS
// =============================================================================

pub fn analyze_inversions_score_calculation_test() {
  // GIVEN: A spec that covers some but not all inversions
  // Security: 10 total (auth-bypass, expired-token, invalid-token via 401)
  // Usability: 8 total (not-found via 404)
  // Integration: 6 total
  // Total: 24 inversions
  let behaviors = [
    test_helpers.make_test_behavior_with_status("auth-fail", 401, []),
    test_helpers.make_test_behavior_with_status("not-found", 404, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Score formula is: (total - gaps) / total * 100
  let total_gaps =
    list.length(report.security_gaps)
    + list.length(report.usability_gaps)
    + list.length(report.integration_gaps)

  // Total inversions = 10 security + 8 usability + 6 integration = 24
  let expected_total = 24

  // Expected score
  let expected_score =
    int.to_float(expected_total - total_gaps)
    /. int.to_float(expected_total)
    *. 100.0

  // Allow small floating point difference
  let diff = report.score -. expected_score
  let abs_diff = case diff >=. 0.0 {
    True -> diff
    False -> -1.0 *. diff
  }

  { abs_diff <. 0.01 }
  |> should.be_true()
}

pub fn analyze_inversions_full_coverage_test() {
  // GIVEN: A spec with behaviors covering all status codes
  let behaviors = [
    test_helpers.make_test_behavior_with_status("auth-fail", 401, []),
    test_helpers.make_test_behavior_with_status("forbidden", 403, []),
    test_helpers.make_test_behavior_with_status("bad-request", 400, []),
    test_helpers.make_test_behavior_with_status("not-found", 404, []),
    test_helpers.make_test_behavior_with_status("conflict", 409, []),
    test_helpers.make_test_behavior_with_status("payload-too-large", 413, []),
    test_helpers.make_test_behavior_with_status("unsupported-media", 415, []),
    test_helpers.make_test_behavior_with_status("rate-limited", 429, []),
    test_helpers.make_test_behavior_with_status("not-allowed", 405, []),
    test_helpers.make_test_behavior_with_status("timeout", 504, []),
    test_helpers.make_test_behavior_with_status("multi-status", 207, []),
    test_helpers.make_test_behavior_with_status("success", 200, []),
    test_helpers.make_test_behavior("sql-injection-test", []),
    test_helpers.make_test_behavior("xss-test", []),
    test_helpers.make_test_behavior("token-expiry", []),
    test_helpers.make_test_behavior("invalid-format", []),
    test_helpers.make_test_behavior("missing-required", []),
    test_helpers.make_test_behavior("empty-list", []),
    test_helpers.make_test_behavior("max-pagination", []),
    test_helpers.make_test_behavior("idempotency-test", []),
    test_helpers.make_test_behavior("version-check", []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Score should be high (>80%)
  { report.score >. 80.0 }
  |> should.be_true()
}

// =============================================================================
// SUGGESTIONS LIMIT TESTS
// =============================================================================

pub fn analyze_inversions_suggestions_limit_test() {
  // GIVEN: An empty spec (will generate many suggestions)
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: DbC postcondition - max 10 suggestions (5 security + 3 usability + 2 integration)
  { list.length(report.suggested_behaviors) <= 10 }
  |> should.be_true()
}

pub fn analyze_inversions_suggestions_categories_test() {
  // GIVEN: An empty spec
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Suggestions should have different categories
  let categories =
    list.map(report.suggested_behaviors, fn(s) { s.category })
    |> list.unique()

  // Should have at least 1 category (likely 3: security, usability, integration)
  { list.length(categories) >= 1 }
  |> should.be_true()
}

// =============================================================================
// SEVERITY ASSIGNMENT TESTS
// =============================================================================

pub fn analyze_inversions_severity_assignment_test() {
  // GIVEN: An empty spec (to see all gaps)
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: sql-injection gap should have Critical severity
  let sql_gap =
    list.find(report.security_gaps, fn(gap) {
      gap.description == "SQL injection in query parameters"
    })

  case sql_gap {
    Ok(gap) ->
      inversion_checker.severity_to_string(gap.severity)
      |> should.equal("critical")
    Error(_) -> should.fail()
  }
}

pub fn analyze_inversions_severity_xss_critical_test() {
  // GIVEN: An empty spec
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: xss-payload gap should have Critical severity
  let xss_gap =
    list.find(report.security_gaps, fn(gap) {
      gap.description == "XSS payloads in user-controlled fields"
    })

  case xss_gap {
    Ok(gap) ->
      inversion_checker.severity_to_string(gap.severity)
      |> should.equal("critical")
    Error(_) -> should.fail()
  }
}

pub fn analyze_inversions_severity_not_found_high_test() {
  // GIVEN: An empty spec
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: not-found gap should have High severity
  let not_found_gap =
    list.find(report.usability_gaps, fn(gap) {
      gap.description == "Requesting non-existent resources"
    })

  case not_found_gap {
    Ok(gap) ->
      inversion_checker.severity_to_string(gap.severity)
      |> should.equal("high")
    Error(_) -> should.fail()
  }
}

// =============================================================================
// FORMAT TESTS
// =============================================================================

pub fn format_report_test() {
  // GIVEN: A report from an empty spec
  let spec = test_helpers.make_test_spec_from_behaviors([])
  let report = inversion_checker.analyze_inversions(spec)

  // WHEN: Formatting the report
  let output = inversion_checker.format_report(report)

  // THEN: Output should not be empty
  { string.length(output) > 0 }
  |> should.be_true()

  // THEN: Should contain KIRK header
  string.contains(output, "KIRK")
  |> should.be_true()

  // THEN: Should contain score percentage
  string.contains(output, "%")
  |> should.be_true()
}

pub fn format_report_no_crash_test() {
  // GIVEN: A report with some gaps covered
  let behaviors = [
    test_helpers.make_test_behavior_with_status("auth", 401, []),
    test_helpers.make_test_behavior_with_status("not-found", 404, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)
  let report = inversion_checker.analyze_inversions(spec)

  // WHEN: Formatting the report
  let output = inversion_checker.format_report(report)

  // THEN: Should complete without error and produce output
  { string.length(output) > 0 }
  |> should.be_true()
}

// =============================================================================
// SEVERITY_TO_STRING TESTS
// =============================================================================

pub fn severity_to_string_test() {
  // GIVEN/WHEN/THEN: All severities convert correctly
  inversion_checker.severity_to_string(inversion_checker.Low)
  |> should.equal("low")

  inversion_checker.severity_to_string(inversion_checker.Medium)
  |> should.equal("medium")

  inversion_checker.severity_to_string(inversion_checker.High)
  |> should.equal("high")

  inversion_checker.severity_to_string(inversion_checker.Critical)
  |> should.equal("critical")
}

// =============================================================================
// POSTCONDITION VERIFICATION TESTS
// =============================================================================

pub fn analyze_inversions_gaps_have_explanations_test() {
  // GIVEN: An empty spec
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: DbC postcondition - each gap has non-empty what_could_fail
  let all_gaps =
    list.concat([
      report.security_gaps,
      report.usability_gaps,
      report.integration_gaps,
    ])

  let all_have_explanations =
    list.all(all_gaps, fn(gap) { string.length(gap.what_could_fail) > 0 })

  all_have_explanations
  |> should.be_true()
}
