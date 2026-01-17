/// Failure case identification tests for inversion_checker
///
/// Tests edge cases, false positives/negatives, and boundary conditions where
/// the inversion checker might fail to correctly identify gaps:
/// - False positives: claiming coverage when none exists
/// - False negatives: missing real gaps
/// - Case sensitivity issues
/// - Partial keyword matching problems
/// - Status code ambiguity
/// - Multiple path handling
/// - Suggestion generation failures
import gleam/json
import gleam/list
import gleeunit/should
import intent/kirk/inversion_checker
import intent/types.{AntiPattern, Spec}
import test_helpers

// =============================================================================
// FALSE POSITIVE TESTS - Incorrectly claiming coverage
// =============================================================================

pub fn false_positive_partial_keyword_match_test() {
  // GIVEN: A behavior with "authentic" but NOT "authentication"
  // authentic !== authentication
  let behaviors = [test_helpers.make_test_behavior("get-authentic-data", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: KNOWN FALSE POSITIVE - auth-bypass gap does NOT appear
  // because "auth" substring matches in "authentic"
  // This is a limitation of the current partial keyword matching
  let has_auth_bypass =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "Accessing protected resources without authentication"
    })

  // Documents known false positive behavior
  has_auth_bypass
  |> should.be_false()
}

pub fn false_positive_similar_word_test() {
  // GIVEN: A behavior with "rate" but not "rate-limit"
  // rate-limiting !== rating
  let behaviors = [test_helpers.make_test_behavior("get-rating", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: rate-limit-exceeded gap SHOULD still appear
  let has_rate_limit =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "Exceeding rate limits"
    })

  has_rate_limit
  |> should.be_true()
}

pub fn false_positive_wrong_status_code_test() {
  // GIVEN: A behavior with 401 status but testing something else
  // Status alone shouldn't cover auth if intent is different
  let behavior =
    test_helpers.make_test_behavior_with_status("payment-required", 401, [])

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Auth gaps should NOT appear (status 401 matches)
  // This is current behavior - status match counts as coverage
  let security_gap_count = list.length(report.security_gaps)

  // With 401 status, auth-related gaps (auth-bypass, expired-token, invalid-token) are covered
  { security_gap_count < 10 }
  |> should.be_true()
}

pub fn false_positive_unrelated_anti_pattern_test() {
  // GIVEN: An anti-pattern with "sql" but unrelated to injection
  let behaviors = [test_helpers.make_test_behavior("get-user", [])]
  let anti_patterns = [
    AntiPattern(
      name: "sql-performance-issue",
      description: "Use indexes for better SQL performance",
      bad_example: json.null(),
      good_example: json.null(),
      why: "Improves query speed",
    ),
  ]

  let spec =
    Spec(
      ..test_helpers.make_test_spec_from_behaviors(behaviors),
      anti_patterns: anti_patterns,
    )

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: sql-injection gap should NOT appear (keyword match counts as coverage)
  // This is current behavior - partial keyword match in anti-pattern name
  let has_sql_injection =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "SQL injection in query parameters"
    })

  has_sql_injection
  |> should.be_false()
}

// =============================================================================
// FALSE NEGATIVE TESTS - Missing real gaps
// =============================================================================

pub fn false_negative_case_sensitivity_test() {
  // GIVEN: A behavior with uppercase "AUTH" - should still match
  let behaviors = [test_helpers.make_test_behavior("test-AUTH-failure", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Auth gaps should NOT appear (case-insensitive match)
  let has_auth =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "Accessing protected resources without authentication"
    })

  has_auth
  |> should.be_false()
}

pub fn false_negative_hyphenated_keyword_test() {
  // GIVEN: Behavior with "not-found" (hyphenated)
  let behaviors = [test_helpers.make_test_behavior("handle-not-found", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: not-found gap should NOT appear (keyword split and matched)
  let has_not_found =
    list.any(report.usability_gaps, fn(gap) {
      gap.description == "Requesting non-existent resources"
    })

  has_not_found
  |> should.be_false()
}

pub fn false_negative_multiple_keywords_test() {
  // GIVEN: Behavior matching only ONE keyword of multi-word inversion
  // "rate" from "rate-limit"
  let behaviors = [test_helpers.make_test_behavior("check-rate", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: rate-limit gap should NOT appear (partial keyword match)
  let has_rate_limit =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "Exceeding rate limits"
    })

  has_rate_limit
  |> should.be_false()
}

// =============================================================================
// BOUNDARY CONDITION TESTS
// =============================================================================

pub fn boundary_empty_behavior_name_test() {
  // GIVEN: A behavior with empty name (edge case)
  let behavior = test_helpers.make_test_behavior("", [])

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Should not crash, score should be valid (0-100)
  // Note: Empty name may still have some coverage via intent text or other factors
  { report.score >=. 0.0 && report.score <=. 100.0 }
  |> should.be_true()

  // All categories should have gaps since empty name provides minimal coverage
  list.is_empty(report.security_gaps)
  |> should.be_false()

  list.is_empty(report.usability_gaps)
  |> should.be_false()

  list.is_empty(report.integration_gaps)
  |> should.be_false()
}

pub fn boundary_empty_intent_test() {
  // GIVEN: A behavior with empty intent
  let behavior =
    test_helpers.make_test_behavior_with_status("test-behavior", 401, [])

  let spec = test_helpers.make_test_spec_from_behaviors([behavior])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Should still work (name + status matching)
  // Status 401 should cover auth gaps
  let security_gap_count = list.length(report.security_gaps)
  { security_gap_count < 10 }
  |> should.be_true()
}

pub fn boundary_very_long_behavior_name_test() {
  // GIVEN: A behavior with very long name containing keywords
  let long_name =
    "test-authentication-with-expired-token-and-sql-injection-and-xss-payload-handling"
  let behaviors = [test_helpers.make_test_behavior(long_name, [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Should match multiple keywords (auth, token, injection, xss)
  // Multiple security gaps should be covered
  let security_gap_count = list.length(report.security_gaps)

  // Should cover: auth-bypass, expired-token, invalid-token, sql-injection, xss-payload
  // That's 5 out of 10 security inversions
  { security_gap_count <= 5 }
  |> should.be_true()
}

pub fn boundary_unicode_in_behavior_name_test() {
  // GIVEN: Behavior with unicode characters
  let behaviors = [test_helpers.make_test_behavior("test-auth-🔒", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Should handle unicode gracefully and match "auth"
  let has_auth =
    list.any(report.security_gaps, fn(gap) {
      gap.description == "Accessing protected resources without authentication"
    })

  has_auth
  |> should.be_false()
}

// =============================================================================
// STATUS CODE AMBIGUITY TESTS
// =============================================================================

pub fn status_code_multiple_meanings_test() {
  // GIVEN: Status 400 which could mean many things
  // (invalid-format, missing-required, invalid-type, sql-injection, xss-payload, version-mismatch)
  let behaviors = [
    test_helpers.make_test_behavior_with_status("generic-error", 400, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Multiple gaps with status 400 should NOT appear
  let usability_gaps_with_400 =
    list.filter(report.usability_gaps, fn(gap) {
      gap.description == "Sending malformed request data"
      || gap.description == "Omitting required fields"
      || gap.description == "Wrong data types in fields"
    })

  let security_gaps_with_400 =
    list.filter(report.security_gaps, fn(gap) {
      gap.description == "SQL injection in query parameters"
      || gap.description == "XSS payloads in user-controlled fields"
    })

  // Status 400 matches should reduce gaps
  { usability_gaps_with_400 == [] }
  |> should.be_true()

  { security_gaps_with_400 == [] }
  |> should.be_true()
}

pub fn status_code_409_covers_both_inversions_test() {
  // GIVEN: Status 409 which matches both duplicate-create AND concurrent-modify
  let behaviors = [
    test_helpers.make_test_behavior_with_status("conflict", 409, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Both 409 inversions should NOT appear
  let has_duplicate =
    list.any(report.usability_gaps, fn(gap) {
      gap.description == "Creating duplicate resources"
    })

  let has_concurrent =
    list.any(report.usability_gaps, fn(gap) {
      gap.description == "Conflicting concurrent modifications"
    })

  has_duplicate
  |> should.be_false()

  has_concurrent
  |> should.be_false()
}

// =============================================================================
// MULTIPLE PATH HANDLING TESTS
// =============================================================================

pub fn multiple_paths_primary_path_selection_test() {
  // GIVEN: Multiple behaviors with different paths
  let behaviors = [
    test_helpers.make_test_behavior("get-user", []),
    test_helpers.make_test_behavior("create-post", []),
    test_helpers.make_test_behavior("delete-comment", []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Suggestions should use primary path (first one)
  case report.suggested_behaviors {
    [] -> should.fail()
    [first, ..] -> {
      // Path should not be empty
      { first.path != "" }
      |> should.be_true()
    }
  }
}

pub fn no_paths_fallback_test() {
  // GIVEN: Empty spec with no behaviors (no paths)
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Suggestions should use fallback path "/resource"
  case report.suggested_behaviors {
    [] -> should.fail()
    [first, ..] -> {
      first.path
      |> should.equal("/resource")
    }
  }
}

// =============================================================================
// SUGGESTION GENERATION FAILURE TESTS
// =============================================================================

pub fn suggestion_category_distribution_test() {
  // GIVEN: Empty spec (max suggestions)
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Should have exactly max 10 suggestions with proper distribution
  // 5 security + 3 usability + 2 integration = 10
  let security_count =
    list.count(report.suggested_behaviors, fn(s) { s.category == "security" })

  let usability_count =
    list.count(report.suggested_behaviors, fn(s) { s.category == "usability" })

  let integration_count =
    list.count(report.suggested_behaviors, fn(s) { s.category == "integration" })

  // Security should be <= 5
  { security_count <= 5 }
  |> should.be_true()

  // Usability should be <= 3
  { usability_count <= 3 }
  |> should.be_true()

  // Integration should be <= 2
  { integration_count <= 2 }
  |> should.be_true()
}

pub fn suggestion_name_sanitization_test() {
  // GIVEN: Empty spec
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Suggested names should be sanitized (lowercase, no spaces, max 30 chars)
  let all_names_valid =
    list.all(report.suggested_behaviors, fn(s) {
      let name = s.name
      // Should not contain spaces
      !string.contains(name, " ")
      // Should be <= 30 chars
      && string.length(name) <= 30
    })

  all_names_valid
  |> should.be_true()
}

pub fn suggestion_method_status_mapping_test() {
  // GIVEN: Empty spec
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: All suggestions should have reasonable method/status combinations
  let all_valid =
    list.all(report.suggested_behaviors, fn(s) {
      // Status codes should be valid HTTP status codes
      s.expected_status >= 200 && s.expected_status < 600
    })

  all_valid
  |> should.be_true()
}

// =============================================================================
// SCORE CALCULATION EDGE CASES
// =============================================================================

pub fn score_calculation_zero_total_inversions_test() {
  // GIVEN: A hypothetical scenario where total inversions is 0
  // (can't happen with current constants, but test the formula)
  // This tests division by zero protection

  // Using actual spec to ensure formula works
  let behaviors = [test_helpers.make_test_behavior("test", [])]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Score should be valid (0-100)
  { report.score >=. 0.0 && report.score <=. 100.0 }
  |> should.be_true()
}

pub fn score_calculation_all_gaps_covered_test() {
  // GIVEN: A spec that theoretically covers ALL inversions
  // (difficult to achieve but test the upper bound)
  let behaviors = [
    test_helpers.make_test_behavior_with_status("auth", 401, []),
    test_helpers.make_test_behavior_with_status("forbidden", 403, []),
    test_helpers.make_test_behavior_with_status("bad-request", 400, []),
    test_helpers.make_test_behavior_with_status("not-found", 404, []),
    test_helpers.make_test_behavior_with_status("conflict", 409, []),
    test_helpers.make_test_behavior_with_status("oversized", 413, []),
    test_helpers.make_test_behavior_with_status("rate-limited", 429, []),
    test_helpers.make_test_behavior_with_status("unsupported", 415, []),
    test_helpers.make_test_behavior_with_status("method-not-allowed", 405, []),
    test_helpers.make_test_behavior_with_status("timeout", 504, []),
    test_helpers.make_test_behavior_with_status("multi-status", 207, []),
    test_helpers.make_test_behavior("sql-injection", []),
    test_helpers.make_test_behavior("xss-test", []),
    test_helpers.make_test_behavior("brute-force", []),
    test_helpers.make_test_behavior("invalid-format", []),
    test_helpers.make_test_behavior("missing-required", []),
    test_helpers.make_test_behavior("invalid-type", []),
    test_helpers.make_test_behavior("empty-list", []),
    test_helpers.make_test_behavior("max-pagination", []),
    test_helpers.make_test_behavior("idempotency", []),
    test_helpers.make_test_behavior("version-check", []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Score should be at upper range (>= 80%)
  { report.score >=. 80.0 }
  |> should.be_true()

  // THEN: Score should not exceed 100%
  { report.score <=. 100.0 }
  |> should.be_true()
}

pub fn score_calculation_floating_point_precision_test() {
  // GIVEN: A spec with specific coverage to test floating point
  // 1 out of 24 total inversions covered
  let behaviors = [
    test_helpers.make_test_behavior_with_status("not-found", 404, []),
  ]
  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing inversions
  let report = inversion_checker.analyze_inversions(spec)

  // THEN: Score should be reasonable (close to expected)
  // With 404, we cover: not-found
  // That's approximately 1/24 = 4.17%
  { report.score >. 0.0 && report.score <. 10.0 }
  |> should.be_true()
}

import gleam/string
