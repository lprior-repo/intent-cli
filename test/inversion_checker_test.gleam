//// Comprehensive tests for the KIRK inversion checker module
//// Tests security, usability, and integration gap detection
//// "Invert, always invert" - Tests what SHOULD fail but isn't tested

import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/string
import gleeunit/should
import intent/kirk/inversion_checker.{
  Critical, High,
  analyze_inversions,
}
import intent/types.{
  type AntiPattern, type Behavior, type Rule, type Spec, AIHints,
  AntiPattern, Behavior, Config, Feature, ImplementationHints, Request, Response,
  Rule, RuleCheck, SecurityHints, Spec, When,
}

// ============================================================================
// Test Helpers
// ============================================================================

/// Create a minimal behavior with customizable properties
fn make_test_behavior(
  name: String,
  intent: String,
  status: Int,
  method: types.Method,
  path: String,
) -> Behavior {
  Behavior(
    name: name,
    intent: intent,
    notes: "",
    requires: [],
    tags: [],
    request: Request(
      method: method,
      path: path,
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: Response(
      status: status,
      example: json.null(),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

/// Create a minimal spec with given behaviors
fn make_test_spec(behaviors: List(Behavior)) -> Spec {
  let feature = Feature(name: "Test", description: "Test feature", behaviors: behaviors)

  Spec(
    name: "Test Spec",
    description: "Test spec for inversion checker",
    audience: "developers",
    version: "1.0.0",
    success_criteria: [],
    config: Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
    ),
    features: [feature],
    rules: [],
    anti_patterns: [],
    ai_hints: AIHints(
      implementation: ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: SecurityHints(
        password_hashing: "",
        jwt_algorithm: "",
        jwt_expiry: "",
        rate_limiting: "",
      ),
      pitfalls: [],
      codebase: None,
    ),
  )
}

/// Create a spec with anti-patterns
fn make_spec_with_anti_patterns(
  behaviors: List(Behavior),
  anti_patterns: List(AntiPattern),
) -> Spec {
  Spec(..make_test_spec(behaviors), anti_patterns: anti_patterns)
}

/// Create a spec with rules
fn make_spec_with_rules(behaviors: List(Behavior), rules: List(Rule)) -> Spec {
  Spec(..make_test_spec(behaviors), rules: rules)
}

/// Create a test anti-pattern
fn make_anti_pattern(name: String) -> AntiPattern {
  AntiPattern(
    name: name,
    description: "Test anti-pattern: " <> name,
    bad_example: json.null(),
    good_example: json.null(),
    why: "Test reason",
  )
}

/// Create a test rule
fn make_rule(name: String) -> Rule {
  Rule(
    name: name,
    description: "Test rule: " <> name,
    when: When(status: "*", method: types.Get, path: "*"),
    check: RuleCheck(
      body_must_not_contain: [],
      body_must_contain: [],
      fields_must_exist: [],
      fields_must_not_exist: [],
      header_must_exist: "",
      header_must_not_exist: "",
    ),
    example: json.null(),
  )
}

// ============================================================================
// Empty Spec Tests
// ============================================================================

pub fn inversion_empty_spec_all_gaps_test() {
  // Empty spec should have maximum gaps
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  // Should have security gaps (10 total inversions)
  list.is_empty(report.security_gaps)
  |> should.be_false

  // Should have usability gaps (8 total inversions)
  list.is_empty(report.usability_gaps)
  |> should.be_false

  // Should have integration gaps (6 total inversions)
  list.is_empty(report.integration_gaps)
  |> should.be_false

  // Score should be 0 (nothing covered)
  report.score
  |> should.equal(0.0)
}

pub fn inversion_empty_spec_has_suggestions_test() {
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  // Should generate suggestions for gaps
  list.is_empty(report.suggested_behaviors)
  |> should.be_false
}

// ============================================================================
// Security Inversion Tests
// ============================================================================

pub fn inversion_auth_bypass_detected_by_name_test() {
  // Behavior with "auth" and "bypass" in name should cover auth-bypass
  let b = make_test_behavior("test-auth-bypass", "Test authentication bypass", 401, types.Get, "/protected")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  // Should NOT have auth-bypass gap (it's covered)
  report.security_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "without authentication") })
  |> should.be_false
}

pub fn inversion_auth_bypass_detected_by_status_test() {
  // Behavior with 401 status should help cover auth-bypass
  let b = make_test_behavior("unauthorized-access", "Test unauthorized", 401, types.Get, "/api")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  // 401 status covers several security inversions
  report.security_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "without authentication") })
  |> should.be_false
}

pub fn inversion_sql_injection_detected_by_intent_test() {
  // Behavior with "injection" in intent
  let b = make_test_behavior("malicious-input", "Test SQL injection in query params", 400, types.Get, "/search")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  // Should NOT have sql-injection gap
  report.security_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "SQL injection") })
  |> should.be_false
}

pub fn inversion_xss_detected_test() {
  // Behavior testing XSS
  let b = make_test_behavior("xss-payload-test", "Test XSS in user fields", 400, types.Post, "/users")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.security_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "XSS") })
  |> should.be_false
}

pub fn inversion_rate_limit_detected_test() {
  // Behavior with 429 status
  let b = make_test_behavior("too-many-requests", "Test rate limiting", 429, types.Get, "/api")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.security_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "rate limit") })
  |> should.be_false
}

pub fn inversion_security_via_anti_pattern_test() {
  // Anti-pattern with security keyword
  let anti = make_anti_pattern("sql-injection-prevention")
  let spec = make_spec_with_anti_patterns([], [anti])
  let report = analyze_inversions(spec)

  report.security_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "SQL injection") })
  |> should.be_false
}

pub fn inversion_security_via_rule_test() {
  // Rule with security keyword
  let rule = make_rule("auth-bypass-check")
  let spec = make_spec_with_rules([], [rule])
  let report = analyze_inversions(spec)

  report.security_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "without authentication") })
  |> should.be_false
}

// ============================================================================
// Usability Inversion Tests
// ============================================================================

pub fn inversion_not_found_detected_test() {
  // Behavior with 404 status
  let b = make_test_behavior("resource-not-found", "Test 404 response", 404, types.Get, "/users/999")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.usability_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "non-existent") })
  |> should.be_false
}

pub fn inversion_invalid_format_detected_test() {
  // Behavior with "invalid" in name
  let b = make_test_behavior("invalid-format-test", "Send malformed data", 400, types.Post, "/users")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.usability_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "malformed") })
  |> should.be_false
}

pub fn inversion_missing_required_detected_test() {
  // Behavior testing missing fields
  let b = make_test_behavior("missing-required-fields", "Test with missing required", 400, types.Post, "/users")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.usability_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "required fields") })
  |> should.be_false
}

pub fn inversion_duplicate_create_detected_test() {
  // Behavior with 409 Conflict status
  let b = make_test_behavior("duplicate-user", "Create existing user", 409, types.Post, "/users")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.usability_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "duplicate") })
  |> should.be_false
}

pub fn inversion_empty_list_detected_test() {
  // Behavior testing empty results
  let b = make_test_behavior("empty-list-pagination", "Test empty results", 200, types.Get, "/users?page=999")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.usability_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "Empty list") })
  |> should.be_false
}

// ============================================================================
// Integration Inversion Tests
// ============================================================================

pub fn inversion_idempotency_detected_test() {
  // Behavior testing idempotency
  let b = make_test_behavior("idempotency-test", "Test idempotency key", 200, types.Post, "/payments")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.integration_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "idempotency") })
  |> should.be_false
}

pub fn inversion_timeout_detected_test() {
  // Behavior with 504 status
  let b = make_test_behavior("gateway-timeout", "Test timeout handling", 504, types.Get, "/slow-endpoint")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.integration_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "timeout") })
  |> should.be_false
}

pub fn inversion_method_not_allowed_detected_test() {
  // Behavior with 405 status
  let b = make_test_behavior("wrong-method", "Test wrong HTTP method", 405, types.Post, "/readonly")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.integration_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "wrong HTTP method") })
  |> should.be_false
}

pub fn inversion_content_type_detected_test() {
  // Behavior with 415 status
  let b = make_test_behavior("unsupported-media", "Test content type", 415, types.Post, "/api")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.integration_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "content types") })
  |> should.be_false
}

// ============================================================================
// Gap Severity Tests
// ============================================================================

pub fn inversion_sql_injection_critical_severity_test() {
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  // SQL injection gap should be Critical
  report.security_gaps
  |> list.any(fn(gap) {
    string.contains(gap.description, "SQL injection") && gap.severity == Critical
  })
  |> should.be_true
}

pub fn inversion_xss_critical_severity_test() {
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  report.security_gaps
  |> list.any(fn(gap) {
    string.contains(gap.description, "XSS") && gap.severity == Critical
  })
  |> should.be_true
}

pub fn inversion_auth_bypass_critical_severity_test() {
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  report.security_gaps
  |> list.any(fn(gap) {
    string.contains(gap.description, "without authentication")
      && gap.severity == Critical
  })
  |> should.be_true
}

pub fn inversion_not_found_high_severity_test() {
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  report.usability_gaps
  |> list.any(fn(gap) {
    string.contains(gap.description, "non-existent") && gap.severity == High
  })
  |> should.be_true
}

// ============================================================================
// Score Calculation Tests
// ============================================================================

pub fn inversion_score_zero_empty_spec_test() {
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  report.score
  |> should.equal(0.0)
}

pub fn inversion_score_increases_with_coverage_test() {
  // Add behaviors that cover some inversions
  let behaviors = [
    make_test_behavior("auth-test", "Test auth bypass", 401, types.Get, "/api"),
    make_test_behavior("not-found", "Test 404", 404, types.Get, "/missing"),
    make_test_behavior("rate-limit", "Test limits", 429, types.Get, "/api"),
  ]
  let spec = make_test_spec(behaviors)
  let report = analyze_inversions(spec)

  // Score should be > 0 with some coverage
  { report.score >. 0.0 }
  |> should.be_true
}

pub fn inversion_score_max_with_full_coverage_test() {
  // Create behaviors covering ALL inversions
  // This is extensive because there are 24 total inversions
  let behaviors = [
    // Security inversions (10)
    make_test_behavior("test-auth-bypass", "auth bypass", 401, types.Get, "/api"),
    make_test_behavior("test-expired-token", "expired token", 401, types.Get, "/api"),
    make_test_behavior("test-invalid-token", "invalid token", 401, types.Get, "/api"),
    make_test_behavior("test-wrong-user-access", "wrong user", 403, types.Get, "/api"),
    make_test_behavior("test-privilege-escalation", "privilege", 403, types.Get, "/api"),
    make_test_behavior("test-sql-injection", "sql injection", 400, types.Get, "/api"),
    make_test_behavior("test-xss-payload", "xss payload", 400, types.Get, "/api"),
    make_test_behavior("test-oversized-request", "oversized", 413, types.Post, "/api"),
    make_test_behavior("test-rate-limit-exceeded", "rate limit", 429, types.Get, "/api"),
    make_test_behavior("test-brute-force-lockout", "brute force", 429, types.Post, "/api"),
    // Usability inversions (8)
    make_test_behavior("test-not-found", "not found", 404, types.Get, "/api"),
    make_test_behavior("test-invalid-format", "invalid format", 400, types.Post, "/api"),
    make_test_behavior("test-missing-required", "missing required", 400, types.Post, "/api"),
    make_test_behavior("test-invalid-type", "invalid type", 400, types.Post, "/api"),
    make_test_behavior("test-empty-list", "empty list", 200, types.Get, "/api"),
    make_test_behavior("test-max-pagination", "max pagination", 200, types.Get, "/api"),
    make_test_behavior("test-duplicate-create", "duplicate create", 409, types.Post, "/api"),
    make_test_behavior("test-concurrent-modify", "concurrent modify", 409, types.Put, "/api"),
    // Integration inversions (6)
    make_test_behavior("test-idempotency", "idempotency", 200, types.Post, "/api"),
    make_test_behavior("test-timeout-handling", "timeout handling", 504, types.Get, "/api"),
    make_test_behavior("test-partial-failure", "partial failure", 207, types.Post, "/api"),
    make_test_behavior("test-version-mismatch", "version mismatch", 400, types.Get, "/api"),
    make_test_behavior("test-content-negotiation", "content negotiation", 415, types.Post, "/api"),
    make_test_behavior("test-method-not-allowed", "method not allowed", 405, types.Post, "/api"),
  ]
  let spec = make_test_spec(behaviors)
  let report = analyze_inversions(spec)

  // Score should be 100 (all inversions covered)
  report.score
  |> should.equal(100.0)

  // No gaps should remain
  list.is_empty(report.security_gaps)
  |> should.be_true

  list.is_empty(report.usability_gaps)
  |> should.be_true

  list.is_empty(report.integration_gaps)
  |> should.be_true
}

// ============================================================================
// Suggested Behaviors Tests
// ============================================================================

pub fn inversion_suggestions_use_existing_paths_test() {
  // Suggestions should use paths from existing behaviors
  let b = make_test_behavior("get-user", "Get user", 200, types.Get, "/users/123")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  // Suggestions should exist
  list.is_empty(report.suggested_behaviors)
  |> should.be_false

  // At least some suggestions should use the existing path
  report.suggested_behaviors
  |> list.any(fn(sb) { string.contains(sb.path, "/users") })
  |> should.be_true
}

pub fn inversion_suggestions_have_category_test() {
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  // All suggestions should have a category
  report.suggested_behaviors
  |> list.all(fn(sb) { !string.is_empty(sb.category) })
  |> should.be_true
}

pub fn inversion_suggestions_have_intent_test() {
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  // All suggestions should have an intent
  report.suggested_behaviors
  |> list.all(fn(sb) { !string.is_empty(sb.intent) })
  |> should.be_true
}

// ============================================================================
// Gap Structure Tests
// ============================================================================

pub fn inversion_gap_has_category_test() {
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  // All gaps should have a category
  let all_gaps =
    list.concat([
      report.security_gaps,
      report.usability_gaps,
      report.integration_gaps,
    ])

  all_gaps
  |> list.all(fn(gap) { !string.is_empty(gap.category) })
  |> should.be_true
}

pub fn inversion_gap_has_what_could_fail_test() {
  let spec = make_test_spec([])
  let report = analyze_inversions(spec)

  // All gaps should have what_could_fail populated
  let all_gaps =
    list.concat([
      report.security_gaps,
      report.usability_gaps,
      report.integration_gaps,
    ])

  all_gaps
  |> list.all(fn(gap) { !string.is_empty(gap.what_could_fail) })
  |> should.be_true
}

// ============================================================================
// Multiple Features Tests
// ============================================================================

pub fn inversion_analyzes_all_features_test() {
  // Create spec with multiple features
  let f1 =
    Feature(
      name: "Auth",
      description: "Auth feature",
      behaviors: [
        make_test_behavior("auth-bypass-test", "auth bypass", 401, types.Get, "/api"),
      ],
    )
  let f2 =
    Feature(
      name: "Users",
      description: "Users feature",
      behaviors: [
        make_test_behavior("not-found-test", "not found", 404, types.Get, "/users"),
      ],
    )
  let spec =
    Spec(
      ..make_test_spec([]),
      features: [f1, f2],
    )

  let report = analyze_inversions(spec)

  // Should NOT have auth-bypass gap (covered by f1)
  report.security_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "without authentication") })
  |> should.be_false

  // Should NOT have not-found gap (covered by f2)
  report.usability_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "non-existent") })
  |> should.be_false
}

// ============================================================================
// Case Insensitivity Tests
// ============================================================================

pub fn inversion_case_insensitive_name_match_test() {
  // Uppercase behavior name should still match
  let b = make_test_behavior("AUTH-BYPASS-TEST", "Test auth", 401, types.Get, "/api")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.security_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "without authentication") })
  |> should.be_false
}

pub fn inversion_case_insensitive_intent_match_test() {
  // Mixed case intent should still match
  let b = make_test_behavior("test", "Test SQL INJECTION attempt", 400, types.Get, "/api")
  let spec = make_test_spec([b])
  let report = analyze_inversions(spec)

  report.security_gaps
  |> list.any(fn(gap) { string.contains(gap.description, "SQL injection") })
  |> should.be_false
}
