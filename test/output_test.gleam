//// Tests for the output module
//// Tests JSON and human-readable formatting of test results
//// Validates display of successes, failures, blocked behaviors, and summaries

import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import intent/anti_patterns
import intent/checker
import intent/http_client
import intent/output
import intent/types
import test_helpers.{make_test_behavior}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Factory Helpers
// ============================================================================

/// Create a minimal passing SpecResult
fn make_passing_result() -> output.SpecResult {
  output.SpecResult(
    pass: True,
    passed: 1,
    failed: 0,
    blocked: 0,
    total: 1,
    summary: "All tests passed",
    failures: [],
    blocked_behaviors: [],
    rule_violations: [],
    anti_patterns_detected: [],
  )
}

/// Create a minimal failing SpecResult
fn make_failing_result() -> output.SpecResult {
  output.SpecResult(
    pass: False,
    passed: 0,
    failed: 1,
    blocked: 0,
    total: 1,
    summary: "Tests failed",
    failures: [make_test_failure()],
    blocked_behaviors: [],
    rule_violations: [],
    anti_patterns_detected: [],
  )
}

/// Create a minimal BehaviorFailure
fn make_test_failure() -> output.BehaviorFailure {
  output.BehaviorFailure(
    feature: "Test Feature",
    behavior: "test-behavior",
    intent: "Demonstrate something",
    problems: [
      output.Problem(
        field: "$.status",
        rule: "equals 'success'",
        expected: "success",
        actual: "error",
        explanation: "Status field mismatch",
      ),
    ],
    request_sent: output.RequestSummary(
      method: "GET",
      url: "http://localhost:8080/test",
      headers: dict.new(),
    ),
    response_received: output.ResponseSummary(status: 200, body: json.null()),
    hint: "Check the field paths",
    see_also: [],
  )
}

/// Create a BlockedBehavior
fn make_blocked_behavior() -> output.BlockedBehavior {
  output.BlockedBehavior(
    behavior: "blocked-test",
    reason: "Requires 'setup' which failed",
    hint: "Fix 'setup' first",
  )
}

/// Create a RuleViolationGroup
fn make_rule_violation() -> output.RuleViolationGroup {
  output.RuleViolationGroup(
    rule: "no-plain-passwords",
    description: "Passwords must be hashed",
    violations: [
      output.BehaviorViolation(
        behavior: "create-user",
        violations: ["Found plain password in response"],
        response: Some(json.null()),
      ),
    ],
  )
}

/// Create an ExecutionResult for testing
fn make_execution_result(status: Int) -> http_client.ExecutionResult {
  http_client.ExecutionResult(
    status: status,
    headers: dict.new(),
    body: json.null(),
    raw_body: "{}",
    elapsed_ms: 100,
    request_method: types.Get,
    request_path: "/test",
  )
}

/// Create a ResponseCheckResult for testing
fn make_check_result(
  passed: List(checker.CheckResult),
  failed: List(checker.CheckResult),
  status_ok: Bool,
  status_expected: Int,
  status_actual: Int,
) -> checker.ResponseCheckResult {
  checker.ResponseCheckResult(
    passed: passed,
    failed: failed,
    status_ok: status_ok,
    status_expected: status_expected,
    status_actual: status_actual,
  )
}

// ============================================================================
// JSON Serialization Tests - SpecResult
// ============================================================================

pub fn spec_result_to_json_passing_test() {
  let result = make_passing_result()
  let json_output = output.spec_result_to_json(result)
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"pass\":true")
  |> should.be_true

  json_str
  |> string.contains("\"passed\":1")
  |> should.be_true
}

pub fn spec_result_to_json_failing_test() {
  let result = make_failing_result()
  let json_output = output.spec_result_to_json(result)
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"pass\":false")
  |> should.be_true

  json_str
  |> string.contains("\"failed\":1")
  |> should.be_true
}

pub fn spec_result_to_json_has_all_fields_test() {
  let result = make_passing_result()
  let json_output = output.spec_result_to_json(result)
  let json_str = json.to_string(json_output)

  // Verify all top-level fields are present
  json_str
  |> string.contains("\"pass\"")
  |> should.be_true

  json_str
  |> string.contains("\"score\"")
  |> should.be_true

  json_str
  |> string.contains("\"summary\"")
  |> should.be_true

  json_str
  |> string.contains("\"failures\"")
  |> should.be_true

  json_str
  |> string.contains("\"blocked\"")
  |> should.be_true

  json_str
  |> string.contains("\"rule_violations\"")
  |> should.be_true

  json_str
  |> string.contains("\"anti_patterns_detected\"")
  |> should.be_true
}

pub fn spec_result_to_json_score_fields_test() {
  let result =
    output.SpecResult(
      pass: False,
      passed: 5,
      failed: 2,
      blocked: 1,
      total: 8,
      summary: "Mixed results",
      failures: [],
      blocked_behaviors: [],
      rule_violations: [],
      anti_patterns_detected: [],
    )

  let json_output = output.spec_result_to_json(result)
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"passed\":5")
  |> should.be_true

  json_str
  |> string.contains("\"failed\":2")
  |> should.be_true

  json_str
  |> string.contains("\"blocked\":1")
  |> should.be_true

  json_str
  |> string.contains("\"total\":8")
  |> should.be_true
}

pub fn spec_result_to_json_with_failures_test() {
  let result = make_failing_result()
  let json_output = output.spec_result_to_json(result)
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"feature\":\"Test Feature\"")
  |> should.be_true

  json_str
  |> string.contains("\"behavior\":\"test-behavior\"")
  |> should.be_true

  json_str
  |> string.contains("\"intent\":\"Demonstrate something\"")
  |> should.be_true
}

pub fn spec_result_to_json_with_blocked_test() {
  let result =
    output.SpecResult(
      ..make_passing_result(),
      blocked: 1,
      blocked_behaviors: [make_blocked_behavior()],
    )

  let json_output = output.spec_result_to_json(result)
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"behavior\":\"blocked-test\"")
  |> should.be_true

  json_str
  |> string.contains("\"reason\":\"Requires 'setup' which failed\"")
  |> should.be_true
}

pub fn spec_result_to_json_with_rule_violations_test() {
  let result =
    output.SpecResult(
      ..make_failing_result(),
      rule_violations: [make_rule_violation()],
    )

  let json_output = output.spec_result_to_json(result)
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"rule\":\"no-plain-passwords\"")
  |> should.be_true

  json_str
  |> string.contains("\"description\":\"Passwords must be hashed\"")
  |> should.be_true
}

pub fn spec_result_to_json_with_anti_patterns_test() {
  let anti_pattern =
    anti_patterns.AntiPatternDetected(
      "plain-text-password",
      "Passwords should be hashed",
      "Found password field",
      json.object([#("password", json.string("plain"))]),
      json.object([#("password_hash", json.string("$2b$..."))]),
    )

  let result =
    output.SpecResult(
      ..make_failing_result(),
      anti_patterns_detected: [anti_pattern],
    )

  let json_output = output.spec_result_to_json(result)
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"pattern\":\"plain-text-password\"")
  |> should.be_true

  json_str
  |> string.contains("\"description\":\"Passwords should be hashed\"")
  |> should.be_true
}

// ============================================================================
// JSON Serialization Tests - BehaviorFailure
// ============================================================================

pub fn behavior_failure_to_json_complete_test() {
  let failure =
    output.BehaviorFailure(
      feature: "User Management",
      behavior: "create-user",
      intent: "Create a new user account",
      problems: [
        output.Problem(
          field: "$.email",
          rule: "matches email pattern",
          expected: "valid email",
          actual: "invalid",
          explanation: "Email format is incorrect",
        ),
      ],
      request_sent: output.RequestSummary(
        method: "POST",
        url: "http://localhost:8080/users",
        headers: dict.from_list([#("Content-Type", "application/json")]),
      ),
      response_received: output.ResponseSummary(
        status: 400,
        body: json.object([#("error", json.string("validation failed"))]),
      ),
      hint: "Check email validation",
      see_also: ["authenticate"],
    )

  let json_output =
    output.spec_result_to_json(
      output.SpecResult(..make_failing_result(), failures: [failure]),
    )
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"feature\":\"User Management\"")
  |> should.be_true

  json_str
  |> string.contains("\"method\":\"POST\"")
  |> should.be_true

  json_str
  |> string.contains("\"status\":400")
  |> should.be_true

  json_str
  |> string.contains("\"hint\":\"Check email validation\"")
  |> should.be_true

  json_str
  |> string.contains("\"see_also\"")
  |> should.be_true
}

pub fn behavior_failure_multiple_problems_test() {
  let failure =
    output.BehaviorFailure(
      ..make_test_failure(),
      problems: [
        output.Problem(
          field: "$.name",
          rule: "exists",
          expected: "present",
          actual: "missing",
          explanation: "Name field is required",
        ),
        output.Problem(
          field: "$.age",
          rule: "greater than 0",
          expected: "> 0",
          actual: "-5",
          explanation: "Age must be positive",
        ),
      ],
    )

  let json_output =
    output.spec_result_to_json(
      output.SpecResult(..make_failing_result(), failures: [failure]),
    )
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"field\":\"$.name\"")
  |> should.be_true

  json_str
  |> string.contains("\"field\":\"$.age\"")
  |> should.be_true
}

// ============================================================================
// Text Formatting Tests - SpecResult
// ============================================================================

pub fn spec_result_to_text_passing_test() {
  let result = make_passing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("PASS")
  |> should.be_true

  text
  |> string.contains("Passed: 1")
  |> should.be_true

  text
  |> string.contains("Failed: 0")
  |> should.be_true
}

pub fn spec_result_to_text_failing_test() {
  let result = make_failing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("FAIL")
  |> should.be_true

  text
  |> string.contains("Failed: 1")
  |> should.be_true
}

pub fn spec_result_to_text_shows_summary_test() {
  let result =
    output.SpecResult(
      ..make_passing_result(),
      summary: "Custom summary message",
    )
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("Custom summary message")
  |> should.be_true
}

pub fn spec_result_to_text_shows_score_test() {
  let result =
    output.SpecResult(
      pass: False,
      passed: 7,
      failed: 2,
      blocked: 1,
      total: 10,
      summary: "Mixed",
      failures: [],
      blocked_behaviors: [],
      rule_violations: [],
      anti_patterns_detected: [],
    )
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("Passed: 7")
  |> should.be_true

  text
  |> string.contains("Failed: 2")
  |> should.be_true

  text
  |> string.contains("Blocked: 1")
  |> should.be_true

  text
  |> string.contains("Total: 10")
  |> should.be_true
}

pub fn spec_result_to_text_no_failures_section_when_empty_test() {
  let result = make_passing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("FAILURES:")
  |> should.be_false
}

pub fn spec_result_to_text_shows_failures_section_test() {
  let result = make_failing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("FAILURES:")
  |> should.be_true

  text
  |> string.contains("[Test Feature]")
  |> should.be_true

  text
  |> string.contains("test-behavior")
  |> should.be_true
}

pub fn spec_result_to_text_shows_blocked_section_test() {
  let result =
    output.SpecResult(
      ..make_passing_result(),
      blocked: 1,
      blocked_behaviors: [make_blocked_behavior()],
    )
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("BLOCKED:")
  |> should.be_true

  text
  |> string.contains("blocked-test")
  |> should.be_true
}

pub fn spec_result_to_text_shows_rule_violations_section_test() {
  let result =
    output.SpecResult(
      ..make_failing_result(),
      rule_violations: [make_rule_violation()],
    )
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("RULE VIOLATIONS:")
  |> should.be_true

  text
  |> string.contains("no-plain-passwords")
  |> should.be_true
}

pub fn spec_result_to_text_shows_anti_patterns_section_test() {
  let anti_pattern =
    anti_patterns.AntiPatternDetected(
      "test-pattern",
      "Test description",
      "Found test",
      json.null(),
      json.null(),
    )

  let result =
    output.SpecResult(
      ..make_failing_result(),
      anti_patterns_detected: [anti_pattern],
    )
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("ANTI-PATTERNS DETECTED:")
  |> should.be_true
}

pub fn spec_result_to_text_no_blocked_section_when_empty_test() {
  let result = make_passing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("BLOCKED:")
  |> should.be_false
}

pub fn spec_result_to_text_no_rule_violations_when_empty_test() {
  let result = make_passing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("RULE VIOLATIONS:")
  |> should.be_false
}

pub fn spec_result_to_text_no_anti_patterns_when_empty_test() {
  let result = make_passing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("ANTI-PATTERNS:")
  |> should.be_false
}

// ============================================================================
// Text Formatting Tests - BehaviorFailure Details
// ============================================================================

pub fn behavior_failure_formatting_includes_intent_test() {
  let result = make_failing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("Intent: Demonstrate something")
  |> should.be_true
}

pub fn behavior_failure_formatting_includes_problems_test() {
  let result = make_failing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("Problems:")
  |> should.be_true

  text
  |> string.contains("$.status")
  |> should.be_true

  text
  |> string.contains("Expected: success")
  |> should.be_true

  text
  |> string.contains("Actual: error")
  |> should.be_true
}

pub fn behavior_failure_formatting_includes_request_info_test() {
  let result = make_failing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("Request: GET")
  |> should.be_true

  text
  |> string.contains("http://localhost:8080/test")
  |> should.be_true
}

pub fn behavior_failure_formatting_includes_response_status_test() {
  let result = make_failing_result()
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("Response: 200")
  |> should.be_true
}

pub fn behavior_failure_formatting_includes_hint_test() {
  let failure =
    output.BehaviorFailure(
      ..make_test_failure(),
      hint: "Try checking the API documentation",
    )

  let result = output.SpecResult(..make_failing_result(), failures: [failure])
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("Hint: Try checking the API documentation")
  |> should.be_true
}

pub fn behavior_failure_formatting_no_hint_when_empty_test() {
  let failure = output.BehaviorFailure(..make_test_failure(), hint: "")

  let result = output.SpecResult(..make_failing_result(), failures: [failure])
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("Hint:")
  |> should.be_false
}

pub fn behavior_failure_multiple_problems_formatted_test() {
  let failure =
    output.BehaviorFailure(
      ..make_test_failure(),
      problems: [
        output.Problem(
          field: "$.field1",
          rule: "rule1",
          expected: "val1",
          actual: "wrong1",
          explanation: "Field 1 problem",
        ),
        output.Problem(
          field: "$.field2",
          rule: "rule2",
          expected: "val2",
          actual: "wrong2",
          explanation: "Field 2 problem",
        ),
      ],
    )

  let result = output.SpecResult(..make_failing_result(), failures: [failure])
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("$.field1")
  |> should.be_true

  text
  |> string.contains("$.field2")
  |> should.be_true

  text
  |> string.contains("Field 1 problem")
  |> should.be_true

  text
  |> string.contains("Field 2 problem")
  |> should.be_true
}

// ============================================================================
// Text Formatting Tests - BlockedBehavior
// ============================================================================

pub fn blocked_behavior_formatting_test() {
  let blocked =
    output.BlockedBehavior(
      behavior: "dependent-test",
      reason: "Requires 'setup-test' which failed",
      hint: "Fix 'setup-test' before running this",
    )

  let result =
    output.SpecResult(
      ..make_passing_result(),
      blocked: 1,
      blocked_behaviors: [blocked],
    )
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("- dependent-test:")
  |> should.be_true

  text
  |> string.contains("Requires 'setup-test' which failed")
  |> should.be_true

  text
  |> string.contains("(Fix 'setup-test' before running this)")
  |> should.be_true
}

pub fn blocked_behavior_formatting_no_hint_test() {
  let blocked =
    output.BlockedBehavior(
      behavior: "dependent-test",
      reason: "Dependency failed",
      hint: "",
    )

  let result =
    output.SpecResult(
      ..make_passing_result(),
      blocked: 1,
      blocked_behaviors: [blocked],
    )
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("dependent-test:")
  |> should.be_true

  text
  |> string.contains("Dependency failed")
  |> should.be_true

  // No parentheses when hint is empty
  text
  |> string.contains("()")
  |> should.be_false
}

// ============================================================================
// Text Formatting Tests - RuleViolationGroup
// ============================================================================

pub fn rule_violation_formatting_test() {
  let violation =
    output.RuleViolationGroup(
      rule: "no-sensitive-data",
      description: "Must not expose sensitive fields",
      violations: [
        output.BehaviorViolation(
          behavior: "get-user",
          violations: ["Found ssn field", "Found credit_card field"],
          response: None,
        ),
      ],
    )

  let result =
    output.SpecResult(..make_failing_result(), rule_violations: [violation])
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("no-sensitive-data")
  |> should.be_true

  text
  |> string.contains("Must not expose sensitive fields")
  |> should.be_true

  text
  |> string.contains("get-user")
  |> should.be_true

  text
  |> string.contains("Found ssn field")
  |> should.be_true

  text
  |> string.contains("Found credit_card field")
  |> should.be_true
}

pub fn rule_violation_multiple_behaviors_test() {
  let violation =
    output.RuleViolationGroup(
      rule: "test-rule",
      description: "Test description",
      violations: [
        output.BehaviorViolation(
          behavior: "behavior1",
          violations: ["violation1"],
          response: None,
        ),
        output.BehaviorViolation(
          behavior: "behavior2",
          violations: ["violation2"],
          response: None,
        ),
      ],
    )

  let result =
    output.SpecResult(..make_failing_result(), rule_violations: [violation])
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("behavior1")
  |> should.be_true

  text
  |> string.contains("behavior2")
  |> should.be_true
}

// ============================================================================
// create_failure Function Tests
// ============================================================================

pub fn create_failure_basic_test() {
  let behavior = make_test_behavior("test-create", [])
  let check_result = make_check_result([], [], True, 200, 200)
  let execution = make_execution_result(200)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  failure.feature
  |> should.equal("Feature Name")

  failure.behavior
  |> should.equal("test-create")

  failure.intent
  |> should.equal("Test intent for test-create")
}

pub fn create_failure_includes_status_mismatch_test() {
  let behavior = make_test_behavior("test-status", [])
  let check_result = make_check_result([], [], False, 200, 404)
  let execution = make_execution_result(404)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  // Should have a problem for status mismatch
  list.length(failure.problems)
  |> should.equal(1)

  let assert [problem] = failure.problems
  problem.field
  |> should.equal("status")

  problem.expected
  |> should.equal("200")

  problem.actual
  |> should.equal("404")
}

pub fn create_failure_includes_check_failures_test() {
  let behavior = make_test_behavior("test-checks", [])
  let failed_checks = [
    checker.CheckFailed(
      field: "$.name",
      rule: "exists",
      expected: "present",
      actual: "missing",
      explanation: "Name is required",
    ),
  ]
  let check_result = make_check_result([], failed_checks, True, 200, 200)
  let execution = make_execution_result(200)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  list.length(failure.problems)
  |> should.equal(1)

  let assert [problem] = failure.problems
  problem.field
  |> should.equal("$.name")

  problem.explanation
  |> should.equal("Name is required")
}

pub fn create_failure_combines_status_and_check_failures_test() {
  let behavior = make_test_behavior("test-combined", [])
  let failed_checks = [
    checker.CheckFailed(
      field: "$.email",
      rule: "valid",
      expected: "valid@email.com",
      actual: "invalid",
      explanation: "Email format invalid",
    ),
  ]
  let check_result = make_check_result([], failed_checks, False, 200, 400)
  let execution = make_execution_result(400)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  // Should have both status problem and check failure
  list.length(failure.problems)
  |> should.equal(2)
}

pub fn create_failure_constructs_url_test() {
  let behavior = make_test_behavior("test-url", [])
  let check_result = make_check_result([], [], True, 200, 200)
  let execution = make_execution_result(200)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://api.example.com",
    )

  failure.request_sent.url
  |> should.equal("http://api.example.com/test-url")
}

pub fn create_failure_includes_requires_test() {
  let behavior = make_test_behavior("test-deps", ["dep1", "dep2"])
  let check_result = make_check_result([], [], True, 200, 200)
  let execution = make_execution_result(200)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  failure.see_also
  |> should.equal(["dep1", "dep2"])
}

pub fn create_failure_generates_hint_for_404_test() {
  let behavior = make_test_behavior("test-404", [])
  let check_result = make_check_result([], [], False, 200, 404)
  let execution = make_execution_result(404)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  failure.hint
  |> string.contains("resource might not exist")
  |> should.be_true
}

pub fn create_failure_generates_hint_for_401_test() {
  let behavior = make_test_behavior("test-401", [])
  let check_result = make_check_result([], [], False, 200, 401)
  let execution = make_execution_result(401)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  failure.hint
  |> string.contains("Authentication")
  |> should.be_true
}

pub fn create_failure_generates_hint_for_403_test() {
  let behavior = make_test_behavior("test-403", [])
  let check_result = make_check_result([], [], False, 200, 403)
  let execution = make_execution_result(403)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  failure.hint
  |> string.contains("Access denied")
  |> should.be_true
}

pub fn create_failure_generates_hint_for_500_test() {
  let behavior = make_test_behavior("test-500", [])
  let check_result = make_check_result([], [], False, 200, 500)
  let execution = make_execution_result(500)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  failure.hint
  |> string.contains("Server error")
  |> should.be_true
}

pub fn create_failure_generates_hint_for_field_failures_test() {
  let behavior = make_test_behavior("test-fields", [])
  let failed_checks = [
    checker.CheckFailed(
      field: "$.name",
      rule: "exists",
      expected: "present",
      actual: "missing",
      explanation: "Name is required",
    ),
  ]
  let check_result = make_check_result([], failed_checks, True, 200, 200)
  let execution = make_execution_result(200)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  failure.hint
  |> string.contains("field paths")
  |> should.be_true
}

pub fn create_failure_no_hint_for_unknown_status_test() {
  let behavior = make_test_behavior("test-unknown", [])
  let check_result = make_check_result([], [], False, 200, 418)
  let execution = make_execution_result(418)

  let failure =
    output.create_failure(
      "Feature Name",
      behavior,
      check_result,
      execution,
      "http://localhost:8080",
    )

  failure.hint
  |> should.equal("")
}

// ============================================================================
// create_blocked Function Tests
// ============================================================================

pub fn create_blocked_basic_test() {
  let blocked = output.create_blocked("blocked-behavior", "required-behavior")

  blocked.behavior
  |> should.equal("blocked-behavior")

  blocked.reason
  |> string.contains("required-behavior")
  |> should.be_true

  blocked.reason
  |> string.contains("failed")
  |> should.be_true
}

pub fn create_blocked_includes_hint_test() {
  let blocked = output.create_blocked("test-blocked", "test-required")

  blocked.hint
  |> string.contains("test-required")
  |> should.be_true

  blocked.hint
  |> string.contains("first")
  |> should.be_true
}

// ============================================================================
// Edge Cases Tests
// ============================================================================

pub fn spec_result_all_zeros_test() {
  let result =
    output.SpecResult(
      pass: True,
      passed: 0,
      failed: 0,
      blocked: 0,
      total: 0,
      summary: "No tests",
      failures: [],
      blocked_behaviors: [],
      rule_violations: [],
      anti_patterns_detected: [],
    )

  let text = output.spec_result_to_text(result)

  text
  |> string.contains("Passed: 0")
  |> should.be_true

  text
  |> string.contains("Total: 0")
  |> should.be_true
}

pub fn spec_result_large_numbers_test() {
  let result =
    output.SpecResult(
      pass: False,
      passed: 9999,
      failed: 1000,
      blocked: 500,
      total: 11_499,
      summary: "Large test suite",
      failures: [],
      blocked_behaviors: [],
      rule_violations: [],
      anti_patterns_detected: [],
    )

  let text = output.spec_result_to_text(result)

  text
  |> string.contains("9999")
  |> should.be_true

  text
  |> string.contains("11499")
  |> should.be_true
}

pub fn spec_result_empty_summary_test() {
  let result = output.SpecResult(..make_passing_result(), summary: "")

  let text = output.spec_result_to_text(result)

  // Should still format correctly with empty summary
  text
  |> string.contains("PASS")
  |> should.be_true
}

pub fn behavior_failure_empty_problems_list_test() {
  let failure = output.BehaviorFailure(..make_test_failure(), problems: [])

  let result = output.SpecResult(..make_failing_result(), failures: [failure])
  let text = output.spec_result_to_text(result)

  // Should still format correctly even with no problems
  text
  |> string.contains("test-behavior")
  |> should.be_true
}

pub fn behavior_failure_empty_headers_test() {
  let failure =
    output.BehaviorFailure(
      ..make_test_failure(),
      request_sent: output.RequestSummary(
        method: "GET",
        url: "http://test.com",
        headers: dict.new(),
      ),
    )

  let json_output =
    output.spec_result_to_json(
      output.SpecResult(..make_failing_result(), failures: [failure]),
    )
  let json_str = json.to_string(json_output)

  // Should serialize empty headers as empty object
  json_str
  |> string.contains("\"headers\":{}")
  |> should.be_true
}

pub fn multiple_failures_formatted_test() {
  let failure1 =
    output.BehaviorFailure(
      ..make_test_failure(),
      behavior: "failure1",
      feature: "Feature1",
    )
  let failure2 =
    output.BehaviorFailure(
      ..make_test_failure(),
      behavior: "failure2",
      feature: "Feature2",
    )

  let result =
    output.SpecResult(
      ..make_failing_result(),
      failed: 2,
      failures: [failure1, failure2],
    )
  let text = output.spec_result_to_text(result)

  text
  |> string.contains("failure1")
  |> should.be_true

  text
  |> string.contains("failure2")
  |> should.be_true

  text
  |> string.contains("[Feature1]")
  |> should.be_true

  text
  |> string.contains("[Feature2]")
  |> should.be_true
}

pub fn mixed_results_comprehensive_test() {
  let failure = make_test_failure()
  let blocked = make_blocked_behavior()
  let violation = make_rule_violation()
  let anti_pattern =
    anti_patterns.AntiPatternDetected(
      "test",
      "desc",
      "found",
      json.null(),
      json.null(),
    )

  let result =
    output.SpecResult(
      pass: False,
      passed: 5,
      failed: 2,
      blocked: 1,
      total: 8,
      summary: "Mixed results with all types",
      failures: [failure],
      blocked_behaviors: [blocked],
      rule_violations: [violation],
      anti_patterns_detected: [anti_pattern],
    )

  let text = output.spec_result_to_text(result)

  // Should have all sections
  text
  |> string.contains("FAIL")
  |> should.be_true

  text
  |> string.contains("FAILURES:")
  |> should.be_true

  text
  |> string.contains("BLOCKED:")
  |> should.be_true

  text
  |> string.contains("RULE VIOLATIONS:")
  |> should.be_true

  text
  |> string.contains("ANTI-PATTERNS DETECTED:")
  |> should.be_true
}

pub fn spec_result_pass_false_with_no_failures_test() {
  // Edge case: pass is False but failures list is empty
  let result =
    output.SpecResult(
      pass: False,
      passed: 0,
      failed: 0,
      blocked: 0,
      total: 1,
      summary: "Failed for other reasons",
      failures: [],
      blocked_behaviors: [],
      rule_violations: [],
      anti_patterns_detected: [],
    )

  let text = output.spec_result_to_text(result)

  text
  |> string.contains("FAIL")
  |> should.be_true

  // No failures section should appear
  text
  |> string.contains("FAILURES:")
  |> should.be_false
}

pub fn behavior_violation_with_response_json_test() {
  let response_body = json.object([#("status", json.string("error"))])
  let violation =
    output.RuleViolationGroup(
      rule: "test-rule",
      description: "Test",
      violations: [
        output.BehaviorViolation(
          behavior: "test",
          violations: ["issue"],
          response: Some(response_body),
        ),
      ],
    )

  let json_output =
    output.spec_result_to_json(
      output.SpecResult(..make_failing_result(), rule_violations: [violation]),
    )
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"response\"")
  |> should.be_true

  json_str
  |> string.contains("\"status\":\"error\"")
  |> should.be_true
}

pub fn behavior_violation_without_response_test() {
  let violation =
    output.RuleViolationGroup(
      rule: "test-rule",
      description: "Test",
      violations: [
        output.BehaviorViolation(
          behavior: "test",
          violations: ["issue"],
          response: None,
        ),
      ],
    )

  let json_output =
    output.spec_result_to_json(
      output.SpecResult(..make_failing_result(), rule_violations: [violation]),
    )
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"response\":null")
  |> should.be_true
}

pub fn anti_pattern_no_anti_patterns_serialization_test() {
  let result =
    output.SpecResult(
      ..make_passing_result(),
      anti_patterns_detected: [anti_patterns.NoAntiPatterns],
    )

  let json_output = output.spec_result_to_json(result)
  let json_str = json.to_string(json_output)

  json_str
  |> string.contains("\"anti_patterns_detected\":[null]")
  |> should.be_true
}
