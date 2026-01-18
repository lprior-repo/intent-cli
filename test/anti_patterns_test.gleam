//// Comprehensive tests for intent/anti_patterns.gleam
//// Tests cover all anti-pattern detection logic and edge cases

import gleam/dict
import gleam/json.{type Json}
import gleam/list
import gleeunit
import gleeunit/should
import intent/anti_patterns.{
  AntiPatternDetected, NoAntiPatterns, check_anti_patterns, format_anti_pattern,
}
import intent/http_client.{type ExecutionResult, ExecutionResult}
import intent/types.{type AntiPattern, AntiPattern, Get}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Test Factories
// ============================================================================

/// Create minimal valid ExecutionResult
fn make_execution_result(status: Int, body: Json) -> ExecutionResult {
  ExecutionResult(
    status: status,
    headers: dict.new(),
    body: body,
    raw_body: json.to_string(body),
    elapsed_ms: 100,
    request_method: Get,
    request_path: "/test",
  )
}

/// Create an anti-pattern with bad and good examples
fn make_anti_pattern(
  name: String,
  description: String,
  bad_example: Json,
  good_example: Json,
) -> AntiPattern {
  AntiPattern(
    name: name,
    description: description,
    bad_example: bad_example,
    good_example: good_example,
    why: "Test anti-pattern",
  )
}

// ============================================================================
// check_anti_patterns Tests
// ============================================================================

pub fn check_anti_patterns_empty_list_test() {
  let body = json.object([#("id", json.int(1)), #("name", json.string("test"))])
  let response = make_execution_result(200, body)

  let results = check_anti_patterns([], response, "test_behavior")

  results
  |> list.length
  |> should.equal(0)
}

pub fn check_anti_patterns_no_match_test() {
  // Bad example has "password" field, good doesn't
  let bad = json.object([#("password", json.string("secret123"))])
  let good = json.object([#("token", json.string("abc123"))])
  let pattern =
    make_anti_pattern("exposed-password", "Passwords in response", bad, good)

  // Response doesn't have password field
  let body = json.object([#("id", json.int(1)), #("token", json.string("xyz"))])
  let response = make_execution_result(200, body)

  let results = check_anti_patterns([pattern], response, "test_behavior")

  results
  |> list.length
  |> should.equal(0)
}

pub fn check_anti_patterns_single_match_test() {
  // Bad example has "password" field, good doesn't
  let bad = json.object([#("password", json.string("secret123"))])
  let good = json.object([#("token", json.string("abc123"))])
  let pattern =
    make_anti_pattern("exposed-password", "Passwords in response", bad, good)

  // Response HAS password field (anti-pattern detected)
  let body =
    json.object([#("id", json.int(1)), #("password", json.string("hunter2"))])
  let response = make_execution_result(200, body)

  let results = check_anti_patterns([pattern], response, "test_behavior")

  results
  |> list.length
  |> should.equal(1)

  case list.first(results) {
    Ok(AntiPatternDetected(name, desc, found, _, _)) -> {
      name |> should.equal("exposed-password")
      desc |> should.equal("Passwords in response")
      found |> should.equal("Response contains: password")
    }
    _ -> should.fail()
  }
}

pub fn check_anti_patterns_multiple_patterns_test() {
  // Pattern 1: password exposure
  let bad1 = json.object([#("password", json.string("secret"))])
  let good1 = json.object([#("token", json.string("token"))])
  let pattern1 =
    make_anti_pattern("exposed-password", "Password leak", bad1, good1)

  // Pattern 2: internal ID exposure
  let bad2 = json.object([#("internal_id", json.int(123))])
  let good2 = json.object([#("id", json.int(123))])
  let pattern2 =
    make_anti_pattern("internal-id", "Internal IDs exposed", bad2, good2)

  // Response contains BOTH anti-patterns
  let body =
    json.object([
      #("password", json.string("hunter2")),
      #("internal_id", json.int(999)),
    ])
  let response = make_execution_result(200, body)

  let results =
    check_anti_patterns([pattern1, pattern2], response, "test_behavior")

  results
  |> list.length
  |> should.equal(2)
}

pub fn check_anti_patterns_partial_match_test() {
  // Pattern 1: matches
  let bad1 = json.object([#("ssn", json.string("123-45-6789"))])
  let good1 = json.object([#("id", json.int(1))])
  let pattern1 = make_anti_pattern("pii-leak", "PII in response", bad1, good1)

  // Pattern 2: doesn't match
  let bad2 = json.object([#("credit_card", json.string("1234-5678"))])
  let good2 = json.object([#("payment_id", json.string("pay_123"))])
  let pattern2 = make_anti_pattern("cc-leak", "Credit card leak", bad2, good2)

  // Response only has SSN, not credit card
  let body =
    json.object([#("id", json.int(1)), #("ssn", json.string("999-88-7777"))])
  let response = make_execution_result(200, body)

  let results =
    check_anti_patterns([pattern1, pattern2], response, "test_behavior")

  results
  |> list.length
  |> should.equal(1)

  case list.first(results) {
    Ok(AntiPatternDetected(name, _, _, _, _)) -> {
      name |> should.equal("pii-leak")
    }
    _ -> should.fail()
  }
}

pub fn check_anti_patterns_nested_fields_test() {
  // Bad example has nested "password" field
  let bad =
    json.object([
      #(
        "user",
        json.object([#("id", json.int(1)), #("password", json.string("secret"))]),
      ),
    ])
  let good =
    json.object([
      #(
        "user",
        json.object([#("id", json.int(1)), #("token", json.string("token"))]),
      ),
    ])
  let pattern =
    make_anti_pattern("nested-password", "Nested password leak", bad, good)

  // Response has nested password (should be detected)
  let body =
    json.object([
      #(
        "user",
        json.object([
          #("id", json.int(5)),
          #("password", json.string("hunter2")),
        ]),
      ),
    ])
  let response = make_execution_result(200, body)

  let results = check_anti_patterns([pattern], response, "test_behavior")

  results
  |> list.length
  |> should.equal(1)
}

pub fn check_anti_patterns_deeply_nested_test() {
  // Test deeply nested anti-pattern detection
  let bad =
    json.object([
      #(
        "data",
        json.object([
          #(
            "users",
            json.object([
              #("admin", json.object([#("secret_key", json.string("abc123"))])),
            ]),
          ),
        ]),
      ),
    ])
  let good =
    json.object([
      #(
        "data",
        json.object([
          #(
            "users",
            json.object([
              #("admin", json.object([#("id", json.string("admin1"))])),
            ]),
          ),
        ]),
      ),
    ])
  let pattern =
    make_anti_pattern("secret-key-leak", "Secret key exposed", bad, good)

  let body =
    json.object([
      #(
        "data",
        json.object([
          #(
            "users",
            json.object([
              #("admin", json.object([#("secret_key", json.string("xyz789"))])),
            ]),
          ),
        ]),
      ),
    ])
  let response = make_execution_result(200, body)

  let results = check_anti_patterns([pattern], response, "test_behavior")

  results
  |> list.length
  |> should.equal(1)
}

pub fn check_anti_patterns_empty_response_test() {
  let bad = json.object([#("error", json.string("Internal error"))])
  let good = json.object([#("message", json.string("Error occurred"))])
  let pattern =
    make_anti_pattern("internal-error", "Internal errors exposed", bad, good)

  // Empty object response
  let body = json.object([])
  let response = make_execution_result(200, body)

  let results = check_anti_patterns([pattern], response, "test_behavior")

  results
  |> list.length
  |> should.equal(0)
}

pub fn check_anti_patterns_null_response_test() {
  let bad = json.object([#("admin", json.bool(True))])
  let good = json.object([#("role", json.string("user"))])
  let pattern = make_anti_pattern("admin-flag", "Admin flag exposed", bad, good)

  // Null response
  let body = json.null()
  let response = make_execution_result(200, body)

  let results = check_anti_patterns([pattern], response, "test_behavior")

  results
  |> list.length
  |> should.equal(0)
}

pub fn check_anti_patterns_multiple_bad_keys_test() {
  // Bad example has multiple problematic fields
  let bad =
    json.object([
      #("password", json.string("secret")),
      #("ssn", json.string("123-45-6789")),
      #("credit_card", json.string("1234-5678")),
    ])
  let good =
    json.object([
      #("id", json.int(1)),
      #("email", json.string("user@example.com")),
    ])
  let pattern = make_anti_pattern("pii-leak", "Multiple PII fields", bad, good)

  // Response contains all bad keys
  let body =
    json.object([
      #("password", json.string("hunter2")),
      #("ssn", json.string("999-88-7777")),
      #("credit_card", json.string("5555-6666")),
    ])
  let response = make_execution_result(200, body)

  let results = check_anti_patterns([pattern], response, "test_behavior")

  results
  |> list.length
  |> should.equal(1)

  // Should report all found keys - just verify we have output
  case list.first(results) {
    Ok(AntiPatternDetected(_, _, found, _, _)) -> {
      // Should contain text
      found
      |> should.not_equal("")
    }
    _ -> should.fail()
  }
}

pub fn check_anti_patterns_same_keys_different_values_test() {
  // Good and bad have same keys, just different values
  // This means NO anti-pattern (keys are the same)
  let bad = json.object([#("status", json.string("ERROR_INTERNAL_500"))])
  let good = json.object([#("status", json.string("error"))])
  let pattern =
    make_anti_pattern("verbose-error", "Verbose error codes", bad, good)

  let body = json.object([#("status", json.string("ERROR_INTERNAL_500"))])
  let response = make_execution_result(200, body)

  let results = check_anti_patterns([pattern], response, "test_behavior")

  // No problem keys detected (both have "status")
  results
  |> list.length
  |> should.equal(0)
}

// ============================================================================
// format_anti_pattern Tests
// ============================================================================

pub fn format_anti_pattern_no_patterns_test() {
  let result = NoAntiPatterns
  let formatted = format_anti_pattern(result)

  formatted |> should.equal("No anti-patterns detected")
}

pub fn format_anti_pattern_detected_test() {
  let bad = json.object([#("password", json.string("secret123"))])
  let good = json.object([#("token", json.string("abc123"))])

  let result =
    AntiPatternDetected(
      pattern_name: "exposed-password",
      description: "Passwords should never be in responses",
      found: "Response contains: password",
      bad_example: bad,
      good_example: good,
    )

  let formatted = format_anti_pattern(result)

  // Check that all components are present
  formatted
  |> should.equal(
    "Anti-pattern detected: exposed-password\n"
    <> "Description: Passwords should never be in responses\n"
    <> "Found: Response contains: password\n"
    <> "Bad example: {\"password\":\"secret123\"}\n"
    <> "Good example: {\"token\":\"abc123\"}",
  )
}

// ============================================================================
// Edge Cases and Contract Guarantees
// ============================================================================

pub fn check_anti_patterns_identical_examples_test() {
  // If bad and good are identical, no problem keys exist
  let example =
    json.object([#("id", json.int(1)), #("name", json.string("test"))])
  let pattern = make_anti_pattern("test", "Test pattern", example, example)

  let body = json.object([#("id", json.int(1)), #("name", json.string("test"))])
  let response = make_execution_result(200, body)

  let results = check_anti_patterns([pattern], response, "test_behavior")

  // No problem keys since bad and good are the same
  results
  |> list.length
  |> should.equal(0)
}
// ============================================================================
// TODO: Future Improvements
// ============================================================================

// TODO: Add support for pattern matching on field values, not just keys
// Currently only checks if bad keys are present, not if values match
// Example: detect "error": "InternalDatabaseError" vs "error": "Not Found"

// TODO: Add support for regex-based field name matching
// Example: detect any field matching ".*_internal$" or "debug_.*"

// TODO: Add severity levels to anti-patterns (critical, warning, info)
// Some anti-patterns are security issues, others are just style concerns

// TODO: Add support for checking array element structures
// Currently arrays are not deeply inspected for anti-patterns

// TODO: Add whitelisting/suppression mechanism
// Allow specific behaviors to suppress certain anti-pattern checks

// TODO: Add performance benchmarks for large response bodies
// Test with realistic 1MB+ JSON responses to ensure good performance

// TODO: Add support for detecting anti-patterns in response headers
// Example: exposing server version, internal routing info

// TODO: Improve error messages to show JSON path to bad field
// Example: "Found 'password' at $.user.credentials.password"
