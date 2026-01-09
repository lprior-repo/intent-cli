//// Comprehensive tests for checker rule evaluation
//// Tests the untested rule types:
//// - JWT validation (valid JWT, is jwt)
//// - Regex pattern matching (string matching)
//// - ArrayWhereEach (complex nested validation)
//// - URI validation
//// - Integer comparison edge cases
//// - Float comparison
//// - Null/NotNull checks

import gleam/dict
import gleam/function
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None}
import gleam/string
import gleeunit/should
import intent/checker
import intent/http_client
import intent/interpolate
import intent/types.{type Check, type Response, Check, Response}

// ============================================================================
// Test Helpers
// ============================================================================

/// Create a minimal execution result with given body
fn make_execution_result(body: Json) -> http_client.ExecutionResult {
  http_client.ExecutionResult(
    status: 200,
    headers: dict.new(),
    body: body,
    raw_body: "",
    elapsed_ms: 10,
    request_method: types.Get,
    request_path: "/test",
  )
}

/// Create a minimal expected response with given checks
fn make_expected_response(checks: List(#(String, Check))) -> Response {
  Response(
    status: 200,
    example: json.null(),
    checks: dict.from_list(checks),
    headers: dict.new(),
  )
}

/// Create an empty context for testing
fn empty_context() -> interpolate.Context {
  interpolate.Context(
    variables: dict.new(),
    request_body: None,
    response_body: None,
  )
}

/// Check that a single check passed
fn assert_passed(result: checker.ResponseCheckResult) {
  list.length(result.failed)
  |> should.equal(0)
  list.is_empty(result.passed)
  |> should.be_false
}

/// Check that a single check failed
fn assert_failed(result: checker.ResponseCheckResult) {
  result.failed
  |> list.is_empty
  |> should.be_false
}

// ============================================================================
// JWT Validation Tests
// ============================================================================

pub fn checker_jwt_valid_format_test() {
  // Valid JWT with proper base64url parts
  // Header: {"alg":"HS256","typ":"JWT"}
  // Payload: {"sub":"1234567890","name":"Test User","iat":1516239022}
  let valid_jwt =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IlRlc3QgVXNlciIsImlhdCI6MTUxNjIzOTAyMn0.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"

  let body = json.object([#("token", json.string(valid_jwt))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("token", Check("jwt", "Token must be JWT"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_jwt_valid_jwt_rule_test() {
  // Test with "valid JWT" rule
  let valid_jwt =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IlRlc3QgVXNlciIsImlhdCI6MTUxNjIzOTAyMn0.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"

  let body = json.object([#("token", json.string(valid_jwt))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("token", Check("valid JWT", "Token validation"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_jwt_missing_parts_test() {
  // JWT with only 2 parts (missing signature)
  let invalid_jwt = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0"

  let body = json.object([#("token", json.string(invalid_jwt))])
  let actual = make_execution_result(body)
  let expected = make_expected_response([#("token", Check("jwt", "JWT check"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)

  // Check error message mentions parts
  case list.first(result.failed) {
    Ok(checker.CheckFailed(_, _, _, _, explanation)) -> {
      explanation
      |> string.contains("3")
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn checker_jwt_invalid_base64_header_test() {
  // Header is not valid base64url
  let invalid_jwt = "not-valid-base64!!!.eyJzdWIiOiIxMjM0NTY3ODkwIn0.signature"

  let body = json.object([#("token", json.string(invalid_jwt))])
  let actual = make_execution_result(body)
  let expected = make_expected_response([#("token", Check("jwt", "JWT check"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

pub fn checker_jwt_missing_alg_field_test() {
  // JWT header missing "alg" field
  // Header: {"typ":"JWT"} (no alg)
  // Base64url of {"typ":"JWT"} is eyJ0eXAiOiJKV1QifQ
  let jwt_no_alg = "eyJ0eXAiOiJKV1QifQ.eyJzdWIiOiIxMjM0NTY3ODkwIn0.signature"

  let body = json.object([#("token", json.string(jwt_no_alg))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("token", Check("valid JWT", "JWT must have alg"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)

  // Error should mention missing alg
  case list.first(result.failed) {
    Ok(checker.CheckFailed(_, _, _, _, explanation)) -> {
      explanation
      |> string.contains("alg")
      |> should.be_true
    }
    _ -> should.fail()
  }
}

pub fn checker_jwt_not_string_test() {
  // JWT field is not a string
  let body = json.object([#("token", json.int(12345))])
  let actual = make_execution_result(body)
  let expected = make_expected_response([#("token", Check("jwt", "JWT check"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

pub fn checker_jwt_empty_string_test() {
  // Empty string is not a valid JWT
  let body = json.object([#("token", json.string(""))])
  let actual = make_execution_result(body)
  let expected = make_expected_response([#("token", Check("jwt", "JWT check"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

pub fn checker_jwt_four_parts_test() {
  // JWT with 4 parts (too many)
  let invalid_jwt = "a.b.c.d"
  let body = json.object([#("token", json.string(invalid_jwt))])
  let actual = make_execution_result(body)
  let expected = make_expected_response([#("token", Check("jwt", "JWT check"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

// ============================================================================
// Regex Pattern Matching Tests
// ============================================================================

pub fn checker_regex_simple_match_test() {
  // Simple alphanumeric pattern
  let body = json.object([#("code", json.string("ABC123"))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("code", Check("string matching ^[A-Z0-9]+$", "Must be alphanumeric uppercase")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_regex_no_match_test() {
  // Pattern doesn't match
  let body = json.object([#("code", json.string("abc123"))])
  // lowercase
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("code", Check("string matching ^[A-Z0-9]+$", "Must be uppercase")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

pub fn checker_regex_email_pattern_test() {
  // Email-like pattern
  let body = json.object([#("email", json.string("test@example.com"))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("email", Check("string matching ^[a-z]+@[a-z]+\\.[a-z]+$", "Email pattern")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_regex_uuid_pattern_test() {
  // UUID pattern match
  let body =
    json.object([#("id", json.string("550e8400-e29b-41d4-a716-446655440000"))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #(
        "id",
        Check(
          "string matching ^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$",
          "UUID format",
        ),
      ),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_regex_partial_match_test() {
  // Regex matches substring (no anchors)
  let body = json.object([#("text", json.string("hello world"))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("text", Check("string matching world", "Contains world")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_regex_special_chars_test() {
  // Pattern with special regex characters
  let body = json.object([#("version", json.string("v1.2.3"))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("version", Check("string matching ^v[0-9]+\\.[0-9]+\\.[0-9]+$", "Semver")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_regex_non_string_value_test() {
  // Trying to match regex on non-string
  let body = json.object([#("count", json.int(42))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("count", Check("string matching ^\\d+$", "Digits"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

// ============================================================================
// URI Validation Tests
// ============================================================================

pub fn checker_uri_valid_http_test() {
  let body = json.object([#("url", json.string("https://example.com/path"))])
  let actual = make_execution_result(body)
  let expected = make_expected_response([#("url", Check("uri", "Valid URI"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_uri_valid_with_query_test() {
  let body =
    json.object([
      #("url", json.string("https://api.example.com/search?q=test&page=1")),
    ])
  let actual = make_execution_result(body)
  let expected = make_expected_response([#("url", Check("uri", "URI check"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_uri_invalid_test() {
  let body = json.object([#("url", json.string("not a valid uri"))])
  let actual = make_execution_result(body)
  let expected = make_expected_response([#("url", Check("uri", "URI check"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

// ============================================================================
// Null/NotNull Tests
// ============================================================================

pub fn checker_not_null_with_value_test() {
  let body = json.object([#("name", json.string("Alice"))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("name", Check("not null", "Name required"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_not_null_with_null_test() {
  let body = json.object([#("name", json.null())])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("name", Check("not null", "Name required"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

pub fn checker_is_null_with_null_test() {
  let body = json.object([#("deleted_at", json.null())])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("deleted_at", Check("null", "Should be null"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_is_null_with_value_test() {
  let body = json.object([#("deleted_at", json.string("2024-01-15"))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("deleted_at", Check("null", "Should be null"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

// ============================================================================
// Integer Comparison Edge Cases
// ============================================================================

pub fn checker_integer_gt_boundary_test() {
  // integer > 5 should fail for exactly 5
  let body = json.object([#("count", json.int(5))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("count", Check("integer > 5", "Must be > 5"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

pub fn checker_integer_gt_pass_test() {
  // integer > 5 should pass for 6
  let body = json.object([#("count", json.int(6))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("count", Check("integer > 5", "Must be > 5"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_integer_lt_boundary_test() {
  // integer < 10 should fail for exactly 10
  let body = json.object([#("count", json.int(10))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("count", Check("integer < 10", "Must be < 10"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

pub fn checker_integer_lt_pass_test() {
  // integer < 10 should pass for 9
  let body = json.object([#("count", json.int(9))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("count", Check("integer < 10", "Must be < 10"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_integer_negative_test() {
  // Negative integer comparison
  let body = json.object([#("offset", json.int(-5))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("offset", Check("integer >= -10", "Valid offset"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_integer_zero_test() {
  // Zero edge case
  let body = json.object([#("count", json.int(0))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("count", Check("integer >= 0", "Non-negative"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

// ============================================================================
// Float/Number Comparison Tests
// ============================================================================

pub fn checker_number_between_pass_test() {
  let body = json.object([#("price", json.float(19.99))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("price", Check("number between 0.0 and 100.0", "Valid price")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_number_between_fail_low_test() {
  let body = json.object([#("price", json.float(-5.0))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("price", Check("number between 0.0 and 100.0", "Valid price")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

pub fn checker_number_between_fail_high_test() {
  let body = json.object([#("price", json.float(150.0))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("price", Check("number between 0.0 and 100.0", "Valid price")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

pub fn checker_number_between_boundary_low_test() {
  // Exactly at lower boundary should pass
  let body = json.object([#("ratio", json.float(0.0))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("ratio", Check("number between 0.0 and 1.0", "Valid ratio")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_number_between_boundary_high_test() {
  // Exactly at upper boundary should pass
  let body = json.object([#("ratio", json.float(1.0))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("ratio", Check("number between 0.0 and 1.0", "Valid ratio")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

// ============================================================================
// Array Tests
// ============================================================================

pub fn checker_array_max_items_pass_test() {
  let body =
    json.object([
      #("tags", json.array([json.string("a"), json.string("b")], function.identity)),
    ])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("tags", Check("array with max 5 items", "Max tags"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_array_max_items_fail_test() {
  let body =
    json.object([
      #(
        "tags",
        json.array(
          [
            json.string("a"),
            json.string("b"),
            json.string("c"),
            json.string("d"),
            json.string("e"),
            json.string("f"),
          ],
          function.identity,
        ),
      ),
    ])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("tags", Check("array with max 5 items", "Max tags"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

// ============================================================================
// Type Checks
// ============================================================================

pub fn checker_is_number_with_int_test() {
  // "number" type should accept integers
  let body = json.object([#("value", json.int(42))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("value", Check("number", "Must be number"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_is_number_with_float_test() {
  // "number" type should accept floats
  let body = json.object([#("value", json.float(3.14))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("value", Check("number", "Must be number"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_is_number_with_string_test() {
  // "number" type should reject strings
  let body = json.object([#("value", json.string("42"))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("value", Check("number", "Must be number"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

// ============================================================================
// Equals with Different Types
// ============================================================================

pub fn checker_equals_float_test() {
  let body = json.object([#("pi", json.float(3.14))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("pi", Check("equals 3.14", "Pi value"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_equals_bool_true_test() {
  let body = json.object([#("active", json.bool(True))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("active", Check("equals true", "Must be true"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_equals_bool_false_test() {
  let body = json.object([#("active", json.bool(False))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("active", Check("equals false", "Must be false"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_passed(result)
}

pub fn checker_equals_bool_mismatch_test() {
  let body = json.object([#("active", json.bool(True))])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([#("active", Check("equals false", "Should be false"))])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)
  assert_failed(result)
}

// ============================================================================
// Multiple Checks on Same Response
// ============================================================================

pub fn checker_multiple_checks_mixed_results_test() {
  let body =
    json.object([
      #("name", json.string("Alice")),
      #("age", json.int(25)),
      #("email", json.string("invalid")),
      // Invalid email
    ])
  let actual = make_execution_result(body)
  let expected =
    make_expected_response([
      #("name", Check("non-empty string", "Name required")),
      #("age", Check("integer >= 18", "Must be adult")),
      #("email", Check("email", "Valid email required")),
    ])
  let ctx = empty_context()

  let result = checker.check_response(expected, actual, ctx)

  // name and age should pass, email should fail
  list.length(result.passed)
  |> should.equal(2)
  list.length(result.failed)
  |> should.equal(1)
}
