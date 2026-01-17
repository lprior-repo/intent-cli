//// Comprehensive tests for intent/interpolate.gleam
//// Tests cover variable resolution, path navigation, array indexing, and edge cases
////
//// Design by Contract:
//// - Preconditions: Context has valid Dict(String, Json), paths follow grammar
//// - Postconditions: All ${...} replaced OR first error returned, no partial replacements
//// - Invariants: Variables case-sensitive, missing vars error, JSON types preserved

import gleam/dict
import gleam/json
import gleam/option
import gleam/string
import gleeunit
import gleeunit/should
import intent/interpolate.{type Context}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Test Factories (Design by Contract: Valid Inputs)
// ============================================================================

/// Create empty context
fn empty_ctx() -> Context {
  interpolate.new_context()
}

/// Create context with single string variable
fn ctx_with_string(name: String, value: String) -> Context {
  empty_ctx()
  |> interpolate.set_variable(name, json.string(value))
}

/// Create context with nested object
fn ctx_with_object() -> Context {
  let user_obj =
    json.object([
      #("id", json.int(123)),
      #("name", json.string("Alice")),
      #("email", json.string("alice@example.com")),
      #("active", json.bool(True)),
    ])

  empty_ctx()
  |> interpolate.set_variable("user", user_obj)
}

/// Create context with array
fn ctx_with_array() -> Context {
  let item1 =
    json.object([#("id", json.int(1)), #("name", json.string("first"))])
  let item2 =
    json.object([#("id", json.int(2)), #("name", json.string("second"))])
  let item3 =
    json.object([#("id", json.int(3)), #("name", json.string("third"))])

  empty_ctx()
  |> interpolate.set_variable(
    "items",
    json.array([item1, item2, item3], of: fn(x) { x }),
  )
}

/// Create context with request/response bodies
fn ctx_with_bodies() -> Context {
  let req_body = json.object([#("user_id", json.int(42))])
  let resp_body = json.object([#("status", json.string("ok"))])

  empty_ctx()
  |> interpolate.set_request_body(req_body)
  |> interpolate.set_response_body(resp_body)
}

/// Helper to check if error message contains substring
fn error_contains(result: Result(a, String), substring: String) -> Bool {
  case result {
    Error(msg) -> string.contains(msg, substring)
    Ok(_) -> False
  }
}

// ============================================================================
// Happy Path Tests - Basic Variable Interpolation
// ============================================================================

pub fn interpolate_simple_string_test() {
  let ctx = ctx_with_string("name", "Alice")
  let result = interpolate.interpolate_string(ctx, "Hello ${name}!")

  result
  |> should.be_ok
  |> should.equal("Hello Alice!")
}

pub fn interpolate_only_variable_test() {
  let ctx = ctx_with_string("token", "abc123")
  let result = interpolate.interpolate_string(ctx, "${token}")

  result
  |> should.be_ok
  |> should.equal("abc123")
}

pub fn interpolate_multiple_variables_test() {
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("first", json.string("John"))
    |> interpolate.set_variable("last", json.string("Doe"))

  let result = interpolate.interpolate_string(ctx, "${first} ${last}")

  result
  |> should.be_ok
  |> should.equal("John Doe")
}

pub fn interpolate_same_variable_twice_test() {
  let ctx = ctx_with_string("id", "xyz")
  let result = interpolate.interpolate_string(ctx, "${id}:${id}")

  result
  |> should.be_ok
  |> should.equal("xyz:xyz")
}

pub fn interpolate_no_variables_test() {
  let ctx = empty_ctx()
  let result = interpolate.interpolate_string(ctx, "plain text no variables")

  result
  |> should.be_ok
  |> should.equal("plain text no variables")
}

pub fn interpolate_empty_string_test() {
  let ctx = empty_ctx()
  let result = interpolate.interpolate_string(ctx, "")

  result
  |> should.be_ok
  |> should.equal("")
}

// ============================================================================
// JSON Type Handling - Verify Proper Serialization
// ============================================================================

pub fn interpolate_string_value_unwraps_quotes_test() {
  let ctx = ctx_with_string("name", "Alice")
  let result = interpolate.interpolate_string(ctx, "${name}")

  // String values should NOT have surrounding quotes
  result
  |> should.be_ok
  |> should.equal("Alice")
}

pub fn interpolate_number_value_test() {
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("count", json.int(42))

  let result = interpolate.interpolate_string(ctx, "Count: ${count}")

  result
  |> should.be_ok
  |> should.equal("Count: 42")
}

pub fn interpolate_boolean_true_test() {
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("active", json.bool(True))

  let result = interpolate.interpolate_string(ctx, "Active: ${active}")

  result
  |> should.be_ok
  |> should.equal("Active: true")
}

pub fn interpolate_boolean_false_test() {
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("deleted", json.bool(False))

  let result = interpolate.interpolate_string(ctx, "${deleted}")

  result
  |> should.be_ok
  |> should.equal("false")
}

pub fn interpolate_null_value_test() {
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("nullable", json.null())

  let result = interpolate.interpolate_string(ctx, "${nullable}")

  result
  |> should.be_ok
  |> should.equal("null")
}

// ============================================================================
// Path Navigation - Nested Fields
// ============================================================================

pub fn interpolate_nested_field_test() {
  let ctx = ctx_with_object()
  let result = interpolate.interpolate_string(ctx, "Email: ${user.email}")

  result
  |> should.be_ok
  |> should.equal("Email: alice@example.com")
}

pub fn interpolate_nested_number_field_test() {
  let ctx = ctx_with_object()
  let result = interpolate.interpolate_string(ctx, "ID: ${user.id}")

  result
  |> should.be_ok
  |> should.equal("ID: 123")
}

pub fn interpolate_nested_boolean_field_test() {
  let ctx = ctx_with_object()
  let result = interpolate.interpolate_string(ctx, "${user.active}")

  result
  |> should.be_ok
  |> should.equal("true")
}

// ============================================================================
// Array Indexing - Positive Indices
// ============================================================================

pub fn interpolate_array_first_element_test() {
  let ctx = ctx_with_array()
  let result = interpolate.interpolate_string(ctx, "${items[0].name}")

  result
  |> should.be_ok
  |> should.equal("first")
}

pub fn interpolate_array_middle_element_test() {
  let ctx = ctx_with_array()
  let result = interpolate.interpolate_string(ctx, "${items[1].id}")

  result
  |> should.be_ok
  |> should.equal("2")
}

pub fn interpolate_array_last_element_positive_index_test() {
  let ctx = ctx_with_array()
  let result = interpolate.interpolate_string(ctx, "${items[2].name}")

  result
  |> should.be_ok
  |> should.equal("third")
}

// ============================================================================
// Array Indexing - Negative Indices
// ============================================================================

pub fn interpolate_array_negative_one_test() {
  let ctx = ctx_with_array()
  let result = interpolate.interpolate_string(ctx, "${items[-1].name}")

  result
  |> should.be_ok
  |> should.equal("third")
}

pub fn interpolate_array_negative_two_test() {
  let ctx = ctx_with_array()
  let result = interpolate.interpolate_string(ctx, "${items[-2].name}")

  result
  |> should.be_ok
  |> should.equal("second")
}

pub fn interpolate_array_negative_three_test() {
  let ctx = ctx_with_array()
  let result = interpolate.interpolate_string(ctx, "${items[-3].id}")

  result
  |> should.be_ok
  |> should.equal("1")
}

// ============================================================================
// Request/Response Body Navigation
// ============================================================================

pub fn interpolate_request_body_field_test() {
  let ctx = ctx_with_bodies()
  let result =
    interpolate.interpolate_string(ctx, "User: ${request.body.user_id}")

  result
  |> should.be_ok
  |> should.equal("User: 42")
}

pub fn interpolate_response_body_field_test() {
  let ctx = ctx_with_bodies()
  let result =
    interpolate.interpolate_string(ctx, "Status: ${response.body.status}")

  result
  |> should.be_ok
  |> should.equal("Status: ok")
}

// ============================================================================
// Error Cases - Missing Variables
// ============================================================================

pub fn interpolate_missing_variable_error_test() {
  let ctx = empty_ctx()
  let result = interpolate.interpolate_string(ctx, "${nonexistent}")

  result
  |> should.be_error
  |> should.equal("Variable not found: nonexistent")
}

pub fn interpolate_missing_nested_field_error_test() {
  let ctx = ctx_with_object()
  let result = interpolate.interpolate_string(ctx, "${user.nonexistent}")

  result
  |> should.be_error

  // Should mention field not found
  let has_field = error_contains(result, "Field")
  let has_name = error_contains(result, "nonexistent")
  { has_field && has_name }
  |> should.be_true
}

pub fn interpolate_missing_request_body_error_test() {
  let ctx = empty_ctx()
  let result = interpolate.interpolate_string(ctx, "${request.body.id}")

  result
  |> should.be_error
  |> should.equal("No request body in context")
}

pub fn interpolate_missing_response_body_error_test() {
  let ctx = empty_ctx()
  let result = interpolate.interpolate_string(ctx, "${response.body.status}")

  result
  |> should.be_error
  |> should.equal("No response body in context")
}

// ============================================================================
// Error Cases - Invalid Paths
// ============================================================================

pub fn interpolate_empty_path_passthrough_test() {
  // NOTE: The regex pattern doesn't match ${} so it passes through unchanged
  // This is technically a bug but documenting actual behavior
  let ctx = empty_ctx()
  let result = interpolate.interpolate_string(ctx, "${}")

  result
  |> should.be_ok
  |> should.equal("${}")
}

// ============================================================================
// Error Cases - Array Indexing Errors
// ============================================================================

pub fn interpolate_array_out_of_bounds_test() {
  let ctx = ctx_with_array()
  let result = interpolate.interpolate_string(ctx, "${items[999].name}")

  // Error occurs during path resolution, not interpolation parsing
  result
  |> should.be_error

  // The error should mention array bounds
  case result {
    Error(msg) -> {
      let has_num = string.contains(msg, "999")
      let has_bounds = string.contains(msg, "bounds")
      { has_num && has_bounds }
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn interpolate_array_negative_out_of_bounds_test() {
  let ctx = ctx_with_array()
  let result = interpolate.interpolate_string(ctx, "${items[-999].name}")

  result
  |> should.be_error

  // Should indicate out of bounds
  case result {
    Error(msg) -> {
      let has_neg = string.contains(msg, "-999")
      let has_bounds = string.contains(msg, "bounds")
      { has_neg && has_bounds }
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn interpolate_indexing_non_array_error_test() {
  let ctx = ctx_with_object()
  let result = interpolate.interpolate_string(ctx, "${user[0]}")

  result
  |> should.be_error

  // Should indicate cannot index non-array
  case result {
    Error(msg) -> {
      let has_index = string.contains(msg, "index")
      let has_non_array = string.contains(msg, "non-array")
      { has_index && has_non_array }
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn interpolate_invalid_array_index_syntax_test() {
  // Array syntax is validated and gives a specific error
  let ctx = ctx_with_array()
  let result = interpolate.interpolate_string(ctx, "${items[abc]}")

  result
  |> should.be_error
  // Error message indicates the index must be numeric
  |> should.equal("Array index must be a number: abc")
}

pub fn interpolate_malformed_array_syntax_missing_bracket_test() {
  // Malformed array syntax is detected and reported
  let ctx = ctx_with_array()
  let result = interpolate.interpolate_string(ctx, "${items[0}")

  result
  |> should.be_error
  // Error message indicates missing closing bracket
  |> should.equal("Missing closing ] in array index: items[0")
}

// ============================================================================
// Error Cases - First Error Stops Processing
// ============================================================================

pub fn interpolate_multiple_errors_returns_first_test() {
  let ctx = empty_ctx()
  let result = interpolate.interpolate_string(ctx, "${missing1} ${missing2}")

  // Should fail on first missing variable
  result
  |> should.be_error
  |> should.equal("Variable not found: missing1")
}

pub fn interpolate_mixed_success_and_error_test() {
  let ctx = ctx_with_string("valid", "ok")
  let result = interpolate.interpolate_string(ctx, "${valid} ${invalid}")

  // Should fail on invalid variable even though first is valid
  result
  |> should.be_error
  |> should.equal("Variable not found: invalid")
}

// ============================================================================
// Variable Name Case Sensitivity
// ============================================================================

pub fn interpolate_case_sensitive_variable_names_test() {
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("UserId", json.string("uppercase"))
    |> interpolate.set_variable("userid", json.string("lowercase"))

  let result1 = interpolate.interpolate_string(ctx, "${UserId}")
  let result2 = interpolate.interpolate_string(ctx, "${userid}")

  result1
  |> should.be_ok
  |> should.equal("uppercase")

  result2
  |> should.be_ok
  |> should.equal("lowercase")
}

pub fn interpolate_case_sensitive_missing_test() {
  let ctx = ctx_with_string("userId", "123")
  let result = interpolate.interpolate_string(ctx, "${UserId}")

  result
  |> should.be_error
  |> should.equal("Variable not found: UserId")
}

// ============================================================================
// Headers Interpolation
// ============================================================================

pub fn interpolate_headers_empty_dict_test() {
  let ctx = empty_ctx()
  let result = interpolate.interpolate_headers(ctx, dict.new())

  result
  |> should.be_ok
  |> should.equal(dict.new())
}

pub fn interpolate_headers_no_variables_test() {
  let ctx = empty_ctx()
  let headers =
    dict.new()
    |> dict.insert("Content-Type", "application/json")
    |> dict.insert("Accept", "application/json")

  let result = interpolate.interpolate_headers(ctx, headers)

  result
  |> should.be_ok
  |> should.equal(headers)
}

pub fn interpolate_headers_with_variables_test() {
  let ctx = ctx_with_string("token", "abc123")
  let headers =
    dict.new()
    |> dict.insert("Authorization", "Bearer ${token}")
    |> dict.insert("Content-Type", "application/json")

  let result = interpolate.interpolate_headers(ctx, headers)

  result
  |> should.be_ok

  case result {
    Ok(interpolated) -> {
      dict.get(interpolated, "Authorization")
      |> should.be_ok
      |> should.equal("Bearer abc123")

      dict.get(interpolated, "Content-Type")
      |> should.be_ok
      |> should.equal("application/json")
    }
    Error(_) -> should.fail()
  }
}

pub fn interpolate_headers_error_stops_processing_test() {
  let ctx = empty_ctx()
  let headers =
    dict.new()
    |> dict.insert("Valid-Header", "no-vars")
    |> dict.insert("Invalid-Header", "Bearer ${missing_token}")

  let result = interpolate.interpolate_headers(ctx, headers)

  result
  |> should.be_error
  |> should.equal("Variable not found: missing_token")
}

// ============================================================================
// Extract Capture Function
// ============================================================================

pub fn extract_capture_simple_variable_test() {
  let ctx = ctx_with_string("id", "xyz789")
  let result = interpolate.extract_capture(ctx, "id")

  // Value should be JSON string
  case result {
    Ok(json_val) -> {
      json.to_string(json_val)
      |> should.equal("\"xyz789\"")
    }
    Error(_) -> should.fail()
  }
}

pub fn extract_capture_nested_path_test() {
  let ctx = ctx_with_object()
  let result = interpolate.extract_capture(ctx, "user.email")

  case result {
    Ok(json_val) -> {
      json.to_string(json_val)
      |> should.equal("\"alice@example.com\"")
    }
    Error(_) -> should.fail()
  }
}

pub fn extract_capture_array_index_test() {
  // extract_capture now correctly handles array indexing
  let ctx = ctx_with_array()
  let result = interpolate.extract_capture(ctx, "items[0].id")

  // Should successfully extract the id field from first array element
  case result {
    Ok(json_val) -> {
      json.to_string(json_val)
      |> should.equal("1")
    }
    Error(_) -> should.fail()
  }
}

pub fn extract_capture_response_body_test() {
  let ctx = ctx_with_bodies()
  let result = interpolate.extract_capture(ctx, "response.body.status")

  case result {
    Ok(json_val) -> {
      json.to_string(json_val)
      |> should.equal("\"ok\"")
    }
    Error(_) -> should.fail()
  }
}

pub fn extract_capture_missing_variable_error_test() {
  let ctx = empty_ctx()
  let result = interpolate.extract_capture(ctx, "missing")

  result
  |> should.be_error
  |> should.equal("Variable not found: missing")
}

// ============================================================================
// JSON to String Conversion
// ============================================================================

pub fn json_to_string_unwraps_string_quotes_test() {
  let json_val = json.string("hello")
  let result = interpolate.json_to_string(json_val)

  result
  |> should.equal("hello")
}

pub fn json_to_string_number_test() {
  let json_val = json.int(42)
  let result = interpolate.json_to_string(json_val)

  result
  |> should.equal("42")
}

pub fn json_to_string_boolean_test() {
  let json_val = json.bool(True)
  let result = interpolate.json_to_string(json_val)

  result
  |> should.equal("true")
}

pub fn json_to_string_null_test() {
  let json_val = json.null()
  let result = interpolate.json_to_string(json_val)

  result
  |> should.equal("null")
}

pub fn json_to_string_object_keeps_json_test() {
  let json_val = json.object([#("key", json.string("value"))])
  let result = interpolate.json_to_string(json_val)

  // Objects should remain JSON-encoded (start with opening brace)
  string.contains(result, "{")
  |> should.be_true
}

pub fn json_to_string_array_keeps_json_test() {
  let arr = json.array([json.int(1), json.int(2)], of: fn(x) { x })
  let result = interpolate.json_to_string(arr)

  // Arrays should remain JSON-encoded (start with bracket)
  string.contains(result, "[")
  |> should.be_true
}

// ============================================================================
// Context Manipulation Functions
// ============================================================================

pub fn new_context_has_empty_variables_test() {
  let ctx = interpolate.new_context()
  let result = interpolate.get_variable(ctx, "anything")

  result
  |> should.equal(option.None)
}

pub fn set_variable_and_retrieve_test() {
  let ctx =
    interpolate.new_context()
    |> interpolate.set_variable("test", json.string("value"))

  let result = interpolate.get_variable(ctx, "test")

  result
  |> should.equal(option.Some(json.string("value")))
}

pub fn get_variable_missing_returns_none_test() {
  let ctx = interpolate.new_context()
  let result = interpolate.get_variable(ctx, "missing")

  result
  |> should.equal(option.None)
}

pub fn set_request_body_test() {
  let body = json.object([#("key", json.string("value"))])
  let ctx =
    interpolate.new_context()
    |> interpolate.set_request_body(body)

  let result = interpolate.interpolate_string(ctx, "${request.body.key}")

  result
  |> should.be_ok
  |> should.equal("value")
}

pub fn set_response_body_test() {
  let body = json.object([#("status", json.string("success"))])
  let ctx =
    interpolate.new_context()
    |> interpolate.set_response_body(body)

  let result = interpolate.interpolate_string(ctx, "${response.body.status}")

  result
  |> should.be_ok
  |> should.equal("success")
}

// ============================================================================
// Edge Cases - Special Characters and Boundary Values
// ============================================================================

pub fn interpolate_special_characters_in_string_test() {
  let ctx = ctx_with_string("emoji", "🎉")
  let result = interpolate.interpolate_string(ctx, "Party time: ${emoji}!")

  result
  |> should.be_ok
  |> should.equal("Party time: 🎉!")
}

pub fn interpolate_newlines_in_value_test() {
  // JSON encoding escapes newlines as \\n
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("multiline", json.string("line1\nline2"))

  let result = interpolate.interpolate_string(ctx, "Text: ${multiline}")

  result
  |> should.be_ok
  // JSON string encoding escapes the newline
  |> should.equal("Text: line1\\nline2")
}

pub fn interpolate_preserves_literal_dollar_without_brace_test() {
  let ctx = empty_ctx()
  let result = interpolate.interpolate_string(ctx, "Price: $50")

  result
  |> should.be_ok
  |> should.equal("Price: $50")
}

pub fn interpolate_preserves_unmatched_brace_test() {
  let ctx = empty_ctx()
  let result = interpolate.interpolate_string(ctx, "Random { or } braces")

  result
  |> should.be_ok
  |> should.equal("Random { or } braces")
}

// ============================================================================
// Circular Reference Detection Tests
// ============================================================================

pub fn interpolate_direct_self_reference_test() {
  // Variable references itself directly: x = "${x}"
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("x", json.string("${x}"))

  let result = interpolate.interpolate_string(ctx, "${x}")

  result
  |> should.be_error

  // Should detect circular reference
  case result {
    Error(msg) -> {
      string.contains(msg, "Circular variable reference")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn interpolate_two_way_cycle_test() {
  // Two variables reference each other: a = "${b}", b = "${a}"
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("a", json.string("${b}"))
    |> interpolate.set_variable("b", json.string("${a}"))

  let result = interpolate.interpolate_string(ctx, "${a}")

  result
  |> should.be_error

  case result {
    Error(msg) -> {
      string.contains(msg, "Circular variable reference")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn interpolate_three_way_cycle_test() {
  // Three-way cycle: a -> b -> c -> a
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("a", json.string("${b}"))
    |> interpolate.set_variable("b", json.string("${c}"))
    |> interpolate.set_variable("c", json.string("${a}"))

  let result = interpolate.interpolate_string(ctx, "${a}")

  result
  |> should.be_error

  case result {
    Error(msg) -> {
      string.contains(msg, "Circular variable reference")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn interpolate_long_cycle_test() {
  // Long chain with cycle at the end
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("a", json.string("${b}"))
    |> interpolate.set_variable("b", json.string("${c}"))
    |> interpolate.set_variable("c", json.string("${d}"))
    |> interpolate.set_variable("d", json.string("${e}"))
    |> interpolate.set_variable("e", json.string("${a}"))

  let result = interpolate.interpolate_string(ctx, "${a}")

  result
  |> should.be_error

  case result {
    Error(msg) -> {
      string.contains(msg, "Circular variable reference")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Depth Limit Tests
// ============================================================================

pub fn interpolate_deep_chain_within_limit_test() {
  // Create a chain of 5 levels (well within the limit of 10)
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("level1", json.string("${level2}"))
    |> interpolate.set_variable("level2", json.string("${level3}"))
    |> interpolate.set_variable("level3", json.string("${level4}"))
    |> interpolate.set_variable("level4", json.string("${level5}"))
    |> interpolate.set_variable("level5", json.string("final_value"))

  let result = interpolate.interpolate_string(ctx, "${level1}")

  result
  |> should.be_ok
  |> should.equal("final_value")
}

pub fn interpolate_exactly_at_depth_limit_test() {
  // Create a chain of exactly 10 levels (at the limit)
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("l1", json.string("${l2}"))
    |> interpolate.set_variable("l2", json.string("${l3}"))
    |> interpolate.set_variable("l3", json.string("${l4}"))
    |> interpolate.set_variable("l4", json.string("${l5}"))
    |> interpolate.set_variable("l5", json.string("${l6}"))
    |> interpolate.set_variable("l6", json.string("${l7}"))
    |> interpolate.set_variable("l7", json.string("${l8}"))
    |> interpolate.set_variable("l8", json.string("${l9}"))
    |> interpolate.set_variable("l9", json.string("${l10}"))
    |> interpolate.set_variable("l10", json.string("final"))

  let result = interpolate.interpolate_string(ctx, "${l1}")

  result
  |> should.be_ok
  |> should.equal("final")
}

pub fn interpolate_exceeds_depth_limit_test() {
  // Create a chain exceeding the depth limit of 10
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("l1", json.string("${l2}"))
    |> interpolate.set_variable("l2", json.string("${l3}"))
    |> interpolate.set_variable("l3", json.string("${l4}"))
    |> interpolate.set_variable("l4", json.string("${l5}"))
    |> interpolate.set_variable("l5", json.string("${l6}"))
    |> interpolate.set_variable("l6", json.string("${l7}"))
    |> interpolate.set_variable("l7", json.string("${l8}"))
    |> interpolate.set_variable("l8", json.string("${l9}"))
    |> interpolate.set_variable("l9", json.string("${l10}"))
    |> interpolate.set_variable("l10", json.string("${l11}"))
    |> interpolate.set_variable("l11", json.string("${l12}"))
    |> interpolate.set_variable("l12", json.string("final"))

  let result = interpolate.interpolate_string(ctx, "${l1}")

  result
  |> should.be_error

  case result {
    Error(msg) -> {
      string.contains(msg, "depth limit exceeded")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Valid Nested Interpolation (No Cycles)
// ============================================================================

pub fn interpolate_valid_nested_no_cycle_test() {
  // Valid nesting without cycles
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("name", json.string("Alice"))
    |> interpolate.set_variable("greeting", json.string("Hello ${name}"))

  let result = interpolate.interpolate_string(ctx, "${greeting}!")

  result
  |> should.be_ok
  |> should.equal("Hello Alice!")
}

pub fn interpolate_multiple_nested_no_cycle_test() {
  // Multiple levels of valid nesting
  let ctx =
    empty_ctx()
    |> interpolate.set_variable("base", json.string("value"))
    |> interpolate.set_variable("mid", json.string("prefix-${base}"))
    |> interpolate.set_variable("top", json.string("START ${mid} END"))

  let result = interpolate.interpolate_string(ctx, "${top}")

  result
  |> should.be_ok
  |> should.equal("START prefix-value END")
}
