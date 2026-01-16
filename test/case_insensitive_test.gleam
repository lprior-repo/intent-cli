//// Comprehensive tests for intent/case_insensitive.gleam
//// Tests cover all case-insensitive comparison functions with edge cases
////
//// Design by Contract:
//// - Preconditions: Valid string inputs (including empty, Unicode, special chars)
//// - Postconditions: Case-insensitive comparisons return correct boolean results
//// - Invariants: Comparison results match standard library with lowercase normalization

import gleeunit
import gleeunit/should
import intent/case_insensitive

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// contains_ignore_case Tests
// ============================================================================

pub fn contains_ignore_case_basic_match_test() {
  case_insensitive.contains_ignore_case("Hello World", "world")
  |> should.be_true

  case_insensitive.contains_ignore_case("Hello World", "WORLD")
  |> should.be_true

  case_insensitive.contains_ignore_case("Hello World", "WoRlD")
  |> should.be_true
}

pub fn contains_ignore_case_no_match_test() {
  case_insensitive.contains_ignore_case("Hello World", "xyz")
  |> should.be_false

  case_insensitive.contains_ignore_case("Hello World", "goodbye")
  |> should.be_false
}

pub fn contains_ignore_case_empty_needle_test() {
  // Empty string is contained in any string
  case_insensitive.contains_ignore_case("Hello World", "")
  |> should.be_true

  case_insensitive.contains_ignore_case("", "")
  |> should.be_true
}

pub fn contains_ignore_case_empty_haystack_test() {
  case_insensitive.contains_ignore_case("", "hello")
  |> should.be_false
}

pub fn contains_ignore_case_exact_match_test() {
  case_insensitive.contains_ignore_case("hello", "hello")
  |> should.be_true

  case_insensitive.contains_ignore_case("HELLO", "hello")
  |> should.be_true
}

pub fn contains_ignore_case_substring_test() {
  case_insensitive.contains_ignore_case("FooBarBaz", "bar")
  |> should.be_true

  case_insensitive.contains_ignore_case("FooBarBaz", "BAR")
  |> should.be_true
}

pub fn contains_ignore_case_unicode_test() {
  case_insensitive.contains_ignore_case("Café Münchën", "café")
  |> should.be_true

  case_insensitive.contains_ignore_case("CAFÉ MÜNCHËN", "münchën")
  |> should.be_true

  case_insensitive.contains_ignore_case("Hello 世界", "世界")
  |> should.be_true
}

pub fn contains_ignore_case_special_chars_test() {
  case_insensitive.contains_ignore_case("API_V2.0", "api_v2")
  |> should.be_true

  case_insensitive.contains_ignore_case("user@example.com", "EXAMPLE.COM")
  |> should.be_true

  case_insensitive.contains_ignore_case("path/to/file", "TO/FILE")
  |> should.be_true
}

pub fn contains_ignore_case_whitespace_test() {
  case_insensitive.contains_ignore_case("Hello World", "hello world")
  |> should.be_true

  case_insensitive.contains_ignore_case("  Spaced  ", "spaced")
  |> should.be_true
}

// ============================================================================
// contains_any_ignore_case Tests
// ============================================================================

pub fn contains_any_ignore_case_match_first_test() {
  case_insensitive.contains_any_ignore_case("User API", ["api", "endpoint"])
  |> should.be_true
}

pub fn contains_any_ignore_case_match_second_test() {
  case_insensitive.contains_any_ignore_case("API Endpoint", ["cli", "endpoint"])
  |> should.be_true
}

pub fn contains_any_ignore_case_match_multiple_test() {
  case_insensitive.contains_any_ignore_case("User API Endpoint", [
    "api", "endpoint",
  ])
  |> should.be_true
}

pub fn contains_any_ignore_case_no_match_test() {
  case_insensitive.contains_any_ignore_case("CLI Command", ["api", "endpoint"])
  |> should.be_false
}

pub fn contains_any_ignore_case_empty_list_test() {
  case_insensitive.contains_any_ignore_case("Hello World", [])
  |> should.be_false
}

pub fn contains_any_ignore_case_case_variations_test() {
  case_insensitive.contains_any_ignore_case("User API", ["API", "ENDPOINT"])
  |> should.be_true

  case_insensitive.contains_any_ignore_case("USER api", ["api", "endpoint"])
  |> should.be_true
}

pub fn contains_any_ignore_case_unicode_test() {
  case_insensitive.contains_any_ignore_case("Café API", ["café", "endpoint"])
  |> should.be_true

  case_insensitive.contains_any_ignore_case("CAFÉ API", ["café", "endpoint"])
  |> should.be_true
}

pub fn contains_any_ignore_case_single_needle_test() {
  case_insensitive.contains_any_ignore_case("Hello", ["hello"])
  |> should.be_true

  case_insensitive.contains_any_ignore_case("Hello", ["goodbye"])
  |> should.be_false
}

pub fn contains_any_ignore_case_empty_string_in_list_test() {
  // Empty string matches anything
  case_insensitive.contains_any_ignore_case("Hello", [""])
  |> should.be_true
}

// ============================================================================
// contains_all_ignore_case Tests
// ============================================================================

pub fn contains_all_ignore_case_match_all_test() {
  case_insensitive.contains_all_ignore_case("User API Endpoint", [
    "api", "endpoint",
  ])
  |> should.be_true
}

pub fn contains_all_ignore_case_missing_one_test() {
  case_insensitive.contains_all_ignore_case("User API", ["api", "endpoint"])
  |> should.be_false
}

pub fn contains_all_ignore_case_empty_list_test() {
  // All of nothing is true (vacuous truth)
  case_insensitive.contains_all_ignore_case("Hello World", [])
  |> should.be_true
}

pub fn contains_all_ignore_case_case_variations_test() {
  case_insensitive.contains_all_ignore_case("User API Endpoint", [
    "USER", "api", "ENDPOINT",
  ])
  |> should.be_true
}

pub fn contains_all_ignore_case_order_independent_test() {
  case_insensitive.contains_all_ignore_case("Endpoint API User", [
    "user", "api", "endpoint",
  ])
  |> should.be_true
}

pub fn contains_all_ignore_case_partial_match_test() {
  case_insensitive.contains_all_ignore_case("User", ["user", "api"])
  |> should.be_false
}

pub fn contains_all_ignore_case_unicode_test() {
  case_insensitive.contains_all_ignore_case("Café Münchën API", [
    "café", "münchën",
  ])
  |> should.be_true
}

pub fn contains_all_ignore_case_single_needle_test() {
  case_insensitive.contains_all_ignore_case("Hello World", ["hello"])
  |> should.be_true

  case_insensitive.contains_all_ignore_case("Hello World", ["goodbye"])
  |> should.be_false
}

pub fn contains_all_ignore_case_duplicates_test() {
  // Duplicates should still work
  case_insensitive.contains_all_ignore_case("Hello", ["hello", "hello"])
  |> should.be_true
}

// ============================================================================
// equals_ignore_case Tests
// ============================================================================

pub fn equals_ignore_case_exact_match_test() {
  case_insensitive.equals_ignore_case("Hello", "hello")
  |> should.be_true

  case_insensitive.equals_ignore_case("Hello", "HELLO")
  |> should.be_true

  case_insensitive.equals_ignore_case("HeLLo", "hElLO")
  |> should.be_true
}

pub fn equals_ignore_case_different_test() {
  case_insensitive.equals_ignore_case("Hello", "World")
  |> should.be_false

  case_insensitive.equals_ignore_case("Hello", "Hellooo")
  |> should.be_false
}

pub fn equals_ignore_case_empty_strings_test() {
  case_insensitive.equals_ignore_case("", "")
  |> should.be_true
}

pub fn equals_ignore_case_one_empty_test() {
  case_insensitive.equals_ignore_case("Hello", "")
  |> should.be_false

  case_insensitive.equals_ignore_case("", "Hello")
  |> should.be_false
}

pub fn equals_ignore_case_unicode_test() {
  case_insensitive.equals_ignore_case("Café", "café")
  |> should.be_true

  case_insensitive.equals_ignore_case("CAFÉ", "café")
  |> should.be_true

  case_insensitive.equals_ignore_case("Münchën", "MÜNCHËN")
  |> should.be_true
}

pub fn equals_ignore_case_special_chars_test() {
  case_insensitive.equals_ignore_case("API_V2.0", "api_v2.0")
  |> should.be_true

  case_insensitive.equals_ignore_case("user@example.com", "USER@EXAMPLE.COM")
  |> should.be_true
}

pub fn equals_ignore_case_whitespace_sensitive_test() {
  // Whitespace is part of the comparison
  case_insensitive.equals_ignore_case("Hello World", "HelloWorld")
  |> should.be_false

  case_insensitive.equals_ignore_case("Hello World", "hello world")
  |> should.be_true
}

pub fn equals_ignore_case_numbers_test() {
  case_insensitive.equals_ignore_case("API123", "api123")
  |> should.be_true

  case_insensitive.equals_ignore_case("V2.0", "v2.0")
  |> should.be_true
}

// ============================================================================
// starts_with_ignore_case Tests
// ============================================================================

pub fn starts_with_ignore_case_match_test() {
  case_insensitive.starts_with_ignore_case("HelloWorld", "hello")
  |> should.be_true

  case_insensitive.starts_with_ignore_case("HelloWorld", "HELLO")
  |> should.be_true

  case_insensitive.starts_with_ignore_case("HelloWorld", "HeLLo")
  |> should.be_true
}

pub fn starts_with_ignore_case_no_match_test() {
  case_insensitive.starts_with_ignore_case("HelloWorld", "world")
  |> should.be_false

  case_insensitive.starts_with_ignore_case("HelloWorld", "goodbye")
  |> should.be_false
}

pub fn starts_with_ignore_case_empty_prefix_test() {
  // Every string starts with empty string
  case_insensitive.starts_with_ignore_case("Hello", "")
  |> should.be_true

  case_insensitive.starts_with_ignore_case("", "")
  |> should.be_true
}

pub fn starts_with_ignore_case_exact_match_test() {
  case_insensitive.starts_with_ignore_case("Hello", "hello")
  |> should.be_true

  case_insensitive.starts_with_ignore_case("HELLO", "hello")
  |> should.be_true
}

pub fn starts_with_ignore_case_longer_prefix_test() {
  // Prefix longer than haystack
  case_insensitive.starts_with_ignore_case("Hi", "Hello")
  |> should.be_false
}

pub fn starts_with_ignore_case_unicode_test() {
  case_insensitive.starts_with_ignore_case("Café Bar", "café")
  |> should.be_true

  case_insensitive.starts_with_ignore_case("CAFÉ Bar", "café")
  |> should.be_true
}

pub fn starts_with_ignore_case_special_chars_test() {
  case_insensitive.starts_with_ignore_case("/api/v2/users", "/API/V2")
  |> should.be_true

  case_insensitive.starts_with_ignore_case("_private_method", "_PRIVATE")
  |> should.be_true
}

pub fn starts_with_ignore_case_whitespace_test() {
  case_insensitive.starts_with_ignore_case("  Hello", "  hello")
  |> should.be_true

  case_insensitive.starts_with_ignore_case("Hello", " Hello")
  |> should.be_false
}

// ============================================================================
// ends_with_ignore_case Tests
// ============================================================================

pub fn ends_with_ignore_case_match_test() {
  case_insensitive.ends_with_ignore_case("HelloWorld", "world")
  |> should.be_true

  case_insensitive.ends_with_ignore_case("HelloWorld", "WORLD")
  |> should.be_true

  case_insensitive.ends_with_ignore_case("HelloWorld", "WoRLd")
  |> should.be_true
}

pub fn ends_with_ignore_case_no_match_test() {
  case_insensitive.ends_with_ignore_case("HelloWorld", "hello")
  |> should.be_false

  case_insensitive.ends_with_ignore_case("HelloWorld", "goodbye")
  |> should.be_false
}

pub fn ends_with_ignore_case_empty_suffix_test() {
  // Every string ends with empty string
  case_insensitive.ends_with_ignore_case("Hello", "")
  |> should.be_true

  case_insensitive.ends_with_ignore_case("", "")
  |> should.be_true
}

pub fn ends_with_ignore_case_exact_match_test() {
  case_insensitive.ends_with_ignore_case("Hello", "hello")
  |> should.be_true

  case_insensitive.ends_with_ignore_case("HELLO", "hello")
  |> should.be_true
}

pub fn ends_with_ignore_case_longer_suffix_test() {
  // Suffix longer than haystack
  case_insensitive.ends_with_ignore_case("Hi", "Hello")
  |> should.be_false
}

pub fn ends_with_ignore_case_unicode_test() {
  case_insensitive.ends_with_ignore_case("Bar Café", "café")
  |> should.be_true

  case_insensitive.ends_with_ignore_case("Bar CAFÉ", "café")
  |> should.be_true
}

pub fn ends_with_ignore_case_special_chars_test() {
  case_insensitive.ends_with_ignore_case("/api/v2/users.json", "USERS.JSON")
  |> should.be_true

  case_insensitive.ends_with_ignore_case("file.TXT", ".txt")
  |> should.be_true
}

pub fn ends_with_ignore_case_whitespace_test() {
  case_insensitive.ends_with_ignore_case("Hello  ", "hello  ")
  |> should.be_true

  case_insensitive.ends_with_ignore_case("Hello", "Hello ")
  |> should.be_false
}

// ============================================================================
// Cross-function Integration Tests
// ============================================================================

pub fn integration_all_functions_consistent_test() {
  let haystack = "User API Endpoint"
  let needle = "API"

  // All these should be true for consistent behavior
  case_insensitive.contains_ignore_case(haystack, needle)
  |> should.be_true

  case_insensitive.contains_any_ignore_case(haystack, [needle])
  |> should.be_true

  case_insensitive.contains_all_ignore_case(haystack, [needle])
  |> should.be_true
}

pub fn integration_empty_string_consistency_test() {
  let haystack = "Test"

  case_insensitive.contains_ignore_case(haystack, "")
  |> should.be_true

  case_insensitive.starts_with_ignore_case(haystack, "")
  |> should.be_true

  case_insensitive.ends_with_ignore_case(haystack, "")
  |> should.be_true

  case_insensitive.equals_ignore_case("", "")
  |> should.be_true
}

pub fn integration_case_variations_all_functions_test() {
  let haystack = "HelloWorld"

  // Test all variations of "hello"
  case_insensitive.starts_with_ignore_case(haystack, "hello")
  |> should.be_true

  case_insensitive.starts_with_ignore_case(haystack, "HELLO")
  |> should.be_true

  case_insensitive.starts_with_ignore_case(haystack, "HeLLo")
  |> should.be_true

  // Test all variations of "world"
  case_insensitive.ends_with_ignore_case(haystack, "world")
  |> should.be_true

  case_insensitive.ends_with_ignore_case(haystack, "WORLD")
  |> should.be_true

  case_insensitive.ends_with_ignore_case(haystack, "WoRLd")
  |> should.be_true
}
