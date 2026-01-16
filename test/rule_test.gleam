//// Comprehensive tests for intent/rule.gleam
//// Tests cover rule parsing, all RuleExpr variants, and edge cases
////
//// Coverage:
//// - Equality rules (strings, ints, floats, booleans, variables)
//// - Type validation rules (string, integer, number, boolean, array, object, null)
//// - String pattern rules (matching, starting with, ending with, containing, formats)
//// - Numeric comparison rules (>=, >, <=, <, between)
//// - Presence rules (present, absent, not null)
//// - Array rules (non-empty, length, min/max, where each)
//// - Compound rules (valid JWT, valid ISO8601, one of)
//// - Variable interpolation (${var})
//// - Edge cases (whitespace, empty strings, invalid formats)
//// - Raw fallback for unparseable rules

import gleam/list
import gleeunit
import gleeunit/should
import intent/rule.{
  Absent, ArrayOfLength, ArrayWhereEach, ArrayWithMaxItems, ArrayWithMinItems,
  ContainsVariable, Equals, EqualsBool, EqualsFloat, EqualsInt, EqualsVariable,
  IntegerBetween, IntegerGt, IntegerGte, IntegerLt, IntegerLte, IsArray,
  IsBoolean, IsEmail, IsInteger, IsIso8601, IsJwt, IsNull, IsNumber, IsObject,
  IsString, IsUri, IsUuid, NonEmptyArray, NonEmptyString, NotNull, NumberBetween,
  OneOf, Present, Raw, StringContaining, StringEndingWith, StringMatching,
  StringStartingWith, ValidIso8601, ValidJwt,
}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// A. Equality Rules - String Values
// ============================================================================

/// Test: "equals foo" should parse as Equals("foo")
pub fn equals_string_basic_test() {
  rule.parse("equals foo")
  |> should.equal(Equals("foo"))
}

/// Test: "equals active" with multi-char string
pub fn equals_string_multichar_test() {
  rule.parse("equals active")
  |> should.equal(Equals("active"))
}

/// Test: Whitespace around rule should be trimmed
pub fn equals_string_with_leading_whitespace_test() {
  rule.parse("  equals foo")
  |> should.equal(Equals("foo"))
}

/// Test: Trailing whitespace
pub fn equals_string_with_trailing_whitespace_test() {
  rule.parse("equals foo  ")
  |> should.equal(Equals("foo"))
}

/// Test: Empty string value should parse
pub fn equals_empty_string_test() {
  rule.parse("equals ")
  |> should.equal(Equals(""))
}

// ============================================================================
// B. Equality Rules - Integer Values
// ============================================================================

/// Test: "equals 42" should parse as EqualsInt(42)
pub fn equals_int_positive_test() {
  rule.parse("equals 42")
  |> should.equal(EqualsInt(42))
}

/// Test: "equals 0" should parse as EqualsInt(0)
pub fn equals_int_zero_test() {
  rule.parse("equals 0")
  |> should.equal(EqualsInt(0))
}

/// Test: "equals -5" should parse as EqualsInt(-5)
pub fn equals_int_negative_test() {
  rule.parse("equals -5")
  |> should.equal(EqualsInt(-5))
}

/// Test: Large integer
pub fn equals_int_large_test() {
  rule.parse("equals 999999")
  |> should.equal(EqualsInt(999_999))
}

// ============================================================================
// C. Equality Rules - Float Values
// ============================================================================

/// Test: "equals 3.14" should parse as EqualsFloat(3.14)
pub fn equals_float_basic_test() {
  rule.parse("equals 3.14")
  |> should.equal(EqualsFloat(3.14))
}

/// Test: "equals 0.0" should parse as EqualsFloat(0.0)
pub fn equals_float_zero_test() {
  rule.parse("equals 0.0")
  |> should.equal(EqualsFloat(0.0))
}

/// Test: "equals -2.5" should parse as EqualsFloat(-2.5)
pub fn equals_float_negative_test() {
  rule.parse("equals -2.5")
  |> should.equal(EqualsFloat(-2.5))
}

/// Test: Float with many decimal places
pub fn equals_float_precision_test() {
  rule.parse("equals 3.14159265")
  |> should.equal(EqualsFloat(3.14159265))
}

// ============================================================================
// D. Equality Rules - Boolean Values
// ============================================================================

/// Test: "equals true" should parse as EqualsBool(True)
pub fn equals_bool_true_test() {
  rule.parse("equals true")
  |> should.equal(EqualsBool(True))
}

/// Test: "equals false" should parse as EqualsBool(False)
pub fn equals_bool_false_test() {
  rule.parse("equals false")
  |> should.equal(EqualsBool(False))
}

// ============================================================================
// E. Equality Rules - Variables
// ============================================================================

/// Test: "equals ${user_id}" should parse as EqualsVariable("user_id")
pub fn equals_variable_basic_test() {
  rule.parse("equals ${user_id}")
  |> should.equal(EqualsVariable("user_id"))
}

/// Test: Variable with underscores
pub fn equals_variable_with_underscore_test() {
  rule.parse("equals ${created_user_id}")
  |> should.equal(EqualsVariable("created_user_id"))
}

/// Test: Variable with hyphens
pub fn equals_variable_with_hyphen_test() {
  rule.parse("equals ${user-id}")
  |> should.equal(EqualsVariable("user-id"))
}

/// Test: Single character variable
pub fn equals_variable_single_char_test() {
  rule.parse("equals ${x}")
  |> should.equal(EqualsVariable("x"))
}

// ============================================================================
// F. Type Validation Rules
// ============================================================================

/// Test: "string" should parse as IsString
pub fn type_string_test() {
  rule.parse("string")
  |> should.equal(IsString)
}

/// Test: "integer" should parse as IsInteger
pub fn type_integer_test() {
  rule.parse("integer")
  |> should.equal(IsInteger)
}

/// Test: "number" should parse as IsNumber
pub fn type_number_test() {
  rule.parse("number")
  |> should.equal(IsNumber)
}

/// Test: "boolean" should parse as IsBoolean
pub fn type_boolean_test() {
  rule.parse("boolean")
  |> should.equal(IsBoolean)
}

/// Test: "array" should parse as IsArray
pub fn type_array_test() {
  rule.parse("array")
  |> should.equal(IsArray)
}

/// Test: "object" should parse as IsObject
pub fn type_object_test() {
  rule.parse("object")
  |> should.equal(IsObject)
}

/// Test: "null" should parse as IsNull
pub fn type_null_test() {
  rule.parse("null")
  |> should.equal(IsNull)
}

/// Test: Type rules with whitespace
pub fn type_with_whitespace_test() {
  rule.parse("  string  ")
  |> should.equal(IsString)
}

// ============================================================================
// G. String Pattern Rules - Basic Patterns
// ============================================================================

/// Test: "non-empty string"
pub fn non_empty_string_test() {
  rule.parse("non-empty string")
  |> should.equal(NonEmptyString)
}

/// Test: "email"
pub fn email_format_test() {
  rule.parse("email")
  |> should.equal(IsEmail)
}

/// Test: "uuid"
pub fn uuid_format_test() {
  rule.parse("uuid")
  |> should.equal(IsUuid)
}

/// Test: "uri"
pub fn uri_format_test() {
  rule.parse("uri")
  |> should.equal(IsUri)
}

/// Test: "jwt"
pub fn jwt_format_test() {
  rule.parse("jwt")
  |> should.equal(IsJwt)
}

/// Test: "iso8601 datetime"
pub fn iso8601_format_test() {
  rule.parse("iso8601 datetime")
  |> should.equal(IsIso8601)
}

// ============================================================================
// H. String Pattern Rules - Pattern Matching
// ============================================================================

/// Test: "string matching ^[a-z]+$"
pub fn string_matching_regex_test() {
  rule.parse("string matching ^[a-z]+$")
  |> should.equal(StringMatching("^[a-z]+$"))
}

/// Test: "string starting with user_"
pub fn string_starting_with_test() {
  rule.parse("string starting with user_")
  |> should.equal(StringStartingWith("user_"))
}

/// Test: "string ending with .pdf"
pub fn string_ending_with_test() {
  rule.parse("string ending with .pdf")
  |> should.equal(StringEndingWith(".pdf"))
}

/// Test: "string containing hello"
pub fn string_containing_test() {
  rule.parse("string containing hello")
  |> should.equal(StringContaining("hello"))
}

/// Test: String patterns with empty value
pub fn string_starting_with_empty_test() {
  rule.parse("string starting with ")
  |> should.equal(StringStartingWith(""))
}

/// Test: String pattern with special characters
pub fn string_containing_special_chars_test() {
  rule.parse("string containing @#$%")
  |> should.equal(StringContaining("@#$%"))
}

// ============================================================================
// I. Numeric Comparison Rules
// ============================================================================

/// Test: "integer >= 5"
pub fn integer_gte_test() {
  rule.parse("integer >= 5")
  |> should.equal(IntegerGte(5))
}

/// Test: "integer >= 0"
pub fn integer_gte_zero_test() {
  rule.parse("integer >= 0")
  |> should.equal(IntegerGte(0))
}

/// Test: "integer >= -10"
pub fn integer_gte_negative_test() {
  rule.parse("integer >= -10")
  |> should.equal(IntegerGte(-10))
}

/// Test: "integer > 5"
pub fn integer_gt_test() {
  rule.parse("integer > 5")
  |> should.equal(IntegerGt(5))
}

/// Test: "integer <= 10"
pub fn integer_lte_test() {
  rule.parse("integer <= 10")
  |> should.equal(IntegerLte(10))
}

/// Test: "integer < 10"
pub fn integer_lt_test() {
  rule.parse("integer < 10")
  |> should.equal(IntegerLt(10))
}

/// Test: "integer > 5 and < 10" parses as IntegerBetween(6, 9)
pub fn integer_between_test() {
  rule.parse("integer > 5 and < 10")
  |> should.equal(IntegerBetween(6, 9))
}

/// Test: "integer > 0 and < 100"
pub fn integer_between_wide_range_test() {
  rule.parse("integer > 0 and < 100")
  |> should.equal(IntegerBetween(1, 99))
}

/// Test: "number between 1.5 and 10.5"
pub fn number_between_test() {
  rule.parse("number between 1.5 and 10.5")
  |> should.equal(NumberBetween(1.5, 10.5))
}

/// Test: "number between 0.0 and 1.0"
pub fn number_between_zero_to_one_test() {
  rule.parse("number between 0.0 and 1.0")
  |> should.equal(NumberBetween(0.0, 1.0))
}

/// Test: Numeric comparison with whitespace
pub fn integer_gte_with_whitespace_test() {
  rule.parse("  integer >= 5  ")
  |> should.equal(IntegerGte(5))
}

// ============================================================================
// J. Presence Rules
// ============================================================================

/// Test: "present"
pub fn present_rule_test() {
  rule.parse("present")
  |> should.equal(Present)
}

/// Test: "absent"
pub fn absent_rule_test() {
  rule.parse("absent")
  |> should.equal(Absent)
}

/// Test: "not null"
pub fn not_null_rule_test() {
  rule.parse("not null")
  |> should.equal(NotNull)
}

/// Test: Presence rules with whitespace
pub fn present_with_whitespace_test() {
  rule.parse("  present  ")
  |> should.equal(Present)
}

// ============================================================================
// K. Array Rules
// ============================================================================

/// Test: "non-empty array"
pub fn non_empty_array_test() {
  rule.parse("non-empty array")
  |> should.equal(NonEmptyArray)
}

/// Test: "array of length 5"
pub fn array_of_length_test() {
  rule.parse("array of length 5")
  |> should.equal(ArrayOfLength(5))
}

/// Test: "array of length 0"
pub fn array_of_length_zero_test() {
  rule.parse("array of length 0")
  |> should.equal(ArrayOfLength(0))
}

/// Test: "array with min 3 items"
pub fn array_with_min_items_test() {
  rule.parse("array with min 3 items")
  |> should.equal(ArrayWithMinItems(3))
}

/// Test: "array with min 1 item" (singular)
pub fn array_with_min_items_singular_test() {
  rule.parse("array with min 1 item")
  |> should.equal(ArrayWithMinItems(1))
}

/// Test: "array with max 10 items"
pub fn array_with_max_items_test() {
  rule.parse("array with max 10 items")
  |> should.equal(ArrayWithMaxItems(10))
}

/// Test: "array with max 1 item" (singular)
pub fn array_with_max_items_singular_test() {
  rule.parse("array with max 1 item")
  |> should.equal(ArrayWithMaxItems(1))
}

// ============================================================================
// L. Array Rules - ArrayWhereEach (Nested Rules)
// ============================================================================

/// Test: "array where each is string"
pub fn array_where_each_is_string_test() {
  rule.parse("array where each is string")
  |> should.equal(ArrayWhereEach(IsString))
}

/// Test: "array where each is integer"
pub fn array_where_each_is_integer_test() {
  rule.parse("array where each is integer")
  |> should.equal(ArrayWhereEach(IsInteger))
}

/// Test: "array where each matches ^[a-z]+$"
pub fn array_where_each_matches_pattern_test() {
  rule.parse("array where each matches ^[a-z]+$")
  |> should.equal(ArrayWhereEach(StringMatching("^[a-z]+$")))
}

/// Test: "array where each string" (without "is")
pub fn array_where_each_direct_type_test() {
  rule.parse("array where each string")
  |> should.equal(ArrayWhereEach(IsString))
}

// ============================================================================
// M. Compound Rules
// ============================================================================

/// Test: "valid JWT"
pub fn valid_jwt_test() {
  rule.parse("valid JWT")
  |> should.equal(ValidJwt)
}

/// Test: "valid ISO8601 datetime"
pub fn valid_iso8601_test() {
  rule.parse("valid ISO8601 datetime")
  |> should.equal(ValidIso8601)
}

/// Test: "one of [\"a\", \"b\", \"c\"]"
pub fn one_of_basic_test() {
  rule.parse("one of [\"a\", \"b\", \"c\"]")
  |> should.equal(OneOf(["a", "b", "c"]))
}

/// Test: "one of [\"active\", \"inactive\", \"pending\"]"
pub fn one_of_status_values_test() {
  rule.parse("one of [\"active\", \"inactive\", \"pending\"]")
  |> should.equal(OneOf(["active", "inactive", "pending"]))
}

/// Test: "one of [\"admin\"]" (single item)
pub fn one_of_single_item_test() {
  rule.parse("one of [\"admin\"]")
  |> should.equal(OneOf(["admin"]))
}

/// Test: "one of [\"a\",\"b\",\"c\"]" (no spaces)
pub fn one_of_no_spaces_test() {
  rule.parse("one of [\"a\",\"b\",\"c\"]")
  |> should.equal(OneOf(["a", "b", "c"]))
}

/// Test: "one of [ \"a\" , \"b\" ]" (extra spaces)
pub fn one_of_extra_spaces_test() {
  rule.parse("one of [ \"a\" , \"b\" ]")
  |> should.equal(OneOf(["a", "b"]))
}

// ============================================================================
// N. Contains Variable
// ============================================================================

/// Test: "contains ${username}"
pub fn contains_variable_test() {
  rule.parse("contains ${username}")
  |> should.equal(ContainsVariable("username"))
}

/// Test: "contains ${search_term}"
pub fn contains_variable_with_underscore_test() {
  rule.parse("contains ${search_term}")
  |> should.equal(ContainsVariable("search_term"))
}

// ============================================================================
// O. Raw Fallback (Unparseable Rules)
// ============================================================================

/// Test: Unknown rule formats fallback to Raw
pub fn raw_unknown_rule_test() {
  rule.parse("unknown rule format")
  |> should.equal(Raw("unknown rule format"))
}

/// Test: Partial match doesn't parse
pub fn raw_partial_equals_test() {
  rule.parse("equal foo")
  // Missing 's', should be Raw
  |> should.equal(Raw("equal foo"))
}

/// Test: Invalid integer comparison
pub fn raw_invalid_integer_comparison_test() {
  rule.parse("integer >= abc")
  // Non-numeric value
  |> should.equal(Raw("integer >= abc"))
}

/// Test: Malformed one_of
pub fn raw_malformed_one_of_test() {
  rule.parse("one of [a, b, c")
  // Missing closing bracket
  |> should.equal(Raw("one of [a, b, c"))
}

/// Test: Empty rule string
pub fn raw_empty_string_test() {
  rule.parse("")
  |> should.equal(Raw(""))
}

/// Test: Only whitespace
pub fn raw_only_whitespace_test() {
  rule.parse("   ")
  |> should.equal(Raw(""))
  // Trimmed to empty
}

/// Test: Complex unknown pattern
pub fn raw_complex_unknown_test() {
  rule.parse("must be valid according to RFC 9999")
  |> should.equal(Raw("must be valid according to RFC 9999"))
}

// ============================================================================
// P. Edge Cases - Number Parsing Priority
// ============================================================================

/// Test: "equals 123" should be EqualsInt(123), not Equals("123")
pub fn equals_prefers_int_over_string_test() {
  rule.parse("equals 123")
  |> should.equal(EqualsInt(123))
}

/// Test: "equals 3.14" should be EqualsFloat(3.14), not Equals("3.14")
pub fn equals_prefers_float_over_string_test() {
  rule.parse("equals 3.14")
  |> should.equal(EqualsFloat(3.14))
}

/// Test: "equals true" should be EqualsBool(True), not Equals("true")
pub fn equals_prefers_bool_over_string_test() {
  rule.parse("equals true")
  |> should.equal(EqualsBool(True))
}

/// Test: "equals 123abc" should be Equals("123abc") (not a valid int)
pub fn equals_invalid_int_is_string_test() {
  rule.parse("equals 123abc")
  |> should.equal(Equals("123abc"))
}

// ============================================================================
// Q. Edge Cases - Whitespace Handling
// ============================================================================

/// Test: Multiple spaces in rule
pub fn multiple_spaces_in_rule_test() {
  rule.parse("string  starting  with  foo")
  // This won't parse due to extra spaces, falls back to Raw
  |> should.equal(Raw("string  starting  with  foo"))
}

/// Test: Tab characters should be trimmed
pub fn tab_characters_trimmed_test() {
  rule.parse("\tpresent\t")
  |> should.equal(Present)
}

/// Test: Newline characters in rule
pub fn newline_in_rule_test() {
  rule.parse("present\n")
  |> should.equal(Raw("present\n"))
  // Newline not trimmed by string.trim in this implementation
}

// ============================================================================
// R. Edge Cases - Special Characters
// ============================================================================

/// Test: Equals with special characters
pub fn equals_special_characters_test() {
  rule.parse("equals foo@bar.com")
  |> should.equal(Equals("foo@bar.com"))
}

/// Test: Equals with spaces in value
pub fn equals_value_with_spaces_test() {
  rule.parse("equals hello world")
  |> should.equal(Equals("hello world"))
}

/// Test: Equals with quotes in value
pub fn equals_value_with_quotes_test() {
  rule.parse("equals \"quoted\"")
  |> should.equal(Equals("\"quoted\""))
}

// ============================================================================
// S. Round-Trip Tests (parse -> to_string -> parse)
// ============================================================================

/// Test: Parse and format equals string
pub fn round_trip_equals_string_test() {
  let original = "equals foo"
  let parsed = rule.parse(original)
  let formatted = rule.to_string(parsed)

  rule.parse(formatted)
  |> should.equal(parsed)
}

/// Test: Parse and format integer gte
pub fn round_trip_integer_gte_test() {
  let original = "integer >= 5"
  let parsed = rule.parse(original)
  let formatted = rule.to_string(parsed)

  rule.parse(formatted)
  |> should.equal(parsed)
}

/// Test: Parse and format type rule
pub fn round_trip_type_string_test() {
  let original = "string"
  let parsed = rule.parse(original)
  let formatted = rule.to_string(parsed)

  rule.parse(formatted)
  |> should.equal(parsed)
}

/// Test: Parse and format present rule
pub fn round_trip_present_test() {
  let original = "present"
  let parsed = rule.parse(original)
  let formatted = rule.to_string(parsed)

  rule.parse(formatted)
  |> should.equal(parsed)
}

/// Test: Parse and format one of rule
pub fn round_trip_one_of_test() {
  let original = "one of [\"a\", \"b\", \"c\"]"
  let parsed = rule.parse(original)
  let formatted = rule.to_string(parsed)

  // Verify the formatted version parses back correctly
  rule.parse(formatted)
  |> should.equal(parsed)
}

// ============================================================================
// T. to_string Function Tests
// ============================================================================

/// Test: to_string for Equals
pub fn to_string_equals_test() {
  rule.to_string(Equals("foo"))
  |> should.equal("equals foo")
}

/// Test: to_string for EqualsInt
pub fn to_string_equals_int_test() {
  rule.to_string(EqualsInt(42))
  |> should.equal("equals 42")
}

/// Test: to_string for EqualsFloat
pub fn to_string_equals_float_test() {
  rule.to_string(EqualsFloat(3.14))
  |> should.equal("equals 3.14")
}

/// Test: to_string for EqualsBool True
pub fn to_string_equals_bool_true_test() {
  rule.to_string(EqualsBool(True))
  |> should.equal("equals true")
}

/// Test: to_string for EqualsBool False
pub fn to_string_equals_bool_false_test() {
  rule.to_string(EqualsBool(False))
  |> should.equal("equals false")
}

/// Test: to_string for EqualsVariable
pub fn to_string_equals_variable_test() {
  rule.to_string(EqualsVariable("user_id"))
  |> should.equal("equals ${user_id}")
}

/// Test: to_string for IsString
pub fn to_string_is_string_test() {
  rule.to_string(IsString)
  |> should.equal("string")
}

/// Test: to_string for IntegerGte
pub fn to_string_integer_gte_test() {
  rule.to_string(IntegerGte(5))
  |> should.equal("integer >= 5")
}

/// Test: to_string for IntegerBetween
pub fn to_string_integer_between_test() {
  rule.to_string(IntegerBetween(6, 9))
  |> should.equal("integer > 5 and < 10")
}

/// Test: to_string for NumberBetween
pub fn to_string_number_between_test() {
  rule.to_string(NumberBetween(1.5, 10.5))
  |> should.equal("number between 1.5 and 10.5")
}

/// Test: to_string for Present
pub fn to_string_present_test() {
  rule.to_string(Present)
  |> should.equal("present")
}

/// Test: to_string for NonEmptyArray
pub fn to_string_non_empty_array_test() {
  rule.to_string(NonEmptyArray)
  |> should.equal("non-empty array")
}

/// Test: to_string for ArrayOfLength
pub fn to_string_array_of_length_test() {
  rule.to_string(ArrayOfLength(5))
  |> should.equal("array of length 5")
}

/// Test: to_string for ArrayWithMinItems
pub fn to_string_array_with_min_items_test() {
  rule.to_string(ArrayWithMinItems(3))
  |> should.equal("array with min 3 items")
}

/// Test: to_string for ArrayWhereEach
pub fn to_string_array_where_each_test() {
  rule.to_string(ArrayWhereEach(IsString))
  |> should.equal("array where each string")
}

/// Test: to_string for OneOf
pub fn to_string_one_of_test() {
  rule.to_string(OneOf(["a", "b", "c"]))
  |> should.equal("one of [\"a\", \"b\", \"c\"]")
}

/// Test: to_string for ContainsVariable
pub fn to_string_contains_variable_test() {
  rule.to_string(ContainsVariable("username"))
  |> should.equal("contains ${username}")
}

/// Test: to_string for Raw
pub fn to_string_raw_test() {
  rule.to_string(Raw("custom rule"))
  |> should.equal("custom rule")
}

// ============================================================================
// U. Comprehensive Integration Tests
// ============================================================================

/// Test: Parse all basic type rules in sequence
pub fn parse_all_types_test() {
  let types = [
    #("string", IsString),
    #("integer", IsInteger),
    #("number", IsNumber),
    #("boolean", IsBoolean),
    #("array", IsArray),
    #("object", IsObject),
    #("null", IsNull),
  ]

  types
  |> should.not_equal([])
  // Verify list is not empty

  // Verify each type parses correctly
  let _ =
    types
    |> list.map(fn(pair) {
      let #(input, expected) = pair
      rule.parse(input)
      |> should.equal(expected)
    })

  Nil
}

/// Test: Parse all presence rules in sequence
pub fn parse_all_presence_rules_test() {
  let rules = [
    #("present", Present),
    #("absent", Absent),
    #("not null", NotNull),
  ]

  rules
  |> should.not_equal([])

  let _ =
    rules
    |> list.map(fn(pair) {
      let #(input, expected) = pair
      rule.parse(input)
      |> should.equal(expected)
    })

  Nil
}

/// Test: Parse all string format rules in sequence
pub fn parse_all_string_formats_test() {
  let formats = [
    #("email", IsEmail),
    #("uuid", IsUuid),
    #("uri", IsUri),
    #("jwt", IsJwt),
    #("iso8601 datetime", IsIso8601),
  ]

  formats
  |> should.not_equal([])

  let _ =
    formats
    |> list.map(fn(pair) {
      let #(input, expected) = pair
      rule.parse(input)
      |> should.equal(expected)
    })

  Nil
}
