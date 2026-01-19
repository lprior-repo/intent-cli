/// Contract Builder for Round 2 (Contracts)
/// Helps build response.checks with rule+why for API contracts
///
/// Round 2 output: response.checks with rule+why
/// This module provides a fluent API for building validation contracts
import gleam/dict.{type Dict}
import gleam/json
import gleam/list
import gleam/string
import intent/types.{type Check, Check}

/// A contract check builder - accumulates checks for a response
pub type ContractBuilder {
  ContractBuilder(checks: Dict(String, Check))
}

/// Create a new empty contract builder
pub fn new() -> ContractBuilder {
  ContractBuilder(checks: dict.new())
}

/// Add a check for a field
pub fn check(
  builder: ContractBuilder,
  field: String,
  rule: String,
  why: String,
) -> ContractBuilder {
  let new_checks = dict.insert(builder.checks, field, Check(rule, why))
  ContractBuilder(checks: new_checks)
}

/// Build the final checks dictionary
pub fn build(builder: ContractBuilder) -> Dict(String, Check) {
  builder.checks
}

// ============================================================================
// PRESENCE CHECKS
// ============================================================================

/// Field must be present (not null/missing)
pub fn present(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "present", reason)
}

/// Field must be absent (not in response)
pub fn absent(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "absent", reason)
}

/// Field must not be null
pub fn not_null(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "not null", reason)
}

// ============================================================================
// TYPE CHECKS
// ============================================================================

/// Field must be a string
pub fn string(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "string", reason)
}

/// Field must be a non-empty string
pub fn non_empty_string(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "non-empty string", reason)
}

/// Field must be an integer
pub fn integer(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "integer", reason)
}

/// Field must be a number (integer or float)
pub fn number(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "number", reason)
}

/// Field must be a boolean
pub fn boolean(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "boolean", reason)
}

/// Field must be an array
pub fn array(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "array", reason)
}

/// Field must be a non-empty array
pub fn non_empty_array(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "non-empty array", reason)
}

/// Field must be an object
pub fn object(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "object", reason)
}

// ============================================================================
// STRING PATTERN CHECKS
// ============================================================================

/// Field must match a regex pattern
pub fn matches(
  builder: ContractBuilder,
  field: String,
  pattern: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "string matching " <> pattern, reason)
}

/// Field must start with a prefix
pub fn starts_with(
  builder: ContractBuilder,
  field: String,
  prefix: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "string starting with " <> prefix, reason)
}

/// Field must end with a suffix
pub fn ends_with(
  builder: ContractBuilder,
  field: String,
  suffix: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "string ending with " <> suffix, reason)
}

/// Field must contain a substring
pub fn contains(
  builder: ContractBuilder,
  field: String,
  substring: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "string containing " <> substring, reason)
}

/// Field must be a valid email
pub fn email(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "email", reason)
}

/// Field must be a valid UUID
pub fn uuid(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "uuid", reason)
}

/// Field must be a valid URI
pub fn uri(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "uri", reason)
}

/// Field must be a valid JWT
pub fn jwt(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "jwt", reason)
}

/// Field must be a valid ISO8601 datetime
pub fn iso8601(
  builder: ContractBuilder,
  field: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "iso8601 datetime", reason)
}

// ============================================================================
// EQUALITY CHECKS
// ============================================================================

/// Field must equal a specific value
pub fn equals(
  builder: ContractBuilder,
  field: String,
  value: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "equals " <> value, reason)
}

/// Field must equal an integer
pub fn equals_int(
  builder: ContractBuilder,
  field: String,
  value: Int,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "equals " <> int.to_string(value), reason)
}

/// Field must equal a boolean
pub fn equals_bool(
  builder: ContractBuilder,
  field: String,
  value: Bool,
  reason: String,
) -> ContractBuilder {
  let bool_str = case value {
    True -> "true"
    False -> "false"
  }
  check(builder, field, "equals " <> bool_str, reason)
}

/// Field must be one of allowed values
pub fn one_of(
  builder: ContractBuilder,
  field: String,
  values: List(String),
  reason: String,
) -> ContractBuilder {
  let values_str =
    list.map(values, fn(v) { "\"" <> v <> "\"" }) |> string.join(", ")
  check(builder, field, "one of [" <> values_str <> "]", reason)
}

// ============================================================================
// NUMBER RANGE CHECKS
// ============================================================================

/// Field must be >= minimum value
pub fn min_int(
  builder: ContractBuilder,
  field: String,
  min: Int,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "integer >= " <> int.to_string(min), reason)
}

/// Field must be > minimum value (exclusive)
pub fn gt_int(
  builder: ContractBuilder,
  field: String,
  min: Int,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "integer > " <> int.to_string(min), reason)
}

/// Field must be <= maximum value
pub fn max_int(
  builder: ContractBuilder,
  field: String,
  max: Int,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "integer <= " <> int.to_string(max), reason)
}

/// Field must be < maximum value (exclusive)
pub fn lt_int(
  builder: ContractBuilder,
  field: String,
  max: Int,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "integer < " <> int.to_string(max), reason)
}

/// Field must be between min and max (inclusive)
pub fn between_int(
  builder: ContractBuilder,
  field: String,
  min: Int,
  max: Int,
  reason: String,
) -> ContractBuilder {
  check(
    builder,
    field,
    "integer > "
      <> int.to_string(min - 1)
      <> " and < "
      <> int.to_string(max + 1),
    reason,
  )
}

// ============================================================================
// ARRAY CHECKS
// ============================================================================

/// Array must have exactly N items
pub fn array_length(
  builder: ContractBuilder,
  field: String,
  length: Int,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "array of length " <> int.to_string(length), reason)
}

/// Array must have at least N items
pub fn array_min_items(
  builder: ContractBuilder,
  field: String,
  min: Int,
  reason: String,
) -> ContractBuilder {
  check(
    builder,
    field,
    "array with min " <> int.to_string(min) <> " items",
    reason,
  )
}

/// Array must have at most N items
pub fn array_max_items(
  builder: ContractBuilder,
  field: String,
  max: Int,
  reason: String,
) -> ContractBuilder {
  check(
    builder,
    field,
    "array with max " <> int.to_string(max) <> " items",
    reason,
  )
}

/// Each item in array must match a rule
pub fn array_where_each(
  builder: ContractBuilder,
  field: String,
  item_rule: String,
  reason: String,
) -> ContractBuilder {
  check(builder, field, "array where each " <> item_rule, reason)
}

// ============================================================================
// SECURITY CHECKS
// ============================================================================

/// Field must be absent for security reasons
pub fn security_absent(
  builder: ContractBuilder,
  field: String,
  threat: String,
) -> ContractBuilder {
  check(builder, field, "absent", "SECURITY: " <> threat)
}

// ============================================================================
// FORMAT AS CUE
// ============================================================================

/// Format checks as CUE syntax for inclusion in spec
pub fn to_cue(builder: ContractBuilder) -> String {
  let check_items =
    dict.to_list(builder.checks)
    |> list.map(fn(pair) {
      let #(field, check) = pair
      "  \""
      <> field
      <> "\": {\n    rule: \""
      <> escape_rule(check.rule)
      <> "\"\n    why:  \""
      <> escape_rule(check.why)
      <> "\"\n  }"
    })
    |> string.join(",\n")

  case check_items {
    "" -> "checks: {}"
    _ -> "checks: {\n" <> check_items <> "\n}"
  }
}

fn escape_rule(s: String) -> String {
  s
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}

// ============================================================================
// BUILDER COMPOSITION
// ============================================================================

/// Merge two contract builders
pub fn merge(a: ContractBuilder, b: ContractBuilder) -> ContractBuilder {
  ContractBuilder(checks: dict.merge(a.checks, b.checks))
}

/// Create contract builder from a single check
pub fn from_check(field: String, rule: String, why: String) -> ContractBuilder {
  new() |> check(field, rule, why)
}

/// Create contract builder from list of checks
pub fn from_list(checks: List(#(String, String, String))) -> ContractBuilder {
  list.fold(checks, new(), fn(builder, check) {
    let #(field, rule, why) = check
    check(builder, field, rule, why)
  })
}

// ============================================================================
// RULE VALIDATION
// ============================================================================

/// Validate that a rule string is well-formed
pub fn validate_rule(rule: String) -> Result(String, String) {
  let trimmed = string.trim(rule)
  case string.length(trimmed) > 0 {
    True -> Ok(trimmed)
    False -> Error("Rule cannot be empty")
  }
}

/// Check if a rule is a known valid pattern
pub fn is_valid_rule_pattern(rule: String) -> Bool {
  let patterns = [
    "present", "absent", "not null", "string", "non-empty string", "integer",
    "number", "boolean", "array", "object", "email", "uuid", "uri", "jwt",
    "iso8601 datetime", "equals ", "one of [", "string matching ",
    "string starting with ", "string ending with ", "string containing ",
    "integer > ", "integer < ", "integer >= ", "integer <= ", "array of length ",
    "array with min ", "array with max ", "array where each ",
  ]
  list.any(patterns, fn(p) { string.starts_with(trimmed_rule(rule), p) })
}

fn trimmed_rule(rule: String) -> String {
  string.trim(rule)
}
