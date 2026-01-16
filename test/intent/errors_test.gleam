//// Comprehensive tests for the errors module
//// Tests rich error reporting with context and suggestions:
//// - Contextual errors with field suggestions
//// - Levenshtein distance field name suggestions
//// - Error formatting for display
//// - Validation error formatting
//// - Available field extraction from JSON
//// - Format validation error formatting
//// - Next step suggestions

import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import intent/errors

// ============================================================================
// ContextualError Tests
// ============================================================================

pub fn field_not_found_creates_error_test() {
  let error =
    errors.field_not_found("create_user", "user.email", ["user.name", "user.id"])

  error.behavior
  |> should.equal("create_user")

  error.field_path
  |> should.equal("user.email")

  error.rule
  |> should.equal("present")
}

pub fn field_not_found_with_exact_match_suggestion_test() {
  let error = errors.field_not_found("test", "email", ["email", "name", "id"])

  // Should include "email" in suggestions since it's an exact match (distance 0)
  error.suggestions
  |> should.equal(["email"])
}

pub fn field_not_found_with_close_match_suggestions_test() {
  let error = errors.field_not_found("test", "emai", ["email", "name", "id"])

  // "email" has distance 1 (one insertion), should be suggested
  error.suggestions
  |> should.equal(["email"])
}

pub fn field_not_found_with_multiple_suggestions_test() {
  let error =
    errors.field_not_found("test", "user", [
      "user_id", "username", "users", "name",
    ])

  // Should suggest fields within distance 2, sorted by distance
  // "users" (distance 1), "user_id" (distance 3), "username" (distance 4)
  // Only "users" within distance 2
  error.suggestions
  |> should.equal(["users"])
}

pub fn field_not_found_no_suggestions_when_no_close_matches_test() {
  let error = errors.field_not_found("test", "xyz", ["email", "name", "id"])

  // No fields within distance 2, should have empty suggestions
  error.suggestions
  |> should.equal([])
}

pub fn field_not_found_limits_suggestions_to_three_test() {
  let error =
    errors.field_not_found("test", "test", [
      "test", "test1", "test2", "test3", "test4",
    ])

  // Should only return top 3 suggestions even if more are close
  list.length(error.suggestions)
  |> should.equal(3)
}

// ============================================================================
// Levenshtein Distance Tests (via suggest_field_names)
// ============================================================================

pub fn suggest_empty_string_to_empty_string_test() {
  let error = errors.field_not_found("test", "", [""])
  // Distance 0, should suggest
  error.suggestions
  |> should.equal([""])
}

pub fn suggest_identical_strings_test() {
  let error = errors.field_not_found("test", "hello", ["hello", "world"])
  // Distance 0 for "hello"
  error.suggestions
  |> should.equal(["hello"])
}

pub fn suggest_single_character_difference_test() {
  let error = errors.field_not_found("test", "cat", ["bat", "rat", "dog"])
  // "bat" and "rat" both have distance 1, should both be suggested
  list.contains(error.suggestions, "bat")
  |> should.equal(True)
  list.contains(error.suggestions, "rat")
  |> should.equal(True)
}

pub fn suggest_insertion_test() {
  let error = errors.field_not_found("test", "tst", ["test", "toast"])
  // "test" has distance 1 (insert 'e')
  list.contains(error.suggestions, "test")
  |> should.equal(True)
}

pub fn suggest_deletion_test() {
  let error = errors.field_not_found("test", "test", ["tst", "toast"])
  // "tst" has distance 1 (delete 'e')
  list.contains(error.suggestions, "tst")
  |> should.equal(True)
}

pub fn suggest_substitution_test() {
  let error = errors.field_not_found("test", "best", ["test", "west"])
  // "test" has distance 1 (substitute 'b' with 't')
  list.contains(error.suggestions, "test")
  |> should.equal(True)
}

pub fn suggest_distance_two_changes_test() {
  let error = errors.field_not_found("test", "cat", ["cart", "cast", "dog"])
  // "cart" has distance 2 (insert 'r', substitute 'c' with 't')
  // "cast" has distance 2
  list.contains(error.suggestions, "cart")
  |> should.equal(True)
  list.contains(error.suggestions, "cast")
  |> should.equal(True)
}

pub fn suggest_does_not_suggest_distance_three_test() {
  let error = errors.field_not_found("test", "abc", ["xyz", "def"])
  // All fields have distance > 2, should have no suggestions
  error.suggestions
  |> should.equal([])
}

// ============================================================================
// Error Formatting Tests
// ============================================================================

pub fn format_error_includes_behavior_test() {
  let error = errors.field_not_found("create_user", "user.email", ["user.name"])
  let formatted = errors.format_error(error)

  string.contains(formatted, "create_user")
  |> should.equal(True)
}

pub fn format_error_includes_field_path_test() {
  let error = errors.field_not_found("create_user", "user.email", ["user.name"])
  let formatted = errors.format_error(error)

  string.contains(formatted, "user.email")
  |> should.equal(True)
}

pub fn format_error_includes_rule_test() {
  let error = errors.field_not_found("create_user", "user.email", ["user.name"])
  let formatted = errors.format_error(error)

  string.contains(formatted, "present")
  |> should.equal(True)
}

pub fn format_error_includes_available_fields_test() {
  let error =
    errors.field_not_found("create_user", "user.email", ["user.name", "user.id"])
  let formatted = errors.format_error(error)

  string.contains(formatted, "user.name")
  |> should.equal(True)
  string.contains(formatted, "user.id")
  |> should.equal(True)
}

pub fn format_error_includes_suggestions_test() {
  let error = errors.field_not_found("test", "nam", ["name", "id"])
  let formatted = errors.format_error(error)

  string.contains(formatted, "Did you mean")
  |> should.equal(True)
  string.contains(formatted, "name")
  |> should.equal(True)
}

pub fn format_error_includes_explanation_test() {
  let error = errors.field_not_found("create_user", "user.email", ["user.name"])
  let formatted = errors.format_error(error)

  string.contains(formatted, "not found in response")
  |> should.equal(True)
}

// ============================================================================
// ValidationError Tests
// ============================================================================

pub fn format_validation_error_single_failure_test() {
  let failure =
    errors.FieldFailure(
      field: "user.email",
      rule: "format:email",
      expected: "valid email address",
      actual: "invalid-email",
      explanation: "Email format invalid",
    )
  let error =
    errors.ValidationError(behavior: "create_user", failures: [failure])
  let formatted = errors.format_validation_error(error)

  string.contains(formatted, "create_user")
  |> should.equal(True)
  string.contains(formatted, "1 failure")
  |> should.equal(True)
  string.contains(formatted, "user.email")
  |> should.equal(True)
}

pub fn format_validation_error_multiple_failures_test() {
  let failure1 =
    errors.FieldFailure(
      field: "user.email",
      rule: "format:email",
      expected: "valid email",
      actual: "bad-email",
      explanation: "Invalid",
    )
  let failure2 =
    errors.FieldFailure(
      field: "user.age",
      rule: "integer >= 18",
      expected: "18 or greater",
      actual: "15",
      explanation: "Too young",
    )
  let error =
    errors.ValidationError(behavior: "test", failures: [failure1, failure2])
  let formatted = errors.format_validation_error(error)

  string.contains(formatted, "2 failures")
  |> should.equal(True)
  string.contains(formatted, "user.email")
  |> should.equal(True)
  string.contains(formatted, "user.age")
  |> should.equal(True)
}

pub fn format_validation_error_uses_plural_correctly_test() {
  let failure =
    errors.FieldFailure(
      field: "test",
      rule: "present",
      expected: "exists",
      actual: "missing",
      explanation: "Missing",
    )
  let single = errors.ValidationError(behavior: "test", failures: [failure])
  let multiple =
    errors.ValidationError(behavior: "test", failures: [failure, failure])

  let single_formatted = errors.format_validation_error(single)
  let multiple_formatted = errors.format_validation_error(multiple)

  string.contains(single_formatted, "1 failure")
  |> should.equal(True)
  string.contains(multiple_formatted, "2 failures")
  |> should.equal(True)
}

// ============================================================================
// Extract Available Fields Tests
// ============================================================================

pub fn extract_available_fields_from_object_test() {
  let obj =
    json.object([
      #("name", json.string("Alice")),
      #("age", json.int(30)),
      #("email", json.string("alice@example.com")),
    ])

  let fields = errors.extract_available_fields(obj)

  list.contains(fields, "name")
  |> should.equal(True)
  list.contains(fields, "age")
  |> should.equal(True)
  list.contains(fields, "email")
  |> should.equal(True)
}

pub fn extract_available_fields_sorted_test() {
  let obj =
    json.object([
      #("zebra", json.string("z")),
      #("apple", json.string("a")),
      #("banana", json.string("b")),
    ])

  let fields = errors.extract_available_fields(obj)

  // Should be sorted alphabetically
  fields
  |> should.equal(["apple", "banana", "zebra"])
}

pub fn extract_available_fields_from_non_object_test() {
  let array = json.array([json.int(1), json.int(2)], fn(x) { x })

  let fields = errors.extract_available_fields(array)

  // Should return empty list for non-objects
  fields
  |> should.equal([])
}

pub fn extract_available_fields_empty_object_test() {
  let obj = json.object([])

  let fields = errors.extract_available_fields(obj)

  fields
  |> should.equal([])
}

// ============================================================================
// Format Error Tests
// ============================================================================

pub fn format_format_error_includes_field_test() {
  let formatted =
    errors.format_format_error(
      "user.email",
      "email address",
      "not-an-email",
      "Missing @ symbol",
    )

  string.contains(formatted, "user.email")
  |> should.equal(True)
}

pub fn format_format_error_includes_format_name_test() {
  let formatted =
    errors.format_format_error(
      "user.email",
      "email address",
      "not-an-email",
      "Missing @ symbol",
    )

  string.contains(formatted, "email address")
  |> should.equal(True)
}

pub fn format_format_error_includes_value_test() {
  let formatted =
    errors.format_format_error(
      "user.email",
      "email address",
      "not-an-email",
      "Missing @ symbol",
    )

  string.contains(formatted, "not-an-email")
  |> should.equal(True)
}

pub fn format_format_error_includes_reason_test() {
  let formatted =
    errors.format_format_error(
      "user.email",
      "email address",
      "not-an-email",
      "Missing @ symbol",
    )

  string.contains(formatted, "Missing @ symbol")
  |> should.equal(True)
}

// ============================================================================
// Suggest Next Steps Tests
// ============================================================================

pub fn suggest_next_steps_format_error_test() {
  let steps = errors.suggest_next_steps("format")

  list.length(steps)
  |> should.equal(2)
  list.contains(
    steps,
    "Check that format validators parse correctly, not just regex match",
  )
  |> should.equal(True)
}

pub fn suggest_next_steps_missing_field_error_test() {
  let steps = errors.suggest_next_steps("missing_field")

  list.length(steps)
  |> should.equal(3)
  list.contains(steps, "Verify the response structure matches the spec example")
  |> should.equal(True)
}

pub fn suggest_next_steps_interpolation_error_test() {
  let steps = errors.suggest_next_steps("interpolation")

  list.length(steps)
  |> should.equal(3)
  list.contains(steps, "Ensure the variable is captured in a previous behavior")
  |> should.equal(True)
}

pub fn suggest_next_steps_circular_dependency_error_test() {
  let steps = errors.suggest_next_steps("circular_dependency")

  list.length(steps)
  |> should.equal(2)
  list.contains(steps, "Review behavior 'requires' declarations for cycles")
  |> should.equal(True)
}

pub fn suggest_next_steps_unknown_error_type_test() {
  let steps = errors.suggest_next_steps("unknown_error")

  list.length(steps)
  |> should.equal(2)
  list.contains(steps, "Check the specification for ambiguities")
  |> should.equal(True)
}

pub fn suggest_next_steps_empty_error_type_test() {
  let steps = errors.suggest_next_steps("")

  // Should return default suggestions
  list.length(steps)
  |> should.equal(2)
}
