import gleam/string
import gleeunit/should
import intent/resolver

// Test AI-friendly error formatting for resolver errors

pub fn format_error_ai_cyclic_dependency_test() {
  let error = resolver.CyclicDependency(["login", "auth", "session"])
  let formatted = resolver.format_error_ai(error)

  // Check CUE structure
  formatted
  |> should_contain("action: \"dependency_error\"")

  formatted
  |> should_contain("type: \"circular_dependency\"")

  formatted
  |> should_contain("login -> auth -> session")

  formatted
  |> should_contain("suggestion:")

  formatted
  |> should_contain("recovery:")
}

pub fn format_error_ai_missing_dependency_test() {
  let error = resolver.MissingDependency("create_post", "authenticate_user")
  let formatted = resolver.format_error_ai(error)

  // Check CUE structure
  formatted
  |> should_contain("action: \"dependency_error\"")

  formatted
  |> should_contain("type: \"missing_dependency\"")

  formatted
  |> should_contain("create_post")

  formatted
  |> should_contain("authenticate_user")

  formatted
  |> should_contain("suggestion:")
}

pub fn format_error_ai_duplicate_name_test() {
  let error = resolver.DuplicateBehaviorName("list_users")
  let formatted = resolver.format_error_ai(error)

  // Check CUE structure
  formatted
  |> should_contain("action: \"validation_error\"")

  formatted
  |> should_contain("type: \"duplicate_behavior_name\"")

  formatted
  |> should_contain("list_users")

  formatted
  |> should_contain("Rename")
}

pub fn format_error_text_cyclic_dependency_test() {
  let error = resolver.CyclicDependency(["A", "B", "C"])
  let formatted = resolver.format_error_text(error)

  // Check human-readable format
  formatted
  |> should_contain("Error:")

  formatted
  |> should_contain("A -> B -> C")

  formatted
  |> should_contain("Suggestion:")

  formatted
  |> should_contain("Recovery Steps:")

  formatted
  |> should_contain("Directed Acyclic Graph")
}

pub fn format_error_text_missing_dependency_test() {
  let error = resolver.MissingDependency("delete_user", "verify_admin")
  let formatted = resolver.format_error_text(error)

  // Check human-readable format
  formatted
  |> should_contain("Error:")

  formatted
  |> should_contain("delete_user")

  formatted
  |> should_contain("verify_admin")

  formatted
  |> should_contain("Suggestion:")

  formatted
  |> should_contain("Recovery Steps:")
}

pub fn format_error_text_duplicate_name_test() {
  let error = resolver.DuplicateBehaviorName("update_profile")
  let formatted = resolver.format_error_text(error)

  // Check human-readable format
  formatted
  |> should_contain("Error:")

  formatted
  |> should_contain("update_profile")

  formatted
  |> should_contain("unique")

  formatted
  |> should_contain("Suggestion:")

  formatted
  |> should_contain("Recovery Steps:")
}

pub fn escape_json_string_in_ai_format_test() {
  // Test that special characters are properly escaped
  let error = resolver.MissingDependency("test\"quote", "missing\nline")
  let formatted = resolver.format_error_ai(error)

  // Should escape quotes and newlines
  formatted
  |> should_contain("test\\\"quote")

  formatted
  |> should_contain("missing\\nline")
}

// Helper function for contains assertion
fn should_contain(haystack: String, needle: String) -> Nil {
  case string.contains(haystack, needle) {
    True -> Nil
    False ->
      should.fail(
        "Expected string to contain: '"
        <> needle
        <> "'\n\nBut got:\n"
        <> haystack,
      )
  }
}
