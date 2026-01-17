import gleam/string
import gleeunit/should
import intent/resolver

pub fn format_error_cyclic_test() {
  let error = resolver.CyclicDependency(["login", "auth", "session", "login"])
  let formatted = resolver.format_error(error)

  formatted
  |> should.equal(
    "Cyclic dependency detected involving: login, auth, session, login",
  )
}

pub fn format_error_missing_dependency_test() {
  let error = resolver.MissingDependency("login", "authenticate")
  let formatted = resolver.format_error(error)

  formatted
  |> should.equal(
    "Behavior 'login' requires 'authenticate' which does not exist",
  )
}

pub fn format_error_duplicate_name_test() {
  let error = resolver.DuplicateBehaviorName("create_user")
  let formatted = resolver.format_error(error)

  formatted
  |> should.equal("Duplicate behavior name: create_user")
}

pub fn format_error_ai_cyclic_test() {
  let error = resolver.CyclicDependency(["login", "auth", "login"])
  let formatted = resolver.format_error_ai(error)

  // Should contain CUE structure
  formatted
  |> should.contain("action: \"dependency_error\"")

  formatted
  |> should.contain("type: \"cyclic_dependency\"")

  formatted
  |> should.contain("login -> auth -> login")
}

pub fn format_error_ai_missing_dependency_test() {
  let error = resolver.MissingDependency("login", "authenticate")
  let formatted = resolver.format_error_ai(error)

  // Should contain CUE structure
  formatted
  |> should.contain("action: \"dependency_error\"")

  formatted
  |> should.contain("type: \"missing_dependency\"")

  formatted
  |> should.contain("authenticate")
}

pub fn format_error_text_cyclic_test() {
  let error = resolver.CyclicDependency(["A", "B", "C", "A"])
  let formatted = resolver.format_error_text(error)

  // Should contain human-readable text
  formatted
  |> should.contain("Error:")

  formatted
  |> should.contain("Suggestion:")

  formatted
  |> should.contain("Recovery Steps:")

  formatted
  |> should.contain("A -> B -> C -> A")
}

pub fn format_error_text_duplicate_test() {
  let error = resolver.DuplicateBehaviorName("create_user")
  let formatted = resolver.format_error_text(error)

  // Should contain actionable recovery steps
  formatted
  |> should.contain("Error:")

  formatted
  |> should.contain("Rename")

  formatted
  |> should.contain("create_user")
}

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
