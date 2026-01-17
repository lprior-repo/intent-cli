//// Tests for plan_mode error formatting

import gleam/string
import gleeunit/should
import intent/plan_mode

// =============================================================================
// AI Format Tests
// =============================================================================

pub fn format_error_ai_session_not_found_test() {
  let error = plan_mode.SessionNotFound("abc123")
  let formatted = plan_mode.format_error_ai(error)

  // Should contain CUE structure
  formatted |> should_contain("action: \"file_error\"")
  formatted |> should_contain("type: \"session_not_found\"")
  formatted |> should_contain("session_id: \"abc123\"")
  formatted |> should_contain("expected_path: \".intent/session-abc123.cue\"")
  formatted |> should_contain("operation: \"execution_plan\"")

  // Should contain recovery steps
  formatted |> should_contain("suggestion:")
  formatted |> should_contain("recovery:")
  formatted |> should_contain("List available sessions")
  formatted |> should_contain("intent interview --profile")
}

pub fn format_error_ai_parse_error_test() {
  let error = plan_mode.ParseError("invalid CUE syntax at line 42")
  let formatted = plan_mode.format_error_ai(error)

  // Should contain CUE structure
  formatted |> should_contain("action: \"parse_error\"")
  formatted |> should_contain("type: \"session_parse_error\"")
  formatted |> should_contain("parse_error: \"invalid CUE syntax at line 42\"")
  formatted |> should_contain("operation: \"bead_extraction\"")

  // Should contain recovery steps
  formatted |> should_contain("cue vet")
  formatted |> should_contain("cue export")
  formatted |> should_contain("beads' array field")
}

pub fn format_error_ai_cyclic_dependency_test() {
  let error =
    plan_mode.CyclicDependency(["bead-1", "bead-2", "bead-3", "bead-1"])
  let formatted = plan_mode.format_error_ai(error)

  // Should contain CUE structure
  formatted |> should_contain("action: \"dependency_error\"")
  formatted |> should_contain("type: \"circular_dependency\"")
  formatted |> should_contain("cycle_path:")
  formatted |> should_contain("beads_involved:")
  formatted |> should_contain("cycle_length: 4")

  // Should contain recovery steps
  formatted |> should_contain("requires' field")
  formatted |> should_contain("Directed Acyclic Graph")
}

pub fn format_error_ai_missing_dependency_test() {
  let error = plan_mode.MissingDependency("bead-5", "bead-99")
  let formatted = plan_mode.format_error_ai(error)

  // Should contain CUE structure
  formatted |> should_contain("action: \"dependency_error\"")
  formatted |> should_contain("type: \"missing_dependency\"")
  formatted |> should_contain("bead_id: \"bead-5\"")
  formatted |> should_contain("missing_dependency: \"bead-99\"")
  formatted |> should_contain("dependency_type: \"bead\"")

  // Should contain recovery steps
  formatted |> should_contain("Check if 'bead-99' is defined")
  formatted |> should_contain("Verify the bead ID is spelled correctly")
}

// =============================================================================
// Text Format Tests
// =============================================================================

pub fn format_error_text_session_not_found_test() {
  let error = plan_mode.SessionNotFound("xyz789")
  let formatted = plan_mode.format_error_text(error)

  // Should be human-readable
  formatted |> should_contain("Error: Session file not found")
  formatted |> should_contain("session_id: xyz789")
  formatted |> should_contain("expected_path: .intent/session-xyz789.cue")
  formatted |> should_contain("operation: execution_plan")

  // Should have structured sections
  formatted |> should_contain("Context:")
  formatted |> should_contain("Suggestion:")
  formatted |> should_contain("Recovery Steps:")

  // Should have numbered steps
  formatted |> should_contain("1. Check if the session ID is correct")
  formatted |> should_contain("2. List available sessions")
}

pub fn format_error_text_parse_error_test() {
  let error = plan_mode.ParseError("unexpected token at position 10")
  let formatted = plan_mode.format_error_text(error)

  // Should be human-readable
  formatted |> should_contain("Error: Failed to parse session CUE file")
  formatted |> should_contain("parse_error: unexpected token at position 10")
  formatted |> should_contain("operation: bead_extraction")

  // Should have numbered steps
  formatted |> should_contain("1. Check CUE syntax")
  formatted |> should_contain("2. Export to JSON")
  formatted |> should_contain("3. Verify session file has 'beads' array field")
}

pub fn format_error_text_cyclic_dependency_test() {
  let error = plan_mode.CyclicDependency(["alpha", "beta", "gamma", "alpha"])
  let formatted = plan_mode.format_error_text(error)

  // Should be human-readable
  formatted |> should_contain("Error: Circular dependency detected")
  formatted |> should_contain("cycle_path: alpha -> beta -> gamma -> alpha")
  formatted |> should_contain("beads_involved: alpha, beta, gamma, alpha")
  formatted |> should_contain("cycle_length: 4")

  // Should have numbered steps
  formatted |> should_contain("1. Review the 'requires' field")
  formatted
  |> should_contain("2. Identify which dependency can be safely removed")
  formatted
  |> should_contain("5. Regenerate session with corrected dependencies")
}

pub fn format_error_text_missing_dependency_test() {
  let error = plan_mode.MissingDependency("task-main", "task-setup")
  let formatted = plan_mode.format_error_text(error)

  // Should be human-readable
  formatted
  |> should_contain("Error: Bead requires a dependency that does not exist")
  formatted |> should_contain("bead_id: task-main")
  formatted |> should_contain("missing_dependency: task-setup")
  formatted |> should_contain("dependency_type: bead")

  // Should have numbered steps
  formatted |> should_contain("1. Check if 'task-setup' is defined")
  formatted |> should_contain("2. Verify the bead ID is spelled correctly")
}

// =============================================================================
// JSON Escaping Tests
// =============================================================================

pub fn format_error_ai_escapes_special_characters_test() {
  let error = plan_mode.SessionNotFound("session-with-\"quotes\"")
  let formatted = plan_mode.format_error_ai(error)

  // Should escape quotes in JSON
  formatted |> should_contain("session-with-\\\"quotes\\\"")
}

pub fn format_error_ai_escapes_newlines_test() {
  let error = plan_mode.ParseError("Error on line 1\nand line 2")
  let formatted = plan_mode.format_error_ai(error)

  // Should escape newlines in JSON
  formatted |> should_contain("line 1\\nand line 2")
}

pub fn format_error_ai_handles_backslashes_test() {
  let error = plan_mode.MissingDependency("bead\\with\\slashes", "other")
  let formatted = plan_mode.format_error_ai(error)

  // Should escape backslashes in JSON
  formatted |> should_contain("bead\\\\with\\\\slashes")
}

// =============================================================================
// Legacy Format Tests (for backwards compatibility)
// =============================================================================

pub fn format_error_legacy_session_not_found_test() {
  let error = plan_mode.SessionNotFound("test-id")
  let formatted = plan_mode.format_error(error)

  formatted |> should_contain("Session not found: test-id")
  formatted |> should_contain("Expected file: .intent/session-test-id.cue")
}

pub fn format_error_legacy_parse_error_test() {
  let error = plan_mode.ParseError("syntax error")
  let formatted = plan_mode.format_error(error)

  formatted |> should.equal("Failed to parse session: syntax error")
}

pub fn format_error_legacy_cyclic_dependency_test() {
  let error = plan_mode.CyclicDependency(["a", "b", "c"])
  let formatted = plan_mode.format_error(error)

  formatted |> should.equal("Cyclic dependency detected involving: a, b, c")
}

pub fn format_error_legacy_missing_dependency_test() {
  let error = plan_mode.MissingDependency("bead-x", "bead-y")
  let formatted = plan_mode.format_error(error)

  formatted
  |> should.equal("Bead 'bead-x' requires 'bead-y' which does not exist")
}

// =============================================================================
// Helper Functions
// =============================================================================

fn should_contain(text: String, substring: String) -> Nil {
  case string.contains(text, substring) {
    True -> Nil
    False ->
      panic as {
        "Expected text to contain: '"
        <> substring
        <> "'\n\nActual text:\n"
        <> text
      }
  }
}
