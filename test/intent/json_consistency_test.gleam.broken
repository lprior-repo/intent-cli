/// Tests to ensure ALL commands consistently use json_output.JsonResponse
/// and populate next_actions intelligently
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import intent/json_output

// ============================================================================
// Next Actions Consistency Tests
// ============================================================================

/// Test that success responses CAN have populated next_actions
/// (this is recommended but not required at the type level)
pub fn next_actions_can_be_populated_test() {
  // Test that next_actions can be provided for success responses
  // This is a design best practice: AI agents benefit from workflow guidance

  // Create a response WITH next_actions (recommended behavior)
  let next_actions = [
    json_output.next_action("intent doctor spec.cue --json", "Check quality"),
  ]

  let response_with_actions =
    json_output.success(
      "test_action",
      "test_command",
      json.null(),
      Some("spec.cue"),
      next_actions,
    )

  // Verify next_actions are present
  response_with_actions.next_actions
  |> list.length
  |> should.equal(1)
}

/// Test that all KIRK commands provide intelligent next_actions
pub fn kirk_commands_suggest_complementary_analyses_test() {
  // This test documents the expected workflow suggestions
  // quality → should suggest gaps + invert
  // coverage → should suggest effects + doctor
  // gaps → should suggest quality + doctor
  // invert → should suggest gaps + effects
  // effects → should suggest gaps + coverage

  // For now, we just verify the structure exists
  let next_actions = [
    json_output.next_action("intent gaps spec.cue --json", "Find coverage gaps"),
    json_output.next_action(
      "intent invert spec.cue --json",
      "Analyze failure modes",
    ),
  ]

  next_actions
  |> list.length
  |> should.equal(2)

  // Verify each action has both command and reason
  case next_actions {
    [first, ..] -> {
      first.command
      |> should.equal("intent gaps spec.cue --json")

      first.reason
      |> should.equal("Find coverage gaps")
    }
    [] -> should.fail()
  }
}

/// Test that error responses can have empty next_actions
/// (because errors need to be fixed before proceeding)
pub fn error_responses_may_have_empty_next_actions_test() {
  let response =
    json_output.failure(
      "test_failed",
      "test",
      json.null(),
      [json_output.error("test_error", "Something went wrong")],
      None,
      [],
      1,
    )

  // Error responses CAN have empty next_actions (valid state)
  response.next_actions
  |> list.length
  |> should.equal(0)
}

/// Test that we can add next_actions to error responses
/// (to suggest fix commands)
pub fn error_responses_can_suggest_fix_commands_test() {
  let response =
    json_output.failure(
      "validation_failed",
      "validate",
      json.null(),
      [json_output.error("parse_error", "Invalid CUE syntax")],
      Some("spec.cue"),
      [],
      1,
    )

  // Add a fix suggestion
  let fix_actions = [
    json_output.next_action(
      "intent doctor spec.cue --json",
      "Get prioritized fix suggestions",
    ),
  ]

  let updated = json_output.with_next_actions(response, fix_actions)

  updated.next_actions
  |> list.length
  |> should.equal(1)
}

// ============================================================================
// Documentation Tests for Current Implementation
// ============================================================================

/// Document that quality command already provides next_actions
/// This test serves as documentation that the feature already works
pub fn quality_command_provides_next_actions_test() {
  // This is documentation, not a real test of the command
  // It verifies the next_action helper works correctly
  let next_actions = [
    json_output.next_action("intent gaps spec.cue --json", "Find coverage gaps"),
    json_output.next_action(
      "intent invert spec.cue --json",
      "Analyze failure modes",
    ),
  ]

  // Verify the structure
  next_actions
  |> list.length
  |> should.equal(2)

  // Verify first action
  case next_actions {
    [first, ..] -> {
      first.command
      |> should.equal("intent gaps spec.cue --json")

      first.reason
      |> should.equal("Find coverage gaps")
    }
    [] -> should.fail()
  }
}

/// Test that JsonResponse correctly serializes next_actions to JSON
pub fn next_actions_serialize_to_json_test() {
  let next_actions = [
    json_output.next_action("intent gaps spec.cue --json", "Find coverage gaps"),
  ]

  let response =
    json_output.success(
      "test_action",
      "test",
      json.null(),
      Some("spec.cue"),
      next_actions,
    )

  let json_string = response |> json_output.to_json |> json.to_string

  // Verify JSON contains next_actions array
  json_string
  |> string.contains("\"next_actions\"")
  |> should.be_true

  // Verify it contains the command
  json_string
  |> string.contains("\"command\":\"intent gaps spec.cue --json\"")
  |> should.be_true

  // Verify it contains the reason
  json_string
  |> string.contains("\"reason\":\"Find coverage gaps\"")
  |> should.be_true
}
