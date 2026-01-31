//// Exit Codes Tests (ATDD + BDD)
//// Tests for bead: intent-cli-exit-codes

import gleeunit/should
import "$TEST_DIR/test_helpers.gleam" as test_helpers

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// ATDD Tests
// ============================================================================

pub fn invalid_command_returns_exit_4_test() {
  // Given: An invalid command
  let result = test_helpers.execute_intent("invalid-command-that-does-not-exist", [])
  
  // When: Command executes
  // Then: Exit code should be 4 (usage error)
  result.exit_code
  |> should.equal(4)
  
  result.success
  |> should.be_false()
}

pub fn command_with_missing_args_returns_exit_4_test() {
  let result = test_helpers.execute_intent("beads", [])
  
  result.exit_code
  |> should.equal(4)
}

pub fn command_with_invalid_flag_returns_exit_4_test() {
  let result = test_helpers.execute_intent("validate", ["--invalid-flag"])
  
  result.exit_code
  |> should.equal(4)
}

pub fn successful_command_returns_exit_0_test() {
  let result = test_helpers.execute_intent("--help", [])
  
  result.exit_code
  |> should.equal(0)
}

// ============================================================================
// BDD User Journey Tests
// ============================================================================

pub fn ai_agent_can_detect_failures_via_exit_codes_test() {
  // Given: I run an invalid command
  let result = test_helpers.execute_intent("invalid-command", [])
  
  // When: I check the exit code
  let exit_code_check = result.exit_code == 4
  
  // Then: I can reliably detect the failure
  exit_code_check
  |> should.be_true()
}

// ============================================================================
// Error Case Matrix
// ============================================================================

pub fn exit_code_error_matrix_test() {
  let error_cases = [
    [#(command: "invalid-command", expected: 4, description: "Command not found"),
     #(command: "beads", expected: 4, description: "Missing required args"),
     #(command: "lint spec.cue --invalid-flag", expected: 4, description: "Invalid flag provided"),
     #(command: "validate non-existent.cue", expected: 3, description: "File not found"),
     #(command: "interview --profile=invalid", expected: 4, description: "Invalid profile value"),
  ]
  
  error_cases
  |> list.each(fn(tuple) {
       let #(command, expected, description) = tuple
       
       let result = test_helpers.execute_intent(command, [])
       let validation = test_helpers.validate_exit_code(result, expected)
       
       validation.valid
       |> should.be_true()
     })
}
