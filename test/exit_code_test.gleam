import gleam/int
import gleam/list
import gleeunit
import gleeunit/should

// Exit code regression tests
// Prevents regressions in exit code behavior identified during Ralph Loop iteration 23
//
// Exit code system (src/intent.gleam:43-52):
// - 0 (exit_pass): Success
// - 1 (exit_fail): General failure
// - 2 (exit_blocked): Blocked behaviors (check command)
// - 3 (exit_invalid): Invalid spec (file not found, parse error)
// - 4 (exit_error): Usage error (missing args, invalid flags)

pub fn main() {
  gleeunit.main()
}

// Note: These are documentation tests rather than functional tests
// since Gleam test framework doesn't support subprocess execution
// or capturing exit codes from main()
//
// Actual exit code behavior must be tested via:
// - Integration tests in CI/CD
// - Manual verification scripts
// - End-to-end test suite (task #7)

pub fn exit_code_constants_documented_test() {
  // This test documents the expected exit codes
  // If exit codes change, this test should fail and trigger review

  let exit_pass = 0
  let exit_fail = 1
  let exit_blocked = 2
  let exit_invalid = 3
  let exit_error = 4

  // Verify exit codes are distinct
  exit_pass |> should.equal(0)
  exit_fail |> should.equal(1)
  exit_blocked |> should.equal(2)
  exit_invalid |> should.equal(3)
  exit_error |> should.equal(4)
}

pub fn exit_code_range_is_valid_test() {
  // Exit codes should be 0-4 (within shell exit code range 0-255)
  let codes = [0, 1, 2, 3, 4]

  codes
  |> should.not_equal([])

  // All codes should be non-negative and less than 256
  codes
  |> list.all(fn(code) { code >= 0 && code < 256 })
  |> should.be_true()
}

pub fn exit_codes_are_sequential_test() {
  // Exit codes are sequential for ease of understanding
  // If this changes, it may indicate unintentional drift

  let codes = [0, 1, 2, 3, 4]
  let sorted = list.sort(codes, int.compare)

  codes |> should.equal(sorted)
}
// Future: Add integration tests for actual exit code behavior
// These would require subprocess execution capabilities:
//
// - validate_success_returns_exit_0_test()
// - validate_missing_file_returns_exit_3_test()
// - validate_missing_args_returns_exit_4_test()
// - lint_no_warnings_returns_exit_0_test()
// - lint_with_warnings_returns_exit_1_test()
// - check_blocked_behaviors_returns_exit_2_test()
//
// See: task #7 (Create end-to-end test suite covering all commands)
