
package validation

import "list"

// Validation schema for bead: intent-cli-20260207232111-ifxonjqo
// Title: ffi: Fix type issues in intent_ffi.erl
//
// This schema validates that implementation is complete.
// Use: cue vet intent-cli-20260207232111-ifxonjqo.cue implementation.cue

#BeadImplementation: {
  bead_id: "intent-cli-20260207232111-ifxonjqo"
  title: "ffi: Fix type issues in intent_ffi.erl"

  // Contract verification
  contracts_verified: {
    preconditions_checked: bool & true
    postconditions_verified: bool & true
    invariants_maintained: bool & true

    // Specific preconditions that must be verified
    precondition_checks: [
      "FFI module exists with type annotations",
    ]

    // Specific postconditions that must be verified
    postcondition_checks: [
      "All functions return declared types",
      "No dead code branches exist",
      "Exception handlers are specific",
    ]

    // Specific invariants that must be maintained
    invariant_checks: [
      "Function return types match Gleam declarations",
      "All error cases are explicitly handled",
    ]
  }

  // Test verification
  tests_passing: {
    all_tests_pass: bool & true

    happy_path_tests: [...string] & list.MinItems(3)
    error_path_tests: [...string] & list.MinItems(2)

    // Note: Actual test names provided by implementer, must include all required tests

    // Required happy path tests
    required_happy_tests: [
      "current_timestamp returns String",
      "to_hex returns flat String not iolist",
      "base64_url_decode returns Error or Ok",
    ]

    // Required error path tests
    required_error_tests: [
      "Invalid base64 returns specific Error not generic catch-all",
      "Type errors are caught at compile time",
    ]
  }

  // Code completion
  code_complete: {
    implementation_exists: string  // Path to implementation file
    tests_exist: string  // Path to test file
    ci_passing: bool & true
    no_unwrap_calls: bool & true  // Rust/functional constraint
    no_panics: bool & true  // Rust constraint
  }

  // Completion criteria
  completion: {
    all_sections_complete: bool & true
    documentation_updated: bool
    beads_closed: bool
    timestamp: string  // ISO8601 completion timestamp
  }
}

// Example implementation proof - create this file to validate completion:
//
// implementation.cue:
// package validation
//
// implementation: #BeadImplementation & {
//   contracts_verified: {
//     preconditions_checked: true
//     postconditions_verified: true
//     invariants_maintained: true
//     precondition_checks: [/* documented checks */]
//     postcondition_checks: [/* documented verifications */]
//     invariant_checks: [/* documented invariants */]
//   }
//   tests_passing: {
//     all_tests_pass: true
//     happy_path_tests: ["test_version_flag_works", "test_version_format", "test_exit_code_zero"]
//     error_path_tests: ["test_invalid_flag_errors", "test_no_flags_normal_behavior"]
//   }
//   code_complete: {
//     implementation_exists: "src/main.rs"
//     tests_exist: "tests/cli_test.rs"
//     ci_passing: true
//     no_unwrap_calls: true
//     no_panics: true
//   }
//   completion: {
//     all_sections_complete: true
//     documentation_updated: true
//     beads_closed: false
//     timestamp: "2026-02-07T23:21:11Z"
//   }
// }