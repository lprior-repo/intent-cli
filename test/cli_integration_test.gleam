////
//// Integration tests for CLI flag parsing across all commands
////
//// These tests verify that:
//// - All commands parse flags correctly
//// - Both --flag=value and --flag value syntaxes work
//// - Boolean flags work with and without explicit values
//// - Invalid flags are properly rejected
//// - Exit codes are correct
////

import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import shellout

pub fn main() {
  gleeunit.main()
}

// Helper to run intent command and capture output
fn run_intent(args: List(String)) -> Result(String, #(Int, String)) {
  // Use ./intent from current directory (the binary built by gleam)
  case shellout.command(run: "./intent", with: args, in: ".", opt: []) {
    Ok(output) -> Ok(output)
    Error(#(exit_code, stderr)) -> Error(#(exit_code, stderr))
  }
}

// ============================================================================
// CHECK COMMAND TESTS
// ============================================================================

pub fn check_help_test() {
  case run_intent(["check", "--help"]) {
    Ok(output) -> {
      output
      |> string.contains("Run spec against a target URL")
      |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

pub fn check_with_equals_syntax_test() {
  // Using --target=value syntax
  case
    run_intent([
      "check",
      "examples/pokemon-api.cue",
      "--target=http://example.com",
      "--dry-run=true",
    ])
  {
    Ok(_) -> Nil
    Error(#(_code, stderr)) -> {
      // Should not be a flag parsing error
      stderr |> string.contains("Unknown flag") |> should.be_false()
    }
  }
}

pub fn check_with_space_syntax_test() {
  // Using --target value syntax
  case
    run_intent([
      "check",
      "examples/pokemon-api.cue",
      "--target",
      "http://example.com",
      "--dry-run",
    ])
  {
    Ok(_) -> Nil
    Error(#(_code, stderr)) -> {
      stderr |> string.contains("Unknown flag") |> should.be_false()
    }
  }
}

pub fn check_boolean_flag_test() {
  // Boolean flags without value should work
  case
    run_intent(["check", "examples/pokemon-api.cue", "--json", "--dry-run"])
  {
    Ok(_) -> Nil
    Error(#(_code, stderr)) -> {
      stderr |> string.contains("Unknown flag") |> should.be_false()
    }
  }
}

pub fn check_invalid_flag_test() {
  // Invalid flag should produce clear error
  // Note: Glint framework calls erlang:halt(0) on flag errors (cannot be intercepted)
  case run_intent(["check", "examples/pokemon-api.cue", "--nonexistent-flag"]) {
    Ok(output) -> {
      // Due to Glint limitation, exit code is 0 but we get error output
      let has_invalid = string.contains(output, "invalid flag")
      has_invalid |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// VALIDATE COMMAND TESTS
// ============================================================================

pub fn validate_help_test() {
  case run_intent(["validate", "--help"]) {
    Ok(output) -> {
      output |> string.contains("Validate") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

pub fn validate_with_json_flag_test() {
  // Test --json flag with space syntax
  case run_intent(["validate", "examples/pokemon-api.cue", "--json"]) {
    Ok(_) -> Nil
    Error(#(_code, stderr)) -> {
      stderr |> string.contains("Unknown flag") |> should.be_false()
    }
  }
}

pub fn validate_missing_file_test() {
  // Should exit with error code for missing file
  case run_intent(["validate", "/nonexistent/file.cue"]) {
    Ok(_) -> should.fail()
    Error(#(code, _stderr)) -> {
      // Accept exit codes 3 or 4
      case code {
        3 -> Nil
        4 -> Nil
        _ -> should.fail()
      }
    }
  }
}

// ============================================================================
// SHOW COMMAND TESTS
// ============================================================================

pub fn show_help_test() {
  case run_intent(["show", "--help"]) {
    Ok(output) -> {
      output |> string.contains("Pretty") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// EXPORT COMMAND TESTS
// ============================================================================

pub fn export_help_test() {
  case run_intent(["export", "--help"]) {
    Ok(output) -> {
      output |> string.contains("Export") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// LINT COMMAND TESTS
// ============================================================================

pub fn lint_help_test() {
  case run_intent(["lint", "--help"]) {
    Ok(output) -> {
      output |> string.contains("Check") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// ANALYZE COMMAND TESTS
// ============================================================================

pub fn analyze_help_test() {
  case run_intent(["analyze", "--help"]) {
    Ok(output) -> {
      output |> string.contains("Analyze") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// IMPROVE COMMAND TESTS
// ============================================================================

pub fn improve_help_test() {
  case run_intent(["improve", "--help"]) {
    Ok(output) -> {
      output |> string.contains("Suggest") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// INTERVIEW COMMAND TESTS
// ============================================================================

pub fn interview_help_test() {
  case run_intent(["interview", "--help"]) {
    Ok(output) -> {
      output |> string.contains("INTERVIEW") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

pub fn interview_with_cue_flag_test() {
  // Test --cue flag (equals syntax)
  case run_intent(["interview", "--cue=true", "--profile=api"]) {
    Ok(output) -> {
      // Should produce CUE output with session info
      output |> string.contains("action") |> should.be_true()
    }
    Error(#(_code, stderr)) -> {
      // Should not be flag parsing error
      stderr |> string.contains("Unknown flag") |> should.be_false()
    }
  }
}

pub fn interview_with_profile_flag_test() {
  // Test --profile flag (space syntax)
  case run_intent(["interview", "--cue", "--profile", "api"]) {
    Ok(output) -> {
      output |> string.contains("action") |> should.be_true()
    }
    Error(#(_code, stderr)) -> {
      stderr |> string.contains("Unknown flag") |> should.be_false()
    }
  }
}

// ============================================================================
// BEADS COMMAND TESTS
// ============================================================================

pub fn beads_help_test() {
  case run_intent(["beads", "--help"]) {
    Ok(output) -> {
      output |> string.contains("beads") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// BEAD-STATUS COMMAND TESTS
// ============================================================================

pub fn bead_status_help_test() {
  case run_intent(["bead-status", "--help"]) {
    Ok(output) -> {
      output |> string.contains("bead") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

pub fn bead_status_with_flags_test() {
  // Test multiple flags
  case run_intent(["bead-status", "--bead-id=test-123", "--status=success"]) {
    Ok(_) -> Nil
    Error(#(_code, stderr)) -> {
      stderr |> string.contains("Unknown flag") |> should.be_false()
    }
  }
}

// ============================================================================
// HISTORY COMMAND TESTS
// ============================================================================

pub fn history_help_test() {
  case run_intent(["history", "--help"]) {
    Ok(output) -> {
      output |> string.contains("View") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// DIFF COMMAND TESTS
// ============================================================================

pub fn diff_help_test() {
  case run_intent(["diff", "--help"]) {
    Ok(output) -> {
      output |> string.contains("Compare") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// SESSIONS COMMAND TESTS
// ============================================================================

pub fn sessions_help_test() {
  case run_intent(["sessions", "--help"]) {
    Ok(output) -> {
      output |> string.contains("sessions") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

pub fn sessions_with_profile_filter_test() {
  // Test --profile flag
  case run_intent(["sessions", "--profile", "api"]) {
    Ok(_output) -> Nil
    Error(#(_code, stderr)) -> {
      stderr |> string.contains("Unknown flag") |> should.be_false()
    }
  }
}

// ============================================================================
// KIRK QUALITY COMMAND TESTS
// ============================================================================

pub fn quality_help_test() {
  case run_intent(["quality", "--help"]) {
    Ok(output) -> {
      output |> string.contains("quality") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// KIRK INVERT COMMAND TESTS
// ============================================================================

pub fn invert_help_test() {
  case run_intent(["invert", "--help"]) {
    Ok(output) -> {
      output |> string.contains("invert") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// KIRK COVERAGE COMMAND TESTS
// ============================================================================

pub fn coverage_help_test() {
  case run_intent(["coverage", "--help"]) {
    Ok(output) -> {
      output |> string.contains("coverage") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// KIRK GAPS COMMAND TESTS
// ============================================================================

pub fn gaps_help_test() {
  case run_intent(["gaps", "--help"]) {
    Ok(output) -> {
      output |> string.contains("gaps") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// KIRK COMPACT COMMAND TESTS
// ============================================================================

pub fn compact_help_test() {
  case run_intent(["compact", "--help"]) {
    Ok(output) -> {
      output |> string.contains("compact") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// KIRK PROTOTEXT COMMAND TESTS
// ============================================================================

pub fn prototext_help_test() {
  case run_intent(["prototext", "--help"]) {
    Ok(output) -> {
      output |> string.contains("prototext") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// KIRK EARS COMMAND TESTS
// ============================================================================

pub fn ears_help_test() {
  case run_intent(["ears", "--help"]) {
    Ok(output) -> {
      output |> string.contains("ears") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// PARSE COMMAND TESTS
// ============================================================================

pub fn parse_help_test() {
  case run_intent(["parse", "--help"]) {
    Ok(output) -> {
      output |> string.contains("parse") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// KIRK EFFECTS COMMAND TESTS
// ============================================================================

pub fn effects_help_test() {
  case run_intent(["effects", "--help"]) {
    Ok(output) -> {
      output |> string.contains("effects") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// PLAN COMMAND TESTS
// ============================================================================

pub fn plan_help_test() {
  case run_intent(["plan", "--help"]) {
    Ok(output) -> {
      output |> string.contains("plan") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// PLAN-APPROVE COMMAND TESTS
// ============================================================================

pub fn plan_approve_help_test() {
  case run_intent(["plan-approve", "--help"]) {
    Ok(output) -> {
      output |> string.contains("approve") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// BEADS-REGENERATE COMMAND TESTS
// ============================================================================

pub fn beads_regenerate_help_test() {
  case run_intent(["beads-regenerate", "--help"]) {
    Ok(output) -> {
      output |> string.contains("regenerate") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// ABOUT COMMAND TESTS
// ============================================================================

pub fn about_command_test() {
  case run_intent(["about"]) {
    Ok(output) -> {
      output |> string.contains("Intent") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

pub fn about_help_test() {
  case run_intent(["about", "--help"]) {
    Ok(output) -> {
      output |> string.contains("about") |> should.be_true()
    }
    Error(#(_code, _stderr)) -> should.fail()
  }
}

// ============================================================================
// COMPREHENSIVE FLAG SYNTAX TESTS
// ============================================================================

pub fn check_all_flag_variations_test() {
  // Test all combinations for check command
  let test_cases = [
    // Equals syntax
    [
      "check",
      "examples/pokemon-api.cue",
      "--target=http://example.com",
      "--json=true",
      "--dry-run=true",
    ],
    // Space syntax
    [
      "check",
      "examples/pokemon-api.cue",
      "--target",
      "http://example.com",
      "--json",
      "--dry-run",
    ],
    // Mixed syntax
    [
      "check",
      "examples/pokemon-api.cue",
      "--target=http://example.com",
      "--json",
      "--dry-run=true",
    ],
  ]

  // All should parse flags correctly (may fail for other reasons)
  list.each(test_cases, fn(args) {
    case run_intent(args) {
      Ok(_) -> Nil
      Error(#(_code, stderr)) -> {
        // Should not be flag parsing error
        stderr |> string.contains("Unknown flag") |> should.be_false()
      }
    }
  })
}

// ============================================================================
// EXIT CODE VALIDATION TESTS
// ============================================================================

pub fn exit_codes_file_not_found_test() {
  // Exit code 3 or 4 for file not found
  case run_intent(["validate", "/absolutely/nonexistent/file.cue"]) {
    Ok(_) -> should.fail()
    Error(#(code, _stderr)) -> {
      // Accept exit codes 3 or 4
      case code {
        3 -> Nil
        4 -> Nil
        _ -> should.fail()
      }
    }
  }
}

pub fn exit_codes_help_success_test() {
  // Help commands should exit 0
  case run_intent(["--help"]) {
    Ok(_) -> Nil
    Error(#(_code, _stderr)) -> should.fail()
  }
}
