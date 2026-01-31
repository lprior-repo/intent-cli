//// JSON Flag Tests (ATDD + BDD)
//// Tests for bead: intent-cli-json-flag

import gleeunit/should
import "$TEST_DIR/test_helpers.gleam" as test_helpers

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// ATDD Tests
// ============================================================================

pub fn json_flag_works_for_all_commands_test() {
  let commands = [
    "validate", "show", "lint", "quality", "doctor",
    "beads", "sessions", "history", "analyze", "improve",
    "coverage", "gaps", "invert", "effects",
  ]
  
  commands
  |> list.each(fn(cmd) {
       let result = test_helpers.execute_intent(cmd, ["spec.cue", "--json"])
       
       result.exit_code
       |> should.equal(0)
       
       result.json
       |> option.is_some()
       |> should.be_true()
     })
}

pub fn json_output_has_required_fields_test() {
  let result = test_helpers.execute_intent("quality", ["spec.cue", "--json"])
  
  case result.json {
    Some(json_str) -> {
      let json = json.decode(json_str, json.dynamic)
      case json {
        Ok(parsed) -> {
          let validation = test_helpers.validate_json_structure(
            json_str,
            ["success", "action", "command", "data", "errors", "metadata", "next_actions"],
          )
          
          validation.valid
          |> should.be_true()
        }
        Error(_) -> should.fail("Invalid JSON")
      }
    }
    None -> should.fail("No JSON output")
  }
}

pub fn json_mode_not_human_mode_test() {
  // Test with --json flag
  let result_with_json = test_helpers.execute_intent("quality", ["spec.cue", "--json"])
  
  // Test without --json flag (human mode)
  let result_without_json = test_helpers.execute_intent("quality", ["spec.cue"])
  
  // JSON mode should have structured output
  result_with_json.json
  |> option.is_some()
  |> should.be_true()
  
  // Human mode output may differ
  result_without_json.json
  |> should.not_equal(result_with_json.json)
}
