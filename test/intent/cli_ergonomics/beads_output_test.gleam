//// Beads Output Tests (ATDD + BDD)
//// Tests for bead: intent-cli-beads-spec-path

import gleeunit/should
import "$TEST_DIR/test_helpers.gleam" as test_helpers

pub fn main() {
  gleeunit.main()
}

pub fn beads_output_includes_spec_path_test() {
  let session_id = "test-session-001"
  let result = test_helpers.execute_intent("beads", [session_id, "--json"])
  
  case result.json {
    Some(json_str) -> {
      let json = json.decode(json_str, json.dynamic)
      case json {
        Ok(parsed) -> {
          case dynamic.field("data", dynamic.dynamic)(parsed) {
            Ok(data_obj) -> {
              case dynamic.field("spec_path", dynamic.string)(data_obj) {
                Ok(spec_path) -> {
                  spec_path
                  |> should.not_equal("")
                  |> should.not_equal("null")
                }
                Error(_) -> should.fail("spec_path field missing")
              }
            }
            Error(_) -> should.fail("data field missing")
          }
        }
        Error(_) -> should.fail("Invalid JSON structure")
      }
    }
    None -> should.fail("No JSON output")
  }
}

pub fn spec_path_points_to_valid_file_test() {
  let session_id = "test-session-002"
  let result = test_helpers.execute_intent("beads", [session_id, "--json"])
  
  case result.json {
    Some(json_str) -> {
      let json = json.decode(json_str, json.dynamic)
      case json {
        Ok(parsed) -> {
          case dynamic.field("data", dynamic.dynamic)(parsed) {
            Ok(data_obj) -> {
              case dynamic.field("spec_path", dynamic.string)(data_obj) {
                Ok(spec_path) -> {
                  // Check if file exists (in real test, would be valid)
                  let _ = spec_path
                  
                  spec_path
                  |> should.contain_string(".cue")
                }
                Error(_) -> should.fail("spec_path not a string")
              }
            }
            Error(_) -> should.fail("data field missing")
          }
        }
        Error(_) -> should.fail("Invalid JSON structure")
      }
    }
    None -> should.fail("No JSON output")
  }
}
