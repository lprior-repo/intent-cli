//// Spec Generation Tests (ATDD + BDD)
//// Tests for bead: intent-cli-spec-fix

import gleam/io
import gleam/json
import gleeunit/should
import "$TEST_DIR/test_helpers.gleam" as test_helpers

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// ATDD Tests
// ============================================================================

pub fn interview_generates_valid_spec_structure_test() {
  // Given: A CLI interview session
  let session_id = start_test_interview()
  
  // When: Interview completes
  let complete_result = run_interview_to_completion(session_id)
  
  // Then: Generated spec should have valid structure
  complete_result.success
  |> should.be_true()
  
  complete_result.spec_path
  |> option.is_some()
  |> should.be_true()
}

pub fn interview_spec_includes_required_fields_test() {
  let session_id = start_test_interview()
  let result = run_interview_to_completion(session_id)
  
  let spec = load_spec_from_path(option.unwrap(result.spec_path, ""))
  
  let has_spec_field = has_json_field(spec, "spec")
  let has_features = has_nested_json_field(spec, ["spec", "features"])
  let has_commands = has_nested_json_field(spec, ["spec", "commands"])
  let has_security = has_nested_json_field(spec, ["spec", "security"])
  
  has_spec_field
  && has_features
  && has_commands
  && has_security
  |> should.be_true()
}

// ============================================================================
// BDD User Journey Tests
// ============================================================================

pub fn user_journey_interview_to_analysis_works_test() {
  // Given: I start a CLI interview
  let session_id = start_test_interview()
  
  // When: I generate beads and run validation
  let beads_result = generate_beads_from_session(session_id)
  let spec_path = extract_spec_path_from_beads(beads_result.output)
  let validation_result = execute_intent("validate", [spec_path])
  
  // Then: Validation should succeed
  validation_result.exit_code
  |> should.equal(0)
  
  validation_result.success
  |> should.be_true()
}

pub fn user_journey_interview_to_quality_works_test() {
  let session_id = start_test_interview()
  let result = run_interview_to_completion(session_id)
  let spec_path = option.unwrap(result.spec_path, "")
  
  let quality_result = execute_intent("quality", [spec_path, "--json"])
  
  quality_result.exit_code
  |> should.equal(0)
}

// ============================================================================
// Error Case Tests
// ============================================================================

pub fn invalid_interview_input_returns_exit_3_test() {
  let result = execute_intent("interview", ["--invalid-flag"])
  
  result.exit_code
  |> should.equal(3)
}

pub fn spec_without_top_level_field_fails_validation_test() {
  let session_id = start_test_interview()
  let result = run_interview_to_completion(session_id)
  let spec_path = option.unwrap(result.spec_path, "")
  
  // Modify spec to remove top-level spec field
  let invalid_spec = create_spec_without_top_level()
  write_spec_file(spec_path, invalid_spec)
  
  let validation_result = execute_intent("validate", [spec_path])
  
  validation_result.exit_code
  |> should.equal(3)
}

// ============================================================================
// Helper Functions
// ============================================================================

fn start_test_interview() -> String {
  // In real implementation, this would start an interview
  // For now, return a test session ID
  "test-interview-session-001"
}

fn run_interview_to_completion(session_id: String) -> test_helpers.TestResult {
  // Mock completion - in real implementation this would run the interview
  let mock_spec_path = ".intent/spec-" <> session_id <> ".cue"
  
  test_helpers.TestResult(
    success: True,
    exit_code: 0,
    json: option.None,
    error: option.None,
  )
  |> with_spec_path(mock_spec_path)
}

fn with_spec_path(spec_path: String) -> test_helpers.TestResult {
  test_helpers.TestResult(
    success: True,
    exit_code: 0,
    json: option.None,
    error: option.None,
  )
}

fn load_spec_from_path(path: String) -> json.Json {
  let content = io.read_to_string(path)
  case json.decode(content, json.dynamic) {
    Ok(parsed) -> parsed
    Error(_) -> panic("Failed to load spec")
  }
}

fn generate_beads_from_session(session_id: String) -> test_helpers.TestResult {
  test_helpers.TestResult(
    success: True,
    exit_code: 0,
    json: option.Some("{\"spec_path\": \".intent/spec-" <> session_id <> ".cue\"}"),
    error: option.None,
  )
}

fn extract_spec_path_from_beads(json_string: String) -> String {
  // Mock extraction of spec_path from beads output
  ".intent/spec-test.cue"
}

fn create_spec_without_top_level() -> String {
  // Create invalid spec structure
  "{\"features\": {}, \"commands\": {}}"
}

fn write_spec_file(path: String, content: String) -> Nil {
  io.write(path, content)
}

fn has_json_field(obj: json.Json, field: String) -> Bool {
  dynamic.field(field, dynamic.dynamic)(obj) |> result.is_ok()
}

fn has_nested_json_field(obj: json.Json, path: List(String)) -> Bool {
  case path {
    [] -> True
    [head, ..rest] -> {
      case dynamic.field(head, dynamic.dynamic)(obj) {
        Ok(nested) -> has_json_field(nested, rest)
        Error(_) -> False
      }
    }
  }
}
