import gleam/string
import gleeunit/should
import intent/progress_output

/// Test that format_progress returns a properly formatted progress message
pub fn format_progress_basic_test() {
  let result = progress_output.format_progress("test_action", "Processing file")
  should.be_true(has_timestamp(result))
  should.be_true(has_action("test_action", result))
  should.be_true(has_message("Processing file", result))
}

/// Test that format_progress handles empty action
pub fn format_progress_empty_action_test() {
  let result = progress_output.format_progress("", "message")
  should.be_true(has_timestamp(result))
  should.be_true(has_message("message", result))
}

/// Test that format_progress handles empty message
pub fn format_progress_empty_message_test() {
  let result = progress_output.format_progress("action", "")
  should.be_true(has_timestamp(result))
  should.be_true(has_action("action", result))
}

/// Test that write_progress writes to stderr when enabled
pub fn write_progress_enabled_test() {
  // When progress is enabled, should return Ok(Nil)
  let result = progress_output.write_progress(True, "test", "message")
  should.be_ok(result)
}

/// Test that write_progress does nothing when disabled
pub fn write_progress_disabled_test() {
  // When progress is disabled, should return Ok(Nil) without writing
  let result = progress_output.write_progress(False, "test", "message")
  should.be_ok(result)
}

/// Test progress JSON format structure
pub fn format_progress_json_structure_test() {
  let result = progress_output.format_progress("validate", "Checking spec")
  // Should contain required JSON-like fields
  should.be_true(contains_substring("\"timestamp\":", result))
  should.be_true(contains_substring("\"action\":", result))
  should.be_true(contains_substring("\"message\":", result))
}

// Helper functions for assertions
fn has_timestamp(text: String) -> Bool {
  string.contains(text, "timestamp")
}

fn has_action(action: String, text: String) -> Bool {
  string.contains(text, action)
}

fn has_message(message: String, text: String) -> Bool {
  string.contains(text, message)
}

fn contains_substring(substring: String, text: String) -> Bool {
  string.contains(text, substring)
}
