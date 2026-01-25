//// Tests for JSONL stdin input parsing for AI-native batch mode
////
//// Each line from stdin is a JSON command object that needs to be:
//// 1. Parsed from JSON
//// 2. Validated for required fields
//// 3. Converted to typed CommandInput records
////
//// This module tests the pure parsing functions (no I/O).

import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit/should
import intent/jsonl_input

// =============================================================================
// Parse Single Line Tests
// =============================================================================

/// Test: parse_line handles valid command with all fields
pub fn test_parse_line_valid_full_test() {
  let line =
    "{\"id\":\"req-1\",\"command\":\"quality\",\"args\":{\"spec_path\":\"api.cue\"}}"
  let result = jsonl_input.parse_line(line)

  result |> should.be_ok()
  let input = result |> result.unwrap(jsonl_input.empty_command_input())
  input.id |> should.equal("req-1")
  input.command |> should.equal("quality")
  // Args should contain spec_path
  case dynamic.field("spec_path", dynamic.string)(input.args) {
    Ok(path) -> path |> should.equal("api.cue")
    Error(_) -> should.fail()
  }
}

/// Test: parse_line handles command with minimal fields (auto-generates id)
pub fn test_parse_line_minimal_test() {
  let line = "{\"command\":\"coverage\"}"
  let result = jsonl_input.parse_line(line)

  result |> should.be_ok()
  let input = result |> result.unwrap(jsonl_input.empty_command_input())
  // ID should be auto-generated (non-empty)
  input.id |> string.length() |> fn(len) { len > 0 } |> should.be_true()
  input.command |> should.equal("coverage")
}

/// Test: parse_line rejects invalid JSON
pub fn test_parse_line_invalid_json_test() {
  let line = "not valid json"
  let result = jsonl_input.parse_line(line)

  result |> should.be_error()
  case result {
    Error(jsonl_input.InvalidJson(_, _)) -> Nil
    _ -> should.fail()
  }
}

/// Test: parse_line rejects missing command field
pub fn test_parse_line_missing_command_test() {
  let line = "{\"id\":\"req-1\",\"args\":{}}"
  let result = jsonl_input.parse_line(line)

  result |> should.be_error()
  case result {
    Error(jsonl_input.MissingField("command")) -> Nil
    _ -> should.fail()
  }
}

/// Test: parse_line rejects empty command
pub fn test_parse_line_empty_command_test() {
  let line = "{\"command\":\"\"}"
  let result = jsonl_input.parse_line(line)

  result |> should.be_error()
  case result {
    Error(jsonl_input.EmptyCommand) -> Nil
    _ -> should.fail()
  }
}

/// Test: parse_line handles empty args gracefully
pub fn test_parse_line_empty_args_test() {
  let line = "{\"command\":\"gaps\"}"
  let result = jsonl_input.parse_line(line)

  result |> should.be_ok()
  let input = result |> result.unwrap(jsonl_input.empty_command_input())
  input.command |> should.equal("gaps")
}

// =============================================================================
// Parse Multiple Lines Tests
// =============================================================================

/// Test: parse_lines handles multiple valid lines
pub fn test_parse_lines_multiple_valid_test() {
  let content =
    "{\"command\":\"quality\",\"args\":{\"spec_path\":\"a.cue\"}}\n{\"command\":\"coverage\",\"args\":{\"spec_path\":\"b.cue\"}}"
  let result = jsonl_input.parse_lines(content)

  result |> list.length() |> should.equal(2)

  // Check first result is Ok
  case list.first(result) {
    Ok(Ok(input)) -> input.command |> should.equal("quality")
    _ -> should.fail()
  }

  // Check second result is Ok
  case list.last(result) {
    Ok(Ok(input)) -> input.command |> should.equal("coverage")
    _ -> should.fail()
  }
}

/// Test: parse_lines handles empty content
pub fn test_parse_lines_empty_content_test() {
  let content = ""
  let result = jsonl_input.parse_lines(content)

  result |> should.equal([])
}

/// Test: parse_lines handles whitespace-only content
pub fn test_parse_lines_whitespace_only_test() {
  let content = "  \n  \n  "
  let result = jsonl_input.parse_lines(content)

  result |> should.equal([])
}

/// Test: parse_lines skips blank lines between valid commands
pub fn test_parse_lines_skips_blank_lines_test() {
  let content = "{\"command\":\"quality\"}\n\n{\"command\":\"gaps\"}\n"
  let result = jsonl_input.parse_lines(content)

  result |> list.length() |> should.equal(2)
}

/// Test: parse_lines collects errors for invalid lines
pub fn test_parse_lines_collects_errors_test() {
  let content =
    "{\"command\":\"quality\"}\ninvalid json\n{\"command\":\"gaps\"}"
  let result = jsonl_input.parse_lines(content)

  result |> list.length() |> should.equal(3)

  // Pattern match on the list to verify each element
  case result {
    [Ok(_), Error(_), Ok(_)] -> Nil
    _ -> should.fail()
  }
}

// =============================================================================
// Extract Fields Tests
// =============================================================================

/// Test: extract_arg extracts string argument
pub fn test_extract_arg_string_test() {
  let line = "{\"command\":\"quality\",\"args\":{\"spec_path\":\"api.cue\"}}"
  let result = jsonl_input.parse_line(line)

  result |> should.be_ok()
  let input = result |> result.unwrap(jsonl_input.empty_command_input())
  let spec_path = jsonl_input.extract_string_arg(input, "spec_path")

  spec_path |> should.equal(Some("api.cue"))
}

/// Test: extract_arg returns None for missing argument
pub fn test_extract_arg_missing_test() {
  let line = "{\"command\":\"quality\",\"args\":{}}"
  let result = jsonl_input.parse_line(line)

  result |> should.be_ok()
  let input = result |> result.unwrap(jsonl_input.empty_command_input())
  let missing = jsonl_input.extract_string_arg(input, "nonexistent")

  missing |> should.equal(None)
}

/// Test: extract_bool_arg extracts boolean argument
pub fn test_extract_bool_arg_test() {
  let line = "{\"command\":\"check\",\"args\":{\"verbose\":true}}"
  let result = jsonl_input.parse_line(line)

  result |> should.be_ok()
  let input = result |> result.unwrap(jsonl_input.empty_command_input())
  let verbose = jsonl_input.extract_bool_arg(input, "verbose")

  verbose |> should.equal(Some(True))
}

/// Test: extract_int_arg extracts integer argument
pub fn test_extract_int_arg_test() {
  let line = "{\"command\":\"check\",\"args\":{\"timeout\":5000}}"
  let result = jsonl_input.parse_line(line)

  result |> should.be_ok()
  let input = result |> result.unwrap(jsonl_input.empty_command_input())
  let timeout = jsonl_input.extract_int_arg(input, "timeout")

  timeout |> should.equal(Some(5000))
}

// =============================================================================
// Error Formatting Tests
// =============================================================================

/// Test: format_error produces readable error message
pub fn test_format_error_invalid_json_test() {
  let error = jsonl_input.InvalidJson("bad json", "Unexpected token")
  let formatted = jsonl_input.format_error(error)

  formatted |> string.contains("Invalid JSON") |> should.be_true()
  formatted |> string.contains("bad json") |> should.be_true()
}

/// Test: format_error for missing field
pub fn test_format_error_missing_field_test() {
  let error = jsonl_input.MissingField("command")
  let formatted = jsonl_input.format_error(error)

  formatted |> string.contains("Missing required field") |> should.be_true()
  formatted |> string.contains("command") |> should.be_true()
}

/// Test: format_error for empty command
pub fn test_format_error_empty_command_test() {
  let error = jsonl_input.EmptyCommand
  let formatted = jsonl_input.format_error(error)

  formatted |> string.contains("Command cannot be empty") |> should.be_true()
}

// =============================================================================
// Error to JSON Tests
// =============================================================================

/// Test: error_to_json produces valid JSON error response
pub fn test_error_to_json_test() {
  let error = jsonl_input.InvalidJson("bad", "parse error")
  let json_str = jsonl_input.error_to_json(error, "req-123")

  // Should be valid JSON
  case json.decode(json_str, dynamic.dynamic) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  // Should contain error info
  json_str |> string.contains("\"success\":false") |> should.be_true()
  json_str |> string.contains("\"id\":\"req-123\"") |> should.be_true()
}

// =============================================================================
// Validate Command Tests
// =============================================================================

/// Test: validate_command accepts known commands
pub fn test_validate_command_known_test() {
  let known_commands = [
    "quality",
    "coverage",
    "gaps",
    "invert",
    "effects",
    "check",
  ]

  list.each(known_commands, fn(cmd) {
    let result = jsonl_input.validate_command(cmd, known_commands)
    result |> should.be_ok()
  })
}

/// Test: validate_command rejects unknown commands
pub fn test_validate_command_unknown_test() {
  let known_commands = ["quality", "coverage", "gaps"]
  let result = jsonl_input.validate_command("unknown", known_commands)

  result |> should.be_error()
  case result {
    Error(jsonl_input.UnknownCommand("unknown", _)) -> Nil
    _ -> should.fail()
  }
}

/// Test: validate_command provides suggestions for typos
pub fn test_validate_command_suggests_similar_test() {
  let known_commands = ["quality", "coverage", "gaps"]
  let result = jsonl_input.validate_command("qualty", known_commands)

  result |> should.be_error()
  case result {
    Error(jsonl_input.UnknownCommand(_, suggestions)) -> {
      // Should suggest "quality" as similar
      suggestions |> list.contains("quality") |> should.be_true()
    }
    _ -> should.fail()
  }
}
