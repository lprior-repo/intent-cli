//// Test Helpers for Ergonomics Tests
//// Common utilities for ATDD, BDD, and error case testing

import gleam/io
import gleeunit/should
import intent/ffi

// ============================================================================
// Test Session Types
// ============================================================================

pub type TestSession {
  TestSession(
    command: String,
    args: List(String),
    exit_code: Int,
    output: String,
    spec_path: option.Option(String),
  )
}

pub type TestResult {
  TestResult(
    success: Bool,
    exit_code: Int,
    json: option.option.Option(String),
    error: option.option.Option(String),
  )
}

pub type ValidationResult {
  ValidationResult(
    valid: Bool,
    reason: String,
    exit_code: Int,
  )
}

// ============================================================================
// Test Execution Helpers
// ============================================================================

/// Execute intent CLI command and capture output and exit code
pub fn execute_intent(command: String, args: List(String)) -> TestResult {
  let full_cmd = "intent " <> command <> " " <> string.join(" ", args)
  
  case io.execute(full_cmd, "") {
    Ok(result) -> {
      let exit_code = result.status_code
      let output = io.read_to_string(result.stdout)
      
      TestResult(
        success: exit_code == 0,
        exit_code: exit_code,
        json: option.from_result(json.decode(output, dynamic.dynamic)),
        error: option.None,
      )
    }
    Error(e) -> {
      TestResult(
        success: False,
        exit_code: 3,  // exit_invalid
        json: option.None,
        error: option.Some(e),
      )
    }
  }
}

/// Validate that JSON has all required fields
pub fn validate_json_structure(json_string: String, required_fields: List(String)) -> ValidationResult {
  case json.decode(json_string, dynamic.dynamic) {
    Ok(parsed) -> {
      let missing_fields = list.filter(required_fields, fn(field) {
        dynamic.field(field, dynamic.dynamic)(parsed) |> result.is_error()
      })
      
      case missing_fields {
        [] -> {
          ValidationResult(
            valid: True,
            reason: "All required fields present",
            exit_code: 0,
          )
        }
        _ -> {
          ValidationResult(
            valid: False,
            reason: "Missing fields: " <> string.join(", ", missing_fields),
            exit_code: 3,
          )
        }
      }
    }
    Error(e) -> {
      ValidationResult(
        valid: False,
        reason: "Invalid JSON: " <> e,
        exit_code: 3,
      )
    }
  }
}

/// Validate exit code matches expected value
pub fn validate_exit_code(result: TestResult, expected: Int) -> ValidationResult {
  let matches = result.exit_code == expected
  let reason = case matches {
    True -> "Exit code correct"
    False -> "Expected exit code " <> int.to_string(expected) <> ", got " <> int.to_string(result.exit_code),
  }
  
  ValidationResult(
    valid: matches,
    reason: reason,
    exit_code: result.exit_code,
  )
}
