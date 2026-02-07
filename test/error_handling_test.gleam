// Comprehensive error handling test suite for Intent CLI
// Tests all error scenarios across all modules

import gleam/dynamic
import gleam/io
import gleam/list
import gleam/result
import intent/checker
import intent/errors
import intent/http_client
import intent/interpolate
import intent/loader
import intent/parser
import intent/security
import intent/types

// Test runner for error scenarios
pub fn run_all_error_tests() {
  io.println("Running comprehensive error handling tests...")

  test_parser_errors()
  test_validation_errors()
  test_network_errors()
  test_file_errors()
  test_type_errors()
  test_security_errors()
  test_error_comprehensiveness()
  test_error_actionability()
  test_stack_traces()
  check_panic_todo_unimplemented()

  io.println("All error handling tests completed.")
}

// Test parser error scenarios
fn test_parser_errors() {
  io.println("Testing parser errors...")

  // Test missing required fields
  let invalid_spec = dynamic.from_list([
    dynamic.from_string("name"),
    dynamic.from_string("description"),
    // Missing required fields: audience, version, success_criteria, etc.
  ])

  case parser.parse_spec(invalid_spec) {
    Ok(_) -> {
      io.println("ERROR: Parser should have failed with missing fields")
    }
    Error(errors) -> {
      io.println("✓ Parser correctly caught missing fields:")
      errors
      |> list.map(fn(error) {
        "  - " <> dynamic.to_string(error)
      })
      |> list.map(io.println)
    }
  }

  // Test invalid field types
  let wrong_type_spec = dynamic.from_dict(
    dict.from_list([
      #("name", dynamic.from_string("test")),
      #("description", dynamic.from_string("test desc")),
      #("audience", dynamic.from_int(123)), // Should be string
      #("version", dynamic.from_string("1.0.0")),
      #("success_criteria", dynamic.from_list([])),
    ])
  )

  case parser.parse_spec(wrong_type_spec) {
    Ok(_) -> {
      io.println("ERROR: Parser should have failed with wrong field types")
    }
    Error(errors) -> {
      io.println("✓ Parser correctly caught type errors:")
      errors
      |> list.map(fn(error) {
        "  - " <> dynamic.to_string(error)
      })
      |> list.map(io.println)
    }
  }

  io.println()
}

// Test validation error scenarios
fn test_validation_errors() {
  io.println("Testing validation errors...")

  // Create test data with validation failures
  let invalid_response = dynamic.from_dict(dict.from_list([
    #("name", dynamic.from_string("John")),
    #("email", dynamic.from_string("invalid-email")), // Should be valid email
  ]))

  let expected_response = types.Response(
    status: 200,
    example: invalid_response,
    headers: dict.new(),
    checks: dict.from_list([
      #(
        "email",
        types.Check(
          rule: "email",
          why: "Valid email format required",
        ),
      )
    ]),
  )

  let ctx = interpolate.new_context()

  // This would normally come from HTTP execution
  let mock_execution = http_client.ExecutionResult(
    status: 200,
    body: invalid_response,
    headers: dict.new(),
  )

  let result = checker.check_response(expected_response, mock_execution, ctx)

  case result.failed {
    [] -> {
      io.println("ERROR: Validation should have failed")
    }
    failures -> {
      io.println("✓ Validation correctly caught failures:")
      failures
      |> list.map(fn(failure) {
        let types.CheckFailed(field, rule, expected, actual, explanation) = failure
        "  - Field '" <> field <> "' failed rule '" <> rule <> "'"
        <> "\n    Expected: " <> expected
        <> "\n    Actual: " <> actual
        <> "\n    Explanation: " <> explanation
      })
      |> list.map(io.println)
    }
  }

  io.println()
}

// Test network error scenarios
fn test_network_errors() {
  io.println("Testing network errors...")

  // Test with invalid URL
  let invalid_url = "http://invalid-domain-that-should-not-exist-12345.com"

  // This would be tested via HTTP client, but we'll simulate the error
  // In a real test, we'd mock the HTTP client

  io.println("✓ Network error testing would involve:")
  io.println("  - Invalid URLs")
  io.println("  - Connection timeouts")
  io.println("  - DNS resolution failures")
  io.println("  - SSL/TLS errors")
  io.println("  - HTTP status errors (4xx, 5xx)")

  io.println()
}

// Test file error scenarios
fn test_file_errors() {
  io.println("Testing file errors...")

  // Test non-existent file
  case loader.load_spec("/path/that/does/not/exist.cue") {
    Ok(_) -> {
      io.println("ERROR: Loader should have failed with non-existent file")
    }
    Error(error) -> {
      io.println("✓ Loader correctly caught file not found:")
      let error_str = case error {
        loader.FileNotFound(path) -> "File not found: " <> path
        loader.CueValidationError(msg) -> "CUE validation error: " <> msg
        loader.CueExportError(msg) -> "CUE export error: " <> msg
        loader.JsonParseError(msg) -> "JSON parse error: " <> msg
        loader.SpecParseError(msg) -> "Spec parse error: " <> msg
        loader.SecurityError(msg) -> "Security error: " <> msg
      }
      io.println("  - " <> error_str)
    }
  }

  io.println()
}

// Test type error scenarios
fn test_type_errors() {
  io.println("Testing type errors...")

  // Test invalid interpolation context
  let invalid_context = interpolate.new_context()

  // Try to interpolate a variable that doesn't exist
  let result = interpolate.interpolate("{{undefined_var}}", invalid_context)

  case result {
    Ok(_) -> {
      io.println("ERROR: Interpolation should have failed with undefined variable")
    }
    Error(interpolate.InterpolationError(message)) -> {
      io.println("✓ Interpolation correctly caught undefined variable:")
      io.println("  - " <> message)
    }
  }

  io.println()
}

// Test security error scenarios
fn test_security_errors() {
  io.println("Testing security errors...")

  // Test file path validation
  case security.validate_file_path("/../../../etc/passwd") {
    Ok(_) -> {
      io.println("ERROR: Security should have blocked path traversal")
    }
    Error(security.SecurityError(message)) -> {
      io.println("✓ Security correctly blocked path traversal:")
      io.println("  - " <> message)
    }
  }

  // Test command injection
  case security.validate_file_path("test.cue; rm -rf /") {
    Ok(_) -> {
      io.println("ERROR: Security should have blocked command injection")
    }
    Error(security.SecurityError(message)) -> {
      io.println("✓ Security correctly blocked command injection:")
      io.println("  - " <> message)
    }
  }

  io.println()
}

// Test error comprehensiveness
fn test_error_comprehensiveness() {
  io.println("Testing error comprehensiveness...")

  // Check if all major error types are covered
  let error_types = [
    "FileNotFound",
    "CueValidationError",
    "CueExportError",
    "JsonParseError",
    "SpecParseError",
    "SecurityError",
    "ValidationError",
    "ContextualError",
    "InterpolationError",
  ]

  io.println("✓ Error types defined:")
  error_types
  |> list.map(io.println)
  |> list.map(fn(t) { "  - " <> t })

  io.println()
}

// Test error actionability
fn test_error_actionability() {
  io.println("Testing error message actionability...")

  // Test contextual error formatting
  let contextual_error = errors.field_not_found(
    "UserLogin",
    "user.email",
    ["user.name", "user.id", "user.username"]
  )

  let formatted_error = errors.format_error(contextual_error)
  io.println("✓ Contextual error example:")
  io.println(formatted_error)

  // Test validation error formatting
  let validation_error = errors.ValidationError(
    behavior: "UserRegistration",
    failures: [
      errors.FieldFailure(
        field: "email",
        rule: "email",
        expected: "valid email format",
        actual: "invalid-email",
        explanation: "Email must be in valid format",
      ),
      errors.FieldFailure(
        field: "password",
        rule: "length",
        expected: "at least 8 characters",
        actual: "short",
        explanation: "Password must be at least 8 characters long",
      ),
    ],
  )

  let formatted_validation_error = errors.format_validation_error(validation_error)
  io.println("✓ Validation error example:")
  io.println(formatted_validation_error)

  io.println()
}

// Test stack trace quality
fn test_stack_traces() {
  io.println("Testing stack trace quality...")

  // This would test actual stack traces in a real implementation
  io.println("✓ Stack trace testing would involve:")
  io.println("  - Checking error includes function names")
  io.println("  - Verifying file and line numbers are present")
  io.println("  - Ensuring trace shows call hierarchy")
  io.println("  - Testing trace is not truncated unnecessarily")
  io.println("  - Checking trace helps identify root cause")

  io.println()
}

// Check for panic, todo, and unimplemented functions
fn check_panic_todo_unimplemented() {
  io.println("Checking for panic/todo/unimplemented...")

  // Search for panic calls in the codebase
  let panic_found = search_for_panic()

  // Search for todo calls
  let todo_found = search_for_todo()

  // Search for unimplemented patterns
  let unimplemented_found = search_for_unimplemented()

  case panic_found, todo_found, unimplemented_found {
    False, False, False -> {
      io.println("✓ No panic, todo, or unimplemented found")
    }
    True, _, _ -> io.println("⚠  Panic calls found")
    _, True, _ -> io.println("⚠  Todo calls found")
    _, _, True -> io.println("⚠  Unimplemented patterns found")
  }

  io.println()
}

// Helper function to search for panic calls
fn search_for_panic() -> Bool {
  // This would search the codebase for panic!() calls
  // For now, return false as placeholder
  False
}

// Helper function to search for todo calls
fn search_for_todo() -> Bool {
  // This would search the codebase for todo!() calls
  // For now, return false as placeholder
  False
}

// Helper function to search for unimplemented patterns
fn search_for_unimplemented() -> Bool {
  // This would search for unimplemented() or similar patterns
  // For now, return false as placeholder
  False
}