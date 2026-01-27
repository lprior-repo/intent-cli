//// Tests for the loader module
//// Validates the Functional Core / Imperative Shell architecture:
//// - Pure functions: parse_cue_validation_result, parse_cue_export_result, parse_json_to_spec
//// - Error formatting: format_error
////
//// Note: The impure functions (load_spec_with_executor, validate_cue_with_executor,
//// export_cue_with_executor) require valid file paths due to security validation.
//// Integration tests for those would need to use real CUE files from examples/.

import gleam/string
import gleeunit/should
import intent/loader.{
  CueExportFailed, CueValidationFailed, FileNotFound, JsonDecodeFailed,
  SecurityError, SpecParseFailed,
}

// ============================================================================
// parse_cue_validation_result Tests (PURE FUNCTION)
// ============================================================================

pub fn parse_cue_validation_result_success_test() {
  // Successful validation returns Ok(Nil)
  loader.parse_cue_validation_result("test.cue", Ok(""))
  |> should.be_ok
}

pub fn parse_cue_validation_result_with_stdout_test() {
  // Successful validation with stdout content still returns Ok(Nil)
  loader.parse_cue_validation_result("test.cue", Ok("some output"))
  |> should.be_ok
}

pub fn parse_cue_validation_result_failure_test() {
  // Failed validation returns CueValidationFailed error
  let result =
    loader.parse_cue_validation_result("test.cue", Error(#(1, "syntax error")))

  case result {
    Error(CueValidationFailed(path, exit_code, stderr)) -> {
      path |> should.equal("test.cue")
      exit_code |> should.equal(1)
      stderr |> should.equal("syntax error")
    }
    _ -> should.fail()
  }
}

pub fn parse_cue_validation_result_exit_code_preserved_test() {
  // Exit code should be preserved in error
  let result =
    loader.parse_cue_validation_result("spec.cue", Error(#(42, "error")))

  case result {
    Error(CueValidationFailed(_, exit_code, _)) -> {
      exit_code |> should.equal(42)
    }
    _ -> should.fail()
  }
}

pub fn parse_cue_validation_result_path_preserved_test() {
  // Path should be preserved in error
  let result =
    loader.parse_cue_validation_result(
      "my/nested/path.cue",
      Error(#(1, "error")),
    )

  case result {
    Error(CueValidationFailed(path, _, _)) -> {
      path |> should.equal("my/nested/path.cue")
    }
    _ -> should.fail()
  }
}

pub fn parse_cue_validation_result_stderr_preserved_test() {
  // Stderr should be preserved in error
  let stderr_message =
    "spec.cue:10:5: invalid type expression\nspec.cue:15:10: undefined field"
  let result =
    loader.parse_cue_validation_result("spec.cue", Error(#(1, stderr_message)))

  case result {
    Error(CueValidationFailed(_, _, stderr)) -> {
      stderr |> should.equal(stderr_message)
    }
    _ -> should.fail()
  }
}

// ============================================================================
// parse_cue_export_result Tests (PURE FUNCTION)
// ============================================================================

pub fn parse_cue_export_result_success_test() {
  // Successful export returns the JSON string
  loader.parse_cue_export_result("test.cue", Ok("{\"name\": \"test\"}"))
  |> should.equal(Ok("{\"name\": \"test\"}"))
}

pub fn parse_cue_export_result_empty_json_test() {
  // Empty JSON is still valid output
  loader.parse_cue_export_result("test.cue", Ok("{}"))
  |> should.equal(Ok("{}"))
}

pub fn parse_cue_export_result_complex_json_test() {
  // Complex JSON should be preserved exactly
  let json = "{\"features\": [{\"name\": \"auth\", \"behaviors\": []}]}"
  loader.parse_cue_export_result("test.cue", Ok(json))
  |> should.equal(Ok(json))
}

pub fn parse_cue_export_result_failure_test() {
  // Failed export returns CueExportFailed error
  let result =
    loader.parse_cue_export_result("test.cue", Error(#(1, "export error")))

  case result {
    Error(CueExportFailed(path, exit_code, stderr)) -> {
      path |> should.equal("test.cue")
      exit_code |> should.equal(1)
      stderr |> should.equal("export error")
    }
    _ -> should.fail()
  }
}

pub fn parse_cue_export_result_exit_code_preserved_test() {
  // Exit code should be preserved in error
  let result =
    loader.parse_cue_export_result("spec.cue", Error(#(99, "export failed")))

  case result {
    Error(CueExportFailed(_, exit_code, _)) -> {
      exit_code |> should.equal(99)
    }
    _ -> should.fail()
  }
}

// ============================================================================
// parse_json_to_spec Tests (PURE FUNCTION)
// ============================================================================

pub fn parse_json_to_spec_minimal_spec_test() {
  // A minimal valid spec should parse successfully
  let json_str =
    "{
    \"name\": \"Test API\",
    \"description\": \"Test description\",
    \"version\": \"1.0.0\",
    \"audience\": \"developers\",
    \"success_criteria\": [],
    \"config\": {
      \"base_url\": \"http://localhost:8080\",
      \"timeout_ms\": 5000,
      \"headers\": {},
      \"allow_localhost\": false
    },
    \"features\": [],
    \"rules\": [],
    \"anti_patterns\": [],
    \"ai_hints\": {
      \"implementation\": {\"suggested_stack\": []},
      \"entities\": {},
      \"security\": {
        \"password_hashing\": \"\",
        \"jwt_algorithm\": \"\",
        \"jwt_expiry\": \"\",
        \"rate_limiting\": \"\"
      },
      \"pitfalls\": []
    }
  }"

  let result = loader.parse_json_to_spec(json_str)

  case result {
    Ok(spec) -> {
      spec.name |> should.equal("Test API")
      spec.description |> should.equal("Test description")
      spec.version |> should.equal("1.0.0")
      spec.audience |> should.equal("developers")
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_json_to_spec_with_features_test() {
  // Spec with features should parse all fields
  let json_str =
    "{
    \"name\": \"User API\",
    \"description\": \"User management API\",
    \"version\": \"2.0.0\",
    \"audience\": \"internal\",
    \"success_criteria\": [\"All endpoints respond within 100ms\"],
    \"config\": {
      \"base_url\": \"https://api.example.com\",
      \"timeout_ms\": 10000,
      \"headers\": {},
      \"allow_localhost\": false
    },
    \"features\": [{
      \"name\": \"User Management\",
      \"description\": \"CRUD operations for users\",
      \"behaviors\": []
    }],
    \"rules\": [],
    \"anti_patterns\": [],
    \"ai_hints\": {
      \"implementation\": {\"suggested_stack\": []},
      \"entities\": {},
      \"security\": {
        \"password_hashing\": \"bcrypt\",
        \"jwt_algorithm\": \"RS256\",
        \"jwt_expiry\": \"1h\",
        \"rate_limiting\": \"100/min\"
      },
      \"pitfalls\": []
    }
  }"

  let result = loader.parse_json_to_spec(json_str)

  case result {
    Ok(spec) -> {
      spec.name |> should.equal("User API")
      spec.version |> should.equal("2.0.0")
      spec.config.base_url |> should.equal("https://api.example.com")
      spec.config.timeout_ms |> should.equal(10_000)
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_json_to_spec_invalid_json_test() {
  // Invalid JSON should return JsonDecodeFailed
  let result = loader.parse_json_to_spec("not valid json")

  case result {
    Error(JsonDecodeFailed(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn parse_json_to_spec_incomplete_json_test() {
  // Incomplete JSON should return JsonDecodeFailed
  let result = loader.parse_json_to_spec("{\"name\": ")

  case result {
    Error(JsonDecodeFailed(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn parse_json_to_spec_missing_required_field_test() {
  // JSON missing required fields should return SpecParseFailed
  let json_str = "{\"name\": \"Test\"}"

  let result = loader.parse_json_to_spec(json_str)

  case result {
    Error(SpecParseFailed(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn parse_json_to_spec_empty_object_test() {
  // Empty object should return SpecParseFailed (missing required fields)
  let result = loader.parse_json_to_spec("{}")

  case result {
    Error(SpecParseFailed(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn parse_json_to_spec_null_json_test() {
  // JSON null should return SpecParseFailed
  let result = loader.parse_json_to_spec("null")

  case result {
    Error(SpecParseFailed(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn parse_json_to_spec_array_json_test() {
  // JSON array should return SpecParseFailed (expected object)
  let result = loader.parse_json_to_spec("[1, 2, 3]")

  case result {
    Error(SpecParseFailed(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn parse_json_to_spec_empty_string_test() {
  // Empty string should return JsonDecodeFailed
  let result = loader.parse_json_to_spec("")

  case result {
    Error(JsonDecodeFailed(_)) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ============================================================================
// format_error Tests
// ============================================================================

pub fn format_error_file_not_found_test() {
  let error = FileNotFound("missing.cue")
  let formatted = loader.format_error(error)

  // Should contain file not found message
  formatted
  |> string.contains("File not found: missing.cue")
  |> should.be_true

  // Should contain helpful guidance
  formatted
  |> string.contains("intent interview")
  |> should.be_true

  // Should suggest examples directory
  formatted
  |> string.contains("examples/")
  |> should.be_true
}

pub fn format_error_file_not_found_with_path_test() {
  let error = FileNotFound("examples/api/spec.cue")
  let formatted = loader.format_error(error)

  // Should contain file not found message
  formatted
  |> string.contains("File not found: examples/api/spec.cue")
  |> should.be_true

  // Should contain helpful guidance
  formatted
  |> string.contains("intent interview")
  |> should.be_true

  // Should suggest examples directory
  formatted
  |> string.contains("examples/")
  |> should.be_true
}

pub fn format_error_validation_failed_test() {
  let error = CueValidationFailed("test.cue", 1, "syntax error")
  let formatted = loader.format_error(error)

  formatted
  |> should.equal(
    "CUE validation failed for 'test.cue' (exit code 1):\nsyntax error",
  )
}

pub fn format_error_validation_failed_multiline_stderr_test() {
  let stderr = "line 5: unexpected token\nline 10: missing field"
  let error = CueValidationFailed("spec.cue", 1, stderr)
  let formatted = loader.format_error(error)

  formatted
  |> should.equal(
    "CUE validation failed for 'spec.cue' (exit code 1):\n" <> stderr,
  )
}

pub fn format_error_export_failed_test() {
  let error = CueExportFailed("test.cue", 2, "no spec found")
  let formatted = loader.format_error(error)

  formatted
  |> should.equal(
    "CUE export failed for 'test.cue' (exit code 2):\nno spec found",
  )
}

pub fn format_error_export_failed_with_exit_code_test() {
  let error = CueExportFailed("api.cue", 127, "command not found")
  let formatted = loader.format_error(error)

  formatted
  |> should.equal(
    "CUE export failed for 'api.cue' (exit code 127):\ncommand not found",
  )
}

pub fn format_error_security_error_test() {
  let error = SecurityError("Path traversal detected")
  loader.format_error(error) |> should.equal("Path traversal detected")
}

pub fn format_error_security_error_file_not_accessible_test() {
  // Test the actual error message when a file doesn't exist
  let error =
    SecurityError(
      "Security error: File 'missing.cue' is not accessible or does not exist.",
    )
  let formatted = loader.format_error(error)

  // Should contain the original error
  formatted
  |> string.contains("File 'missing.cue' is not accessible or does not exist")
  |> should.be_true

  // Should contain helpful guidance
  formatted
  |> string.contains("intent interview")
  |> should.be_true

  // Should suggest examples directory
  formatted
  |> string.contains("examples/")
  |> should.be_true
}

pub fn format_error_security_error_with_examples_path_test() {
  // Test error message for missing file in examples/
  let error =
    SecurityError(
      "Security error: File 'examples/api.cue' is not accessible or does not exist.",
    )
  let formatted = loader.format_error(error)

  // Should contain the original error
  formatted
  |> string.contains(
    "File 'examples/api.cue' is not accessible or does not exist",
  )
  |> should.be_true

  // Should contain helpful guidance
  formatted
  |> string.contains("intent interview")
  |> should.be_true

  // Should suggest specs directory
  formatted
  |> string.contains("specs/")
  |> should.be_true
}

pub fn format_error_security_error_detailed_test() {
  let error =
    SecurityError(
      "Security error: Invalid path '../etc/passwd': Not a regular file",
    )
  loader.format_error(error)
  |> should.equal(
    "Security error: Invalid path '../etc/passwd': Not a regular file",
  )
}

pub fn format_error_json_decode_failed_test() {
  // JsonDecodeFailed should format decode errors
  let error = JsonDecodeFailed([])
  let formatted = loader.format_error(error)

  // Should contain decode error message
  formatted |> should.not_equal("")
}

pub fn format_error_spec_parse_failed_test() {
  // SpecParseFailed should format spec errors
  let error = SpecParseFailed([])
  let formatted = loader.format_error(error)

  // Should contain parse error message
  formatted |> should.not_equal("")
}

// ============================================================================
// Error Type Construction Tests
// ============================================================================

pub fn load_error_file_not_found_construction_test() {
  let error: loader.LoadError = FileNotFound("path/to/file.cue")
  let FileNotFound(path) = error
  path |> should.equal("path/to/file.cue")
}

pub fn load_error_cue_validation_failed_construction_test() {
  let error: loader.LoadError =
    CueValidationFailed("spec.cue", 1, "error message")
  let CueValidationFailed(path, exit_code, stderr) = error
  path |> should.equal("spec.cue")
  exit_code |> should.equal(1)
  stderr |> should.equal("error message")
}

pub fn load_error_cue_export_failed_construction_test() {
  let error: loader.LoadError = CueExportFailed("spec.cue", 2, "export error")
  let CueExportFailed(path, exit_code, stderr) = error
  path |> should.equal("spec.cue")
  exit_code |> should.equal(2)
  stderr |> should.equal("export error")
}

pub fn load_error_security_error_construction_test() {
  let error: loader.LoadError = SecurityError("security violation detected")
  let SecurityError(message) = error
  message |> should.equal("security violation detected")
}
