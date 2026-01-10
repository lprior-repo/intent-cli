//// Tests for the CUE spec loader module
//// Tests file I/O errors, CUE validation, and error formatting
//// Note: These tests focus on error handling since actual CUE parsing
//// requires the cue CLI tool and valid CUE files

import gleam/string
import gleeunit/should
import intent/loader.{
  CueExportError, CueValidationError, FileNotFound, JsonParseError,
  LightSpecParseError, SpecParseError,
}

// ============================================================================
// File Not Found Tests
// ============================================================================

pub fn loader_file_not_found_test() {
  // Attempt to load a non-existent file
  let result = loader.load_spec("/nonexistent/path/to/spec.cue")

  case result {
    Error(FileNotFound(path)) -> {
      path
      |> should.equal("/nonexistent/path/to/spec.cue")
    }
    _ -> should.fail()
  }
}

pub fn loader_file_not_found_relative_path_test() {
  // Relative path that doesn't exist
  let result = loader.load_spec("nonexistent.cue")

  case result {
    Error(FileNotFound(path)) -> {
      path
      |> should.equal("nonexistent.cue")
    }
    _ -> should.fail()
  }
}

pub fn loader_file_not_found_empty_path_test() {
  // Empty path should fail
  let result = loader.load_spec("")

  case result {
    Error(FileNotFound(_)) -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn loader_directory_instead_of_file_test() {
  // Passing a directory instead of a file
  let result = loader.load_spec("/tmp")

  case result {
    Error(FileNotFound(_)) -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

// ============================================================================
// CUE Validation Tests
// ============================================================================

pub fn loader_validate_nonexistent_file_test() {
  let result = loader.validate_cue("/nonexistent/spec.cue")

  // validate_cue doesn't check file existence first - cue vet will fail
  case result {
    Error(_) -> should.be_ok(Ok(Nil))
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// Error Formatting Tests
// ============================================================================

pub fn loader_format_file_not_found_test() {
  let error = FileNotFound("/path/to/spec.cue")
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("not found")
  |> should.be_true

  formatted
  |> string.contains("/path/to/spec.cue")
  |> should.be_true
}

pub fn loader_format_cue_validation_error_test() {
  let error = CueValidationError("line 10: undefined field 'foo'")
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("CUE validation failed")
  |> should.be_true

  formatted
  |> string.contains("undefined field")
  |> should.be_true
}

pub fn loader_format_cue_export_error_test() {
  let error = CueExportError("cannot export: field 'spec' not found")
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("CUE export failed")
  |> should.be_true

  formatted
  |> string.contains("spec")
  |> should.be_true
}

pub fn loader_format_json_parse_error_test() {
  let error = JsonParseError("Unexpected byte: 'x' in JSON")
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("JSON parse error")
  |> should.be_true
}

pub fn loader_format_spec_parse_error_test() {
  let error = SpecParseError("Expected string but found int at .name")
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("Spec parse error")
  |> should.be_true

  formatted
  |> string.contains("Expected string")
  |> should.be_true
}

pub fn loader_format_light_spec_parse_error_test() {
  let error = LightSpecParseError("Missing required field 'behaviors'")
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("Light spec parse error")
  |> should.be_true

  formatted
  |> string.contains("behaviors")
  |> should.be_true
}

// ============================================================================
// Export Spec JSON Tests
// ============================================================================

pub fn loader_export_nonexistent_file_test() {
  let result = loader.export_spec_json("/nonexistent/spec.cue")

  case result {
    Error(FileNotFound(path)) -> {
      path
      |> should.equal("/nonexistent/spec.cue")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// Edge Cases
// ============================================================================

pub fn loader_format_error_multiline_message_test() {
  // Error message with multiple lines
  let error =
    CueValidationError(
      "error1: field 'name' is required\nerror2: field 'version' is required",
    )
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("error1")
  |> should.be_true

  formatted
  |> string.contains("error2")
  |> should.be_true
}

pub fn loader_format_error_empty_message_test() {
  // Empty error message
  let error = CueValidationError("")
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("CUE validation failed")
  |> should.be_true
}

pub fn loader_format_error_special_characters_test() {
  // Error message with special characters
  let error = CueValidationError("expected string, got <nil>")
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("<nil>")
  |> should.be_true
}

pub fn loader_format_error_unicode_path_test() {
  // File path with unicode characters
  let error = FileNotFound("/home/user/specs/测试.cue")
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("测试.cue")
  |> should.be_true
}

pub fn loader_format_error_path_with_spaces_test() {
  // File path with spaces
  let error = FileNotFound("/home/user/my specs/test spec.cue")
  let formatted = loader.format_error(error)

  formatted
  |> string.contains("my specs")
  |> should.be_true
}

// ============================================================================
// load_spec_quiet Tests (intent-cli-clm.2)
// ============================================================================

pub fn loader_load_spec_quiet_file_not_found_test() {
  // load_spec_quiet should work without spinner UI
  let result = loader.load_spec_quiet("/nonexistent/path/to/spec.cue")

  case result {
    Error(FileNotFound(path)) -> {
      path
      |> should.equal("/nonexistent/path/to/spec.cue")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// validate_spec Tests (intent-cli-a6u)
// Tests for the new validate_spec function that runs full parsing
// ============================================================================

pub fn loader_validate_spec_nonexistent_file_test() {
  // validate_spec should return FileNotFound for missing files
  let result = loader.validate_spec("/nonexistent/spec.cue")

  case result {
    Error(FileNotFound(path)) -> {
      path
      |> should.equal("/nonexistent/spec.cue")
    }
    _ -> should.fail()
  }
}

pub fn loader_validate_spec_empty_path_test() {
  // Empty path should fail validation
  let result = loader.validate_spec("")

  case result {
    Error(FileNotFound(_)) -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn loader_load_spec_quiet_empty_path_test() {
  // Empty path should fail without spinner
  let result = loader.load_spec_quiet("")

  case result {
    Error(FileNotFound(_)) -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}

pub fn loader_load_spec_quiet_directory_test() {
  // Directory instead of file should fail without spinner
  let result = loader.load_spec_quiet("/tmp")

  case result {
    Error(FileNotFound(_)) -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }
}
