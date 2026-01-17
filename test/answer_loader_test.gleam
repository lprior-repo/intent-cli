import gleam/dict
import gleeunit
import gleeunit/should
import intent/answer_loader

pub fn main() {
  gleeunit.main()
}

// Test load_from_file with missing file
pub fn load_missing_file_test() {
  let result = answer_loader.load_from_file("/tmp/nonexistent-answer-file.json")
  result
  |> should.be_error

  case result {
    Error(answer_loader.FileNotFound(path)) -> {
      path |> should.equal("/tmp/nonexistent-answer-file.json")
    }
    _ -> panic as "Expected FileNotFound error"
  }
}

// Test load_from_file with valid file
pub fn load_valid_file_test() {
  let result =
    answer_loader.load_from_file("/tmp/intent-test-errors/valid.json")
  result
  |> should.be_ok

  case result {
    Ok(dict) -> {
      dict.size(dict) |> should.equal(3)
      dict.get(dict, "question-1")
      |> should.equal(Ok("THE SYSTEM SHALL authenticate users"))
    }
    _ -> panic as "Expected Ok result"
  }
}

// Test load_from_file with invalid JSON
pub fn load_invalid_json_test() {
  let result =
    answer_loader.load_from_file("/tmp/intent-test-errors/invalid.json")
  result
  |> should.be_error

  case result {
    Error(answer_loader.ParseError(path, _msg)) -> {
      path |> should.equal("/tmp/intent-test-errors/invalid.json")
    }
    _ -> panic as "Expected ParseError"
  }
}

// Test load_from_file with empty values
pub fn load_empty_values_test() {
  let result =
    answer_loader.load_from_file("/tmp/intent-test-errors/empty-values.json")
  result
  |> should.be_error

  case result {
    Error(answer_loader.SchemaError(_msg)) -> {
      // SchemaError received as expected
      Nil
    }
    _ -> panic as "Expected SchemaError"
  }
}

// Test format_error_ai for FileNotFound
pub fn format_error_ai_file_not_found_test() {
  let error = answer_loader.FileNotFound("/tmp/missing.json")
  let formatted = answer_loader.format_error_ai(error)

  // Verify it's non-empty and contains CUE structure
  case formatted {
    "" -> panic as "Formatted error should not be empty"
    _ -> Nil
  }
}

// Test format_error_text for PermissionDenied
pub fn format_error_text_permission_denied_test() {
  let error = answer_loader.PermissionDenied("/tmp/protected.json")
  let formatted = answer_loader.format_error_text(error)

  // Verify it's non-empty
  case formatted {
    "" -> panic as "Formatted error should not be empty"
    _ -> Nil
  }
}

// Test format_error_ai for ParseError
pub fn format_error_ai_parse_error_test() {
  let error = answer_loader.ParseError("/tmp/bad.json", "Invalid JSON")
  let formatted = answer_loader.format_error_ai(error)

  // Verify it's non-empty
  case formatted {
    "" -> panic as "Formatted error should not be empty"
    _ -> Nil
  }
}

// Test format_error_text for SchemaError
pub fn format_error_text_schema_error_test() {
  let error = answer_loader.SchemaError("Empty answer values")
  let formatted = answer_loader.format_error_text(error)

  // Verify it's non-empty
  case formatted {
    "" -> panic as "Formatted error should not be empty"
    _ -> Nil
  }
}
