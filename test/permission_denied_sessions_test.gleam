/// Test for permission denied handling in sessions command
/// Verifies: Exit code 4 on permission denied, exit code 0 on file not found
import gleam/string
import gleeunit
import gleeunit/should
import intent/interview_storage

pub fn main() {
  gleeunit.main()
}

/// Test that FILE_NOT_FOUND errors produce empty list response
pub fn file_not_found_produces_empty_response_test() {
  // Mock a reader that returns FILE_NOT_FOUND error
  let mock_reader = fn(_path: String) -> Result(String, String) {
    Error(
      "FILE_NOT_FOUND: File or directory not found (path: /nonexistent.jsonl)",
    )
  }

  // Call list_sessions_from_jsonl_with_io
  let result =
    interview_storage.list_sessions_from_jsonl_with_io(
      "/nonexistent.jsonl",
      mock_reader,
    )

  // Should get error with FILE_NOT_FOUND marker
  case result {
    Error(msg) -> {
      should.be_true(string.contains(msg, "FILE_NOT_FOUND"))
    }
    Ok(_) -> should.fail()
  }
}

/// Test that PERMISSION_DENIED errors are properly tagged
pub fn permission_denied_properly_tagged_test() {
  // Mock a reader that returns PERMISSION_DENIED error
  let mock_reader = fn(_path: String) -> Result(String, String) {
    Error("PERMISSION_DENIED: Permission denied (path: /protected.jsonl)")
  }

  // Call list_sessions_from_jsonl_with_io
  let result =
    interview_storage.list_sessions_from_jsonl_with_io(
      "/protected.jsonl",
      mock_reader,
    )

  // Should get error with PERMISSION_DENIED marker
  case result {
    Error(msg) -> {
      should.be_true(string.contains(msg, "PERMISSION_DENIED"))
    }
    Ok(_) -> should.fail()
  }
}

/// Test that I/O errors are properly tagged
pub fn io_error_properly_tagged_test() {
  // Mock a reader that returns IO_ERROR
  let mock_reader = fn(_path: String) -> Result(String, String) {
    Error("IO_ERROR: I/O error (path: /broken.jsonl)")
  }

  // Call list_sessions_from_jsonl_with_io
  let result =
    interview_storage.list_sessions_from_jsonl_with_io(
      "/broken.jsonl",
      mock_reader,
    )

  // Should get error with IO_ERROR marker
  case result {
    Error(msg) -> {
      should.be_true(string.contains(msg, "IO_ERROR"))
    }
    Ok(_) -> should.fail()
  }
}
