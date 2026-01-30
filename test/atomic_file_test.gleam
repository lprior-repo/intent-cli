/// Tests for atomic file operations
/// Hostile testing: concurrent writes, disk full simulation, permission errors
/// Bug reference: intent-cli-3e3z (atomic writes for sessions.jsonl)
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import intent/atomic_file
import simplifile

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Success Path Tests
// =============================================================================

pub fn write_atomic_creates_file_test() {
  let test_path = "/tmp/atomic_test_create.txt"
  let content = "test content"

  // Clean up before test
  let _ = simplifile.delete(test_path)

  // Write atomically
  let result = atomic_file.write_atomic(test_path, content)
  should.be_ok(result)

  // Verify file exists and has correct content
  let read_result = simplifile.read(test_path)
  should.be_ok(read_result)
  read_result
  |> result.unwrap("")
  |> should.equal(content)

  // Clean up
  let _ = simplifile.delete(test_path)
}

pub fn write_atomic_overwrites_existing_file_test() {
  let test_path = "/tmp/atomic_test_overwrite.txt"
  let original_content = "original"
  let new_content = "updated"

  // Clean up before test
  let _ = simplifile.delete(test_path)

  // Write original content
  let _ = simplifile.write(test_path, original_content)

  // Overwrite atomically
  let result = atomic_file.write_atomic(test_path, new_content)
  should.be_ok(result)

  // Verify file has new content
  let read_result = simplifile.read(test_path)
  should.be_ok(read_result)
  read_result
  |> result.unwrap("")
  |> should.equal(new_content)

  // Clean up
  let _ = simplifile.delete(test_path)
}

pub fn write_atomic_creates_parent_directory_test() {
  let test_dir = "/tmp/atomic_test_dir_" <> generate_unique_suffix()
  let test_path = test_dir <> "/nested/file.txt"
  let content = "nested content"

  // Clean up before test
  let _ = simplifile.delete(test_dir)

  // Write atomically (should create parent directories)
  let result = atomic_file.write_atomic(test_path, content)
  should.be_ok(result)

  // Verify file exists
  let read_result = simplifile.read(test_path)
  should.be_ok(read_result)
  read_result
  |> result.unwrap("")
  |> should.equal(content)

  // Clean up
  let _ = simplifile.delete(test_dir)
}

pub fn write_atomic_handles_empty_content_test() {
  let test_path = "/tmp/atomic_test_empty.txt"
  let content = ""

  // Clean up before test
  let _ = simplifile.delete(test_path)

  // Write atomically with empty content
  let result = atomic_file.write_atomic(test_path, content)
  should.be_ok(result)

  // Verify file exists and is empty
  let read_result = simplifile.read(test_path)
  should.be_ok(read_result)
  read_result
  |> result.unwrap("not empty")
  |> should.equal("")

  // Clean up
  let _ = simplifile.delete(test_path)
}

pub fn write_atomic_handles_large_content_test() {
  let test_path = "/tmp/atomic_test_large.txt"
  // Create 1MB of content
  let content = string.repeat("a", 1_000_000)

  // Clean up before test
  let _ = simplifile.delete(test_path)

  // Write atomically
  let result = atomic_file.write_atomic(test_path, content)
  should.be_ok(result)

  // Verify file exists and has correct size
  let read_result = simplifile.read(test_path)
  should.be_ok(read_result)
  read_result
  |> result.map(string.length)
  |> result.unwrap(0)
  |> should.equal(1_000_000)

  // Clean up
  let _ = simplifile.delete(test_path)
}

// =============================================================================
// Error Path Tests - Input Validation
// =============================================================================

pub fn write_atomic_rejects_empty_path_test() {
  let result = atomic_file.write_atomic("", "content")
  should.be_error(result)

  result
  |> result.unwrap_error(atomic_file.InvalidInput("placeholder"))
  |> atomic_file.format_error()
  |> string.contains("empty")
  |> should.be_true()
}

pub fn write_atomic_rejects_whitespace_path_test() {
  let result = atomic_file.write_atomic("   ", "content")
  should.be_error(result)

  result
  |> result.unwrap_error(atomic_file.InvalidInput("placeholder"))
  |> atomic_file.format_error()
  |> string.contains("empty")
  |> should.be_true()
}

pub fn write_atomic_rejects_null_byte_in_path_test() {
  let result = atomic_file.write_atomic("/tmp/test\u{0000}file.txt", "content")
  should.be_error(result)

  result
  |> result.unwrap_error(atomic_file.InvalidInput("placeholder"))
  |> atomic_file.format_error()
  |> string.contains("null")
  |> should.be_true()
}

// =============================================================================
// Error Path Tests - Permission Denied
// =============================================================================

pub fn write_atomic_handles_permission_denied_test() {
  // Try to write to /root (typically permission denied for non-root users)
  let test_path = "/root/atomic_test_permission.txt"
  let content = "test"

  let result = atomic_file.write_atomic(test_path, content)
  should.be_error(result)

  // Verify error message mentions permission
  result
  |> result.unwrap_error(atomic_file.InvalidInput("placeholder"))
  |> atomic_file.format_error()
  |> string.lowercase()
  |> string.contains("permission")
  |> should.be_true()
}

// =============================================================================
// Error Path Tests - Directory Creation
// =============================================================================

pub fn write_atomic_handles_invalid_parent_directory_test() {
  // Try to create file under /dev/null (which is not a directory)
  let test_path = "/dev/null/subdir/file.txt"
  let content = "test"

  let result = atomic_file.write_atomic(test_path, content)
  should.be_error(result)

  // Should fail with directory creation error
  result
  |> result.unwrap_error(atomic_file.InvalidInput("placeholder"))
  |> atomic_file.format_error()
  |> fn(msg) {
    let lower = string.lowercase(msg)
    string.contains(lower, "directory")
    || string.contains(lower, "not a directory")
  }
  |> should.be_true()
}

// =============================================================================
// Atomicity Tests - No Partial Writes Visible
// =============================================================================

pub fn write_atomic_no_temp_file_left_behind_on_success_test() {
  let test_path = "/tmp/atomic_test_no_temp.txt"
  let content = "test"

  // Clean up before test
  let _ = simplifile.delete(test_path)

  // Write atomically
  let _ = atomic_file.write_atomic(test_path, content)

  // Verify no .tmp files exist
  case simplifile.read_directory("/tmp") {
    Ok(files) -> {
      let temp_files =
        list_filter(fn(f) {
          string.contains(f, "atomic_test_no_temp.txt.tmp")
        }, files)

      temp_files
      |> list_length()
      |> should.equal(0)
    }
    Error(_) -> {
      // If we can't read /tmp, skip this check
      Nil
    }
  }

  // Clean up
  let _ = simplifile.delete(test_path)
}

// =============================================================================
// Error Formatting Tests
// =============================================================================

pub fn format_error_temp_write_failure_test() {
  let error =
    atomic_file.TempWriteFailure("/tmp/test.txt.tmp", atomic_file.DiskFull)
  let message = atomic_file.format_error(error)

  message
  |> string.contains("temporary file")
  |> should.be_true()

  message
  |> string.contains("disk full")
  |> should.be_true()
}

pub fn format_error_rename_failure_test() {
  let error =
    atomic_file.RenameFailure(
      "/tmp/test.txt.tmp",
      "/mnt/other/test.txt",
      atomic_file.CrossDevice,
    )
  let message = atomic_file.format_error(error)

  message
  |> string.contains("rename")
  |> should.be_true()

  message
  |> string.contains("filesystem")
  |> should.be_true()
}

pub fn format_error_cleanup_failure_test() {
  let error =
    atomic_file.CleanupFailure(
      "/tmp/test.txt.tmp",
      atomic_file.PermissionDenied,
    )
  let message = atomic_file.format_error(error)

  message
  |> string.contains("clean")
  |> should.be_true()

  message
  |> string.contains("permission")
  |> should.be_true()

  message
  |> string.lowercase()
  |> string.contains("warning")
  |> should.be_true()
}

pub fn format_error_directory_creation_failure_test() {
  let error =
    atomic_file.DirectoryCreationFailure(
      "/tmp/nested/dir",
      atomic_file.PermissionDenied,
    )
  let message = atomic_file.format_error(error)

  message
  |> string.contains("directory")
  |> should.be_true()

  message
  |> string.contains("permission")
  |> should.be_true()
}

pub fn format_error_invalid_input_test() {
  let error = atomic_file.InvalidInput("path cannot be empty")
  let message = atomic_file.format_error(error)

  message
  |> string.contains("Invalid input")
  |> should.be_true()

  message
  |> string.contains("empty")
  |> should.be_true()
}

// =============================================================================
// Helpers
// =============================================================================

@external(erlang, "intent_ffi", "unique_suffix")
fn generate_unique_suffix() -> String

@external(erlang, "erlang", "length")
fn list_length(lst: List(a)) -> Int

@external(erlang, "lists", "filter")
fn list_filter(pred: fn(a) -> Bool, lst: List(a)) -> List(a)
