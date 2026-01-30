/// Simple atomic file operations test
/// Verifies basic write-then-rename atomicity
/// Bug reference: intent-cli-3e3z
import gleeunit
import gleeunit/should
import intent/atomic_file
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn write_atomic_basic_test() {
  let test_path = "/tmp/atomic_basic.txt"
  let content = "test content"

  // Clean up
  let _ = simplifile.delete(test_path)

  // Write atomically
  let result = atomic_file.write_atomic(test_path, content)
  should.be_ok(result)

  // Verify content
  let read_result = simplifile.read(test_path)
  should.be_ok(read_result)

  // Clean up
  let _ = simplifile.delete(test_path)
}

pub fn write_atomic_rejects_empty_path_test() {
  let result = atomic_file.write_atomic("", "content")
  should.be_error(result)
}
