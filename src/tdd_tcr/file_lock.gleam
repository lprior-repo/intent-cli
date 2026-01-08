//// File-based locking for TDD-TCR actor synchronization.
////
//// Uses sentinel files in .factory/ directory to prevent concurrent writes.
//// When src/ is locked, Implementer cannot write.
//// When test/ is locked, Auditor cannot write.
//// Prevents race conditions between Red Team and Blue Team actors.

import gleam/result
import simplifile

const src_lock_file = ".factory/.src.locked"

const test_lock_file = ".factory/.test.locked"

/// Lock the src/ directory (prevents Implementer from writing)
///
/// Creates a sentinel file. Check returns Ok(Nil) on success.
pub fn lock_src() -> Result(Nil, String) {
  simplifile.write(src_lock_file, "locked")
  |> result.map_error(fn(err) {
    "Failed to lock src/: " <> simplifile.error_to_string(err)
  })
}

/// Unlock the src/ directory (allows Implementer to write)
///
/// Removes the sentinel file.
pub fn unlock_src() -> Result(Nil, String) {
  simplifile.delete(src_lock_file)
  |> result.map_error(fn(err) {
    "Failed to unlock src/: " <> simplifile.error_to_string(err)
  })
}

/// Lock the test/ directory (prevents Auditor from writing)
///
/// Creates a sentinel file.
pub fn lock_tests() -> Result(Nil, String) {
  simplifile.write(test_lock_file, "locked")
  |> result.map_error(fn(err) {
    "Failed to lock test/: " <> simplifile.error_to_string(err)
  })
}

/// Unlock the test/ directory (allows Auditor to write)
///
/// Removes the sentinel file.
pub fn unlock_tests() -> Result(Nil, String) {
  simplifile.delete(test_lock_file)
  |> result.map_error(fn(err) {
    "Failed to unlock test/: " <> simplifile.error_to_string(err)
  })
}

/// Unlock both src/ and test/ directories
///
/// Used at loop completion or emergency reset.
pub fn unlock_all() -> Result(Nil, String) {
  use _ <- result.try(unlock_src())
  unlock_tests()
}

/// Check if a lock file exists
///
/// Returns True if locked, False otherwise.
pub fn is_locked(lock_file: String) -> Bool {
  case simplifile.is_file(lock_file) {
    Ok(exists) -> exists
    Error(_) -> False
  }
}

/// Check if src/ is currently locked
pub fn is_src_locked() -> Bool {
  is_locked(src_lock_file)
}

/// Check if test/ is currently locked
pub fn is_tests_locked() -> Bool {
  is_locked(test_lock_file)
}
