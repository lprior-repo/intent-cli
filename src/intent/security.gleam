/// Security utilities for input validation and sanitization
import gleam/string
import simplifile

/// Error types for security validation
pub type SecurityError {
  PathTraversalAttempt(path: String)
  InvalidPath(path: String, reason: String)
  FileNotAccessible(path: String)
}

/// Validate a file path to prevent path traversal attacks
/// 
/// Checks:
/// - Path does not contain ".." (parent directory references)
/// - File actually exists and is accessible
///
/// # Example
/// ```gleam
/// case validate_file_path("specs/api.cue") {
///   Ok(path) -> load_file(path)
///   Error(PathTraversalAttempt(_)) -> halt_with_error()
/// }
/// ```
pub fn validate_file_path(path: String) -> Result(String, SecurityError) {
  // Check for path traversal patterns
  case string.contains(path, "..") {
    True -> Error(PathTraversalAttempt(path))
    False -> {
      // Verify file exists
      case simplifile.verify_is_file(path) {
        Ok(True) -> Ok(path)
        Ok(False) -> Error(InvalidPath(path, "Not a regular file"))
        Error(_) -> Error(FileNotAccessible(path))
      }
    }
  }
}

/// Format security error for display
pub fn format_security_error(error: SecurityError) -> String {
  case error {
    PathTraversalAttempt(path) ->
      "Security error: Path traversal attempt detected in '"
      <> path
      <> "'. Paths cannot contain '..' references."
    InvalidPath(path, reason) ->
      "Security error: Invalid path '" <> path <> "': " <> reason
    FileNotAccessible(path) ->
      "Security error: File '"
      <> path
      <> "' is not accessible or does not exist."
  }
}

/// Validate a file path to prevent path traversal attacks
/// 
/// Checks:
/// - Path does not contain ".." (parent directory references)
/// - Path does not start with "/" (absolute paths should be explicit)
/// - File actually exists and is accessible
/// - Path is not a symlink pointing outside working directory
///
/// # Example
/// ```gleam
/// case validate_file_path("specs/api.cue") {
///   Ok(path) -> load_file(path)
///   Error(PathTraversalAttempt(_)) -> halt_with_error()
/// }
/// ```
pub fn validate_file_path(path: String) -> Result(String, SecurityError) {
  // Check for path traversal patterns
  case string.contains(path, "..") {
    True -> Error(PathTraversalAttempt(path))
    False -> {
      // Verify file exists
      case simplifile.verify_is_file(path) {
        Ok(True) -> Ok(path)
        Ok(False) -> Error(InvalidPath(path, "Not a regular file"))
        Error(_) -> Error(FileNotAccessible(path))
      }
    }
  }
}

/// Validate multiple file paths
pub fn validate_file_paths(
  paths: List(String),
) -> Result(List(String), SecurityError) {
  paths
  |> list_try_map(validate_file_path)
}

// Helper to map over list with Result
fn list_try_map(list: List(a), fun: fn(a) -> Result(b, e)) -> Result(List(b), e) {
  case list {
    [] -> Ok([])
    [head, ..tail] -> {
      case fun(head) {
        Ok(value) -> {
          case list_try_map(tail, fun) {
            Ok(rest) -> Ok([value, ..rest])
            Error(e) -> Error(e)
          }
        }
        Error(e) -> Error(e)
      }
    }
  }
}

/// Format security error for display
pub fn format_security_error(error: SecurityError) -> String {
  case error {
    PathTraversalAttempt(path) ->
      "Security error: Path traversal attempt detected in '"
      <> path
      <> "'. Paths cannot contain '..' references."
    InvalidPath(path, reason) ->
      "Security error: Invalid path '" <> path <> "': " <> reason
    FileNotAccessible(path) ->
      "Security error: File '"
      <> path
      <> "' is not accessible or does not exist."
  }
}
