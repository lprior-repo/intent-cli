/// Security utilities for input validation and sanitization
import gleam/list
import gleam/regexp
import gleam/string
import simplifile

/// Error types for security validation
pub type SecurityError {
  PathTraversalAttempt(path: String)
  InvalidPath(path: String, reason: String)
  FileNotAccessible(path: String)
  UnsafeRegexPattern(pattern: String, reason: String)
  ShellMetacharactersDetected(path: String)
}

/// Check if a path contains only safe characters to prevent command injection
///
/// Only allows alphanumeric characters, forward slashes, underscores, dots, and hyphens.
/// This prevents shell metacharacters like ; | & $ ` from being injected.
///
/// # Example
/// ```gleam
/// is_safe_path("examples/api.cue") // True
/// is_safe_path("; rm -rf /") // False
/// is_safe_path("$(whoami).cue") // False
/// ```
pub fn is_safe_path(path: String) -> Bool {
  case regexp.from_string("^[a-zA-Z0-9/_.-]+$") {
    Ok(pattern) -> regexp.check(pattern, path)
    Error(_) -> False
  }
}

/// Validate a file path to prevent path traversal attacks
///
/// Checks:
/// - Path does not contain ".." (parent directory references)
/// - URL-encoded traversal sequences (%2e%2e, %2f, %5c)
/// - Backslash variants (..\, \..\)
/// - Null byte injection (%00)
/// - Double-encoded sequences (%252e)
/// - Alternative dot representations (....)
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
  // First check for shell metacharacters to prevent command injection
  case is_safe_path(path) {
    False -> Error(ShellMetacharactersDetected(path))
    True -> {
      // Normalize to lowercase for case-insensitive checks
      let path_lower = string.lowercase(path)

      // Check for literal path traversal
      case string.contains(path, "..") {
        True -> Error(PathTraversalAttempt(path))
        False -> {
          // Check for URL-encoded dot sequences
          // %2e = . (dot)
          // %2f = / (forward slash)
          // %5c = \ (backslash)
          case
            string.contains(path_lower, "%2e")
            || string.contains(path_lower, "%2f")
            || string.contains(path_lower, "%5c")
          {
            True -> Error(PathTraversalAttempt(path))
            False -> {
              // Check for backslash path traversal (Windows-style)
              case
                string.contains(path, "..\\\\")
                || string.contains(path, "\\\\..")
              {
                True -> Error(PathTraversalAttempt(path))
                False -> {
                  // Check for null byte injection (URL-encoded only)
                  case string.contains(path_lower, "%00") {
                    True -> Error(PathTraversalAttempt(path))
                    False -> {
                      // Check for double-encoded sequences
                      // %25 = % (percent sign, used for double encoding)
                      case string.contains(path_lower, "%25") {
                        True -> Error(PathTraversalAttempt(path))
                        False -> {
                          // Check for alternative dot representations
                          // .... can be interpreted as .. in some parsers
                          case string.contains(path, "....") {
                            True -> Error(PathTraversalAttempt(path))
                            False -> {
                              // Verify file exists
                              case simplifile.verify_is_file(path) {
                                Ok(True) -> Ok(path)
                                Ok(False) ->
                                  Error(InvalidPath(path, "Not a regular file"))
                                Error(_) -> Error(FileNotAccessible(path))
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Validate a regex pattern to prevent ReDoS (Regular Expression Denial of Service) attacks
///
/// Checks for known dangerous patterns that can cause exponential backtracking:
/// - Nested quantifiers like (.+)+ or ([^)]*)+
/// - Multiple overlapping quantifiers
/// - Catastrophic backtracking patterns
///
/// This is a basic check - it won't catch all ReDoS patterns but prevents the most common ones.
///
/// # Example
/// ```gleam
/// case validate_regex_pattern("^[a-z]+$") {
///   Ok(pattern) -> compile_regex(pattern)
///   Error(UnsafeRegexPattern(_, reason)) -> Error(reason)
/// }
/// ```
pub fn validate_regex_pattern(pattern: String) -> Result(String, SecurityError) {
  // List of dangerous regex patterns that can cause ReDoS
  let dangerous_patterns = [
    // Nested quantifiers
    "(.+)+", "(.*)\\+", "(.*)*", "([^)]*)+", "([^(]+)+", "(\\w+)+", "(\\d+)+",
    "(\\s+)+", "(.+)+$", "^(.+)+",
    // Multiple overlapping quantifiers
    ".*.*", ".+.+",
    // Alternation with overlapping patterns
    "(a+)+", "(a*)*",
  ]

  // Check if pattern contains any dangerous constructs
  let has_danger =
    list.any(dangerous_patterns, fn(dangerous) {
      string.contains(pattern, dangerous)
    })

  case has_danger {
    True ->
      Error(UnsafeRegexPattern(
        pattern,
        "Pattern contains potentially unsafe construct that could cause ReDoS (exponential backtracking)",
      ))
    False -> Ok(pattern)
  }
}

/// Format security error for display
pub fn format_security_error(error: SecurityError) -> String {
  case error {
    PathTraversalAttempt(path) ->
      "Security error: Path traversal attempt detected in '"
      <> path
      <> "'. Paths cannot contain '..' references or encoded variants (%2e, %2f, %5c, %25, %00, etc.)."
    InvalidPath(path, reason) ->
      "Security error: Invalid path '" <> path <> "': " <> reason
    FileNotAccessible(path) ->
      "Security error: File '"
      <> path
      <> "' is not accessible or does not exist."
    UnsafeRegexPattern(pattern, reason) ->
      "Security error: Unsafe regex pattern '"
      <> pattern
      <> "': "
      <> reason
      <> "\nFor security, patterns with nested quantifiers are not allowed."
    ShellMetacharactersDetected(path) ->
      "Security error: Invalid file path '"
      <> path
      <> "'. Path contains shell metacharacters. Only alphanumeric characters, forward slashes, underscores, dots, and hyphens are allowed."
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
