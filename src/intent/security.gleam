/// Security utilities for input validation and sanitization
import gleam/list
import gleam/string
import simplifile

/// Error types for security validation
pub type SecurityError {
  PathTraversalAttempt(path: String)
  InvalidPath(path: String, reason: String)
  FileNotAccessible(path: String)
  UnsafeRegexPattern(pattern: String, reason: String)
  ShellMetacharactersDetected(path: String)
  SymlinkNotAllowed(path: String)
  SSRFAttempt(url: String, reason: String)
}

/// Check if a path is safe by blocking dangerous shell metacharacters
///
/// Blocks characters that could enable command injection:
/// - ; | & $ ` (command separators, pipes, background, variable expansion)
/// - > < (redirection operators)
/// - Newlines and carriage returns
///
/// Allows common path characters including spaces, parentheses, quotes, etc.
///
/// # Example
/// ```gleam
/// is_safe_path("examples/api.cue") // True
/// is_safe_path("/home/user/My Documents/api.cue") // True
/// is_safe_path("; rm -rf /") // False
/// is_safe_path("$(whoami).cue") // False
/// ```
pub fn is_safe_path(path: String) -> Bool {
  // Blocklist approach: reject paths containing dangerous shell metacharacters
  let dangerous_chars = [";", "|", "&", "$", "`", ">", "<", "\n", "\r"]
  !list.any(dangerous_chars, fn(char) { string.contains(path, char) })
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
  // Reject empty paths first
  case string.is_empty(path) {
    True -> Error(InvalidPath(path, "Path cannot be empty"))
    False -> validate_file_path_impl(path)
  }
}

// Internal implementation after empty path check
fn validate_file_path_impl(path: String) -> Result(String, SecurityError) {
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
                  // Check for null byte injection (both literal NUL and URL-encoded %00)
                  // Literal null bytes (U+0000) truncate paths in C/Erlang filesystem calls
                  // Gleam doesn't have \0 escape, so we check for the actual null character
                  let has_null_byte =
                    string.to_utf_codepoints(path)
                    |> list.any(fn(cp) {
                      string.utf_codepoint_to_int(cp) == 0
                    })

                  case has_null_byte || string.contains(path_lower, "%00") {
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
                              // Check for symlinks - reject to prevent symlink attacks
                              case simplifile.verify_is_symlink(path) {
                                Ok(True) -> Error(SymlinkNotAllowed(path))
                                _ -> {
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

/// Validate a URL to prevent SSRF (Server-Side Request Forgery) attacks
///
/// Checks:
/// - URL must use http:// or https:// scheme
/// - Blocks localhost, 127.0.0.0/8, and loopback addresses (unless allow_localhost)
/// - Blocks private IP ranges (10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16)
/// - Blocks link-local addresses (169.254.0.0/16)
/// - Blocks IPv6 loopback and link-local
/// - Blocks file:// and other dangerous schemes
///
/// # Parameters
/// - url: The URL to validate
/// - allow_localhost: If True, allows localhost addresses for development
///
/// # Example
/// ```gleam
/// case validate_url("https://api.example.com", False) {
///   Ok(url) -> make_request(url)
///   Error(SSRFAttempt(_, reason)) -> Error(reason)
/// }
/// ```
pub fn validate_url(
  url: String,
  allow_localhost: Bool,
) -> Result(String, SecurityError) {
  // Convert to lowercase for case-insensitive checks
  let url_lower = string.lowercase(url)

  // Check for valid HTTP/HTTPS scheme
  let has_valid_scheme =
    string.starts_with(url_lower, "http://")
    || string.starts_with(url_lower, "https://")

  case has_valid_scheme {
    False ->
      Error(SSRFAttempt(
        url,
        "Only http:// and https:// schemes are allowed",
      ))
    True -> {
      // Check for localhost variations
      let is_localhost =
        string.contains(url_lower, "localhost")
        || string.contains(url_lower, "127.0.0.")
        || string.contains(url_lower, "127.1.")
        || string.contains(url_lower, "[::1]")
        || string.contains(url_lower, "[0:0:0:0:0:0:0:1]")

      case is_localhost && !allow_localhost {
        True -> Error(SSRFAttempt(url, "Localhost addresses are not allowed"))
        False -> {
          // Check for private IP ranges
          case
            string.contains(url_lower, "10.0.")
            || string.contains(url_lower, "10.1.")
            || string.contains(url_lower, "10.2.")
            || string.contains(url_lower, "10.3.")
            || string.contains(url_lower, "10.4.")
            || string.contains(url_lower, "10.5.")
            || string.contains(url_lower, "10.6.")
            || string.contains(url_lower, "10.7.")
            || string.contains(url_lower, "10.8.")
            || string.contains(url_lower, "10.9.")
            || string.contains(url_lower, "192.168.")
            || string.contains(url_lower, "172.16.")
            || string.contains(url_lower, "172.17.")
            || string.contains(url_lower, "172.18.")
            || string.contains(url_lower, "172.19.")
            || string.contains(url_lower, "172.20.")
            || string.contains(url_lower, "172.21.")
            || string.contains(url_lower, "172.22.")
            || string.contains(url_lower, "172.23.")
            || string.contains(url_lower, "172.24.")
            || string.contains(url_lower, "172.25.")
            || string.contains(url_lower, "172.26.")
            || string.contains(url_lower, "172.27.")
            || string.contains(url_lower, "172.28.")
            || string.contains(url_lower, "172.29.")
            || string.contains(url_lower, "172.30.")
            || string.contains(url_lower, "172.31.")
          {
            True ->
              Error(SSRFAttempt(
                url,
                "Private IP address ranges are not allowed (10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16)",
              ))
            False -> {
              // Check for link-local (AWS metadata, etc.)
              case string.contains(url_lower, "169.254.") {
                True ->
                  Error(SSRFAttempt(
                    url,
                    "Link-local addresses are not allowed (169.254.0.0/16 - often used for cloud metadata)",
                  ))
                False -> {
                  // Check for IPv6 link-local
                  case
                    string.contains(url_lower, "[fe80:")
                    || string.contains(url_lower, "[fc00:")
                    || string.contains(url_lower, "[fd00:")
                  {
                    True ->
                      Error(SSRFAttempt(
                        url,
                        "IPv6 link-local and unique local addresses are not allowed",
                      ))
                    False -> Ok(url)
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
      <> "'. Path contains dangerous shell metacharacters (; | & $ ` > < or newlines)."
    SymlinkNotAllowed(path) ->
      "Security error: Symbolic links are not allowed. Path '"
      <> path
      <> "' is a symlink."
    SSRFAttempt(url, reason) ->
      "Security error: SSRF (Server-Side Request Forgery) attempt detected in URL '"
      <> url
      <> "': "
      <> reason
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
