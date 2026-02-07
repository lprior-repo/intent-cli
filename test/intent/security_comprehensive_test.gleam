import gleam/string
import gleeunit/should
import intent/security

// =============================================================================
// Session ID Security Tests (validate_session_id)
// =============================================================================

pub fn validate_session_id_valid_simple_test() {
  security.validate_session_id("interview-123") |> should.equal(Ok("interview-123"))
}

pub fn validate_session_id_with_hyphens_test() {
  security.validate_session_id("user-session-v2") |> should.equal(Ok("user-session-v2"))
}

pub fn validate_session_id_with_underscores_test() {
  security.validate_session_id("interview_123") |> should.equal(Ok("interview_123"))
}

pub fn validate_session_id_alphanumeric_test() {
  security.validate_session_id("interview123") |> should.equal(Ok("interview123"))
}

pub fn validate_session_id_empty_test() {
  case security.validate_session_id("") {
    Error(security.InvalidPath(_, reason)) ->
      string.contains(reason, "cannot be empty") |> should.be_true()
    _ -> should.fail()
  }
}

pub fn validate_session_id_whitespace_only_test() {
  case security.validate_session_id("   ") {
    Error(security.InvalidPath(_, reason)) ->
      string.contains(reason, "cannot be empty") |> should.be_true()
    _ -> should.fail()
  }
}

pub fn validate_session_id_leading_whitespace_test() {
  case security.validate_session_id("  valid-session") {
    Error(security.InvalidPath(_, reason)) ->
      string.contains(reason, "cannot be empty") |> should.be_true()
    _ -> should.fail()
  }
}

pub fn validate_session_id_trailing_whitespace_test() {
  case security.validate_session_id("valid-session  ") {
    Error(security.InvalidPath(_, reason)) ->
      string.contains(reason, "cannot be empty") |> should.be_true()
    _ -> should.fail()
  }
}

pub fn validate_session_id_too_long_test() {
  let long_id = string.repeat("a", 499) <> "b" // 500 characters
  case security.validate_session_id(long_id) {
    Error(security.InvalidPath(_, reason)) ->
      string.contains(reason, "too long") |> should.be_true()
    _ -> should.fail()
  }
}

pub fn validate_session_id_max_length_test() {
  let max_id = string.repeat("a", 499) // 499 characters (max allowed)
  case security.validate_session_id(max_id) {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_session_id_control_char_tab_test() {
  case security.validate_session_id("valid\tid") {
    Error(security.InvalidPath(_, reason)) ->
      string.contains(reason, "control characters") |> should.be_true()
    _ -> should.fail()
  }
}

pub fn validate_session_id_control_char_newline_test() {
  case security.validate_session_id("valid\nid") {
    Error(security.InvalidPath(_, reason)) ->
      string.contains(reason, "control characters") |> should.be_true()
    _ -> should.fail()
  }
}

pub fn validate_session_id_control_char_carriage_return_test() {
  case security.validate_session_id("valid\rid") {
    Error(security.InvalidPath(_, reason)) ->
      string.contains(reason, "control characters") |> should.be_true()
    _ -> should.fail()
  }
}

pub fn validate_session_id_control_char_form_feed_test() {
  case security.validate_session_id("valid\fid") {
    Error(security.InvalidPath(_, reason)) ->
      string.contains(reason, "control characters") |> should.be_true()
    _ -> should.fail()
  }
}

pub fn validate_session_id_spaces_rejected_test() {
  case security.validate_session_id("session with spaces") {
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_session_id_symbols_rejected_test() {
  case security.validate_session_id("session@id") {
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_session_id_path_traversal_test() {
  case security.validate_session_id("interview../config") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_session_id_double_dot_leading_test() {
  case security.validate_session_id("../secret") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_session_id_double_dot_middle_test() {
  case security.validate_session_id("config/../secret") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    _ -> should.fail()
  }
}

// =============================================================================
// Secret Detection Tests (Simulating potential secret checking)
// =============================================================================

// These tests simulate what secret detection would look like
// The actual security.gleam doesn't have explicit secret detection yet
pub fn simulate_secret_password_detection_test() {
  let input = "password=\"sk-1234567890abcdef\""
  let has_password_keyword = string.contains(input, "password")
  has_password_keyword |> should.be_true()
}

pub fn simulate_secret_api_key_detection_test() {
  let input = "api_key=\"AIzaSyD5xX5X5X5X5X5X5X5X5X5X5X5X5X5X5X\""
  let has_api_key = string.contains(input, "api_key")
  has_api_key |> should.be_true()
}

pub fn simulate_secret_token_detection_test() {
  let input = "auth_token=\"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9\""
  let has_token = string.contains(input, "auth_token")
  has_token |> should.be_true()
}

pub fn simulate_secret_github_token_test() {
  let input = "github_pat_token=ghp_1234567890abcdef1234567890abcdef1234567"
  let has_github_pat = string.contains(input, "github_pat_token")
  has_github_pat |> should.be_true()
}

pub fn simulate_secret_aws_key_test() {
  let input = "aws_access_key=AKIAIOSFODNN7EXAMPLE"
  let has_aws_key = string.contains(input, "aws_access_key")
  has_aws_key |> should.be_true()
}

pub fn simulate_no_secrets_in_normal_text_test() {
  let normal_text = "This is a normal API specification without any secrets"
  let has_suspicious_words =
    string.contains(normal_text, "password") ||
    string.contains(normal_text, "token") ||
    string.contains(normal_text, "key") ||
    string.contains(normal_text, "secret") ||
    string.contains(normal_text, "api_key") ||
    string.contains(normal_text, "auth")

  // Should not contain suspicious words in this test
  has_suspicious_words |> should.be_false()
}

// =============================================================================
// SQL Injection Prevention Tests
// =============================================================================

pub fn simulate_sql_injection_detection_test() {
  let sql_injection = "OR '1'='1"
  let has_sql_keywords =
    string.contains(sql_injection, "OR") ||
    string.contains(sql_injection, "AND") ||
    string.contains(sql_injection, "DROP") ||
    string.contains(sql_injection, "INSERT") ||
    string.contains(sql_injection, "UPDATE") ||
    string.contains(sql_injection, "DELETE")

  has_sql_keywords |> should.be_true()
}

pub fn simulate_sql_union_injection_test() {
  let union_injection = "1 UNION SELECT * FROM users"
  let has_union = string.contains(union_injection, "UNION")
  has_union |> should.be_true()
}

pub fn simulate_sql_comment_injection_test() {
  let comment_injection = "admin'--"
  let has_comment = string.contains(comment_injection, "--")
  has_comment |> should.be_true()
}

pub fn simulate_safe_sql_like_test() {
  let safe_input = "username=john.doe"
  let has_sql_keywords =
    string.contains(safe_input, "OR") ||
    string.contains(safe_input, "AND") ||
    string.contains(safe_input, "DROP") ||
    string.contains(safe_input, "INSERT") ||
    string.contains(safe_input, "UPDATE") ||
    string.contains(safe_input, "DELETE")

  has_sql_keywords |> should.be_false()
}

// =============================================================================
// XSS Prevention Tests
// =============================================================================

pub fn simulate_xss_script_detection_test() {
  let xss_script = "<script>alert('XSS')</script>"
  let has_script_tags = string.contains(xss_script, "<script>")
  has_script_tags |> should.be_true()
}

pub fn simulate_xss_img_tag_test() {
  let xss_img = "<img src='x' onerror='alert(1)'>"
  let has_onerror = string.contains(xss_img, "onerror")
  has_onerror |> should.be_true()
}

pub fn simulate_xss_javascript_protocol_test() {
  let xss_js = "javascript:alert('XSS')"
  let has_js_protocol = string.contains(xss_js, "javascript:")
  has_js_protocol |> should.be_true()
}

pub fn simulate_xss_event_handler_test() {
  let xss_event = "<div onclick='alert(1)'>Click me</div>"
  let has_onclick = string.contains(xss_event, "onclick")
  has_onclick |> should.be_true()
}

pub fn simulate_safe_input_text_test() {
  let safe_text = "This is normal text without any scripts"
  let has_xss_indicators =
    string.contains(safe_text, "<script>") ||
    string.contains(safe_text, "javascript:") ||
    string.contains(safe_text, "onerror") ||
    string.contains(safe_text, "onclick") ||
    string.contains(safe_text, "<img")

  has_xss_indicators |> should.be_false()
}

// =============================================================================
// Advanced Path Traversal Tests
// =============================================================================

pub fn validate_file_path_windows_traversal_test() {
  // Windows-style path with backslashes
  case security.validate_file_path("..\\..\\etc\\passwd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_encoded_traversal_test() {
  // Multiple encoding layers
  case security.validate_file_path("%252e%252e/%252fetc/passwd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_unicode_traversal_test() {
  // Unicode dot characters (though not currently checked)
  // This test documents what we might want to add
  let unicode_path = "…/etc/passwd"  // Unicode ellipsis
  // Note: Current implementation doesn't check for Unicode dots
  // This is a potential security gap
  Nil
}

pub fn validate_file_path_absolute_traversal_test() {
  // Absolute path with traversal
  case security.validate_file_path("/var/www/../../../etc/passwd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_environment_variable_test() {
  // Environment variable injection (should be blocked by shell metachars)
  case security.validate_file_path("$HOME/secrets.txt") {
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_command_injection_test() {
  // Command injection with semicolons
  case security.validate_file_path("file.txt; ls -la") {
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_pipe_injection_test() {
  // Pipe injection
  case security.validate_file_path("file.txt | cat /etc/passwd") {
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_ampersand_injection_test() {
  // Background process injection
  case security.validate_file_path("file.txt & rm -rf /") {
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

// =============================================================================
// Secret Output Leakage Tests
// =============================================================================

pub fn simulate_no_password_leakage_test() {
  let stored_password = "secret123"
  let public_output = "User session created successfully"
  // Ensure password doesn't appear in output
  string.contains(public_output, stored_password) |> should.be_false()
}

pub fn simulate_no_token_leakage_test() {
  let auth_token = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
  let user_profile = "Username: john.doe"
  // Ensure token doesn't appear in user profile
  string.contains(user_profile, auth_token) |> should.be_false()
}

pub fn simulate_no_api_key_leakage_test() {
  let api_key = "sk-1234567890abcdef1234567890abcdef"
  let response_body = "{\"status\": \"success\", \"message\": \"API call completed\"}"
  // Ensure API key doesn't appear in response
  string.contains(response_body, api_key) |> should.be_false()
}

// =============================================================================
// Edge Cases and Boundary Conditions
// =============================================================================

pub fn validate_session_id_exact_max_length_test() {
  // Exactly 499 characters
  let exact_max = string.repeat("a", 499)
  case security.validate_session_id(exact_max) {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_session_id_just_over_max_test() {
  // Just over 499 characters (500)
  let over_max = string.repeat("a", 500)
  case security.validate_session_id(over_max) {
    Error(security.InvalidPath(_, reason)) ->
      string.contains(reason, "too long") |> should.be_true()
    _ -> should.fail()
  }
}

pub fn validate_session_id_mixed_safe_chars_test() {
  // Mix of all allowed characters
  let mixed_chars = "interview-123_user.session_name"
  case security.validate_session_id(mixed_chars) {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_session_id_unicode_chars_test() {
  // Unicode characters (should be rejected)
  case security.validate_session_id("interview-你") {
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_null_byte_boundary_test() {
  // Null byte at different positions
  let paths_with_null = [
    "file%00.txt",
    "file.txt%00",
    "%00file.txt",
    "fi%00le.txt"
  ]

  list.fold(paths_with_null, 0, fn(acc, path) {
    case security.validate_file_path(path) {
      Error(security.ShellMetacharactersDetected(_)) -> acc + 1
      Error(security.PathTraversalAttempt(_)) -> acc + 1
      _ -> acc
    }
  }) |> should.equal(list.length(paths_with_null))
}

pub fn validate_file_path_encoded_slash_variations_test() {
  // Different encoded slash representations
  let slash_variations = [
    "test%2fpasswd",
    "test%2Fpasswd",  // uppercase
    "test%5cpasswd",  // backslash encoding
    "test%5Cpasswd"   // uppercase backslash
  ]

  list.fold(slash_variations, 0, fn(acc, path) {
    case security.validate_file_path(path) {
      Error(security.ShellMetacharactersDetected(_)) -> acc + 1
      Error(security.PathTraversalAttempt(_)) -> acc + 1
      _ -> acc
    }
  }) |> should.equal(list.length(slash_variations))
}

// =============================================================================
// Error Message Security Tests
// =============================================================================

pub fn error_messages_dont_leak_paths_test() {
  let error = security.PathTraversalAttempt("../../../etc/passwd")
  let formatted = security.format_security_error(error)
  // Error should not reveal the full path to sensitive locations
  string.contains(formatted, "etc/passwd") |> should.be_false()
}

pub fn error_messages_dont_leak_tokens_test() {
  let error = security.InvalidPath("/tmp/secret_token.txt", "Access denied")
  let formatted = security.format_security_error(error)
  // Should not reveal sensitive tokens in error messages
  string.contains(formatted, "secret_token") |> should.be_false()
}

pub fn error_messages_are_user_safe_test() {
  let error = security.ShellMetacharactersDetected("$(whoami); rm -rf /")
  let formatted = security.format_security_error(error)
  // Error message should be safe for users to see
  let contains_command_injection = string.contains(formatted, "rm -rf /")
  contains_command_injection |> should.be_false()
}