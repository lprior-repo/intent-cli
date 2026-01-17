import gleam/string
import gleeunit/should
import intent/security

// =============================================================================
// Path Safety Tests (is_safe_path)
// =============================================================================

pub fn is_safe_path_valid_test() {
  security.is_safe_path("examples/api.cue") |> should.be_true()
}

pub fn is_safe_path_with_underscores_test() {
  security.is_safe_path("specs/user_api.cue") |> should.be_true()
}

pub fn is_safe_path_with_hyphens_test() {
  security.is_safe_path("specs/my-api-spec.cue") |> should.be_true()
}

pub fn is_safe_path_absolute_path_test() {
  security.is_safe_path("/home/user/specs/api.cue") |> should.be_true()
}

pub fn is_safe_path_nested_dirs_test() {
  security.is_safe_path("a/b/c/d/e/file.txt") |> should.be_true()
}

pub fn is_safe_path_rejects_semicolon_test() {
  security.is_safe_path("; rm -rf /") |> should.be_false()
}

pub fn is_safe_path_rejects_pipe_test() {
  security.is_safe_path("file.txt | cat") |> should.be_false()
}

pub fn is_safe_path_rejects_ampersand_test() {
  security.is_safe_path("file.txt & ls") |> should.be_false()
}

pub fn is_safe_path_rejects_backtick_test() {
  security.is_safe_path("`whoami`.txt") |> should.be_false()
}

pub fn is_safe_path_rejects_dollar_test() {
  security.is_safe_path("$(whoami).cue") |> should.be_false()
}

pub fn is_safe_path_rejects_redirection_test() {
  security.is_safe_path("file.txt > output") |> should.be_false()
}

pub fn is_safe_path_accepts_spaces_test() {
  // Spaces are now allowed in paths (blocklist approach)
  security.is_safe_path("my file.txt") |> should.be_true()
}

// =============================================================================
// Path Traversal Tests (validate_file_path)
// =============================================================================

pub fn validate_file_path_valid_file_test() {
  // This will fail because the file doesn't exist, but that's expected
  // We're testing the validation logic
  let result = security.validate_file_path("nonexistent.txt")
  // Should return FileNotAccessible or InvalidPath for nonexistent files
  case result {
    Error(security.FileNotAccessible(_)) -> Nil
    Error(security.InvalidPath(_, _)) -> Nil  // Also acceptable
    Ok(_) -> {
      should.fail()
      Nil
    }
    Error(_) -> {
      should.fail()
      Nil
    }
  }
}

pub fn validate_file_path_rejects_parent_ref_test() {
  case security.validate_file_path("../etc/passwd") {
    Error(security.PathTraversalAttempt(path)) -> {
      string.contains(path, "..") |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_url_encoded_dot_test() {
  case security.validate_file_path("test%2e%2e/passwd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil  // % is not in safe chars
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_url_encoded_slash_test() {
  case security.validate_file_path("test%2fpasswd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil  // % is not in safe chars
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_url_encoded_backslash_test() {
  case security.validate_file_path("test%5cpasswd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil  // % is not in safe chars
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_backslash_traversal_test() {
  case security.validate_file_path("test\\..\\passwd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil  // Backslash not in safe chars
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_null_byte_url_encoded_test() {
  case security.validate_file_path("test%00.txt") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil  // % is not in safe chars
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_null_byte_literal_test() {
  // Test literal null byte (path truncation attack)
  // Create a path with a null byte using utf_codepoint
  let null_byte_path = case string.utf_codepoint(0) {
    Ok(null_cp) -> "/etc/passwd" <> string.from_utf_codepoints([null_cp]) <> ".txt"
    Error(_) -> "/etc/passwd.txt"  // fallback if null creation fails
  }

  case security.validate_file_path(null_byte_path) {
    Error(security.PathTraversalAttempt(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_double_encoded_test() {
  case security.validate_file_path("test%252e%252e/passwd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil  // % is not in safe chars
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_multiple_dots_test() {
  case security.validate_file_path("test..../passwd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_shell_metacharacters_test() {
  case security.validate_file_path("; rm -rf /") {
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_command_substitution_test() {
  case security.validate_file_path("$(whoami).txt") {
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_backtick_injection_test() {
  case security.validate_file_path("`id`.txt") {
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    _ -> should.fail()
  }
}

// =============================================================================
// ReDoS Protection Tests (validate_regex_pattern)
// =============================================================================

pub fn validate_regex_pattern_simple_pattern_test() {
  case security.validate_regex_pattern("^[a-z]+$") {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_digit_class_test() {
  case security.validate_regex_pattern("\\d{3}-\\d{4}") {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_word_boundary_test() {
  case security.validate_regex_pattern("\\bword\\b") {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_rejects_nested_plus_test() {
  case security.validate_regex_pattern("(.+)+") {
    Error(security.UnsafeRegexPattern(_, reason)) -> {
      string.contains(reason, "ReDoS") |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_rejects_nested_star_test() {
  case security.validate_regex_pattern("(.*)\\+") {
    Error(security.UnsafeRegexPattern(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_rejects_double_star_test() {
  case security.validate_regex_pattern("(.*)*") {
    Error(security.UnsafeRegexPattern(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_rejects_negated_class_plus_test() {
  case security.validate_regex_pattern("([^)]*)+") {
    Error(security.UnsafeRegexPattern(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_rejects_word_plus_test() {
  case security.validate_regex_pattern("(\\w+)+") {
    Error(security.UnsafeRegexPattern(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_rejects_digit_plus_test() {
  case security.validate_regex_pattern("(\\d+)+") {
    Error(security.UnsafeRegexPattern(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_rejects_space_plus_test() {
  case security.validate_regex_pattern("(\\s+)+") {
    Error(security.UnsafeRegexPattern(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_rejects_overlapping_quantifiers_test() {
  case security.validate_regex_pattern(".*.*") {
    Error(security.UnsafeRegexPattern(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_rejects_alternation_star_test() {
  case security.validate_regex_pattern("(a*)*") {
    Error(security.UnsafeRegexPattern(_, _)) -> Nil
    _ -> should.fail()
  }
}

// =============================================================================
// Error Formatting Tests
// =============================================================================

pub fn format_path_traversal_error_test() {
  let error = security.PathTraversalAttempt("../etc/passwd")
  let formatted = security.format_security_error(error)

  string.contains(formatted, "Path traversal") |> should.be_true()
  string.contains(formatted, "../etc/passwd") |> should.be_true()
  string.contains(formatted, "..") |> should.be_true()
}

pub fn format_invalid_path_error_test() {
  let error = security.InvalidPath("/tmp/socket", "Not a regular file")
  let formatted = security.format_security_error(error)

  string.contains(formatted, "Invalid path") |> should.be_true()
  string.contains(formatted, "/tmp/socket") |> should.be_true()
  string.contains(formatted, "Not a regular file") |> should.be_true()
}

pub fn format_file_not_accessible_error_test() {
  let error = security.FileNotAccessible("/root/secret.txt")
  let formatted = security.format_security_error(error)

  string.contains(formatted, "not accessible") |> should.be_true()
  string.contains(formatted, "/root/secret.txt") |> should.be_true()
}

pub fn format_unsafe_regex_error_test() {
  let error = security.UnsafeRegexPattern("(.+)+", "exponential backtracking")
  let formatted = security.format_security_error(error)

  string.contains(formatted, "Unsafe regex") |> should.be_true()
  string.contains(formatted, "(.+)+") |> should.be_true()
  string.contains(formatted, "exponential backtracking") |> should.be_true()
  string.contains(formatted, "nested quantifiers") |> should.be_true()
}

pub fn format_shell_metacharacters_error_test() {
  let error = security.ShellMetacharactersDetected("; rm -rf /")
  let formatted = security.format_security_error(error)

  string.contains(formatted, "shell metacharacters") |> should.be_true()
  string.contains(formatted, "; rm -rf /") |> should.be_true()
}

// =============================================================================
// Multiple Paths Validation Tests
// =============================================================================

pub fn validate_file_paths_all_invalid_test() {
  case security.validate_file_paths(["../etc/passwd", "$(whoami).txt"]) {
    Error(_) -> Nil  // Should fail on first invalid path
    _ -> should.fail()
  }
}

pub fn validate_file_paths_mixed_test() {
  // All will fail since files don't exist, but testing traversal detection
  let result = security.validate_file_paths(["valid.txt", "../etc/passwd"])
  case result {
    Error(security.PathTraversalAttempt(_)) -> Nil  // Should catch traversal on second item
    Error(security.FileNotAccessible(_)) -> Nil  // Or file not found on valid.txt
    Error(security.InvalidPath(_, _)) -> Nil  // Or invalid path on valid.txt
    Error(security.ShellMetacharactersDetected(_)) -> Nil
    Ok(_) -> {
      should.fail()
      Nil
    }
    Error(_) -> {
      should.fail()
      Nil
    }
  }
}

pub fn validate_file_paths_empty_list_test() {
  case security.validate_file_paths([]) {
    Ok(paths) -> {
      paths |> should.equal([])
    }
    _ -> should.fail()
  }
}

// =============================================================================
// Edge Cases and Attack Vectors
// =============================================================================

pub fn validate_file_path_case_insensitive_encoding_test() {
  // Test uppercase URL encoding
  case security.validate_file_path("test%2E%2E/passwd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil  // % is not in safe chars
    _ -> should.fail()
  }
}

pub fn validate_file_path_mixed_case_encoding_test() {
  case security.validate_file_path("test%2e%2F../passwd") {
    Error(security.PathTraversalAttempt(_)) -> Nil
    Error(security.ShellMetacharactersDetected(_)) -> Nil  // % is not in safe chars
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_complex_safe_pattern_test() {
  // Complex but safe pattern
  case security.validate_regex_pattern("^(?:[a-z]+|[0-9]+)$") {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_regex_pattern_email_like_safe_test() {
  case security.validate_regex_pattern("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$") {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn is_safe_path_empty_string_test() {
  // Empty string has no dangerous chars, but file existence is checked elsewhere
  security.is_safe_path("") |> should.be_true()
}

pub fn is_safe_path_only_dots_test() {
  security.is_safe_path("...") |> should.be_true()  // Three dots is ok, four is not
}

pub fn validate_regex_pattern_empty_pattern_test() {
  case security.validate_regex_pattern("") {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_file_path_rejects_empty_string_test() {
  case security.validate_file_path("") {
    Error(security.InvalidPath(_, reason)) -> {
      string.contains(reason, "empty") |> should.be_true()
    }
    _ -> should.fail()
  }
}

// =============================================================================
// SSRF Protection Tests (validate_url)
// =============================================================================

pub fn validate_url_accepts_valid_https_test() {
  case security.validate_url("https://api.example.com", False) {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_accepts_valid_http_test() {
  case security.validate_url("http://api.example.com", False) {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_accepts_valid_with_port_test() {
  case security.validate_url("https://api.example.com:8080", False) {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_accepts_valid_with_path_test() {
  case security.validate_url("https://api.example.com/v1/users", False) {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_accepts_valid_with_query_test() {
  case security.validate_url("https://api.example.com/search?q=test", False) {
    Ok(_) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_file_scheme_test() {
  case security.validate_url("file:///etc/passwd", False) {
    Error(security.SSRFAttempt(_, reason)) -> {
      string.contains(reason, "http") |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_ftp_scheme_test() {
  case security.validate_url("ftp://example.com", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_localhost_test() {
  case security.validate_url("http://localhost:8080", False) {
    Error(security.SSRFAttempt(_, reason)) -> {
      string.contains(reason, "ocalhost") |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_localhost_uppercase_test() {
  case security.validate_url("http://LOCALHOST:8080", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_127_0_0_1_test() {
  case security.validate_url("http://127.0.0.1", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_127_loopback_test() {
  case security.validate_url("http://127.1.1.1", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_ipv6_loopback_test() {
  case security.validate_url("http://[::1]:8080", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_ipv6_loopback_long_test() {
  case security.validate_url("http://[0:0:0:0:0:0:0:1]:8080", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_10_private_range_test() {
  case security.validate_url("http://10.0.0.1", False) {
    Error(security.SSRFAttempt(_, reason)) -> {
      string.contains(reason, "rivate") |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_10_255_private_range_test() {
  case security.validate_url("http://10.9.255.255", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_192_168_private_range_test() {
  case security.validate_url("http://192.168.1.1", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_172_16_private_range_test() {
  case security.validate_url("http://172.16.0.1", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_172_31_private_range_test() {
  case security.validate_url("http://172.31.255.255", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_169_254_link_local_test() {
  case security.validate_url("http://169.254.169.254", False) {
    Error(security.SSRFAttempt(_, reason)) -> {
      string.contains(reason, "ink-local") |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_169_254_aws_metadata_test() {
  case
    security.validate_url("http://169.254.169.254/latest/meta-data/", False)
  {
    Error(security.SSRFAttempt(_, reason)) -> {
      string.contains(reason, "metadata") |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_ipv6_link_local_test() {
  case security.validate_url("http://[fe80::1]", False) {
    Error(security.SSRFAttempt(_, reason)) -> {
      string.contains(reason, "IPv6") |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_ipv6_unique_local_fc_test() {
  case security.validate_url("http://[fc00::1]", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn validate_url_rejects_ipv6_unique_local_fd_test() {
  case security.validate_url("http://[fd00::1]", False) {
    Error(security.SSRFAttempt(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn format_security_error_ssrf_test() {
  let error = security.SSRFAttempt("http://localhost", "Test reason")
  let formatted = security.format_security_error(error)

  string.contains(formatted, "SSRF") |> should.be_true()
  string.contains(formatted, "http://localhost") |> should.be_true()
  string.contains(formatted, "Test reason") |> should.be_true()
}
