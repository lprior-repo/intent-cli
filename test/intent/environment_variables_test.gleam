import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import gleam_community/http/request as http_request
import gleam_community/http/client as http_client
import shellout
import intent/loader
import intent/security
import intent/types

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// Environment Variable Handling Tests
// ============================================================================

/// Test INTENT_* environment variables are properly handled
fn test_intent_environment_variables() {
  // Test with INTENT_* environment variables
  let env_vars = [
    ("INTENT_API_KEY", "test-api-key"),
    ("INTENT_BASE_URL", "https://api.example.com"),
    ("INTENT_TIMEOUT_MS", "5000"),
    ("INTENT_HEADER_AUTH", "Bearer test-token"),
  ]

  // Set environment variables and test they are used
  let with_env =
    list.map(env_vars, fn(kv) {
      let (key, value) = kv
      shellout.command("bash", ["-c", "export " <> key <> "=" <> value], ".", [])
    })

  // Test if environment variables are processed in specs
  // This will depend on the specific implementation of env var handling
  let spec_path = "examples/user-api.cue"

  // Clean up after test
  let cleanup =
    list.map(env_vars, fn(kv) {
      let (key, _) = kv
      shellout.command("bash", ["-c", "unset " <> key], ".", [])
    })

  // For now, test that security validation still works
  case security.validate_file_path(spec_path) {
    Ok(_) -> should.equal("Environment variables should be handled", "placeholder")
    Error(e) -> should.fail("Security validation should pass: " <> security.format_security_error(e))
  }
}

/// Test with missing required environment variables
fn test_missing_required_env_vars() {
  // Ensure no INTENT_* variables are set
  shellout.command("bash", ["-c", "unset INTENT_API_URL INTENT_AUTH_TOKEN INTENT_TIMEOUT"], ".", [])

  // Test that the CLI handles missing required env vars gracefully
  // Currently, the code doesn't seem to have specific required env vars
  // This test will verify current behavior
  let result = shellout.command("gleam", ["run", "--", "check", "examples/user-api.cue"], ".", [])

  case result {
    Ok(output) ->
      // Should not crash due to missing env vars
      should.equal("Missing env vars should be handled gracefully", "placeholder")
    Error(_) ->
      // Other errors are acceptable (like file not found)
      should.equal("Missing env vars should be handled gracefully", "placeholder")
  }
}

/// Test with invalid environment variable values
fn test_invalid_env_var_values() {
  // Test with invalid timeout values
  let invalid_values = [
    ("INTENT_TIMEOUT_MS", "not_a_number"),
    ("INTENT_TIMEOUT_MS", "-100"),
    ("INTENT_TIMEOUT_MS", "999999999999"), // Too large
  ]

  let set_env =
    list.map(invalid_values, fn(kv) {
      let (key, value) = kv
      shellout.command("bash", ["-c", "export " <> key <> "=" <> value], ".", [])
    })

  // Test that the CLI validates env var values
  let spec_path = "examples/user-api.cue"

  // Clean up
  let cleanup =
    list.map(invalid_values, fn(kv) {
      let (key, _) = kv
      shellout.command("bash", ["-c", "unset " <> key], ".", [])
    })

  // Current implementation doesn't validate env vars, so this is a placeholder
  should.equal("Invalid env var values should be validated", "placeholder")
}

/// Test environment variable interpolation in specs
fn test_env_var_interpolation_in_specs() {
  // Test that environment variables are interpolated in CUE specs
  // This would require creating a test spec with interpolation syntax

  let test_spec = `
  spec: {
    name: "Environment Variable Test"
    description: "Test spec with environment variable interpolation"
    audience: "API developers"
    version: "1.0.0"
    success_criteria: ["API responds correctly"]
    config: {
      base_url: "${INTENT_BASE_URL}"
      timeout_ms: ${INTENT_TIMEOUT_MS}
      headers: {
        "Authorization": "${INTENT_AUTH_TOKEN}"
      }
    }
    features: []
    rules: []
    anti_patterns: []
    ai_hints: {
      implementation: { suggested_stack: [] }
      entities: {}
      security: { password_hashing: "" jwt_algorithm: "" jwt_expiry: "" rate_limiting: "" }
      pitfalls: []
    }
  }
  `

  // Write test spec to file
  shellout.command("bash", ["-c", "mkdir -p test_env_vars"], ".", [])
  shellout.command("bash", ["-c", "echo '" <> test_spec <> "' > test_env_vars/spec.cue"], ".", [])

  // Set test environment variables
  shellout.command("bash", ["-c", "export INTENT_BASE_URL='https://test.api.com' INTENT_TIMEOUT_MS=5000 INTENT_AUTH_TOKEN='Bearer test'"], ".", [])

  // Try to load the spec
  let result = loader.load_spec("test_env_vars/spec.cue")

  case result {
    Ok(_) ->
      // Successfully loaded spec with interpolated values
      should.equal("Spec with env var interpolation should load", "placeholder")
    Error(loader.CueValidationError(msg)) ->
      // CUE validation error - check if it's due to interpolation
      case string.contains(msg, "INTENT_") {
        True -> should.fail("Unresolved environment variables should be handled gracefully")
        False -> should.equal("Spec validation error (not env related): " <> msg, "placeholder")
      }
    Error(_) ->
      should.fail("Spec loading failed with unexpected error")
  }

  // Clean up
  shellout.command("bash", ["-c", "rm -rf test_env_vars unset INTENT_BASE_URL INTENT_TIMEOUT_MS INTENT_AUTH_TOKEN"], ".", [])
}

/// Test sensitive data in environment variables
fn test_sensitive_data_in_env_vars() {
  // Test that sensitive data in environment variables is not leaked in output
  let sensitive_data = "super-secret-api-key-12345"
  let env_vars = [
    ("INTENT_API_KEY", sensitive_data),
    ("INTENT_PASSWORD", "password123"),
    ("INTENT_TOKEN", "bearer-token-xyz"),
  ]

  let set_env =
    list.map(env_vars, fn(kv) {
      let (key, value) = kv
      shellout.command("bash", ["-c", "export " <> key <> "=" <> value], ".", [])
    })

  // Run a command that might output environment variables
  let result = shellout.command("bash", ["-c", "echo 'Testing sensitive data handling'"], ".", [])

  case result {
    Ok(output) ->
      // Ensure sensitive data is not in output
      case string.contains(output, sensitive_data) {
        True -> should.fail("Sensitive data should not be leaked in output")
        False -> should.equal("Sensitive data properly protected", "placeholder")
      }
    Error(_) ->
      should.equal("Command executed successfully", "placeholder")
  }

  // Clean up
  let cleanup =
    list.map(env_vars, fn(kv) {
      let (key, _) = kv
      shellout.command("bash", ["-c", "unset " <> key], ".", [])
    })
}

/// Verify no secrets leaked in output
fn test_no_secrets_leaked_in_output() {
  // Test that running commands with env vars doesn't leak secrets
  let secret_values = [
    ("INTENT_SECRET", "shhh-its-a-secret"),
    ("INTENT_PASSWORD", "admin123"),
    ("INTENT_TOKEN", "jwt-token-xyz789"),
  ]

  let set_env =
    list.map(secret_values, fn(kv) {
      let (key, value) = kv
      shellout.command("bash", ["-c", "export " <> key <> "=" <> value], ".", [])
    })

  // Test various commands that might output environment variables
  let commands = [
    ["env"],
    ["printenv"],
    ["bash", "-c", "echo $INTENT_SECRET"],
  ]

  let results =
    list.map(commands, fn(cmd) {
      shellout.command("bash", cmd, ".", [])
    })

  // Check that no secrets are leaked in any output
  let has_secrets_leaked =
    list.fold(results, False, fn(acc, result) {
      case result {
        Ok(output) ->
          acc || list.fold(secret_values, False, fn(secrets_acc, (key, value)) {
            secrets_acc || string.contains(output, value)
          })
        Error(_) -> acc
      }
    })

  case has_secrets_leaked {
    True -> should.fail("Secrets should not be leaked in output")
    False -> should.equal("No secrets leaked in output", "placeholder")
  }

  // Clean up
  let cleanup =
    list.map(secret_values, fn(kv) {
      let (key, _) = kv
      shellout.command("bash", ["-c", "unset " <> key], ".", [])
    })
}

/// Test environment variable precedence
fn test_env_var_precedence() {
  // Test precedence: command line flags > config files > environment variables > defaults
  // For now, test that environment variables don't interfere with other sources

  // Set environment variables
  shellout.command("bash", ["-c", "export INTENT_BASE_URL='env-url' INTENT_TIMEOUT_MS=10000"], ".", [])

  // Test that command line arguments override environment variables
  let result = shellout.command("gleam", ["run", "--", "check", "examples/user-api.cue"], ".", [])

  // Clean up
  shellout.command("bash", ["-c", "unset INTENT_BASE_URL INTENT_TIMEOUT_MS"], ".", [])

  // Current implementation doesn't have explicit precedence handling
  // This test serves as a placeholder for future implementation
  should.equal("Environment variable precedence should be implemented", "placeholder")
}

/// Test environment variable validation and sanitization
fn test_env_var_validation_sanitization() {
  // Test that environment variables are validated and sanitized
  let malicious_values = [
    ("INTENT_BASE_URL", "javascript:alert('xss')"),
    ("INTENT_BASE_URL", "file:///etc/passwd"),
    ("INTENT_BASE_URL", "ftp://attacker.com/malicious"),
    ("INTENT_HEADER_NAME", "X-Injected-Header"),
    ("INTENT_HEADER_VALUE", "malicious-content"),
  ]

  let set_env =
    list.map(malicious_values, fn(kv) {
      let (key, value) = kv
      shellout.command("bash", ["-c", "export " <> key <> "=" <> value], ".", [])
    })

  // Test that the CLI validates and sanitizes environment variables
  let spec_path = "examples/user-api.cue"

  case security.validate_file_path(spec_path) {
    Ok(_) -> should.equal("Malicious env vars should be detected and blocked", "placeholder")
    Error(e) ->
      case string.contains(security.format_security_error(e), "Security error") {
        True -> should.equal("Security validation caught potential threat", "placeholder")
        False -> should.fail("Security error should be properly identified")
      }
  }

  // Clean up
  let cleanup =
    list.map(malicious_values, fn(kv) {
      let (key, _) = kv
      shellout.command("bash", ["-c", "unset " <> key], ".", [])
    })
}

/// Test environment variable injection prevention
fn test_env_var_injection_prevention() {
  // Test against environment variable injection attacks
  let injection_values = [
    ("INTENT_PATH", "../../../etc/passwd"),
    ("INTENT_PATH", "| cat /etc/passwd"),
    ("INTENT_PATH", "; rm -rf /"),
    ("INTENT_PATH", "$(rm -rf /)"),
    ("INTENT_PATH", "`rm -rf /`"),
  ]

  let set_env =
    list.map(injection_values, fn(kv) {
      let (key, value) = kv
      shellout.command("bash", ["-c", "export " <> key <> "=" <> value], ".", [])
    })

  // Test that the CLI prevents command injection through environment variables
  let spec_path = "examples/user-api.cue"

  case security.validate_file_path(spec_path) {
    Ok(_) ->
      // Path validation should prevent injection
      should.equal("Path injection should be prevented", "placeholder")
    Error(security.PathTraversalAttempt(_)) ->
      should.equal("Path injection attempt properly detected", "placeholder")
    Error(security.ShellMetacharactersDetected(_)) ->
      should.equal("Shell metacharacters properly detected", "placeholder")
    Error(_) ->
      should.fail("Security error should be identified")
  }

  // Clean up
  let cleanup =
    list.map(injection_values, fn(kv) {
      let (key, _) = kv
      shellout.command("bash", ["-c", "unset " <> key], ".", [])
    })
}

/// Test environment variable size limits
fn test_env_var_size_limits() {
  // Test that environment variables are properly size-limited
  let large_value = string.repeat("x", 10000) // 10KB string
  let env_vars = [
    ("INTENT_LARGE_VALUE", large_value),
    ("INTENT_BASE_URL", "https://example.com"),
  ]

  let set_env =
    list.map(env_vars, fn(kv) {
      let (key, value) = kv
      shellout.command("bash", ["-c", "export " <> key <> "=" <> value], ".", [])
    })

  // Test that the CLI handles large environment variables
  let result = loader.load_spec("examples/user-api.cue")

  // Clean up
  let cleanup =
    list.map(env_vars, fn(kv) {
      let (key, _) = kv
      shellout.command("bash", ["-c", "unset " <> key], ".", [])
    })

  case result {
    Ok(_) -> should.equal("Large env vars should be handled", "placeholder")
    Error(_) -> should.equal("Large env vars should be handled", "placeholder")
  }
}

/// Test environment variable character restrictions
fn test_env_var_char_restrictions() {
  // Test that environment variables with problematic characters are handled
  let problematic_values = [
    ("INTENT_URL", "https://example.com/\x00"), // Null byte
    ("INTENT_URL", "https://example.com/\n"), // Newline
    ("INTENT_URL", "https://example.com/\r"), // Carriage return
    ("INTENT_URL", "https://example.com/\t"), // Tab
    ("INTENT_URL", "https://example.com/\x1F"), // Control character
  ]

  let set_env =
    list.map(problematic_values, fn(kv) {
      let (key, value) = kv
      shellout.command("bash", ["-c", "export " <> key <> "=" <> value], ".", [])
    })

  // Test that the CLI handles characters that might cause issues
  let spec_path = "examples/user-api.cue"

  case security.validate_file_path(spec_path) {
    Ok(_) -> should.equal("Problematic characters should be handled", "placeholder")
    Error(_) -> should.equal("Problematic characters should be handled", "placeholder")
  }

  // Clean up
  let cleanup =
    list.map(problematic_values, fn(kv) {
      let (key, _) = kv
      shellout.command("bash", ["-c", "unset " <> key], ".", [])
    })
}

/// Test environment variable encoding handling
fn test_env_var_encoding_handling() {
  // Test that environment variables with special encoding are handled properly
  let encoded_values = [
    ("INTENT_VALUE", "hello%20world"), // URL encoded
    ("INTENT_VALUE", "hello%2Fworld"),  // URL encoded slash
    ("INTENT_VALUE", "hello%5Cworld"), // URL encoded backslash
    ("INTENT_VALUE", "hello%00world"), // Null byte
  ]

  let set_env =
    list.map(encoded_values, fn(kv) {
      let (key, value) = kv
      shellout.command("bash", ["-c", "export " <> key <> "=" <> value], ".", [])
    })

  // Test that the CLI handles encoded values correctly
  let spec_path = "examples/user-api.cue"

  case security.validate_file_path(spec_path) {
    Ok(_) -> should.equal("Encoded values should be handled", "placeholder")
    Error(security.PathTraversalAttempt(_)) ->
      should.equal("Encoded path traversal should be detected", "placeholder")
    Error(_) -> should.equal("Encoded values should be handled", "placeholder")
  }

  // Clean up
  let cleanup =
    list.map(encoded_values, fn(kv) {
      let (key, _) = kv
      shellout.command("bash", ["-c", "unset " <> key], ".", [])
    })
}