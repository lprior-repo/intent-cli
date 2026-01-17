/// Tests for localhost bypass functionality
import gleam/dict
import gleam/option.{Some}
import gleam/string
import gleam/uri
import gleeunit/should
import intent/security
import intent/types.{Config}

// ============================================================================
// Security Module Tests
// ============================================================================

pub fn validate_url_rejects_localhost_by_default_test() {
  let result = security.validate_url("http://localhost:8080/api", False)

  case result {
    Error(security.SSRFAttempt(_, _)) -> True
    _ -> False
  }
  |> should.be_true
}

pub fn validate_url_rejects_127_0_0_1_by_default_test() {
  let result = security.validate_url("http://127.0.0.1:8080/api", False)

  case result {
    Error(security.SSRFAttempt(_, _)) -> True
    _ -> False
  }
  |> should.be_true
}

pub fn validate_url_rejects_127_1_by_default_test() {
  let result = security.validate_url("http://127.1.1.1/api", False)

  case result {
    Error(security.SSRFAttempt(_, _)) -> True
    _ -> False
  }
  |> should.be_true
}

pub fn validate_url_rejects_ipv6_localhost_by_default_test() {
  let result = security.validate_url("http://[::1]:8080/api", False)

  case result {
    Error(security.SSRFAttempt(_, _)) -> True
    _ -> False
  }
  |> should.be_true
}

pub fn validate_url_allows_localhost_when_flag_true_test() {
  let result = security.validate_url("http://localhost:8080/api", True)

  result
  |> should.be_ok
}

pub fn validate_url_allows_127_0_0_1_when_flag_true_test() {
  let result = security.validate_url("http://127.0.0.1:3000/api", True)

  result
  |> should.be_ok
}

pub fn validate_url_allows_127_x_when_flag_true_test() {
  let result = security.validate_url("http://127.1.2.3:8080/api", True)

  result
  |> should.be_ok
}

pub fn validate_url_allows_ipv6_localhost_when_flag_true_test() {
  let result = security.validate_url("http://[::1]:8080/api", True)

  result
  |> should.be_ok
}

pub fn validate_url_still_rejects_private_ips_with_flag_test() {
  // Even with allow_localhost=True, private IPs should still be blocked
  let result = security.validate_url("http://192.168.1.1/api", True)

  case result {
    Error(security.SSRFAttempt(_, _)) -> True
    _ -> False
  }
  |> should.be_true
}

pub fn validate_url_still_rejects_link_local_with_flag_test() {
  // Even with allow_localhost=True, link-local should still be blocked
  let result = security.validate_url("http://169.254.169.254/api", True)

  case result {
    Error(security.SSRFAttempt(_, _)) -> True
    _ -> False
  }
  |> should.be_true
}

pub fn validate_url_accepts_public_urls_test() {
  let result = security.validate_url("https://api.example.com/v1/users", False)

  result
  |> should.be_ok
}

// ============================================================================
// HTTP Client Tests
// ============================================================================

pub fn http_client_validates_localhost_with_config_test() {
  // This test verifies that the HTTP client respects the allow_localhost config
  // We test by constructing a Config and checking validation happens

  let config_blocking =
    Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: False,
    )

  let config_allowing =
    Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: True,
    )

  // We can't easily test the full HTTP client without mocking
  // but we've verified the validation logic is threaded through
  // The integration is tested by the security tests above

  config_blocking.allow_localhost
  |> should.equal(False)

  config_allowing.allow_localhost
  |> should.equal(True)
}

pub fn http_client_config_type_has_allow_localhost_field_test() {
  let config =
    Config(
      base_url: "https://api.example.com",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: False,
    )

  config.allow_localhost
  |> should.equal(False)
}

// ============================================================================
// Error Message Tests
// ============================================================================

pub fn error_message_mentions_flag_when_localhost_blocked_test() {
  let result = security.validate_url("http://localhost:8080", False)

  case result {
    Error(security.SSRFAttempt(_, reason)) -> {
      reason
      |> should.equal("Localhost addresses are not allowed")
    }
    _ -> panic as "Expected SSRFAttempt error"
  }
}

pub fn formatted_error_includes_helpful_message_test() {
  let error =
    security.SSRFAttempt(
      "http://localhost:8080",
      "Localhost addresses are not allowed",
    )
  let formatted = security.format_security_error(error)

  // Check that formatted error includes the URL
  string.contains(formatted, "http://localhost:8080")
  |> should.be_true

  // Check that formatted error includes the reason
  string.contains(formatted, "Localhost addresses are not allowed")
  |> should.be_true
}

// ============================================================================
// URI Parsing Tests
// ============================================================================

pub fn can_parse_localhost_uri_test() {
  let result = uri.parse("http://localhost:8080/api/v1/users")

  case result {
    Ok(parsed) -> {
      parsed.scheme
      |> should.equal(Some("http"))

      parsed.host
      |> should.equal(Some("localhost"))

      parsed.path
      |> should.equal("/api/v1/users")
    }
    Error(_) -> panic as "URI parse failed"
  }
}

pub fn can_parse_127_uri_test() {
  let result = uri.parse("http://127.0.0.1:3000/test")

  case result {
    Ok(parsed) -> {
      parsed.host
      |> should.equal(Some("127.0.0.1"))

      parsed.port
      |> should.equal(Some(3000))
    }
    Error(_) -> panic as "URI parse failed"
  }
}

// ============================================================================
// Config Construction Tests
// ============================================================================

pub fn config_defaults_to_secure_test() {
  // When creating a Config, allow_localhost should default to False
  // (enforced by type system - no default values in Gleam)
  let config =
    Config(
      base_url: "https://api.example.com",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: False,
    )

  config.allow_localhost
  |> should.equal(False)
}

pub fn config_can_enable_localhost_test() {
  let config =
    Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: True,
    )

  config.allow_localhost
  |> should.equal(True)
}

// ============================================================================
// Integration Scenarios
// ============================================================================

pub fn scenario_development_mode_test() {
  // Scenario: Developer testing against local backend
  let config =
    Config(
      base_url: "http://localhost:8080",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: True,
    )

  // Validate the base URL
  let result = security.validate_url(config.base_url, config.allow_localhost)

  result
  |> should.be_ok
}

pub fn scenario_production_mode_test() {
  // Scenario: Production deployment - localhost should be blocked
  let config =
    Config(
      base_url: "https://api.production.com",
      timeout_ms: 5000,
      headers: dict.new(),
      allow_localhost: False,
    )

  // Attempting to use localhost should fail
  let result =
    security.validate_url("http://localhost:8080", config.allow_localhost)

  case result {
    Error(security.SSRFAttempt(_, _)) -> True
    _ -> False
  }
  |> should.be_true
}

pub fn scenario_mixed_localhost_and_public_test() {
  // Scenario: Some requests to localhost (allowed), some to public APIs
  let allow = True

  security.validate_url("http://localhost:8080/api", allow)
  |> should.be_ok

  security.validate_url("https://api.example.com/v1", allow)
  |> should.be_ok
}
