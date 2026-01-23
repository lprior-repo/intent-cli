//// Tests for global --timeout flag functionality
//// Verifies timeout override behavior for HTTP-calling commands

import gleam/option.{None, Some}
import gleeunit/should
import intent/http_client
import intent/runner.{RunOptions}

// ============================================================================
// RunOptions Timeout Tests
// ============================================================================

pub fn run_options_default_timeout_test() {
  // Default options should have None for timeout (uses spec config)
  let options = runner.default_options()

  options.timeout_ms
  |> should.equal(None)
}

pub fn run_options_custom_timeout_test() {
  // Custom timeout should be stored in options
  let options =
    RunOptions(
      feature_filter: None,
      behavior_filter: None,
      output_level: runner.Normal,
      timeout_ms: Some(5000),
    )

  options.timeout_ms
  |> should.equal(Some(5000))
}

pub fn run_options_with_timeout_helper_test() {
  // Test the with_timeout helper function
  let options =
    runner.default_options()
    |> runner.with_timeout(10_000)

  options.timeout_ms
  |> should.equal(Some(10_000))
}

pub fn run_options_timeout_overrides_spec_test() {
  // When timeout is provided, it should override spec config
  // The effective_timeout helper should return CLI flag when set
  let spec_timeout = 30_000
  let cli_timeout = 5000

  runner.effective_timeout(Some(cli_timeout), spec_timeout)
  |> should.equal(cli_timeout)
}

pub fn run_options_timeout_falls_back_to_spec_test() {
  // When timeout is None, should use spec config timeout
  let spec_timeout = 30_000

  runner.effective_timeout(None, spec_timeout)
  |> should.equal(spec_timeout)
}

// ============================================================================
// Default Timeout Value Tests
// ============================================================================

pub fn default_timeout_constant_test() {
  // AI guardrails specify 30000ms (30 seconds) as default
  runner.default_timeout_ms
  |> should.equal(30_000)
}

pub fn http_client_default_timeout_constant_test() {
  // http_client should also have the same default
  http_client.default_timeout_ms
  |> should.equal(30_000)
}
