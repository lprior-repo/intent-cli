//// Tests for standardized CLI error formatting
//// Ensures consistent error message format across the CLI

import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import intent/ai_errors

// =============================================================================
// Basic Error Message Tests
// =============================================================================

pub fn format_cli_error_basic_message_test() {
  let error = ai_errors.cli_error("spec file path required")
  let formatted = ai_errors.format_cli_error(error)

  formatted
  |> should.equal("Error: spec file path required")
}

pub fn format_cli_error_with_prefix_test() {
  let error = ai_errors.cli_error("invalid session ID")
  let formatted = ai_errors.format_cli_error(error)

  string.starts_with(formatted, "Error: ")
  |> should.be_true
}

pub fn format_cli_error_with_usage_hint_test() {
  let error =
    ai_errors.cli_error_with_usage(
      message: "spec file path required",
      usage: "intent export <spec.cue>",
    )
  let formatted = ai_errors.format_cli_error(error)

  string.contains(formatted, "Error: spec file path required")
  |> should.be_true

  string.contains(formatted, "Usage: intent export <spec.cue>")
  |> should.be_true
}

pub fn format_cli_error_with_context_test() {
  let error =
    ai_errors.cli_error_with_context(message: "session not found", context: [
      "Run 'intent sessions' to list available sessions",
    ])
  let formatted = ai_errors.format_cli_error(error)

  string.contains(formatted, "session not found")
  |> should.be_true

  string.contains(formatted, "Run 'intent sessions'")
  |> should.be_true
}

pub fn format_cli_error_full_format_test() {
  let error =
    ai_errors.CliError(
      message: "unexpected argument",
      usage_hint: Some("intent interview --profile=api"),
      context: ["Valid profiles: api, cli"],
    )
  let formatted = ai_errors.format_cli_error(error)

  string.contains(formatted, "Error:")
  |> should.be_true

  string.contains(formatted, "Usage:")
  |> should.be_true

  string.contains(formatted, "Valid profiles")
  |> should.be_true
}

pub fn format_cli_error_empty_context_test() {
  let error =
    ai_errors.CliError(message: "test error", usage_hint: None, context: [])
  let formatted = ai_errors.format_cli_error(error)

  formatted
  |> should.equal("Error: test error")
}

pub fn cli_error_from_structured_test() {
  let structured = ai_errors.file_not_found("/path/to/spec.cue")
  let cli = ai_errors.cli_error_from_structured(structured)
  let formatted = ai_errors.format_cli_error(cli)

  string.starts_with(formatted, "Error:")
  |> should.be_true

  string.contains(formatted, "File not found")
  |> should.be_true
}
