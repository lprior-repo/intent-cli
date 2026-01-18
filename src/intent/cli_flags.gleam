/// CLI flag builders and validation helpers for consistent flag patterns
///
/// This module provides:
/// - Reusable flag builders for common patterns
/// - Flag validation helpers
/// - Standardized error messages
/// - Environment variable fallback support

import gleam/int
import gleam/list
import gleam/result
import gleam/string
import glint/flag

/// Create a target URL flag with environment variable support
pub fn target_flag() -> flag.FlagBuilder(String) {
  flag.string()
  |> flag.default("")
  |> flag.description(
    "Target base URL to test against [env: INTENT_TARGET] (required)",
  )
}

/// Create a JSON output flag
pub fn json_flag() -> flag.FlagBuilder(Bool) {
  flag.bool()
  |> flag.default(False)
  |> flag.description("Output results as JSON (short: -j)")
}

/// Create a verbose flag
pub fn verbose_flag() -> flag.FlagBuilder(Bool) {
  flag.bool()
  |> flag.default(False)
  |> flag.description("Verbose output (short: -v)")
}

/// Create a quiet flag
pub fn quiet_flag() -> flag.FlagBuilder(Bool) {
  flag.bool()
  |> flag.default(False)
  |> flag.description("Quiet output - errors only (short: -q)")
}

/// Create an output file flag
pub fn output_file_flag() -> flag.FlagBuilder(String) {
  flag.string()
  |> flag.default("")
  |> flag.description(
    "Output file path [env: INTENT_OUTPUT] (short: -o) (optional)",
  )
}

/// Create a profile flag
pub fn profile_flag() -> flag.FlagBuilder(String) {
  flag.string()
  |> flag.default("api")
  |> flag.description(
    "System profile: api, cli, event, data, workflow, or ui [env: INTENT_PROFILE]",
  )
}

/// Create an allow-localhost flag
pub fn allow_localhost_flag() -> flag.FlagBuilder(Bool) {
  flag.bool()
  |> flag.default(False)
  |> flag.description(
    "Allow localhost URLs for development (bypasses SSRF protection) [env: INTENT_ALLOW_LOCALHOST]",
  )
}

/// Create a session ID flag
pub fn session_flag() -> flag.FlagBuilder(String) {
  flag.string()
  |> flag.default("")
  |> flag.description("Session ID (optional)")
}

/// Create a bead ID flag
pub fn bead_id_flag() -> flag.FlagBuilder(String) {
  flag.string()
  |> flag.default("")
  |> flag.description("Bead ID (required)")
}

/// Create a feature filter flag
pub fn feature_flag() -> flag.FlagBuilder(String) {
  flag.string()
  |> flag.default("")
  |> flag.description("Filter to a specific feature (optional)")
}

/// Create a behavior filter flag
pub fn only_flag() -> flag.FlagBuilder(String) {
  flag.string()
  |> flag.default("")
  |> flag.description("Run only a specific behavior (optional)")
}

// =============================================================================
// VALIDATION HELPERS
// =============================================================================

/// Validate that a required string flag is not empty
pub fn validate_required_string(
  value: String,
  flag_name: String,
) -> Result(String, String) {
  case string.is_empty(value) {
    True -> Error("--" <> flag_name <> ": required flag is missing or empty")
    False -> Ok(value)
  }
}

/// Validate that a number is within a range
pub fn validate_range(
  value: Int,
  min: Int,
  max: Int,
  flag_name: String,
) -> Result(Int, String) {
  case value >= min && value <= max {
    True -> Ok(value)
    False ->
      Error(
        "--"
        <> flag_name
        <> ": value "
        <> int.to_string(value)
        <> " is out of range ["
        <> int.to_string(min)
        <> ".."
        <> int.to_string(max)
        <> "]",
      )
  }
}

/// Validate that a string is one of allowed values
pub fn validate_enum(
  value: String,
  allowed: List(String),
  flag_name: String,
) -> Result(String, String) {
  case string.is_empty(value) {
    True -> Ok(value)
    False -> {
      case list.contains(allowed, value) {
        True -> Ok(value)
        False ->
          Error(
            "--"
            <> flag_name
            <> ": invalid value '"
            <> value
            <> "'. Expected one of: "
            <> string.join(allowed, ", "),
          )
      }
    }
  }
}

/// Validate flag dependencies (flag A requires flag B)
pub fn validate_dependency(
  dependent_value: String,
  required_value: String,
  dependent_name: String,
  required_name: String,
) -> Result(Nil, String) {
  case string.is_empty(dependent_value), string.is_empty(required_value) {
    False, True ->
      Error("--" <> dependent_name <> " requires --" <> required_name <> " to be set")
    _, _ -> Ok(Nil)
  }
}

// =============================================================================
// ENVIRONMENT VARIABLE HELPERS
// =============================================================================

/// Get string value from environment with fallback (exported for config module to use)
pub fn get_env_string(
  env_getter: fn(String) -> Result(String, Nil),
  env_var: String,
  default: String,
) -> String {
  env_getter(env_var) |> result.unwrap(default)
}

/// Get bool value from environment with fallback (exported for config module to use)
pub fn get_env_bool(
  env_getter: fn(String) -> Result(String, Nil),
  env_var: String,
  default: Bool,
) -> Bool {
  case env_getter(env_var) {
    Ok("true") | Ok("1") | Ok("yes") -> True
    Ok("false") | Ok("0") | Ok("no") -> False
    _ -> default
  }
}

/// Get int value from environment with fallback (exported for config module to use)
pub fn get_env_int(
  env_getter: fn(String) -> Result(String, Nil),
  env_var: String,
  default: Int,
) -> Int {
  case env_getter(env_var) {
    Ok(value) ->
      int.parse(value)
      |> result.unwrap(default)
    _ -> default
  }
}

/// Format a required flag error
pub fn format_required_error(flag_name: String) -> String {
  "--" <> flag_name <> ": required flag is missing or empty"
}

/// Format an enum validation error
pub fn format_enum_error(
  flag_name: String,
  value: String,
  allowed: List(String),
) -> String {
  "--"
  <> flag_name
  <> ": invalid value '"
  <> value
  <> "'. Expected one of: "
  <> string.join(allowed, ", ")
}
