/// Global configuration management with environment variable support
///
/// This module provides:
/// - Config record for common settings
/// - Environment variable loading
/// - Configuration merging (env vars + flags)
/// - Getter functions for config values
import gleam/string
import intent/cli_flags

/// Global configuration for Intent CLI
pub type Config {
  Config(
    target_url: String,
    allow_localhost: Bool,
    profile: String,
    output_file: String,
    timeout_ms: Int,
  )
}

/// Default configuration values
pub fn default() -> Config {
  Config(
    target_url: "",
    allow_localhost: False,
    profile: "api",
    output_file: "",
    timeout_ms: 30_000,
  )
}

/// Load configuration from environment variables using cli_flags helpers
pub fn load_from_env(env_getter: fn(String) -> Result(String, Nil)) -> Config {
  Config(
    target_url: cli_flags.get_env_string(env_getter, "INTENT_TARGET", ""),
    allow_localhost: cli_flags.get_env_bool(
      env_getter,
      "INTENT_ALLOW_LOCALHOST",
      False,
    ),
    profile: cli_flags.get_env_string(env_getter, "INTENT_PROFILE", "api"),
    output_file: cli_flags.get_env_string(env_getter, "INTENT_OUTPUT", ""),
    timeout_ms: cli_flags.get_env_int(env_getter, "INTENT_TIMEOUT_MS", 30_000),
  )
}

/// Merge configuration with flag overrides
/// Non-empty strings and true booleans from overrides take precedence
pub fn merge_with_flags(base: Config, overrides: Config) -> Config {
  Config(
    target_url: case string.is_empty(overrides.target_url) {
      True -> base.target_url
      False -> overrides.target_url
    },
    allow_localhost: overrides.allow_localhost || base.allow_localhost,
    profile: case string.is_empty(overrides.profile) {
      True -> base.profile
      False -> overrides.profile
    },
    output_file: case string.is_empty(overrides.output_file) {
      True -> base.output_file
      False -> overrides.output_file
    },
    timeout_ms: overrides.timeout_ms,
  )
}

/// Create config from flag values
pub fn from_flags(
  target_url: String,
  allow_localhost: Bool,
  profile: String,
  output_file: String,
  timeout_ms: Int,
) -> Config {
  Config(
    target_url: target_url,
    allow_localhost: allow_localhost,
    profile: profile,
    output_file: output_file,
    timeout_ms: timeout_ms,
  )
}

/// Validate that target URL is set
pub fn validate_target_required(config: Config) -> Result(Nil, String) {
  case string.is_empty(config.target_url) {
    True ->
      Error(
        "--target: required flag is missing or empty. Set via --target=<url> or INTENT_TARGET environment variable",
      )
    False -> Ok(Nil)
  }
}

/// Check if configuration has valid target URL
pub fn has_target(config: Config) -> Bool {
  !string.is_empty(config.target_url)
}

/// Check if localhost is allowed
pub fn is_localhost_allowed(config: Config) -> Bool {
  config.allow_localhost
}
