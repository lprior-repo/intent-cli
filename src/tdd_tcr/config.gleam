/// Configuration for TDD-TCR loop
/// Centralized settings matching Bash script parameters

import gleam/option.{type Option, None}

pub type Config {
  Config(
    project_lang: String,
    // ^ Project language: "gleam"
    test_cmd: String,
    // ^ Command to run tests: "gleam test"
    build_cmd: String,
    // ^ Command to build: "gleam build"
    requirements: String,
    // ^ Feature/requirement to implement
    bead_id: Option(String),
    // ^ Optional linked Beads issue ID
    max_iterations: Int,
    // ^ Stop after this many iterations (default: 20)
    refactor_interval: Int,
    // ^ Refactor after every N successful implementations (default: 5)
    max_impl_attempts: Int,
    // ^ Implementer tries this many times per test (default: 3)
    escalation_threshold: Int,
    // ^ Switch to Opus after N consecutive failures (default: 3)
    model_default: String,
    // ^ Default model: "sonnet"
    model_escalated: String,
    // ^ Escalation model: "opus"
  )
}

/// Default configuration
pub fn default_config() -> Config {
  Config(
    project_lang: "gleam",
    test_cmd: "gleam test",
    build_cmd: "gleam build",
    requirements: "",
    bead_id: None,
    max_iterations: 20,
    refactor_interval: 5,
    max_impl_attempts: 3,
    escalation_threshold: 3,
    model_default: "sonnet",
    model_escalated: "opus",
  )
}

/// Create config from requirements string
pub fn from_requirements(requirements: String) -> Config {
  let default = default_config()
  Config(..default, requirements: requirements)
}

/// Create config from requirements and optional bead ID
pub fn from_requirements_with_bead(
  requirements: String,
  bead_id: Option(String),
) -> Config {
  let default = default_config()
  Config(..default, requirements: requirements, bead_id: bead_id)
}

/// Validate configuration
pub fn validate(config: Config) -> Result(Config, String) {
  case config.requirements {
    "" -> Error("Requirements cannot be empty")
    _ -> Ok(config)
  }
}
