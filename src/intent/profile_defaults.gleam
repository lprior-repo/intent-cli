//// Smart Profile Defaults Module
//// Reduces profile friction by auto-detecting from multiple sources
////
//// Priority order:
//// 1. CLI flag (--profile) - explicit user intent
//// 2. .intent/config.yaml - project-level configuration
//// 3. Git remote detection - repository type inference
//// 4. Default 'api' - fallback
////
//// Also supports mid-interview profile changes

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/interview.{type Profile}
import simplifile

// =============================================================================
// PUBLIC TYPES
// =============================================================================

/// Profile source for debugging and logging
pub type ProfileSource {
  CliFlag
  ConfigFile
  GitRemote
  Default
}

// =============================================================================
// GIT REMOTE DETECTION
// =============================================================================

/// Detect profile from git remote URL
/// Returns Error if unable to detect or no git remote exists
pub fn detect_profile_from_git_remote(
  remote_url: String,
) -> Result(Profile, String) {
  case remote_url {
    "" -> Error("No git remote found")
    _ -> {
      let url_lower = string.lowercase(remote_url)

      // CLI detection: github.com/cli/*, gh-cli, cli-tools
      let cli_patterns = [
        "github.com/cli/",
        "github.com/*cli*",
        "*cli-tool*",
        "*command-line*",
      ]

      // API detection: *api*, github.com/*api*
      let api_patterns = ["*api*", "*rest*", "*endpoint*"]

      // Event detection: *event*, *event-driven*, *bus*, *messaging*
      let event_patterns = [
        "*event*",
        "*bus*",
        "*messaging*",
        "*kafka*",
        "*rabbitmq*",
      ]

      // Workflow detection: *workflow*, *automation*, *pipeline*
      let workflow_patterns = [
        "*workflow*",
        "*automation*",
        "*pipeline*",
        "*orchestrat*",
      ]

      // Data detection: data-*, *data-pipeline*, *etl*
      let data_patterns = ["data-*", "*data-pipeline*", "*etl*"]

      // UI detection: frontend-, ui-, *web-app*, *spa*
      let ui_patterns = [
        "frontend-*",
        "ui-*",
        "*web-app*",
        "*spa*",
        "*dashboard*",
      ]

      // Check patterns in priority order
      case check_patterns(url_lower, cli_patterns) {
        True -> Ok(interview.Cli)
        False ->
          case check_patterns(url_lower, api_patterns) {
            True -> Ok(interview.Api)
            False ->
              case check_patterns(url_lower, event_patterns) {
                True -> Ok(interview.Event)
                False ->
                  case check_patterns(url_lower, workflow_patterns) {
                    True -> Ok(interview.Workflow)
                    False ->
                      case check_patterns(url_lower, data_patterns) {
                        True -> Ok(interview.Data)
                        False ->
                          case check_patterns(url_lower, ui_patterns) {
                            True -> Ok(interview.UI)
                            False ->
                              Error(
                                "Could not detect profile from git remote: "
                                <> remote_url,
                              )
                          }
                      }
                  }
              }
          }
      }
    }
  }
}

/// Check if URL matches any of the given patterns
fn check_patterns(url: String, patterns: List(String)) -> Bool {
  list.any(patterns, fn(pattern) {
    glob_match(url, pattern)
  })
}

/// Simple glob pattern matching
/// Supports * wildcard only
fn glob_match(text: String, pattern: String) -> Bool {
  case string.split(pattern, "*") {
    [] -> True
    [single] -> text == single
    parts -> {
      // Check if text starts with first part
      let first = list.first(parts)
      let rest = list.drop(parts, 1)
      let last = list.last(rest)

      case first, last {
        Ok(f), Ok(l) -> {
          string.starts_with(text, f)
          && string.ends_with(text, l)
          && contains_all(text, rest)
        }
        _, _ -> False
      }
    }
  }
}

/// Check if text contains all the given substrings
fn contains_all(text: String, substrings: List(String)) -> Bool {
  list.all(substrings, fn(sub) { string.contains(text, sub) })
}

// =============================================================================
// CONFIG FILE DETECTION
// =============================================================================

/// Parse profile from YAML config content
/// Expects simple format: "profile: api"
pub fn parse_profile_from_config(
  config_content: String,
) -> Result(Profile, String) {
  case string.is_empty(config_content) {
    True -> Error("Config file is empty")
    False -> {
      // Simple YAML parsing for "profile: <value>" pattern
      let lines = string.split(config_content, "\n")

      let profile_line =
        list.find_map(lines, fn(line) {
          let trimmed = string.trim(line)
          case string.starts_with(trimmed, "profile:") {
            True -> {
              let after_colon =
                string.drop_left(trimmed, 8)
                |> string.trim

              case after_colon {
                "" -> Error(Nil)
                _ -> Ok(after_colon)
              }
            }
            False -> Error(Nil)
          }
        })

      case profile_line {
        Ok(profile_str) -> parse_profile_string(profile_str)
        Error(_) -> Error("No 'profile:' field found in config")
      }
    }
  }
}

/// Load profile from .intent/config.yaml
pub fn load_profile_from_config() -> Result(Profile, String) {
  let config_path = ".intent/config.yaml"

  case simplifile.read(config_path) {
    Ok(content) -> parse_profile_from_config(content)
    Error(_) -> Error("Config file not found or unreadable")
  }
}

// =============================================================================
// PROFILE RESOLUTION
// =============================================================================

/// Get effective profile from all available sources
/// Priority: cli_flag > config_override > git_remote_profile > default
pub fn get_effective_profile(
  config_override config_override: Option(Profile),
  git_remote_profile git_remote_profile: Option(Profile),
  cli_flag cli_flag: Option(Profile),
  default default: Profile,
) -> Profile {
  case cli_flag {
    Some(profile) -> profile
    None -> {
      case config_override {
        Some(profile) -> profile
        None -> {
          case git_remote_profile {
            Some(profile) -> profile
            None -> default
          }
        }
      }
    }
  }
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/// Parse profile string to Profile type
pub fn parse_profile_string(profile_str: String) -> Result(Profile, String) {
  case string.lowercase(string.trim(profile_str)) {
    "api" -> Ok(interview.Api)
    "cli" -> Ok(interview.Cli)
    "event" -> Ok(interview.Event)
    "data" -> Ok(interview.Data)
    "workflow" -> Ok(interview.Workflow)
    "ui" -> Ok(interview.UI)
    _ ->
      Error(
        "Unknown profile '"
        <> profile_str
        <> "'. Valid profiles: api, cli, event, data, workflow, ui",
      )
  }
}

/// Convert Profile to string
pub fn profile_to_string(profile: Profile) -> String {
  case profile {
    interview.Api -> "api"
    interview.Cli -> "cli"
    interview.Event -> "event"
    interview.Data -> "data"
    interview.Workflow -> "workflow"
    interview.UI -> "ui"
  }
}
