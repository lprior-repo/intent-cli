/// Configuration file (.intentrc) support with inheritance
/// Supports config hierarchy: project → user → system
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/result
import simplifile

@external(erlang, "intent_ffi", "get_home_dir")
fn get_home_dir() -> Result(String, Nil)

/// Configuration with inheritance support
pub type IntentConfig {
  IntentConfig(
    project: Dict(String, Dynamic),
    user: Dict(String, Dynamic),
    system: Dict(String, Dynamic),
  )
}

/// Load configuration with inheritance (project → user → system)
/// Returns config with all available levels loaded
pub fn load_intentrc() -> Result(IntentConfig, String) {
  let project = load_config_file(".intentrc")
  let user = case get_home_dir() {
    Ok(home) -> {
      // Try ~/.config/intent/config first, then ~/.intentrc
      case load_config_file(home <> "/.config/intent/config") {
        Ok(cfg) -> Ok(cfg)
        Error(_) -> load_config_file(home <> "/.intentrc")
      }
    }
    Error(_) -> Error("No home directory")
  }
  let system = load_config_file("/etc/intentrc")

  // Return config even if all files are missing (empty dicts)
  Ok(IntentConfig(
    project: result.unwrap(project, dict.new()),
    user: result.unwrap(user, dict.new()),
    system: result.unwrap(system, dict.new()),
  ))
}

/// Load a single config file
fn load_config_file(path: String) -> Result(Dict(String, Dynamic), String) {
  case simplifile.read(path) {
    Error(_) -> Error("File not found: " <> path)
    Ok(content) -> parse_config_content(content)
  }
}

/// Parse config content as JSON
fn parse_config_content(
  content: String,
) -> Result(Dict(String, Dynamic), String) {
  case json.decode(content, dynamic.dict(dynamic.string, dynamic.dynamic)) {
    Ok(values) -> Ok(values)
    Error(_) -> Error("Failed to parse config as JSON")
  }
}

/// Get string value from config with inheritance (project → user → system)
pub fn get_string(
  config: Result(IntentConfig, String),
  key: String,
  default: String,
) -> String {
  case config {
    Error(_) -> default
    Ok(cfg) -> {
      // Try project config first
      case dict.get(cfg.project, key) {
        Ok(value) ->
          case dynamic.string(value) {
            Ok(str) -> str
            Error(_) -> check_user_then_system_string(cfg, key, default)
          }
        Error(_) -> check_user_then_system_string(cfg, key, default)
      }
    }
  }
}

/// Helper to check user then system config for string
fn check_user_then_system_string(
  cfg: IntentConfig,
  key: String,
  default: String,
) -> String {
  // Try user config
  case dict.get(cfg.user, key) {
    Ok(value) ->
      case dynamic.string(value) {
        Ok(str) -> str
        Error(_) -> check_system_string(cfg, key, default)
      }
    Error(_) -> check_system_string(cfg, key, default)
  }
}

/// Helper to check system config for string
fn check_system_string(
  cfg: IntentConfig,
  key: String,
  default: String,
) -> String {
  case dict.get(cfg.system, key) {
    Ok(value) ->
      case dynamic.string(value) {
        Ok(str) -> str
        Error(_) -> default
      }
    Error(_) -> default
  }
}

/// Get boolean value from config with inheritance (project → user → system)
pub fn get_bool(
  config: Result(IntentConfig, String),
  key: String,
  default: Bool,
) -> Bool {
  case config {
    Error(_) -> default
    Ok(cfg) -> {
      // Try project config first
      case dict.get(cfg.project, key) {
        Ok(value) ->
          case dynamic.bool(value) {
            Ok(b) -> b
            Error(_) -> check_user_then_system_bool(cfg, key, default)
          }
        Error(_) -> check_user_then_system_bool(cfg, key, default)
      }
    }
  }
}

/// Helper to check user then system config for bool
fn check_user_then_system_bool(
  cfg: IntentConfig,
  key: String,
  default: Bool,
) -> Bool {
  // Try user config
  case dict.get(cfg.user, key) {
    Ok(value) ->
      case dynamic.bool(value) {
        Ok(b) -> b
        Error(_) -> check_system_bool(cfg, key, default)
      }
    Error(_) -> check_system_bool(cfg, key, default)
  }
}

/// Helper to check system config for bool
fn check_system_bool(cfg: IntentConfig, key: String, default: Bool) -> Bool {
  case dict.get(cfg.system, key) {
    Ok(value) ->
      case dynamic.bool(value) {
        Ok(b) -> b
        Error(_) -> default
      }
    Error(_) -> default
  }
}
