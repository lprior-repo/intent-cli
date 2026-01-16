/// Configuration file (.intentrc) support
/// Provides configuration from .intentrc with fallback to environment variables
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/json
import simplifile

/// Configuration loaded from .intentrc file
pub type IntentConfig {
  IntentConfig(values: Dict(String, Dynamic))
}

/// Load .intentrc from current directory
/// Returns Error if file doesn't exist or can't be parsed
pub fn load_intentrc() -> Result(IntentConfig, String) {
  case simplifile.read(".intentrc") {
    Error(_) -> Error("No .intentrc file found")
    Ok(content) -> parse_intentrc(content)
  }
}

/// Parse .intentrc content as JSON
fn parse_intentrc(content: String) -> Result(IntentConfig, String) {
  case json.decode(content, dynamic.dict(dynamic.string, dynamic.dynamic)) {
    Ok(values) -> Ok(IntentConfig(values: values))
    Error(_) -> Error("Failed to parse .intentrc as JSON")
  }
}

/// Get string value from config with fallback to default
pub fn get_string(
  config: Result(IntentConfig, String),
  key: String,
  default: String,
) -> String {
  case config {
    Error(_) -> default
    Ok(cfg) -> {
      case dict.get(cfg.values, key) {
        Error(_) -> default
        Ok(value) -> {
          case dynamic.string(value) {
            Ok(str) -> str
            Error(_) -> default
          }
        }
      }
    }
  }
}

/// Get boolean value from config with fallback to default
pub fn get_bool(
  config: Result(IntentConfig, String),
  key: String,
  default: Bool,
) -> Bool {
  case config {
    Error(_) -> default
    Ok(cfg) -> {
      case dict.get(cfg.values, key) {
        Error(_) -> default
        Ok(value) -> {
          case dynamic.bool(value) {
            Ok(b) -> b
            Error(_) -> default
          }
        }
      }
    }
  }
}
