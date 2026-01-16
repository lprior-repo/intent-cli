/// Field validation - extracts and validates field values from JSON responses
import gleam/dict
import gleam/dynamic
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/parser

/// Get a field value from JSON using dot notation
pub fn get_field_value(body: Json, field: String) -> Option(Json) {
  navigate_json_path(body, string.split(field, "."))
}

/// Navigate through JSON using a path of keys
fn navigate_json_path(value: Json, path: List(String)) -> Option(Json) {
  case path {
    [] -> Some(value)
    [key, ..rest] -> {
      // Need to decode the JSON to navigate it
      let json_str = json.to_string(value)
      case
        json.decode(json_str, dynamic.dict(dynamic.string, dynamic.dynamic))
      {
        Ok(obj) ->
          case dict.get(obj, key) {
            Ok(next) -> {
              let next_json = parser.dynamic_to_json(next)
              navigate_json_path(next_json, rest)
            }
            Error(_) -> None
          }
        Error(_) -> None
      }
    }
  }
}

/// Convert JSON to raw string (strips quotes and unescapes JSON escape sequences)
pub fn json_to_raw_string(value: Json) -> String {
  let encoded = json.to_string(value)
  // Remove quotes from strings and unescape
  case string.starts_with(encoded, "\"") && string.ends_with(encoded, "\"") {
    True -> {
      let without_quotes =
        encoded
        |> string.drop_left(1)
        |> string.drop_right(1)
      unescape_json_string(without_quotes)
    }
    False -> encoded
  }
}

/// Unescape JSON escape sequences in a string
/// Handles: \", \\, \/, \b, \f, \n, \r, \t
/// Note: \uXXXX unicode escapes are not currently supported
fn unescape_json_string(s: String) -> String {
  s
  // First, protect escaped backslashes by replacing with placeholder
  // This prevents \\n from becoming \n then newline
  |> string.replace("\\\\", "\u{0000}")
  // Now replace all other escape sequences
  |> string.replace("\\\"", "\"")
  |> string.replace("\\/", "/")
  |> string.replace("\\b", "\u{0008}")
  |> string.replace("\\f", "\u{000C}")
  |> string.replace("\\n", "\n")
  |> string.replace("\\r", "\r")
  |> string.replace("\\t", "\t")
  // Finally, replace placeholder back to single backslash
  |> string.replace("\u{0000}", "\\")
}

/// Convert JSON to display string
pub fn json_to_display_string(value: Json) -> String {
  json.to_string(value)
}
