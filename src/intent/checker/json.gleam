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
      case json.decode(json_str, dynamic.dict(dynamic.string, dynamic.dynamic)) {
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

/// Convert JSON to raw string (strips quotes from strings)
pub fn json_to_raw_string(value: Json) -> String {
  let encoded = json.to_string(value)
  // Remove quotes from strings
  case string.starts_with(encoded, "\"") && string.ends_with(encoded, "\"") {
    True ->
      encoded
      |> string.drop_left(1)
      |> string.drop_right(1)
    False -> encoded
  }
}

/// Convert JSON to display string
pub fn json_to_display_string(value: Json) -> String {
  json.to_string(value)
}
