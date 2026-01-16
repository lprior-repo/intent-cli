/// Variable interpolation for captured values
/// Handles ${variable} syntax in strings
/// Supports array indexing: ${items[0].id}, ${array[-1]}, etc.

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import intent/array_indexing
import intent/parser

/// Context containing captured variables
pub type Context {
  Context(
    variables: Dict(String, Json),
    request_body: Option(Json),
    response_body: Option(Json),
  )
}

/// Create a new empty context
pub fn new_context() -> Context {
  Context(variables: dict.new(), request_body: None, response_body: None)
}

/// Add a captured value to the context
pub fn set_variable(ctx: Context, name: String, value: Json) -> Context {
  Context(..ctx, variables: dict.insert(ctx.variables, name, value))
}

/// Set the request body in context
pub fn set_request_body(ctx: Context, body: Json) -> Context {
  Context(..ctx, request_body: Some(body))
}

/// Set the response body in context
pub fn set_response_body(ctx: Context, body: Json) -> Context {
  Context(..ctx, response_body: Some(body))
}

/// Get a variable value from context
pub fn get_variable(ctx: Context, name: String) -> Option(Json) {
  dict.get(ctx.variables, name)
  |> option.from_result
}

/// Interpolate variables in a string
/// Replaces ${var_name} with the stringified value of the variable
pub fn interpolate_string(ctx: Context, s: String) -> Result(String, String) {
  interpolate_string_with_depth(ctx, s, 0, [])
}

fn interpolate_string_with_depth(
  ctx: Context,
  s: String,
  depth: Int,
  visited: List(String),
) -> Result(String, String) {
  // Check depth limit to prevent infinite recursion
  case depth > 10 {
    True -> Error("Variable interpolation depth limit exceeded")
    False -> {
      let pattern = "\\$\\{([^}]+)\\}"
      case regexp.from_string(pattern) {
        Ok(re) -> {
          let matches = regexp.scan(re, s)
          interpolate_matches_with_depth(ctx, s, matches, depth, visited)
        }
        Error(_) -> Ok(s)
      }
    }
  }
}

fn interpolate_matches_with_depth(
  ctx: Context,
  s: String,
  matches: List(regexp.Match),
  depth: Int,
  visited: List(String),
) -> Result(String, String) {
  case matches {
    [] -> Ok(s)
    [match, ..rest] -> {
      case match.submatches {
        [Some(var_path)] -> {
          // Check for circular reference
          case list.contains(visited, var_path) {
            True -> Error("Circular variable reference detected: " <> var_path)
            False -> {
              case resolve_path_with_depth(ctx, var_path, depth + 1, [var_path, ..visited]) {
                Ok(value) -> {
                  let value_str = json_to_string(value)
                  let new_s = string.replace(s, match.content, value_str)
                  interpolate_matches_with_depth(ctx, new_s, rest, depth, visited)
                }
                Error(e) -> Error(e)
              }
            }
          }
        }
        _ -> interpolate_matches_with_depth(ctx, s, rest, depth, visited)
      }
    }
  }
}

/// Resolve a path with depth tracking for cycle detection
fn resolve_path_with_depth(
  ctx: Context,
  path: String,
  depth: Int,
  visited: List(String),
) -> Result(Json, String) {
  case resolve_path(ctx, path) {
    Ok(value) -> {
      // If the value is a string, try to interpolate it recursively
      let value_str = json.to_string(value)
      case string.starts_with(value_str, "\"") && string.contains(value_str, "${") {
        True -> {
          // It's a JSON string that might contain variables
          // Remove quotes and interpolate
          let unquoted =
            value_str
            |> string.drop_left(1)
            |> string.drop_right(1)

          case interpolate_string_with_depth(ctx, unquoted, depth, visited) {
            Ok(interpolated) -> Ok(json.string(interpolated))
            Error(e) -> Error(e)
          }
        }
        False -> Ok(value)
      }
    }
    Error(e) -> Error(e)
  }
}

/// Resolve a variable path like "response.body.id" or "user_id" or "items[0].name"
fn resolve_path(ctx: Context, path: String) -> Result(Json, String) {
  let parts = string.split(path, ".")

  case parts {
    ["request", "body", ..rest] ->
      case ctx.request_body {
        Some(body) -> navigate_json(body, rest)
        None -> Error("No request body in context")
      }
    ["response", "body", ..rest] ->
      case ctx.response_body {
        Some(body) -> navigate_json(body, rest)
        None -> Error("No response body in context")
      }
    [first_part, ..rest] -> {
      // Parse the first part to check for array indexing
      case array_indexing.parse_path_component(first_part) {
        Ok(#(var_name, array_spec)) -> {
          case get_variable(ctx, var_name) {
            Some(value) -> {
              // Apply array indexing if present
              case array_spec {
                array_indexing.NoArray -> {
                  // No array index, just navigate the rest
                  case rest {
                    [] -> Ok(value)
                    _ -> navigate_json(value, rest)
                  }
                }
                array_indexing.Index(idx) -> {
                  // Apply positive array index
                  case get_array_element_from_json(value, idx) {
                    Ok(elem) -> {
                      case rest {
                        [] -> Ok(elem)
                        _ -> navigate_json(elem, rest)
                      }
                    }
                    Error(e) -> Error(e)
                  }
                }
                array_indexing.LastN(n) -> {
                  // Apply negative array index
                  case get_array_element_last_from_json(value, n) {
                    Ok(elem) -> {
                      case rest {
                        [] -> Ok(elem)
                        _ -> navigate_json(elem, rest)
                      }
                    }
                    Error(e) -> Error(e)
                  }
                }
                array_indexing.All -> {
                  Error("Array wildcard [*] not supported in variable paths")
                }
              }
            }
            None -> Error("Variable not found: " <> var_name)
          }
        }
        Error(e) -> Error(e)
      }
    }
    [] -> Error("Empty variable path")
  }
}

/// Get array element by positive index from JSON
fn get_array_element_from_json(json: Json, index: Int) -> Result(Json, String) {
  let json_str = json.to_string(json)
  case json.decode(json_str, dynamic.list(dynamic.dynamic)) {
    Ok(lst) -> {
      case list.drop(lst, index) |> list.first {
        Ok(elem) -> {
          let json_val = parser.dynamic_to_json(elem)
          Ok(json_val)
        }
        Error(_) ->
          Error(
            "Array index " <> int.to_string(index) <> " out of bounds (length: " <> int.to_string(list.length(lst)) <> ")",
          )
      }
    }
    Error(_) -> Error("Cannot index non-array with [" <> int.to_string(index) <> "]")
  }
}

/// Get array element by negative index from JSON
fn get_array_element_last_from_json(json: Json, from_end: Int) -> Result(Json, String) {
  let json_str = json.to_string(json)
  case json.decode(json_str, dynamic.list(dynamic.dynamic)) {
    Ok(lst) -> {
      let length = list.length(lst)
      let actual_index = length - from_end
      case actual_index >= 0 && actual_index < length {
        False ->
          Error(
            "Array index -" <> int.to_string(from_end) <> " out of bounds (length: " <> int.to_string(length) <> ")",
          )
        True -> {
          case list.drop(lst, actual_index) |> list.first {
            Ok(elem) -> {
              let json_val = parser.dynamic_to_json(elem)
              Ok(json_val)
            }
            Error(_) -> Error("Failed to access array element")
          }
        }
      }
    }
    Error(_) -> Error("Cannot index non-array with negative index")
  }
}

/// Navigate into a JSON value using a path with array indexing support
/// Supports: field, field[0], field[-1], field.nested[0].value
fn navigate_json(value: Json, path: List(String)) -> Result(Json, String) {
  case path {
    [] -> Ok(value)
    components -> {
      // Use array_indexing module for full path navigation with array support
      array_indexing.navigate_path(value, components)
    }
  }
}

/// Convert a JSON value to a string representation
pub fn json_to_string(value: Json) -> String {
  // For simple values, we want the raw value not JSON-encoded
  let encoded = json.to_string(value)
  // If it's a string, remove the quotes
  case string.starts_with(encoded, "\"") && string.ends_with(encoded, "\"") {
    True ->
      encoded
      |> string.drop_left(1)
      |> string.drop_right(1)
    False -> encoded
  }
}

/// Interpolate variables in headers dict
pub fn interpolate_headers(
  ctx: Context,
  headers: Dict(String, String),
) -> Result(Dict(String, String), String) {
  headers
  |> dict.to_list
  |> list.try_map(fn(pair) {
    let #(key, value) = pair
    case interpolate_string(ctx, value) {
      Ok(new_value) -> Ok(#(key, new_value))
      Error(e) -> Error(e)
    }
  })
  |> result.map(dict.from_list)
}

/// Extract a value from JSON using a capture path like "response.body.id"
pub fn extract_capture(
  ctx: Context,
  capture_path: String,
) -> Result(Json, String) {
  resolve_path(ctx, capture_path)
}
