/// Variable interpolation for captured values
/// Handles ${variable} syntax in strings
/// Supports array indexing: ${items[0].id}, ${array[-1]}, etc.

import gleam/dict.{type Dict}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import intent/array_indexing

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
  let pattern = "\\$\\{([^}]+)\\}"
  case regexp.from_string(pattern) {
    Ok(re) -> {
      let matches = regexp.scan(re, s)
      interpolate_matches(ctx, s, matches)
    }
    Error(_) -> Ok(s)
  }
}

fn interpolate_matches(
  ctx: Context,
  s: String,
  matches: List(regexp.Match),
) -> Result(String, String) {
  case matches {
    [] -> Ok(s)
    [match, ..rest] -> {
      case match.submatches {
        [Some(var_path)] -> {
          case resolve_path(ctx, var_path) {
            Ok(value) -> {
              let value_str = json_to_string(value)
              let new_s = string.replace(s, match.content, value_str)
              interpolate_matches(ctx, new_s, rest)
            }
            Error(e) -> Error(e)
          }
        }
        _ -> interpolate_matches(ctx, s, rest)
      }
    }
  }
}

/// Resolve a variable path like "response.body.id" or "user_id"
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
    [var_name] ->
      case get_variable(ctx, var_name) {
        Some(value) -> Ok(value)
        None -> Error("Variable not found: " <> var_name)
      }
    [var_name, ..rest] ->
      case get_variable(ctx, var_name) {
        Some(value) -> navigate_json(value, rest)
        None -> Error("Variable not found: " <> var_name)
      }
    [] -> Error("Empty variable path")
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
