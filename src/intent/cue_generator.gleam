/// CUE specification generation from Spec types
/// Reverse parser that generates CUE code from parsed Spec structures

import gleam/dict
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import intent/types.{type Spec, type Behavior, type Feature, type Rule, type Method, Get, Post, Put, Patch, Delete, Head, Options}

/// Generated CUE code
pub type GeneratedCUE {
  GeneratedCUE(
    package: String,
    imports: List(String),
    body: String,
  )
}

/// Generate CUE code from a Spec
pub fn spec_to_cue(spec: Spec) -> GeneratedCUE {
  let package_line = "package api"

  let imports = generate_imports(spec)

  let features_lines =
    spec.features
    |> list.map(feature_to_cue)
    |> string.join("\\n\\n")

  let rules_lines = case list.is_empty(spec.rules) {
    True -> ""
    False ->
      "\\n\\n// Global validation rules\\n" <> {
        spec.rules
        |> list.map(rule_to_cue)
        |> string.join("\\n\\n")
      }
  }

  let anti_patterns_lines = case list.is_empty(spec.anti_patterns) {
    True -> ""
    False ->
      "\\n\\n// Anti-patterns to avoid\\n" <> {
        spec.anti_patterns
        |> list.map(fn(ap) {
          "anti_pattern \\\"" <> ap.name <> "\\\": {
  description: \\\"" <> ap.description <> "\\\"
  bad_example: " <> json_to_cue(ap.bad_example) <> "
}"
        })
        |> string.join("\\n\\n")
      }
  }

  let body =
    features_lines
    <> rules_lines
    <> anti_patterns_lines

  GeneratedCUE(
    package: package_line,
    imports: imports,
    body: body,
  )
}

/// Generate imports needed for the spec
fn generate_imports(spec: Spec) -> List(String) {
  []
  |> append_if(
    has_http_behaviors(spec),
    ["import (", "\\t\\\"net/http\\\"", ")"],
  )
  |> append_if(
    has_regex_in_rules(spec),
    ["import \\\"regexp\\\""],
  )
  |> append_if(
    has_time_rules(spec),
    ["import \\\"time\\\""],
  )
}

/// Check if spec has HTTP behaviors
fn has_http_behaviors(spec: Spec) -> Bool {
  spec.features
  |> list.any(fn(f) {
    f.behaviors
    |> list.any(fn(_b) { True })
  })
}

/// Check if spec has regex patterns in rules
fn has_regex_in_rules(spec: Spec) -> Bool {
  spec.rules
  |> list.any(fn(_rule) {
    False
  })
}

/// Check if spec has time-based rules
fn has_time_rules(spec: Spec) -> Bool {
  spec.rules
  |> list.any(fn(_rule) {
    False
  })
}

/// Convert a Feature to CUE code
fn feature_to_cue(feature: Feature) -> String {
  "feature \\\"" <> feature.name <> "\\\": {
  description: \\\"" <> feature.description <> "\\\"

" <> {
    feature.behaviors
    |> list.map(behavior_to_cue)
    |> string.join("\\n\\n")
  } <> "
}"
}

/// Convert a Behavior to CUE code
fn behavior_to_cue(behavior: Behavior) -> String {
  let method = method_to_string(behavior.request.method)

  let path = behavior.request.path

  let intent_line = case string.is_empty(behavior.intent) {
    True -> ""
    False -> "  intent: \\\"" <> behavior.intent <> "\\\"\\n"
  }

  let notes_line = case string.is_empty(behavior.notes) {
    True -> ""
    False -> "  notes: \\\"" <> behavior.notes <> "\\\"\\n"
  }

  let request_line = "  request: {\\n    method: \\\"" <> method <> "\\\"\\n    path: \\\"" <> path <> "\\\"\\n"

  let headers_line = case dict.is_empty(behavior.request.headers) {
    True -> ""
    False ->
      "    headers: {\\n" <> {
        behavior.request.headers
        |> dict.to_list
        |> list.map(fn(pair) {
          let #(k, v) = pair
          "      \\\"" <> k <> "\\\": \\\"" <> v <> "\\\""
        })
        |> string.join("\\n")
      } <> "\\n    }\\n"
  }

  let request_close = "  }\\n"

  let response_line = "  response: {\\n    status: " <> int.to_string(behavior.response.status) <> "\\n"

  let example_line = case behavior.response.example {
    None -> ""
    Some(ex) ->
      "    example: " <> json_to_cue(ex) <> "\\n"
  }

  let checks_lines = case dict.is_empty(behavior.response.checks) {
    True -> ""
    False ->
      "    checks: {\\n" <> {
        behavior.response.checks
        |> dict.to_list
        |> list.map(fn(pair) {
          let #(field, check) = pair
          "      \\\"" <> field <> "\\\": {\\n        rule: \\\"" <> check.rule <> "\\\"" <> {
            case string.is_empty(check.why) {
              True -> ""
              False -> "\\n        why: \\\"" <> check.why <> "\\\""
            }
          } <> "\\n      }"
        })
        |> string.join("\\n")
      } <> "\\n    }\\n"
  }

  let response_close = "  }\\n"

  let requires_line = case list.is_empty(behavior.requires) {
    True -> ""
    False ->
      "  requires: [" <> {
        behavior.requires
        |> list.map(fn(b) { "\\\"" <> b <> "\\\"" })
        |> string.join(", ")
      } <> "]\\n"
  }

  let captures_line = case dict.is_empty(behavior.captures) {
    True -> ""
    False ->
      "  captures: {\\n" <> {
        behavior.captures
        |> dict.to_list
        |> list.map(fn(pair) {
          let #(name, path) = pair
          "    \\\"" <> name <> "\\\": \\\"" <> path <> "\\\""
        })
        |> string.join("\\n")
      } <> "\\n  }\\n"
  }

  let behaviors_name = string.replace(behavior.name, "-", "_")

  behaviors_name <> ": {\\n" <> intent_line <> notes_line <> request_line <> headers_line <> request_close <> response_line <> example_line <> checks_lines <> response_close <> requires_line <> captures_line <> "}"
}

/// Convert HTTP Method to string
fn method_to_string(method: Method) -> String {
  case method {
    Get -> "GET"
    Post -> "POST"
    Put -> "PUT"
    Patch -> "PATCH"
    Delete -> "DELETE"
    Head -> "HEAD"
    Options -> "OPTIONS"
  }
}

/// Convert a Rule to CUE code
fn rule_to_cue(rule: Rule) -> String {
  "rule \\\"" <> rule.name <> "\\\": {
  description: \\\"" <> rule.description <> "\\\"
}"
}

/// Convert JSON to CUE representation
fn json_to_cue(json: Json) -> String {
  json.to_string(json)
}

/// Format generated CUE for output
pub fn format_cue(generated: GeneratedCUE) -> String {
  let import_section = case list.is_empty(generated.imports) {
    True -> ""
    False -> string.join(generated.imports, "\\n") <> "\\n\\n"
  }

  generated.package <> "\\n\\n" <> import_section <> generated.body
}

/// Helper to conditionally append items
fn append_if(
  items: List(String),
  condition: Bool,
  to_append: List(String),
) -> List(String) {
  case condition {
    True -> list.append(items, to_append)
    False -> items
  }
}
