// KIRK EARS Parser
// Easy Approach to Requirements Syntax
// Converts structured English requirements to Intent behaviors

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regex
import gleam/string

// =============================================================================
// TYPES
// =============================================================================

/// EARS requirement types based on the original taxonomy
pub type EarsRequirement {
  EarsRequirement(
    id: String,
    pattern: EarsPattern,
    trigger: Option(String),
    state: Option(String),
    condition: Option(String),
    system_shall: String,
    system_shall_not: Option(String),
    raw_text: String,
  )
}

/// The five EARS patterns
pub type EarsPattern {
  /// "THE SYSTEM SHALL [behavior]"
  Ubiquitous
  /// "WHEN [trigger] THE SYSTEM SHALL [behavior]"
  EventDriven
  /// "WHILE [state] THE SYSTEM SHALL [behavior]"
  StateDriven
  /// "WHERE [condition] THE SYSTEM SHALL [behavior]"
  Optional
  /// "IF [condition] THEN THE SYSTEM SHALL NOT [behavior]"
  Unwanted
  /// Combination patterns
  Complex
}

pub type EarsParseResult {
  EarsParseResult(
    requirements: List(EarsRequirement),
    errors: List(EarsError),
    warnings: List(String),
  )
}

pub type EarsError {
  EarsError(line: Int, message: String, suggestion: String)
}

pub type IntentBehavior {
  IntentBehavior(
    name: String,
    intent: String,
    method: String,
    path: String,
    status: Int,
    preconditions: List(String),
    postconditions: List(String),
  )
}

// =============================================================================
// PARSER
// =============================================================================

/// Parse EARS requirements from text
pub fn parse(text: String) -> EarsParseResult {
  let lines =
    text
    |> string.split("\n")
    |> list.index_map(fn(line, idx) { #(idx + 1, string.trim(line)) })
    |> list.filter(fn(pair) {
      let #(_, line) = pair
      !string.is_empty(line) && !string.starts_with(line, "#")
    })

  let #(requirements, errors) =
    lines
    |> list.fold(#([], []), fn(acc, pair) {
      let #(reqs, errs) = acc
      let #(line_num, line) = pair

      case parse_line(line, line_num) {
        Ok(req) -> #([req, ..reqs], errs)
        Error(err) -> #(reqs, [err, ..errs])
      }
    })

  let warnings = generate_warnings(requirements)

  EarsParseResult(
    requirements: list.reverse(requirements),
    errors: list.reverse(errors),
    warnings: warnings,
  )
}

fn parse_line(line: String, line_num: Int) -> Result(EarsRequirement, EarsError) {
  let upper = string.uppercase(line)
  let id = "REQ-" <> int.to_string(line_num)

  // Try patterns in order of specificity
  case True {
    _ if string.contains(upper, "WHILE") && string.contains(upper, "WHEN") ->
      parse_complex(line, id)
    _ if string.contains(upper, "IF") && string.contains(upper, "SHALL NOT") ->
      parse_unwanted(line, id)
    _ if string.contains(upper, "WHEN") && string.contains(upper, "SHALL") ->
      parse_event_driven(line, id)
    _ if string.contains(upper, "WHILE") && string.contains(upper, "SHALL") ->
      parse_state_driven(line, id)
    _ if string.contains(upper, "WHERE") && string.contains(upper, "SHALL") ->
      parse_optional(line, id)
    _ if string.contains(upper, "THE SYSTEM SHALL") ->
      parse_ubiquitous(line, id)
    _ if string.contains(upper, "SHALL") ->
      parse_ubiquitous(line, id)
    _ ->
      Error(EarsError(
        line: line_num,
        message: "Line doesn't match any EARS pattern",
        suggestion: "Use patterns like 'WHEN [trigger] THE SYSTEM SHALL [behavior]'",
      ))
  }
}

/// Pattern: "THE SYSTEM SHALL [behavior]"
fn parse_ubiquitous(line: String, id: String) -> Result(EarsRequirement, EarsError) {
  let pattern_str = "(?i)(?:the\\s+)?system\\s+shall\\s+(.+)"

  case regex.from_string(pattern_str) {
    Ok(re) ->
      case regex.scan(re, line) {
        [match, ..] ->
          case match.submatches {
            [Some(behavior)] ->
              Ok(EarsRequirement(
                id: id,
                pattern: Ubiquitous,
                trigger: None,
                state: None,
                condition: None,
                system_shall: string.trim(behavior),
                system_shall_not: None,
                raw_text: line,
              ))
            _ -> Error(EarsError(line: 0, message: "Failed to extract behavior", suggestion: ""))
          }
        [] -> Error(EarsError(line: 0, message: "No match found", suggestion: ""))
      }
    Error(_) -> Error(EarsError(line: 0, message: "Invalid regex", suggestion: ""))
  }
}

/// Pattern: "WHEN [trigger] THE SYSTEM SHALL [behavior]"
fn parse_event_driven(line: String, id: String) -> Result(EarsRequirement, EarsError) {
  let pattern_str = "(?i)when\\s+(.+?)\\s+(?:the\\s+)?system\\s+shall\\s+(.+)"

  case regex.from_string(pattern_str) {
    Ok(re) ->
      case regex.scan(re, line) {
        [match, ..] ->
          case match.submatches {
            [Some(trigger), Some(behavior)] ->
              Ok(EarsRequirement(
                id: id,
                pattern: EventDriven,
                trigger: Some(string.trim(trigger)),
                state: None,
                condition: None,
                system_shall: string.trim(behavior),
                system_shall_not: None,
                raw_text: line,
              ))
            _ -> Error(EarsError(line: 0, message: "Failed to extract trigger/behavior", suggestion: ""))
          }
        [] -> Error(EarsError(line: 0, message: "No match found", suggestion: ""))
      }
    Error(_) -> Error(EarsError(line: 0, message: "Invalid regex", suggestion: ""))
  }
}

/// Pattern: "WHILE [state] THE SYSTEM SHALL [behavior]"
fn parse_state_driven(line: String, id: String) -> Result(EarsRequirement, EarsError) {
  let pattern_str = "(?i)while\\s+(.+?)\\s+(?:the\\s+)?system\\s+shall\\s+(.+)"

  case regex.from_string(pattern_str) {
    Ok(re) ->
      case regex.scan(re, line) {
        [match, ..] ->
          case match.submatches {
            [Some(state), Some(behavior)] ->
              Ok(EarsRequirement(
                id: id,
                pattern: StateDriven,
                trigger: None,
                state: Some(string.trim(state)),
                condition: None,
                system_shall: string.trim(behavior),
                system_shall_not: None,
                raw_text: line,
              ))
            _ -> Error(EarsError(line: 0, message: "Failed to extract state/behavior", suggestion: ""))
          }
        [] -> Error(EarsError(line: 0, message: "No match found", suggestion: ""))
      }
    Error(_) -> Error(EarsError(line: 0, message: "Invalid regex", suggestion: ""))
  }
}

/// Pattern: "WHERE [condition] THE SYSTEM SHALL [behavior]"
fn parse_optional(line: String, id: String) -> Result(EarsRequirement, EarsError) {
  let pattern_str = "(?i)where\\s+(.+?)\\s+(?:the\\s+)?system\\s+shall\\s+(.+)"

  case regex.from_string(pattern_str) {
    Ok(re) ->
      case regex.scan(re, line) {
        [match, ..] ->
          case match.submatches {
            [Some(condition), Some(behavior)] ->
              Ok(EarsRequirement(
                id: id,
                pattern: Optional,
                trigger: None,
                state: None,
                condition: Some(string.trim(condition)),
                system_shall: string.trim(behavior),
                system_shall_not: None,
                raw_text: line,
              ))
            _ -> Error(EarsError(line: 0, message: "Failed to extract condition/behavior", suggestion: ""))
          }
        [] -> Error(EarsError(line: 0, message: "No match found", suggestion: ""))
      }
    Error(_) -> Error(EarsError(line: 0, message: "Invalid regex", suggestion: ""))
  }
}

/// Pattern: "IF [condition] THEN THE SYSTEM SHALL NOT [behavior]"
fn parse_unwanted(line: String, id: String) -> Result(EarsRequirement, EarsError) {
  let pattern_str = "(?i)if\\s+(.+?)\\s+(?:then\\s+)?(?:the\\s+)?system\\s+shall\\s+not\\s+(.+)"

  case regex.from_string(pattern_str) {
    Ok(re) ->
      case regex.scan(re, line) {
        [match, ..] ->
          case match.submatches {
            [Some(condition), Some(behavior)] ->
              Ok(EarsRequirement(
                id: id,
                pattern: Unwanted,
                trigger: None,
                state: None,
                condition: Some(string.trim(condition)),
                system_shall: "",
                system_shall_not: Some(string.trim(behavior)),
                raw_text: line,
              ))
            _ -> Error(EarsError(line: 0, message: "Failed to extract condition/behavior", suggestion: ""))
          }
        [] -> Error(EarsError(line: 0, message: "No match found", suggestion: ""))
      }
    Error(_) -> Error(EarsError(line: 0, message: "Invalid regex", suggestion: ""))
  }
}

/// Pattern: "WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]"
fn parse_complex(line: String, id: String) -> Result(EarsRequirement, EarsError) {
  let pattern_str = "(?i)while\\s+(.+?)\\s+when\\s+(.+?)\\s+(?:the\\s+)?system\\s+shall\\s+(.+)"

  case regex.from_string(pattern_str) {
    Ok(re) ->
      case regex.scan(re, line) {
        [match, ..] ->
          case match.submatches {
            [Some(state), Some(trigger), Some(behavior)] ->
              Ok(EarsRequirement(
                id: id,
                pattern: Complex,
                trigger: Some(string.trim(trigger)),
                state: Some(string.trim(state)),
                condition: None,
                system_shall: string.trim(behavior),
                system_shall_not: None,
                raw_text: line,
              ))
            _ -> Error(EarsError(line: 0, message: "Failed to extract components", suggestion: ""))
          }
        [] -> Error(EarsError(line: 0, message: "No match found", suggestion: ""))
      }
    Error(_) -> Error(EarsError(line: 0, message: "Invalid regex", suggestion: ""))
  }
}

fn generate_warnings(requirements: List(EarsRequirement)) -> List(String) {
  let warnings = []

  // Check for missing negative cases
  let has_unwanted = list.any(requirements, fn(r) { r.pattern == Unwanted })
  let warnings = case has_unwanted {
    False -> ["Consider adding IF...SHALL NOT patterns for unwanted behaviors", ..warnings]
    True -> warnings
  }

  // Check for missing error handling
  let mentions_error =
    requirements
    |> list.any(fn(r) {
      string.contains(string.lowercase(r.system_shall), "error")
        || string.contains(string.lowercase(r.system_shall), "fail")
        || string.contains(string.lowercase(r.system_shall), "reject")
    })
  let warnings = case mentions_error {
    False -> ["No requirements mention error handling", ..warnings]
    True -> warnings
  }

  warnings
}

// =============================================================================
// CONVERSION TO INTENT BEHAVIORS
// =============================================================================

/// Convert EARS requirements to Intent behaviors
pub fn to_behaviors(result: EarsParseResult) -> List(IntentBehavior) {
  result.requirements
  |> list.map(requirement_to_behavior)
}

fn requirement_to_behavior(req: EarsRequirement) -> IntentBehavior {
  let name = generate_behavior_name(req)
  let #(method, path, status) = infer_http_details(req)
  let preconditions = extract_preconditions(req)
  let postconditions = extract_postconditions(req)

  IntentBehavior(
    name: name,
    intent: req.system_shall,
    method: method,
    path: path,
    status: status,
    preconditions: preconditions,
    postconditions: postconditions,
  )
}

fn generate_behavior_name(req: EarsRequirement) -> String {
  let behavior = string.lowercase(req.system_shall)

  // Extract key verbs and nouns
  let name = case True {
    _ if string.contains(behavior, "create") -> "create"
    _ if string.contains(behavior, "validate") -> "validate"
    _ if string.contains(behavior, "return") -> "return"
    _ if string.contains(behavior, "reject") -> "reject"
    _ if string.contains(behavior, "authenticate") -> "authenticate"
    _ if string.contains(behavior, "authorize") -> "authorize"
    _ if string.contains(behavior, "delete") -> "delete"
    _ if string.contains(behavior, "update") -> "update"
    _ -> "handle"
  }

  // Add context from trigger/state/condition
  let suffix = case req.pattern {
    EventDriven ->
      case req.trigger {
        Some(t) -> "-on-" <> slugify(t)
        None -> ""
      }
    StateDriven ->
      case req.state {
        Some(s) -> "-while-" <> slugify(s)
        None -> ""
      }
    Optional ->
      case req.condition {
        Some(c) -> "-if-" <> slugify(c)
        None -> ""
      }
    Unwanted ->
      case req.condition {
        Some(c) -> "-prevented-when-" <> slugify(c)
        None -> ""
      }
    _ -> ""
  }

  req.id <> "-" <> name <> suffix
}

fn slugify(text: String) -> String {
  text
  |> string.lowercase()
  |> string.replace(" ", "-")
  |> string.replace("_", "-")
  |> string.slice(0, 20)
}

fn infer_http_details(req: EarsRequirement) -> #(String, String, Int) {
  let behavior = string.lowercase(req.system_shall)

  case True {
    _ if string.contains(behavior, "create") -> #("POST", "/resource", 201)
    _ if string.contains(behavior, "delete") -> #("DELETE", "/resource/{id}", 204)
    _ if string.contains(behavior, "update") -> #("PUT", "/resource/{id}", 200)
    _ if string.contains(behavior, "return") && string.contains(behavior, "list") -> #("GET", "/resources", 200)
    _ if string.contains(behavior, "return") -> #("GET", "/resource/{id}", 200)
    _ if string.contains(behavior, "reject") -> #("POST", "/resource", 400)
    _ if string.contains(behavior, "authenticate") -> #("POST", "/auth/login", 200)
    _ if string.contains(behavior, "authorize") -> #("GET", "/protected", 403)
    _ if string.contains(behavior, "401") -> #("GET", "/protected", 401)
    _ if string.contains(behavior, "403") -> #("GET", "/protected", 403)
    _ if string.contains(behavior, "404") -> #("GET", "/resource/{id}", 404)
    _ -> #("GET", "/endpoint", 200)
  }
}

fn extract_preconditions(req: EarsRequirement) -> List(String) {
  let preconditions = []

  // State becomes precondition
  let preconditions = case req.state {
    Some(s) -> [s, ..preconditions]
    None -> preconditions
  }

  // Trigger context becomes precondition
  let preconditions = case req.trigger {
    Some(t) if string.contains(string.lowercase(t), "authenticated") ->
      ["user is authenticated", ..preconditions]
    Some(t) if string.contains(string.lowercase(t), "valid") ->
      ["input is valid", ..preconditions]
    _ -> preconditions
  }

  preconditions
}

fn extract_postconditions(req: EarsRequirement) -> List(String) {
  // The SHALL behavior is the postcondition
  [req.system_shall]
}

// =============================================================================
// FORMATTING
// =============================================================================

pub fn format_result(result: EarsParseResult) -> String {
  let header = "╔══════════════════════════════════════╗\n"
    <> "║         EARS Parser Results          ║\n"
    <> "╚══════════════════════════════════════╝\n\n"

  let summary =
    "Parsed: " <> int.to_string(list.length(result.requirements)) <> " requirements\n"
    <> "Errors: " <> int.to_string(list.length(result.errors)) <> "\n"
    <> "Warnings: " <> int.to_string(list.length(result.warnings)) <> "\n\n"

  let requirements_section = format_requirements(result.requirements)
  let errors_section = format_errors(result.errors)
  let warnings_section = format_warnings(result.warnings)

  header <> summary <> requirements_section <> errors_section <> warnings_section
}

fn format_requirements(requirements: List(EarsRequirement)) -> String {
  case list.is_empty(requirements) {
    True -> "Requirements: None\n\n"
    False ->
      "Requirements:\n"
      <> {
        requirements
        |> list.map(format_requirement)
        |> string.join("\n")
      }
      <> "\n\n"
  }
}

fn format_requirement(req: EarsRequirement) -> String {
  let pattern_icon = case req.pattern {
    Ubiquitous -> "○"
    EventDriven -> "⚡"
    StateDriven -> "◐"
    Optional -> "◇"
    Unwanted -> "✗"
    Complex -> "◈"
  }

  let pattern_name = pattern_to_string(req.pattern)

  "  " <> pattern_icon <> " [" <> req.id <> "] " <> pattern_name <> "\n"
  <> "    SHALL: " <> req.system_shall <> "\n"
  <> case req.trigger {
    Some(t) -> "    TRIGGER: " <> t <> "\n"
    None -> ""
  }
  <> case req.state {
    Some(s) -> "    STATE: " <> s <> "\n"
    None -> ""
  }
  <> case req.condition {
    Some(c) -> "    CONDITION: " <> c <> "\n"
    None -> ""
  }
}

fn format_errors(errors: List(EarsError)) -> String {
  case list.is_empty(errors) {
    True -> ""
    False ->
      "Errors:\n"
      <> {
        errors
        |> list.map(fn(e) {
          "  ❌ Line " <> int.to_string(e.line) <> ": " <> e.message <> "\n"
          <> "     💡 " <> e.suggestion
        })
        |> string.join("\n")
      }
      <> "\n\n"
  }
}

fn format_warnings(warnings: List(String)) -> String {
  case list.is_empty(warnings) {
    True -> ""
    False ->
      "Warnings:\n"
      <> {
        warnings
        |> list.map(fn(w) { "  ⚠️  " <> w })
        |> string.join("\n")
      }
      <> "\n"
  }
}

pub fn pattern_to_string(pattern: EarsPattern) -> String {
  case pattern {
    Ubiquitous -> "Ubiquitous"
    EventDriven -> "Event-Driven"
    StateDriven -> "State-Driven"
    Optional -> "Optional"
    Unwanted -> "Unwanted"
    Complex -> "Complex"
  }
}

// =============================================================================
// CUE OUTPUT
// =============================================================================

/// Generate CUE spec from EARS requirements
pub fn to_cue(result: EarsParseResult, spec_name: String) -> String {
  let behaviors = to_behaviors(result)

  let header =
    "// Generated from EARS requirements\n"
    <> "// Source of truth: requirements.md\n"
    <> "package " <> slugify(spec_name) <> "\n\n"
    <> "import \"github.com/intent-cli/intent/schema:intent\"\n\n"
    <> "spec: intent.#Spec & {\n"
    <> "  name: \"" <> spec_name <> "\"\n"
    <> "  description: \"Generated from EARS requirements\"\n"
    <> "  audience: \"API consumers\"\n"
    <> "  version: \"1.0.0\"\n\n"
    <> "  success_criteria: [\n"
    <> "    \"All EARS requirements are satisfied\",\n"
    <> "  ]\n\n"
    <> "  config: {\n"
    <> "    base_url: \"http://localhost:8080\"\n"
    <> "    timeout_ms: 5000\n"
    <> "  }\n\n"

  let features_section =
    "  features: [\n"
    <> "    {\n"
    <> "      name: \"EARS Requirements\"\n"
    <> "      description: \"Behaviors derived from EARS requirements\"\n\n"
    <> "      behaviors: [\n"
    <> {
      behaviors
      |> list.map(behavior_to_cue)
      |> string.join(",\n")
    }
    <> "\n      ]\n"
    <> "    },\n"
    <> "  ]\n\n"

  let footer =
    "  rules: []\n"
    <> "  anti_patterns: []\n"
    <> "  ai_hints: {}\n"
    <> "}\n"

  header <> features_section <> footer
}

fn behavior_to_cue(b: IntentBehavior) -> String {
  "        {\n"
  <> "          name: \"" <> b.name <> "\"\n"
  <> "          intent: \"" <> escape_string(b.intent) <> "\"\n"
  <> "          notes: \"\"\n"
  <> "          requires: []\n"
  <> "          tags: []\n\n"
  <> "          request: {\n"
  <> "            method: \"" <> b.method <> "\"\n"
  <> "            path: \"" <> b.path <> "\"\n"
  <> "            headers: {}\n"
  <> "            query: {}\n"
  <> "            body: {}\n"
  <> "          }\n\n"
  <> "          response: {\n"
  <> "            status: " <> int.to_string(b.status) <> "\n"
  <> "            example: {}\n"
  <> "            checks: {}\n"
  <> "            headers: {}\n"
  <> "          }\n\n"
  <> "          captures: {}\n"
  <> "        }"
}

fn escape_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}
