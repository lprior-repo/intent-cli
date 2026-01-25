//// JSONL Input Parser for AI-native batch mode
////
//// Parses JSONL from stdin where each line is a JSON command object.
//// Provides:
//// - Single line parsing with validation
//// - Multi-line batch parsing
//// - Argument extraction helpers
//// - Error formatting for AI feedback
////
//// ## Usage
////
//// ```gleam
//// import intent/jsonl_input
////
//// // Parse a single command line
//// let result = jsonl_input.parse_line("{\"command\":\"quality\",\"args\":{\"spec_path\":\"api.cue\"}}")
////
//// // Parse multiple lines (batch mode)
//// let results = jsonl_input.parse_lines(stdin_content)
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import intent/ffi

// =============================================================================
// Types
// =============================================================================

/// A parsed command input from JSONL
pub type CommandInput {
  CommandInput(
    /// Unique identifier for this request (auto-generated if not provided)
    id: String,
    /// The command to execute (e.g., "quality", "coverage", "gaps")
    command: String,
    /// Dynamic arguments passed to the command
    args: Dynamic,
  )
}

/// Errors that can occur during JSONL parsing
pub type ParseError {
  /// Line contains invalid JSON syntax
  InvalidJson(line: String, reason: String)
  /// Required field is missing from the JSON object
  MissingField(field: String)
  /// Command field is empty
  EmptyCommand
  /// Command is not recognized
  UnknownCommand(command: String, suggestions: List(String))
}

// =============================================================================
// Public API
// =============================================================================

/// Create an empty CommandInput (for use with result.unwrap in tests)
pub fn empty_command_input() -> CommandInput {
  CommandInput(id: "", command: "", args: dynamic.from(json.null()))
}

/// Parse a single JSONL line into a CommandInput
///
/// Required fields:
/// - command: String (the command to execute)
///
/// Optional fields:
/// - id: String (auto-generated UUID if not provided)
/// - args: Object (defaults to empty object)
pub fn parse_line(line: String) -> Result(CommandInput, ParseError) {
  case json.decode(from: line, using: dynamic.dynamic) {
    Error(_) -> Error(InvalidJson(line: line, reason: "Invalid JSON syntax"))
    Ok(data) -> parse_command_data(data)
  }
}

/// Parse multiple JSONL lines into a list of results
///
/// Returns a list where each element is either Ok(CommandInput) or Error(ParseError).
/// Blank lines are skipped.
pub fn parse_lines(content: String) -> List(Result(CommandInput, ParseError)) {
  content
  |> string.split("\n")
  |> list.filter(fn(line) { string.trim(line) != "" })
  |> list.map(parse_line)
}

/// Extract a string argument from CommandInput
pub fn extract_string_arg(input: CommandInput, key: String) -> Option(String) {
  case dynamic.field(key, dynamic.string)(input.args) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

/// Extract a boolean argument from CommandInput
pub fn extract_bool_arg(input: CommandInput, key: String) -> Option(Bool) {
  case dynamic.field(key, dynamic.bool)(input.args) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

/// Extract an integer argument from CommandInput
pub fn extract_int_arg(input: CommandInput, key: String) -> Option(Int) {
  case dynamic.field(key, dynamic.int)(input.args) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

/// Validate that a command is in the list of known commands
///
/// Returns Ok(command) if valid, or Error with suggestions for similar commands
pub fn validate_command(
  command: String,
  known_commands: List(String),
) -> Result(String, ParseError) {
  case list.contains(known_commands, command) {
    True -> Ok(command)
    False -> {
      let suggestions = suggest_commands(command, known_commands)
      Error(UnknownCommand(command: command, suggestions: suggestions))
    }
  }
}

/// Format a ParseError as a human-readable string
pub fn format_error(error: ParseError) -> String {
  case error {
    InvalidJson(line, reason) ->
      "Invalid JSON: " <> reason <> "\nLine: " <> line
    MissingField(field) -> "Missing required field: '" <> field <> "'"
    EmptyCommand -> "Command cannot be empty"
    UnknownCommand(cmd, suggestions) -> {
      let base = "Unknown command: '" <> cmd <> "'"
      case suggestions {
        [] -> base
        _ -> base <> "\nDid you mean: " <> string.join(suggestions, ", ") <> "?"
      }
    }
  }
}

/// Convert a ParseError to a JSON error response string
pub fn error_to_json(error: ParseError, request_id: String) -> String {
  let error_type = case error {
    InvalidJson(_, _) -> "INVALID_JSON"
    MissingField(_) -> "MISSING_FIELD"
    EmptyCommand -> "EMPTY_COMMAND"
    UnknownCommand(_, _) -> "UNKNOWN_COMMAND"
  }

  let message = format_error(error)

  json.object([
    #("success", json.bool(False)),
    #("id", json.string(request_id)),
    #(
      "error",
      json.object([
        #("type", json.string(error_type)),
        #("message", json.string(message)),
      ]),
    ),
  ])
  |> json.to_string
}

// =============================================================================
// Internal Functions
// =============================================================================

/// Parse command data from a Dynamic value
fn parse_command_data(data: Dynamic) -> Result(CommandInput, ParseError) {
  // Extract command field (required)
  case dynamic.field("command", dynamic.string)(data) {
    Error(_) -> Error(MissingField("command"))
    Ok(command) -> {
      // Check for empty command
      case string.trim(command) {
        "" -> Error(EmptyCommand)
        trimmed_command -> {
          // Extract id field (optional - generate UUID if missing)
          let id =
            dynamic.field("id", dynamic.string)(data)
            |> result.unwrap(ffi.generate_uuid())

          // Extract args field (optional - default to empty object)
          let args =
            dynamic.field("args", dynamic.dynamic)(data)
            |> result.unwrap(dynamic.from(json.object([])))

          Ok(CommandInput(id: id, command: trimmed_command, args: args))
        }
      }
    }
  }
}

/// Suggest similar commands based on Levenshtein distance
fn suggest_commands(
  target: String,
  known_commands: List(String),
) -> List(String) {
  known_commands
  |> list.map(fn(cmd) { #(cmd, levenshtein_distance(target, cmd)) })
  |> list.filter(fn(pair) {
    let #(_, dist) = pair
    dist <= 2
  })
  |> list.sort(fn(a, b) {
    let #(_, ad) = a
    let #(_, bd) = b
    int.compare(ad, bd)
  })
  |> list.map(fn(pair) {
    let #(cmd, _) = pair
    cmd
  })
  |> list.take(3)
}

/// Calculate Levenshtein distance between two strings
fn levenshtein_distance(s1: String, s2: String) -> Int {
  let len1 = string.length(s1)
  let len2 = string.length(s2)

  case len1, len2 {
    0, _ -> len2
    _, 0 -> len1
    _, _ -> {
      // Simple character-level comparison approximation
      let chars1 = string.to_graphemes(s1)
      let chars2 = string.to_graphemes(s2)

      let common =
        list.filter(chars1, fn(c1) { list.contains(chars2, c1) })
        |> list.length

      len1 + len2 - 2 * common
    }
  }
}
