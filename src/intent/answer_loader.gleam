//// Load pre-filled answers from CUE files for non-interactive interview mode.
////
//// Parses CUE answer files and returns a mapping of question IDs to answers.
//// Validates answers against expected format and provides clear error messages.

import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string
import simplifile

/// Error type for answer loading operations
pub type AnswerLoaderError {
  FileNotFound(path: String)
  PermissionDenied(path: String)
  ParseError(path: String, message: String)
  SchemaError(message: String)
  IoError(message: String)
}

/// Load answers from a CUE file.
///
/// Returns a dictionary mapping question IDs to answer strings.
/// Question IDs must be valid (alphanumeric + underscores).
/// Answers must be non-empty strings.
pub fn load_from_file(
  path: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  use file_content <- result.try(read_file(path))
  parse_cue_answers(file_content, path)
}

/// Load answers from a CUE string directly (for testing).
pub fn load_from_string(
  content: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  parse_cue_answers(content, "<string>")
}

// =============================================================================
// PRIVATE: File I/O
// =============================================================================

fn read_file(path: String) -> Result(String, AnswerLoaderError) {
  simplifile.read(path)
  |> result.map_error(fn(err) {
    case err {
      simplifile.Enoent -> FileNotFound(path)
      simplifile.Eacces -> PermissionDenied(path)
      _ -> IoError("Failed to read " <> path <> ": " <> string_of_error(err))
    }
  })
}

fn string_of_error(err: simplifile.FileError) -> String {
  case err {
    simplifile.Enoent -> "File not found"
    simplifile.Eacces -> "Permission denied"
    simplifile.Eisdir -> "Is a directory"
    simplifile.Enotdir -> "Not a directory"
    simplifile.Eexist -> "File exists"
    simplifile.Eio -> "I/O error"
    simplifile.Enospc -> "No space left on device"
    _ -> "Unknown file error"
  }
}

// =============================================================================
// PRIVATE: CUE Parsing
// =============================================================================

/// Simple CUE answer map parser.
///
/// Expected format:
/// ```cue
/// answers: {
///   question_1: "answer text"
///   question_2: "multi-line\nanswer"
/// }
/// ```
fn parse_cue_answers(
  content: String,
  path: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  let trimmed = string.trim(content)
  case string.is_empty(trimmed) {
    True -> Ok(dict.new())
    False -> {
      // Find the answers block
      case extract_answers_block(trimmed) {
        Ok(block) -> parse_answer_entries(block)
        Error(msg) -> Error(ParseError(path, msg))
      }
    }
  }
}

/// Extract the answers {...} block from CUE content.
fn extract_answers_block(content: String) -> Result(String, String) {
  // Look for pattern: answers: { ... }
  let lower = string.lowercase(content)
  case string.contains(lower, "answers") {
    False -> Error("No 'answers' key found in CUE file")
    True -> {
      // Find opening brace
      case find_brace_block(content, "answers:") {
        Ok(block) -> Ok(block)
        Error(Nil) ->
          Error("Malformed answers block: missing or unclosed braces")
      }
    }
  }
}

/// Find a {...} block after a key.
fn find_brace_block(content: String, key: String) -> Result(String, Nil) {
  case string.split_once(content, key) {
    Ok(#(_, after_key)) -> {
      // Find first non-whitespace character (should be {)
      let trimmed = string.trim_left(after_key)
      case string.starts_with(trimmed, "{") {
        False -> Error(Nil)
        True -> extract_until_closing_brace(string.drop_left(trimmed, 1))
      }
    }
    Error(Nil) -> Error(Nil)
  }
}

/// Extract content until matching closing brace.
fn extract_until_closing_brace(content: String) -> Result(String, Nil) {
  let chars = string.to_graphemes(content)
  case extract_brace_contents(chars, 0, []) {
    Ok(block_chars) -> {
      Ok(string.join(block_chars, ""))
    }
    Error(Nil) -> Error(Nil)
  }
}

fn extract_brace_contents(
  chars: List(String),
  depth: Int,
  acc: List(String),
) -> Result(List(String), Nil) {
  case chars {
    [] -> {
      case depth {
        0 -> Ok(list.reverse(acc))
        _ -> Error(Nil)
      }
    }
    [char, ..rest] -> {
      case char {
        "{" -> extract_brace_contents(rest, depth + 1, [char, ..acc])
        "}" -> {
          case depth {
            0 -> {
              // Found closing brace at depth 0, return accumulated content
              Ok(list.reverse(acc))
            }
            _ -> extract_brace_contents(rest, depth - 1, [char, ..acc])
          }
        }
        _ -> extract_brace_contents(rest, depth, [char, ..acc])
      }
    }
  }
}

/// Parse individual answer entries from a CUE block.
fn parse_answer_entries(
  block: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  // Split by lines and parse key: "value" pairs
  block
  |> string.split("\n")
  |> list.map(string.trim)
  |> list.filter(fn(line) {
    !string.is_empty(line) && !string.starts_with(line, "//")
  })
  |> list.fold(Ok(dict.new()), fn(acc, line) {
    case acc {
      Error(_) as err -> err
      Ok(dict_so_far) -> {
        case parse_answer_entry(line) {
          Ok(#(key, value)) -> {
            case validate_question_id(key) {
              True -> {
                case string.is_empty(string.trim(value)) {
                  True ->
                    Error(SchemaError("Empty answer for question: " <> key))
                  False -> Ok(dict.insert(dict_so_far, key, string.trim(value)))
                }
              }
              False ->
                Error(SchemaError(
                  "Invalid question ID: "
                  <> key
                  <> " (must be alphanumeric + underscore)",
                ))
            }
          }
          Error(msg) -> Error(ParseError("<answers>", msg))
        }
      }
    }
  })
}

/// Parse a single "key: value" line from CUE.
fn parse_answer_entry(line: String) -> Result(#(String, String), String) {
  case string.contains(line, ":") {
    False -> Error("Line missing colon: " <> line)
    True -> {
      case string.split_once(line, ":") {
        Ok(#(key, rest)) -> {
          let key_trimmed = string.trim(key)
          let value = extract_string_value(string.trim(rest))
          Ok(#(key_trimmed, value))
        }
        Error(Nil) -> Error("Malformed key:value pair")
      }
    }
  }
}

/// Extract string value from "..." or unquoted text.
fn extract_string_value(value_str: String) -> String {
  case string.starts_with(value_str, "\"") {
    True -> {
      // Remove quotes and unescape
      value_str
      |> string.drop_left(1)
      |> fn(s) {
        case string.ends_with(s, "\"") {
          True -> string.drop_right(s, 1)
          False -> s
        }
      }
      |> unescape_string
    }
    False -> {
      // Unquoted: take until comma or closing brace
      value_str
      |> string.split_once(",")
      |> fn(result) {
        case result {
          Ok(#(before, _)) -> string.trim(before)
          Error(Nil) -> {
            string.split_once(value_str, "}")
            |> fn(result2) {
              case result2 {
                Ok(#(before, _)) -> string.trim(before)
                Error(Nil) -> value_str
              }
            }
          }
        }
      }
    }
  }
}

/// Unescape common escape sequences in strings.
fn unescape_string(s: String) -> String {
  s
  |> string.replace("\\n", "\n")
  |> string.replace("\\t", "\t")
  |> string.replace("\\\"", "\"")
  |> string.replace("\\\\", "\\")
}

/// Validate question ID format: alphanumeric and underscores only.
fn validate_question_id(id: String) -> Bool {
  let trimmed = string.trim(id)
  case string.length(trimmed) {
    0 -> False
    _ -> {
      trimmed
      |> string.to_graphemes
      |> list.all(fn(char) {
        case char {
          "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
          "a"
          | "b"
          | "c"
          | "d"
          | "e"
          | "f"
          | "g"
          | "h"
          | "i"
          | "j"
          | "k"
          | "l"
          | "m"
          | "n"
          | "o"
          | "p"
          | "q"
          | "r"
          | "s"
          | "t"
          | "u"
          | "v"
          | "w"
          | "x"
          | "y"
          | "z" -> True
          "A"
          | "B"
          | "C"
          | "D"
          | "E"
          | "F"
          | "G"
          | "H"
          | "I"
          | "J"
          | "K"
          | "L"
          | "M"
          | "N"
          | "O"
          | "P"
          | "Q"
          | "R"
          | "S"
          | "T"
          | "U"
          | "V"
          | "W"
          | "X"
          | "Y"
          | "Z" -> True
          "_" -> True
          _ -> False
        }
      })
    }
  }
}
