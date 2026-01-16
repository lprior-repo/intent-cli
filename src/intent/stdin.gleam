/// Module for reading user input from standard input
/// Provides functions for interactive command-line prompts

@external(erlang, "intent_ffi_stdin", "read_line")
pub fn read_line() -> Result(String, String)

@external(erlang, "intent_ffi_stdin", "read_line_trimmed")
pub fn read_line_trimmed() -> Result(String, String)

/// Read a single line from stdin, validating it's not empty
/// Returns error if input is empty or whitespace-only
pub fn read_non_empty_line() -> Result(String, String) {
  case read_line_trimmed() {
    Ok(line) -> {
      case string.is_empty(string.trim(line)) {
        True -> Error("Input cannot be empty. Please try again.")
        False -> Ok(string.trim(line))
      }
    }
    Error(reason) -> Error("Failed to read input: " <> reason)
  }
}

/// Maximum number of lines to prevent memory exhaustion
const max_lines: Int = 10_000

/// Read multiple lines until user enters a blank line
/// Useful for collecting multi-line responses
/// Returns error if more than max_lines are read without blank line
pub fn read_until_blank() -> Result(String, String) {
  read_until_blank_helper([], 0)
}

fn read_until_blank_helper(
  lines: List(String),
  line_count: Int,
) -> Result(String, String) {
  // Safety check: prevent memory exhaustion
  case line_count >= max_lines {
    True ->
      Error(
        "Input exceeds maximum line limit ("
        <> int.to_string(max_lines)
        <> " lines)",
      )
    False ->
      case read_line_trimmed() {
        Error(_reason) -> {
          case line_count {
            0 -> Error("Failed to read input")
            _ -> Ok(string.join(list.reverse(lines), "\n"))
          }
        }
        Ok(line) -> {
          case string.is_empty(line) {
            // User entered blank line - stop collecting
            True -> {
              case line_count {
                0 -> Error("No input provided")
                _ -> Ok(string.join(list.reverse(lines), "\n"))
              }
            }
            // Continue collecting
            False ->
              read_until_blank_helper([line, ..lines], line_count + 1)
          }
        }
      }
  }
}

/// Format and display a prompt, then read a response
pub fn prompt_for_answer(prompt_text: String) -> Result(String, String) {
  io.print(prompt_text)
  read_non_empty_line()
}

/// Display a yes/no prompt and read response
/// Returns True if user enters 'y' or 'yes' (case-insensitive)
pub fn prompt_yes_no(prompt_text: String) -> Result(Bool, String) {
  case prompt_for_answer(prompt_text <> " (y/n): ") {
    Error(reason) -> Error(reason)
    Ok(response) -> {
      let lower = string.lowercase(string.trim(response))
      case lower {
        "y" | "yes" -> Ok(True)
        "n" | "no" -> Ok(False)
        _ -> Error("Please enter 'y' or 'n'")
      }
    }
  }
}

// Required imports
import gleam/int
import gleam/io
import gleam/list
import gleam/string
