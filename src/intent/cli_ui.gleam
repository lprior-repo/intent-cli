/// CLI UI helpers using gleam_community_ansi for rich terminal output
/// Provides colored headers, status messages, and formatted text
/// Respects NO_COLOR environment variable for disabling colors (type-safe)
import gleam/erlang/os
import gleam/io
import gleam/result
import gleam_community/ansi

/// Check if colors should be disabled (NO_COLOR environment variable)
/// Type-safe check using gleam/erlang/os
fn colors_disabled() -> Bool {
  os.get_env("NO_COLOR")
  |> result.is_ok()
}

/// Apply color function only if colors are enabled
fn apply_color(color_fn: fn(String) -> String, text: String) -> String {
  case colors_disabled() {
    True -> text
    False -> color_fn(text)
  }
}

/// Print a bold, colored section header
pub fn print_header(title: String) {
  io.println("")
  io.println(apply_color(
    fn(s) { ansi.bold(ansi.cyan(s)) },
    "═══════════════════════════════════════════════════════════════════",
  ))
  io.println(apply_color(fn(s) { ansi.bold(ansi.cyan(s)) }, title))
  io.println(apply_color(
    fn(s) { ansi.bold(ansi.cyan(s)) },
    "═══════════════════════════════════════════════════════════════════",
  ))
  io.println("")
}

/// Print a success message with checkmark
pub fn print_success(message: String) {
  io.println(apply_color(ansi.green, "✓ " <> message))
}

/// Print a warning message with warning symbol
pub fn print_warning(message: String) {
  io.println(apply_color(ansi.yellow, "⚠️  " <> message))
}

/// Print an error message with X symbol
pub fn print_error(message: String) {
  io.println_error(apply_color(ansi.red, "✗ " <> message))
}

/// Print an info message with info symbol
pub fn print_info(message: String) {
  io.println(apply_color(ansi.blue, "ℹ " <> message))
}

/// Print a bold label with value
pub fn print_labeled(label: String, value: String) {
  io.println(apply_color(ansi.bold, label) <> ": " <> value)
}

/// Print a list item with bullet
pub fn print_list_item(item: String, indent: Int) {
  let padding = case indent {
    0 -> ""
    n -> string.repeat(" ", n * 2)
  }
  io.println(padding <> "• " <> item)
}

/// Print a line of text with color
pub fn print_colored(color_fn: fn(String) -> String, text: String) {
  io.println(apply_color(color_fn, text))
}

/// Format a number as a badge with color
pub fn badge(
  label: String,
  count: Int,
  color_fn: fn(String) -> String,
) -> String {
  apply_color(color_fn, "[" <> label <> ": " <> string.inspect(count) <> "]")
}

// Required imports
import gleam/string
