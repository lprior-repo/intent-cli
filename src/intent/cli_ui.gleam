/// CLI UI helpers using gleam_community_ansi for rich terminal output
/// Provides colored headers, status messages, and formatted text
///
/// When OutputMode is Json or Quiet, all UI output is suppressed to ensure
/// clean machine-readable output on stdout.
import gleam/io
import gleam/string
import gleam_community/ansi
import intent/output_mode.{type OutputMode}

/// Print a bold, colored section header
/// Suppressed in Json and Quiet modes for clean output
pub fn print_header(title: String, mode: OutputMode) {
  case output_mode.is_interactive(mode) {
    True -> {
  io.println("")
  io.println(ansi.bold(ansi.cyan("═══════════════════════════════════════════════════════════════════")))
  io.println(ansi.bold(ansi.cyan(title)))
  io.println(ansi.bold(ansi.cyan("═══════════════════════════════════════════════════════════════════")))
  io.println("")
}
    False -> Nil
  }
}

/// Print a success message with checkmark
/// Suppressed in Json and Quiet modes
pub fn print_success(message: String, mode: OutputMode) {
  case output_mode.is_interactive(mode) {
    True -> io.println(ansi.green("✓ " <> message))
    False -> Nil
  }
}

/// Print a warning message with warning symbol
/// Suppressed in Json and Quiet modes
pub fn print_warning(message: String, mode: OutputMode) {
  case output_mode.is_interactive(mode) {
    True -> io.println(ansi.yellow("⚠️  " <> message))
    False -> Nil
  }
}

/// Print an error message with X symbol
/// Always shown on stderr (even in Json mode for debugging)
pub fn print_error(message: String, mode: OutputMode) {
  case output_mode.is_interactive(mode) {
    True -> io.println_error(ansi.red("✗ " <> message))
    False -> io.println_error(message)
    // Plain text in Json mode
  }
}

/// Print an info message with info symbol
/// Suppressed in Json and Quiet modes
pub fn print_info(message: String, mode: OutputMode) {
  case output_mode.is_interactive(mode) {
    True -> io.println(ansi.blue("ℹ " <> message))
    False -> Nil
  }
}

/// Print a bold label with value
/// Suppressed in Json and Quiet modes
pub fn print_labeled(label: String, value: String, mode: OutputMode) {
  case output_mode.is_interactive(mode) {
    True -> io.println(ansi.bold(label) <> ": " <> value)
    False -> Nil
  }
}

/// Print a list item with bullet
/// Suppressed in Json and Quiet modes
pub fn print_list_item(item: String, indent: Int, mode: OutputMode) {
  case output_mode.is_interactive(mode) {
    True -> {
  let padding = case indent {
    0 -> ""
    n -> string.repeat(" ", n * 2)
  }
  io.println(padding <> "• " <> item)
}
    False -> Nil
  }
}

/// Print a line of text with color
/// Suppressed in Json and Quiet modes
pub fn print_colored(
  color_fn: fn(String) -> String,
  text: String,
  mode: OutputMode,
) {
  case output_mode.is_interactive(mode) {
    True -> io.println(color_fn(text))
    False -> Nil
  }
}

/// Format a number as a badge with color
/// Note: badge() returns a string, so it's usable in any mode
pub fn badge(
  label: String,
  count: Int,
  color_fn: fn(String) -> String,
) -> String {
  color_fn("[" <> label <> ": " <> string.inspect(count) <> "]")
}
