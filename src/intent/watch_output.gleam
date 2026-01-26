/// Watch mode output formatting
import gleam/int
import gleam/io
import gleam/string
import gleam_community/ansi
import intent/loader

/// Clear the terminal screen
pub fn clear_screen() -> Nil {
  io.print("\u{001b}[2J\u{001b}[H")
}

/// Get current timestamp in readable format
pub fn timestamp() -> String {
  let #(#(year, month, day), #(hour, minute, second)) =
    erlang_calendar_local_time()
  pad_zero(year, 4)
  <> "-"
  <> pad_zero(month, 2)
  <> "-"
  <> pad_zero(day, 2)
  <> " "
  <> pad_zero(hour, 2)
  <> ":"
  <> pad_zero(minute, 2)
  <> ":"
  <> pad_zero(second, 2)
}

fn pad_zero(num: Int, width: Int) -> String {
  let str = int.to_string(num)
  let current_width = string.length(str)
  case current_width < width {
    True -> {
      let padding = string.repeat("0", width - current_width)
      padding <> str
    }
    False -> str
  }
}

@external(erlang, "calendar", "local_time")
fn erlang_calendar_local_time() -> #(#(Int, Int, Int), #(Int, Int, Int))

/// Format validation success message
pub fn format_success(spec_path: String, timestamp: String) -> String {
  let header = ansi.bold(ansi.green("✓ VALIDATION PASSED"))
  let time = ansi.dim("Last checked: " <> timestamp)
  let file = ansi.dim("File: " <> spec_path)
  let watching = ansi.dim("\nWatching for changes... (Press Ctrl+C to exit)")
  header <> "\n" <> time <> "\n" <> file <> watching
}

/// Format validation failure message
pub fn format_failure(
  spec_path: String,
  error: String,
  timestamp: String,
) -> String {
  let header = ansi.bold(ansi.red("✗ VALIDATION FAILED"))
  let time = ansi.dim("Last checked: " <> timestamp)
  let file = ansi.dim("File: " <> spec_path)
  let error_section = ansi.red("\nError:\n" <> error)
  let watching = ansi.dim("\nWatching for changes... (Press Ctrl+C to exit)")
  header <> "\n" <> time <> "\n" <> file <> error_section <> watching
}

/// Display validation result in watch mode
pub fn display_result(
  spec_path: String,
  result: Result(a, loader.LoadError),
) -> Nil {
  clear_screen()
  let ts = timestamp()
  case result {
    Ok(_) -> {
      io.println(format_success(spec_path, ts))
    }
    Error(e) -> {
      let error_msg = loader.format_error(e)
      io.println(format_failure(spec_path, error_msg, ts))
    }
  }
}
