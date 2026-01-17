/// Spec Preview and Diff
/// Provides preview, diff, and confirmation functionality for spec file writing
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam_community/ansi
import simplifile

/// Preview spec content with line numbers (first 50 lines)
pub fn preview_spec(content: String, max_lines: Int) -> String {
  let lines = string.split(content, "\n")
  let preview_lines = case list.length(lines) > max_lines {
    True -> list.take(lines, max_lines)
    False -> lines
  }

  let numbered =
    list.index_map(preview_lines, fn(line, idx) {
      let line_num = int.to_string(idx + 1)
      let padded = string.pad_left(line_num, to: 4, with: " ")
      apply_color(ansi.dim, padded) <> " │ " <> line
    })

  let result = string.join(numbered, "\n")

  case list.length(lines) > max_lines {
    True ->
      result
      <> "\n"
      <> apply_color(
        ansi.dim,
        "... ("
          <> int.to_string(list.length(lines) - max_lines)
          <> " more lines)",
      )
    False -> result
  }
}

/// Generate unified diff between old and new content
pub fn diff_specs(old_content: String, new_content: String) -> String {
  let old_lines = string.split(old_content, "\n")
  let new_lines = string.split(new_content, "\n")

  // Simple line-by-line diff (using zip to pair lines)
  let padded_old =
    pad_list(
      old_lines,
      int.max(list.length(old_lines), list.length(new_lines)),
      "",
    )
  let padded_new =
    pad_list(
      new_lines,
      int.max(list.length(old_lines), list.length(new_lines)),
      "",
    )

  list.zip(padded_old, padded_new)
  |> list.filter_map(fn(pair) {
    let #(old_line, new_line) = pair

    case old_line == new_line {
      True ->
        case old_line {
          "" -> Error(Nil)
          _ -> Ok(" " <> old_line)
        }
      False -> {
        case old_line, new_line {
          "", "" -> Error(Nil)
          "", new -> Ok(apply_color(ansi.green, "+ " <> new))
          old, "" -> Ok(apply_color(ansi.red, "- " <> old))
          old, new ->
            Ok(
              apply_color(ansi.red, "- " <> old)
              <> "\n"
              <> apply_color(ansi.green, "+ " <> new),
            )
        }
      }
    }
  })
  |> string.join("\n")
}

/// Pad a list to a given length with a default value
fn pad_list(lst: List(a), target_len: Int, default: a) -> List(a) {
  let current_len = list.length(lst)
  case current_len >= target_len {
    True -> lst
    False -> list.append(lst, list.repeat(default, target_len - current_len))
  }
}

/// Prompt user for confirmation (Y/n)
/// Returns True if user confirms (Y/y/Enter), False otherwise
pub fn prompt_confirmation(message: String) -> Bool {
  io.print(message <> " [Y/n] ")

  case stdin_read_line() {
    Ok(response) -> {
      let trimmed = string.trim(string.lowercase(response))
      case trimmed {
        "" -> True
        "y" -> True
        "yes" -> True
        _ -> False
      }
    }
    Error(_) -> False
  }
}

/// Read a line from stdin (FFI to Erlang io:get_line)
@external(erlang, "intent_ffi", "read_stdin_line")
fn stdin_read_line() -> Result(String, Nil)

/// Apply color function (colors always enabled for now)
/// TODO: Add NO_COLOR environment variable support when gleam_erlang adds env functions
fn apply_color(color_fn: fn(String) -> String, text: String) -> String {
  color_fn(text)
}

/// Print preview header
pub fn print_preview_header(path: String) {
  io.println("")
  io.println(apply_color(fn(s) { ansi.bold(ansi.cyan(s)) }, "Preview: " <> path))
  io.println(apply_color(ansi.dim, string.repeat("─", 80)))
}

/// Print diff header
pub fn print_diff_header(path: String) {
  io.println("")
  io.println(apply_color(fn(s) { ansi.bold(ansi.yellow(s)) }, "Diff: " <> path))
  io.println(apply_color(ansi.dim, string.repeat("─", 80)))
}

/// Check if file exists and read its content
pub fn read_existing_file(path: String) -> Result(String, Nil) {
  simplifile.read(path)
  |> result.map_error(fn(_) { Nil })
}
