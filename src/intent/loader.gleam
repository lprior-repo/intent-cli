/// CUE spec loader - loads and validates CUE files using the cue command

import gleam/dynamic
import gleam/json
import gleam/list
import gleam/string
import gleam_community/ansi
import intent/parser
import intent/types.{type Spec}
import shellout
import simplifile
import spinner

/// Error types for loading specs
pub type LoadError {
  FileNotFound(path: String)
  CueValidationError(message: String)
  CueExportError(message: String)
  JsonParseError(message: String)
  SpecParseError(message: String)
}

/// Load a spec from a CUE file
pub fn load_spec(path: String) -> Result(Spec, LoadError) {
  // First check the file exists
  case simplifile.verify_is_file(path) {
    Ok(True) -> load_and_parse(path)
    _ -> Error(FileNotFound(path))
  }
}

fn load_and_parse(path: String) -> Result(Spec, LoadError) {
  // Start spinner for loading
  let sp =
    spinner.new("Validating CUE spec...")
    |> spinner.with_colour(ansi.yellow)
    |> spinner.start

  // First validate the CUE file
  case validate_cue(path) {
    Ok(_) -> {
      spinner.set_text(sp, "Exporting CUE to JSON...")
      let result = export_and_parse(path)
      spinner.stop(sp)
      result
    }
    Error(e) -> {
      spinner.stop(sp)
      Error(e)
    }
  }
}

/// Validate a CUE file without exporting
pub fn validate_cue(path: String) -> Result(Nil, LoadError) {
  case shellout.command("cue", ["vet", path], ".", []) {
    Ok(_) -> Ok(Nil)
    Error(#(_, stderr)) -> Error(CueValidationError(stderr))
  }
}

fn export_and_parse(path: String) -> Result(Spec, LoadError) {
  // Export CUE to JSON
  case shellout.command("cue", ["export", path, "-e", "spec"], ".", []) {
    Ok(json_str) -> parse_json_spec(json_str)
    Error(#(_, stderr)) -> Error(CueExportError(stderr))
  }
}

fn parse_json_spec(json_str: String) -> Result(Spec, LoadError) {
  case json.decode(json_str, dynamic.dynamic) {
    Ok(data) ->
      case parser.parse_spec(data) {
        Ok(spec) -> Ok(spec)
        Error(errors) -> {
          let msg =
            errors
            |> format_decode_errors
          Error(SpecParseError(msg))
        }
      }
    Error(e) -> Error(JsonParseError(format_json_error(e)))
  }
}

fn format_decode_errors(errors: List(dynamic.DecodeError)) -> String {
  case errors {
    [] -> "Unknown decode error"
    [error] -> format_single_decode_error(error)
    multiple -> {
      "Multiple decode errors:\n"
      <> string.join(
        list.map(multiple, fn(e) { "  • " <> format_single_decode_error(e) }),
        "\n",
      )
    }
  }
}

fn format_single_decode_error(error: dynamic.DecodeError) -> String {
  let path_str =
    case error.path {
      [] -> "at root"
      path_parts ->
        "at " <> string.join(path_parts, ".") <> " (path: ." <> string.join(
          path_parts,
          ".",
        ) <> ")"
    }

  "Expected " <> error.expected <> " but found " <> error.found <> " " <> path_str
}

fn format_json_error(error: json.DecodeError) -> String {
  case error {
    json.UnexpectedEndOfInput ->
      "Unexpected end of input - JSON is incomplete or truncated.\n"
      <> "  • Check that your JSON is properly closed with matching braces/brackets"
    json.UnexpectedByte(b) ->
      "Unexpected byte: '" <> b <> "' in JSON at this position.\n"
      <> "  • Check for syntax errors like missing commas, quotes, or brackets\n"
      <> "  • Ensure strings are properly quoted"
    json.UnexpectedSequence(s) ->
      "Unexpected sequence: '" <> s <> "' in JSON.\n"
      <> "  • This sequence is not valid JSON syntax\n"
      <> "  • Check for typos or invalid characters"
    json.UnexpectedFormat(errs) ->
      "JSON format error:\n" <> format_decode_errors(errs)
  }
}

/// Export a spec to JSON format (for AI consumption)
pub fn export_spec_json(path: String) -> Result(String, LoadError) {
  case simplifile.verify_is_file(path) {
    Ok(True) ->
      case shellout.command("cue", ["export", path, "-e", "spec"], ".", []) {
        Ok(json_str) -> Ok(json_str)
        Error(#(_, stderr)) -> Error(CueExportError(stderr))
      }
    _ -> Error(FileNotFound(path))
  }
}

/// Format a LoadError as a human-readable string
pub fn format_error(error: LoadError) -> String {
  case error {
    FileNotFound(path) -> "File not found: " <> path
    CueValidationError(msg) -> "CUE validation failed:\n" <> msg
    CueExportError(msg) -> "CUE export failed:\n" <> msg
    JsonParseError(msg) -> "JSON parse error: " <> msg
    SpecParseError(msg) -> "Spec parse error: " <> msg
  }
}
