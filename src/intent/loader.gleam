/// CUE spec loader - loads and validates CUE files using the cue command
import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/string
import gleam_community/ansi
import intent/ai_errors
import intent/parser
import intent/security
import intent/types.{type Spec, Spec}
import shellout
import spinner

/// Error types for loading specs
pub type LoadError {
  FileNotFound(path: String)
  CueValidationError(message: String)
  CueExportError(message: String)
  JsonParseError(message: String)
  SpecParseError(message: String)
  SecurityError(message: String)
}

/// Load a spec from a CUE file (with spinner UI)
pub fn load_spec(path: String) -> Result(Spec, LoadError) {
  // Validate path for security
  case security.validate_file_path(path) {
    Ok(validated_path) -> load_and_parse_with_spinner(validated_path)
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

/// Load a spec from a CUE file without spinner UI
/// Use this for testing and automation where no UI output is desired
pub fn load_spec_quiet(path: String) -> Result(Spec, LoadError) {
  // Validate path for security
  case security.validate_file_path(path) {
    Ok(validated_path) -> load_and_parse_impl(validated_path)
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

/// Pure business logic - validates and parses without UI
/// This is the testable core implementation
fn load_and_parse_impl(path: String) -> Result(Spec, LoadError) {
  case validate_cue(path) {
    Ok(_) -> export_and_parse(path)
    Error(e) -> Error(e)
  }
}

/// Load and parse with spinner UI for interactive use
fn load_and_parse_with_spinner(path: String) -> Result(Spec, LoadError) {
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

/// Validate a CUE file without exporting (CUE syntax only)
pub fn validate_cue(path: String) -> Result(Nil, LoadError) {
  // Validate path for security FIRST
  case security.validate_file_path(path) {
    Ok(validated_path) ->
      case shellout.command("cue", ["vet", validated_path], ".", []) {
        Ok(_) -> Ok(Nil)
        Error(#(_, stderr)) -> Error(CueValidationError(stderr))
      }
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

fn export_and_parse(path: String) -> Result(Spec, LoadError) {
  // Try to export as full spec first
  case shellout.command("cue", ["export", path, "-e", "spec"], ".", []) {
    Ok(json_str) -> parse_json_spec(json_str)
    Error(#(_, stderr)) -> Error(CueExportError(stderr))
  }
}

fn parse_json_spec(json_str: String) -> Result(Spec, LoadError) {
  // Validate JSON safety before parsing (prevents DOS attacks)
  case parser.validate_json_safety(json_str) {
    Error(parser.PayloadTooLarge(size, max)) ->
      Error(SecurityError(
        "JSON payload too large: "
        <> string.inspect(size)
        <> " bytes (maximum: "
        <> string.inspect(max)
        <> " bytes). This protects against memory exhaustion attacks.",
      ))
    Error(parser.NestingTooDeep(depth, max)) ->
      Error(SecurityError(
        "JSON nesting too deep: "
        <> string.inspect(depth)
        <> " levels (maximum: "
        <> string.inspect(max)
        <> " levels). This protects against stack overflow attacks.",
      ))
    Ok(_) ->
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
  let path_str = case error.path {
    [] -> "at root"
    path_parts ->
      "at "
      <> string.join(path_parts, ".")
      <> " (path: ."
      <> string.join(path_parts, ".")
      <> ")"
  }

  "Expected "
  <> error.expected
  <> " but found "
  <> error.found
  <> " "
  <> path_str
}

fn format_json_error(error: json.DecodeError) -> String {
  case error {
    json.UnexpectedEndOfInput ->
      "Unexpected end of input - JSON is incomplete or truncated.\n"
      <> "  • Check that your JSON is properly closed with matching braces/brackets"
    json.UnexpectedByte(b) ->
      "Unexpected byte: '"
      <> b
      <> "' in JSON at this position.\n"
      <> "  • Check for syntax errors like missing commas, quotes, or brackets\n"
      <> "  • Ensure strings are properly quoted"
    json.UnexpectedSequence(s) ->
      "Unexpected sequence: '"
      <> s
      <> "' in JSON.\n"
      <> "  • This sequence is not valid JSON syntax\n"
      <> "  • Check for typos or invalid characters"
    json.UnexpectedFormat(errs) ->
      "JSON format error:\n" <> format_decode_errors(errs)
  }
}

/// Export a spec to JSON format (for AI consumption)
pub fn export_spec_json(path: String) -> Result(String, LoadError) {
  // Validate path for security FIRST
  case security.validate_file_path(path) {
    Ok(validated_path) ->
      case
        shellout.command(
          "cue",
          ["export", validated_path, "-e", "spec"],
          ".",
          [],
        )
      {
        Ok(json_str) -> Ok(json_str)
        Error(#(_, stderr)) -> Error(CueExportError(stderr))
      }
    Error(security_error) ->
      Error(SecurityError(security.format_security_error(security_error)))
  }
}

/// Format a LoadError as a human-readable string (legacy format)
pub fn format_error(error: LoadError) -> String {
  case error {
    FileNotFound(path) -> "File not found: " <> path
    CueValidationError(msg) -> "CUE validation failed:\n" <> msg
    CueExportError(msg) -> "CUE export failed:\n" <> msg
    JsonParseError(msg) -> "JSON parse error: " <> msg
    SpecParseError(msg) -> "Spec parse error: " <> msg
    SecurityError(msg) -> msg
  }
}

/// Format a LoadError as AI-friendly structured output (CUE format)
pub fn format_error_ai(error: LoadError) -> String {
  case error {
    FileNotFound(path) ->
      ai_errors.file_not_found(path, "CUE specification file")
      |> ai_errors.format_cue

    CueValidationError(msg) ->
      ai_errors.cue_validation_error(msg, extract_file_from_error(msg))
      |> ai_errors.format_cue

    CueExportError(msg) ->
      ai_errors.cue_export_error(msg, extract_file_from_error(msg))
      |> ai_errors.format_cue

    JsonParseError(msg) -> {
      let error =
        ai_errors.AiError(
          action: "json_error",
          error_type: "parse_error",
          message: "JSON parsing failed: " <> msg,
          context: dict.from_list([#("parse_error", msg)]),
          suggestion: "Check JSON syntax and structure",
          recovery_steps: [
            "Verify JSON is well-formed with matching braces/brackets",
            "Check for trailing commas (not allowed in JSON)",
            "Ensure all strings are properly quoted",
            "Use a JSON validator to identify syntax errors",
          ],
        )
      ai_errors.format_cue(error)
    }

    SpecParseError(msg) -> {
      let error =
        ai_errors.AiError(
          action: "spec_error",
          error_type: "parse_error",
          message: "Spec parsing failed: " <> msg,
          context: dict.from_list([#("parse_error", msg)]),
          suggestion: "Ensure the spec matches the required schema",
          recovery_steps: [
            "Check that all required fields are present (name, description, features, etc.)",
            "Verify field types match expected types",
            "Review examples in examples/ directory for reference",
            "Use 'cue vet' to validate against the schema",
            "Check for typos in field names",
          ],
        )
      ai_errors.format_cue(error)
    }

    SecurityError(msg) -> {
      let error =
        ai_errors.AiError(
          action: "security_error",
          error_type: "validation_failed",
          message: msg,
          context: dict.from_list([#("security_check", "failed")]),
          suggestion: "Address the security concern before proceeding",
          recovery_steps: [
            "Review the security error message",
            "Ensure file paths are valid and don't contain malicious patterns",
            "Check JSON payload size and nesting depth limits",
            "Contact security team if uncertain about the restriction",
          ],
        )
      ai_errors.format_cue(error)
    }
  }
}

/// Format a LoadError as human-readable text with suggestions
pub fn format_error_text(error: LoadError) -> String {
  case error {
    FileNotFound(path) ->
      ai_errors.file_not_found(path, "CUE specification file")
      |> ai_errors.format_text

    CueValidationError(msg) ->
      ai_errors.cue_validation_error(msg, extract_file_from_error(msg))
      |> ai_errors.format_text

    CueExportError(msg) ->
      ai_errors.cue_export_error(msg, extract_file_from_error(msg))
      |> ai_errors.format_text

    JsonParseError(msg) -> {
      let error =
        ai_errors.AiError(
          action: "json_error",
          error_type: "parse_error",
          message: "JSON parsing failed: " <> msg,
          context: dict.from_list([#("parse_error", msg)]),
          suggestion: "Check JSON syntax and structure",
          recovery_steps: [
            "Verify JSON is well-formed with matching braces/brackets",
            "Check for trailing commas (not allowed in JSON)",
            "Ensure all strings are properly quoted",
            "Use a JSON validator to identify syntax errors",
          ],
        )
      ai_errors.format_text(error)
    }

    SpecParseError(msg) -> {
      let error =
        ai_errors.AiError(
          action: "spec_error",
          error_type: "parse_error",
          message: "Spec parsing failed: " <> msg,
          context: dict.from_list([#("parse_error", msg)]),
          suggestion: "Ensure the spec matches the required schema",
          recovery_steps: [
            "Check that all required fields are present (name, description, features, etc.)",
            "Verify field types match expected types",
            "Review examples in examples/ directory for reference",
            "Use 'cue vet' to validate against the schema",
            "Check for typos in field names",
          ],
        )
      ai_errors.format_text(error)
    }

    SecurityError(msg) -> {
      let error =
        ai_errors.AiError(
          action: "security_error",
          error_type: "validation_failed",
          message: msg,
          context: dict.from_list([#("security_check", "failed")]),
          suggestion: "Address the security concern before proceeding",
          recovery_steps: [
            "Review the security error message",
            "Ensure file paths are valid and don't contain malicious patterns",
            "Check JSON payload size and nesting depth limits",
            "Contact security team if uncertain about the restriction",
          ],
        )
      ai_errors.format_text(error)
    }
  }
}

/// Extract file path from error message (best effort)
fn extract_file_from_error(msg: String) -> String {
  // Try to find .cue file path in error message
  case string.split(msg, ".cue") {
    [before, ..] ->
      case string.split(before, " ") |> list.reverse {
        [last_word, ..] -> last_word <> ".cue"
        _ -> "unknown"
      }
    _ -> "unknown"
  }
}
