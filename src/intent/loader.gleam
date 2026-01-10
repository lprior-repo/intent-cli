/// CUE spec loader - loads and validates CUE files using the cue command
import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/string
import gleam_community/ansi
import intent/parser
import intent/security
import intent/types.{
  type LightSpec, type Spec, AIHints, Behavior, Config, Feature,
  ImplementationHints, Request, Response, SecurityHints, Spec,
}
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
  LightSpecParseError(message: String)
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

/// Validate a CUE file without exporting
pub fn validate_cue(path: String) -> Result(Nil, LoadError) {
  case shellout.command("cue", ["vet", path], ".", []) {
    Ok(_) -> Ok(Nil)
    Error(#(_, stderr)) -> Error(CueValidationError(stderr))
  }
}

fn export_and_parse(path: String) -> Result(Spec, LoadError) {
  // Try to export as full spec first
  case shellout.command("cue", ["export", path, "-e", "spec"], ".", []) {
    Ok(json_str) -> parse_json_spec(json_str)
    Error(_) -> {
      // Try as light_spec
      case
        shellout.command("cue", ["export", path, "-e", "light_spec"], ".", [])
      {
        Ok(json_str) -> parse_json_light_spec(json_str)
        Error(#(_, stderr)) -> Error(CueExportError(stderr))
      }
    }
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

fn parse_json_light_spec(json_str: String) -> Result(Spec, LoadError) {
  case json.decode(json_str, dynamic.dynamic) {
    Ok(data) ->
      case parser.parse_light_spec(data) {
        Ok(light_spec) -> Ok(light_spec_to_spec(light_spec))
        Error(errors) -> {
          let msg =
            errors
            |> format_decode_errors
          Error(LightSpecParseError(msg))
        }
      }
    Error(e) -> Error(JsonParseError(format_json_error(e)))
  }
}

/// Convert a LightSpec to a full Spec for unified handling
fn light_spec_to_spec(light: LightSpec) -> Spec {
  // Convert light behaviors to full behaviors
  let behaviors =
    list.map(light.behaviors, fn(lb) {
      Behavior(
        name: lb.name,
        intent: lb.intent,
        notes: "",
        requires: [],
        tags: [],
        request: Request(
          method: lb.request.method,
          path: lb.request.path,
          headers: dict.new(),
          query: dict.new(),
          body: lb.request.body,
        ),
        response: Response(
          status: lb.response.status,
          example: json.null(),
          checks: lb.response.checks,
          headers: dict.new(),
        ),
        captures: dict.new(),
      )
    })

  // Wrap in a single feature
  let feature =
    Feature(
      name: "default",
      description: "Behaviors from light spec",
      behaviors: behaviors,
    )

  // Build default AI hints
  let default_ai_hints =
    AIHints(
      implementation: ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: SecurityHints(
        password_hashing: "",
        jwt_algorithm: "",
        jwt_expiry: "",
        rate_limiting: "",
      ),
      pitfalls: [],
      codebase: None,
    )

  Spec(
    name: light.name,
    description: light.description,
    audience: "",
    version: "1.0.0",
    success_criteria: [],
    config: Config(base_url: "", timeout_ms: 5000, headers: dict.new()),
    features: [feature],
    rules: [],
    anti_patterns: light.anti_patterns,
    ai_hints: case light.ai_hints {
      None -> default_ai_hints
      option.Some(hints) -> hints
    },
  )
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
    LightSpecParseError(msg) -> "Light spec parse error: " <> msg
  }
}
