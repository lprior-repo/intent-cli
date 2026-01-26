import gleam/dynamic.{DecodeError}
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import intent/prompt_errors.{
  BeadLoadError, BeadNotFound, BeadParseError, CueValidationError, SecurityError,
  SessionNotFound, TemplateError,
}

// =============================================================================
// BeadLoadError Tests
// =============================================================================

pub fn bead_load_error_preserves_context_test() {
  let error =
    prompt_errors.bead_load_error("session-123", ".intent/session-123.cue", #(
      1,
      "cue: marshal error: field not found",
    ))

  case error {
    BeadLoadError(session_id, path, exit_code, stderr, error_file, error_line) -> {
      session_id |> should.equal("session-123")
      path |> should.equal(".intent/session-123.cue")
      exit_code |> should.equal(1)
      stderr |> should.equal("cue: marshal error: field not found")
      error_file |> should.equal(None)
      error_line |> should.equal(None)
    }
    _ -> panic as "Expected BeadLoadError"
  }
}

pub fn bead_load_error_format_test() {
  let error =
    BeadLoadError(
      session_id: "test-session",
      path: ".intent/session-test.cue",
      exit_code: 1,
      stderr: "field 'beads' not found",
      error_file: None,
      error_line: None,
    )

  let formatted = prompt_errors.format_error(error)

  formatted |> should.not_equal("")
  string.contains(formatted, "test-session") |> should.be_true
  string.contains(formatted, "Exit code: 1") |> should.be_true
  string.contains(formatted, "field 'beads' not found") |> should.be_true
}

pub fn bead_load_error_extracts_file_and_line_test() {
  let error =
    prompt_errors.bead_load_error("session-123", ".intent/session.cue", #(
      1,
      "schema.cue:42:10: undefined field 'foo'",
    ))

  case error {
    BeadLoadError(_, _, _, _, error_file, error_line) -> {
      error_file |> should.equal(Some("schema.cue"))
      error_line |> should.equal(Some(42))
    }
    _ -> panic as "Expected BeadLoadError"
  }
}

pub fn bead_load_error_json_test() {
  let error =
    BeadLoadError(
      session_id: "json-session",
      path: ".intent/session.cue",
      exit_code: 2,
      stderr: "syntax error",
      error_file: None,
      error_line: None,
    )

  let json_value = prompt_errors.to_json(error)
  let json_str = json.to_string(json_value)

  string.contains(json_str, "bead_load_error") |> should.be_true
  string.contains(json_str, "json-session") |> should.be_true
  string.contains(json_str, "syntax error") |> should.be_true
}

// =============================================================================
// BeadParseError Tests
// =============================================================================

pub fn bead_parse_error_preserves_decode_errors_test() {
  let decode_errors = [
    DecodeError(expected: "String", found: "Int", path: ["beads", "0", "title"]),
    DecodeError(expected: "List", found: "Object", path: [
      "beads",
      "0",
      "labels",
    ]),
  ]

  let error =
    prompt_errors.bead_parse_error(
      "parse-session",
      ".intent/session.cue",
      decode_errors,
      Some("{\"invalid\": true}"),
    )

  case error {
    BeadParseError(session_id, source, errors, preview) -> {
      session_id |> should.equal("parse-session")
      source |> should.equal(".intent/session.cue")
      errors |> should.equal(decode_errors)
      preview |> should.equal(Some("{\"invalid\": true}"))
    }
    _ -> panic as "Expected BeadParseError"
  }
}

pub fn bead_parse_error_truncates_content_test() {
  let long_content = string.repeat("x", 600)

  let error =
    prompt_errors.bead_parse_error(
      "truncate-session",
      "stdin",
      [],
      Some(long_content),
    )

  case error {
    BeadParseError(_, _, _, preview) -> {
      case preview {
        Some(p) -> {
          // Should be truncated to ~503 chars (500 + "...")
          { string.length(p) < 510 } |> should.be_true
          string.ends_with(p, "...") |> should.be_true
        }
        None -> panic as "Expected content preview"
      }
    }
    _ -> panic as "Expected BeadParseError"
  }
}

pub fn bead_parse_error_format_includes_errors_test() {
  let decode_errors = [
    DecodeError(expected: "String", found: "Null", path: ["title"]),
  ]

  let error =
    BeadParseError(
      session_id: "format-session",
      source: "test.json",
      decode_errors: decode_errors,
      content_preview: None,
    )

  let formatted = prompt_errors.format_error(error)

  string.contains(formatted, "format-session") |> should.be_true
  string.contains(formatted, "title") |> should.be_true
  string.contains(formatted, "String") |> should.be_true
  string.contains(formatted, "Null") |> should.be_true
}

// =============================================================================
// BeadNotFound Tests
// =============================================================================

pub fn bead_not_found_includes_session_context_test() {
  let error =
    prompt_errors.bead_not_found(
      "AUTH-001",
      "my-session",
      ".intent/session-my-session.cue",
      ["AUTH-002", "API-001", "DATA-001"],
    )

  case error {
    BeadNotFound(bead_id, session_id, session_path, available) -> {
      bead_id |> should.equal("AUTH-001")
      session_id |> should.equal("my-session")
      session_path |> should.equal(".intent/session-my-session.cue")
      available |> should.equal(["AUTH-002", "API-001", "DATA-001"])
    }
    _ -> panic as "Expected BeadNotFound"
  }
}

pub fn bead_not_found_format_shows_suggestions_test() {
  let error =
    BeadNotFound(
      bead_id: "MISSING-001",
      session_id: "suggest-session",
      session_path: ".intent/session.cue",
      available_beads: ["AUTH-001", "API-001"],
    )

  let formatted = prompt_errors.format_error(error)

  string.contains(formatted, "MISSING-001") |> should.be_true
  string.contains(formatted, "suggest-session") |> should.be_true
  string.contains(formatted, "AUTH-001") |> should.be_true
  string.contains(formatted, "API-001") |> should.be_true
}

// =============================================================================
// SessionNotFound Tests
// =============================================================================

pub fn session_not_found_shows_available_sessions_test() {
  let error =
    prompt_errors.session_not_found(
      "missing-session",
      ".intent/session-missing.cue",
      [
        "session-a",
        "session-b",
      ],
    )

  let formatted = prompt_errors.format_error(error)

  string.contains(formatted, "missing-session") |> should.be_true
  string.contains(formatted, "session-a") |> should.be_true
  string.contains(formatted, "session-b") |> should.be_true
}

pub fn session_not_found_empty_sessions_suggestion_test() {
  let error =
    prompt_errors.session_not_found(
      "new-session",
      ".intent/session-new.cue",
      [],
    )

  let formatted = prompt_errors.format_error(error)

  string.contains(formatted, "No sessions found") |> should.be_true
  string.contains(formatted, "intent interview") |> should.be_true
}

// =============================================================================
// CueValidationError Tests
// =============================================================================

pub fn cue_validation_error_extracts_line_number_test() {
  let error =
    prompt_errors.cue_validation_error("cue-session", "schema.cue", #(
      1,
      "schema.cue:42:10: undefined field",
    ))

  case error {
    CueValidationError(_, _, _, _, line_number) -> {
      line_number |> should.equal(Some(42))
    }
    _ -> panic as "Expected CueValidationError"
  }
}

pub fn cue_validation_error_no_line_number_test() {
  let error =
    prompt_errors.cue_validation_error("cue-session", "schema.cue", #(
      1,
      "generic error without line",
    ))

  case error {
    CueValidationError(_, _, _, _, line_number) -> {
      line_number |> should.equal(None)
    }
    _ -> panic as "Expected CueValidationError"
  }
}

// =============================================================================
// Error Categorization Tests
// =============================================================================

pub fn is_recoverable_test() {
  prompt_errors.is_recoverable(BeadLoadError("s", "p", 1, "err", None, None))
  |> should.be_true

  prompt_errors.is_recoverable(BeadParseError("s", "p", [], None))
  |> should.be_true

  prompt_errors.is_recoverable(BeadNotFound("b", "s", "p", []))
  |> should.be_false

  prompt_errors.is_recoverable(SessionNotFound("s", "p", []))
  |> should.be_false

  prompt_errors.is_recoverable(SecurityError("op", "in", "reason"))
  |> should.be_false
}

pub fn get_session_id_test() {
  prompt_errors.get_session_id(BeadLoadError(
    "load-session",
    "p",
    1,
    "err",
    None,
    None,
  ))
  |> should.equal(Some("load-session"))

  prompt_errors.get_session_id(BeadNotFound("b", "notfound-session", "p", []))
  |> should.equal(Some("notfound-session"))

  prompt_errors.get_session_id(SecurityError("op", "in", "reason"))
  |> should.equal(None)

  prompt_errors.get_session_id(TemplateError("b", "t", "reason"))
  |> should.equal(None)
}

pub fn error_code_test() {
  prompt_errors.error_code(BeadLoadError("s", "p", 1, "err", None, None))
  |> should.equal("PROMPT_LOAD_ERR")

  prompt_errors.error_code(BeadParseError("s", "p", [], None))
  |> should.equal("PROMPT_PARSE_ERR")

  prompt_errors.error_code(BeadNotFound("b", "s", "p", []))
  |> should.equal("PROMPT_BEAD_404")

  prompt_errors.error_code(SessionNotFound("s", "p", []))
  |> should.equal("PROMPT_SESSION_404")

  prompt_errors.error_code(SecurityError("op", "in", "reason"))
  |> should.equal("PROMPT_SECURITY_ERR")
}

// =============================================================================
// JSON Output Tests
// =============================================================================

pub fn all_error_types_produce_valid_json_test() {
  let errors = [
    BeadLoadError("s", "p", 1, "err", None, None),
    BeadParseError("s", "p", [], None),
    BeadNotFound("b", "s", "p", []),
    SessionNotFound("s", "p", []),
    CueValidationError("s", "p", 1, "err", None),
    TemplateError("b", "t", "reason"),
    SecurityError("op", "in", "reason"),
  ]

  // All should produce valid JSON (no panics)
  errors
  |> list.each(fn(err) {
    let json_value = prompt_errors.to_json(err)
    let json_str = json.to_string(json_value)
    // Basic validation: should contain error_type
    string.contains(json_str, "error_type") |> should.be_true
  })
}

import gleam/list
