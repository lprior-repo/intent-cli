/// Tests for unified error handling system
import gleam/dict
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import intent/unified_errors.{
  ConflictingFlags, Error, FileNotFound, FilePermissionDenied, InternalError, InvalidInput, LoadError,
  MissingInput, SessionNotFound, SpecParseError, ValidationFailed, Warning,
  conflicting_flags, error_code_to_string, exit_internal_error,
  exit_runtime_error, exit_success, exit_user_input_error, exit_validation_error,
  file_not_found, file_permission_denied, format_error_brief, format_error_text,
  get_exit_code_for_error, internal_error, invalid_input, load_error,
  missing_input, session_not_found, spec_parse_error, unified_error,
  unified_error_to_json, validation_failed, with_context, with_context_list,
  with_severity,
}

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// ERROR CODE STRING CONVERSION TESTS
// =============================================================================

pub fn test_error_code_to_string_missing_input() {
  error_code_to_string(MissingInput)
  |> should.equal("missing_input")
}

pub fn test_error_code_to_string_validation_failed() {
  error_code_to_string(ValidationFailed)
  |> should.equal("validation_failed")
}

pub fn test_error_code_to_string_file_not_found() {
  error_code_to_string(FileNotFound)
  |> should.equal("file_not_found")
}

pub fn test_error_code_to_string_file_permission_denied() {
  error_code_to_string(FilePermissionDenied)
  |> should.equal("file_permission_denied")
}

pub fn test_error_code_to_string_invalid_input() {
  error_code_to_string(InvalidInput)
  |> should.equal("invalid_input")
}

pub fn test_error_code_to_string_spec_parse_error() {
  error_code_to_string(SpecParseError)
  |> should.equal("spec_parse_error")
}

pub fn test_error_code_to_string_load_error() {
  error_code_to_string(LoadError)
  |> should.equal("load_error")
}

pub fn test_error_code_to_string_session_not_found() {
  error_code_to_string(SessionNotFound)
  |> should.equal("session_not_found")
}

pub fn test_error_code_to_string_conflicting_flags() {
  error_code_to_string(ConflictingFlags)
  |> should.equal("conflicting_flags")
}

pub fn test_error_code_to_string_internal_error() {
  error_code_to_string(InternalError)
  |> should.equal("internal_error")
}

// =============================================================================
// EXIT CODE TESTS
// =============================================================================

pub fn test_exit_code_constants() {
  exit_success |> should.equal(0)
  exit_user_input_error |> should.equal(2)
  exit_validation_error |> should.equal(3)
  exit_runtime_error |> should.equal(4)
  exit_internal_error |> should.equal(5)
}

pub fn test_get_exit_code_for_missing_input() {
  get_exit_code_for_error(MissingInput)
  |> should.equal(exit_user_input_error)
}

pub fn test_get_exit_code_for_validation_failed() {
  get_exit_code_for_error(ValidationFailed)
  |> should.equal(exit_validation_error)
}

pub fn test_get_exit_code_for_file_not_found() {
  get_exit_code_for_error(FileNotFound)
  |> should.equal(exit_user_input_error)
}

pub fn test_get_exit_code_for_file_permission_denied() {
  get_exit_code_for_error(FilePermissionDenied)
  |> should.equal(exit_runtime_error)
}

pub fn test_get_exit_code_for_invalid_input() {
  get_exit_code_for_error(InvalidInput)
  |> should.equal(exit_validation_error)
}

pub fn test_get_exit_code_for_spec_parse_error() {
  get_exit_code_for_error(SpecParseError)
  |> should.equal(exit_validation_error)
}

pub fn test_get_exit_code_for_load_error() {
  get_exit_code_for_error(LoadError)
  |> should.equal(exit_runtime_error)
}

pub fn test_get_exit_code_for_session_not_found() {
  get_exit_code_for_error(SessionNotFound)
  |> should.equal(exit_user_input_error)
}

pub fn test_get_exit_code_for_conflicting_flags() {
  get_exit_code_for_error(ConflictingFlags)
  |> should.equal(exit_user_input_error)
}

pub fn test_get_exit_code_for_internal_error() {
  get_exit_code_for_error(InternalError)
  |> should.equal(exit_internal_error)
}

// =============================================================================
// FACTORY FUNCTION TESTS
// =============================================================================

pub fn test_unified_error_creates_basic_error() {
  let error =
    unified_error(
      code: MissingInput,
      message: "Missing spec file",
      suggestion: "Provide a spec file path",
      fix_command: None,
    )

  error.code |> should.equal(MissingInput)
  error.message |> should.equal("Missing spec file")
  error.suggestion |> should.equal("Provide a spec file path")
  error.fix_command |> should.equal(None)
  error.exit_code |> should.equal(exit_user_input_error)
  error.severity |> should.equal(Error)
  dict.is_empty(error.context) |> should.equal(True)
}

pub fn test_unified_error_sets_severity_to_error() {
  let error =
    unified_error(
      code: ValidationFailed,
      message: "Validation failed",
      suggestion: "Check spec",
      fix_command: None,
    )

  error.severity |> should.equal(Error)
}

// =============================================================================
// CONTEXT MANIPULATION TESTS
// =============================================================================

pub fn test_with_context_adds_single_entry() {
  let error =
    unified_error(
      code: FileNotFound,
      message: "File not found",
      suggestion: "Check path",
      fix_command: None,
    )
    |> with_context(key: "path", value: "/tmp/spec.cue")

  dict.get(error.context, "path")
  |> should.equal(Ok("/tmp/spec.cue"))
}

pub fn test_with_context_preserves_existing_context() {
  let error =
    unified_error(
      code: FileNotFound,
      message: "File not found",
      suggestion: "Check path",
      fix_command: None,
    )
    |> with_context(key: "path", value: "/tmp/spec.cue")
    |> with_context(key: "reason", value: "Does not exist")

  dict.get(error.context, "path") |> should.equal(Ok("/tmp/spec.cue"))
  dict.get(error.context, "reason") |> should.equal(Ok("Does not exist"))
}

pub fn test_with_context_list_adds_multiple_entries() {
  let error =
    unified_error(
      code: FileNotFound,
      message: "File not found",
      suggestion: "Check path",
      fix_command: None,
    )
    |> with_context_list(entries: [
      #("path", "/tmp/spec.cue"),
      #("reason", "Does not exist"),
      #("checked", "True"),
    ])

  dict.get(error.context, "path") |> should.equal(Ok("/tmp/spec.cue"))
  dict.get(error.context, "reason") |> should.equal(Ok("Does not exist"))
  dict.get(error.context, "checked") |> should.equal(Ok("True"))
}

pub fn test_with_severity_changes_severity() {
  let error =
    unified_error(
      code: ValidationFailed,
      message: "Warning condition",
      suggestion: "Check this",
      fix_command: None,
    )
    |> with_severity(sev: Warning)

  error.severity |> should.equal(Warning)
}

// =============================================================================
// COMMON ERROR BUILDERS TESTS
// =============================================================================

pub fn test_missing_input_builder() {
  let error = missing_input(what: "spec file", suggestion: "Provide path")

  error.code |> should.equal(MissingInput)
  error.message |> should.equal("Missing required input: spec file")
  error.suggestion |> should.equal("Provide path")
  error.exit_code |> should.equal(exit_user_input_error)
}

pub fn test_validation_failed_builder() {
  let error =
    validation_failed(
      what: "spec",
      reason: "Invalid structure",
      suggestion: "Check spec",
    )

  error.code |> should.equal(ValidationFailed)
  error.message
  |> should.equal("Validation failed for spec: Invalid structure")
  error.exit_code |> should.equal(exit_validation_error)
}

pub fn test_file_not_found_builder() {
  let error = file_not_found(path: "/tmp/spec.cue")

  error.code |> should.equal(FileNotFound)
  error.message |> should.equal("File not found: /tmp/spec.cue")
  dict.get(error.context, "path") |> should.equal(Ok("/tmp/spec.cue"))
  error.fix_command |> should.equal(Some("ls -l /tmp/spec.cue"))
}

pub fn test_file_permission_denied_builder() {
  let error = file_permission_denied(path: "/etc/passwd", operation: "write")

  error.code |> should.equal(FilePermissionDenied)
  error.message
  |> should.equal("Permission denied: cannot write /etc/passwd")
  dict.get(error.context, "path") |> should.equal(Ok("/etc/passwd"))
  dict.get(error.context, "operation") |> should.equal(Ok("write"))
  error.exit_code |> should.equal(exit_runtime_error)
}

pub fn test_invalid_input_builder() {
  let error =
    invalid_input(
      input: "invalid-json",
      reason: "Unexpected character",
      suggestion: "Check JSON syntax",
    )

  error.code |> should.equal(InvalidInput)
  error.message
  |> should.equal("Invalid input: invalid-json (Unexpected character)")
  dict.get(error.context, "input") |> should.equal(Ok("invalid-json"))
  dict.get(error.context, "reason") |> should.equal(Ok("Unexpected character"))
}

pub fn test_spec_parse_error_builder() {
  let error =
    spec_parse_error(path: "/tmp/spec.cue", reason: "Missing required field")

  error.code |> should.equal(SpecParseError)
  error.message
  |> should.equal(
    "Failed to parse spec from /tmp/spec.cue: Missing required field",
  )
  dict.get(error.context, "path") |> should.equal(Ok("/tmp/spec.cue"))
  dict.get(error.context, "reason")
  |> should.equal(Ok("Missing required field"))
  error.fix_command |> should.equal(Some("intent validate /tmp/spec.cue"))
}

pub fn test_load_error_builder() {
  let error = load_error(resource: "session.json", reason: "File corrupted")

  error.code |> should.equal(LoadError)
  error.message
  |> should.equal("Failed to load session.json: File corrupted")
  dict.get(error.context, "resource") |> should.equal(Ok("session.json"))
  dict.get(error.context, "reason") |> should.equal(Ok("File corrupted"))
  error.exit_code |> should.equal(exit_runtime_error)
}

pub fn test_session_not_found_builder() {
  let error = session_not_found(session_id: "abc123")

  error.code |> should.equal(SessionNotFound)
  error.message |> should.equal("Session not found: abc123")
  dict.get(error.context, "session_id") |> should.equal(Ok("abc123"))
  error.fix_command |> should.equal(Some("intent sessions"))
}

pub fn test_conflicting_flags_builder() {
  let error = conflicting_flags(flag1: "json", flag2: "cue")

  error.code |> should.equal(ConflictingFlags)
  error.message |> should.equal("Conflicting flags: --json and --cue")
  dict.get(error.context, "flag1") |> should.equal(Ok("json"))
  dict.get(error.context, "flag2") |> should.equal(Ok("cue"))
  error.exit_code |> should.equal(exit_user_input_error)
}

pub fn test_internal_error_builder() {
  let error = internal_error(operation: "parsing spec", reason: "Null pointer")

  error.code |> should.equal(InternalError)
  error.message
  |> should.equal("Internal error during parsing spec: Null pointer")
  error.severity |> should.equal(Warning)
  error.exit_code |> should.equal(exit_internal_error)
}

// =============================================================================
// TEXT FORMATTING TESTS
// =============================================================================

pub fn test_format_error_brief() {
  let error =
    unified_error(
      code: FileNotFound,
      message: "File not found: /tmp/spec.cue",
      suggestion: "Check path",
      fix_command: None,
    )

  let formatted = format_error_brief(error)
  string.contains(formatted, "file_not_found") |> should.equal(True)
  string.contains(formatted, "File not found: /tmp/spec.cue")
  |> should.equal(True)
  string.contains(formatted, "exit 2") |> should.equal(True)
}

pub fn test_format_error_text_includes_all_fields() {
  let error =
    unified_error(
      code: InvalidInput,
      message: "Invalid spec",
      suggestion: "Check the spec",
      fix_command: Some("intent validate spec.cue"),
    )
    |> with_context(key: "field", value: "name")

  let formatted = format_error_text(error)
  string.contains(formatted, "Error") |> should.equal(True)
  string.contains(formatted, "invalid_input") |> should.equal(True)
  string.contains(formatted, "Invalid spec") |> should.equal(True)
  string.contains(formatted, "Check the spec") |> should.equal(True)
  string.contains(formatted, "intent validate spec.cue")
  |> should.equal(True)
  string.contains(formatted, "field: name") |> should.equal(True)
}

pub fn test_format_error_text_without_context() {
  let error =
    unified_error(
      code: MissingInput,
      message: "Missing spec",
      suggestion: "Provide spec",
      fix_command: None,
    )

  let formatted = format_error_text(error)
  string.contains(formatted, "Error") |> should.equal(True)
  string.contains(formatted, "missing_input") |> should.equal(True)
  string.contains(formatted, "Missing spec") |> should.equal(True)
  string.contains(formatted, "Provide spec") |> should.equal(True)
}

pub fn test_format_error_text_without_fix_command() {
  let error =
    unified_error(
      code: ValidationFailed,
      message: "Validation failed",
      suggestion: "Check spec",
      fix_command: None,
    )

  let formatted = format_error_text(error)
  string.contains(formatted, "Error") |> should.equal(True)
  string.contains(formatted, "Validation failed") |> should.equal(True)
  string.contains(formatted, "Fix Command")
  |> should.equal(False)
}

// =============================================================================
// JSON SERIALIZATION TESTS
// =============================================================================

pub fn test_unified_error_to_json_basic() {
  let error =
    unified_error(
      code: FileNotFound,
      message: "File not found",
      suggestion: "Check path",
      fix_command: None,
    )

  let json = unified_error_to_json(error)
  let json_str = json |> json.to_string()
  string.contains(json_str, "\"action\":\"error\"")
  |> should.equal(True)
  string.contains(json_str, "\"code\":\"file_not_found\"")
  |> should.equal(True)
  string.contains(json_str, "\"message\":\"File not found\"")
  |> should.equal(True)
}

pub fn test_unified_error_to_json_with_context() {
  let error =
    unified_error(
      code: InvalidInput,
      message: "Invalid input",
      suggestion: "Fix it",
      fix_command: Some("fix-cmd"),
    )
    |> with_context(key: "field", value: "name")

  let json = unified_error_to_json(error)
  let json_str = json |> json.to_string()
  string.contains(json_str, "\"field\":\"name\"")
  |> should.equal(True)
  string.contains(json_str, "\"fix_command\":\"fix-cmd\"")
  |> should.equal(True)
}

pub fn test_unified_error_to_json_preserves_exit_code() {
  let error =
    unified_error(
      code: ValidationFailed,
      message: "Validation failed",
      suggestion: "Check",
      fix_command: None,
    )

  let json = unified_error_to_json(error)
  let json_str = json |> json.to_string()
  string.contains(json_str, "\"exit_code\":3")
  |> should.equal(True)
}
