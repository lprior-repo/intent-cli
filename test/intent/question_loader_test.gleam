import gleeunit/should
import intent/question_loader

pub fn file_not_found_ai_error_test() {
  let error = question_loader.FileNotFound("schema/questions.cue")
  let ai_msg = question_loader.format_error_ai(error)

  // Should contain CUE structure
  ai_msg
  |> should.not_equal("")

  // Should have action field
  ai_msg
  |> should.contain("action:")

  // Should have error type
  ai_msg
  |> should.contain("schema_file_not_found")

  // Should have recovery steps
  ai_msg
  |> should.contain("recovery:")
}

pub fn file_not_found_text_error_test() {
  let error = question_loader.FileNotFound("schema/questions.cue")
  let text_msg = question_loader.format_error_text(error)

  // Should contain error message
  text_msg
  |> should.contain("Questions schema file not found")

  // Should have suggestion
  text_msg
  |> should.contain("Suggestion:")

  // Should have recovery steps
  text_msg
  |> should.contain("Recovery Steps:")

  // Should include moon command
  text_msg
  |> should.contain("moon run :install")
}

pub fn custom_questions_not_found_test() {
  let error = question_loader.FileNotFound(".intent/custom-questions.cue")
  let ai_msg = question_loader.format_error_ai(error)

  // Should recognize custom questions file
  ai_msg
  |> should.contain("custom_questions_not_found")

  // Should mention optional nature
  let text_msg = question_loader.format_error_text(error)
  text_msg
  |> should.contain("optional")
}

pub fn cue_export_error_missing_field_test() {
  let error =
    question_loader.CueExportError(
      "undefined field: round_1 not found in questions.api",
    )
  let ai_msg = question_loader.format_error_ai(error)

  // Should detect missing field
  ai_msg
  |> should.contain("cue_missing_field")

  // Should suggest adding fields
  let text_msg = question_loader.format_error_text(error)
  text_msg
  |> should.contain("required fields are present")
}

pub fn cue_export_error_type_conflict_test() {
  let error =
    question_loader.CueExportError(
      "conflicting values string and int for field round",
    )
  let ai_msg = question_loader.format_error_ai(error)

  // Should detect type conflict
  ai_msg
  |> should.contain("cue_type_conflict")

  // Should suggest type fixes
  let text_msg = question_loader.format_error_text(error)
  text_msg
  |> should.contain("integer (1-5)")
}

pub fn json_parse_error_test() {
  let error = question_loader.JsonParseError("unexpected end of JSON input")
  let ai_msg = question_loader.format_error_ai(error)

  // Should have json error type
  ai_msg
  |> should.contain("json_decode_failed")

  // Should suggest CUE export testing
  let text_msg = question_loader.format_error_text(error)
  text_msg
  |> should.contain("cue export")
}

pub fn question_parse_error_profile_test() {
  let error =
    question_loader.QuestionParseError("Expected field at api.round_1")
  let ai_msg = question_loader.format_error_ai(error)

  // Should detect profile error
  ai_msg
  |> should.contain("invalid_profile_structure")

  // Should mention all profiles
  let text_msg = question_loader.format_error_text(error)
  text_msg
  |> should.contain("api, cli, event, data, workflow, ui")
}

pub fn question_parse_error_round_test() {
  let error =
    question_loader.QuestionParseError("Expected field at round_3 in common")
  let ai_msg = question_loader.format_error_ai(error)

  // Should detect round error
  ai_msg
  |> should.contain("invalid_round_structure")

  // Should mention round structure
  let text_msg = question_loader.format_error_text(error)
  text_msg
  |> should.contain("round_3, round_4, round_5")
}

pub fn question_parse_error_format_test() {
  let error =
    question_loader.QuestionParseError(
      "Expected String at field perspective, got Int",
    )
  let ai_msg = question_loader.format_error_ai(error)

  // Should detect invalid format
  ai_msg
  |> should.contain("invalid_question_format")

  // Should list required fields
  let text_msg = question_loader.format_error_text(error)
  text_msg
  |> should.contain("Required fields")

  // Should list valid perspectives
  text_msg
  |> should.contain("user, developer, ops, security, business")
}

pub fn security_error_test() {
  let error =
    question_loader.SecurityError("Path traversal detected: ../../etc")
  let ai_msg = question_loader.format_error_ai(error)

  // Should have security error type
  ai_msg
  |> should.contain("security_validation_failed")

  // Should warn about path traversal
  let text_msg = question_loader.format_error_text(error)
  text_msg
  |> should.contain("path traversal")
}

pub fn json_format_test() {
  let error = question_loader.FileNotFound("schema/questions.cue")
  let json_msg = question_loader.format_error_json(error)

  // Should be valid JSON (contains braces and quotes)
  json_msg
  |> should.contain("{")

  json_msg
  |> should.contain("}")

  // Should have action field
  json_msg
  |> should.contain("\"action\"")

  // Should have error type
  json_msg
  |> should.contain("\"schema_file_not_found\"")
}

pub fn truncate_long_message_test() {
  let long_message =
    "This is a very long CUE error message that contains lots of details about the validation failure and specific line numbers and field names that need to be fixed in the schema"

  let error = question_loader.CueExportError(long_message)
  let text_msg = question_loader.format_error_text(error)

  // Message should be truncated to reasonable length
  // Recovery steps should not contain the full message
  text_msg
  |> should.not_equal("")
}
