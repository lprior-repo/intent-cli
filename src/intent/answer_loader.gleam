//// Answer Loader - Load pre-filled answers from JSON files for non-interactive interviews
////
//// Supports loading answers from JSON files where keys are question IDs
//// and values are answer strings. Used with `intent interview --answers=file.json`.

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/json
import gleam/string
import simplifile

/// Errors that can occur when loading answer files
pub type AnswerLoaderError {
  FileNotFound(path: String)
  PermissionDenied(path: String)
  ParseError(path: String, message: String)
  SchemaError(message: String)
  IoError(message: String)
}

/// Load answers from a JSON file
///
/// Expected format:
/// {
///   "question-id-1": "THE SYSTEM SHALL authenticate users",
///   "question-id-2": "THE SYSTEM SHALL return 200 OK on success"
/// }
pub fn load_from_file(
  path: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  case simplifile.read(path) {
    Error(simplifile.Enoent) -> Error(FileNotFound(path))
    Error(simplifile.Eacces) -> Error(PermissionDenied(path))
    Error(_) -> Error(IoError("Failed to read file: " <> path))
    Ok(content) -> parse_answers_json(content, path)
  }
}

/// Parse JSON content into a dictionary of answers
fn parse_answers_json(
  content: String,
  path: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  case json.decode(content, dynamic.dict(dynamic.string, dynamic.string)) {
    Ok(answers_dict) -> {
      // Validate that all values are non-empty strings
      case validate_answers(answers_dict) {
        Ok(_) -> Ok(answers_dict)
        Error(msg) -> Error(SchemaError(msg))
      }
    }
    Error(_) ->
      Error(ParseError(
        path,
        "Invalid JSON format. Expected object with string keys and values.",
      ))
  }
}

/// Validate that all answer values are non-empty
fn validate_answers(answers: Dict(String, String)) -> Result(Nil, String) {
  let empty_answers =
    answers
    |> dict.filter(fn(_key, value) { value == "" })
    |> dict.keys

  case empty_answers {
    [] -> Ok(Nil)
    keys ->
      Error("Found empty answers for questions: " <> debug_list_string(keys))
  }
}

/// Helper to format list as string (since string.inspect for list is verbose)
fn debug_list_string(items: List(String)) -> String {
  "[" <> string.join(items, ", ") <> "]"
}

// =============================================================================
// AI-FRIENDLY ERROR FORMATTING
// =============================================================================

/// Format error as AI-friendly CUE structure
/// Returns structured error with action, context, suggestion, and recovery steps
pub fn format_error_ai(error: AnswerLoaderError) -> String {
  case error {
    FileNotFound(path) ->
      "{\n"
      <> "    action: \"file_error\"\n"
      <> "    error: {\n"
      <> "        type: \"file_not_found\"\n"
      <> "        message: \"Answer file not found: "
      <> path
      <> "\"\n"
      <> "        context: {\n"
      <> "            path: \""
      <> path
      <> "\"\n"
      <> "            expected_location: \"JSON file with pre-filled answers\"\n"
      <> "        }\n"
      <> "    }\n"
      <> "    suggestion: \"Create the answers file or check the path\"\n"
      <> "    recovery: [\n"
      <> "        \"Check if the file path is correct\",\n"
      <> "        \"Create the file with format: {\\\"question-id\\\": \\\"answer\\\"}\",\n"
      <> "        \"Ensure the file has .json extension\",\n"
      <> "        \"Verify the file is in the expected directory\"\n"
      <> "    ]\n"
      <> "}"

    PermissionDenied(path) ->
      "{\n"
      <> "    action: \"permission_error\"\n"
      <> "    error: {\n"
      <> "        type: \"permission_denied\"\n"
      <> "        message: \"Permission denied reading answer file: "
      <> path
      <> "\"\n"
      <> "        context: {\n"
      <> "            path: \""
      <> path
      <> "\"\n"
      <> "            operation: \"read\"\n"
      <> "        }\n"
      <> "    }\n"
      <> "    suggestion: \"Fix file permissions to allow read access\"\n"
      <> "    recovery: [\n"
      <> "        \"Run: chmod +r "
      <> path
      <> "\",\n"
      <> "        \"Check file ownership with: ls -l "
      <> path
      <> "\",\n"
      <> "        \"Ensure you have read permissions for the file\",\n"
      <> "        \"Contact system administrator if access is required\"\n"
      <> "    ]\n"
      <> "}"

    ParseError(path, msg) ->
      "{\n"
      <> "    action: \"parse_error\"\n"
      <> "    error: {\n"
      <> "        type: \"json_parse_error\"\n"
      <> "        message: \"Failed to parse answer file: "
      <> msg
      <> "\"\n"
      <> "        context: {\n"
      <> "            path: \""
      <> path
      <> "\"\n"
      <> "            format: \"JSON\"\n"
      <> "            details: \""
      <> escape_json_string(msg)
      <> "\"\n"
      <> "        }\n"
      <> "    }\n"
      <> "    suggestion: \"Fix JSON syntax errors in the answer file\"\n"
      <> "    recovery: [\n"
      <> "        \"Validate JSON with: cat "
      <> path
      <> " | jq .\",\n"
      <> "        \"Check for missing commas, quotes, or brackets\",\n"
      <> "        \"Ensure format is: {\\\"question-id\\\": \\\"answer\\\"}\",\n"
      <> "        \"Use a JSON validator or linter to find syntax errors\"\n"
      <> "    ]\n"
      <> "}"

    SchemaError(msg) ->
      "{\n"
      <> "    action: \"schema_error\"\n"
      <> "    error: {\n"
      <> "        type: \"answer_validation_error\"\n"
      <> "        message: \"Answer file schema validation failed: "
      <> msg
      <> "\"\n"
      <> "        context: {\n"
      <> "            expected_schema: \"Object with string keys and non-empty string values\"\n"
      <> "            validation_error: \""
      <> escape_json_string(msg)
      <> "\"\n"
      <> "        }\n"
      <> "    }\n"
      <> "    suggestion: \"Ensure all answers are non-empty strings\"\n"
      <> "    recovery: [\n"
      <> "        \"Check that all answer values are non-empty strings\",\n"
      <> "        \"Remove any empty string values from the JSON\",\n"
      <> "        \"Ensure format matches: {\\\"question-id\\\": \\\"THE SYSTEM SHALL...\\\"}\",\n"
      <> "        \"Verify all keys are valid question IDs\"\n"
      <> "    ]\n"
      <> "}"

    IoError(msg) ->
      "{\n"
      <> "    action: \"io_error\"\n"
      <> "    error: {\n"
      <> "        type: \"file_read_error\"\n"
      <> "        message: \"I/O error reading answer file: "
      <> msg
      <> "\"\n"
      <> "        context: {\n"
      <> "            operation: \"file_read\"\n"
      <> "            error_details: \""
      <> escape_json_string(msg)
      <> "\"\n"
      <> "        }\n"
      <> "    }\n"
      <> "    suggestion: \"Check file system and disk status\"\n"
      <> "    recovery: [\n"
      <> "        \"Verify disk space is available with: df -h\",\n"
      <> "        \"Check for file system errors\",\n"
      <> "        \"Ensure the file is not corrupted\",\n"
      <> "        \"Try copying the file to a different location\"\n"
      <> "    ]\n"
      <> "}"
  }
}

/// Format error as human-readable text with context and recovery steps
pub fn format_error_text(error: AnswerLoaderError) -> String {
  case error {
    FileNotFound(path) ->
      "Error: Answer file not found: "
      <> path
      <> "\n\n"
      <> "Context:\n"
      <> "  path: "
      <> path
      <> "\n"
      <> "  expected_location: JSON file with pre-filled answers\n\n"
      <> "Suggestion: Create the answers file or check the path\n\n"
      <> "Recovery Steps:\n"
      <> "  1. Check if the file path is correct\n"
      <> "  2. Create the file with format: {\"question-id\": \"answer\"}\n"
      <> "  3. Ensure the file has .json extension\n"
      <> "  4. Verify the file is in the expected directory"

    PermissionDenied(path) ->
      "Error: Permission denied reading answer file: "
      <> path
      <> "\n\n"
      <> "Context:\n"
      <> "  path: "
      <> path
      <> "\n"
      <> "  operation: read\n\n"
      <> "Suggestion: Fix file permissions to allow read access\n\n"
      <> "Recovery Steps:\n"
      <> "  1. Run: chmod +r "
      <> path
      <> "\n"
      <> "  2. Check file ownership with: ls -l "
      <> path
      <> "\n"
      <> "  3. Ensure you have read permissions for the file\n"
      <> "  4. Contact system administrator if access is required"

    ParseError(path, msg) ->
      "Error: Failed to parse answer file: "
      <> msg
      <> "\n\n"
      <> "Context:\n"
      <> "  path: "
      <> path
      <> "\n"
      <> "  format: JSON\n"
      <> "  details: "
      <> msg
      <> "\n\n"
      <> "Suggestion: Fix JSON syntax errors in the answer file\n\n"
      <> "Recovery Steps:\n"
      <> "  1. Validate JSON with: cat "
      <> path
      <> " | jq .\n"
      <> "  2. Check for missing commas, quotes, or brackets\n"
      <> "  3. Ensure format is: {\"question-id\": \"answer\"}\n"
      <> "  4. Use a JSON validator or linter to find syntax errors"

    SchemaError(msg) ->
      "Error: Answer file schema validation failed: "
      <> msg
      <> "\n\n"
      <> "Context:\n"
      <> "  expected_schema: Object with string keys and non-empty string values\n"
      <> "  validation_error: "
      <> msg
      <> "\n\n"
      <> "Suggestion: Ensure all answers are non-empty strings\n\n"
      <> "Recovery Steps:\n"
      <> "  1. Check that all answer values are non-empty strings\n"
      <> "  2. Remove any empty string values from the JSON\n"
      <> "  3. Ensure format matches: {\"question-id\": \"THE SYSTEM SHALL...\"}\n"
      <> "  4. Verify all keys are valid question IDs"

    IoError(msg) ->
      "Error: I/O error reading answer file: "
      <> msg
      <> "\n\n"
      <> "Context:\n"
      <> "  operation: file_read\n"
      <> "  error_details: "
      <> msg
      <> "\n\n"
      <> "Suggestion: Check file system and disk status\n\n"
      <> "Recovery Steps:\n"
      <> "  1. Verify disk space is available with: df -h\n"
      <> "  2. Check for file system errors\n"
      <> "  3. Ensure the file is not corrupted\n"
      <> "  4. Try copying the file to a different location"
  }
}

/// Escape special characters in JSON strings
fn escape_json_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}
