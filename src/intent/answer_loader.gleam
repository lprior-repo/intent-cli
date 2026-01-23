/// Answer loader for pre-filling interview responses
/// Loads question answers from JSON files
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/json
import simplifile

/// Error types for answer loading
pub type AnswerLoaderError {
  FileNotFound(path: String)
  PermissionDenied(path: String)
  ParseError(path: String, message: String)
  SchemaError(message: String)
  IoError(message: String)
}

/// Load answers from a JSON file
/// Returns a dict mapping question IDs to answer strings
pub fn load_from_file(
  path: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  case simplifile.read(path) {
    Error(simplifile.Enoent) -> Error(FileNotFound(path))
    Error(simplifile.Eacces) -> Error(PermissionDenied(path))
    Error(err) -> Error(IoError(simplifile.describe_error(err)))
    Ok(content) -> parse_answers(content, path)
  }
}

fn parse_answers(
  content: String,
  path: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  case json.decode(content, dynamic.dict(dynamic.string, dynamic.string)) {
    Ok(answers) -> Ok(answers)
    Error(_) -> Error(ParseError(path, "Invalid JSON format"))
  }
}
