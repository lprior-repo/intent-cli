// Answer loader module - loads pre-filled answers from files
import gleam/dict.{type Dict}
import gleam/result
import simplifile

pub type AnswerLoaderError {
  FileNotFound(path: String)
  PermissionDenied(path: String)
  ParseError(path: String, message: String)
  SchemaError(message: String)
  IoError(message: String)
}

/// Load answers from a file (JSON format)
pub fn load_from_file(path: String) -> Result(Dict(String, String), AnswerLoaderError) {
  case simplifile.read(path) {
    Error(_) -> Error(FileNotFound(path))
    Ok(_contents) -> {
      // TODO: Implement JSON parsing
      // For now, return an empty dict to allow compilation
      Ok(dict.new())
    }
  }
}
