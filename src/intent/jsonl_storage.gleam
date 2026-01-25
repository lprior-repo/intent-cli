//// Generic JSONL Storage Pattern
////
//// Functional Core / Imperative Shell architecture for JSONL file storage.
//// Provides dependency injection for I/O operations to enable testing with mocks.
////
//// This module extracts the JSONL storage pattern from interview_storage.gleam
//// to be reusable for Vision, Shape, Spec, and Ready storage modules.
////
//// ## Pattern
////
//// 1. **Pure Functions**: Serialization, parsing, and content manipulation
//// 2. **I/O Function Types**: FileReader, FileWriter, DirectoryCreator
//// 3. **Dependency Injection**: I/O functions accept reader/writer/creator parameters
//// 4. **Simplifile Adapters**: Convenience wrappers for real file I/O
////
//// ## Usage Example
////
//// ```gleam
//// import gleam/json
//// import gleam/dynamic
//// import intent/jsonl_storage
////
//// // Define your record type
//// pub type MyRecord {
////   MyRecord(id: String, name: String, value: Int)
//// }
////
//// // Create encoder
//// fn my_encoder(record: MyRecord) -> json.Json {
////   json.object([
////     #("id", json.string(record.id)),
////     #("name", json.string(record.name)),
////     #("value", json.int(record.value)),
////   ])
//// }
////
//// // Create decoder
//// fn my_decoder(json_value: dynamic.Dynamic) -> Result(MyRecord, dynamic.DecodeErrors) {
////   // ... decoder implementation
//// }
////
//// // Create ID extractor
//// fn my_id_extractor(record: MyRecord) -> String {
////   record.id
//// }
////
//// // Use storage operations
//// let record = MyRecord(id: "123", name: "Example", value: 42)
//// jsonl_storage.append_to_jsonl(record, ".data/records.jsonl", my_encoder, my_id_extractor)
//// ```

import gleam/dynamic
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import simplifile

// =============================================================================
// File I/O Function Types (Dependency Injection)
// =============================================================================

/// File reader function type - takes path, returns content or error
pub type FileReader =
  fn(String) -> Result(String, String)

/// File writer function type - takes path and content, returns unit or error
pub type FileWriter =
  fn(String, String) -> Result(Nil, String)

/// Directory creator function type - takes path, returns unit or error
pub type DirectoryCreator =
  fn(String) -> Result(Nil, String)

// =============================================================================
// Simplifile Adapter Functions
// =============================================================================

/// Create a FileReader that uses simplifile
pub fn simplifile_reader() -> FileReader {
  fn(path: String) -> Result(String, String) {
    simplifile.read(path)
    |> result.map_error(fn(err) {
      "Failed to read file '" <> path <> "': " <> string.inspect(err)
    })
  }
}

/// Create a FileWriter that uses simplifile
pub fn simplifile_writer() -> FileWriter {
  fn(path: String, content: String) -> Result(Nil, String) {
    simplifile.write(path, content)
    |> result.map_error(fn(err) {
      let err_msg = case err {
        simplifile.Enoent -> "File or directory not found"
        simplifile.Eacces -> "Permission denied"
        simplifile.Enospc -> "No space left on device"
        simplifile.Eio -> "I/O error"
        _ -> "Unknown error"
      }
      "Failed to write file '" <> path <> "': " <> err_msg
    })
  }
}

/// Create a DirectoryCreator that uses simplifile
pub fn simplifile_dir_creator() -> DirectoryCreator {
  fn(path: String) -> Result(Nil, String) {
    simplifile.create_directory_all(path)
    |> result.map_error(fn(err) {
      let err_msg = case err {
        simplifile.Enoent -> "Parent directory not found"
        simplifile.Eacces -> "Permission denied"
        simplifile.Enospc -> "No space left on device"
        simplifile.Eio -> "I/O error"
        _ -> "Unknown error"
      }
      "Failed to create directory '" <> path <> "': " <> err_msg
    })
  }
}

// =============================================================================
// Pure JSONL Functions
// =============================================================================

/// Convert a record to a JSONL line (pure)
pub fn to_jsonl_line(record: a, encoder: fn(a) -> json.Json) -> String {
  record
  |> encoder
  |> json.to_string
}

/// Parse JSONL content to list of records (pure)
pub fn parse_jsonl_content(
  content: String,
  decoder: fn(dynamic.Dynamic) -> Result(a, dynamic.DecodeErrors),
) -> Result(List(a), String) {
  case string.length(string.trim(content)) {
    0 -> Ok([])
    _ -> {
      string.split(content, "\n")
      |> list.filter_map(fn(line) {
        case string.length(string.trim(line)) {
          0 -> Error(Nil)
          _ ->
            json.decode(line, decoder)
            |> result.map_error(fn(_) { Nil })
        }
      })
      |> Ok
    }
  }
}

/// Update JSONL content by replacing/adding a record (pure)
/// Strategy: Filter out old version of record (by ID) and append new version
pub fn update_jsonl_content(
  existing_content: String,
  record: a,
  encoder: fn(a) -> json.Json,
  id_extractor: fn(a) -> String,
) -> String {
  let new_id = id_extractor(record)
  let new_line = to_jsonl_line(record, encoder)

  let lines = case existing_content {
    "" -> []
    content -> string.split(content, "\n")
  }

  // Filter out lines with matching ID
  let filtered =
    list.filter(lines, fn(line) {
      case string.length(string.trim(line)) {
        0 -> True
        // Keep empty lines for now (will be filtered)
        _ ->
          // Try to decode and check ID
          case json.decode(line, dynamic.field("id", dynamic.string)) {
            Ok(line_id) -> line_id != new_id
            Error(_) -> True
            // Keep lines that don't decode
          }
      }
    })
    |> list.filter(fn(line) { string.length(string.trim(line)) > 0 })

  let all_lines = list.append(filtered, [new_line])
  string.join(all_lines, "\n")
}

/// Find a record by ID in a list (pure)
/// Note: Requires that records have an 'id' field that can be extracted
/// This is a generic function that works with any record type
pub fn find_by_id_with_extractor(
  records: List(a),
  id: String,
  id_extractor: fn(a) -> String,
) -> Result(a, String) {
  list.find(records, fn(record) { id_extractor(record) == id })
  |> result.map_error(fn(_) { "Record not found: " <> id })
}

/// Find a record by ID in a list (pure)
/// DEPRECATED: This function cannot be implemented generically in Gleam without reflection
/// Use find_by_id_with_extractor instead, which requires an id_extractor function
pub fn find_by_id(_records: List(a), _id: String) -> Result(a, String) {
  Error(
    "find_by_id is deprecated and cannot work generically. Use find_by_id_with_extractor instead.",
  )
}

// =============================================================================
// I/O Functions with Dependency Injection
// =============================================================================

/// Append record to JSONL file (with DI)
pub fn append_to_jsonl_with_io(
  record: a,
  jsonl_path: String,
  encoder: fn(a) -> json.Json,
  id_extractor: fn(a) -> String,
  reader: FileReader,
  writer: FileWriter,
  dir_creator: DirectoryCreator,
) -> Result(Nil, String) {
  let existing = reader(jsonl_path) |> result.unwrap("")
  let new_content =
    update_jsonl_content(existing, record, encoder, id_extractor)
  use _ <- result.try(ensure_parent_directory_with_io(jsonl_path, dir_creator))
  writer(jsonl_path, new_content)
}

/// List all records from JSONL file (with DI)
pub fn list_from_jsonl_with_io(
  jsonl_path: String,
  decoder: fn(dynamic.Dynamic) -> Result(a, dynamic.DecodeErrors),
  reader: FileReader,
) -> Result(List(a), String) {
  use content <- result.try(reader(jsonl_path))
  parse_jsonl_content(content, decoder)
}

/// Get record by ID from JSONL file (with DI)
pub fn get_from_jsonl_with_io(
  jsonl_path: String,
  id: String,
  decoder: fn(dynamic.Dynamic) -> Result(a, dynamic.DecodeErrors),
  reader: FileReader,
) -> Result(a, String) {
  use content <- result.try(reader(jsonl_path))

  // Parse each line and find the one with matching ID
  let lines = string.split(content, "\n")
  let matching_line =
    list.find(lines, fn(line) {
      case string.length(string.trim(line)) {
        0 -> False
        _ -> {
          // Try to extract ID field from JSON
          case json.decode(line, dynamic.field("id", dynamic.string)) {
            Ok(line_id) -> line_id == id
            Error(_) -> False
          }
        }
      }
    })

  case matching_line {
    Ok(line) -> {
      // Decode the full record
      json.decode(line, decoder)
      |> result.map_error(fn(_) { "Failed to decode record: " <> id })
    }
    Error(_) -> Error("Record not found: " <> id)
  }
}

// =============================================================================
// Utility Functions
// =============================================================================

/// Extract parent directory path from a file path (pure)
pub fn get_parent_directory(file_path: String) -> Result(String, Nil) {
  let parts = string.split(file_path, "/")
  let dir_parts = list.take(parts, list.length(parts) - 1)
  case list.length(dir_parts) {
    0 -> Error(Nil)
    _ -> Ok(string.join(dir_parts, "/"))
  }
}

/// Ensure parent directory exists for a file path (with DI)
pub fn ensure_parent_directory_with_io(
  file_path: String,
  dir_creator: DirectoryCreator,
) -> Result(Nil, String) {
  case get_parent_directory(file_path) {
    Error(Nil) -> Ok(Nil)
    Ok(dir_path) -> dir_creator(dir_path)
  }
}

/// Ensure parent directory exists for a file path using simplifile
pub fn ensure_parent_directory(file_path: String) -> Result(Nil, String) {
  ensure_parent_directory_with_io(file_path, simplifile_dir_creator())
}

// =============================================================================
// Simplifile Convenience Wrappers
// =============================================================================

/// Append record to JSONL file using simplifile
pub fn append_to_jsonl(
  record: a,
  jsonl_path: String,
  encoder: fn(a) -> json.Json,
  id_extractor: fn(a) -> String,
) -> Result(Nil, String) {
  append_to_jsonl_with_io(
    record,
    jsonl_path,
    encoder,
    id_extractor,
    simplifile_reader(),
    simplifile_writer(),
    simplifile_dir_creator(),
  )
}

/// List all records from JSONL file using simplifile
pub fn list_from_jsonl(
  jsonl_path: String,
  decoder: fn(dynamic.Dynamic) -> Result(a, dynamic.DecodeErrors),
) -> Result(List(a), String) {
  list_from_jsonl_with_io(jsonl_path, decoder, simplifile_reader())
}

/// Get record by ID from JSONL file using simplifile
pub fn get_from_jsonl(
  jsonl_path: String,
  id: String,
  decoder: fn(dynamic.Dynamic) -> Result(a, dynamic.DecodeErrors),
) -> Result(a, String) {
  get_from_jsonl_with_io(jsonl_path, id, decoder, simplifile_reader())
}
