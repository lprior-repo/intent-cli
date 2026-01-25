//// Shape Document Storage
//// JSONL persistence for shape documents following Functional Core / Imperative Shell pattern
//// Mirrors vision_storage.gleam architecture with dependency injection

import gleam/dynamic
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import intent/planning_types.{
  type FeatureShape, type MVPSlice, type ShapeSection, FeatureShape, MVPSlice,
  ShapeSection,
}
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
// Data Types
// =============================================================================

/// Shape document containing metadata and shape section
pub type ShapeDocument {
  ShapeDocument(
    id: String,
    title: String,
    created_at: String,
    updated_at: String,
    shape: ShapeSection,
  )
}

// =============================================================================
// JSON Serialization - Pure Functions
// =============================================================================

/// Convert FeatureShape to JSON
pub fn feature_shape_to_json(feature: FeatureShape) -> json.Json {
  json.object([
    #("name", json.string(feature.name)),
    #("description", json.string(feature.description)),
  ])
}

/// Convert MVPSlice to JSON
pub fn mvp_slice_to_json(slice: MVPSlice) -> json.Json {
  json.object([
    #("description", json.string(slice.description)),
    #("features", json.array(slice.features, json.string)),
    #("shortcuts", json.array(slice.shortcuts, json.string)),
  ])
}

/// Convert ShapeSection to JSON
pub fn shape_section_to_json(section: ShapeSection) -> json.Json {
  json.object([
    #("features", json.array(section.features, feature_shape_to_json)),
    #("critical_path", json.array(section.critical_path, json.string)),
    #("mvp_slice", mvp_slice_to_json(section.mvp_slice)),
    #("post_mvp", json.array(section.post_mvp, json.string)),
    #("validation_moment", json.string(section.validation_moment)),
  ])
}

/// Convert ShapeDocument to JSON
pub fn shape_document_to_json(document: ShapeDocument) -> json.Json {
  json.object([
    #("id", json.string(document.id)),
    #("title", json.string(document.title)),
    #("created_at", json.string(document.created_at)),
    #("updated_at", json.string(document.updated_at)),
    #("shape", shape_section_to_json(document.shape)),
  ])
}

/// Helper to convert JSON to string (for testing)
pub fn json_to_string(j: json.Json) -> String {
  json.to_string(j)
}

// =============================================================================
// JSON Deserialization - Decoders
// =============================================================================

fn feature_shape_decoder(
  json_value: dynamic.Dynamic,
) -> Result(FeatureShape, dynamic.DecodeErrors) {
  use name <- result.try(dynamic.field("name", dynamic.string)(json_value))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    json_value,
  ))

  Ok(FeatureShape(name: name, description: description))
}

fn mvp_slice_decoder(
  json_value: dynamic.Dynamic,
) -> Result(MVPSlice, dynamic.DecodeErrors) {
  use description <- result.try(dynamic.field("description", dynamic.string)(
    json_value,
  ))
  use features <- result.try(dynamic.field(
    "features",
    dynamic.list(dynamic.string),
  )(json_value))
  use shortcuts <- result.try(dynamic.field(
    "shortcuts",
    dynamic.list(dynamic.string),
  )(json_value))

  Ok(MVPSlice(
    description: description,
    features: features,
    shortcuts: shortcuts,
  ))
}

fn shape_section_decoder(
  json_value: dynamic.Dynamic,
) -> Result(ShapeSection, dynamic.DecodeErrors) {
  use features <- result.try(dynamic.field(
    "features",
    dynamic.list(feature_shape_decoder),
  )(json_value))
  use critical_path <- result.try(dynamic.field(
    "critical_path",
    dynamic.list(dynamic.string),
  )(json_value))
  use mvp_slice <- result.try(dynamic.field("mvp_slice", mvp_slice_decoder)(
    json_value,
  ))
  use post_mvp <- result.try(dynamic.field(
    "post_mvp",
    dynamic.list(dynamic.string),
  )(json_value))
  use validation_moment <- result.try(dynamic.field(
    "validation_moment",
    dynamic.string,
  )(json_value))

  Ok(ShapeSection(
    features: features,
    critical_path: critical_path,
    mvp_slice: mvp_slice,
    post_mvp: post_mvp,
    validation_moment: validation_moment,
  ))
}

fn shape_document_decoder(
  json_value: dynamic.Dynamic,
) -> Result(ShapeDocument, dynamic.DecodeErrors) {
  use id <- result.try(dynamic.field("id", dynamic.string)(json_value))
  use title <- result.try(dynamic.field("title", dynamic.string)(json_value))
  use created_at <- result.try(dynamic.field("created_at", dynamic.string)(
    json_value,
  ))
  use updated_at <- result.try(dynamic.field("updated_at", dynamic.string)(
    json_value,
  ))
  use shape <- result.try(dynamic.field("shape", shape_section_decoder)(
    json_value,
  ))

  Ok(ShapeDocument(
    id: id,
    title: title,
    created_at: created_at,
    updated_at: updated_at,
    shape: shape,
  ))
}

fn document_id_decoder(
  json_value: dynamic.Dynamic,
) -> Result(String, dynamic.DecodeErrors) {
  dynamic.field("id", dynamic.string)(json_value)
}

// =============================================================================
// JSONL Operations - Pure Functions
// =============================================================================

/// Encode shape document to JSONL line (pure - for git storage)
pub fn shape_document_to_jsonl_line(document: ShapeDocument) -> String {
  document
  |> shape_document_to_json
  |> json.to_string
}

/// Parse JSONL content into list of shape documents (pure)
/// Returns list of successfully parsed documents
pub fn parse_shape_documents_content(content: String) -> List(ShapeDocument) {
  case string.length(string.trim(content)) {
    0 -> []
    _ -> {
      string.split(content, "\n")
      |> list.filter_map(fn(line) {
        case string.length(string.trim(line)) {
          0 -> Error(Nil)
          _ ->
            json.decode(line, shape_document_decoder)
            |> result.map_error(fn(_) { Nil })
        }
      })
    }
  }
}

/// Update documents content by replacing/adding a document (pure)
/// Filters out existing document with same ID and appends the new version
/// Returns the new complete content string
pub fn update_documents_content(
  existing_content: String,
  document: ShapeDocument,
) -> String {
  let lines = case existing_content {
    "" -> []
    content -> string.split(content, "\n")
  }

  let filtered =
    list.filter(lines, fn(line) {
      // Parse each line and keep if document ID doesn't match
      case json.decode(line, document_id_decoder) {
        Ok(id) -> id != document.id
        Error(_) -> True
      }
    })

  let new_line = shape_document_to_jsonl_line(document)
  let all_lines = list.append(filtered, [new_line])
  string.join(all_lines, "\n")
}

/// Find a document by ID in parsed documents (pure)
pub fn find_document_by_id(
  documents: List(ShapeDocument),
  document_id: String,
) -> Result(ShapeDocument, String) {
  list.find(documents, fn(d) { d.id == document_id })
  |> result.map_error(fn(_) { "Document not found: " <> document_id })
}

/// Extract parent directory path from a file path (pure)
pub fn get_parent_directory(file_path: String) -> Result(String, Nil) {
  let parts = string.split(file_path, "/")
  let dir_parts = list.take(parts, list.length(parts) - 1)
  case list.length(dir_parts) {
    0 -> Error(Nil)
    _ -> Ok(string.join(dir_parts, "/"))
  }
}

// =============================================================================
// I/O Operations with Dependency Injection
// =============================================================================

/// Ensure parent directory exists for a file path (with DI)
pub fn ensure_parent_directory_with_io(
  file_path: String,
  dir_creator: DirectoryCreator,
) -> Result(Nil, String) {
  case get_parent_directory(file_path) {
    Error(_) -> Ok(Nil)
    Ok(dir_path) -> dir_creator(dir_path)
  }
}

/// Append shape document to JSONL file (with DI)
/// Each document ID appears once, most recent last (for efficient updates)
pub fn append_document_with_io(
  document: ShapeDocument,
  jsonl_path: String,
  reader: FileReader,
  writer: FileWriter,
  dir_creator: DirectoryCreator,
) -> Result(Nil, String) {
  let existing = reader(jsonl_path) |> result.unwrap("")
  let new_content = update_documents_content(existing, document)
  use _ <- result.try(ensure_parent_directory_with_io(jsonl_path, dir_creator))
  writer(jsonl_path, new_content)
}

/// List all shape documents from JSONL file (with DI)
pub fn list_documents_with_io(
  jsonl_path: String,
  reader: FileReader,
) -> Result(List(ShapeDocument), String) {
  use content <- result.try(reader(jsonl_path))
  Ok(parse_shape_documents_content(content))
}

/// Get shape document by ID from JSONL (with DI)
pub fn get_document_with_io(
  jsonl_path: String,
  document_id: String,
  reader: FileReader,
) -> Result(ShapeDocument, String) {
  use documents <- result.try(list_documents_with_io(jsonl_path, reader))
  find_document_by_id(documents, document_id)
}

// =============================================================================
// Simplifile Convenience Wrappers
// =============================================================================

/// Append shape document to JSONL file using simplifile
pub fn append_document(
  document: ShapeDocument,
  jsonl_path: String,
) -> Result(Nil, String) {
  append_document_with_io(
    document,
    jsonl_path,
    simplifile_reader(),
    simplifile_writer(),
    simplifile_dir_creator(),
  )
}

/// List all shape documents from JSONL file using simplifile
pub fn list_documents(jsonl_path: String) -> Result(List(ShapeDocument), String) {
  list_documents_with_io(jsonl_path, simplifile_reader())
}

/// Get shape document by ID from JSONL using simplifile
pub fn get_document(
  jsonl_path: String,
  document_id: String,
) -> Result(ShapeDocument, String) {
  get_document_with_io(jsonl_path, document_id, simplifile_reader())
}
