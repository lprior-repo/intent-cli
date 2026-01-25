//// Vision Document Storage
//// JSONL persistence for vision documents following Functional Core / Imperative Shell pattern
//// Mirrors interview_storage.gleam architecture with dependency injection
////
//// Architecture: Functional Core / Imperative Shell
//// - Pure serialization/deserialization functions at the core
//// - File I/O operations accept reader/writer functions (dependency injection)
//// - Simplifile wrappers provided for convenience

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import intent/vision_types.{type Scenario, type Stakeholder, type VisionSection}
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

/// Vision document containing metadata and sections
pub type VisionDocument {
  VisionDocument(
    id: String,
    title: String,
    created_at: String,
    updated_at: String,
    sections: List(VisionSection),
  )
}

// =============================================================================
// JSON Serialization - Pure Functions
// =============================================================================

/// Convert Scenario to JSON
pub fn scenario_to_json(scenario: Scenario) -> json.Json {
  json.object([
    #("character", json.string(scenario.character)),
    #("persona", json.string(scenario.persona)),
    #("motivation", json.string(scenario.motivation)),
    #("simulation", json.string(scenario.simulation)),
    #("outcome", json.string(scenario.outcome)),
  ])
}

/// Convert Stakeholder to JSON
pub fn stakeholder_to_json(stakeholder: Stakeholder) -> json.Json {
  json.object([
    #("name", json.string(stakeholder.name)),
    #("role", json.string(stakeholder.role)),
    #("needs", json.array(stakeholder.needs, json.string)),
    #("pain_points", json.array(stakeholder.pain_points, json.string)),
  ])
}

/// Convert Option to JSON (None becomes null)
fn option_to_json(
  opt: option.Option(a),
  converter: fn(a) -> json.Json,
) -> json.Json {
  case opt {
    option.Some(value) -> converter(value)
    option.None -> json.null()
  }
}

/// Convert VisionSection to JSON
pub fn vision_section_to_json(section: VisionSection) -> json.Json {
  json.object([
    #("press_release", json.string(section.press_release)),
    #("persona", json.string(section.persona)),
    #("non_personas", json.array(section.non_personas, json.string)),
    #("north_star", json.string(section.north_star)),
    #("scenarios", json.array(section.scenarios, scenario_to_json)),
    #("replaces", option_to_json(section.replaces, json.string)),
    #("vorp", json.string(section.vorp)),
    #("out_of_scope", json.array(section.out_of_scope, json.string)),
  ])
}

/// Convert VisionDocument to JSON
pub fn vision_document_to_json(document: VisionDocument) -> json.Json {
  json.object([
    #("id", json.string(document.id)),
    #("title", json.string(document.title)),
    #("created_at", json.string(document.created_at)),
    #("updated_at", json.string(document.updated_at)),
    #("sections", json.array(document.sections, vision_section_to_json)),
  ])
}

/// Helper to convert JSON to string (for testing)
pub fn json_to_string(j: json.Json) -> String {
  json.to_string(j)
}

// =============================================================================
// JSON Deserialization - Decoders
// =============================================================================

fn scenario_decoder(
  json_value: dynamic.Dynamic,
) -> Result(Scenario, dynamic.DecodeErrors) {
  use character <- result.try(dynamic.field("character", dynamic.string)(
    json_value,
  ))
  use persona <- result.try(dynamic.field("persona", dynamic.string)(json_value))
  use motivation <- result.try(dynamic.field("motivation", dynamic.string)(
    json_value,
  ))
  use simulation <- result.try(dynamic.field("simulation", dynamic.string)(
    json_value,
  ))
  use outcome <- result.try(dynamic.field("outcome", dynamic.string)(json_value))

  Ok(vision_types.Scenario(
    character: character,
    persona: persona,
    motivation: motivation,
    simulation: simulation,
    outcome: outcome,
  ))
}

fn stakeholder_decoder(
  json_value: dynamic.Dynamic,
) -> Result(Stakeholder, dynamic.DecodeErrors) {
  use name <- result.try(dynamic.field("name", dynamic.string)(json_value))
  use role <- result.try(dynamic.field("role", dynamic.string)(json_value))
  use needs <- result.try(dynamic.field("needs", dynamic.list(dynamic.string))(
    json_value,
  ))
  use pain_points <- result.try(dynamic.field(
    "pain_points",
    dynamic.list(dynamic.string),
  )(json_value))

  Ok(vision_types.Stakeholder(
    name: name,
    role: role,
    needs: needs,
    pain_points: pain_points,
  ))
}

fn vision_section_decoder(
  json_value: dynamic.Dynamic,
) -> Result(VisionSection, dynamic.DecodeErrors) {
  use press_release <- result.try(dynamic.field("press_release", dynamic.string)(
    json_value,
  ))
  use persona <- result.try(dynamic.field("persona", dynamic.string)(json_value))
  use non_personas <- result.try(dynamic.field(
    "non_personas",
    dynamic.list(dynamic.string),
  )(json_value))
  use north_star <- result.try(dynamic.field("north_star", dynamic.string)(
    json_value,
  ))
  use scenarios <- result.try(dynamic.field(
    "scenarios",
    dynamic.list(scenario_decoder),
  )(json_value))
  use replaces <- result.try(dynamic.field(
    "replaces",
    dynamic.optional(dynamic.string),
  )(json_value))
  use vorp <- result.try(dynamic.field("vorp", dynamic.string)(json_value))
  use out_of_scope <- result.try(dynamic.field(
    "out_of_scope",
    dynamic.list(dynamic.string),
  )(json_value))

  Ok(vision_types.VisionSection(
    press_release: press_release,
    persona: persona,
    non_personas: non_personas,
    north_star: north_star,
    scenarios: scenarios,
    replaces: replaces,
    vorp: vorp,
    out_of_scope: out_of_scope,
  ))
}

fn vision_document_decoder(
  json_value: dynamic.Dynamic,
) -> Result(VisionDocument, dynamic.DecodeErrors) {
  use id <- result.try(dynamic.field("id", dynamic.string)(json_value))
  use title <- result.try(dynamic.field("title", dynamic.string)(json_value))
  use created_at <- result.try(dynamic.field("created_at", dynamic.string)(
    json_value,
  ))
  use updated_at <- result.try(dynamic.field("updated_at", dynamic.string)(
    json_value,
  ))
  use sections <- result.try(dynamic.field(
    "sections",
    dynamic.list(vision_section_decoder),
  )(json_value))

  Ok(VisionDocument(
    id: id,
    title: title,
    created_at: created_at,
    updated_at: updated_at,
    sections: sections,
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

/// Encode vision document to JSONL line (pure - for git storage)
pub fn vision_document_to_jsonl_line(document: VisionDocument) -> String {
  document
  |> vision_document_to_json
  |> json.to_string
}

/// Parse JSONL content into list of vision documents (pure)
/// Returns list of successfully parsed documents
pub fn parse_vision_documents_content(content: String) -> List(VisionDocument) {
  case string.length(string.trim(content)) {
    0 -> []
    _ -> {
      string.split(content, "\n")
      |> list.filter_map(fn(line) {
        case string.length(string.trim(line)) {
          0 -> Error(Nil)
          _ ->
            json.decode(line, vision_document_decoder)
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
  document: VisionDocument,
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

  let new_line = vision_document_to_jsonl_line(document)
  let all_lines = list.append(filtered, [new_line])
  string.join(all_lines, "\n")
}

/// Find a document by ID in parsed documents (pure)
pub fn find_document_by_id(
  documents: List(VisionDocument),
  document_id: String,
) -> Result(VisionDocument, String) {
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

/// Append vision document to JSONL file (with DI)
/// Each document ID appears once, most recent last (for efficient updates)
pub fn append_document_with_io(
  document: VisionDocument,
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

/// List all vision documents from JSONL file (with DI)
pub fn list_documents_with_io(
  jsonl_path: String,
  reader: FileReader,
) -> Result(List(VisionDocument), String) {
  use content <- result.try(reader(jsonl_path))
  Ok(parse_vision_documents_content(content))
}

/// Get vision document by ID from JSONL (with DI)
pub fn get_document_with_io(
  jsonl_path: String,
  document_id: String,
  reader: FileReader,
) -> Result(VisionDocument, String) {
  use documents <- result.try(list_documents_with_io(jsonl_path, reader))
  find_document_by_id(documents, document_id)
}

// =============================================================================
// Simplifile Convenience Wrappers
// =============================================================================

/// Append vision document to JSONL file using simplifile
pub fn append_document(
  document: VisionDocument,
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

/// List all vision documents from JSONL file using simplifile
pub fn list_documents(
  jsonl_path: String,
) -> Result(List(VisionDocument), String) {
  list_documents_with_io(jsonl_path, simplifile_reader())
}

/// Get vision document by ID from JSONL using simplifile
pub fn get_document(
  jsonl_path: String,
  document_id: String,
) -> Result(VisionDocument, String) {
  get_document_with_io(jsonl_path, document_id, simplifile_reader())
}
