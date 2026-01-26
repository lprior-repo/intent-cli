//// Comprehensive tests for intent/vision_storage.gleam
//// Tests JSONL persistence for vision documents following DI pattern
////
//// Design by Contract:
//// - Preconditions: Valid vision documents with all required fields
//// - Postconditions: JSONL format is correct, atomic writes succeed
//// - Invariants: Pure functions have no side effects, DI functions accept readers/writers

import gleam/dict
import gleam/list
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import intent/vision_storage
import intent/vision_types

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// VisionDocument Construction Tests
// ============================================================================

pub fn vision_document_creation_test() {
  let section =
    vision_types.VisionSection(
      title: "Test Section",
      description: "A test vision section",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc =
    vision_storage.VisionDocument(
      id: "doc-001",
      title: "Test Vision",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  doc.id
  |> should.equal("doc-001")

  doc.title
  |> should.equal("Test Vision")

  doc.sections
  |> list.length()
  |> should.equal(1)
}

// ============================================================================
// JSON Serialization Tests (Pure Functions)
// ============================================================================

pub fn scenario_to_json_test() {
  let scenario =
    vision_types.Scenario(
      character: "Agent Smith",
      persona: "AI agent",
      motivation: "Test motivation",
      simulation: "Test simulation",
      outcome: "Test outcome",
    )

  let json = vision_storage.scenario_to_json(scenario)
  let json_string = vision_storage.json_to_string(json)

  json_string
  |> string.contains("Agent Smith")
  |> should.be_true()

  json_string
  |> string.contains("AI agent")
  |> should.be_true()
}

pub fn stakeholder_to_json_test() {
  let stakeholder =
    vision_types.Stakeholder(
      name: "AI Agents",
      role: "Primary User",
      needs: ["Speed", "Accuracy"],
      pain_points: ["Latency"],
    )

  let json = vision_storage.stakeholder_to_json(stakeholder)
  let json_string = vision_storage.json_to_string(json)

  json_string
  |> string.contains("AI Agents")
  |> should.be_true()

  json_string
  |> string.contains("Primary User")
  |> should.be_true()
}

pub fn vision_section_to_json_test() {
  let section =
    vision_types.VisionSection(
      title: "The Vision",
      description: "A compelling vision",
      scenarios: [],
      stakeholders: [],
      principles: ["Principle 1", "Principle 2"],
    )

  let json = vision_storage.vision_section_to_json(section)
  let json_string = vision_storage.json_to_string(json)

  json_string
  |> string.contains("The Vision")
  |> should.be_true()

  json_string
  |> string.contains("Principle 1")
  |> should.be_true()
}

pub fn vision_document_to_json_test() {
  let section =
    vision_types.VisionSection(
      title: "Section 1",
      description: "Description 1",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc =
    vision_storage.VisionDocument(
      id: "doc-test",
      title: "Test Document",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let json = vision_storage.vision_document_to_json(doc)
  let json_string = vision_storage.json_to_string(json)

  json_string
  |> string.contains("doc-test")
  |> should.be_true()

  json_string
  |> string.contains("Test Document")
  |> should.be_true()
}

// ============================================================================
// JSONL Serialization Tests (Pure Functions)
// ============================================================================

pub fn vision_document_to_jsonl_line_test() {
  let section =
    vision_types.VisionSection(
      title: "Section",
      description: "Desc",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc =
    vision_storage.VisionDocument(
      id: "doc-jsonl",
      title: "JSONL Test",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let line = vision_storage.vision_document_to_jsonl_line(doc)

  // JSONL should be a single line with no newlines
  line
  |> string.contains("\n")
  |> should.be_false()

  // Should contain document ID
  line
  |> string.contains("doc-jsonl")
  |> should.be_true()
}

pub fn parse_vision_documents_content_empty_test() {
  let content = ""
  let documents = vision_storage.parse_vision_documents_content(content)

  documents
  |> should.equal([])
}

pub fn parse_vision_documents_content_single_test() {
  let section =
    vision_types.VisionSection(
      title: "Parse Test",
      description: "Test parsing",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc =
    vision_storage.VisionDocument(
      id: "parse-001",
      title: "Parse Doc",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let line = vision_storage.vision_document_to_jsonl_line(doc)
  let parsed = vision_storage.parse_vision_documents_content(line)

  parsed
  |> list.length()
  |> should.equal(1)

  let first = case parsed {
    [first, ..] -> first
    [] -> panic as "Expected one document"
  }

  first.id
  |> should.equal("parse-001")
}

pub fn parse_vision_documents_content_multiple_test() {
  let section =
    vision_types.VisionSection(
      title: "Multi",
      description: "Multi test",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc1 =
    vision_storage.VisionDocument(
      id: "multi-001",
      title: "Doc 1",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let doc2 =
    vision_storage.VisionDocument(
      id: "multi-002",
      title: "Doc 2",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let line1 = vision_storage.vision_document_to_jsonl_line(doc1)
  let line2 = vision_storage.vision_document_to_jsonl_line(doc2)
  let content = line1 <> "\n" <> line2

  let parsed = vision_storage.parse_vision_documents_content(content)

  parsed
  |> list.length()
  |> should.equal(2)
}

// ============================================================================
// Update Content Tests (Pure Functions)
// ============================================================================

pub fn update_documents_content_new_document_test() {
  let section =
    vision_types.VisionSection(
      title: "New",
      description: "New document",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc =
    vision_storage.VisionDocument(
      id: "new-001",
      title: "New Doc",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let existing_content = ""
  let new_content =
    vision_storage.update_documents_content(existing_content, doc)

  new_content
  |> string.contains("new-001")
  |> should.be_true()
}

pub fn update_documents_content_replace_existing_test() {
  let section1 =
    vision_types.VisionSection(
      title: "V1",
      description: "Version 1",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc_v1 =
    vision_storage.VisionDocument(
      id: "replace-001",
      title: "Original",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section1],
    )

  let section2 =
    vision_types.VisionSection(
      title: "V2",
      description: "Version 2",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc_v2 =
    vision_storage.VisionDocument(
      id: "replace-001",
      title: "Updated",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T01:00:00Z",
      sections: [section2],
    )

  let content_v1 = vision_storage.vision_document_to_jsonl_line(doc_v1)
  let content_v2 = vision_storage.update_documents_content(content_v1, doc_v2)

  // Parse to verify only one document exists
  let parsed = vision_storage.parse_vision_documents_content(content_v2)

  parsed
  |> list.length()
  |> should.equal(1)

  let updated = case parsed {
    [doc, ..] -> doc
    [] -> panic as "Expected one document"
  }

  updated.title
  |> should.equal("Updated")
}

// ============================================================================
// Find Document Tests (Pure Functions)
// ============================================================================

pub fn find_document_by_id_found_test() {
  let section =
    vision_types.VisionSection(
      title: "Find Test",
      description: "Test finding",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc1 =
    vision_storage.VisionDocument(
      id: "find-001",
      title: "Doc 1",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let doc2 =
    vision_storage.VisionDocument(
      id: "find-002",
      title: "Doc 2",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let documents = [doc1, doc2]
  let result = vision_storage.find_document_by_id(documents, "find-002")

  result
  |> should.be_ok()

  case result {
    Ok(found) ->
      found.title
      |> should.equal("Doc 2")
    Error(_) -> panic as "Expected Ok"
  }
}

pub fn find_document_by_id_not_found_test() {
  let section =
    vision_types.VisionSection(
      title: "Not Found",
      description: "Test not found",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc =
    vision_storage.VisionDocument(
      id: "notfound-001",
      title: "Doc",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let documents = [doc]
  let result = vision_storage.find_document_by_id(documents, "missing-id")

  result
  |> should.be_error()
}

// ============================================================================
// DI I/O Tests (Mock Readers/Writers)
// ============================================================================

pub fn append_document_with_io_test() {
  // Mock storage
  let storage = dict.new()

  // Mock reader that returns empty content initially
  let reader = fn(_path: String) -> Result(String, String) { Ok("") }

  // Mock writer that validates the content
  let writer = fn(_path: String, content: String) -> Result(Nil, String) {
    // Verify content is valid JSONL
    case string.contains(content, "io-test-001") {
      True -> Ok(Nil)
      False -> Error("Content validation failed")
    }
  }

  // Mock directory creator
  let dir_creator = fn(_path: String) -> Result(Nil, String) { Ok(Nil) }

  let section =
    vision_types.VisionSection(
      title: "IO Test",
      description: "Testing I/O",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc =
    vision_storage.VisionDocument(
      id: "io-test-001",
      title: "IO Doc",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let result =
    vision_storage.append_document_with_io(
      doc,
      ".test/visions.jsonl",
      reader,
      writer,
      dir_creator,
    )

  result
  |> should.be_ok()
}

pub fn list_documents_with_io_test() {
  let section =
    vision_types.VisionSection(
      title: "List Test",
      description: "Testing list",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc =
    vision_storage.VisionDocument(
      id: "list-001",
      title: "List Doc",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let content = vision_storage.vision_document_to_jsonl_line(doc)

  // Mock reader that returns prepared content
  let reader = fn(_path: String) -> Result(String, String) { Ok(content) }

  let result =
    vision_storage.list_documents_with_io(".test/visions.jsonl", reader)

  result
  |> should.be_ok()

  case result {
    Ok(documents) -> {
      documents
      |> list.length()
      |> should.equal(1)

      case documents {
        [first, ..] ->
          first.id
          |> should.equal("list-001")
        [] -> panic as "Expected one document"
      }
    }
    Error(_) -> panic as "Expected Ok"
  }
}

pub fn get_document_with_io_test() {
  let section =
    vision_types.VisionSection(
      title: "Get Test",
      description: "Testing get",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc =
    vision_storage.VisionDocument(
      id: "get-001",
      title: "Get Doc",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let content = vision_storage.vision_document_to_jsonl_line(doc)

  // Mock reader
  let reader = fn(_path: String) -> Result(String, String) { Ok(content) }

  let result =
    vision_storage.get_document_with_io(
      ".test/visions.jsonl",
      "get-001",
      reader,
    )

  result
  |> should.be_ok()

  case result {
    Ok(found) ->
      found.title
      |> should.equal("Get Doc")
    Error(_) -> panic as "Expected Ok"
  }
}

// ============================================================================
// Concurrent Access Pattern Tests
// ============================================================================

pub fn concurrent_update_same_document_test() {
  // Test that update_documents_content handles last-write-wins correctly
  let section =
    vision_types.VisionSection(
      title: "Concurrent Test",
      description: "Testing concurrent updates",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc_v1 =
    vision_storage.VisionDocument(
      id: "concurrent-001",
      title: "Version 1",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section],
    )

  let doc_v2 =
    vision_storage.VisionDocument(
      id: "concurrent-001",
      title: "Version 2",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T01:00:00Z",
      sections: [section],
    )

  // Simulate concurrent writes (both reading same initial state)
  let initial_content = ""
  let content_from_writer1 =
    vision_storage.update_documents_content(initial_content, doc_v1)
  let content_from_writer2 =
    vision_storage.update_documents_content(initial_content, doc_v2)

  // Last write wins - both should produce single-line JSONL
  let final_content = content_from_writer2

  let parsed = vision_storage.parse_vision_documents_content(final_content)

  // Should have exactly one document (last write wins)
  parsed
  |> list.length()
  |> should.equal(1)

  case parsed {
    [doc, ..] ->
      doc.title
      |> should.equal("Version 2")
    [] -> panic as "Expected one document"
  }
}

pub fn append_multiple_documents_atomicity_test() {
  // Test that multiple document appends maintain JSONL integrity
  let section1 =
    vision_types.VisionSection(
      title: "Doc 1",
      description: "First document",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let section2 =
    vision_types.VisionSection(
      title: "Doc 2",
      description: "Second document",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let section3 =
    vision_types.VisionSection(
      title: "Doc 3",
      description: "Third document",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  let doc1 =
    vision_storage.VisionDocument(
      id: "atomic-001",
      title: "Document 1",
      created_at: "2026-01-25T00:00:00Z",
      updated_at: "2026-01-25T00:00:00Z",
      sections: [section1],
    )

  let doc2 =
    vision_storage.VisionDocument(
      id: "atomic-002",
      title: "Document 2",
      created_at: "2026-01-25T00:01:00Z",
      updated_at: "2026-01-25T00:01:00Z",
      sections: [section2],
    )

  let doc3 =
    vision_storage.VisionDocument(
      id: "atomic-003",
      title: "Document 3",
      created_at: "2026-01-25T00:02:00Z",
      updated_at: "2026-01-25T00:02:00Z",
      sections: [section3],
    )

  // Sequentially build content (simulates serial writes)
  let content1 = vision_storage.update_documents_content("", doc1)
  let content2 = vision_storage.update_documents_content(content1, doc2)
  let content3 = vision_storage.update_documents_content(content2, doc3)

  // Verify all documents are present and parseable
  let parsed = vision_storage.parse_vision_documents_content(content3)

  parsed
  |> list.length()
  |> should.equal(3)

  // Verify all IDs are present
  let ids =
    list.map(parsed, fn(doc) { doc.id })
    |> list.sort(string.compare)

  ids
  |> should.equal(["atomic-001", "atomic-002", "atomic-003"])
}
