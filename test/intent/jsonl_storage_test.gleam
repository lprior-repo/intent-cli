//// Tests for generic JSONL storage pattern
//// Tests pure functions and I/O functions with mock dependencies

import gleam/dynamic
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import gleeunit/should
import intent/jsonl_storage

// =============================================================================
// Test Record Type
// =============================================================================

/// Simple test record to verify generic pattern
pub type TestRecord {
  TestRecord(id: String, name: String, value: Int)
}

/// Encoder for TestRecord
fn test_record_encoder(record: TestRecord) -> json.Json {
  json.object([
    #("id", json.string(record.id)),
    #("name", json.string(record.name)),
    #("value", json.int(record.value)),
  ])
}

/// Decoder for TestRecord
fn test_record_decoder(
  json_value: dynamic.Dynamic,
) -> Result(TestRecord, dynamic.DecodeErrors) {
  use id <- result.try(dynamic.field("id", dynamic.string)(json_value))
  use name <- result.try(dynamic.field("name", dynamic.string)(json_value))
  use value <- result.try(dynamic.field("value", dynamic.int)(json_value))
  Ok(TestRecord(id: id, name: name, value: value))
}

/// ID extractor for TestRecord
fn test_record_id_extractor(record: TestRecord) -> String {
  record.id
}

// =============================================================================
// Mock I/O Functions
// =============================================================================

/// Mock file reader that returns provided content
fn mock_reader(content: String) -> jsonl_storage.FileReader {
  fn(_path: String) -> Result(String, String) { Ok(content) }
}

/// Mock file reader that returns error
fn mock_reader_error() -> jsonl_storage.FileReader {
  fn(_path: String) -> Result(String, String) { Error("Mock read error") }
}

/// Mock file writer that returns success
fn mock_writer() -> jsonl_storage.FileWriter {
  fn(_path: String, _content: String) -> Result(Nil, String) { Ok(Nil) }
}

/// Mock directory creator that returns success
fn mock_dir_creator() -> jsonl_storage.DirectoryCreator {
  fn(_path: String) -> Result(Nil, String) { Ok(Nil) }
}

// =============================================================================
// Pure Function Tests
// =============================================================================

/// Test: to_jsonl_line encodes record to valid JSON line
pub fn test_to_jsonl_line_encodes_correctly_test() {
  let record = TestRecord(id: "test-1", name: "Test Record", value: 42)
  let line = jsonl_storage.to_jsonl_line(record, test_record_encoder)

  // Should contain all fields
  line |> string.contains("\"id\":\"test-1\"") |> should.be_true()
  line |> string.contains("\"name\":\"Test Record\"") |> should.be_true()
  line |> string.contains("\"value\":42") |> should.be_true()

  // Should not contain newlines (single line)
  line |> string.contains("\n") |> should.be_false()
}

/// Test: parse_jsonl_content handles empty content
pub fn test_parse_jsonl_content_empty_test() {
  let content = ""
  let result = jsonl_storage.parse_jsonl_content(content, test_record_decoder)

  result |> should.be_ok()
  result |> result.unwrap([]) |> should.equal([])
}

/// Test: parse_jsonl_content handles single record
pub fn test_parse_jsonl_content_single_record_test() {
  let content = "{\"id\":\"test-1\",\"name\":\"Test\",\"value\":42}"
  let result = jsonl_storage.parse_jsonl_content(content, test_record_decoder)

  result |> should.be_ok()
  let records = result |> result.unwrap([])
  records |> list.length() |> should.equal(1)

  case records {
    [record] -> {
      record.id |> should.equal("test-1")
      record.name |> should.equal("Test")
      record.value |> should.equal(42)
    }
    _ -> should.fail()
  }
}

/// Test: parse_jsonl_content handles multiple records
pub fn test_parse_jsonl_content_multiple_records_test() {
  let content =
    "{\"id\":\"test-1\",\"name\":\"First\",\"value\":1}\n{\"id\":\"test-2\",\"name\":\"Second\",\"value\":2}"
  let result = jsonl_storage.parse_jsonl_content(content, test_record_decoder)

  result |> should.be_ok()
  let records = result |> result.unwrap([])
  records |> list.length() |> should.equal(2)
}

/// Test: parse_jsonl_content skips invalid lines
pub fn test_parse_jsonl_content_with_invalid_lines_test() {
  let content =
    "{\"id\":\"test-1\",\"name\":\"Valid\",\"value\":1}\ninvalid json\n{\"id\":\"test-2\",\"name\":\"Also Valid\",\"value\":2}"
  let result = jsonl_storage.parse_jsonl_content(content, test_record_decoder)

  result |> should.be_ok()
  let records = result |> result.unwrap([])
  // Should skip the invalid line
  records |> list.length() |> should.equal(2)
}

/// Test: update_jsonl_content adds new record
pub fn test_update_jsonl_content_new_record_test() {
  let existing = ""
  let new_record = TestRecord(id: "test-1", name: "New", value: 100)

  let updated =
    jsonl_storage.update_jsonl_content(
      existing,
      new_record,
      test_record_encoder,
      test_record_id_extractor,
    )

  updated |> string.contains("\"id\":\"test-1\"") |> should.be_true()
  updated |> string.contains("\"name\":\"New\"") |> should.be_true()
}

/// Test: update_jsonl_content replaces existing record
pub fn test_update_jsonl_content_replace_existing_test() {
  let existing =
    "{\"id\":\"test-1\",\"name\":\"Old\",\"value\":1}\n{\"id\":\"test-2\",\"name\":\"Keep\",\"value\":2}"
  let updated_record = TestRecord(id: "test-1", name: "New", value: 100)

  let updated =
    jsonl_storage.update_jsonl_content(
      existing,
      updated_record,
      test_record_encoder,
      test_record_id_extractor,
    )

  // Should not contain old version
  updated |> string.contains("\"name\":\"Old\"") |> should.be_false()
  // Should contain new version
  updated |> string.contains("\"name\":\"New\"") |> should.be_true()
  updated |> string.contains("\"value\":100") |> should.be_true()
  // Should keep other record
  updated |> string.contains("\"id\":\"test-2\"") |> should.be_true()
}

/// Test: find_by_id_with_extractor finds existing record (fixed from find_by_id)
pub fn test_find_by_id_found_test() {
  let records = [
    TestRecord(id: "test-1", name: "First", value: 1),
    TestRecord(id: "test-2", name: "Second", value: 2),
  ]

  let result =
    jsonl_storage.find_by_id_with_extractor(
      records,
      "test-2",
      test_record_id_extractor,
    )

  result |> should.be_ok()
  let record = result |> result.unwrap(TestRecord(id: "", name: "", value: 0))
  record.id |> should.equal("test-2")
  record.name |> should.equal("Second")
}

/// Test: find_by_id_with_extractor returns error for missing record (fixed from find_by_id)
pub fn test_find_by_id_not_found_test() {
  let records = [TestRecord(id: "test-1", name: "First", value: 1)]

  let result =
    jsonl_storage.find_by_id_with_extractor(
      records,
      "nonexistent",
      test_record_id_extractor,
    )

  result |> should.be_error()
}

// =============================================================================
// I/O Function Tests with Mocks
// =============================================================================

/// Test: append_to_jsonl_with_io creates new file
pub fn test_append_to_jsonl_with_io_new_file_test() {
  let record = TestRecord(id: "test-1", name: "New", value: 1)
  let reader = mock_reader("")
  let writer = mock_writer()
  let dir_creator = mock_dir_creator()

  let result =
    jsonl_storage.append_to_jsonl_with_io(
      record,
      "/test/path.jsonl",
      test_record_encoder,
      test_record_id_extractor,
      reader,
      writer,
      dir_creator,
    )

  result |> should.be_ok()
}

/// Test: append_to_jsonl_with_io appends to existing file
pub fn test_append_to_jsonl_with_io_existing_file_test() {
  let existing = "{\"id\":\"test-1\",\"name\":\"Existing\",\"value\":1}"
  let new_record = TestRecord(id: "test-2", name: "New", value: 2)
  let reader = mock_reader(existing)
  let writer = mock_writer()
  let dir_creator = mock_dir_creator()

  let result =
    jsonl_storage.append_to_jsonl_with_io(
      new_record,
      "/test/path.jsonl",
      test_record_encoder,
      test_record_id_extractor,
      reader,
      writer,
      dir_creator,
    )

  result |> should.be_ok()
}

/// Test: append_to_jsonl_with_io replaces record with same ID
pub fn test_append_to_jsonl_with_io_replace_existing_id_test() {
  let existing = "{\"id\":\"test-1\",\"name\":\"Old\",\"value\":1}"
  let updated = TestRecord(id: "test-1", name: "Updated", value: 100)
  let reader = mock_reader(existing)
  let writer = mock_writer()
  let dir_creator = mock_dir_creator()

  let result =
    jsonl_storage.append_to_jsonl_with_io(
      updated,
      "/test/path.jsonl",
      test_record_encoder,
      test_record_id_extractor,
      reader,
      writer,
      dir_creator,
    )

  result |> should.be_ok()
}

/// Test: list_from_jsonl_with_io handles empty file
pub fn test_list_from_jsonl_with_io_empty_test() {
  let reader = mock_reader("")

  let result =
    jsonl_storage.list_from_jsonl_with_io(
      "/test/path.jsonl",
      test_record_decoder,
      reader,
    )

  result |> should.be_ok()
  result |> result.unwrap([]) |> should.equal([])
}

/// Test: list_from_jsonl_with_io returns multiple records
pub fn test_list_from_jsonl_with_io_multiple_test() {
  let content =
    "{\"id\":\"test-1\",\"name\":\"First\",\"value\":1}\n{\"id\":\"test-2\",\"name\":\"Second\",\"value\":2}"
  let reader = mock_reader(content)

  let result =
    jsonl_storage.list_from_jsonl_with_io(
      "/test/path.jsonl",
      test_record_decoder,
      reader,
    )

  result |> should.be_ok()
  let records = result |> result.unwrap([])
  records |> list.length() |> should.equal(2)
}

/// Test: get_from_jsonl_with_io finds record
pub fn test_get_from_jsonl_with_io_found_test() {
  let content = "{\"id\":\"test-1\",\"name\":\"Found\",\"value\":42}"
  let reader = mock_reader(content)

  let result =
    jsonl_storage.get_from_jsonl_with_io(
      "/test/path.jsonl",
      "test-1",
      test_record_decoder,
      reader,
    )

  result |> should.be_ok()
  let record = result |> result.unwrap(TestRecord(id: "", name: "", value: 0))
  record.name |> should.equal("Found")
}

/// Test: get_from_jsonl_with_io returns error when not found
pub fn test_get_from_jsonl_with_io_not_found_test() {
  let content = "{\"id\":\"test-1\",\"name\":\"Found\",\"value\":42}"
  let reader = mock_reader(content)

  let result =
    jsonl_storage.get_from_jsonl_with_io(
      "/test/path.jsonl",
      "nonexistent",
      test_record_decoder,
      reader,
    )

  result |> should.be_error()
}

/// Test: ensure_parent_directory_with_io creates directory
pub fn test_ensure_parent_directory_creates_dirs_test() {
  let dir_creator = mock_dir_creator()

  let result =
    jsonl_storage.ensure_parent_directory_with_io(
      "/test/nested/path/file.jsonl",
      dir_creator,
    )

  result |> should.be_ok()
}
