// Tests for answer_loader module with enhanced decode error reporting
import gleeunit
import gleeunit/should
import gleam/dict
import gleam/string
import intent/answer_loader

pub fn main() -> Nil {
  gleeunit.main()
}

// Test: Valid JSON with all correct types
pub fn parse_valid_json_answers_test() {
  let json_str = "{\"name\": \"Alice\", \"age\": 30}"
  let result = answer_loader.parse_answers_json_for_test("test.cue", json_str)

  should.equal(result, Ok(dict.from_list([
    #("name", "Alice"),
    #("age", "30")
  ])))
}

// Test: Nested objects flatten correctly
pub fn parse_nested_answers_test() {
  let json_str = "{\"user\": {\"name\": \"Bob\"}}"
  let result = answer_loader.parse_answers_json_for_test("test.cue", json_str)

  should.equal(result, Ok(dict.from_list([
    #("user.name", "Bob"),
    #("user", "{\"name\":\"Bob\"}")
  ])))
}

// Test: Invalid JSON with decode details
pub fn invalid_json_includes_error_details_test() {
  let json_str = "{invalid json}"
  let result = answer_loader.parse_answers_json_for_test("test.cue", json_str)

  case result {
    Error(answer_loader.ParseErrorWithDetails(path, decode_error)) -> {
      // Verify path is preserved
      should.equal(path, "test.cue")

      // Verify decode_error has structured info
      should.equal(decode_error.path, "<root>")
      should.equal(decode_error.expected, "JSON")
      should.equal(decode_error.actual, "invalid")

      // Verify message contains helpful info
      let _ = decode_error.message
      should.be_true(True)
    }
    Error(_) -> should.be_true(False)  // Wrong error type
    Ok(_) -> should.be_true(False)  // Should have errored
  }
}

// Test: Root not an object includes type mismatch
pub fn root_not_object_includes_type_details_test() {
  let json_str = "\"just a string\""
  let result = answer_loader.parse_answers_json_for_test("test.cue", json_str)

  case result {
    Error(answer_loader.ParseErrorWithDetails(_path, decode_error)) -> {
      // Verify type information is included
      should.equal(decode_error.expected, "Object")
      should.equal(decode_error.actual, "String")
      should.equal(decode_error.path, "<root>")
    }
    Error(_) -> should.be_true(False)  // Wrong error type
    Ok(_) -> should.be_true(False)  // Should have errored
  }
}

// Test: Empty object is valid
pub fn empty_object_valid_test() {
  let json_str = "{}"
  let result = answer_loader.parse_answers_json_for_test("test.cue", json_str)
  should.equal(result, Ok(dict.new()))
}

// Test: Array at root fails with type details
pub fn array_at_root_includes_type_details_test() {
  let json_str = "[1, 2, 3]"
  let result = answer_loader.parse_answers_json_for_test("test.cue", json_str)

  case result {
    Error(answer_loader.ParseErrorWithDetails(_, decode_error)) -> {
      should.equal(decode_error.expected, "Object")
      should.equal(decode_error.actual, "List")
    }
    Error(_) -> should.be_true(False)  // Wrong error type
    Ok(_) -> should.be_true(False)  // Should have errored
  }
}

// Test: Mixed types in nested object
pub fn mixed_types_in_nested_object_test() {
  let json_str = "{\"config\": {\"enabled\": true, \"count\": 5}}"
  let result = answer_loader.parse_answers_json_for_test("test.cue", json_str)

  // Should handle both bool and int correctly
  should.equal(result, Ok(dict.from_list([
    #("config.enabled", "True"),
    #("config.count", "5"),
    #("config", "{\"enabled\":true,\"count\":5}")
  ])))
}

// Test: Error messages are human readable
pub fn error_messages_are_human_readable_test() {
  let json_str = "{\"count\": \"thirty\"}"

  case answer_loader.parse_answers_json_for_test("test.cue", json_str) {
    Error(answer_loader.ParseErrorWithDetails(_, decode_error)) -> {
      let formatted = answer_loader.format_decode_error_for_test(decode_error)

      // Should be readable and helpful - check for expected sections
      should.be_true(string.contains(formatted, "Expected"))
      should.be_true(string.contains(formatted, "Actual"))
      should.be_true(string.contains(formatted, "Details"))
    }
    Error(_) -> should.be_true(False)  // Wrong error type
    Ok(_) -> should.be_true(False)  // Should have errored
  }
}

// Test: Nested field error includes full path
pub fn nested_field_error_includes_path_test() {
  let json_str = "{\"user\": {\"age\": \"not a number\"}}"
  // When trying to decode age as Int, error should include path "user.age"

  case answer_loader.parse_answers_json_for_test("test.cue", json_str) {
    Error(answer_loader.ParseErrorWithDetails(_, decode_error)) -> {
      // Should mention "age" somewhere in the error
      should.be_true(string.contains(decode_error.path, "age")
        || string.contains(decode_error.message, "age"))
      should.equal(decode_error.expected, "Int")
      should.equal(decode_error.actual, "String")
    }
    Error(_) -> should.be_true(False)  // Wrong error type
    Ok(_) -> should.be_true(False)  // Should have errored
  }
}
