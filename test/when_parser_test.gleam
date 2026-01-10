//// Tests for parsing optional When clause fields (intent-cli-5zd)
//// The When clause should support partial conditions (status only, path only, etc)

import gleam/dynamic
import gleam/json
import gleam/option.{None, Some}
import gleeunit/should
import intent/parser
import intent/types.{Get, Post, When}

// Test: Parse when clause with only status field
pub fn parse_when_status_only_test() {
  let json_str = "{\"status\": \">= 400\"}"
  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)

  // This should parse successfully after the fix
  let result = parser.parse_when_for_test(data)

  case result {
    Ok(when) -> {
      // Status should be present
      should.equal(when.status, Some(">= 400"))
      // Method and path should be None (wildcards)
      should.equal(when.method, None)
      should.equal(when.path, None)
    }
    Error(_) -> should.fail()
  }
}

// Test: Parse when clause with only path field
pub fn parse_when_path_only_test() {
  let json_str = "{\"path\": \"/api/.*\"}"
  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)

  let result = parser.parse_when_for_test(data)

  case result {
    Ok(when) -> {
      should.equal(when.status, None)
      should.equal(when.method, None)
      should.equal(when.path, Some("/api/.*"))
    }
    Error(_) -> should.fail()
  }
}

// Test: Parse when clause with only method field
pub fn parse_when_method_only_test() {
  let json_str = "{\"method\": \"POST\"}"
  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)

  let result = parser.parse_when_for_test(data)

  case result {
    Ok(when) -> {
      should.equal(when.status, None)
      should.equal(when.method, Some(Post))
      should.equal(when.path, None)
    }
    Error(_) -> should.fail()
  }
}

// Test: Parse when clause with all fields (existing behavior)
pub fn parse_when_all_fields_test() {
  let json_str = "{\"status\": \"200\", \"method\": \"GET\", \"path\": \"/users\"}"
  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)

  let result = parser.parse_when_for_test(data)

  case result {
    Ok(when) -> {
      should.equal(when.status, Some("200"))
      should.equal(when.method, Some(Get))
      should.equal(when.path, Some("/users"))
    }
    Error(_) -> should.fail()
  }
}

// Test: Parse empty when clause (all wildcards)
pub fn parse_when_empty_test() {
  let json_str = "{}"
  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)

  let result = parser.parse_when_for_test(data)

  case result {
    Ok(when) -> {
      should.equal(when.status, None)
      should.equal(when.method, None)
      should.equal(when.path, None)
    }
    Error(_) -> should.fail()
  }
}

// Test: Parse when clause with two fields
pub fn parse_when_status_and_method_test() {
  let json_str = "{\"status\": \">= 400\", \"method\": \"POST\"}"
  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)

  let result = parser.parse_when_for_test(data)

  case result {
    Ok(when) -> {
      should.equal(when.status, Some(">= 400"))
      should.equal(when.method, Some(Post))
      should.equal(when.path, None)
    }
    Error(_) -> should.fail()
  }
}
