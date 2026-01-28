import gleam/dynamic
import gleam/json
import gleam/string
import gleeunit/should
import intent/parser

pub fn test_sanitize_control_chars() {
  let input = "line1\nline2\rcarriage\ttab"

  // After sanitization, control chars should be escaped
  let json_str =
    json.object([
      #("value", json.string(input)),
    ])
    |> json.to_string

  // Should not contain literal control chars in JSON output
  json_str
  |> string.contains("\n")
  |> should.be_false

  json_str
  |> string.contains("\r")
  |> should.be_false

  json_str
  |> string.contains("\t")
  |> should.be_false

  // Should be valid JSON when parsed back
  let assert Ok(_) = json.decode(json_str, dynamic.dynamic)
}

pub fn test_sanitize_null_byte() {
  let input = "text\u{0000}more"

  let json_str =
    json.object([
      #("value", json.string(input)),
    ])
    |> json.to_string

  // Should be valid JSON
  let assert Ok(_) = json.decode(json_str, dynamic.dynamic)

  // Null byte should be escaped
  json_str
  |> string.contains("\\u{0000}")
  |> should.be_true
}

pub fn test_sanitize_json_special_chars() {
  let input = "quote\" and backslash\\"

  let json_str =
    json.object([
      #("value", json.string(input)),
    ])
    |> json.to_string

  // Should be valid JSON
  let assert Ok(_) = json.decode(json_str, dynamic.dynamic)

  // Special chars should be escaped
  json_str
  |> string.contains("\\\"")
  |> should.be_true

  json_str
  |> string.contains("\\\\")
  |> should.be_true
}

pub fn test_dynamic_to_json_escapes_control_chars() {
  let data = dynamic.from("text with\nnewline and\rcarriage return")

  let json_obj = parser.dynamic_to_json(data)
  let json_str = json.to_string(json_obj)

  // Should be valid JSON
  let assert Ok(_) = json.decode(json_str, dynamic.dynamic)

  // Control chars should be escaped
  json_str
  |> string.contains("\\u{000a}")
  // \n
  |> should.be_true

  json_str
  |> string.contains("\\u{000d}")
  // \r
  |> should.be_true
}
