//// Tests for light spec parsing and conversion

import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/option.{None}
import gleeunit
import gleeunit/should
import intent/parser
import intent/types

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// Light Spec Parsing Tests
// ============================================================================

pub fn parse_minimal_light_spec_test() {
  let json_str =
    "{
    \"name\": \"Test API\",
    \"description\": \"A minimal test spec\",
    \"behaviors\": [{
      \"name\": \"get-health\",
      \"intent\": \"Check service health\",
      \"request\": {
        \"method\": \"GET\",
        \"path\": \"/health\"
      },
      \"response\": {
        \"status\": 200
      }
    }]
  }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let assert Ok(spec) = parser.parse_light_spec(data)

  spec.name
  |> should.equal("Test API")

  spec.description
  |> should.equal("A minimal test spec")

  spec.behaviors
  |> list.length
  |> should.equal(1)

  spec.anti_patterns
  |> should.equal([])

  spec.ai_hints
  |> should.equal(None)
}

pub fn parse_light_spec_with_multiple_behaviors_test() {
  let json_str =
    "{
    \"name\": \"User API\",
    \"description\": \"User management API\",
    \"behaviors\": [
      {
        \"name\": \"list-users\",
        \"intent\": \"Get all users\",
        \"request\": {\"method\": \"GET\", \"path\": \"/users\"},
        \"response\": {\"status\": 200}
      },
      {
        \"name\": \"create-user\",
        \"intent\": \"Create a new user\",
        \"request\": {
          \"method\": \"POST\",
          \"path\": \"/users\",
          \"body\": {\"name\": \"Test\", \"email\": \"test@example.com\"}
        },
        \"response\": {
          \"status\": 201,
          \"checks\": {
            \"id\": {\"rule\": \"exists\", \"why\": \"User must have ID\"}
          }
        }
      }
    ]
  }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let assert Ok(spec) = parser.parse_light_spec(data)

  spec.behaviors
  |> list.length
  |> should.equal(2)

  // Check first behavior
  let assert [first, second] = spec.behaviors

  first.name
  |> should.equal("list-users")

  first.request.method
  |> should.equal(types.Get)

  // Check second behavior
  second.name
  |> should.equal("create-user")

  second.request.method
  |> should.equal(types.Post)

  second.response.status
  |> should.equal(201)

  // Check response has checks
  second.response.checks
  |> dict.size
  |> should.equal(1)
}

pub fn parse_light_spec_with_request_body_test() {
  let json_str =
    "{
    \"name\": \"Body Test\",
    \"description\": \"Test request bodies\",
    \"behaviors\": [{
      \"name\": \"create-order\",
      \"intent\": \"Submit an order\",
      \"request\": {
        \"method\": \"POST\",
        \"path\": \"/orders\",
        \"body\": {
          \"items\": [{\"id\": 1, \"qty\": 2}],
          \"total\": 99.99
        }
      },
      \"response\": {\"status\": 201}
    }]
  }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let assert Ok(spec) = parser.parse_light_spec(data)

  let assert [behavior] = spec.behaviors

  // Body should not be null
  behavior.request.body
  |> json.to_string
  |> should.not_equal("null")
}

pub fn parse_light_spec_all_http_methods_test() {
  let methods = ["GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS"]

  list.each(methods, fn(method) {
    let json_str = "{
      \"name\": \"Method Test\",
      \"description\": \"Test " <> method <> "\",
      \"behaviors\": [{
        \"name\": \"test-method\",
        \"intent\": \"Test the method\",
        \"request\": {\"method\": \"" <> method <> "\", \"path\": \"/test\"},
        \"response\": {\"status\": 200}
      }]
    }"

    let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
    let result = parser.parse_light_spec(data)

    result
    |> should.be_ok
  })
}

pub fn parse_light_spec_missing_name_fails_test() {
  let json_str =
    "{
    \"description\": \"No name\",
    \"behaviors\": [{
      \"name\": \"test\",
      \"intent\": \"Test\",
      \"request\": {\"method\": \"GET\", \"path\": \"/\"},
      \"response\": {\"status\": 200}
    }]
  }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parser.parse_light_spec(data)

  result
  |> should.be_error
}

pub fn parse_light_spec_missing_behaviors_fails_test() {
  let json_str =
    "{
    \"name\": \"Test\",
    \"description\": \"No behaviors\"
  }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parser.parse_light_spec(data)

  result
  |> should.be_error
}

pub fn parse_light_behavior_missing_intent_fails_test() {
  let json_str =
    "{
    \"name\": \"Test\",
    \"description\": \"Test\",
    \"behaviors\": [{
      \"name\": \"test\",
      \"request\": {\"method\": \"GET\", \"path\": \"/\"},
      \"response\": {\"status\": 200}
    }]
  }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let result = parser.parse_light_spec(data)

  result
  |> should.be_error
}

// ============================================================================
// Light Spec Detection Tests
// ============================================================================

pub fn is_light_spec_detects_light_spec_test() {
  let json_str =
    "{
    \"name\": \"Light Spec\",
    \"description\": \"Has behaviors at root\",
    \"behaviors\": []
  }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)

  parser.is_light_spec(data)
  |> should.be_true
}

pub fn is_light_spec_detects_full_spec_with_config_test() {
  let json_str =
    "{
    \"name\": \"Full Spec\",
    \"description\": \"Has config\",
    \"config\": {\"base_url\": \"http://localhost\"},
    \"behaviors\": []
  }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)

  parser.is_light_spec(data)
  |> should.be_false
}

pub fn is_light_spec_detects_full_spec_with_features_test() {
  let json_str =
    "{
    \"name\": \"Full Spec\",
    \"description\": \"Has features\",
    \"features\": [],
    \"behaviors\": []
  }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)

  parser.is_light_spec(data)
  |> should.be_false
}

// ============================================================================
// Response Checks Tests
// ============================================================================

pub fn parse_light_response_with_checks_test() {
  let json_str =
    "{
    \"name\": \"Checks Test\",
    \"description\": \"Test response checks\",
    \"behaviors\": [{
      \"name\": \"validated-response\",
      \"intent\": \"Verify response structure\",
      \"request\": {\"method\": \"GET\", \"path\": \"/users/1\"},
      \"response\": {
        \"status\": 200,
        \"checks\": {
          \"id\": {\"rule\": \"exists\", \"why\": \"Must have ID\"},
          \"name\": {\"rule\": \"is_string\", \"why\": \"Name must be string\"},
          \"age\": {\"rule\": \">= 0\", \"why\": \"Age cannot be negative\"}
        }
      }
    }]
  }"

  let assert Ok(data) = json.decode(json_str, dynamic.dynamic)
  let assert Ok(spec) = parser.parse_light_spec(data)
  let assert [behavior] = spec.behaviors

  behavior.response.checks
  |> dict.size
  |> should.equal(3)

  let assert Ok(id_check) = dict.get(behavior.response.checks, "id")
  id_check.rule
  |> should.equal("exists")
  id_check.why
  |> should.equal("Must have ID")
}
