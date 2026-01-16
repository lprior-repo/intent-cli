//// Test UUID v4 RFC 4122 compliance
////
//// Verifies that generated UUIDs have:
//// - Version 4 marker (character 14 is '4')
//// - Variant 1 marker (character 19 is '8', '9', 'a', or 'b')

import gleam/string
import gleeunit/should

@external(erlang, "intent_ffi", "generate_uuid")
fn generate_uuid() -> String

pub fn uuid_v4_version_bits_test() {
  // GIVEN/WHEN: Generate a UUID
  let uuid = generate_uuid()

  // THEN: UUID should be 36 characters (32 hex + 4 hyphens)
  string.length(uuid)
  |> should.equal(36)

  // THEN: Character at position 14 should be '4' (version 4)
  case string.slice(uuid, 14, 1) {
    "4" -> should.be_true(True)
    _other -> {
      should.fail()
    }
  }
}

pub fn uuid_v4_variant_bits_test() {
  // GIVEN/WHEN: Generate a UUID
  let uuid = generate_uuid()

  // THEN: Character at position 19 should be 8, 9, a/A, or b/B (variant 1)
  let variant_char = string.slice(uuid, 19, 1) |> string.lowercase()
  case variant_char {
    "8" -> should.be_true(True)
    "9" -> should.be_true(True)
    "a" -> should.be_true(True)
    "b" -> should.be_true(True)
    _other -> {
      should.fail()
    }
  }
}

pub fn uuid_format_test() {
  // GIVEN/WHEN: Generate a UUID
  let uuid = generate_uuid()

  // THEN: Should have hyphens at correct positions
  string.slice(uuid, 8, 1)
  |> should.equal("-")

  string.slice(uuid, 13, 1)
  |> should.equal("-")

  string.slice(uuid, 18, 1)
  |> should.equal("-")

  string.slice(uuid, 23, 1)
  |> should.equal("-")
}

pub fn uuid_uniqueness_test() {
  // GIVEN: Generate 100 UUIDs
  let uuids = [
    generate_uuid(), generate_uuid(), generate_uuid(), generate_uuid(),
    generate_uuid(), generate_uuid(), generate_uuid(), generate_uuid(),
    generate_uuid(), generate_uuid(),
  ]

  // WHEN: Check for duplicates
  let unique_count =
    uuids
    |> list.unique()
    |> list.length()

  // THEN: All should be unique
  unique_count
  |> should.equal(10)
}

import gleam/list
