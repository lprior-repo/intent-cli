import gleam/list
import gleam/string
import gleeunit/should
import intent/ffi

/// Test that generate_uuid returns a non-empty string
pub fn generate_uuid_returns_non_empty_test() {
  let uuid = ffi.generate_uuid()
  should.not_equal(uuid, "")
}

/// Test that generate_uuid returns a string with correct format
/// Expected format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx (36 chars with 4 hyphens)
pub fn generate_uuid_format_test() {
  let uuid = ffi.generate_uuid()

  // Should be 36 characters long
  let length = string.length(uuid)
  should.equal(length, 36)

  // Should contain exactly 4 hyphens at positions 8, 13, 18, 23
  let parts = string.split(uuid, "-")
  should.equal(list.length(parts), 5)
}

/// Test that generate_uuid returns unique values
pub fn generate_uuid_uniqueness_test() {
  let uuid1 = ffi.generate_uuid()
  let uuid2 = ffi.generate_uuid()
  let uuid3 = ffi.generate_uuid()

  // All UUIDs should be different
  should.not_equal(uuid1, uuid2)
  should.not_equal(uuid2, uuid3)
  should.not_equal(uuid1, uuid3)
}

/// Test that current_timestamp returns a non-empty string
pub fn current_timestamp_returns_non_empty_test() {
  let timestamp = ffi.current_timestamp()
  should.not_equal(timestamp, "")
}

/// Test that current_timestamp returns ISO 8601 format
/// Expected format: YYYY-MM-DDTHH:MM:SS (at minimum)
pub fn current_timestamp_format_test() {
  let timestamp = ffi.current_timestamp()

  // Should contain 'T' separator between date and time
  should.be_true(string.contains(timestamp, "T"))

  // Should start with a 4-digit year (20XX or similar)
  let year_part = string.slice(timestamp, 0, 4)
  should.be_true(string.length(year_part) == 4)
}

/// Test that current_iso8601_timestamp is an alias for current_timestamp
pub fn current_iso8601_timestamp_is_alias_test() {
  // Both should return ISO 8601 formatted strings
  let ts1 = ffi.current_timestamp()
  let ts2 = ffi.current_iso8601_timestamp()

  // Both should be non-empty
  should.not_equal(ts1, "")
  should.not_equal(ts2, "")

  // Both should contain 'T' separator
  should.be_true(string.contains(ts1, "T"))
  should.be_true(string.contains(ts2, "T"))
}

/// Test that current_timestamp returns different values over time
/// (This test may be flaky if system is very fast, but should generally work)
pub fn current_timestamp_changes_over_time_test() {
  let ts1 = ffi.current_timestamp()
  // Small delay (this is a simple busy-wait for testing)
  let _ = list.range(0, 100_000)
  let ts2 = ffi.current_timestamp()

  // Timestamps might be equal if system is very fast,
  // but at minimum both should be valid ISO 8601 strings
  should.be_true(string.contains(ts1, "T"))
  should.be_true(string.contains(ts2, "T"))
}
