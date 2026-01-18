import gleeunit
import gleeunit/should
import intent

pub fn main() {
  gleeunit.main()
}

// Test empty args
pub fn normalize_empty_args_test() {
  []
  |> intent.normalize_flag_syntax
  |> should.equal([])
}

// Test flag with equals (already normalized)
pub fn normalize_flag_with_equals_test() {
  ["--target=http://localhost"]
  |> intent.normalize_flag_syntax
  |> should.equal(["--target=http://localhost"])
}

// Test flag with space (needs normalization)
pub fn normalize_flag_with_space_test() {
  ["--target", "http://localhost"]
  |> intent.normalize_flag_syntax
  |> should.equal(["--target=http://localhost"])
}

// Test boolean flag alone
pub fn normalize_boolean_flag_alone_test() {
  ["--json"]
  |> intent.normalize_flag_syntax
  |> should.equal(["--json"])
}

// Test boolean flag with value
pub fn normalize_boolean_flag_with_value_test() {
  ["--json", "true"]
  |> intent.normalize_flag_syntax
  |> should.equal(["--json=true"])
}

// Test multiple flags mixed syntax
pub fn normalize_multiple_mixed_syntax_test() {
  ["--target", "http://localhost", "--json=true", "--verbose"]
  |> intent.normalize_flag_syntax
  |> should.equal(["--target=http://localhost", "--json=true", "--verbose"])
}

// Test positional args preserved
pub fn normalize_positional_args_preserved_test() {
  ["check", "spec.cue", "--target", "http://localhost"]
  |> intent.normalize_flag_syntax
  |> should.equal(["check", "spec.cue", "--target=http://localhost"])
}

// Test multiple positional args
pub fn normalize_multiple_positional_args_test() {
  ["interview", "--profile", "api", "--answer", "THE SYSTEM SHALL validate"]
  |> intent.normalize_flag_syntax
  |> should.equal([
    "interview", "--profile=api", "--answer=THE SYSTEM SHALL validate",
  ])
}

// Test flag followed by another flag (boolean)
pub fn normalize_flag_followed_by_flag_test() {
  ["--json", "--verbose"]
  |> intent.normalize_flag_syntax
  |> should.equal(["--json", "--verbose"])
}

// Test value with equals signs (URL query params)
pub fn normalize_value_with_equals_test() {
  ["--url", "http://api.com?x=1&y=2"]
  |> intent.normalize_flag_syntax
  |> should.equal(["--url=http://api.com?x=1&y=2"])
}

// Test complex real-world example
pub fn normalize_real_world_example_test() {
  [
    "check", "examples/pokemon-api.cue", "--target", "http://localhost:8080",
    "--json", "--verbose=false",
  ]
  |> intent.normalize_flag_syntax
  |> should.equal([
    "check", "examples/pokemon-api.cue", "--target=http://localhost:8080",
    "--json", "--verbose=false",
  ])
}

// Test flag at end of args
pub fn normalize_flag_at_end_test() {
  ["check", "spec.cue", "--json"]
  |> intent.normalize_flag_syntax
  |> should.equal(["check", "spec.cue", "--json"])
}

// Test value starting with dash (edge case)
pub fn normalize_value_with_dash_test() {
  ["--message", "Hello World"]
  |> intent.normalize_flag_syntax
  |> should.equal(["--message=Hello World"])
}
