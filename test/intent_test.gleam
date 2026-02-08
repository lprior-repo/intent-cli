// Test suite for intent CLI argument handling
import gleeunit
import gleeunit/should
import intent
import gleam/list
import gleam/string

pub fn main() -> Nil {
  gleeunit.main()
}

// =============================================================================
// CLI ARGUMENT NORMALIZATION TESTS
// =============================================================================

pub fn normalize_cli_args_bool_flag_test() {
  let args = ["--json", "--verbose"]
  let result = intent.normalize_cli_args(args)

  list.length(result)
  |> should.equal(2)
}

pub fn normalize_cli_args_bool_flag_with_value_test() {
  let args = ["--json", "false"]
  let result = intent.normalize_cli_args(args)

  case list.first(result) {
    Ok(first) -> {
      first
      |> should.equal("--json=false")
    }
    Error(_) -> should.fail()
  }
}

pub fn normalize_cli_args_bool_flag_uppercase_test() {
  let args = ["--json", "TRUE"]
  let result = intent.normalize_cli_args(args)

  case list.first(result) {
    Ok(first) -> {
      first
      |> should.equal("--json=true")
    }
    Error(_) -> should.fail()
  }
}

pub fn normalize_cli_args_value_flag_test() {
  let args = ["--profile", "api"]
  let result = intent.normalize_cli_args(args)

  case list.first(result) {
    Ok(first) -> {
      first
      |> should.equal("--profile=api")
    }
    Error(_) -> should.fail()
  }
}

pub fn normalize_cli_args_mixed_flags_test() {
  let args = ["--json", "--profile", "api", "--verbose"]
  let result = intent.normalize_cli_args(args)

  list.length(result)
  |> should.equal(3)

  result
  |> list.all(fn(arg) { string.contains(arg, "=") })
  |> should.equal(True)
}

pub fn normalize_cli_args_unknown_flag_test() {
  let args = ["--unknown-flag", "value"]
  let result = intent.normalize_cli_args(args)

  list.length(result)
  |> should.equal(2)

  case list.first(result) {
    Ok(first) -> {
      first
      |> should.equal("--unknown-flag")
    }
    Error(_) -> should.fail()
  }
}

pub fn normalize_cli_args_empty_list_test() {
  let args: List(String) = []
  let result = intent.normalize_cli_args(args)

  list.length(result)
  |> should.equal(0)
}

pub fn normalize_cli_args_single_bool_flag_test() {
  let args = ["--json"]
  let result = intent.normalize_cli_args(args)

  case list.first(result) {
    Ok(first) -> {
      first
      |> should.equal("--json=true")
    }
    Error(_) -> should.fail()
  }
}

pub fn normalize_cli_args_multiple_bool_flags_test() {
  let args = ["--json", "--quiet", "--yes", "--draft"]
  let result = intent.normalize_cli_args(args)

  list.length(result)
  |> should.equal(4)

  result
  |> list.all(fn(arg) {
    string.ends_with(arg, "=true")
  })
  |> should.equal(True)
}

pub fn normalize_cli_args_already_has_equals_test() {
  let args = ["--profile=default"]
  let result = intent.normalize_cli_args(args)

  case list.first(result) {
    Ok(first) -> {
      first
      |> should.equal("--profile=default")
    }
    Error(_) -> should.fail()
  }
}
