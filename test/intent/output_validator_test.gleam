import gleam/string
import gleeunit
import gleeunit/should
import intent/output_validator.{
  CueCommandFailed, InvalidJson, InvalidSchema, SchemaValidationFailed,
  TempFileError,
}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Tests for parse_validation_result (Pure Function)
// ============================================================================

pub fn parse_validation_result_success_test() {
  let result = Ok("validation successful")
  output_validator.parse_validation_result(result)
  |> should.be_ok
  |> should.equal(Nil)
}

pub fn parse_validation_result_failure_test() {
  let result = Error(#(1, "response.name: conflicting values string and int"))
  output_validator.parse_validation_result(result)
  |> should.be_error
  |> fn(err) {
    case err {
      SchemaValidationFailed(msg) -> {
        msg
        |> should.equal("response.name: conflicting values string and int")
      }
      _ -> panic as "Expected SchemaValidationFailed"
    }
  }
}

pub fn parse_validation_result_cue_syntax_error_test() {
  let result = Error(#(1, "invalid CUE syntax"))
  output_validator.parse_validation_result(result)
  |> should.be_error
  |> fn(err) {
    case err {
      SchemaValidationFailed(msg) -> msg |> should.equal("invalid CUE syntax")
      _ -> panic as "Expected SchemaValidationFailed"
    }
  }
}

// ============================================================================
// Tests for format_error (Pure Function)
// ============================================================================

pub fn format_error_schema_validation_failed_test() {
  let error = SchemaValidationFailed("response.age: invalid value -5")
  output_validator.format_error(error)
  |> should.equal("Schema validation failed:\n  response.age: invalid value -5")
}

pub fn format_error_invalid_schema_test() {
  let error = InvalidSchema("malformed CUE syntax")
  output_validator.format_error(error)
  |> should.equal("Invalid schema: malformed CUE syntax")
}

pub fn format_error_invalid_json_test() {
  let error = InvalidJson("unexpected end of input")
  output_validator.format_error(error)
  |> should.equal("Invalid JSON: unexpected end of input")
}

pub fn format_error_cue_command_failed_test() {
  let error = CueCommandFailed(127, "cue: command not found")
  output_validator.format_error(error)
  |> should.equal(
    "CUE command failed (exit code 127):\n  cue: command not found",
  )
}

pub fn format_error_temp_file_error_test() {
  let error = TempFileError("permission denied")
  output_validator.format_error(error)
  |> should.equal("Temporary file error: permission denied")
}

// ============================================================================
// Tests for validate_with_executor (Shell Function with DI)
// ============================================================================

pub fn validate_with_executor_success_test() {
  let schema = "response: {name: string, age: int}"
  let data = "{\"name\": \"Alice\", \"age\": 30}"

  // Mock executor that always succeeds
  let mock_executor = fn(_cmd, _args, _dir) { Ok("") }

  output_validator.validate_with_executor(schema, data, mock_executor)
  |> should.be_ok
  |> should.equal(Nil)
}

pub fn validate_with_executor_validation_fails_test() {
  let schema = "response: {name: string, age: int}"
  let data = "{\"name\": \"Alice\", \"age\": \"thirty\"}"

  // Mock executor that returns validation error
  let mock_executor = fn(_cmd, _args, _dir) {
    Error(#(1, "response.age: conflicting values int and \"thirty\""))
  }

  output_validator.validate_with_executor(schema, data, mock_executor)
  |> should.be_error
  |> fn(err) {
    case err {
      SchemaValidationFailed(msg) -> {
        string.contains(msg, "response.age") |> should.be_true
        string.contains(msg, "conflicting values") |> should.be_true
      }
      _ -> panic as "Expected SchemaValidationFailed"
    }
  }
}

pub fn validate_with_executor_cue_command_not_found_test() {
  let schema = "response: {name: string}"
  let data = "{\"name\": \"test\"}"

  // Mock executor simulating command not found
  let mock_executor = fn(_cmd, _args, _dir) {
    Error(#(127, "cue: command not found"))
  }

  output_validator.validate_with_executor(schema, data, mock_executor)
  |> should.be_error
  |> fn(err) {
    case err {
      SchemaValidationFailed(msg) -> string.contains(msg, "command not found") |> should.be_true
      _ -> panic as "Expected SchemaValidationFailed"
    }
  }
}

// ============================================================================
// Integration Tests for validate_against_schema (Public API)
// ============================================================================

pub fn validate_against_schema_valid_simple_test() {
  let schema = "response: {name: string, age: int}"
  let data = "{\"name\": \"Bob\", \"age\": 25}"

  output_validator.validate_against_schema(schema, data)
  |> should.be_ok
  |> should.equal(Nil)
}

pub fn validate_against_schema_invalid_type_test() {
  let schema = "response: {name: string, age: int}"
  let data = "{\"name\": \"Bob\", \"age\": \"twenty-five\"}"

  output_validator.validate_against_schema(schema, data)
  |> should.be_error
  |> fn(err) {
    case err {
      SchemaValidationFailed(msg) -> string.contains(msg, "age") |> should.be_true
      _ -> panic as "Expected SchemaValidationFailed"
    }
  }
}

pub fn validate_against_schema_complex_nested_test() {
  let schema =
    "response: {
  user: {
    name: string
    email: string
    age: int & >0 & <120
  }
  posts: [...{
    title: string
    content: string
  }]
}"

  let data =
    "{
  \"user\": {
    \"name\": \"Alice\",
    \"email\": \"alice@example.com\",
    \"age\": 30
  },
  \"posts\": [
    {\"title\": \"Hello\", \"content\": \"World\"},
    {\"title\": \"Test\", \"content\": \"Post\"}
  ]
}"

  output_validator.validate_against_schema(schema, data)
  |> should.be_ok
  |> should.equal(Nil)
}

pub fn validate_against_schema_constraint_violation_test() {
  let schema = "response: {age: int & >0 & <120}"
  let data = "{\"age\": -5}"

  output_validator.validate_against_schema(schema, data)
  |> should.be_error
  |> fn(err) {
    case err {
      SchemaValidationFailed(_) -> Nil
      _ -> panic as "Expected SchemaValidationFailed"
    }
  }
}

pub fn validate_against_schema_empty_schema_test() {
  let schema = ""
  let data = "{\"name\": \"test\"}"

  output_validator.validate_against_schema(schema, data)
  |> should.be_error
  |> fn(err) {
    case err {
      SchemaValidationFailed(_) | InvalidSchema(_) -> Nil
      _ -> panic as "Expected SchemaValidationFailed or InvalidSchema"
    }
  }
}

pub fn validate_against_schema_empty_data_test() {
  let schema = "response: {name: string}"
  let data = ""

  output_validator.validate_against_schema(schema, data)
  |> should.be_error
  |> fn(err) {
    case err {
      SchemaValidationFailed(_) | InvalidJson(_) -> Nil
      _ -> panic as "Expected SchemaValidationFailed or InvalidJson"
    }
  }
}
