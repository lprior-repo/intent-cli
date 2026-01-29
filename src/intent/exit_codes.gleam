/// Exit Codes and Error Handling
///
/// Defines semantic exit codes and error handling helpers for CLI commands:
/// - exit_pass (0): Success
/// - exit_fail (1): General failure (tests failed, warnings)
/// - exit_invalid (3): Invalid input (file not found, parse errors)
/// - exit_error (4): Usage error (missing args, invalid flags)
///
/// Provides structured error handlers that output JSON and halt with
/// appropriate exit codes for machine-readable error handling.
import gleam/json
import gleam/option.{None, Some}
import intent/json_output
import intent/loader

@external(erlang, "intent_ffi", "halt")
pub fn halt(code: Int) -> Nil

/// Exit code for successful operation
pub const exit_pass = 0

/// Exit code for general failure (tests failed, warnings found)
pub const exit_fail = 1

/// Exit code for invalid input (file not found, parse errors)
pub const exit_invalid = 3

/// Exit code for usage errors (missing args, invalid flags)
pub const exit_error = 4

/// Handle file-not-found errors → exits 3 (invalid input)
pub fn handle_file_not_found(
  file_path file_path: String,
  command_name command_name: String,
) -> Nil {
  let error =
    json_output.error("file_not_found", "File not found: " <> file_path)

  let response =
    json_output.failure(
      command_name <> "_failed",
      command_name,
      json.object([]),
      [error],
      Some(file_path),
      [],
      exit_invalid,
    )

  json_output.output(response)
  halt(exit_invalid)
}

/// Handle loader errors → exits 3 or 4 based on error type
pub fn handle_load_error(
  error error: loader.LoadError,
  file_path file_path: String,
  command_name command_name: String,
) -> Nil {
  let exit_code = case error {
    loader.FileNotFound(_) -> exit_invalid
    loader.CueValidationFailed(_, _, _) -> exit_invalid
    loader.CueExportFailed(_, _, _) -> exit_invalid
    loader.JsonDecodeFailed(_) -> exit_invalid
    loader.SpecParseFailed(_) -> exit_invalid
    loader.SecurityError(_) -> exit_invalid
  }

  let error_msg = loader.format_error(error)
  let error = json_output.error("load_error", error_msg)

  let response =
    json_output.failure(
      command_name <> "_failed",
      command_name,
      json.object([]),
      [error],
      Some(file_path),
      [],
      exit_code,
    )

  json_output.output(response)
  halt(exit_code)
}

/// Handle missing arguments → exits 4 (usage error)
pub fn handle_missing_args(
  command_name command_name: String,
  usage usage: String,
) -> Nil {
  let error =
    json_output.error("missing_arguments", "File path or arguments required")

  let response =
    json_output.failure(
      command_name <> "_failed",
      command_name,
      json.object([#("usage", json.string(usage))]),
      [error],
      None,
      [],
      exit_error,
    )

  json_output.output(response)
  halt(exit_error)
}

/// Handle missing required flags → exits 4 (usage error)
pub fn handle_missing_flag(
  flag_name flag_name: String,
  command_name command_name: String,
  usage usage: String,
) -> Nil {
  let error = json_output.error("missing_flag", "Flag required: " <> flag_name)

  let response =
    json_output.failure(
      command_name <> "_failed",
      command_name,
      json.object([#("usage", json.string(usage))]),
      [error],
      None,
      [],
      exit_error,
    )

  json_output.output(response)
  halt(exit_error)
}

/// Handle invalid input → exits 3 (invalid input)
pub fn handle_invalid_input(
  message message: String,
  command_name command_name: String,
) -> Nil {
  let error = json_output.error("invalid_input", message)

  let response =
    json_output.failure(
      command_name <> "_failed",
      command_name,
      json.object([]),
      [error],
      None,
      [],
      exit_invalid,
    )

  json_output.output(response)
  halt(exit_invalid)
}

/// Handle usage errors → exits 4 (usage error)
pub fn handle_usage_error(
  message message: String,
  command_name command_name: String,
  usage usage: String,
) -> Nil {
  let error = json_output.error("usage_error", message)

  let response =
    json_output.failure(
      command_name <> "_failed",
      command_name,
      json.object([#("usage", json.string(usage))]),
      [error],
      None,
      [],
      exit_error,
    )

  json_output.output(response)
  halt(exit_error)
}
