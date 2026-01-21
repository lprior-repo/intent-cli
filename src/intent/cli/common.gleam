/// Common CLI utilities and types shared across all command modules
///
/// This module contains shared infrastructure for CLI commands:
/// - Exit codes with semantic meaning
/// - Common flag parsing helpers
/// - Output formatting utilities

import gleam/io
import gleam/result

/// Exit codes with semantic meaning - replaces magic numbers
pub type ExitCode {
  /// 0 - Command succeeded, tests passed
  ExitPass
  /// 1 - Command succeeded but tests/checks failed
  ExitFail
  /// 2 - Command blocked by dependencies
  ExitBlocked
  /// 3 - Invalid input (spec parse error, bad file, etc.)
  ExitInvalid
  /// 4 - Internal error (unexpected failure)
  ExitError
}

/// Convert ExitCode to integer for halt()
pub fn exit_code_to_int(code: ExitCode) -> Int {
  case code {
    ExitPass -> 0
    ExitFail -> 1
    ExitBlocked -> 2
    ExitInvalid -> 3
    ExitError -> 4
  }
}

/// FFI to halt with exit code - delegates to intent_ffi
@external(erlang, "intent_ffi", "halt")
pub fn halt(code: Int) -> Nil

/// Halt with a semantic exit code
pub fn exit(code: ExitCode) -> Nil {
  halt(exit_code_to_int(code))
}

/// Get string flag with empty default
pub fn get_string_flag(
  flags: a,
  name: String,
  getter: fn(a, String) -> Result(String, b),
) -> String {
  getter(flags, name)
  |> result.unwrap("")
}

/// Get bool flag with False default
pub fn get_bool_flag(
  flags: a,
  name: String,
  getter: fn(a, String) -> Result(Bool, b),
) -> Bool {
  getter(flags, name)
  |> result.unwrap(False)
}

/// Print usage error and exit
pub fn usage_error(command: String, usage: String) -> Nil {
  io.println_error("Error: spec file path required")
  io.println_error("Usage: intent " <> command <> " " <> usage)
  exit(ExitError)
}

/// Generate UUID using FFI
@external(erlang, "intent_ffi", "generate_uuid")
pub fn generate_uuid() -> String

/// Get current timestamp using FFI
@external(erlang, "intent_ffi", "current_timestamp")
pub fn current_timestamp() -> String
