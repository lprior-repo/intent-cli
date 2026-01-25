/// FFI utilities for UUID generation and timestamps
///
/// This module provides a central interface to Erlang FFI functions
/// defined in intent_ffi.erl. All external FFI declarations should
/// be consolidated here to avoid duplication.
/// Generate a UUID v4 string
///
/// Returns a string in the format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
/// Each invocation returns a unique identifier.
@external(erlang, "intent_ffi", "generate_uuid")
pub fn generate_uuid() -> String

/// Get current timestamp in ISO 8601 format
///
/// Returns a timestamp string like: "2026-01-25T10:21:08.123Z"
@external(erlang, "intent_ffi", "current_timestamp")
pub fn current_timestamp() -> String

/// Get current timestamp in ISO 8601 format (alias)
///
/// This is an alias for current_timestamp() for backward compatibility.
/// Both functions return identical ISO 8601 formatted timestamps.
@external(erlang, "intent_ffi", "current_iso8601_timestamp")
pub fn current_iso8601_timestamp() -> String

/// Write text to stderr with a newline
///
/// Outputs the given text to standard error, appending a newline.
/// Used for progress messages that should not interfere with stdout JSON output.
@external(erlang, "intent_ffi", "write_stderr")
pub fn write_stderr(text: String) -> Nil
