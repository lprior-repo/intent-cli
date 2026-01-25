/// Progress output to stderr for AI-friendly pipelines
///
/// This module provides utilities for writing progress information to stderr
/// while keeping stdout clean for JSON output. Progress messages are formatted
/// as JSON for easy parsing by AI tools.
import gleam/json
import intent/ffi

/// Format a progress message as JSON
///
/// Returns a JSON string with timestamp, action, and message fields
pub fn format_progress(action: String, message: String) -> String {
  let timestamp = ffi.current_timestamp()

  json.object([
    #("timestamp", json.string(timestamp)),
    #("action", json.string(action)),
    #("message", json.string(message)),
  ])
  |> json.to_string
}

/// Write progress to stderr if enabled
///
/// When progress_enabled is True, writes a formatted JSON message to stderr.
/// When False, does nothing (silent mode).
pub fn write_progress(
  progress_enabled: Bool,
  action: String,
  message: String,
) -> Result(Nil, Nil) {
  case progress_enabled {
    True -> {
      let progress = format_progress(action, message)
      ffi.write_stderr(progress)
      Ok(Nil)
    }
    False -> Ok(Nil)
  }
}
