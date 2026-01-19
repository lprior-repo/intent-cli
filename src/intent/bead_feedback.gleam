//// Track bead execution results via append-only CUE files.
////
//// Implements Option B architecture from beads.cue:
//// - Feedback appended to .intent/feedback-{session_id}.cue
//// - CUE unification automatically merges with session file
//// - No JSONL - pure CUE state for all operations
////
//// Architecture: Functional Core / Imperative Shell
//// - Pure functions: parse_feedback_json(), feedback_to_cue(), validation
//// - I/O functions: load_feedback_for_session(), mark_bead_*() - shell layer

import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import simplifile

/// Represents the result of bead execution
pub type BeadResult {
  Success
  Failed
  Blocked
  Skipped
}

/// Feedback for a single bead execution
pub type BeadFeedback {
  BeadFeedback(
    bead_id: String,
    result: BeadResult,
    reason: String,
    executed_at: String,
    duration_ms: Int,
    error: option.Option(BeadError),
    blocked_by: option.Option(BlockedReason),
  )
}

pub type BeadError {
  BeadError(error_type: String, message: String, trace: option.Option(String))
}

pub type BlockedReason {
  BlockedReason(blocker_type: String, details: String, unblocks_when: String)
}

pub type FeedbackError {
  SessionNotFound(session_id: String)
  WriteError(path: String, message: String)
  ValidationError(message: String)
}

// =============================================================================
// PURE: Feedback Creation (Functional Core)
// =============================================================================

/// Pure function: Create a BeadFeedback record for a successful execution.
/// This is the functional core - no I/O, no timestamp generation.
pub fn create_success_feedback(
  bead_id: String,
  reason: String,
  executed_at: String,
  duration_ms: Int,
) -> BeadFeedback {
  BeadFeedback(
    bead_id: bead_id,
    result: Success,
    reason: reason,
    executed_at: executed_at,
    duration_ms: duration_ms,
    error: option.None,
    blocked_by: option.None,
  )
}

/// Pure function: Create a BeadFeedback record for a failed execution.
/// This is the functional core - no I/O, no timestamp generation.
pub fn create_failed_feedback(
  bead_id: String,
  reason: String,
  executed_at: String,
  duration_ms: Int,
  error_type: String,
  error_message: String,
  trace: option.Option(String),
) -> BeadFeedback {
  BeadFeedback(
    bead_id: bead_id,
    result: Failed,
    reason: reason,
    executed_at: executed_at,
    duration_ms: duration_ms,
    error: option.Some(BeadError(
      error_type: error_type,
      message: error_message,
      trace: trace,
    )),
    blocked_by: option.None,
  )
}

/// Pure function: Create a BeadFeedback record for a blocked execution.
/// This is the functional core - no I/O, no timestamp generation.
pub fn create_blocked_feedback(
  bead_id: String,
  reason: String,
  executed_at: String,
  duration_ms: Int,
  blocker_type: String,
  blocker_details: String,
  unblocks_when: String,
) -> BeadFeedback {
  BeadFeedback(
    bead_id: bead_id,
    result: Blocked,
    reason: reason,
    executed_at: executed_at,
    duration_ms: duration_ms,
    error: option.None,
    blocked_by: option.Some(BlockedReason(
      blocker_type: blocker_type,
      details: blocker_details,
      unblocks_when: unblocks_when,
    )),
  )
}

/// Pure function: Create a BeadFeedback record for a skipped execution.
/// This is the functional core - no I/O, no timestamp generation.
pub fn create_skipped_feedback(
  bead_id: String,
  reason: String,
  executed_at: String,
) -> BeadFeedback {
  BeadFeedback(
    bead_id: bead_id,
    result: Skipped,
    reason: reason,
    executed_at: executed_at,
    duration_ms: 0,
    error: option.None,
    blocked_by: option.None,
  )
}

/// Pure function: Serialize a BeadFeedback record to CUE string.
/// This is the functional core - no I/O.
pub fn feedback_to_cue_string(feedback: BeadFeedback) -> String {
  let result_str = bead_result_to_string(feedback.result)
  let duration_str = string.inspect(feedback.duration_ms)

  let error_str = case feedback.error {
    option.None -> ""
    option.Some(err) -> {
      let trace_str = case err.trace {
        option.None -> ""
        option.Some(t) -> "\n\t\ttrace: \"\"\"" <> t <> "\"\"\""
      }
      "\n\terror: {\n\t\ttype: \""
      <> err.error_type
      <> "\"\n\t\tmessage: \""
      <> escape_cue_string(err.message)
      <> "\""
      <> trace_str
      <> "\n\t}"
    }
  }

  let blocked_str = case feedback.blocked_by {
    option.None -> ""
    option.Some(b) -> {
      "\n\tblocked_by: {\n\t\ttype: \""
      <> b.blocker_type
      <> "\"\n\t\tdetails: \""
      <> escape_cue_string(b.details)
      <> "\"\n\t\tunblocks_when: \""
      <> escape_cue_string(b.unblocks_when)
      <> "\"\n\t}"
    }
  }

  "{\n\tbead_id: \""
  <> feedback.bead_id
  <> "\"\n\tresult: \""
  <> result_str
  <> "\"\n\treason: \""
  <> escape_cue_string(feedback.reason)
  <> "\"\n\texecuted_at: \""
  <> feedback.executed_at
  <> "\"\n\tduration_ms: "
  <> duration_str
  <> error_str
  <> blocked_str
  <> "\n}\n"
}

/// Pure function: Validate a session ID format.
/// Returns Ok if valid, Error with message if invalid.
pub fn validate_session_id_pure(session_id: String) -> Result(Nil, String) {
  case validate_session_id(session_id) {
    True -> Ok(Nil)
    False -> Error("Invalid session ID format: " <> session_id)
  }
}

/// Pure function: Validate a bead ID format.
/// Returns Ok if valid, Error with message if invalid.
pub fn validate_bead_id_pure(bead_id: String) -> Result(Nil, String) {
  case validate_bead_id(bead_id) {
    True -> Ok(Nil)
    False -> Error("Invalid bead ID format: " <> bead_id)
  }
}

// =============================================================================
// I/O: Persistence Functions (Imperative Shell)
// =============================================================================

/// I/O function: Persist feedback to file.
/// This is the shell layer - handles file I/O.
pub fn persist_feedback(
  session_id: String,
  feedback: BeadFeedback,
) -> Result(Nil, FeedbackError) {
  case validate_session_id(session_id) {
    False -> Error(ValidationError("Invalid session ID format: " <> session_id))
    True -> {
      case validate_bead_id(feedback.bead_id) {
        False ->
          Error(ValidationError("Invalid bead ID format: " <> feedback.bead_id))
        True -> {
          let path = feedback_file_path(session_id)
          let cue_str = feedback_to_cue_string(feedback)
          append_to_file(path, cue_str)
        }
      }
    }
  }
}

// =============================================================================
// LEGACY: Mark functions (I/O - call pure functions internally)
// These are kept for backward compatibility but are deprecated.
// Prefer using create_*_feedback() + persist_feedback() for new code.
// =============================================================================

/// Mark a bead as executed with success status.
pub fn mark_bead_executed(
  session_id: String,
  bead_id: String,
  result: BeadResult,
  reason: String,
  duration_ms: Int,
) -> Result(Nil, FeedbackError) {
  mark_bead_with_details(
    session_id,
    bead_id,
    result,
    reason,
    duration_ms,
    option.None,
    option.None,
  )
}

/// Mark a bead as blocked with additional details.
pub fn mark_bead_blocked(
  session_id: String,
  bead_id: String,
  reason: String,
  blocker_type: String,
  blocker_details: String,
  unblocks_when: String,
  duration_ms: Int,
) -> Result(Nil, FeedbackError) {
  let blocked_reason =
    option.Some(BlockedReason(
      blocker_type: blocker_type,
      details: blocker_details,
      unblocks_when: unblocks_when,
    ))
  mark_bead_with_details(
    session_id,
    bead_id,
    Blocked,
    reason,
    duration_ms,
    option.None,
    blocked_reason,
  )
}

/// Mark a bead as failed with error details.
pub fn mark_bead_failed(
  session_id: String,
  bead_id: String,
  reason: String,
  error_type: String,
  error_message: String,
  trace: option.Option(String),
  duration_ms: Int,
) -> Result(Nil, FeedbackError) {
  let error =
    option.Some(BeadError(
      error_type: error_type,
      message: error_message,
      trace: trace,
    ))
  mark_bead_with_details(
    session_id,
    bead_id,
    Failed,
    reason,
    duration_ms,
    error,
    option.None,
  )
}

/// Internal: Mark bead with all details.
fn mark_bead_with_details(
  session_id: String,
  bead_id: String,
  result: BeadResult,
  reason: String,
  duration_ms: Int,
  error: option.Option(BeadError),
  blocked_by: option.Option(BlockedReason),
) -> Result(Nil, FeedbackError) {
  // Validate session ID format (alphanumeric + hyphen)
  case validate_session_id(session_id) {
    False -> Error(ValidationError("Invalid session ID format: " <> session_id))
    True -> {
      // Validate bead ID format
      case validate_bead_id(bead_id) {
        False -> Error(ValidationError("Invalid bead ID format: " <> bead_id))
        True -> {
          let feedback_path = ".intent/feedback-" <> session_id <> ".cue"
          let cue_entry =
            feedback_to_cue(
              bead_id,
              result,
              reason,
              duration_ms,
              error,
              blocked_by,
            )
          append_to_file(feedback_path, cue_entry)
        }
      }
    }
  }
}

/// Load all feedback for a session from CUE file.
/// NOTE: This is an I/O function - for FC/IS architecture, prefer using
/// parse_feedback_json() with content from shell layer.
/// 
/// @deprecated Use parse_feedback_json() with file content from shell layer.
pub fn load_feedback_for_session(
  session_id: String,
) -> Result(List(BeadFeedback), FeedbackError) {
  let feedback_path = ".intent/feedback-" <> session_id <> ".cue"
  case simplifile.read(feedback_path) {
    Ok(content) -> parse_feedback_content(content)
    Error(_) -> {
      // File doesn't exist yet (empty feedback)
      Ok([])
    }
  }
}

/// Pure function: Parse feedback from content string.
/// Supports both JSON (from `cue export`) and raw CUE format.
/// Prefer JSON format for reliable parsing.
pub fn parse_feedback_content(
  content: String,
) -> Result(List(BeadFeedback), FeedbackError) {
  // If content looks like JSON array, parse as JSON
  case string.starts_with(string.trim(content), "[") {
    True -> parse_feedback_json(content)
    False -> {
      // Raw CUE content - return empty for now
      // Callers should run `cue export` first for reliable parsing
      Ok([])
    }
  }
}

/// Get the feedback file path for a session.
/// Use this from shell layer when doing file I/O.
pub fn feedback_file_path(session_id: String) -> String {
  ".intent/feedback-" <> session_id <> ".cue"
}

// =============================================================================
// PRIVATE: CUE Serialization
// =============================================================================

fn feedback_to_cue(
  bead_id: String,
  result: BeadResult,
  reason: String,
  duration_ms: Int,
  error: option.Option(BeadError),
  blocked_by: option.Option(BlockedReason),
) -> String {
  let result_str = bead_result_to_string(result)
  let timestamp = current_iso8601_timestamp()
  let duration_str = string.inspect(duration_ms)

  let error_str = case error {
    option.None -> ""
    option.Some(err) -> {
      let trace_str = case err.trace {
        option.None -> ""
        option.Some(t) -> "\n\t\ttrace: \"\"\"" <> t <> "\"\"\""
      }
      "\n\terror: {\n\t\ttype: \""
      <> err.error_type
      <> "\"\n\t\tmessage: \""
      <> escape_cue_string(err.message)
      <> "\""
      <> trace_str
      <> "\n\t}"
    }
  }

  let blocked_str = case blocked_by {
    option.None -> ""
    option.Some(b) -> {
      "\n\tblocked_by: {\n\t\ttype: \""
      <> b.blocker_type
      <> "\"\n\t\tdetails: \""
      <> escape_cue_string(b.details)
      <> "\"\n\t\tunblocks_when: \""
      <> escape_cue_string(b.unblocks_when)
      <> "\"\n\t}"
    }
  }

  "{\n\tbead_id: \""
  <> bead_id
  <> "\"\n\tresult: \""
  <> result_str
  <> "\"\n\treason: \""
  <> escape_cue_string(reason)
  <> "\"\n\texecuted_at: \""
  <> timestamp
  <> "\"\n\tduration_ms: "
  <> duration_str
  <> error_str
  <> blocked_str
  <> "\n}\n"
}

fn bead_result_to_string(result: BeadResult) -> String {
  case result {
    Success -> "success"
    Failed -> "failed"
    Blocked -> "blocked"
    Skipped -> "skipped"
  }
}

fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

// =============================================================================
// PRIVATE: File Operations
// =============================================================================

fn append_to_file(path: String, content: String) -> Result(Nil, FeedbackError) {
  // Read existing content (if file exists)
  let existing = case simplifile.read(path) {
    Ok(text) -> text
    Error(_) -> ""
  }

  // Append new content
  let updated = existing <> content

  // Write back (atomic replacement)
  simplifile.write(path, updated)
  |> result.map_error(fn(err) {
    let err_msg = case err {
      simplifile.Enoent -> "File not found"
      simplifile.Eacces -> "Permission denied"
      simplifile.Enospc -> "No space left on device"
      simplifile.Eio -> "I/O error"
      _ -> "Unknown error"
    }
    WriteError(path, err_msg)
  })
}

// =============================================================================
// PRIVATE: Validation
// =============================================================================

fn validate_session_id(id: String) -> Bool {
  let trimmed = string.trim(id)
  case string.length(trimmed) {
    0 -> False
    _ -> {
      trimmed
      |> string.to_graphemes
      |> list.all(fn(char) {
        case char {
          "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
          "a"
          | "b"
          | "c"
          | "d"
          | "e"
          | "f"
          | "g"
          | "h"
          | "i"
          | "j"
          | "k"
          | "l"
          | "m"
          | "n"
          | "o"
          | "p"
          | "q"
          | "r"
          | "s"
          | "t"
          | "u"
          | "v"
          | "w"
          | "x"
          | "y"
          | "z" -> True
          "A"
          | "B"
          | "C"
          | "D"
          | "E"
          | "F"
          | "G"
          | "H"
          | "I"
          | "J"
          | "K"
          | "L"
          | "M"
          | "N"
          | "O"
          | "P"
          | "Q"
          | "R"
          | "S"
          | "T"
          | "U"
          | "V"
          | "W"
          | "X"
          | "Y"
          | "Z" -> True
          "-" -> True
          "_" -> True
          _ -> False
        }
      })
    }
  }
}

fn validate_bead_id(id: String) -> Bool {
  // Format: PREFIX-NNN (e.g., AUTH-001, API-042)
  let trimmed = string.trim(id)
  case string.length(trimmed) >= 5 {
    False -> False
    True -> {
      case string.contains(trimmed, "-") {
        False -> False
        True -> {
          case string.split_once(trimmed, "-") {
            Error(Nil) -> False
            Ok(#(prefix, suffix)) -> {
              let prefix_ok =
                string.length(prefix) > 0 && string.length(prefix) <= 10
              let suffix_ok =
                string.length(suffix) == 3 && is_numeric_string(suffix)
              prefix_ok && suffix_ok
            }
          }
        }
      }
    }
  }
}

// =============================================================================
// PRIVATE: Helpers
// =============================================================================

/// Check if a string contains only numeric digits.
fn is_numeric_string(s: String) -> Bool {
  s
  |> string.to_graphemes
  |> list.all(fn(char) {
    case char {
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
      _ -> False
    }
  })
}

// =============================================================================
// PURE: JSON Parsing (Functional Core)
// =============================================================================

/// Parse feedback from JSON content (output of `cue export`).
/// This is a pure function - no file I/O.
/// 
/// Expected format (from cue export):
/// ```json
/// [
///   {
///     "bead_id": "AUTH-001",
///     "result": "success",
///     "reason": "Passed all checks",
///     "executed_at": "2026-01-17T10:30:00Z",
///     "duration_ms": 123,
///     "error": { "type": "...", "message": "...", "trace": "..." },
///     "blocked_by": { "type": "...", "details": "...", "unblocks_when": "..." }
///   }
/// ]
/// ```
pub fn parse_feedback_json(
  json_content: String,
) -> Result(List(BeadFeedback), FeedbackError) {
  case json.decode(json_content, dynamic.list(decode_bead_feedback)) {
    Ok(feedback_list) -> Ok(feedback_list)
    Error(json_err) -> {
      let err_msg = case json_err {
        json.UnexpectedFormat(errs) ->
          "Unexpected JSON format: "
          <> string.join(list.map(errs, describe_decode_error), ", ")
        json.UnexpectedByte(_) -> "Unexpected byte in JSON"
        json.UnexpectedEndOfInput -> "Unexpected end of JSON input"
        json.UnexpectedSequence(_) -> "Unexpected sequence in JSON"
      }
      Error(ValidationError(err_msg))
    }
  }
}

/// Decoder for a single BeadFeedback record from JSON
fn decode_bead_feedback(
  dyn: Dynamic,
) -> Result(BeadFeedback, List(dynamic.DecodeError)) {
  // Required fields
  let bead_id_result = dynamic.field("bead_id", dynamic.string)(dyn)
  let result_result = dynamic.field("result", decode_bead_result)(dyn)
  let reason_result = dynamic.field("reason", dynamic.string)(dyn)
  let executed_at_result = dynamic.field("executed_at", dynamic.string)(dyn)
  let duration_ms_result = dynamic.field("duration_ms", dynamic.int)(dyn)

  // Optional fields
  let error_result = dynamic.optional_field("error", decode_bead_error)(dyn)
  let blocked_by_result =
    dynamic.optional_field("blocked_by", decode_blocked_reason)(dyn)

  // Combine results
  case
    bead_id_result,
    result_result,
    reason_result,
    executed_at_result,
    duration_ms_result
  {
    Ok(bead_id), Ok(bead_result), Ok(reason), Ok(executed_at), Ok(duration_ms) -> {
      let error_opt = result.unwrap(error_result, option.None)
      let blocked_opt = result.unwrap(blocked_by_result, option.None)
      Ok(BeadFeedback(
        bead_id: bead_id,
        result: bead_result,
        reason: reason,
        executed_at: executed_at,
        duration_ms: duration_ms,
        error: error_opt,
        blocked_by: blocked_opt,
      ))
    }
    _, _, _, _, _ -> {
      // Collect all errors
      let errors = []
      let errors = case bead_id_result {
        Error(e) -> list.append(errors, e)
        Ok(_) -> errors
      }
      let errors = case result_result {
        Error(e) -> list.append(errors, e)
        Ok(_) -> errors
      }
      let errors = case reason_result {
        Error(e) -> list.append(errors, e)
        Ok(_) -> errors
      }
      let errors = case executed_at_result {
        Error(e) -> list.append(errors, e)
        Ok(_) -> errors
      }
      let errors = case duration_ms_result {
        Error(e) -> list.append(errors, e)
        Ok(_) -> errors
      }
      Error(errors)
    }
  }
}

/// Decode the result field ("success", "failed", "blocked", "skipped")
fn decode_bead_result(
  dyn: Dynamic,
) -> Result(BeadResult, List(dynamic.DecodeError)) {
  case dynamic.string(dyn) {
    Ok("success") -> Ok(Success)
    Ok("failed") -> Ok(Failed)
    Ok("blocked") -> Ok(Blocked)
    Ok("skipped") -> Ok(Skipped)
    Ok(other) ->
      Error([
        dynamic.DecodeError(
          expected: "success|failed|blocked|skipped",
          found: other,
          path: ["result"],
        ),
      ])
    Error(e) -> Error(e)
  }
}

/// Decode the optional error field
fn decode_bead_error(
  dyn: Dynamic,
) -> Result(BeadError, List(dynamic.DecodeError)) {
  let error_type_result = dynamic.field("type", dynamic.string)(dyn)
  let message_result = dynamic.field("message", dynamic.string)(dyn)
  let trace_result = dynamic.optional_field("trace", dynamic.string)(dyn)

  case error_type_result, message_result {
    Ok(error_type), Ok(message) -> {
      let trace_opt = result.unwrap(trace_result, option.None)
      Ok(BeadError(error_type: error_type, message: message, trace: trace_opt))
    }
    _, _ -> {
      let errors = []
      let errors = case error_type_result {
        Error(e) -> list.append(errors, e)
        Ok(_) -> errors
      }
      let errors = case message_result {
        Error(e) -> list.append(errors, e)
        Ok(_) -> errors
      }
      Error(errors)
    }
  }
}

/// Decode the optional blocked_by field
fn decode_blocked_reason(
  dyn: Dynamic,
) -> Result(BlockedReason, List(dynamic.DecodeError)) {
  let blocker_type_result = dynamic.field("type", dynamic.string)(dyn)
  let details_result = dynamic.field("details", dynamic.string)(dyn)
  let unblocks_when_result = dynamic.field("unblocks_when", dynamic.string)(dyn)

  case blocker_type_result, details_result, unblocks_when_result {
    Ok(blocker_type), Ok(details), Ok(unblocks_when) -> {
      Ok(BlockedReason(
        blocker_type: blocker_type,
        details: details,
        unblocks_when: unblocks_when,
      ))
    }
    _, _, _ -> {
      let errors = []
      let errors = case blocker_type_result {
        Error(e) -> list.append(errors, e)
        Ok(_) -> errors
      }
      let errors = case details_result {
        Error(e) -> list.append(errors, e)
        Ok(_) -> errors
      }
      let errors = case unblocks_when_result {
        Error(e) -> list.append(errors, e)
        Ok(_) -> errors
      }
      Error(errors)
    }
  }
}

/// Convert DecodeError to a human-readable string
fn describe_decode_error(err: dynamic.DecodeError) -> String {
  "expected "
  <> err.expected
  <> " at "
  <> string.join(err.path, ".")
  <> ", found "
  <> err.found
}

// =============================================================================
// PRIVATE: Timestamp
// =============================================================================

@external(erlang, "intent_ffi", "current_timestamp")
fn current_iso8601_timestamp() -> String
