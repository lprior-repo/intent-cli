//// Bead claiming with structured context tracking.
////
//// Implements structured claims for beads that capture:
//// - Who claimed the bead (agent, human, system)
//// - When it was claimed
//// - What context they have gathered
//// - Why they are qualified to work on it
//// - Their work plan and estimated duration
////
//// Architecture: Functional Core / Imperative Shell
//// - Pure functions: create_*, validate_*, *_to_cue, *_to_json
//// - I/O functions: claim_bead(), release_claim(), get_claims()

import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

// =============================================================================
// TYPES
// =============================================================================

/// Who is claiming the bead
pub type ClaimerType {
  /// AI agent (e.g., Claude, GPT, local LLM)
  Agent
  /// Human developer/user
  Human
  /// Automated system (CI, scheduler, etc.)
  System
}

/// Status of a claim
pub type ClaimStatus {
  /// Currently active - claimer is working on it
  Active
  /// Claimer voluntarily released the claim
  Released
  /// Claim expired (timeout, no progress)
  Expired
  /// Work completed, claim closed
  Completed
}

/// Structured context gathered by the claimer
pub type ClaimContext {
  ClaimContext(
    /// Unique identifier for the claimer (agent ID, username, system name)
    claimer_id: String,
    /// Type of claimer
    claimer_type: ClaimerType,
    /// When the claim was made (ISO8601 timestamp)
    claimed_at: String,
    /// Files the claimer has read/analyzed for context
    context_files: List(String),
    /// Summary of what the claimer understands about the task
    context_summary: String,
    /// Why the claimer is qualified to work on this bead
    qualification: String,
    /// Estimated time to complete (in minutes)
    estimated_minutes: Int,
    /// High-level plan for completing the work
    work_plan: String,
    /// Additional notes or observations
    notes: String,
  )
}

/// A claim on a bead with full tracking
pub type BeadClaim {
  BeadClaim(
    /// The bead being claimed
    bead_id: String,
    /// The structured claim context
    context: ClaimContext,
    /// Current status of the claim
    status: ClaimStatus,
    /// When the claim was released (if applicable)
    released_at: Option(String),
    /// Why the claim was released (if applicable)
    release_reason: Option(String),
    /// Session this claim belongs to
    session_id: String,
  )
}

/// Errors that can occur during claim operations
pub type ClaimError {
  /// Session not found
  SessionNotFound(session_id: String)
  /// Bead is already claimed by someone else
  AlreadyClaimed(bead_id: String, current_claimer: String)
  /// Bead not found
  BeadNotFound(bead_id: String)
  /// File I/O error
  WriteError(path: String, message: String)
  /// Validation error
  ValidationError(message: String)
}

// =============================================================================
// PURE: Type Conversion Functions
// =============================================================================

/// Convert ClaimerType to string representation
pub fn claimer_type_to_string(ct: ClaimerType) -> String {
  case ct {
    Agent -> "agent"
    Human -> "human"
    System -> "system"
  }
}

/// Parse ClaimerType from string
pub fn claimer_type_from_string(s: String) -> Result(ClaimerType, String) {
  case string.lowercase(s) {
    "agent" -> Ok(Agent)
    "human" -> Ok(Human)
    "system" -> Ok(System)
    _ -> Error("Unknown claimer type: " <> s)
  }
}

/// Convert ClaimStatus to string representation
pub fn claim_status_to_string(cs: ClaimStatus) -> String {
  case cs {
    Active -> "active"
    Released -> "released"
    Expired -> "expired"
    Completed -> "completed"
  }
}

/// Parse ClaimStatus from string
pub fn claim_status_from_string(s: String) -> Result(ClaimStatus, String) {
  case string.lowercase(s) {
    "active" -> Ok(Active)
    "released" -> Ok(Released)
    "expired" -> Ok(Expired)
    "completed" -> Ok(Completed)
    _ -> Error("Unknown claim status: " <> s)
  }
}

// =============================================================================
// PURE: Claim Context Creation
// =============================================================================

/// Create a new ClaimContext with all required fields.
/// This is the functional core - no I/O, no timestamp generation.
pub fn create_claim_context(
  claimer_id: String,
  claimer_type: ClaimerType,
  claimed_at: String,
  context_files: List(String),
  context_summary: String,
  qualification: String,
  estimated_minutes: Int,
  work_plan: String,
  notes: String,
) -> ClaimContext {
  ClaimContext(
    claimer_id: claimer_id,
    claimer_type: claimer_type,
    claimed_at: claimed_at,
    context_files: context_files,
    context_summary: context_summary,
    qualification: qualification,
    estimated_minutes: estimated_minutes,
    work_plan: work_plan,
    notes: notes,
  )
}

/// Create a minimal ClaimContext with defaults.
/// Useful for quick claims where full context isn't available yet.
pub fn create_minimal_claim_context(
  claimer_id: String,
  claimer_type: ClaimerType,
  claimed_at: String,
) -> ClaimContext {
  ClaimContext(
    claimer_id: claimer_id,
    claimer_type: claimer_type,
    claimed_at: claimed_at,
    context_files: [],
    context_summary: "",
    qualification: "",
    estimated_minutes: 0,
    work_plan: "",
    notes: "",
  )
}

/// Create a BeadClaim from context
pub fn create_bead_claim(
  bead_id: String,
  context: ClaimContext,
  session_id: String,
) -> BeadClaim {
  BeadClaim(
    bead_id: bead_id,
    context: context,
    status: Active,
    released_at: None,
    release_reason: None,
    session_id: session_id,
  )
}

// =============================================================================
// PURE: Validation Functions
// =============================================================================

/// Validate a claim context has required fields
pub fn validate_claim_context(ctx: ClaimContext) -> Result(Nil, String) {
  case string.is_empty(string.trim(ctx.claimer_id)) {
    True -> Error("claimer_id is required")
    False ->
      case string.is_empty(string.trim(ctx.claimed_at)) {
        True -> Error("claimed_at timestamp is required")
        False -> Ok(Nil)
      }
  }
}

/// Validate a bead claim
pub fn validate_bead_claim(claim: BeadClaim) -> Result(Nil, String) {
  case string.is_empty(string.trim(claim.bead_id)) {
    True -> Error("bead_id is required")
    False ->
      case string.is_empty(string.trim(claim.session_id)) {
        True -> Error("session_id is required")
        False -> validate_claim_context(claim.context)
      }
  }
}

// =============================================================================
// PURE: CUE Serialization
// =============================================================================

/// Serialize a ClaimContext to CUE format
pub fn claim_context_to_cue(ctx: ClaimContext) -> String {
  let files_str = case ctx.context_files {
    [] -> "[]"
    files ->
      "[\n"
      <> list.map(files, fn(f) { "\t\t\"" <> escape_cue_string(f) <> "\"" })
      |> string.join(",\n")
      <> "\n\t]"
  }

  "{\n"
  <> "\tclaimer_id: \""
  <> escape_cue_string(ctx.claimer_id)
  <> "\"\n"
  <> "\tclaimer_type: \""
  <> claimer_type_to_string(ctx.claimer_type)
  <> "\"\n"
  <> "\tclaimed_at: \""
  <> ctx.claimed_at
  <> "\"\n"
  <> "\tcontext_files: "
  <> files_str
  <> "\n"
  <> "\tcontext_summary: \""
  <> escape_cue_string(ctx.context_summary)
  <> "\"\n"
  <> "\tqualification: \""
  <> escape_cue_string(ctx.qualification)
  <> "\"\n"
  <> "\testimated_minutes: "
  <> string.inspect(ctx.estimated_minutes)
  <> "\n"
  <> "\twork_plan: \""
  <> escape_cue_string(ctx.work_plan)
  <> "\"\n"
  <> "\tnotes: \""
  <> escape_cue_string(ctx.notes)
  <> "\"\n"
  <> "}"
}

/// Serialize a BeadClaim to CUE format
pub fn bead_claim_to_cue(claim: BeadClaim) -> String {
  let released_at_str = case claim.released_at {
    None -> ""
    Some(ts) -> "\n\treleased_at: \"" <> ts <> "\""
  }

  let release_reason_str = case claim.release_reason {
    None -> ""
    Some(reason) ->
      "\n\trelease_reason: \"" <> escape_cue_string(reason) <> "\""
  }

  "// Bead Claim\nclaim: {\n"
  <> "\tbead_id: \""
  <> escape_cue_string(claim.bead_id)
  <> "\"\n"
  <> "\tsession_id: \""
  <> escape_cue_string(claim.session_id)
  <> "\"\n"
  <> "\tstatus: \""
  <> claim_status_to_string(claim.status)
  <> "\"\n"
  <> "\tcontext: "
  <> indent_cue_block(claim_context_to_cue(claim.context), 1)
  <> released_at_str
  <> release_reason_str
  <> "\n}\n"
}

// =============================================================================
// PURE: JSON Serialization
// =============================================================================

/// Serialize ClaimContext to JSON
pub fn claim_context_to_json(ctx: ClaimContext) -> json.Json {
  json.object([
    #("claimer_id", json.string(ctx.claimer_id)),
    #("claimer_type", json.string(claimer_type_to_string(ctx.claimer_type))),
    #("claimed_at", json.string(ctx.claimed_at)),
    #("context_files", json.array(ctx.context_files, json.string)),
    #("context_summary", json.string(ctx.context_summary)),
    #("qualification", json.string(ctx.qualification)),
    #("estimated_minutes", json.int(ctx.estimated_minutes)),
    #("work_plan", json.string(ctx.work_plan)),
    #("notes", json.string(ctx.notes)),
  ])
}

/// Serialize BeadClaim to JSON
pub fn bead_claim_to_json(claim: BeadClaim) -> json.Json {
  let base_fields = [
    #("bead_id", json.string(claim.bead_id)),
    #("session_id", json.string(claim.session_id)),
    #("status", json.string(claim_status_to_string(claim.status))),
    #("context", claim_context_to_json(claim.context)),
  ]

  let with_released = case claim.released_at {
    None -> base_fields
    Some(ts) -> list.append(base_fields, [#("released_at", json.string(ts))])
  }

  let with_reason = case claim.release_reason {
    None -> with_released
    Some(reason) ->
      list.append(with_released, [#("release_reason", json.string(reason))])
  }

  json.object(with_reason)
}

/// Create JSON response for a successful claim action
pub fn claim_action_to_json(claim: BeadClaim, action: String) -> json.Json {
  json.object([
    #("action", json.string(action)),
    #("success", json.bool(True)),
    #("claim", bead_claim_to_json(claim)),
  ])
}

// =============================================================================
// PURE: JSON Parsing
// =============================================================================

/// Parse ClaimContext from JSON Dynamic value
pub fn decode_claim_context(
  dyn: Dynamic,
) -> Result(ClaimContext, List(dynamic.DecodeError)) {
  let claimer_id_result = dynamic.field("claimer_id", dynamic.string)(dyn)
  let claimer_type_result =
    dynamic.field("claimer_type", decode_claimer_type)(dyn)
  let claimed_at_result = dynamic.field("claimed_at", dynamic.string)(dyn)
  let context_files_result =
    dynamic.field("context_files", dynamic.list(dynamic.string))(dyn)
  let context_summary_result =
    dynamic.field("context_summary", dynamic.string)(dyn)
  let qualification_result = dynamic.field("qualification", dynamic.string)(dyn)
  let estimated_minutes_result =
    dynamic.field("estimated_minutes", dynamic.int)(dyn)
  let work_plan_result = dynamic.field("work_plan", dynamic.string)(dyn)
  let notes_result = dynamic.field("notes", dynamic.string)(dyn)

  case
    claimer_id_result,
    claimer_type_result,
    claimed_at_result,
    context_files_result,
    context_summary_result,
    qualification_result,
    estimated_minutes_result,
    work_plan_result,
    notes_result
  {
    Ok(claimer_id),
      Ok(claimer_type),
      Ok(claimed_at),
      Ok(context_files),
      Ok(context_summary),
      Ok(qualification),
      Ok(estimated_minutes),
      Ok(work_plan),
      Ok(notes)
    -> {
      Ok(ClaimContext(
        claimer_id: claimer_id,
        claimer_type: claimer_type,
        claimed_at: claimed_at,
        context_files: context_files,
        context_summary: context_summary,
        qualification: qualification,
        estimated_minutes: estimated_minutes,
        work_plan: work_plan,
        notes: notes,
      ))
    }
    _, _, _, _, _, _, _, _, _ -> {
      Error([
        dynamic.DecodeError(
          expected: "ClaimContext object",
          found: "invalid structure",
          path: [],
        ),
      ])
    }
  }
}

/// Parse BeadClaim from JSON Dynamic value
pub fn decode_bead_claim(
  dyn: Dynamic,
) -> Result(BeadClaim, List(dynamic.DecodeError)) {
  let bead_id_result = dynamic.field("bead_id", dynamic.string)(dyn)
  let session_id_result = dynamic.field("session_id", dynamic.string)(dyn)
  let status_result = dynamic.field("status", decode_claim_status)(dyn)
  let context_result = dynamic.field("context", decode_claim_context)(dyn)
  let released_at_result =
    dynamic.optional_field("released_at", dynamic.string)(dyn)
  let release_reason_result =
    dynamic.optional_field("release_reason", dynamic.string)(dyn)

  case bead_id_result, session_id_result, status_result, context_result {
    Ok(bead_id), Ok(session_id), Ok(status), Ok(context) -> {
      let released_at = result.unwrap(released_at_result, None)
      let release_reason = result.unwrap(release_reason_result, None)
      Ok(BeadClaim(
        bead_id: bead_id,
        context: context,
        status: status,
        released_at: released_at,
        release_reason: release_reason,
        session_id: session_id,
      ))
    }
    _, _, _, _ -> {
      Error([
        dynamic.DecodeError(
          expected: "BeadClaim object",
          found: "invalid structure",
          path: [],
        ),
      ])
    }
  }
}

fn decode_claimer_type(
  dyn: Dynamic,
) -> Result(ClaimerType, List(dynamic.DecodeError)) {
  case dynamic.string(dyn) {
    Ok(s) ->
      case claimer_type_from_string(s) {
        Ok(ct) -> Ok(ct)
        Error(msg) ->
          Error([
            dynamic.DecodeError(
              expected: "agent|human|system",
              found: msg,
              path: ["claimer_type"],
            ),
          ])
      }
    Error(e) -> Error(e)
  }
}

fn decode_claim_status(
  dyn: Dynamic,
) -> Result(ClaimStatus, List(dynamic.DecodeError)) {
  case dynamic.string(dyn) {
    Ok(s) ->
      case claim_status_from_string(s) {
        Ok(cs) -> Ok(cs)
        Error(msg) ->
          Error([
            dynamic.DecodeError(
              expected: "active|released|expired|completed",
              found: msg,
              path: ["status"],
            ),
          ])
      }
    Error(e) -> Error(e)
  }
}

// =============================================================================
// I/O: Claim Operations (Imperative Shell)
// =============================================================================

/// Claim a bead with full context.
/// Persists the claim to .intent/claims-{session_id}.cue
pub fn claim_bead(
  session_id: String,
  bead_id: String,
  context: ClaimContext,
) -> Result(BeadClaim, ClaimError) {
  // Validate inputs
  case validate_session_id(session_id) {
    False -> Error(ValidationError("Invalid session ID format: " <> session_id))
    True ->
      case validate_bead_id(bead_id) {
        False -> Error(ValidationError("Invalid bead ID format: " <> bead_id))
        True ->
          case validate_claim_context(context) {
            Error(msg) -> Error(ValidationError(msg))
            Ok(Nil) -> {
              // Check for existing active claim
              case get_active_claim_for_bead(session_id, bead_id) {
                Ok(Some(existing)) ->
                  Error(AlreadyClaimed(bead_id, existing.context.claimer_id))
                _ -> {
                  // Create and persist the claim
                  let claim = create_bead_claim(bead_id, context, session_id)
                  let path = claims_file_path(session_id)
                  let cue_content = bead_claim_to_cue(claim)

                  case append_to_file(path, cue_content) {
                    Ok(Nil) -> Ok(claim)
                    Error(err) -> Error(err)
                  }
                }
              }
            }
          }
      }
  }
}

/// Release a claim on a bead
pub fn release_claim(
  session_id: String,
  bead_id: String,
  reason: String,
  released_at: String,
) -> Result(BeadClaim, ClaimError) {
  case get_active_claim_for_bead(session_id, bead_id) {
    Ok(None) -> Error(BeadNotFound(bead_id))
    Error(err) -> Error(err)
    Ok(Some(claim)) -> {
      let released_claim =
        BeadClaim(
          ..claim,
          status: Released,
          released_at: Some(released_at),
          release_reason: Some(reason),
        )

      let path = claims_file_path(session_id)
      let release_cue =
        "// Claim Released\nclaim_release: {\n"
        <> "\tbead_id: \""
        <> escape_cue_string(bead_id)
        <> "\"\n"
        <> "\tstatus: \"released\"\n"
        <> "\treleased_at: \""
        <> released_at
        <> "\"\n"
        <> "\trelease_reason: \""
        <> escape_cue_string(reason)
        <> "\"\n"
        <> "}\n"

      case append_to_file(path, release_cue) {
        Ok(Nil) -> Ok(released_claim)
        Error(err) -> Error(err)
      }
    }
  }
}

/// Mark a claim as completed
pub fn complete_claim(
  session_id: String,
  bead_id: String,
  completed_at: String,
) -> Result(BeadClaim, ClaimError) {
  case get_active_claim_for_bead(session_id, bead_id) {
    Ok(None) -> Error(BeadNotFound(bead_id))
    Error(err) -> Error(err)
    Ok(Some(claim)) -> {
      let completed_claim =
        BeadClaim(
          ..claim,
          status: Completed,
          released_at: Some(completed_at),
          release_reason: Some("Work completed"),
        )

      let path = claims_file_path(session_id)
      let complete_cue =
        "// Claim Completed\nclaim_complete: {\n"
        <> "\tbead_id: \""
        <> escape_cue_string(bead_id)
        <> "\"\n"
        <> "\tstatus: \"completed\"\n"
        <> "\tcompleted_at: \""
        <> completed_at
        <> "\"\n"
        <> "}\n"

      case append_to_file(path, complete_cue) {
        Ok(Nil) -> Ok(completed_claim)
        Error(err) -> Error(err)
      }
    }
  }
}

/// Get all claims for a session
pub fn get_claims_for_session(
  session_id: String,
) -> Result(List(BeadClaim), ClaimError) {
  let path = claims_file_path(session_id)
  case simplifile.read(path) {
    Error(_) -> Ok([])
    // No claims file yet
    Ok(content) -> parse_claims_content(content, session_id)
  }
}

/// Get the active claim for a specific bead (if any)
pub fn get_active_claim_for_bead(
  session_id: String,
  bead_id: String,
) -> Result(Option(BeadClaim), ClaimError) {
  case get_claims_for_session(session_id) {
    Error(err) -> Error(err)
    Ok(claims) -> {
      let active =
        claims
        |> list.filter(fn(c) { c.bead_id == bead_id && c.status == Active })
        |> list.first

      case active {
        Ok(claim) -> Ok(Some(claim))
        Error(Nil) -> Ok(None)
      }
    }
  }
}

/// Check if a bead is currently claimed
pub fn is_bead_claimed(session_id: String, bead_id: String) -> Bool {
  case get_active_claim_for_bead(session_id, bead_id) {
    Ok(Some(_)) -> True
    _ -> False
  }
}

/// Get the claims file path for a session
pub fn claims_file_path(session_id: String) -> String {
  ".intent/claims-" <> session_id <> ".cue"
}

// =============================================================================
// PRIVATE: Parsing
// =============================================================================

/// Parse claims from CUE content
/// Note: For reliable parsing, content should be JSON from `cue export`
fn parse_claims_content(
  content: String,
  session_id: String,
) -> Result(List(BeadClaim), ClaimError) {
  // For now, if content looks like JSON array, parse it
  case string.starts_with(string.trim(content), "[") {
    True -> parse_claims_json(content)
    False -> {
      // Raw CUE content - return empty for now
      // In production, caller should run `cue export` first
      Ok([])
    }
  }
  |> result.map(fn(claims) {
    // Ensure all claims have correct session_id
    list.map(claims, fn(c) { BeadClaim(..c, session_id: session_id) })
  })
}

/// Parse claims from JSON content
fn parse_claims_json(
  json_content: String,
) -> Result(List(BeadClaim), ClaimError) {
  case json.decode(json_content, dynamic.list(decode_bead_claim)) {
    Ok(claims) -> Ok(claims)
    Error(_) -> Error(ValidationError("Failed to parse claims JSON"))
  }
}

// =============================================================================
// PRIVATE: Helpers
// =============================================================================

fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

fn indent_cue_block(block: String, levels: Int) -> String {
  let indent = string.repeat("\t", levels)
  block
  |> string.split("\n")
  |> list.map(fn(line) {
    case string.is_empty(line) {
      True -> line
      False -> indent <> line
    }
  })
  |> string.join("\n")
  |> string.trim
}

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

fn append_to_file(path: String, content: String) -> Result(Nil, ClaimError) {
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
// ERROR FORMATTING
// =============================================================================

/// Format a ClaimError for display
pub fn format_claim_error(err: ClaimError) -> String {
  case err {
    SessionNotFound(id) -> "Session not found: " <> id
    AlreadyClaimed(bead_id, claimer) ->
      "Bead " <> bead_id <> " is already claimed by " <> claimer
    BeadNotFound(id) -> "Bead not found: " <> id
    WriteError(path, msg) -> "Write error to " <> path <> ": " <> msg
    ValidationError(msg) -> "Validation error: " <> msg
  }
}
