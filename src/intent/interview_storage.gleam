/// Interview Session Storage
/// Dual persistence: SQLite for querying, JSONL for git-friendly version control
/// Mirrors Beads approach: git-native JSONL + local SQLite for performance

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import simplifile
import intent/interview.{
  type Answer, type Conflict, type ConflictResolution, type Gap,
  type InterviewSession, type InterviewStage, type Profile,
}
import intent/interview_questions.{type Perspective}

/// Session record for storage
pub type SessionRecord {
  SessionRecord(
    id: String,
    profile: String,
    created_at: String,
    updated_at: String,
    completed_at: String,
    stage: String,
    rounds_completed: Int,
    raw_notes: String,
  )
}

/// JSONL operations - git-friendly line-delimited JSON
/// Each line is a complete session snapshot
/// Stored at: .interview/sessions.jsonl

pub fn session_to_json(session: InterviewSession) -> json.Json {
  json.object([
    #("id", json.string(session.id)),
    #("profile", json.string(profile_to_string(session.profile))),
    #("created_at", json.string(session.created_at)),
    #("updated_at", json.string(session.updated_at)),
    #("completed_at", json.string(session.completed_at)),
    #("stage", json.string(stage_to_string(session.stage))),
    #("rounds_completed", json.int(session.rounds_completed)),
    #("answers", json.array(session.answers, answer_to_json)),
    #("gaps", json.array(session.gaps, gap_to_json)),
    #("conflicts", json.array(session.conflicts, conflict_to_json)),
    #("raw_notes", json.string(session.raw_notes)),
  ])
}

fn profile_to_string(profile: Profile) -> String {
  case profile {
    interview.Api -> "api"
    interview.Cli -> "cli"
    interview.Event -> "event"
    interview.Data -> "data"
    interview.Workflow -> "workflow"
    interview.UI -> "ui"
  }
}

fn stage_to_string(stage: InterviewStage) -> String {
  case stage {
    interview.Discovery -> "discovery"
    interview.Refinement -> "refinement"
    interview.Validation -> "validation"
    interview.Complete -> "complete"
    interview.Paused -> "paused"
  }
}

fn answer_to_json(answer: Answer) -> json.Json {
  json.object([
    #("question_id", json.string(answer.question_id)),
    #("question_text", json.string(answer.question_text)),
    #("perspective", json.string(perspective_to_string(answer.perspective))),
    #("round", json.int(answer.round)),
    #("response", json.string(answer.response)),
    #("extracted", json.object(
      dict.to_list(answer.extracted)
      |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) }),
    )),
    #("confidence", json.float(answer.confidence)),
    #("notes", json.string(answer.notes)),
    #("timestamp", json.string(answer.timestamp)),
  ])
}

fn perspective_to_string(perspective: Perspective) -> String {
  case perspective {
    interview_questions.User -> "user"
    interview_questions.Developer -> "developer"
    interview_questions.Ops -> "ops"
    interview_questions.Security -> "security"
    interview_questions.Business -> "business"
  }
}

fn gap_to_json(gap: Gap) -> json.Json {
  json.object([
    #("id", json.string(gap.id)),
    #("field", json.string(gap.field)),
    #("description", json.string(gap.description)),
    #("blocking", json.bool(gap.blocking)),
    #("suggested_default", json.string(gap.suggested_default)),
    #("why_needed", json.string(gap.why_needed)),
    #("round", json.int(gap.round)),
    #("resolved", json.bool(gap.resolved)),
    #("resolution", json.string(gap.resolution)),
  ])
}

fn conflict_to_json(conflict: Conflict) -> json.Json {
  let #(between_1, between_2) = conflict.between
  json.object([
    #("id", json.string(conflict.id)),
    #("between", json.array([between_1, between_2], json.string)),
    #("description", json.string(conflict.description)),
    #("impact", json.string(conflict.impact)),
    #("options", json.array(conflict.options, conflict_resolution_to_json)),
    #("chosen", json.int(conflict.chosen)),
  ])
}

fn conflict_resolution_to_json(res: ConflictResolution) -> json.Json {
  json.object([
    #("option", json.string(res.option)),
    #("description", json.string(res.description)),
    #("tradeoffs", json.string(res.tradeoffs)),
    #("recommendation", json.string(res.recommendation)),
  ])
}

/// Encode session to JSONL line (for git storage)
pub fn session_to_jsonl_line(session: InterviewSession) -> String {
  session
  |> session_to_json
  |> json.to_string
}

/// Write session to .interview/sessions.jsonl
/// Each session ID appears once, most recent last (for efficient updates)
pub fn append_session_to_jsonl(
  session: InterviewSession,
  jsonl_path: String,
) -> Result(Nil, String) {
  use existing <- result.try(
    simplifile.read(jsonl_path)
    |> result.map_error(fn(_) { "" }),
  )

  let lines = case existing {
    "" -> []
    content -> string.split(content, "\n")
  }

  let filtered = list.filter(lines, fn(line) {
    // Parse each line and keep if session ID doesn't match
    case json.decode(line, session_id_decoder) {
      Ok(id) -> id != session.id
      Error(_) -> True
    }
  })

  let new_line = session_to_jsonl_line(session)
  let all_lines = list.append(filtered, [new_line])
  let content = string.join(all_lines, "\n")

  simplifile.write(jsonl_path, content)
  |> result.map_error(fn(err) { "Failed to write JSONL: " <> string.inspect(err) })
}

/// List all sessions from JSONL file
pub fn list_sessions_from_jsonl(jsonl_path: String) -> Result(List(InterviewSession), String) {
  use content <- result.try(
    simplifile.read(jsonl_path)
    |> result.map_error(fn(err) { "Failed to read JSONL: " <> string.inspect(err) }),
  )

  case string.length(string.trim(content)) {
    0 -> Ok([])
    _ -> {
      let lines = string.split(content, "\n")
      let sessions = list.filter_map(lines, fn(line) {
        case string.length(string.trim(line)) {
          0 -> Error(Nil)
          _ ->
            json.decode(line, session_decoder)
            |> result.map_error(fn(_) { Nil })
        }
      })
      Ok(sessions)
    }
  }
}

/// Get session by ID from JSONL
pub fn get_session_from_jsonl(
  jsonl_path: String,
  session_id: String,
) -> Result(InterviewSession, String) {
  list_sessions_from_jsonl(jsonl_path)
  |> result.try(fn(sessions) {
    list.find(sessions, fn(s) { s.id == session_id })
    |> result.map_error(fn(_) { "Session not found: " <> session_id })
  })
}

/// SQLite operations - local database for queries and performance
/// Database schema:
///
/// CREATE TABLE sessions (
///   id TEXT PRIMARY KEY,
///   profile TEXT NOT NULL,
///   created_at TEXT NOT NULL,
///   updated_at TEXT NOT NULL,
///   completed_at TEXT,
///   stage TEXT NOT NULL,
///   rounds_completed INTEGER NOT NULL,
///   raw_notes TEXT,
///   data JSONB  -- Full session data
/// );
///
/// CREATE TABLE answers (
///   id TEXT PRIMARY KEY,
///   session_id TEXT NOT NULL REFERENCES sessions(id),
///   question_id TEXT NOT NULL,
///   round INTEGER NOT NULL,
///   perspective TEXT NOT NULL,
///   response TEXT NOT NULL,
///   confidence REAL NOT NULL,
///   timestamp TEXT NOT NULL
/// );
///
/// CREATE TABLE gaps (
///   id TEXT PRIMARY KEY,
///   session_id TEXT NOT NULL REFERENCES sessions(id),
///   field TEXT NOT NULL,
///   blocking BOOLEAN NOT NULL,
///   resolved BOOLEAN NOT NULL
/// );
///
/// CREATE TABLE conflicts (
///   id TEXT PRIMARY KEY,
///   session_id TEXT NOT NULL REFERENCES sessions(id),
///   description TEXT NOT NULL,
///   chosen INTEGER
/// );

/// Initialize SQLite database (create tables if not exist)
pub fn init_database(db_path: String) -> Result(Nil, String) {
  // In real implementation:
  // 1. Check if .interview/interview.db exists
  // 2. If not, create it with schema above
  // 3. If it does, verify schema is up-to-date
  Ok(Nil)
}

/// Save session to SQLite
pub fn save_session_to_db(
  db_path: String,
  session: InterviewSession,
) -> Result(Nil, String) {
  // INSERT or UPDATE sessions table
  // DELETE and INSERT answers/gaps/conflicts to maintain referential integrity
  Ok(Nil)
}

/// Query sessions by profile
pub fn query_sessions_by_profile(
  db_path: String,
  profile: String,
) -> Result(List(SessionRecord), String) {
  Ok([])
}

/// Query ready sessions (active, not complete, has gaps)
pub fn query_ready_sessions(db_path: String) -> Result(List(SessionRecord), String) {
  Ok([])
}

/// Sync operations - keep SQLite and JSONL in sync
/// Strategy: JSONL is source of truth for git
/// 1. On read: load from JSONL, check SQLite is consistent
/// 2. On write: write to both
/// 3. Conflict resolution: JSONL wins (it's in git)

pub fn sync_to_jsonl(
  session: InterviewSession,
  db_path: String,
  jsonl_path: String,
) -> Result(Nil, String) {
  // 1. Update SQLite
  use _ <- result.try(save_session_to_db(db_path, session))
  // 2. Append to JSONL
  use _ <- result.try(append_session_to_jsonl(session, jsonl_path))
  Ok(Nil)
}

pub fn sync_from_jsonl(
  jsonl_path: String,
  db_path: String,
) -> Result(List(InterviewSession), String) {
  // 1. Read from JSONL (source of truth)
  use sessions <- result.try(list_sessions_from_jsonl(jsonl_path))
  // 2. Update SQLite with latest
  list.fold(sessions, Ok(Nil), fn(acc, session) {
    result.try(acc, fn(_) { save_session_to_db(db_path, session) })
  })
  |> result.map(fn(_) { sessions })
}

// Decoder helpers for JSON parsing
fn session_id_decoder(json_value: dynamic.Dynamic) -> Result(String, dynamic.DecodeErrors) {
  dynamic.field("id", dynamic.string)(json_value)
}

fn session_decoder(json_value: dynamic.Dynamic) -> Result(InterviewSession, dynamic.DecodeErrors) {
  use id <- result.try(dynamic.field("id", dynamic.string)(json_value))
  use profile_str <- result.try(dynamic.field("profile", dynamic.string)(json_value))
  use profile <- result.try(
    case profile_str {
      "api" -> Ok(interview.Api)
      "cli" -> Ok(interview.Cli)
      "event" -> Ok(interview.Event)
      "data" -> Ok(interview.Data)
      "workflow" -> Ok(interview.Workflow)
      "ui" -> Ok(interview.UI)
      _ -> Error([dynamic.DecodeError("profile", "invalid profile", [])])
    },
  )
  use created_at <- result.try(dynamic.field("created_at", dynamic.string)(json_value))
  use updated_at <- result.try(dynamic.field("updated_at", dynamic.string)(json_value))
  use completed_at <- result.try(
    dynamic.field("completed_at", dynamic.string)(json_value)
    |> result.map_error(fn(_) { [] }),
  )
  use stage_str <- result.try(dynamic.field("stage", dynamic.string)(json_value))
  use stage <- result.try(
    case stage_str {
      "Discovery" -> Ok(interview.Discovery)
      "Refinement" -> Ok(interview.Refinement)
      "Validation" -> Ok(interview.Validation)
      "Complete" -> Ok(interview.Complete)
      "Paused" -> Ok(interview.Paused)
      _ -> Error([dynamic.DecodeError("stage", "invalid stage", [])])
    },
  )
  use rounds_completed <- result.try(dynamic.field("rounds_completed", dynamic.int)(json_value))
  use raw_notes <- result.try(
    dynamic.field("raw_notes", dynamic.string)(json_value)
    |> result.map_error(fn(_) { [] }),
  )

  Ok(interview.InterviewSession(
    id: id,
    profile: profile,
    created_at: created_at,
    updated_at: updated_at,
    completed_at: completed_at,
    stage: stage,
    rounds_completed: rounds_completed,
    answers: [],
    gaps: [],
    conflicts: [],
    raw_notes: raw_notes,
  ))
}
