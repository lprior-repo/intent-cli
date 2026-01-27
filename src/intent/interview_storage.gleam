/// Interview Session Storage
/// Dual persistence: SQLite for querying, JSONL for git-friendly version control
/// Mirrors Beads approach: git-native JSONL + local SQLite for performance
/// Includes answer history tracking and diff comparison
///
/// Architecture: Functional Core / Imperative Shell
/// - Pure serialization/deserialization functions at the core
/// - File I/O operations accept reader/writer functions (dependency injection)
/// - Simplifile wrappers provided for convenience
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import intent/interview.{
  type Answer, type Conflict, type ConflictResolution, type Gap,
  type InterviewSession, type InterviewStage, type Profile,
}
import intent/question_types.{
  type Perspective, Business, Developer, Ops, Security, User,
}
import simplifile

// =============================================================================
// File I/O Function Types (Dependency Injection)
// =============================================================================

/// File reader function type - takes path, returns content or error
pub type FileReader =
  fn(String) -> Result(String, String)

/// File writer function type - takes path and content, returns unit or error
pub type FileWriter =
  fn(String, String) -> Result(Nil, String)

/// Directory creator function type - takes path, returns unit or error
pub type DirectoryCreator =
  fn(String) -> Result(Nil, String)

// =============================================================================
// Simplifile Adapter Functions
// =============================================================================

/// Create a FileReader that uses simplifile
pub fn simplifile_reader() -> FileReader {
  fn(path: String) -> Result(String, String) {
    simplifile.read(path)
    |> result.map_error(fn(err) {
      "Failed to read file '" <> path <> "': " <> string.inspect(err)
    })
  }
}

/// Create a FileWriter that uses simplifile
pub fn simplifile_writer() -> FileWriter {
  fn(path: String, content: String) -> Result(Nil, String) {
    simplifile.write(path, content)
    |> result.map_error(fn(err) {
      let err_msg = case err {
        simplifile.Enoent -> "File or directory not found"
        simplifile.Eacces -> "Permission denied"
        simplifile.Enospc -> "No space left on device"
        simplifile.Eio -> "I/O error"
        _ -> "Unknown error"
      }
      "Failed to write file '" <> path <> "': " <> err_msg
    })
  }
}

/// Create a DirectoryCreator that uses simplifile
pub fn simplifile_dir_creator() -> DirectoryCreator {
  fn(path: String) -> Result(Nil, String) {
    simplifile.create_directory_all(path)
    |> result.map_error(fn(err) {
      let err_msg = case err {
        simplifile.Enoent -> "Parent directory not found"
        simplifile.Eacces -> "Permission denied"
        simplifile.Enospc -> "No space left on device"
        simplifile.Eio -> "I/O error"
        _ -> "Unknown error"
      }
      "Failed to create directory '" <> path <> "': " <> err_msg
    })
  }
}

// =============================================================================
// Data Types
// =============================================================================

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

// =============================================================================
// Answer History Tracking
// =============================================================================

/// A historical version of an answer
pub type AnswerVersion {
  AnswerVersion(
    version: Int,
    response: String,
    extracted: Dict(String, String),
    confidence: Float,
    timestamp: String,
    change_reason: String,
  )
}

/// Answer with full history
pub type AnswerWithHistory {
  AnswerWithHistory(
    question_id: String,
    question_text: String,
    perspective: Perspective,
    round: Int,
    current: Answer,
    history: List(AnswerVersion),
    notes: String,
  )
}

/// Session snapshot for diff comparison
pub type SessionSnapshot {
  SessionSnapshot(
    session_id: String,
    snapshot_id: String,
    timestamp: String,
    description: String,
    answers: Dict(String, String),
    // question_id -> response
    gaps_count: Int,
    conflicts_count: Int,
    stage: String,
  )
}

/// Diff between two sessions or snapshots
pub type SessionDiff {
  SessionDiff(
    from_id: String,
    to_id: String,
    from_timestamp: String,
    to_timestamp: String,
    answers_added: List(AnswerDiff),
    answers_modified: List(AnswerDiff),
    answers_removed: List(String),
    gaps_added: Int,
    gaps_resolved: Int,
    conflicts_added: Int,
    conflicts_resolved: Int,
    stage_changed: Option(#(String, String)),
  )
}

/// Diff for a single answer
pub type AnswerDiff {
  AnswerDiff(
    question_id: String,
    question_text: String,
    old_response: Option(String),
    new_response: String,
    change_type: AnswerChangeType,
  )
}

/// Type of change to an answer
pub type AnswerChangeType {
  Added
  Modified
  Removed
}

// =============================================================================
// History Operations (Pure Functions)
// =============================================================================

/// Create an AnswerVersion from an Answer
pub fn answer_to_version(
  answer: Answer,
  version: Int,
  change_reason: String,
) -> AnswerVersion {
  AnswerVersion(
    version: version,
    response: answer.response,
    extracted: answer.extracted,
    confidence: answer.confidence,
    timestamp: answer.timestamp,
    change_reason: change_reason,
  )
}

/// Create a session snapshot for comparison
pub fn create_snapshot(
  session: InterviewSession,
  description: String,
) -> SessionSnapshot {
  let answers_dict =
    list.fold(session.answers, dict.new(), fn(acc, answer) {
      dict.insert(acc, answer.question_id, answer.response)
    })

  let unresolved_gaps = list.filter(session.gaps, fn(g) { !g.resolved })
  let unresolved_conflicts =
    list.filter(session.conflicts, fn(c) { c.chosen < 0 })

  SessionSnapshot(
    session_id: session.id,
    snapshot_id: session.id <> "-" <> session.updated_at,
    timestamp: session.updated_at,
    description: description,
    answers: answers_dict,
    gaps_count: list.length(unresolved_gaps),
    conflicts_count: list.length(unresolved_conflicts),
    stage: stage_to_string(session.stage),
  )
}

/// Compare two sessions and produce a diff
pub fn diff_sessions(
  from_session: InterviewSession,
  to_session: InterviewSession,
) -> SessionDiff {
  // Build lookup maps for answers
  let from_answers =
    list.fold(from_session.answers, dict.new(), fn(acc, a) {
      dict.insert(acc, a.question_id, a)
    })
  let to_answers =
    list.fold(to_session.answers, dict.new(), fn(acc, a) {
      dict.insert(acc, a.question_id, a)
    })

  // Find added answers (in to but not in from)
  let added =
    list.filter_map(to_session.answers, fn(answer) {
      case dict.get(from_answers, answer.question_id) {
        Ok(_) -> Error(Nil)
        Error(_) ->
          Ok(AnswerDiff(
            question_id: answer.question_id,
            question_text: answer.question_text,
            old_response: option.None,
            new_response: answer.response,
            change_type: Added,
          ))
      }
    })

  // Find modified answers (in both but different)
  let modified =
    list.filter_map(to_session.answers, fn(answer) {
      case dict.get(from_answers, answer.question_id) {
        Ok(old_answer) -> {
          case old_answer.response == answer.response {
            True -> Error(Nil)
            False ->
              Ok(AnswerDiff(
                question_id: answer.question_id,
                question_text: answer.question_text,
                old_response: option.Some(old_answer.response),
                new_response: answer.response,
                change_type: Modified,
              ))
          }
        }
        Error(_) -> Error(Nil)
      }
    })

  // Find removed answers (in from but not in to)
  let removed =
    list.filter_map(from_session.answers, fn(answer) {
      case dict.get(to_answers, answer.question_id) {
        Ok(_) -> Error(Nil)
        Error(_) -> Ok(answer.question_id)
      }
    })

  // Build lookup maps for gaps
  let from_gaps_dict =
    list.fold(from_session.gaps, dict.new(), fn(acc, g) {
      dict.insert(acc, g.id, g)
    })
  let _to_gaps_dict =
    list.fold(to_session.gaps, dict.new(), fn(acc, g) {
      dict.insert(acc, g.id, g)
    })

  // Count gaps resolved: in both sessions, from.resolved=False, to.resolved=True
  let gaps_resolved =
    list.filter(to_session.gaps, fn(gap) {
      case dict.get(from_gaps_dict, gap.id) {
        Ok(from_gap) -> !from_gap.resolved && gap.resolved
        Error(_) -> False
      }
    })
    |> list.length()

  // Count gaps added: in to but not in from, and unresolved
  let gaps_added =
    list.filter(to_session.gaps, fn(gap) {
      case dict.get(from_gaps_dict, gap.id) {
        Ok(_) -> False
        Error(_) -> !gap.resolved
      }
    })
    |> list.length()

  // Count conflict changes
  let from_unresolved_conflicts =
    list.filter(from_session.conflicts, fn(c) { c.chosen < 0 })
  let to_unresolved_conflicts =
    list.filter(to_session.conflicts, fn(c) { c.chosen < 0 })
  let conflicts_added =
    list.length(to_unresolved_conflicts)
    - list.length(from_unresolved_conflicts)
  let conflicts_resolved = case conflicts_added < 0 {
    True -> -conflicts_added
    False -> 0
  }

  // Check stage change
  let stage_changed = case from_session.stage == to_session.stage {
    True -> option.None
    False ->
      option.Some(#(
        stage_to_string(from_session.stage),
        stage_to_string(to_session.stage),
      ))
  }

  SessionDiff(
    from_id: from_session.id,
    to_id: to_session.id,
    from_timestamp: from_session.updated_at,
    to_timestamp: to_session.updated_at,
    answers_added: added,
    answers_modified: modified,
    answers_removed: removed,
    gaps_added: gaps_added,
    gaps_resolved: gaps_resolved,
    conflicts_added: case conflicts_added > 0 {
      True -> conflicts_added
      False -> 0
    },
    conflicts_resolved: conflicts_resolved,
    stage_changed: stage_changed,
  )
}

/// Format a SessionDiff as a human-readable string
pub fn format_diff(diff: SessionDiff) -> String {
  let lines = []

  // Header
  let lines =
    list.append(lines, [
      "Session Diff: " <> diff.from_id <> " → " <> diff.to_id,
      "Time: " <> diff.from_timestamp <> " → " <> diff.to_timestamp,
      "",
    ])

  // Stage change
  let lines = case diff.stage_changed {
    option.Some(#(from, to)) ->
      list.append(lines, ["Stage: " <> from <> " → " <> to, ""])
    option.None -> lines
  }

  // Answers added
  let lines = case list.length(diff.answers_added) {
    0 -> lines
    n -> {
      let header = ["Answers Added (" <> string.inspect(n) <> "):"]
      let answer_lines =
        list.map(diff.answers_added, fn(a) {
          "  + [" <> a.question_id <> "] " <> truncate(a.new_response, 50)
        })
      list.append(lines, list.append(header, list.append(answer_lines, [""])))
    }
  }

  // Answers modified
  let lines = case list.length(diff.answers_modified) {
    0 -> lines
    n -> {
      let header = ["Answers Modified (" <> string.inspect(n) <> "):"]
      let answer_lines =
        list.flat_map(diff.answers_modified, fn(a) {
          let old = case a.old_response {
            option.Some(r) -> truncate(r, 40)
            option.None -> "(none)"
          }
          [
            "  ~ [" <> a.question_id <> "]",
            "    - " <> old,
            "    + " <> truncate(a.new_response, 40),
          ]
        })
      list.append(lines, list.append(header, list.append(answer_lines, [""])))
    }
  }

  // Answers removed
  let lines = case list.length(diff.answers_removed) {
    0 -> lines
    n -> {
      let header = ["Answers Removed (" <> string.inspect(n) <> "):"]
      let answer_lines =
        list.map(diff.answers_removed, fn(id) { "  - [" <> id <> "]" })
      list.append(lines, list.append(header, list.append(answer_lines, [""])))
    }
  }

  // Gaps and conflicts summary
  let lines = case diff.gaps_added > 0 || diff.gaps_resolved > 0 {
    True ->
      list.append(lines, [
        "Gaps: +"
        <> string.inspect(diff.gaps_added)
        <> " added, -"
        <> string.inspect(diff.gaps_resolved)
        <> " resolved",
      ])
    False -> lines
  }

  let lines = case diff.conflicts_added > 0 || diff.conflicts_resolved > 0 {
    True ->
      list.append(lines, [
        "Conflicts: +"
        <> string.inspect(diff.conflicts_added)
        <> " added, -"
        <> string.inspect(diff.conflicts_resolved)
        <> " resolved",
      ])
    False -> lines
  }

  string.join(lines, "\n")
}

/// Truncate a string with ellipsis
fn truncate(s: String, max_len: Int) -> String {
  let trimmed = string.trim(s)
  case string.length(trimmed) > max_len {
    True -> string.slice(trimmed, 0, max_len - 3) <> "..."
    False -> trimmed
  }
}

// =============================================================================
// Session History JSONL - Pure Serialization Functions
// =============================================================================

/// Serialize a snapshot to a JSONL line (pure)
pub fn snapshot_to_jsonl_line(snapshot: SessionSnapshot) -> String {
  json.object([
    #("session_id", json.string(snapshot.session_id)),
    #("snapshot_id", json.string(snapshot.snapshot_id)),
    #("timestamp", json.string(snapshot.timestamp)),
    #("description", json.string(snapshot.description)),
    #(
      "answers",
      json.object(
        dict.to_list(snapshot.answers)
        |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) }),
      ),
    ),
    #("gaps_count", json.int(snapshot.gaps_count)),
    #("conflicts_count", json.int(snapshot.conflicts_count)),
    #("stage", json.string(snapshot.stage)),
  ])
  |> json.to_string
}

/// Append a snapshot to existing history content (pure)
/// Returns the new complete content string
pub fn append_history_content(
  existing_content: String,
  snapshot: SessionSnapshot,
) -> String {
  let line = snapshot_to_jsonl_line(snapshot)
  case string.length(string.trim(existing_content)) {
    0 -> line
    _ -> existing_content <> "\n" <> line
  }
}

/// Parse history content and filter by session ID (pure)
pub fn parse_history_content(
  content: String,
  session_id: String,
) -> List(SessionSnapshot) {
  case string.length(string.trim(content)) {
    0 -> []
    _ -> {
      string.split(content, "\n")
      |> list.filter_map(fn(line) {
        case string.length(string.trim(line)) {
          0 -> Error(Nil)
          _ ->
            json.decode(line, snapshot_decoder)
            |> result.map_error(fn(_) { Nil })
        }
      })
      |> list.filter(fn(s) { s.session_id == session_id })
    }
  }
}

/// Parse all history content without session filtering (pure)
pub fn parse_all_history_content(content: String) -> List(SessionSnapshot) {
  case string.length(string.trim(content)) {
    0 -> []
    _ -> {
      string.split(content, "\n")
      |> list.filter_map(fn(line) {
        case string.length(string.trim(line)) {
          0 -> Error(Nil)
          _ ->
            json.decode(line, snapshot_decoder)
            |> result.map_error(fn(_) { Nil })
        }
      })
    }
  }
}

/// Convert a snapshot to JSON for API output
pub fn snapshot_to_json(snapshot: SessionSnapshot) -> json.Json {
  json.object([
    #("session_id", json.string(snapshot.session_id)),
    #("snapshot_id", json.string(snapshot.snapshot_id)),
    #("timestamp", json.string(snapshot.timestamp)),
    #("description", json.string(snapshot.description)),
    #(
      "answers",
      json.object(
        dict.to_list(snapshot.answers)
        |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) }),
      ),
    ),
    #("gaps_count", json.int(snapshot.gaps_count)),
    #("conflicts_count", json.int(snapshot.conflicts_count)),
    #("stage", json.string(snapshot.stage)),
  ])
}

// =============================================================================
// Session History JSONL - I/O Functions with Dependency Injection
// =============================================================================

/// Append a session snapshot to history JSONL (with DI)
/// Accepts reader/writer functions for testability
pub fn append_to_history_with_io(
  session: InterviewSession,
  description: String,
  history_path: String,
  reader: FileReader,
  writer: FileWriter,
) -> Result(Nil, String) {
  let snapshot = create_snapshot(session, description)
  let existing = reader(history_path) |> result.unwrap("")
  let new_content = append_history_content(existing, snapshot)
  writer(history_path, new_content)
}

/// List all snapshots for a session from history (with DI)
pub fn list_session_history_with_io(
  history_path: String,
  session_id: String,
  reader: FileReader,
) -> Result(List(SessionSnapshot), String) {
  use content <- result.try(reader(history_path))
  Ok(parse_history_content(content, session_id))
}

// =============================================================================
// Session History JSONL - Simplifile Convenience Wrappers
// =============================================================================

/// Append a session snapshot to history JSONL using simplifile
/// File: .intent/history.jsonl
pub fn append_to_history(
  session: InterviewSession,
  description: String,
  history_path: String,
) -> Result(Nil, String) {
  append_to_history_with_io(
    session,
    description,
    history_path,
    simplifile_reader(),
    simplifile_writer(),
  )
}

/// List all snapshots for a session from history using simplifile
pub fn list_session_history(
  history_path: String,
  session_id: String,
) -> Result(List(SessionSnapshot), String) {
  list_session_history_with_io(history_path, session_id, simplifile_reader())
}

/// List all history snapshots from all sessions using simplifile
pub fn list_all_history(
  history_path: String,
) -> Result(List(SessionSnapshot), String) {
  list_all_history_with_io(history_path, simplifile_reader())
}

/// List all history snapshots from all sessions (with DI)
pub fn list_all_history_with_io(
  history_path: String,
  reader: FileReader,
) -> Result(List(SessionSnapshot), String) {
  use content <- result.try(reader(history_path))
  Ok(parse_all_history_content(content))
}

fn snapshot_decoder(
  json_value: dynamic.Dynamic,
) -> Result(SessionSnapshot, dynamic.DecodeErrors) {
  use session_id <- result.try(dynamic.field("session_id", dynamic.string)(
    json_value,
  ))
  use snapshot_id <- result.try(dynamic.field("snapshot_id", dynamic.string)(
    json_value,
  ))
  use timestamp <- result.try(dynamic.field("timestamp", dynamic.string)(
    json_value,
  ))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    json_value,
  ))
  use answers_list <- result.try(dynamic.field(
    "answers",
    dynamic.dict(dynamic.string, dynamic.string),
  )(json_value))
  use gaps_count <- result.try(dynamic.field("gaps_count", dynamic.int)(
    json_value,
  ))
  use conflicts_count <- result.try(dynamic.field(
    "conflicts_count",
    dynamic.int,
  )(json_value))
  use stage <- result.try(dynamic.field("stage", dynamic.string)(json_value))

  Ok(SessionSnapshot(
    session_id: session_id,
    snapshot_id: snapshot_id,
    timestamp: timestamp,
    description: description,
    answers: answers_list,
    gaps_count: gaps_count,
    conflicts_count: conflicts_count,
    stage: stage,
  ))
}

// =============================================================================
// Session JSON Serialization - Pure Functions
// =============================================================================

/// JSONL operations - git-friendly line-delimited JSON
/// Each line is a complete session snapshot
/// Stored at: .intent/sessions.jsonl
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
    #(
      "extracted",
      json.object(
        dict.to_list(answer.extracted)
        |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) }),
      ),
    ),
    #("confidence", json.float(answer.confidence)),
    #("notes", json.string(answer.notes)),
    #("timestamp", json.string(answer.timestamp)),
  ])
}

fn perspective_to_string(perspective: Perspective) -> String {
  case perspective {
    User -> "user"
    Developer -> "developer"
    Ops -> "ops"
    Security -> "security"
    Business -> "business"
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

// =============================================================================
// Sessions JSONL - Pure Functions
// =============================================================================

/// Encode session to JSONL line (pure - for git storage)
pub fn session_to_jsonl_line(session: InterviewSession) -> String {
  session
  |> session_to_json
  |> json.to_string
}

/// Update sessions content by replacing/adding a session (pure)
/// Filters out existing session with same ID and appends the new version
/// Returns the new complete content string
pub fn update_sessions_content(
  existing_content: String,
  session: InterviewSession,
) -> String {
  let lines = case existing_content {
    "" -> []
    content -> string.split(content, "\n")
  }

  let filtered =
    list.filter(lines, fn(line) {
      // Parse each line and keep if session ID doesn't match
      case json.decode(line, session_id_decoder) {
        Ok(id) -> id != session.id
        Error(_) -> True
      }
    })

  let new_line = session_to_jsonl_line(session)
  let all_lines = list.append(filtered, [new_line])
  string.join(all_lines, "\n")
}

/// Parse sessions content (pure)
/// Returns list of successfully parsed sessions
pub fn parse_sessions_content(content: String) -> List(InterviewSession) {
  case string.length(string.trim(content)) {
    0 -> []
    _ -> {
      string.split(content, "\n")
      |> list.filter_map(fn(line) {
        case string.length(string.trim(line)) {
          0 -> Error(Nil)
          _ ->
            json.decode(line, session_decoder)
            |> result.map_error(fn(_) { Nil })
        }
      })
    }
  }
}

/// Find a session by ID in parsed sessions (pure)
pub fn find_session_by_id(
  sessions: List(InterviewSession),
  session_id: String,
) -> Result(InterviewSession, String) {
  list.find(sessions, fn(s) { s.id == session_id })
  |> result.map_error(fn(_) { "Session not found: " <> session_id })
}

/// Extract parent directory path from a file path (pure)
pub fn get_parent_directory(file_path: String) -> Option(String) {
  let parts = string.split(file_path, "/")
  let dir_parts = list.take(parts, list.length(parts) - 1)
  case list.length(dir_parts) {
    0 -> option.None
    _ -> option.Some(string.join(dir_parts, "/"))
  }
}

// =============================================================================
// Sessions JSONL - I/O Functions with Dependency Injection
// =============================================================================

/// Ensure parent directory exists for a file path (with DI)
pub fn ensure_parent_directory_with_io(
  file_path: String,
  dir_creator: DirectoryCreator,
) -> Result(Nil, String) {
  case get_parent_directory(file_path) {
    option.None -> Ok(Nil)
    option.Some(dir_path) -> dir_creator(dir_path)
  }
}

/// Write session to sessions.jsonl (with DI)
/// Each session ID appears once, most recent last (for efficient updates)
pub fn append_session_to_jsonl_with_io(
  session: InterviewSession,
  jsonl_path: String,
  reader: FileReader,
  writer: FileWriter,
  dir_creator: DirectoryCreator,
) -> Result(Nil, String) {
  let existing = reader(jsonl_path) |> result.unwrap("")
  let new_content = update_sessions_content(existing, session)
  use _ <- result.try(ensure_parent_directory_with_io(jsonl_path, dir_creator))
  writer(jsonl_path, new_content)
}

/// List all sessions from JSONL file (with DI)
pub fn list_sessions_from_jsonl_with_io(
  jsonl_path: String,
  reader: FileReader,
) -> Result(List(InterviewSession), String) {
  use content <- result.try(reader(jsonl_path))
  Ok(parse_sessions_content(content))
}

/// Get session by ID from JSONL (with DI)
pub fn get_session_from_jsonl_with_io(
  jsonl_path: String,
  session_id: String,
  reader: FileReader,
) -> Result(InterviewSession, String) {
  use sessions <- result.try(list_sessions_from_jsonl_with_io(
    jsonl_path,
    reader,
  ))
  find_session_by_id(sessions, session_id)
}

// =============================================================================
// Sessions JSONL - Simplifile Convenience Wrappers
// =============================================================================

/// Ensure parent directory exists for a file path using simplifile
pub fn ensure_parent_directory(file_path: String) -> Result(Nil, String) {
  ensure_parent_directory_with_io(file_path, simplifile_dir_creator())
}

/// Write session to .intent/sessions.jsonl using simplifile
/// Each session ID appears once, most recent last (for efficient updates)
pub fn append_session_to_jsonl(
  session: InterviewSession,
  jsonl_path: String,
) -> Result(Nil, String) {
  append_session_to_jsonl_with_io(
    session,
    jsonl_path,
    simplifile_reader(),
    simplifile_writer(),
    simplifile_dir_creator(),
  )
}

/// List all sessions from JSONL file using simplifile
pub fn list_sessions_from_jsonl(
  jsonl_path: String,
) -> Result(List(InterviewSession), String) {
  list_sessions_from_jsonl_with_io(jsonl_path, simplifile_reader())
}

/// Get session by ID from JSONL using simplifile
pub fn get_session_from_jsonl(
  jsonl_path: String,
  session_id: String,
) -> Result(InterviewSession, String) {
  get_session_from_jsonl_with_io(jsonl_path, session_id, simplifile_reader())
}

// =============================================================================
// SQLite Operations (Stubs)
// =============================================================================

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
pub fn init_database(_db_path: String) -> Result(Nil, String) {
  // In real implementation:
  // 1. Check if .intent/interview.db exists
  // 2. If not, create it with schema above
  // 3. If it does, verify schema is up-to-date
  Ok(Nil)
}

/// Save session to SQLite
pub fn save_session_to_db(
  _db_path: String,
  _session: InterviewSession,
) -> Result(Nil, String) {
  // INSERT or UPDATE sessions table
  // DELETE and INSERT answers/gaps/conflicts to maintain referential integrity
  Ok(Nil)
}

/// Query sessions by profile
pub fn query_sessions_by_profile(
  _db_path: String,
  _profile: String,
) -> Result(List(SessionRecord), String) {
  Ok([])
}

/// Query ready sessions (active, not complete, has gaps)
pub fn query_ready_sessions(
  _db_path: String,
) -> Result(List(SessionRecord), String) {
  Ok([])
}

// =============================================================================
// Sync Operations
// =============================================================================

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

// =============================================================================
// Decoder Helpers for JSON Parsing
// =============================================================================

fn session_id_decoder(
  json_value: dynamic.Dynamic,
) -> Result(String, dynamic.DecodeErrors) {
  dynamic.field("id", dynamic.string)(json_value)
}

fn session_decoder(
  json_value: dynamic.Dynamic,
) -> Result(InterviewSession, dynamic.DecodeErrors) {
  use id <- result.try(dynamic.field("id", dynamic.string)(json_value))
  use profile_str <- result.try(dynamic.field("profile", dynamic.string)(
    json_value,
  ))
  use profile <- result.try(case profile_str {
    "api" -> Ok(interview.Api)
    "cli" -> Ok(interview.Cli)
    "event" -> Ok(interview.Event)
    "data" -> Ok(interview.Data)
    "workflow" -> Ok(interview.Workflow)
    "ui" -> Ok(interview.UI)
    _ -> Error([dynamic.DecodeError("profile", "invalid profile", [])])
  })
  use created_at <- result.try(dynamic.field("created_at", dynamic.string)(
    json_value,
  ))
  use updated_at <- result.try(dynamic.field("updated_at", dynamic.string)(
    json_value,
  ))
  use completed_at <- result.try(
    dynamic.field("completed_at", dynamic.string)(json_value)
    |> result.map_error(fn(_) { [] }),
  )
  use stage_str <- result.try(dynamic.field("stage", dynamic.string)(json_value))
  use stage <- result.try(case stage_str {
    "discovery" -> Ok(interview.Discovery)
    "refinement" -> Ok(interview.Refinement)
    "validation" -> Ok(interview.Validation)
    "complete" -> Ok(interview.Complete)
    "paused" -> Ok(interview.Paused)
    _ -> Error([dynamic.DecodeError("stage", "invalid stage", [])])
  })
  use rounds_completed <- result.try(dynamic.field(
    "rounds_completed",
    dynamic.int,
  )(json_value))
  use answers <- result.try(
    dynamic.field("answers", dynamic.list(answer_decoder))(json_value)
    |> result.map_error(fn(_) { [] }),
  )
  use gaps <- result.try(
    dynamic.field("gaps", dynamic.list(gap_decoder))(json_value)
    |> result.map_error(fn(_) { [] }),
  )
  use conflicts <- result.try(
    dynamic.field("conflicts", dynamic.list(conflict_decoder))(json_value)
    |> result.map_error(fn(_) { [] }),
  )
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
    answers: answers,
    gaps: gaps,
    conflicts: conflicts,
    raw_notes: raw_notes,
  ))
}

/// Decode an Answer from JSON
fn answer_decoder(
  json_value: dynamic.Dynamic,
) -> Result(Answer, dynamic.DecodeErrors) {
  use question_id <- result.try(
    dynamic.field("question_id", dynamic.string)(json_value),
  )
  use question_text <- result.try(
    dynamic.field("question_text", dynamic.string)(json_value),
  )
  use perspective_str <- result.try(
    dynamic.field("perspective", dynamic.string)(json_value),
  )
  use perspective <- result.try(case perspective_str {
    "user" -> Ok(User)
    "developer" -> Ok(Developer)
    "ops" -> Ok(Ops)
    "security" -> Ok(Security)
    "business" -> Ok(Business)
    _ -> Error([dynamic.DecodeError("perspective", "invalid perspective", [])])
  })
  use round <- result.try(dynamic.field("round", dynamic.int)(json_value))
  use response <- result.try(dynamic.field("response", dynamic.string)(json_value))

  // Decode extracted dict
  use extracted_dict <- result.try(
    dynamic.field("extracted", dynamic.dict(dynamic.string, dynamic.string))(
      json_value,
    ),
  )
  let extracted = extracted_dict

  use confidence <- result.try(
    dynamic.field("confidence", dynamic.float)(json_value),
  )
  use notes <- result.try(
    dynamic.field("notes", dynamic.string)(json_value)
    |> result.map_error(fn(_) { [] }),
  )
  use timestamp <- result.try(
    dynamic.field("timestamp", dynamic.string)(json_value),
  )

  Ok(
    interview.Answer(
      question_id: question_id,
      question_text: question_text,
      perspective: perspective,
      round: round,
      response: response,
      extracted: extracted,
      confidence: confidence,
      notes: notes,
      timestamp: timestamp,
    ),
  )
}

/// Decode a Gap from JSON
fn gap_decoder(
  json_value: dynamic.Dynamic,
) -> Result(Gap, dynamic.DecodeErrors) {
  use id <- result.try(dynamic.field("id", dynamic.string)(json_value))
  use field <- result.try(dynamic.field("field", dynamic.string)(json_value))
  use description <- result.try(
    dynamic.field("description", dynamic.string)(json_value),
  )
  use blocking <- result.try(dynamic.field("blocking", dynamic.bool)(json_value))
  use suggested_default <- result.try(
    dynamic.field("suggested_default", dynamic.string)(json_value),
  )
  use why_needed <- result.try(
    dynamic.field("why_needed", dynamic.string)(json_value),
  )
  use round <- result.try(dynamic.field("round", dynamic.int)(json_value))
  use resolved <- result.try(dynamic.field("resolved", dynamic.bool)(json_value))
  use resolution <- result.try(
    dynamic.field("resolution", dynamic.string)(json_value)
    |> result.map_error(fn(_) { [] }),
  )

  Ok(
    interview.Gap(
      id: id,
      field: field,
      description: description,
      blocking: blocking,
      suggested_default: suggested_default,
      why_needed: why_needed,
      round: round,
      resolved: resolved,
      resolution: resolution,
    ),
  )
}

/// Decode a Conflict from JSON
fn conflict_decoder(
  json_value: dynamic.Dynamic,
) -> Result(Conflict, dynamic.DecodeErrors) {
  use id <- result.try(dynamic.field("id", dynamic.string)(json_value))
  use between_list <- result.try(
    dynamic.field("between", dynamic.list(dynamic.string))(json_value),
  )
  let between = case between_list {
    [first, second] -> #(first, second)
    _ -> #("", "")
  }
  use description <- result.try(
    dynamic.field("description", dynamic.string)(json_value),
  )
  use impact <- result.try(dynamic.field("impact", dynamic.string)(json_value))
  use options <- result.try(
    dynamic.field("options", dynamic.list(conflict_resolution_decoder))(json_value),
  )
  use chosen <- result.try(dynamic.field("chosen", dynamic.int)(json_value))

  Ok(
    interview.Conflict(
      id: id,
      between: between,
      description: description,
      impact: impact,
      options: options,
      chosen: chosen,
    ),
  )
}

/// Decode a ConflictResolution from JSON
fn conflict_resolution_decoder(
  json_value: dynamic.Dynamic,
) -> Result(interview.ConflictResolution, dynamic.DecodeErrors) {
  use option <- result.try(dynamic.field("option", dynamic.string)(json_value))
  use description <- result.try(
    dynamic.field("description", dynamic.string)(json_value),
  )
  use tradeoffs <- result.try(
    dynamic.field("tradeoffs", dynamic.string)(json_value),
  )
  use recommendation <- result.try(
    dynamic.field("recommendation", dynamic.string)(json_value),
  )

  Ok(
    interview.ConflictResolution(
      option: option,
      description: description,
      tradeoffs: tradeoffs,
      recommendation: recommendation,
    ),
  )
}
