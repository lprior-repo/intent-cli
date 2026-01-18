/// Session Handoff Module
/// Provides context preservation for pausing and resuming interview sessions
/// Separates handoff logic from general session storage
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import intent/interview.{
  type InterviewSession, type InterviewStage, type Profile,
}
import simplifile

// =============================================================================
// Session Handoff Type
// =============================================================================

/// Session handoff - captures context for pausing and resuming interviews
pub type SessionHandoff {
  SessionHandoff(
    session_id: String,
    // Reference to session being paused
    handoff_id: String,
    // Unique ID: session_id + timestamp
    created_at: String,
    // When handoff was created
    profile: String,
    // Profile for quick lookup
    pause_reason: String,
    // Why session was paused (user choice, timeout, etc)
    stage: String,
    // Current interview stage
    rounds_completed: Int,
    // How many rounds fully completed
    current_round: Int,
    // Which round resuming into
    questions_answered: Int,
    // Total answers collected so far
    gaps_unresolved: Int,
    // Count of unresolved gaps
    conflicts_unresolved: Int,
    // Count of unresolved conflicts
    next_question_id: String,
    // ID of next question to ask
    mental_model_indicators: Dict(String, String),
    // Round completion % ("round_1" -> "100%", etc)
    key_assumptions: List(String),
    // Assumptions made that user should review
    full_session_jsonl: String,
    // Complete session encoded as JSONL for restoration
  )
}

// =============================================================================
// Pure Functions - Handoff Creation and Manipulation
// =============================================================================

/// Calculate mental model progress indicators (pure)
pub fn calculate_mental_model_indicators(
  session: InterviewSession,
) -> Dict(String, String) {
  let total_rounds = 5
  let mut_dict = dict.new()

  // Mark completed rounds
  let mut_dict = case session.rounds_completed {
    r if r >= 1 ->
      dict.insert(mut_dict, "round_1_ears", "100%")
      |> dict.insert("round_1_status", "Complete")
    _ ->
      dict.insert(mut_dict, "round_1_status", "Pending")
  }

  let mut_dict = case session.rounds_completed {
    r if r >= 2 ->
      dict.insert(mut_dict, "round_2_contracts", "100%")
      |> dict.insert("round_2_status", "Complete")
    _ ->
      dict.insert(mut_dict, "round_2_status", "Pending")
  }

  let mut_dict = case session.rounds_completed {
    r if r >= 3 ->
      dict.insert(mut_dict, "round_3_inversion", "100%")
      |> dict.insert("round_3_status", "Complete")
    _ ->
      dict.insert(mut_dict, "round_3_status", "Pending")
  }

  let mut_dict = case session.rounds_completed {
    r if r >= 4 ->
      dict.insert(mut_dict, "round_4_effects", "100%")
      |> dict.insert("round_4_status", "Complete")
    _ ->
      dict.insert(mut_dict, "round_4_status", "Pending")
  }

  let mut_dict = case session.rounds_completed {
    r if r >= 5 ->
      dict.insert(mut_dict, "round_5_premortem", "100%")
      |> dict.insert("round_5_status", "Complete")
    _ ->
      dict.insert(mut_dict, "round_5_status", "Pending")
  }

  dict.insert(
    mut_dict,
    "total_progress",
    string.inspect(session.rounds_completed)
    <> "/"
    <> string.inspect(total_rounds),
  )
}

/// Extract key assumptions from session for user review (pure)
pub fn extract_key_assumptions(session: InterviewSession) -> List(String) {
  let mut_assumptions = []

  // Extract from gaps
  let gap_assumptions =
    list.map(session.gaps, fn(gap) {
      "Gap: " <> gap.field <> " - " <> gap.why_needed
    })
  let mut_assumptions = list.append(mut_assumptions, gap_assumptions)

  // Extract from conflicts (unresolved ones highlight assumptions)
  let conflict_assumptions =
    list.filter_map(session.conflicts, fn(conflict) {
      case conflict.chosen {
        -1 ->
          Ok("Unresolved conflict: " <> conflict.description)
        _ ->
          Error(Nil)
      }
    })
  let mut_assumptions = list.append(mut_assumptions, conflict_assumptions)

  // Add stage-based assumptions
  let stage_assumption = case session.stage {
    interview.Discovery ->
      "In Discovery phase - focusing on core intent understanding"
    interview.Refinement ->
      "In Refinement phase - scope and boundary clarification"
    interview.Validation ->
      "In Validation phase - error cases and edge case coverage"
    interview.Complete ->
      "Interview marked as Complete"
    interview.Paused ->
      "Interview was paused mid-session"
  }
  list.append(mut_assumptions, [stage_assumption])
}

/// Profile to string for serialization
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

/// Stage to string for serialization
fn stage_to_string(stage: InterviewStage) -> String {
  case stage {
    interview.Discovery -> "discovery"
    interview.Refinement -> "refinement"
    interview.Validation -> "validation"
    interview.Complete -> "complete"
    interview.Paused -> "paused"
  }
}

/// Create a session handoff for pause context (pure)
pub fn create_handoff(
  session: InterviewSession,
  pause_reason: String,
  next_question_id: String,
  timestamp: String,
  session_jsonl_line: String,
) -> SessionHandoff {
  let unresolved_gaps =
    list.filter(session.gaps, fn(g) { !g.resolved })
  let unresolved_conflicts =
    list.filter(session.conflicts, fn(c) { c.chosen < 0 })

  SessionHandoff(
    session_id: session.id,
    handoff_id: session.id <> "-handoff-" <> timestamp,
    created_at: timestamp,
    profile: profile_to_string(session.profile),
    pause_reason: pause_reason,
    stage: stage_to_string(session.stage),
    rounds_completed: session.rounds_completed,
    current_round: interview.get_current_round(session),
    questions_answered: list.length(session.answers),
    gaps_unresolved: list.length(unresolved_gaps),
    conflicts_unresolved: list.length(unresolved_conflicts),
    next_question_id: next_question_id,
    mental_model_indicators: calculate_mental_model_indicators(session),
    key_assumptions: extract_key_assumptions(session),
    full_session_jsonl: session_jsonl_line,
  )
}

// =============================================================================
// Serialization Functions
// =============================================================================

/// Encode handoff to JSON (pure)
pub fn handoff_to_json(handoff: SessionHandoff) -> json.Json {
  json.object([
    #("session_id", json.string(handoff.session_id)),
    #("handoff_id", json.string(handoff.handoff_id)),
    #("created_at", json.string(handoff.created_at)),
    #("profile", json.string(handoff.profile)),
    #("pause_reason", json.string(handoff.pause_reason)),
    #("stage", json.string(handoff.stage)),
    #("rounds_completed", json.int(handoff.rounds_completed)),
    #("current_round", json.int(handoff.current_round)),
    #("questions_answered", json.int(handoff.questions_answered)),
    #("gaps_unresolved", json.int(handoff.gaps_unresolved)),
    #("conflicts_unresolved", json.int(handoff.conflicts_unresolved)),
    #("next_question_id", json.string(handoff.next_question_id)),
    #(
      "mental_model_indicators",
      json.object(
        dict.to_list(handoff.mental_model_indicators)
        |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) }),
      ),
    ),
    #("key_assumptions", json.array(handoff.key_assumptions, json.string)),
    #("full_session_jsonl", json.string(handoff.full_session_jsonl)),
  ])
}

/// Encode handoff to JSONL line (pure)
pub fn handoff_to_jsonl_line(handoff: SessionHandoff) -> String {
  handoff
  |> handoff_to_json
  |> json.to_string
}

/// Parse JSONL lines into handoffs (pure)
pub fn parse_handoff_lines(lines: List(String)) -> List(SessionHandoff) {
  list.filter_map(lines, fn(line) {
    case string.length(string.trim(line)) {
      0 -> Error(Nil)
      _ ->
        json.decode(line, handoff_decoder())
        |> result.map_error(fn(_) { Nil })
    }
  })
}

// =============================================================================
// Decoder for JSON Parsing
// =============================================================================

fn handoff_decoder() -> dynamic.Decoder(SessionHandoff) {
  fn(dyn) {
    let decode_mental_model = dynamic.dict(dynamic.string, dynamic.string)
    use session_id <- result.try(dynamic.field("session_id", dynamic.string)(dyn))
    use handoff_id <- result.try(dynamic.field("handoff_id", dynamic.string)(dyn))
    use created_at <- result.try(dynamic.field("created_at", dynamic.string)(dyn))
    use profile <- result.try(dynamic.field("profile", dynamic.string)(dyn))
    use pause_reason <- result.try(dynamic.field("pause_reason", dynamic.string)(dyn))
    use stage <- result.try(dynamic.field("stage", dynamic.string)(dyn))
    use rounds_completed <- result.try(dynamic.field("rounds_completed", dynamic.int)(dyn))
    use current_round <- result.try(dynamic.field("current_round", dynamic.int)(dyn))
    use questions_answered <- result.try(dynamic.field("questions_answered", dynamic.int)(dyn))
    use gaps_unresolved <- result.try(dynamic.field("gaps_unresolved", dynamic.int)(dyn))
    use conflicts_unresolved <- result.try(dynamic.field("conflicts_unresolved", dynamic.int)(dyn))
    use next_question_id <- result.try(dynamic.field("next_question_id", dynamic.string)(dyn))
    use mental_model_indicators <- result.try(dynamic.field("mental_model_indicators", decode_mental_model)(dyn))
    use key_assumptions <- result.try(dynamic.field("key_assumptions", dynamic.list(dynamic.string))(dyn))
    use full_session_jsonl <- result.try(dynamic.field("full_session_jsonl", dynamic.string)(dyn))
    Ok(SessionHandoff(
      session_id: session_id,
      handoff_id: handoff_id,
      created_at: created_at,
      profile: profile,
      pause_reason: pause_reason,
      stage: stage,
      rounds_completed: rounds_completed,
      current_round: current_round,
      questions_answered: questions_answered,
      gaps_unresolved: gaps_unresolved,
      conflicts_unresolved: conflicts_unresolved,
      next_question_id: next_question_id,
      mental_model_indicators: mental_model_indicators,
      key_assumptions: key_assumptions,
      full_session_jsonl: full_session_jsonl,
    ))
  }
}

// =============================================================================
// I/O Operations - File Persistence
// =============================================================================

/// Ensure parent directory exists (I/O only)
fn ensure_parent_directory(file_path: String) -> Result(Nil, String) {
  let parts = string.split(file_path, "/")
  let dir_parts = list.take(parts, list.length(parts) - 1)
  case list.length(dir_parts) {
    0 -> Ok(Nil)
    _ -> {
      let dir_path = string.join(dir_parts, "/")
      simplifile.create_directory_all(dir_path)
      |> result.map_error(fn(err) {
        "Failed to create directory '" <> dir_path <> "': " <> string.inspect(err)
      })
    }
  }
}

/// Read handoffs file (I/O only)
/// Returns empty string if file doesn't exist, propagates other errors
fn read_handoffs_file(handoffs_path: String) -> Result(String, String) {
  case simplifile.read(handoffs_path) {
    Ok(content) -> Ok(content)
    Error(simplifile.Enoent) -> Ok("")
    Error(err) ->
      Error(
        "Failed to read handoffs from '"
        <> handoffs_path
        <> "': "
        <> string.inspect(err),
      )
  }
}

/// Write handoffs file (I/O only)
fn write_handoffs_file(handoffs_path: String, content: String) -> Result(Nil, String) {
  simplifile.write(handoffs_path, content)
  |> result.map_error(fn(err) {
    "Failed to write handoffs to '" <> handoffs_path <> "': " <> string.inspect(err)
  })
}

/// Append a handoff to handoffs JSONL (orchestrates pure + I/O)
pub fn append_handoff_to_jsonl(
  handoff: SessionHandoff,
  handoffs_path: String,
) -> Result(Nil, String) {
  let line = handoff_to_jsonl_line(handoff)

  use existing <- result.try(read_handoffs_file(handoffs_path))
  use _ <- result.try(ensure_parent_directory(handoffs_path))

  let content = case string.length(string.trim(existing)) {
    0 -> line
    _ -> existing <> "\n" <> line
  }

  write_handoffs_file(handoffs_path, content)
}

/// List all handoffs from JSONL file (orchestrates pure + I/O)
pub fn list_handoffs_from_jsonl(
  handoffs_path: String,
) -> Result(List(SessionHandoff), String) {
  use content <- result.try(read_handoffs_file(handoffs_path))

  case string.length(string.trim(content)) {
    0 -> Ok([])
    _ -> {
      let lines = string.split(content, "\n")
      Ok(parse_handoff_lines(lines))
    }
  }
}

/// Get latest handoff for a session (orchestrates pure + I/O)
pub fn get_latest_handoff_for_session(
  handoffs_path: String,
  session_id: String,
) -> Result(SessionHandoff, String) {
  use handoffs <- result.try(list_handoffs_from_jsonl(handoffs_path))

  handoffs
  |> list.filter(fn(h) { h.session_id == session_id })
  |> list.last
  |> result.map_error(fn(_) { "No handoff found for session: " <> session_id })
}

/// List all handoffs for a session (orchestrates pure + I/O)
pub fn list_handoffs_for_session(
  handoffs_path: String,
  session_id: String,
) -> Result(List(SessionHandoff), String) {
  use handoffs <- result.try(list_handoffs_from_jsonl(handoffs_path))

  Ok(list.filter(handoffs, fn(h) { h.session_id == session_id }))
}
