/// Ready Session Management
/// State management for Ready phase (Phase 4 of INTENT_4_PLAN.md)
/// Follows Functional Core pattern - all functions are pure
import gleam/dict.{type Dict}
import gleam/json.{type Json}

/// Ready session status
pub type ReadyStatus {
  InProgress
  ReadyForCritique
  Complete
  Approved
}

/// A response to a critique issue
pub type CritiqueResponse {
  CritiqueResponse(issue_id: String, response: String, timestamp: String)
}

/// Ready session - persistent state for ready phase
pub type ReadySession {
  ReadySession(
    id: String,
    spec_path: String,
    created_at: String,
    updated_at: String,
    status: ReadyStatus,
    critique_score: Int,
    responses: Dict(String, CritiqueResponse),
    blockers_resolved: Int,
    approval_notes: String,
  )
}

/// Minimum score required to approve for launch
const min_approval_score = 70

/// Create a new ready session
pub fn create_session(
  id: String,
  spec_path: String,
  created_at: String,
) -> ReadySession {
  ReadySession(
    id: id,
    spec_path: spec_path,
    created_at: created_at,
    updated_at: created_at,
    status: InProgress,
    critique_score: 0,
    responses: dict.new(),
    blockers_resolved: 0,
    approval_notes: "",
  )
}

/// Get status as string for JSON output
pub fn get_status_string(status: ReadyStatus) -> String {
  case status {
    InProgress -> "in_progress"
    ReadyForCritique -> "ready_for_critique"
    Complete -> "complete"
    Approved -> "approved"
  }
}

/// Record a response to a critique issue
pub fn record_response(
  session: ReadySession,
  issue_id: String,
  response: String,
  timestamp: String,
) -> ReadySession {
  let critique_response =
    CritiqueResponse(
      issue_id: issue_id,
      response: response,
      timestamp: timestamp,
    )

  ReadySession(
    ..session,
    responses: dict.insert(session.responses, issue_id, critique_response),
    updated_at: timestamp,
  )
}

/// Set status to ready for critique
pub fn set_status_ready_for_critique(
  session: ReadySession,
  timestamp: String,
) -> ReadySession {
  ReadySession(..session, status: ReadyForCritique, updated_at: timestamp)
}

/// Set status to complete with score
pub fn set_status_complete(
  session: ReadySession,
  critique_score: Int,
  blockers_resolved: Int,
  timestamp: String,
) -> ReadySession {
  ReadySession(
    ..session,
    status: Complete,
    critique_score: critique_score,
    blockers_resolved: blockers_resolved,
    updated_at: timestamp,
  )
}

/// Approve session for launch
/// Returns Error if session is not complete or score is below threshold
pub fn approve_session(
  session: ReadySession,
  notes: String,
  timestamp: String,
) -> Result(ReadySession, String) {
  case session.status {
    Complete ->
      case session.critique_score >= min_approval_score {
        True ->
          Ok(
            ReadySession(
              ..session,
              status: Approved,
              approval_notes: notes,
              updated_at: timestamp,
            ),
          )
        False ->
          Error(
            "Critique score ("
            <> int_to_string(session.critique_score)
            <> ") is below minimum threshold ("
            <> int_to_string(min_approval_score)
            <> ")",
          )
      }
    _ -> Error("Session must be in Complete status to approve")
  }
}

/// Convert session to JSON for output
pub fn session_to_json(session: ReadySession) -> Json {
  json.object([
    #("id", json.string(session.id)),
    #("spec_path", json.string(session.spec_path)),
    #("status", json.string(get_status_string(session.status))),
    #("created_at", json.string(session.created_at)),
    #("updated_at", json.string(session.updated_at)),
    #("critique_score", json.int(session.critique_score)),
    #("responses_count", json.int(dict.size(session.responses))),
    #("blockers_resolved", json.int(session.blockers_resolved)),
    #("approval_notes", json.string(session.approval_notes)),
  ])
}

// Helper to convert int to string (avoiding circular import)
fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    _ -> do_int_to_string(n, "")
  }
}

fn do_int_to_string(n: Int, acc: String) -> String {
  case n {
    0 -> acc
    _ -> {
      let digit = n % 10
      let char = case digit {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        _ -> "9"
      }
      do_int_to_string(n / 10, char <> acc)
    }
  }
}
