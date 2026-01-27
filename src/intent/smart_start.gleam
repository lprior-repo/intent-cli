//// Smart Start Module
//// Auto-detect and resume sessions when intent runs with no args
////
//// Logic:
//// 1. Check .intent/sessions.jsonl for existing sessions
//// 2. If one incomplete session exists, auto-resume it
//// 3. If multiple or none, start new interview with default 'api' profile
//// 4. Only show help on explicit --help flag

import gleam/list
import intent/interview.{type InterviewSession, type Profile, Api, Complete}
import intent/interview_storage.{
  type FileReader, list_sessions_from_jsonl_with_io,
}

/// Action to take when starting intent
pub type StartAction {
  /// Resume an existing incomplete session
  Resume(session_id: String)

  /// Start a new interview with the given profile
  StartNew(profile: Profile)
}

/// Determine what action to take when intent starts
/// This is the main entry point for smart start functionality
pub fn determine_start_action(
  sessions_path: String,
  reader: FileReader,
) -> StartAction {
  // Try to read existing sessions
  case list_sessions_from_jsonl_with_io(sessions_path, reader) {
    Ok(sessions) -> {
      let incomplete = filter_incomplete_sessions(sessions)

      case incomplete {
        // One incomplete session: resume it
        [session] -> Resume(session.id)

        // Zero or multiple incomplete: start new
        _ -> StartNew(Api)
      }
    }
    // On any error, start new interview (fail gracefully)
    Error(_) -> StartNew(Api)
  }
}

/// Check if a session is complete
/// A session is complete if it's in the Complete stage OR has a completed_at timestamp
pub fn is_session_complete(session: InterviewSession) -> Bool {
  case session.stage {
    Complete -> True
    _ -> {
      case session.completed_at {
        "" -> False
        _ -> True
      }
    }
  }
}

/// Filter a list of sessions to only include incomplete ones
pub fn filter_incomplete_sessions(
  sessions: List(InterviewSession),
) -> List(InterviewSession) {
  list.filter(sessions, fn(session) { !is_session_complete(session) })
}
