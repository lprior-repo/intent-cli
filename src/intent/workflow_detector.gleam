/// Workflow State Detection
/// Provides helpful guidance when command prerequisites are missing
/// Part of user-friendly error handling system
///
/// Design: Railway-Oriented Programming with contextual guidance
/// - Check prerequisites before expensive operations
/// - Provide actionable next steps instead of cryptic errors
/// - Maintain consistency across all workflow-dependent commands
import gleam/list
import gleam/result
import gleam/string
import intent/interview_storage
import intent/loader
import simplifile

// ============================================================================
// Error Types with Guidance
// ============================================================================

/// Workflow state errors with suggested remediation
pub type WorkflowError {
  NoInterviewSessions(profile: String)
  NoSpecFile(path: String)
  InvalidSpec(path: String, reason: String)
  SessionNotFound(session_id: String)
}

// ============================================================================
// Detection Functions
// ============================================================================

/// Check if interview sessions exist
/// Returns Ok if at least one session exists, Error with guidance otherwise
pub fn check_sessions_exist(profile profile: String) -> Result(Nil, WorkflowError) {
  let jsonl_path = ".intent/sessions.jsonl"

  case interview_storage.list_sessions_from_jsonl(jsonl_path) {
    Error(_) -> Error(NoInterviewSessions(profile))
    Ok([]) -> Error(NoInterviewSessions(profile))
    Ok(_sessions) -> Ok(Nil)
  }
}

/// Check if a specific session exists
/// Returns Ok if session found, Error with guidance otherwise
pub fn check_session_exists(session_id: String) -> Result(Nil, WorkflowError) {
  let jsonl_path = ".intent/sessions.jsonl"

  case interview_storage.get_session_from_jsonl(jsonl_path, session_id) {
    Error(_) -> Error(SessionNotFound(session_id))
    Ok(_session) -> Ok(Nil)
  }
}

/// Check if spec file exists
/// Returns Ok if file exists, Error with guidance otherwise
pub fn check_spec_exists(spec_path: String) -> Result(Nil, WorkflowError) {
  case simplifile.verify_is_file(spec_path) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(NoSpecFile(spec_path))
  }
}

/// Check if spec is valid (can be loaded)
/// Returns Ok if spec loads successfully, Error with guidance otherwise
pub fn check_spec_valid(spec_path: String) -> Result(Nil, WorkflowError) {
  // First check if file exists
  case check_spec_exists(spec_path) {
    Error(err) -> Error(err)
    Ok(_) -> {
      // Try to load the spec
      case loader.load_spec_quiet(spec_path) {
        Ok(_spec) -> Ok(Nil)
        Error(err) -> {
          let reason = format_load_error(err)
          Error(InvalidSpec(spec_path, reason))
        }
      }
    }
  }
}

// ============================================================================
// Error Formatting
// ============================================================================

/// Format workflow error with helpful guidance
pub fn format_error(error: WorkflowError) -> String {
  case error {
    NoInterviewSessions(profile) -> {
      let lines = [
        "No interview sessions found.",
        "",
        "Start a new interview session:",
        "  intent interview --profile " <> profile,
        "",
        "Or list existing sessions:",
        "  intent sessions",
      ]
      string.join(lines, "\n")
    }

    SessionNotFound(session_id) -> {
      let lines = [
        "Session not found: " <> session_id,
        "",
        "List available sessions:",
        "  intent sessions",
        "",
        "Or start a new interview:",
        "  intent interview --profile api",
      ]
      string.join(lines, "\n")
    }

    NoSpecFile(path) -> {
      let lines = [
        "Spec file not found: " <> path,
        "",
        "Create a spec using an interview session:",
        "  intent export <session-id> --output " <> path,
        "",
        "Or validate the file path is correct.",
      ]
      string.join(lines, "\n")
    }

    InvalidSpec(path, reason) -> {
      let lines = [
        "Invalid spec file: " <> path,
        "",
        "Error: " <> reason,
        "",
        "Fix the spec or validate it:",
        "  intent validate " <> path,
      ]
      string.join(lines, "\n")
    }
  }
}

/// Format loader error for display
fn format_load_error(error: loader.LoadError) -> String {
  case error {
    loader.FileNotFound(path) -> "File not found: " <> path
    loader.CueValidationFailed(_path, _code, stderr) ->
      "CUE validation failed: " <> stderr
    loader.CueExportFailed(_path, _code, stderr) ->
      "CUE export failed: " <> stderr
    loader.JsonDecodeFailed(errors) -> {
      let error_msgs =
        list.map(errors, fn(err) { string.inspect(err) })
        |> string.join(", ")
      "JSON decode failed: " <> error_msgs
    }
    loader.SpecParseFailed(errors) -> {
      let error_msgs =
        list.map(errors, fn(err) { string.inspect(err) })
        |> string.join(", ")
      "Spec parse failed: " <> error_msgs
    }
    loader.SecurityError(msg) -> "Security error: " <> msg
  }
}

// ============================================================================
// Convenience Combinators
// ============================================================================

/// Chain multiple checks together
/// Returns first error encountered, or Ok if all pass
pub fn check_all(
  checks: List(Result(Nil, WorkflowError)),
) -> Result(Nil, WorkflowError) {
  list.fold(checks, Ok(Nil), fn(acc, check) {
    case acc {
      Error(err) -> Error(err)
      Ok(_) -> check
    }
  })
}

/// Run a check and provide a default error if it fails
pub fn check_or(
  check: Result(a, WorkflowError),
  default_error: WorkflowError,
) -> Result(a, WorkflowError) {
  result.or(check, Error(default_error))
}
