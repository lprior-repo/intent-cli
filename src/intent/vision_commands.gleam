/// Vision Commands Module
/// State management for Vision phase CLI commands (Phase 1 of INTENT_4_PLAN.md)
/// Follows Functional Core pattern - all functions are pure
///
/// Commands:
/// - vision start: Start a new vision session
/// - vision check: Check vision session status
/// - vision critique: Run Skeptical PM critique
/// - vision respond: Submit response to critique issue
/// - vision agree: Finalize vision session
import gleam/dict.{type Dict}
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import intent/vision_critique.{type CritiqueResult, critique_vision}
import intent/vision_types.{
  type Scenario, type VisionSection, Scenario, VisionSection,
}

// =============================================================================
// Types - Vision Command Session
// =============================================================================

/// Vision command session status
pub type VisionCommandStatus {
  CommandInProgress
  CommandReadyForCritique
  CommandComplete
}

/// Response to a critique issue
pub type CritiqueResponse {
  CritiqueResponse(issue_id: String, response_text: String, timestamp: String)
}

/// Vision command session - persistent state for vision phase commands
pub type VisionCommandSession {
  VisionCommandSession(
    id: String,
    profile: String,
    created_at: String,
    updated_at: String,
    status: VisionCommandStatus,
    critique_score: Int,
    responses: Dict(String, CritiqueResponse),
    issues_addressed: Int,
    finalized_at: Option(String),
  )
}

// =============================================================================
// Session Management Functions
// =============================================================================

/// Create a new vision command session
pub fn create_command_session(
  id: String,
  profile: String,
  created_at: String,
) -> VisionCommandSession {
  VisionCommandSession(
    id: id,
    profile: profile,
    created_at: created_at,
    updated_at: created_at,
    status: CommandInProgress,
    critique_score: 0,
    responses: dict.new(),
    issues_addressed: 0,
    finalized_at: None,
  )
}

/// Get status as string for JSON output
pub fn get_status_string(status: VisionCommandStatus) -> String {
  case status {
    CommandInProgress -> "in_progress"
    CommandReadyForCritique -> "ready_for_critique"
    CommandComplete -> "complete"
  }
}

/// Record a response to a critique issue
pub fn record_response(
  session: VisionCommandSession,
  issue_id: String,
  response_text: String,
  timestamp: String,
) -> VisionCommandSession {
  let response =
    CritiqueResponse(
      issue_id: issue_id,
      response_text: response_text,
      timestamp: timestamp,
    )

  VisionCommandSession(
    ..session,
    responses: dict.insert(session.responses, issue_id, response),
    updated_at: timestamp,
  )
}

/// Set session status to ready for critique
pub fn set_status_ready_for_critique(
  session: VisionCommandSession,
  timestamp: String,
) -> VisionCommandSession {
  VisionCommandSession(
    ..session,
    status: CommandReadyForCritique,
    updated_at: timestamp,
  )
}

/// Set session status to complete with score
pub fn set_status_complete(
  session: VisionCommandSession,
  score: Int,
  issues_addressed: Int,
  timestamp: String,
) -> VisionCommandSession {
  VisionCommandSession(
    ..session,
    status: CommandComplete,
    critique_score: score,
    issues_addressed: issues_addressed,
    updated_at: timestamp,
  )
}

/// Agree/finalize a session (requires complete status and passing score)
pub fn agree_session(
  session: VisionCommandSession,
  _notes: String,
  timestamp: String,
) -> Result(VisionCommandSession, String) {
  case session.status {
    CommandComplete -> {
      case session.critique_score >= 70 {
        True ->
          Ok(
            VisionCommandSession(
              ..session,
              finalized_at: Some(timestamp),
              updated_at: timestamp,
            ),
          )
        False ->
          Error(
            "Critique score must be >= 70 to finalize vision (current: "
            <> int.to_string(session.critique_score)
            <> ")",
          )
      }
    }
    _ -> Error("Session must be complete before finalizing")
  }
}

// =============================================================================
// JSON Serialization
// =============================================================================

/// Convert session to JSON
pub fn session_to_json(session: VisionCommandSession) -> json.Json {
  json.object([
    #("id", json.string(session.id)),
    #("profile", json.string(session.profile)),
    #("created_at", json.string(session.created_at)),
    #("updated_at", json.string(session.updated_at)),
    #("status", json.string(get_status_string(session.status))),
    #("critique_score", json.int(session.critique_score)),
    #("issues_addressed", json.int(session.issues_addressed)),
    #("responses_count", json.int(dict.size(session.responses))),
    #("finalized_at", option_to_json(session.finalized_at)),
  ])
}

fn option_to_json(opt: Option(String)) -> json.Json {
  case opt {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

// =============================================================================
// Critique Functions
// =============================================================================

/// Create an empty vision section for testing critique
pub fn empty_vision_section() -> VisionSection {
  VisionSection(
    press_release: "",
    persona: "",
    non_personas: [],
    north_star: "",
    scenarios: [],
    replaces: None,
    vorp: "",
    out_of_scope: [],
  )
}

/// Create a sample complete vision section for testing
pub fn sample_complete_vision() -> VisionSection {
  VisionSection(
    press_release: "Intent CLI enables API developers to write contract-driven tests with 10x faster feedback loops. No more guessing if your API changes break clients.",
    persona: "Backend engineers building microservices with REST APIs who need to verify API contracts before deployment",
    non_personas: ["Frontend-only developers", "Database administrators"],
    north_star: "A developer writes a single CUE spec and gets immediate validation of API behavior, including edge cases, error handling, and contract compliance - all before deployment.",
    scenarios: [
      Scenario(
        character: "Sarah",
        persona: "Backend Engineer",
        motivation: "Verify API contract compliance",
        simulation: "Sarah runs intent check api.cue and sees immediate validation results",
        outcome: "API deployment confidence increased from 60% to 95%",
      ),
      Scenario(
        character: "Mike",
        persona: "API Team Lead",
        motivation: "Reduce API regression bugs",
        simulation: "Mike adds intent to CI pipeline, catches breaking changes before merge",
        outcome: "API regression bugs reduced by 80%",
      ),
    ],
    replaces: Some("Manual API testing with curl/Postman + custom scripts"),
    vorp: "10x faster API validation: Complete contract verification in 30 seconds vs 4+ hours of manual testing. 100x fewer escaped defects through automated edge case testing.",
    out_of_scope: ["Database testing", "UI testing", "Load testing"],
  )
}

/// Run critique on a vision section
pub fn run_critique(vision: VisionSection) -> CritiqueResult {
  critique_vision(vision)
}
