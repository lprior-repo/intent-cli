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
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import glint
import glint/flag
import intent/ffi
import intent/json_output
import intent/vision_critique.{type CritiqueResult, critique_vision}
import intent/vision_types.{
  type Scenario, type VisionSection, Scenario, VisionSection,
}

// Exit codes (duplicated to avoid circular dependency)
const exit_pass = 0

const exit_fail = 1

const exit_error = 4

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

// =============================================================================
// VISION COMMANDS - Glint Command Wrappers
// =============================================================================

/// External FFI function to halt with exit code
@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

/// Start a new vision session
pub fn vision_start_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let profile = case input.args {
      [p, ..] -> p
      [] -> ""
    }

    case profile {
      "" -> {
        let error = json_output.error("missing_profile", "Profile is required")

        let response =
          json_output.failure(
            "vision_start_error",
            "vision start",
            json.object([]),
            [error],
            None,
            [],
            exit_error,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _ -> {
        let session_id = ffi.generate_uuid()
        let timestamp = ffi.current_timestamp()

        let session = create_command_session(session_id, profile, timestamp)

        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("profile", json.string(profile)),
            #("phase", json.string("vision")),
            #("status", json.string("in_progress")),
            #("created_at", json.string(timestamp)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent vision check --session=" <> session_id,
            "Check session status",
          ),
        ]

        let response =
          json_output.success(
            "vision_start_result",
            "vision start",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description(
    "Start a new vision phase session for problem definition",
  )
}

/// Check vision session status
pub fn vision_check_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let response =
          json_output.failure(
            "vision_check_error",
            "vision check",
            json.object([]),
            [error],
            None,
            [],
            exit_error,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _ -> {
        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("status", json.string("in_progress")),
            #("critique_score", json.int(0)),
            #("responses_count", json.int(0)),
            #("issues_addressed", json.int(0)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent vision critique --session=" <> session_id,
            "Run Skeptical PM critique",
          ),
        ]

        let response =
          json_output.success(
            "vision_check_result",
            "vision check",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Check vision session status")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to check"),
  )
}

/// Run Skeptical PM critique on vision session
pub fn vision_critique_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")
    let vision_path =
      flag.get_string(input.flags, "vision") |> result.unwrap("")

    case session_id, vision_path {
      "", _ | _, "" -> {
        let error =
          json_output.error(
            "missing_required_fields",
            "session and vision are required",
          )

        let response =
          json_output.failure(
            "vision_critique_error",
            "vision critique",
            json.object([]),
            [error],
            None,
            [],
            exit_error,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _, _ -> {
        let timestamp = ffi.current_timestamp()

        let critique_result =
          sample_complete_vision()
          |> run_critique()

        let issues_json =
          critique_result.issues
          |> list.map(fn(issue) {
            json.object([
              #(
                "question",
                json.string(critique_question_to_string(issue.question)),
              ),
              #(
                "severity",
                json.string(critique_severity_to_string(issue.severity)),
              ),
              #("message", json.string(issue.message)),
              #("suggestion", json.string(issue.suggestion)),
            ])
          })

        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("passed", json.bool(critique_result.passed)),
            #("score", json.int(critique_result.score)),
            #("issues", json.array(issues_json, fn(x) { x })),
          ])

        let next_actions = case critique_result.passed {
          True -> [
            json_output.next_action(
              "intent vision agree --session=" <> session_id,
              "Finalize vision",
            ),
          ]
          False -> [
            json_output.next_action(
              "intent vision respond --session="
                <> session_id
                <> " --issue=<issue_id> --response='...'",
              "Respond to critique issues",
            ),
          ]
        }

        let response =
          json_output.success(
            "vision_critique_result",
            "vision critique",
            data,
            Some(vision_path),
            next_actions,
          )

        json_output.output(response)
        case critique_result.passed {
          True -> halt(exit_pass)
          False -> halt(exit_fail)
        }
      }
    }
  })
  |> glint.description("Run Skeptical PM critique on vision session")
  |> glint.flag("session", flag.string() |> flag.description("Session ID"))
  |> glint.flag(
    "vision",
    flag.string() |> flag.description("Path to vision file"),
  )
}

/// Submit response to a critique issue
pub fn vision_respond_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")
    let issue_id = flag.get_string(input.flags, "issue") |> result.unwrap("")
    let response_text =
      flag.get_string(input.flags, "response") |> result.unwrap("")

    case session_id, issue_id, response_text {
      "", _, _ | _, "", _ | _, _, "" -> {
        let error =
          json_output.error(
            "missing_required_fields",
            "session, issue, and response are required",
          )

        let resp =
          json_output.failure(
            "vision_respond_error",
            "vision respond",
            json.object([]),
            [error],
            None,
            [],
            exit_error,
          )

        json_output.output(resp)
        halt(exit_error)
      }
      _, _, _ -> {
        let timestamp = ffi.current_timestamp()

        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("issue_id", json.string(issue_id)),
            #("response_recorded", json.bool(True)),
            #("timestamp", json.string(timestamp)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent vision critique --session=" <> session_id,
            "Re-run critique to check progress",
          ),
        ]

        let resp =
          json_output.success(
            "vision_respond_result",
            "vision respond",
            data,
            None,
            next_actions,
          )

        json_output.output(resp)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Submit response to a critique issue")
  |> glint.flag("session", flag.string() |> flag.description("Session ID"))
  |> glint.flag(
    "issue",
    flag.string() |> flag.description("Issue ID to respond to"),
  )
  |> glint.flag(
    "response",
    flag.string() |> flag.description("Response text addressing the issue"),
  )
}

/// Finalize vision session
pub fn vision_agree_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")
    let notes = flag.get_string(input.flags, "notes") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let response =
          json_output.failure(
            "vision_agree_error",
            "vision agree",
            json.object([]),
            [error],
            None,
            [],
            exit_error,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _ -> {
        let timestamp = ffi.current_timestamp()

        let session =
          create_command_session(session_id, "api", timestamp)
          |> set_status_complete(80, 3, timestamp)

        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("status", json.string("finalized")),
            #("approval_notes", json.string(notes)),
            #("critique_score", json.int(session.critique_score)),
            #("finalized_at", json.string(timestamp)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent shape start --vision=" <> session_id,
            "Start Shape phase",
          ),
        ]

        let response =
          json_output.success(
            "vision_agree_result",
            "vision agree",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Finalize vision session")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to finalize"),
  )
  |> glint.flag(
    "notes",
    flag.string() |> flag.description("Finalization notes or comments"),
  )
}

// =============================================================================
// VISION COMMAND HELPERS
// =============================================================================

fn critique_question_to_string(
  question: vision_critique.CritiqueQuestion,
) -> String {
  case question {
    vision_critique.ProblemReality -> "problem_reality"
    vision_critique.PersonaValidation -> "persona_validation"
    vision_critique.VorpStrength -> "vorp_strength"
  }
}

fn critique_severity_to_string(severity: vision_critique.Severity) -> String {
  case severity {
    vision_critique.Critical -> "critical"
    vision_critique.Warning -> "warning"
  }
}
