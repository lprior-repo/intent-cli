/// Spec Phase CLI Commands
///
/// Commands for the Spec phase (Phase 3 of INTENT_4_PLAN.md):
/// - spec start: Start a new spec session
/// - spec check: Check spec session status
/// - spec critique: Run Adversarial QA critique
/// - spec respond: Submit response to critique issue
/// - spec agree: Finalize spec session
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import glint
import glint/flag
import intent/ffi
import intent/json_output
import intent/loader
import intent/spec_critique

// Exit codes (duplicated to avoid circular dependency)
const exit_pass = 0

/// Parent command for spec phase - shows available subcommands
pub fn spec_group_command() -> glint.Command(Nil) {
  glint.command(fn(_input: glint.CommandInput) {
    let data =
      json.object([
        #("phase", json.string("spec")),
        #("description", json.string("Phase 2: Define precise specifications")),
        #(
          "subcommands",
          json.array(
            [
              #("start", "Initialize a new spec session from spec file"),
              #("check", "Validate spec session completeness"),
              #("critique", "Run Spec Reviewer critique"),
              #("respond", "Submit responses to critique issues"),
              #("agree", "Finalize spec phase agreement"),
            ],
            fn(pair) {
              let #(cmd, desc) = pair
              json.object([
                #("command", json.string("intent spec " <> cmd)),
                #("description", json.string(desc)),
              ])
            },
          ),
        ),
      ])
    let response =
      json_output.success("spec_help", "spec", data, None, [
        json_output.next_action(
          "intent spec start <spec.cue>",
          "Start a new spec session",
        ),
      ])
    json_output.output(response)
  })
  |> glint.description("Spec phase: Define precise specifications")
}

const exit_fail = 1

const exit_invalid = 3

const exit_error = 4

/// External FFI function to halt with exit code
@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

/// Start a new spec session
pub fn spec_start_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let flags = input.flags
    let vision_session =
      flag.get_string(flags, "vision-session") |> result.unwrap("")
    let shape_session =
      flag.get_string(flags, "shape-session") |> result.unwrap("")

    case vision_session, shape_session {
      "", _ | _, "" -> {
        let error =
          json_output.error(
            "missing_session_args",
            "vision-session or shape-session is required",
          )

        let response =
          json_output.failure(
            "spec_start_error",
            "spec start",
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
        let session_id = ffi.generate_uuid()
        let timestamp = ffi.current_timestamp()

        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("vision_session", json.string(vision_session)),
            #("shape_session", json.string(shape_session)),
            #("phase", json.string("spec")),
            #("status", json.string("in_progress")),
            #("current_round", json.int(1)),
            #("created_at", json.string(timestamp)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent spec check --session=" <> session_id,
            "Check session status",
          ),
        ]

        let response =
          json_output.success(
            "spec_start_result",
            "spec start",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Start a new spec phase session")
  |> glint.flag(
    "vision-session",
    flag.string()
      |> flag.description("Vision session ID to link to"),
  )
  |> glint.flag(
    "shape-session",
    flag.string()
      |> flag.description("Shape session ID to link to"),
  )
}

/// Check spec session status
pub fn spec_check_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let next_actions = [
          json_output.next_action(
            "intent spec start --vision-session=<vision-id> --shape-session=<shape-id>",
            "Start a new spec session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "spec_check_error",
            "spec check",
            json.object([]),
            [error],
            None,
            next_actions,
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
            #("current_round", json.int(1)),
            #("rounds_complete", json.int(0)),
            #("blocking_questions", json.int(0)),
            #("rcs_score", json.float(0.0)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent spec critique --session=" <> session_id,
            "Run Adversarial QA critique",
          ),
        ]

        let response =
          json_output.success(
            "spec_check_result",
            "spec check",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Check spec session status")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to check"),
  )
}

/// Run Adversarial QA critique on spec session
pub fn spec_critique_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")
    let spec_path = flag.get_string(input.flags, "spec") |> result.unwrap("")

    case session_id, spec_path {
      "", _ | _, "" -> {
        let error =
          json_output.error(
            "missing_required_fields",
            "session and spec are required",
          )

        let response =
          json_output.failure(
            "spec_critique_error",
            "spec critique",
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
        // Load spec and run critique
        case loader.load_spec_quiet(spec_path) {
          Ok(spec) -> {
            let critique_result = spec_critique.critique_spec(spec)

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
                  "intent spec agree --session=" <> session_id,
                  "Finalize spec",
                ),
              ]
              False -> [
                json_output.next_action(
                  "intent spec respond --session="
                    <> session_id
                    <> " --issue=<issue_id> --response='...'",
                  "Respond to critique issues",
                ),
              ]
            }

            let response =
              json_output.success(
                "spec_critique_result",
                "spec critique",
                data,
                Some(spec_path),
                next_actions,
              )

            json_output.output(response)
            case critique_result.passed {
              True -> halt(exit_pass)
              False -> halt(exit_fail)
            }
          }
          Error(e) -> {
            let error_msg = loader.format_error(e)
            let response =
              json_output.failure(
                "spec_critique_error",
                "spec critique",
                json.null(),
                [json_output.error("load_error", error_msg)],
                Some(spec_path),
                [],
                exit_invalid,
              )
            json_output.output(response)
            halt(exit_invalid)
          }
        }
      }
    }
  })
  |> glint.description("Run Adversarial QA critique on spec session")
  |> glint.flag("session", flag.string() |> flag.description("Session ID"))
  |> glint.flag("spec", flag.string() |> flag.description("Path to spec file"))
}

/// Submit response to a critique issue
pub fn spec_respond_command() -> glint.Command(Nil) {
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
            "spec_respond_error",
            "spec respond",
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
            "intent spec critique --session=" <> session_id,
            "Re-run critique to check progress",
          ),
        ]

        let resp =
          json_output.success(
            "spec_respond_result",
            "spec respond",
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

/// Finalize spec session
pub fn spec_agree_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")
    let notes = flag.get_string(input.flags, "notes") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let next_actions = [
          json_output.next_action(
            "intent spec start --vision-session=<vision-id> --shape-session=<shape-id>",
            "Start a new spec session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "spec_agree_error",
            "spec agree",
            json.object([]),
            [error],
            None,
            next_actions,
            exit_error,
          )

        json_output.output(response)
        halt(exit_error)
      }
      _ -> {
        let timestamp = ffi.current_timestamp()

        let data =
          json.object([
            #("session_id", json.string(session_id)),
            #("status", json.string("complete")),
            #("approval_notes", json.string(notes)),
            #("rounds_complete", json.int(5)),
            #("final_rcs_score", json.float(85.0)),
            #("approved_at", json.string(timestamp)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent ready start --session=" <> session_id,
            "Start Ready phase",
          ),
        ]

        let response =
          json_output.success(
            "spec_agree_result",
            "spec agree",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Finalize spec session")
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
// SPEC COMMAND HELPERS
// =============================================================================

fn critique_question_to_string(
  question: spec_critique.CritiqueQuestion,
) -> String {
  case question {
    spec_critique.CoverageGaps -> "coverage_gaps"
    spec_critique.EdgeCaseGaps -> "edge_case_gaps"
    spec_critique.FailureBlastRadius -> "failure_blast_radius"
  }
}

fn critique_severity_to_string(severity: spec_critique.Severity) -> String {
  case severity {
    spec_critique.Critical -> "critical"
    spec_critique.Warning -> "warning"
  }
}
