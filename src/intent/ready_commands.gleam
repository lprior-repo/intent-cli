/// Ready Phase CLI Commands
///
/// Commands for the Ready phase (Phase 4 of INTENT_4_PLAN.md):
/// - ready start: Start a new ready session
/// - ready check: Check ready session status
/// - ready critique: Run Pre-Launch Auditor critique
/// - ready respond: Submit response to critique issue
/// - ready agree: Finalize ready session and approve for launch
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import glint
import glint/flag
import intent/ffi
import intent/json_output
import intent/kirk/ready
import intent/loader
import intent/planning_types.{type ReadyReport}
import intent/ready_critique

// Exit codes (duplicated to avoid circular dependency)
const exit_pass = 0

/// Parent command for ready phase - shows available subcommands
pub fn ready_group_command() -> glint.Command(Nil) {
  glint.command(fn(_input: glint.CommandInput) {
    let data =
      json.object([
        #("phase", json.string("ready")),
        #("description", json.string("Phase 4: Pre-launch readiness check")),
        #(
          "subcommands",
          json.array(
            [
              #("start", "Initialize a new ready session"),
              #("check", "Validate ready session completeness"),
              #("critique", "Run Pre-Launch Auditor critique"),
              #("respond", "Submit responses to critique issues"),
              #("agree", "Finalize ready phase and approve launch"),
            ],
            fn(pair) {
              let #(cmd, desc) = pair
              json.object([
                #("command", json.string("intent ready " <> cmd)),
                #("description", json.string(desc)),
              ])
            },
          ),
        ),
      ])
    let response =
      json_output.success("ready_help", "ready", data, None, [
        json_output.next_action(
          "intent ready start <spec.cue>",
          "Start a new ready session",
        ),
      ])
    json_output.output(response)
  })
  |> glint.description("Ready phase: Pre-launch readiness check")
}

const exit_fail = 1

const exit_error = 2

const exit_invalid = 3

/// External FFI function to halt with exit code
@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

/// Start a new ready session
pub fn ready_start_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let spec_path = case input.args {
      [path, ..] -> path
      [] -> ""
    }

    let session_id = ffi.generate_uuid()
    let timestamp = ffi.current_timestamp()

    let data =
      json.object([
        #("session_id", json.string(session_id)),
        #("spec_path", json.string(spec_path)),
        #("phase", json.string("ready")),
        #("status", json.string("in_progress")),
        #("created_at", json.string(timestamp)),
      ])

    let next_actions = [
      json_output.next_action(
        "intent ready check --session=" <> session_id,
        "Check session status",
      ),
      json_output.next_action(
        "intent ready critique --session=" <> session_id,
        "Run Pre-Launch Auditor critique",
      ),
    ]

    let spec_path_opt = case spec_path {
      "" -> None
      _ -> Some(spec_path)
    }

    let response =
      json_output.success(
        "ready_start_result",
        "ready start",
        data,
        spec_path_opt,
        next_actions,
      )

    json_output.output(response)
    halt(exit_pass)
  })
  |> glint.description(
    "Start a new ready phase session for pre-launch validation",
  )
}

/// Check ready session status
pub fn ready_check_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session") |> result.unwrap("")

    case session_id {
      "" -> {
        let error =
          json_output.error("missing_session_id", "Session ID is required")

        let next_actions = [
          json_output.next_action(
            "intent ready start <spec-path>",
            "Start a new ready session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "ready_check_error",
            "ready check",
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
            #("critique_score", json.int(0)),
            #("responses_count", json.int(0)),
            #("blockers_resolved", json.int(0)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent ready critique --session=" <> session_id,
            "Run Pre-Launch Auditor critique",
          ),
        ]

        let response =
          json_output.success(
            "ready_check_result",
            "ready check",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Check ready session status")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to check"),
  )
}

/// Run Pre-Launch Auditor critique on ready session
pub fn ready_critique_command() -> glint.Command(Nil) {
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
            "ready_critique_error",
            "ready critique",
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
        // Load spec and run READY analysis
        case loader.load_spec_quiet(spec_path) {
          Ok(spec) -> {
            let ready_report = ready.analyze_ready(spec)
            let critique_result = ready_critique.critique_ready(ready_report)

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
                #("overall_readiness", json.int(ready_report.overall_readiness)),
                #("issues", json.array(issues_json, fn(x) { x })),
                #("dimensions", dimensions_to_json(ready_report)),
              ])

            let next_actions = case critique_result.passed {
              True -> [
                json_output.next_action(
                  "intent ready agree --session=" <> session_id,
                  "Approve for launch",
                ),
              ]
              False -> [
                json_output.next_action(
                  "intent ready respond --session="
                    <> session_id
                    <> " --issue=<issue_id> --response='...'",
                  "Respond to critique issues",
                ),
              ]
            }

            let response =
              json_output.success(
                "ready_critique_result",
                "ready critique",
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
                "ready_critique_error",
                "ready critique",
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
  |> glint.description("Run Pre-Launch Auditor critique on ready session")
  |> glint.flag("session", flag.string() |> flag.description("Session ID"))
  |> glint.flag("spec", flag.string() |> flag.description("Path to spec file"))
}

/// Submit response to a critique issue
pub fn ready_respond_command() -> glint.Command(Nil) {
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
            "ready_respond_error",
            "ready respond",
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
            "intent ready critique --session="
              <> session_id
              <> " --spec=<spec_path>",
            "Re-run critique to check progress",
          ),
        ]

        let resp =
          json_output.success(
            "ready_respond_result",
            "ready respond",
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

/// Finalize ready session and approve for launch
pub fn ready_agree_command() -> glint.Command(Nil) {
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
            "intent ready start <spec-path>",
            "Start a new ready session",
          ),
          json_output.next_action(
            "intent sessions --profile=api",
            "List available sessions",
          ),
        ]

        let response =
          json_output.failure(
            "ready_agree_error",
            "ready agree",
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
            #("status", json.string("approved")),
            #("approval_notes", json.string(notes)),
            #("approved_at", json.string(timestamp)),
          ])

        let next_actions = [
          json_output.next_action(
            "intent beads <session-id>",
            "Generate implementation beads",
          ),
          json_output.next_action(
            "intent prompt <session-id>",
            "Generate AI implementation prompts",
          ),
        ]

        let response =
          json_output.success(
            "ready_agree_result",
            "ready agree",
            data,
            None,
            next_actions,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Finalize ready session and approve for launch")
  |> glint.flag(
    "session",
    flag.string() |> flag.description("Session ID to approve"),
  )
  |> glint.flag(
    "notes",
    flag.string() |> flag.description("Approval notes or comments"),
  )
}

// =============================================================================
// READY COMMAND HELPERS
// =============================================================================

fn critique_question_to_string(
  question: ready_critique.CritiqueQuestion,
) -> String {
  case question {
    ready_critique.VisionAlignment -> "vision_alignment"
    ready_critique.SuccessCriteria -> "success_criteria"
    ready_critique.RollbackPlan -> "rollback_plan"
  }
}

fn critique_severity_to_string(severity: ready_critique.Severity) -> String {
  case severity {
    ready_critique.Critical -> "critical"
    ready_critique.Warning -> "warning"
  }
}

fn dimensions_to_json(report: ReadyReport) -> json.Json {
  json.object([
    #("replacement", dimension_to_json(report.replacement)),
    #("empathy", dimension_to_json(report.empathy)),
    #("actionable", dimension_to_json(report.actionable)),
    #("discoverable", dimension_to_json(report.discoverable)),
    #("yet_complete", dimension_to_json(report.yet_complete)),
  ])
}

fn dimension_to_json(dim: planning_types.DimensionScore) -> json.Json {
  json.object([
    #("score", json.int(dim.score)),
    #("reasoning", json.string(dim.reasoning)),
    #("issues", json.array(dim.issues, json.string)),
  ])
}
