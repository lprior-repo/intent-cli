/// Plan Commands Module
/// Commands for execution plan generation and approval from interview sessions.
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/ffi
import intent/interview_storage
import intent/json_output
import intent/plan_mode
import simplifile

// Exit codes
const exit_pass = 0

const exit_error = 4

// Local constants to avoid circular dependency
const sessions_jsonl = ".intent/sessions.jsonl"

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil

// Private utility functions

fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

fn current_iso8601_timestamp() -> String {
  ffi.current_iso8601_timestamp()
}

// Public command functions

pub fn plan_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let format =
      flag.get_string(input.flags, "format")
      |> result.unwrap("human")

    let rounds =
      flag.get_int(input.flags, "rounds")
      |> result.unwrap(5)

    // Validate rounds is in range 1-5
    case rounds >= 1 && rounds <= 5 {
      False -> {
        let response =
          json_output.failure(
            "plan_failed",
            "plan",
            json.object([
              #("usage", json.string("intent plan <session_id> [--rounds=1..5]")),
              #("provided_rounds", json.int(rounds)),
            ]),
            [json_output.error("usage_error", "Rounds must be between 1 and 5")],
            None,
            [
              json_output.next_action(
                "intent plan <session_id> --rounds=3",
                "Use a valid rounds value (1-5)",
              ),
            ],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
      True -> Nil
    }

    case input.args {
      [session_id, ..] -> {
        case compute_plan_with_session(session_id) {
          Error(err) -> {
            // Return JSON error instead of plain text
            let error_message = plan_mode.format_error(err)
            let error_type = case err {
              plan_mode.SessionNotFound(_) -> "session_not_found"
              plan_mode.ParseError(_) -> "parse_error"
              plan_mode.CyclicDependency(_) -> "cyclic_dependency"
              plan_mode.MissingDependency(_, _) -> "missing_dependency"
            }
            
            let response =
              json_output.failure(
                "plan_failed",
                "plan",
                json.object([#("session_id", json.string(session_id))]),
                [json_output.error(error_type, error_message)],
                Some(session_id),
                [
                  json_output.next_action(
                    "intent sessions",
                    "List available sessions",
                  ),
                  json_output.next_action(
                    "intent beads " <> session_id,
                    "Generate beads from session",
                  ),
                ],
                exit_error,
              )
            json_output.output(response)
            halt(exit_error)
          }
          Ok(plan) -> {
            let output = case format {
              "json" -> plan_mode.format_plan_json(plan)
              _ -> plan_mode.format_plan_human(plan)
            }
            io.println(output)
            halt(exit_pass)
          }
        }
      }
      [] -> {
        // No session_id provided - return JSON usage info with exit code 0
        // This makes the command more testable and AI-friendly
        let error =
          json_output.error(
            "missing_arguments",
            "Session ID is required for plan command",
          )

        let response =
          json_output.failure(
            "plan_usage",
            "plan",
            json.object([
              #(
                "usage",
                json.string("intent plan <session_id> [--format human|json]"),
              ),
              #(
                "description",
                json.string("Display execution plan from session beads"),
              ),
            ]),
            [error],
            None,
            [
              json_output.next_action(
                "intent sessions",
                "List available interview sessions",
              ),
              json_output.next_action(
                "intent beads <session_id>",
                "Generate beads from a session first",
              ),
            ],
            exit_pass,
          )

        json_output.output(response)
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Display execution plan from session beads")
  |> glint.flag(
    "format",
    flag.string()
      |> flag.default("human")
      |> flag.description("Output format: human or json"),
  )
  |> glint.flag(
    "rounds",
    flag.int()
      |> flag.default(5)
      |> flag.description("Number of mental model rounds to consider (1-5)"),
  )
}

pub fn plan_approve_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let auto_approve =
      flag.get_bool(input.flags, "yes")
      |> result.unwrap(False)

    let notes =
      flag.get_string(input.flags, "notes")
      |> result.unwrap("")

    case input.args {
      [session_id, ..] -> {
        // First verify the session exists and has a valid plan
        case compute_plan_with_session(session_id) {
          Error(err) -> {
            let response =
              json_output.failure(
                "plan_approve_failed",
                "plan_approve",
                json.object([#("session_id", json.string(session_id))]),
                [json_output.error("plan_error", plan_mode.format_error(err))],
                Some(session_id),
                [
                  json_output.next_action(
                    "intent plan " <> session_id,
                    "Generate plan for session",
                  ),
                  json_output.next_action(
                    "intent sessions",
                    "List available sessions",
                  ),
                ],
                exit_error,
              )
            json_output.output(response)
            halt(exit_error)
          }
          Ok(plan) -> {
            // Auto-approve or prompt
            case auto_approve {
              True -> {
                case approve_plan(session_id, "ci", notes) {
                  Ok(Nil) -> {
                    let response =
                      json_output.success(
                        "plan_approved",
                        "plan_approve",
                        json.object([
                          #("session_id", json.string(session_id)),
                          #("total_beads", json.int(plan.total_beads)),
                          #("total_effort", json.string(plan.total_effort)),
                          #(
                            "risk_level",
                            json.string(risk_level_to_string(plan.risk)),
                          ),
                          #("phases", json.int(list.length(plan.phases))),
                          #("approved_by", json.string("ci")),
                        ]),
                        Some(session_id),
                        [
                          json_output.next_action(
                            "bd list --status open",
                            "Show work items to execute",
                          ),
                          json_output.next_action(
                            "intent check <spec>",
                            "Begin execution",
                          ),
                        ],
                      )
                    json_output.output(response)
                    halt(exit_pass)
                  }
                  Error(err) -> {
                    let response =
                      json_output.failure(
                        "plan_approve_failed",
                        "plan_approve",
                        json.object([#("session_id", json.string(session_id))]),
                        [json_output.error("approval_error", err)],
                        Some(session_id),
                        [
                          json_output.next_action(
                            "intent plan " <> session_id,
                            "Review plan again",
                          ),
                        ],
                        exit_error,
                      )
                    json_output.output(response)
                    halt(exit_error)
                  }
                }
              }
              False -> {
                // Interactive mode removed - AI-only mode requires --yes flag
                let response =
                  json_output.failure(
                    "plan_approve_failed",
                    "plan_approve",
                    json.null(),
                    [
                      json_output.error(
                        "interactive_not_available",
                        "Interactive approval not available in AI-only mode",
                      ),
                    ],
                    None,
                    [
                      json_output.next_action(
                        "intent plan-approve " <> session_id <> " --yes",
                        "Approve plan for CI execution",
                      ),
                      json_output.next_action(
                        "intent plan-approve "
                          <> session_id
                          <> " --yes --notes='...'",
                        "Approve with notes",
                      ),
                    ],
                    exit_error,
                  )
                json_output.output(response)
                halt(exit_error)
              }
            }
          }
        }
      }
      [] -> {
        let response =
          json_output.failure(
            "plan_approve_failed",
            "plan_approve",
            json.null(),
            [json_output.error("usage_error", "session ID required")],
            None,
            [
              json_output.next_action(
                "intent plan-approve <session_id> --yes",
                "Approve execution plan for session",
              ),
              json_output.next_action(
                "intent plan <session_id>",
                "Generate plan first",
              ),
              json_output.next_action(
                "intent sessions",
                "List available sessions",
              ),
            ],
            exit_error,
          )
        json_output.output(response)
        halt(exit_error)
      }
    }
  })
  |> glint.description("Approve execution plan for session")
  |> glint.flag(
    "yes",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Auto-approve for CI (non-interactive)"),
  )
  |> glint.flag(
    "notes",
    flag.string() |> flag.default("") |> flag.description("Approval notes"),
  )
}

// Private helper functions

/// Compute plan with rounds_completed from session JSONL
/// Loads session to get rounds_completed, then computes plan with RCS score
fn compute_plan_with_session(
  session_id: String,
) -> Result(plan_mode.ExecutionPlan, plan_mode.PlanError) {
  let jsonl_path = sessions_jsonl
  let session_path = ".intent/session-" <> session_id <> ".cue"

  // Try to load rounds_completed from session JSONL (default to 0 if not found)
  let rounds_completed =
    interview_storage.get_session_from_jsonl(jsonl_path, session_id)
    |> result.map(fn(session) { session.rounds_completed })
    |> result.unwrap(0)

  // Read the CUE content and compute plan with rounds_completed
  case simplifile.read(session_path) {
    Error(_) -> Error(plan_mode.SessionNotFound(session_id))
    Ok(content) ->
      plan_mode.compute_plan_from_content(session_id, content, rounds_completed)
  }
}

/// Write plan approval to session CUE file
fn approve_plan(
  session_id: String,
  approved_by: String,
  notes: String,
) -> Result(Nil, String) {
  let session_path = ".intent/session-" <> session_id <> ".cue"
  let timestamp = current_iso8601_timestamp()

  let notes_line = case string.is_empty(notes) {
    True -> ""
    False -> "\n\tnotes: \"" <> escape_cue_string(notes) <> "\""
  }

  let approval_cue =
    "\n// Plan Approval\napproval: {\n\tapproved: true\n\tapproved_at: \""
    <> timestamp
    <> "\"\n\tapproved_by: \""
    <> approved_by
    <> "\""
    <> notes_line
    <> "\n}\n"

  case simplifile.append(session_path, approval_cue) {
    Ok(Nil) -> Ok(Nil)
    Error(err) -> Error("Failed to write approval: " <> string.inspect(err))
  }
}

fn risk_level_to_string(risk: plan_mode.RiskLevel) -> String {
  case risk {
    plan_mode.Low -> "low"
    plan_mode.Medium -> "medium"
    plan_mode.High -> "high"
    plan_mode.Critical -> "critical"
  }
}
