/// Plan Mode CLI Commands
/// Commands for plan execution, approval, and bead regeneration
///
/// ## Commands
/// - plan: Display execution plan for a session
/// - plan-approve: Approve execution plan for CI/automation
/// - beads-regenerate: Regenerate failed/blocked beads
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/bead_feedback
import intent/cli/common.{ExitError, ExitFail, ExitPass, exit}
import intent/plan_mode
import intent/stdin
import simplifile

// =============================================================================
// PLAN COMMAND
// =============================================================================

/// The `plan` command - display execution plan for a session
pub fn plan_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let format =
      flag.get_string(input.flags, "format")
      |> result.unwrap("human")

    case input.args {
      [session_id, ..] -> {
        case plan_mode.compute_plan(session_id) {
          Error(err) -> {
            io.println_error(plan_mode.format_error(err))
            exit(ExitError)
          }
          Ok(plan) -> {
            let output = case format {
              "json" -> plan_mode.format_plan_json(plan)
              _ -> plan_mode.format_plan_human(plan)
            }
            io.println(output)
            exit(ExitPass)
          }
        }
      }
      [] -> {
        io.println_error(
          "Usage: intent plan <session_id> [--format human|json]",
        )
        io.println_error("")
        io.println_error("Display execution plan from session beads.")
        io.println_error("")
        io.println_error("Examples:")
        io.println_error(
          "  intent plan abc123              # Human-readable output",
        )
        io.println_error("  intent plan abc123 --format json  # JSON output")
        exit(ExitError)
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
}

// =============================================================================
// PLAN-APPROVE COMMAND
// =============================================================================

/// The `plan-approve` command - approve execution plan for CI/automation
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
        case plan_mode.compute_plan(session_id) {
          Error(err) -> {
            io.println_error(plan_mode.format_error(err))
            exit(ExitError)
          }
          Ok(plan) -> {
            // Show plan summary
            io.println("")
            io.println(
              "═══════════════════════════════════════════════════════════════════",
            )
            io.println("                    PLAN APPROVAL")
            io.println(
              "═══════════════════════════════════════════════════════════════════",
            )
            io.println("")
            io.println("Session: " <> plan.session_id)
            io.println("Total Beads: " <> string.inspect(plan.total_beads))
            io.println("Total Effort: " <> plan.total_effort)
            io.println("Risk Level: " <> risk_level_to_string(plan.risk))
            io.println("Phases: " <> string.inspect(list.length(plan.phases)))
            io.println("")

            case list.is_empty(plan.blockers) {
              True -> Nil
              False -> {
                io.println("⚠ BLOCKERS:")
                list.each(plan.blockers, fn(b) { io.println("  • " <> b) })
                io.println("")
              }
            }

            // Auto-approve or prompt
            case auto_approve {
              True -> {
                case approve_plan(session_id, "ci", notes) {
                  Ok(Nil) -> {
                    io.println("✓ Plan approved automatically (CI mode)")
                    exit(ExitPass)
                  }
                  Error(err) -> {
                    io.println_error("✗ Failed to approve plan: " <> err)
                    exit(ExitError)
                  }
                }
              }
              False -> {
                io.println("Approve this plan? (yes/no)")
                case stdin.read_line() {
                  Ok(response) -> {
                    let cleaned = string.trim(string.lowercase(response))
                    case cleaned {
                      "yes" | "y" -> {
                        case approve_plan(session_id, "human", notes) {
                          Ok(Nil) -> {
                            io.println("✓ Plan approved")
                            exit(ExitPass)
                          }
                          Error(err) -> {
                            io.println_error(
                              "✗ Failed to approve plan: " <> err,
                            )
                            exit(ExitError)
                          }
                        }
                      }
                      "no" | "n" -> {
                        io.println("Plan not approved")
                        exit(ExitFail)
                      }
                      _ -> {
                        io.println_error(
                          "Invalid response. Please enter 'yes' or 'no'",
                        )
                        exit(ExitError)
                      }
                    }
                  }
                  Error(_) -> {
                    io.println_error("Failed to read input")
                    exit(ExitError)
                  }
                }
              }
            }
          }
        }
      }
      [] -> {
        io.println_error(
          "Usage: intent plan-approve <session_id> [--yes] [--notes 'text']",
        )
        io.println_error("")
        io.println_error("Approve execution plan for a session.")
        io.println_error("")
        io.println_error("Flags:")
        io.println_error(
          "  --yes      Auto-approve for CI pipelines (non-interactive)",
        )
        io.println_error("  --notes    Optional approval notes")
        io.println_error("")
        io.println_error("Examples:")
        io.println_error(
          "  intent plan-approve abc123           # Interactive approval",
        )
        io.println_error(
          "  intent plan-approve abc123 --yes     # CI auto-approval",
        )
        exit(ExitError)
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

fn escape_cue_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

@external(erlang, "intent_ffi", "current_iso8601_timestamp")
fn current_iso8601_timestamp() -> String

// =============================================================================
// BEADS-REGENERATE COMMAND
// =============================================================================

/// The `beads-regenerate` command - regenerate failed/blocked beads
pub fn beads_regenerate_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let strategy =
      flag.get_string(input.flags, "strategy")
      |> result.unwrap("hybrid")

    case input.args {
      [session_id, ..] -> {
        let session_path = ".intent/session-" <> session_id <> ".cue"

        // Check session exists (verify_is_file returns Ok(True) if file exists, Ok(False) if not)
        case simplifile.verify_is_file(session_path) {
          Error(_) | Ok(False) -> {
            io.println_error("Session not found: " <> session_id)
            io.println_error("Expected file: " <> session_path)
            exit(ExitError)
          }
          Ok(True) -> {
            // Load feedback
            case bead_feedback.load_feedback_for_session(session_id) {
              Error(err) -> {
                io.println_error(
                  "Failed to load feedback: "
                  <> bead_feedback_error_to_string(err),
                )
                exit(ExitError)
              }
              Ok(feedback) -> {
                // Filter failed/blocked beads
                let needs_regen =
                  feedback
                  |> list.filter(fn(fb) {
                    case fb.result {
                      bead_feedback.Failed -> True
                      bead_feedback.Blocked -> True
                      _ -> False
                    }
                  })

                io.println("")
                io.println(
                  "═══════════════════════════════════════════════════════════════════",
                )
                io.println("                    BEAD REGENERATION")
                io.println(
                  "═══════════════════════════════════════════════════════════════════",
                )
                io.println("")
                io.println("Session: " <> session_id)
                io.println("Strategy: " <> strategy)
                io.println(
                  "Feedback entries: " <> string.inspect(list.length(feedback)),
                )
                io.println(
                  "Beads needing regeneration: "
                  <> string.inspect(list.length(needs_regen)),
                )
                io.println("")

                case list.is_empty(needs_regen) {
                  True -> {
                    io.println(
                      "✓ No beads need regeneration - all passed or skipped",
                    )
                    exit(ExitPass)
                  }
                  False -> {
                    // Display beads that need regeneration
                    io.println("Beads to regenerate:")
                    list.each(needs_regen, fn(fb) {
                      let status_icon = case fb.result {
                        bead_feedback.Failed -> "✗"
                        bead_feedback.Blocked -> "⊘"
                        _ -> "?"
                      }
                      io.println(
                        "  "
                        <> status_icon
                        <> " "
                        <> fb.bead_id
                        <> ": "
                        <> fb.reason,
                      )
                    })
                    io.println("")

                    // Generate regeneration entries
                    let regen_entries =
                      generate_regeneration_entries(needs_regen, strategy)

                    // Append regeneration metadata to session
                    case
                      append_regeneration_to_session(
                        session_path,
                        regen_entries,
                      )
                    {
                      Ok(Nil) -> {
                        io.println("✓ Regeneration metadata added to session")
                        io.println("  Strategy: " <> strategy)
                        io.println(
                          "  Beads marked for regeneration: "
                          <> string.inspect(list.length(needs_regen)),
                        )
                        io.println("")
                        io.println("Next steps:")
                        io.println(
                          "  1. Review regeneration suggestions in "
                          <> session_path,
                        )
                        io.println(
                          "  2. Run 'intent plan "
                          <> session_id
                          <> "' to see updated plan",
                        )
                        io.println("  3. Execute regenerated beads")
                        exit(ExitPass)
                      }
                      Error(err) -> {
                        io.println_error(
                          "✗ Failed to update session: " <> err,
                        )
                        exit(ExitError)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      [] -> {
        io.println_error(
          "Usage: intent beads-regenerate <session_id> [--strategy hybrid|inversion|premortem]",
        )
        io.println_error("")
        io.println_error(
          "Regenerate failed/blocked beads with adjusted approach.",
        )
        io.println_error("")
        io.println_error("Strategies:")
        io.println_error("  hybrid     - Use all analysis methods (default)")
        io.println_error("  inversion  - Focus on failure mode analysis")
        io.println_error("  premortem  - Focus on what could go wrong")
        io.println_error("")
        io.println_error("Examples:")
        io.println_error("  intent beads-regenerate abc123")
        io.println_error(
          "  intent beads-regenerate abc123 --strategy inversion",
        )
        exit(ExitError)
      }
    }
  })
  |> glint.description("Regenerate failed/blocked beads with adjusted approach")
  |> glint.flag(
    "strategy",
    flag.string()
      |> flag.default("hybrid")
      |> flag.description(
        "Regeneration strategy: hybrid, inversion, or premortem",
      ),
  )
}

/// Generate regeneration entries based on failed beads and strategy
fn generate_regeneration_entries(
  failed_beads: List(bead_feedback.BeadFeedback),
  strategy: String,
) -> String {
  let timestamp = current_iso8601_timestamp()

  let entries =
    failed_beads
    |> list.map(fn(fb) {
      let root_cause = case fb.error {
        Some(err) -> err.message
        None -> fb.reason
      }

      "  {\n"
      <> "    bead_id: \""
      <> fb.bead_id
      <> "\"\n"
      <> "    strategy: \""
      <> strategy
      <> "\"\n"
      <> "    root_cause: \""
      <> escape_cue_string(root_cause)
      <> "\"\n"
      <> "    regenerated_at: \""
      <> timestamp
      <> "\"\n"
      <> "  }"
    })
    |> string.join(",\n")

  entries
}

/// Append regeneration metadata to session CUE file
fn append_regeneration_to_session(
  session_path: String,
  entries: String,
) -> Result(Nil, String) {
  let regen_cue =
    "\n// Regeneration Metadata\nregenerations: [\n" <> entries <> "\n]\n"

  case simplifile.append(session_path, regen_cue) {
    Ok(Nil) -> Ok(Nil)
    Error(err) -> Error("Failed to append: " <> string.inspect(err))
  }
}

// =============================================================================
// ERROR FORMATTING
// =============================================================================

fn bead_feedback_error_to_string(err: bead_feedback.FeedbackError) -> String {
  case err {
    bead_feedback.SessionNotFound(id) -> "Session not found: " <> id
    bead_feedback.WriteError(path, msg) ->
      "Write error to " <> path <> ": " <> msg
    bead_feedback.ValidationError(msg) -> "Validation error: " <> msg
  }
}
