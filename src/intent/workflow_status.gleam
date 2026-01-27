//// Workflow Status Detection and Display
//// BEAD: intent-cli-dxp (P1)
////
//// This module detects the current workflow phase by examining what files
//// exist in the .intent directory and provides status reporting.
////
//// Workflow Phases:
//// 1. NoWorkflow - No .intent directory or empty
//// 2. Interview - Have sessions.jsonl but no spec files
//// 3. Plan - Have spec files but no beads
//// 4. Execute - Have beads directory with claimed/working beads
//// 5. Verify - All beads complete, running verification

import gleam/dynamic
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

// ============================================================================
// TYPES
// ============================================================================

/// The four workflow phases
pub type Phase {
  NoWorkflow
  Interview
  Plan
  Execute
  Verify
}

/// Bead execution progress tracking
pub type BeadProgress {
  BeadProgress(completed: Int, total: Int)
}

/// Next step command with explanation
pub type NextStep {
  NextStep(command: String, reason: String)
}

/// Complete workflow status
pub type Status {
  Status(
    current_phase: Phase,
    message: String,
    session_count: Option(Int),
    spec_count: Option(Int),
    bead_count: Option(Int),
    bead_progress: Option(BeadProgress),
    progress_percent: Option(Int),
    next_steps: List(NextStep),
  )
}

// ============================================================================
// STATUS DETECTION
// ============================================================================

/// Detect current workflow phase by examining files in .intent directory
pub fn detect_status() -> Status {
  let intent_dir = ".intent"

  case check_directory_exists(intent_dir) {
    False -> {
      // No workflow started
      Status(
        current_phase: NoWorkflow,
        message: "No workflow started. Run 'intent interview --profile api' to begin.",
        session_count: None,
        spec_count: None,
        bead_count: None,
        bead_progress: None,
        progress_percent: None,
        next_steps: next_steps_for_phase(NoWorkflow),
      )
    }
    True -> {
      // Check what exists in .intent
      let has_sessions = file_exists(intent_dir <> "/sessions.jsonl")
      let spec_files = list_files_with_extension(intent_dir, ".cue")
      let has_beads_dir = check_directory_exists(".beads")

      case has_sessions, spec_files, has_beads_dir {
        False, _, _ -> {
          // Directory exists but no sessions - corrupted state
          Status(
            current_phase: NoWorkflow,
            message: "No workflow started. Run 'intent interview --profile api' to begin.",
            session_count: None,
            spec_count: None,
            bead_count: None,
            bead_progress: None,
            progress_percent: None,
            next_steps: next_steps_for_phase(NoWorkflow),
          )
        }
        True, [], False -> {
          // Have sessions but no specs - Interview phase
          let session_count = count_sessions(intent_dir <> "/sessions.jsonl")

          Status(
            current_phase: Interview,
            message: "Interview phase - "
              <> int.to_string(session_count)
              <> " session(s) completed. Export a session to continue.",
            session_count: Some(session_count),
            spec_count: None,
            bead_count: None,
            bead_progress: None,
            progress_percent: None,
            next_steps: next_steps_for_phase(Interview),
          )
        }
        True, _, False -> {
          // Have specs but no beads - Plan phase
          Status(
            current_phase: Plan,
            message: "Plan phase - Have "
              <> int.to_string(list.length(spec_files))
              <> " spec(s). Generate beads to continue.",
            session_count: Some(count_sessions(intent_dir <> "/sessions.jsonl")),
            spec_count: Some(list.length(spec_files)),
            bead_count: None,
            bead_progress: None,
            progress_percent: None,
            next_steps: next_steps_for_phase(Plan),
          )
        }
        True, _, True -> {
          // Have beads - Check execution status
          let bead_progress = get_bead_progress()

          case bead_progress.completed, bead_progress.total {
            0, _ -> {
              // Beads generated but none claimed
              Status(
                current_phase: Plan,
                message: "Plan phase - "
                  <> int.to_string(bead_progress.total)
                  <> " beads ready. Start execution.",
                session_count: Some(count_sessions(
                  intent_dir <> "/sessions.jsonl",
                )),
                spec_count: Some(list.length(spec_files)),
                bead_count: Some(bead_progress.total),
                bead_progress: None,
                progress_percent: Some(0),
                next_steps: next_steps_for_phase(Plan),
              )
            }
            completed, total if completed < total -> {
              // Execution in progress
              let percent = completed * 100 / total

              Status(
                current_phase: Execute,
                message: "Execute phase - "
                  <> int.to_string(completed)
                  <> "/"
                  <> int.to_string(total)
                  <> " beads complete ("
                  <> int.to_string(percent)
                  <> "%).",
                session_count: Some(count_sessions(
                  intent_dir <> "/sessions.jsonl",
                )),
                spec_count: Some(list.length(spec_files)),
                bead_count: Some(total),
                bead_progress: Some(bead_progress),
                progress_percent: Some(percent),
                next_steps: next_steps_for_phase(Execute),
              )
            }
            _, _ -> {
              // All beads complete
              Status(
                current_phase: Verify,
                message: "Verify phase - All beads complete. Run verification tests.",
                session_count: Some(count_sessions(
                  intent_dir <> "/sessions.jsonl",
                )),
                spec_count: Some(list.length(spec_files)),
                bead_count: Some(bead_progress.total),
                bead_progress: Some(bead_progress),
                progress_percent: Some(100),
                next_steps: next_steps_for_phase(Verify),
              )
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// NEXT STEPS GENERATION
// ============================================================================

/// Generate next steps for a given phase
pub fn next_steps_for_phase(phase: Phase) -> List(NextStep) {
  case phase {
    NoWorkflow -> [
      NextStep(
        "intent interview --profile api",
        "Start a new interview session to gather requirements",
      ),
      NextStep("intent sessions", "List existing interview sessions (if any)"),
    ]
    Interview -> [
      NextStep("intent sessions", "List all interview sessions"),
      NextStep(
        "intent export <session-id> --output spec.cue",
        "Export an interview session to CUE spec format",
      ),
    ]
    Plan -> [
      NextStep(
        "intent plan <session-id>",
        "Generate execution plan with waves and beads",
      ),
      NextStep(
        "intent beads <session-id>",
        "Generate work items from interview session",
      ),
      NextStep("intent plan-approve <session-id>", "Approve the execution plan"),
    ]
    Execute -> [
      NextStep("bd ready --json", "List available beads ready to be claimed"),
      NextStep("bd claim <bead-id>", "Claim a bead to start working on it"),
      NextStep(
        "bd close <bead-id> --reason 'Completed'",
        "Mark a bead as complete",
      ),
    ]
    Verify -> [
      NextStep(
        "intent check <spec.cue> --target=<URL>",
        "Run contract tests against API",
      ),
      NextStep(
        "intent validate <spec.cue>",
        "Validate spec syntax and structure",
      ),
      NextStep("intent quality <spec.cue>", "Check spec quality score"),
    ]
  }
}

// ============================================================================
// FORMATTING
// ============================================================================

/// Format status as human-readable text
/// Format status as human-readable text
pub fn format_text(status: Status) -> String {
  let phase_name = phase_to_string(status.current_phase)

  let base_parts = [
    "Current Phase: " <> phase_name,
    "",
    status.message,
    "",
  ]

  let with_counts =
    base_parts
    |> append_optional("Sessions: ", status.session_count)
    |> append_optional("Specs: ", status.spec_count)
    |> append_optional("Beads: ", status.bead_count)

  let with_progress =
    append_progress(with_counts, status.bead_progress, status.progress_percent)

  let with_separator = case list.length(with_progress) > 0 {
    True -> list.append(with_progress, [""])
    False -> with_progress
  }

  let final_parts = append_next_steps(with_separator, status.next_steps)

  string.join(
    list.reverse(final_parts),
    "
",
  )
}

/// Format status as JSON
pub fn format_json(status: Status) -> String {
  let json_object =
    [
      #("phase", json.string(phase_to_string(status.current_phase))),
      #("message", json.string(status.message)),
    ]
    |> append_optional_json("sessions", status.session_count, json.int)
    |> append_optional_json("specs", status.spec_count, json.int)
    |> append_optional_json("beads", status.bead_count, json.int)
    |> append_progress_json(status.bead_progress, status.progress_percent)
    |> append_next_steps_json(status.next_steps)

  json.object(json_object)
  |> json.to_string()
}

// ============================================================================
// PRIVATE HELPERS
// ============================================================================

/// Check if a directory exists
fn check_directory_exists(path: String) -> Bool {
  simplifile.verify_is_file(path)
  |> result.is_error
  |> fn(is_not_file) {
    case is_not_file {
      True -> {
        // Not a file, check if directory
        simplifile.verify_is_directory(path)
        |> result.unwrap(False)
      }
      False -> False
    }
  }
}

/// Check if a file exists
fn file_exists(path: String) -> Bool {
  simplifile.verify_is_file(path)
  |> result.is_ok
}

/// List files in directory with given extension
fn list_files_with_extension(dir: String, ext: String) -> List(String) {
  case simplifile.read_directory(dir) {
    Ok(files) -> {
      files
      |> list.filter(fn(f) { string.ends_with(f, ext) })
    }
    Error(_) -> []
  }
}

/// Count sessions in sessions.jsonl
fn count_sessions(path: String) -> Int {
  case file_exists(path) {
    False -> 0
    True -> {
      case simplifile.read(path) {
        Ok(content) -> {
          content
          |> string.split("\n")
          |> list.filter(fn(line) { !string.is_empty(line) })
          |> list.length
        }
        Error(_) -> 0
      }
    }
  }
}

/// Get bead execution progress from .beads directory
fn get_bead_progress() -> BeadProgress {
  let state_file = ".beads/export-state/state.json"

  case file_exists(state_file) {
    False -> BeadProgress(completed: 0, total: 0)
    True -> {
      // Parse state file to get bead counts
      case simplifile.read(state_file) {
        Ok(content) -> parse_bead_progress(content)
        Error(_) -> BeadProgress(completed: 0, total: 0)
      }
    }
  }
}

/// Parse bead progress from state file JSON
fn parse_bead_progress(content: String) -> BeadProgress {
  // Try to extract bead counts from JSON
  case json.decode(content, dynamic.dynamic) {
    Ok(json_data) -> {
      let beads =
        json_data
        |> dynamic.field("beads", dynamic.list(dynamic.dynamic))

      case beads {
        Ok(bead_list) -> {
          let completed =
            bead_list
            |> list.filter(fn(b) {
              b
              |> dynamic.field("status", dynamic.string)
              |> result.unwrap("")
              |> string.lowercase
              |> fn(s) { s == "done" || s == "complete" }
            })
            |> list.length

          BeadProgress(completed: completed, total: list.length(bead_list))
        }
        Error(_) -> BeadProgress(completed: 0, total: 0)
      }
    }
    Error(_) -> BeadProgress(completed: 0, total: 0)
  }
}

/// Convert phase to string
fn phase_to_string(phase: Phase) -> String {
  case phase {
    NoWorkflow -> "No Workflow"
    Interview -> "Interview"
    Plan -> "Plan"
    Execute -> "Execute"
    Verify -> "Verify"
  }
}

/// Append optional field to parts list
fn append_optional(
  parts: List(String),
  prefix: String,
  value: Option(Int),
) -> List(String) {
  case value {
    None -> parts
    Some(v) -> [prefix <> int.to_string(v), ..parts]
  }
}

/// Append progress info to parts list
fn append_progress(
  parts: List(String),
  bead_progress: Option(BeadProgress),
  percent: Option(Int),
) -> List(String) {
  case bead_progress, percent {
    Some(BeadProgress(completed, total)), Some(p) -> {
      [
        "Progress: "
          <> int.to_string(completed)
          <> "/"
          <> int.to_string(total)
          <> " ("
          <> int.to_string(p)
          <> "%)",
        ..parts
      ]
    }
    _, _ -> parts
  }
}

/// Append list separator
fn append_list(parts: List(String), sep: String, val: String) -> List(String) {
  case list.length(parts) > 0 {
    True -> list.append(parts, [sep])
    False -> parts
  }
}

/// Append next steps to parts list
fn append_next_steps(parts: List(String), steps: List(NextStep)) -> List(String) {
  case steps {
    [] -> parts
    _ -> {
      let step_lines =
        steps
        |> list.map(fn(step) {
          "  - " <> step.command <> "  (" <> step.reason <> ")"
        })

      ["Next Steps:", ..list.append(step_lines, parts)]
    }
  }
}

/// Append optional JSON field
fn append_optional_json(
  obj: List(#(String, json.Json)),
  key: String,
  value: Option(Int),
  encoder: fn(Int) -> json.Json,
) -> List(#(String, json.Json)) {
  case value {
    None -> obj
    Some(v) -> [#(key, encoder(v)), ..obj]
  }
}

/// Append progress JSON fields
fn append_progress_json(
  obj: List(#(String, json.Json)),
  bead_progress: Option(BeadProgress),
  percent: Option(Int),
) -> List(#(String, json.Json)) {
  case bead_progress, percent {
    Some(BeadProgress(completed, total)), Some(p) -> {
      [
        #("beads_completed", json.int(completed)),
        #("beads_total", json.int(total)),
        #("progress", json.int(p)),
        ..obj
      ]
    }
    _, _ -> obj
  }
}

/// Append next steps as JSON
fn append_next_steps_json(
  obj: List(#(String, json.Json)),
  steps: List(NextStep),
) -> List(#(String, json.Json)) {
  case steps {
    [] -> obj
    _ -> {
      let steps_json =
        steps
        |> list.map(fn(step) {
          json.object([
            #("command", json.string(step.command)),
            #("reason", json.string(step.reason)),
          ])
        })

      [#("next_steps", json.array(steps_json, fn(x) { x })), ..obj]
    }
  }
}
