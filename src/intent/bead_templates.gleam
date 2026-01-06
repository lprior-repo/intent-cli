/// Bead Templates
/// Generates work items (beads) from interview sessions for the `bd` issue tracker

import gleam/dict.{type Dict}
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import intent/case_insensitive.{contains_any_ignore_case}
import intent/interview.{type InterviewSession, type Profile}

/// A single work item (bead) record
pub type BeadRecord {
  BeadRecord(
    title: String,
    description: String,
    profile_type: String,
    priority: Int,
    issue_type: String,
    labels: List(String),
    ai_hints: String,
    acceptance_criteria: List(String),
    dependencies: List(String),
  )
}

/// Generate beads from a completed interview session
pub fn generate_beads_from_session(session: InterviewSession) -> List(BeadRecord) {
  let profile_str = profile_to_string(session.profile)

  case session.profile {
    interview.Api -> generate_api_beads(session, profile_str)
    interview.Cli -> generate_cli_beads(session, profile_str)
    interview.Event -> generate_event_beads(session, profile_str)
    interview.Data -> generate_data_beads(session, profile_str)
    interview.Workflow -> generate_workflow_beads(session, profile_str)
    interview.UI -> generate_ui_beads(session, profile_str)
  }
}

/// Generate API endpoint beads
fn generate_api_beads(session: InterviewSession, profile: String) -> List(BeadRecord) {
  let endpoint_answers = list.filter(session.answers, fn(answer) {
    contains_any_ignore_case(answer.question_text, ["endpoint", "path"])
  })

  list.map(endpoint_answers, fn(answer) {
    BeadRecord(
      title: "Implement API endpoint",
      description: answer.response,
      profile_type: profile,
      priority: 3,
      issue_type: "api_endpoint",
      labels: ["api", "endpoint", "implementation"],
      ai_hints: "Use interview response to build OpenAPI spec and implementation",
      acceptance_criteria: [
        "Endpoint responds with correct status code",
        "Response schema matches spec",
        "Error handling implemented",
        "Documentation added",
      ],
      dependencies: [],
    )
  })
}

/// Generate CLI command beads
fn generate_cli_beads(session: InterviewSession, profile: String) -> List(BeadRecord) {
  let command_answers = list.filter(session.answers, fn(answer) {
    contains_any_ignore_case(answer.question_text, ["command", "subcommand"])
  })

  list.map(command_answers, fn(answer) {
    BeadRecord(
      title: "Implement CLI command",
      description: answer.response,
      profile_type: profile,
      priority: 3,
      issue_type: "cli_command",
      labels: ["cli", "command", "implementation"],
      ai_hints: "Review interview response for command syntax, options, and behavior",
      acceptance_criteria: [
        "Command parses arguments correctly",
        "Output format matches spec",
        "Help text is clear",
        "Error messages are helpful",
      ],
      dependencies: [],
    )
  })
}

/// Generate event beads
fn generate_event_beads(session: InterviewSession, profile: String) -> List(BeadRecord) {
  let event_answers = list.filter(session.answers, fn(answer) {
    contains_any_ignore_case(answer.question_text, ["event", "message"])
  })

  list.map(event_answers, fn(answer) {
    BeadRecord(
      title: "Define and emit event",
      description: answer.response,
      profile_type: profile,
      priority: 2,
      issue_type: "event",
      labels: ["event", "messaging", "integration"],
      ai_hints: "Create event schema and producer/consumer implementation",
      acceptance_criteria: [
        "Event schema defined",
        "Producer implementation complete",
        "Consumer can subscribe",
        "Event routing working",
      ],
      dependencies: [],
    )
  })
}

/// Generate data model beads
fn generate_data_beads(session: InterviewSession, profile: String) -> List(BeadRecord) {
  let entity_answers = list.filter(session.answers, fn(answer) {
    contains_any_ignore_case(answer.question_text, ["entity", "data model", "schema"])
  })

  list.map(entity_answers, fn(answer) {
    BeadRecord(
      title: "Implement data model",
      description: answer.response,
      profile_type: profile,
      priority: 4,
      issue_type: "data_model",
      labels: ["data", "schema", "storage"],
      ai_hints: "Generate database schema and ORM/repository layer",
      acceptance_criteria: [
        "Schema migrations ready",
        "Validation rules implemented",
        "Indexes optimized",
        "Tests cover all fields",
      ],
      dependencies: [],
    )
  })
}

/// Generate workflow beads
fn generate_workflow_beads(session: InterviewSession, profile: String) -> List(BeadRecord) {
  let workflow_answers = list.filter(session.answers, fn(answer) {
    contains_any_ignore_case(answer.question_text, ["workflow", "process", "step"])
  })

  list.map(workflow_answers, fn(answer) {
    BeadRecord(
      title: "Implement workflow step",
      description: answer.response,
      profile_type: profile,
      priority: 2,
      issue_type: "workflow",
      labels: ["workflow", "orchestration", "automation"],
      ai_hints: "Design state machine and implement step logic",
      acceptance_criteria: [
        "State transitions working",
        "Error handling and retries",
        "Step completion detection",
        "Monitoring/logging implemented",
      ],
      dependencies: [],
    )
  })
}

/// Generate UI screen beads
fn generate_ui_beads(session: InterviewSession, profile: String) -> List(BeadRecord) {
  let screen_answers = list.filter(session.answers, fn(answer) {
    contains_any_ignore_case(answer.question_text, ["screen", "view", "interface"])
  })

  list.map(screen_answers, fn(answer) {
    BeadRecord(
      title: "Build UI screen",
      description: answer.response,
      profile_type: profile,
      priority: 2,
      issue_type: "ui_screen",
      labels: ["ui", "frontend", "component"],
      ai_hints: "Create mockup, component hierarchy, and responsive design",
      acceptance_criteria: [
        "All required fields present",
        "Responsive on mobile/tablet/desktop",
        "Accessibility standards met",
        "User testing completed",
      ],
      dependencies: [],
    )
  })
}

/// Convert bead record to JSONL line format (for .beads/issues.jsonl)
pub fn bead_to_jsonl_line(bead: BeadRecord) -> String {
  let json_list = [
    #("title", json.string(bead.title)),
    #("description", json.string(bead.description)),
    #("profile_type", json.string(bead.profile_type)),
    #("priority", json.int(bead.priority)),
    #("issue_type", json.string(bead.issue_type)),
    #(
      "labels",
      json.array(bead.labels, json.string),
    ),
    #("ai_hints", json.string(bead.ai_hints)),
    #(
      "acceptance_criteria",
      json.array(bead.acceptance_criteria, json.string),
    ),
    #(
      "dependencies",
      json.array(bead.dependencies, json.string),
    ),
  ]

  json.object(json_list)
  |> json.to_string
}

/// Format beads for output as JSONL (newline-delimited JSON)
pub fn beads_to_jsonl(beads: List(BeadRecord)) -> String {
  beads
  |> list.map(bead_to_jsonl_line)
  |> string.join("\n")
}

/// Extract beads with specific issue type
pub fn filter_beads_by_type(
  beads: List(BeadRecord),
  issue_type: String,
) -> List(BeadRecord) {
  list.filter(beads, fn(bead) { bead.issue_type == issue_type })
}

/// Sort beads by priority (higher number = higher priority)
pub fn sort_beads_by_priority(beads: List(BeadRecord)) -> List(BeadRecord) {
  list.sort(beads, fn(a, b) {
    int.compare(b.priority, a.priority)
  })
}

/// Add dependency between beads (updates beads in place)
pub fn add_dependency(
  beads: List(BeadRecord),
  from_title: String,
  to_title: String,
) -> List(BeadRecord) {
  list.map(beads, fn(bead) {
    case bead.title == from_title {
      True ->
        BeadRecord(
          ..bead,
          dependencies: list.append(bead.dependencies, [to_title]),
        )
      False -> bead
    }
  })
}

/// Helper: convert Profile to string
fn profile_to_string(profile: Profile) -> String {
  case profile {
    interview.Api -> "api"
    interview.Cli -> "cli"
    interview.Event -> "event"
    interview.Data -> "data"
    interview.Workflow -> "workflow"
    interview.UI -> "ui"
  }
}

/// Summary stats for beads
pub type BeadStats {
  BeadStats(
    total: Int,
    by_type: Dict(String, Int),
    by_priority: Dict(Int, Int),
  )
}

/// Calculate stats for a list of beads
pub fn bead_stats(beads: List(BeadRecord)) -> BeadStats {
  let total = list.length(beads)

  let by_type = list.fold(beads, dict.new(), fn(acc, bead) {
    let current = dict.get(acc, bead.issue_type) |> result.unwrap(0)
    dict.insert(acc, bead.issue_type, current + 1)
  })

  let by_priority = list.fold(beads, dict.new(), fn(acc, bead) {
    let current = dict.get(acc, bead.priority) |> result.unwrap(0)
    dict.insert(acc, bead.priority, current + 1)
  })

  BeadStats(total: total, by_type: by_type, by_priority: by_priority)
}
