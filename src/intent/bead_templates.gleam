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
/// Extended with AI-friendly format: input_example, output_example, must_return, must_not, edge_cases
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
    // Simplified AI-friendly format (BEAD-FORMAT)
    input_example: String,
    output_example: String,
    must_return: List(String),
    must_not: List(String),
    edge_cases: List(String),
  )
}

/// Create a basic BeadRecord with default empty AI-friendly fields
/// Used for backward compatibility and tests
pub fn new_bead(
  title title: String,
  description description: String,
  profile_type profile_type: String,
  priority priority: Int,
  issue_type issue_type: String,
  labels labels: List(String),
  ai_hints ai_hints: String,
  acceptance_criteria acceptance_criteria: List(String),
  dependencies dependencies: List(String),
) -> BeadRecord {
  BeadRecord(
    title: title,
    description: description,
    profile_type: profile_type,
    priority: priority,
    issue_type: issue_type,
    labels: labels,
    ai_hints: ai_hints,
    acceptance_criteria: acceptance_criteria,
    dependencies: dependencies,
    input_example: "",
    output_example: "",
    must_return: [],
    must_not: [],
    edge_cases: [],
  )
}

/// Generate beads from a completed interview session
pub fn generate_beads_from_session(
  session: InterviewSession,
) -> List(BeadRecord) {
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
fn generate_api_beads(
  session: InterviewSession,
  profile: String,
) -> List(BeadRecord) {
  let endpoint_answers =
    list.filter(session.answers, fn(answer) {
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
      // AI-friendly format
      input_example: "GET /resource/123 with Authorization: Bearer token",
      output_example: "{\"id\": 123, \"name\": \"example\", \"status\": \"active\"}",
      must_return: ["200 OK for valid requests", "JSON body with resource data"],
      must_not: [
        "Return 500 for validation errors",
        "Expose internal error details",
      ],
      edge_cases: [
        "Invalid ID format",
        "Missing auth header",
        "Expired token",
        "Resource not found",
      ],
    )
  })
}

/// Generate CLI command beads
fn generate_cli_beads(
  session: InterviewSession,
  profile: String,
) -> List(BeadRecord) {
  let command_answers =
    list.filter(session.answers, fn(answer) {
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
      // AI-friendly format
      input_example: "mycli process --input data.json --output result.txt",
      output_example: "Processing complete. Wrote 42 records to result.txt",
      must_return: ["Exit code 0 on success", "Clear success message"],
      must_not: ["Exit 0 on failure", "Print stack traces in production"],
      edge_cases: [
        "Missing required args",
        "Invalid file path",
        "Permission denied",
        "Empty input",
      ],
    )
  })
}

/// Generate event beads
fn generate_event_beads(
  session: InterviewSession,
  profile: String,
) -> List(BeadRecord) {
  let event_answers =
    list.filter(session.answers, fn(answer) {
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
      // AI-friendly format
      input_example: "{\"user_id\": \"u123\", \"action\": \"created\", \"timestamp\": \"2026-01-09T12:00:00Z\"}",
      output_example: "Event published to topic: user.created with correlation_id: abc-123",
      must_return: ["Event with valid schema", "Unique correlation_id"],
      must_not: [
        "Emit event without required fields",
        "Block on publish failure",
      ],
      edge_cases: [
        "Duplicate event detection",
        "Consumer offline",
        "Schema version mismatch",
      ],
    )
  })
}

/// Generate data model beads
fn generate_data_beads(
  session: InterviewSession,
  profile: String,
) -> List(BeadRecord) {
  let entity_answers =
    list.filter(session.answers, fn(answer) {
      contains_any_ignore_case(answer.question_text, [
        "entity",
        "data model",
        "schema",
      ])
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
      // AI-friendly format
      input_example: "{\"name\": \"John\", \"email\": \"john@example.com\", \"age\": 30}",
      output_example: "User{id: 1, name: \"John\", email: \"john@example.com\", age: 30, created_at: ...}",
      must_return: [
        "All required fields populated",
        "Auto-generated ID and timestamps",
      ],
      must_not: ["Allow null for required fields", "Store unvalidated data"],
      edge_cases: [
        "Duplicate unique key",
        "Max field length exceeded",
        "Invalid foreign key",
      ],
    )
  })
}

/// Generate workflow beads
fn generate_workflow_beads(
  session: InterviewSession,
  profile: String,
) -> List(BeadRecord) {
  let workflow_answers =
    list.filter(session.answers, fn(answer) {
      contains_any_ignore_case(answer.question_text, [
        "workflow",
        "process",
        "step",
      ])
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
      // AI-friendly format
      input_example: "{\"order_id\": \"ord-123\", \"action\": \"approve\", \"approver\": \"user-456\"}",
      output_example: "{\"order_id\": \"ord-123\", \"status\": \"approved\", \"next_step\": \"ship\"}",
      must_return: ["Updated workflow state", "Next step indication"],
      must_not: [
        "Skip required approval steps",
        "Allow invalid state transitions",
      ],
      edge_cases: [
        "Timeout waiting for approval",
        "Concurrent modifications",
        "Rollback on failure",
      ],
    )
  })
}

/// Generate UI screen beads
fn generate_ui_beads(
  session: InterviewSession,
  profile: String,
) -> List(BeadRecord) {
  let screen_answers =
    list.filter(session.answers, fn(answer) {
      contains_any_ignore_case(answer.question_text, [
        "screen",
        "view",
        "interface",
      ])
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
      // AI-friendly format
      input_example: "User clicks 'Add to Cart' button on product page",
      output_example: "Cart counter increments, toast shows 'Added to cart', button state changes",
      must_return: ["Visual feedback within 100ms", "Updated cart state"],
      must_not: [
        "Block UI during API call",
        "Allow double-click duplicate adds",
      ],
      edge_cases: [
        "Slow network",
        "Item out of stock",
        "Session expired",
        "Mobile landscape",
      ],
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
    #("labels", json.array(bead.labels, json.string)),
    #("ai_hints", json.string(bead.ai_hints)),
    #("acceptance_criteria", json.array(bead.acceptance_criteria, json.string)),
    #("dependencies", json.array(bead.dependencies, json.string)),
    // AI-friendly format fields (BEAD-FORMAT)
    #("input_example", json.string(bead.input_example)),
    #("output_example", json.string(bead.output_example)),
    #("must_return", json.array(bead.must_return, json.string)),
    #("must_not", json.array(bead.must_not, json.string)),
    #("edge_cases", json.array(bead.edge_cases, json.string)),
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
  list.sort(beads, fn(a, b) { int.compare(b.priority, a.priority) })
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
  BeadStats(total: Int, by_type: Dict(String, Int), by_priority: Dict(Int, Int))
}

/// Calculate stats for a list of beads
pub fn bead_stats(beads: List(BeadRecord)) -> BeadStats {
  let total = list.length(beads)

  let by_type =
    list.fold(beads, dict.new(), fn(acc, bead) {
      let current = dict.get(acc, bead.issue_type) |> result.unwrap(0)
      dict.insert(acc, bead.issue_type, current + 1)
    })

  let by_priority =
    list.fold(beads, dict.new(), fn(acc, bead) {
      let current = dict.get(acc, bead.priority) |> result.unwrap(0)
      dict.insert(acc, bead.priority, current + 1)
    })

  BeadStats(total: total, by_type: by_type, by_priority: by_priority)
}

/// Format a single bead for progressive preview display
/// Shows a condensed single-line view of the bead
pub fn format_bead_preview(bead: BeadRecord) -> String {
  let type_badge = case bead.issue_type {
    "api_endpoint" -> "[API]"
    "cli_command" -> "[CLI]"
    "event" -> "[EVT]"
    "data_model" -> "[DAT]"
    "workflow" -> "[WFL]"
    "ui_screen" -> "[UI]"
    _ -> "[???]"
  }

  // Truncate description to 50 chars
  let desc = case string.length(bead.description) > 50 {
    True -> string.slice(bead.description, 0, 47) <> "..."
    False -> bead.description
  }

  type_badge <> " " <> bead.title <> ": " <> desc
}

/// Format beads for progressive preview during interview
/// Shows what beads would be generated based on current answers
pub fn format_progressive_preview(beads: List(BeadRecord), round: Int) -> String {
  case beads {
    [] -> ""
    _ -> {
      let count = list.length(beads)
      let header = case round {
        1 -> "BEAD PREVIEW (rough outline based on Round 1):"
        2 -> "BEAD PREVIEW (refined with scope from Round 2):"
        3 -> "BEAD PREVIEW (error cases added from Round 3):"
        4 -> "BEAD PREVIEW (security hardened from Round 4):"
        5 -> "BEAD PREVIEW (production-ready from Round 5):"
        _ -> "BEAD PREVIEW:"
      }

      let bead_lines =
        beads
        |> list.take(5)
        // Only show first 5 in preview
        |> list.map(fn(b) { "  • " <> format_bead_preview(b) })
        |> string.join("\n")

      let more_indicator = case count > 5 {
        True -> "\n  ... and " <> int.to_string(count - 5) <> " more"
        False -> ""
      }

      "\n┌─────────────────────────────────────────────────────────────────┐\n"
      <> "│ "
      <> header
      <> "\n"
      <> "├─────────────────────────────────────────────────────────────────┤\n"
      <> bead_lines
      <> more_indicator
      <> "\n"
      <> "└─────────────────────────────────────────────────────────────────┘\n"
    }
  }
}
