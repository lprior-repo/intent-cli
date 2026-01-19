//// Bead-to-Prompt Pipeline
////
//// Converts work items (beads) into AI-ready prompts for automated implementation.
//// Supports multiple output templates optimized for different AI agents.
////
//// Pipeline: Interview -> Beads -> Prompts -> AI Implementation

import gleam/dict
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import intent/bead_templates.{type BeadRecord}
import intent/json_output
import simplifile

/// AI-ready prompt generated from a bead
pub type Prompt {
  Prompt(
    /// Unique identifier for the prompt (derived from bead)
    id: String,
    /// Role context for the AI (e.g., "You are a backend engineer...")
    role: String,
    /// The main task to accomplish
    task: String,
    /// Background context about the work item
    context: String,
    /// Step-by-step implementation instructions
    instructions: List(String),
    /// What success looks like (from acceptance_criteria)
    acceptance_criteria: List(String),
    /// Additional hints for the AI
    hints: String,
    /// Guardrail block with safety instructions, scope limitations, and constraints
    guardrail: String,
    /// Dependencies that must be completed first
    dependencies: List(String),
    /// Original bead metadata
    metadata: PromptMetadata,
  )
}

/// Metadata about the original bead
pub type PromptMetadata {
  PromptMetadata(
    profile_type: String,
    issue_type: String,
    priority: Int,
    labels: List(String),
  )
}

/// Output template format
pub type PromptTemplate {
  /// Human-readable format for developers
  Human
  /// Generic AI agent format (default)
  AI
  /// Optimized for Claude with structured sections
  Claude
  /// Minimal format with just essential info
  Minimal
}

// =============================================================================
// CORE: Bead to Prompt Conversion
// =============================================================================

// =============================================================================
// GUARDRAIL BLOCK
// =============================================================================

const guardrail_block = "<guardrail>
## Safety Instructions
- Do not modify code unrelated to the task
- Do not commit secrets, credentials, or API keys
- Do not bypass security checks or validation
- Do not introduce SQL injection, XSS, or other vulnerabilities
- Do not remove or disable logging, monitoring, or alerting
- Always validate inputs and handle errors gracefully

## Scope Limitations
- Only implement the specific feature described in this task
- Do not add unplanned features or functionality
- Do not refactor unrelated code or change coding style
- Do not update dependencies unless explicitly required
- Do not change public APIs unless specified in acceptance criteria

## Constraints
- Follow existing code patterns and conventions
- Maintain backward compatibility where applicable
- Write tests for new functionality
- Document your implementation
- Ensure code compiles without warnings
- Use the tech stack and libraries already in use
</guardrail>"

/// Convert a single BeadRecord into an AI-ready Prompt
pub fn bead_to_prompt(bead: BeadRecord, index: Int) -> Prompt {
  let role = get_role_for_type(bead.issue_type, bead.profile_type)
  let instructions = get_instructions_for_type(bead.issue_type)

  Prompt(
    id: "prompt-" <> int.to_string(index + 1),
    role: role,
    task: bead.title <> ": " <> truncate(bead.description, 200),
    context: build_context(bead),
    instructions: instructions,
    acceptance_criteria: bead.acceptance_criteria,
    hints: bead.ai_hints,
    guardrail: guardrail_block,
    dependencies: bead.dependencies,
    metadata: PromptMetadata(
      profile_type: bead.profile_type,
      issue_type: bead.issue_type,
      priority: bead.priority,
      labels: bead.labels,
    ),
  )
}

/// Convert multiple beads to prompts
pub fn beads_to_prompts(beads: List(BeadRecord)) -> List(Prompt) {
  list.index_map(beads, fn(bead, index) { bead_to_prompt(bead, index) })
}

// =============================================================================
// ROLE GENERATION
// =============================================================================

/// Get the appropriate role context based on issue type
fn get_role_for_type(issue_type: String, profile_type: String) -> String {
  case issue_type {
    "api_endpoint" ->
      "You are a senior backend engineer implementing a RESTful API endpoint. "
      <> "Follow best practices for error handling, validation, and documentation."
    "cli_command" ->
      "You are a systems engineer implementing a CLI command. "
      <> "Focus on clear help text, intuitive flags, and helpful error messages."
    "event" ->
      "You are an integration engineer implementing event-driven architecture. "
      <> "Ensure proper schema design, error handling, and idempotency."
    "data_model" ->
      "You are a database engineer designing and implementing a data model. "
      <> "Consider normalization, indexing, and migration strategies."
    "workflow" ->
      "You are a workflow engineer implementing an automated process. "
      <> "Design for reliability, observability, and error recovery."
    "ui_screen" ->
      "You are a frontend engineer building a user interface. "
      <> "Prioritize accessibility, responsiveness, and user experience."
    _ ->
      "You are a software engineer implementing a feature for a "
      <> profile_type
      <> " system. "
      <> "Follow best practices and ensure comprehensive testing."
  }
}

// =============================================================================
// INSTRUCTIONS BY TYPE
// =============================================================================

/// Get implementation instructions based on issue type
fn get_instructions_for_type(issue_type: String) -> List(String) {
  case issue_type {
    "api_endpoint" -> [
      "1. Define the request/response schema with proper types",
      "2. Implement input validation with clear error messages",
      "3. Handle all error cases (400, 401, 403, 404, 500)",
      "4. Add OpenAPI/Swagger documentation",
      "5. Write unit tests covering happy path and edge cases",
      "6. Add integration tests for the full request cycle",
      "7. Document rate limits and authentication requirements",
    ]
    "cli_command" -> [
      "1. Define command syntax and all flags/options",
      "2. Write comprehensive --help output",
      "3. Implement argument parsing and validation",
      "4. Handle stdin/stdout/stderr appropriately",
      "5. Support both human-readable and JSON output (--json flag)",
      "6. Implement proper exit codes (0=success, 1=error)",
      "7. Add shell completion scripts if applicable",
    ]
    "event" -> [
      "1. Define the event schema with version field",
      "2. Implement the event producer with proper serialization",
      "3. Create consumer handlers with error handling",
      "4. Ensure idempotent processing (handle duplicates)",
      "5. Add dead-letter queue handling for failures",
      "6. Implement event validation on both sides",
      "7. Add monitoring/metrics for event flow",
    ]
    "data_model" -> [
      "1. Design the schema with proper normalization",
      "2. Create database migration scripts",
      "3. Add appropriate indexes for query patterns",
      "4. Implement the repository/DAO layer",
      "5. Add validation constraints at database level",
      "6. Write seed data for development/testing",
      "7. Document relationships and constraints",
    ]
    "workflow" -> [
      "1. Design the state machine with all transitions",
      "2. Implement each step with proper error handling",
      "3. Add retry logic with exponential backoff",
      "4. Implement compensation/rollback for failures",
      "5. Add logging and observability hooks",
      "6. Create monitoring dashboard/alerts",
      "7. Document manual intervention procedures",
    ]
    "ui_screen" -> [
      "1. Create wireframe/mockup of the screen",
      "2. Build component hierarchy with proper props",
      "3. Implement responsive design (mobile-first)",
      "4. Add loading states and error handling",
      "5. Ensure WCAG 2.1 accessibility compliance",
      "6. Write component tests",
      "7. Add E2E tests for critical user flows",
    ]
    _ -> [
      "1. Understand the requirements from the description",
      "2. Design the solution with extensibility in mind",
      "3. Implement with proper error handling",
      "4. Write comprehensive tests",
      "5. Document the implementation",
      "6. Review for security concerns",
      "7. Verify acceptance criteria are met",
    ]
  }
}

// =============================================================================
// CONTEXT BUILDING
// =============================================================================

/// Build context string from bead information
fn build_context(bead: BeadRecord) -> String {
  let type_context = case bead.issue_type {
    "api_endpoint" -> "This is an API endpoint implementation task."
    "cli_command" -> "This is a CLI command implementation task."
    "event" -> "This is an event/messaging system task."
    "data_model" -> "This is a data model/schema design task."
    "workflow" -> "This is a workflow/automation task."
    "ui_screen" -> "This is a UI/frontend implementation task."
    _ -> "This is a " <> bead.issue_type <> " task."
  }

  let priority_context = case bead.priority {
    4 -> "Priority: HIGH - This is a foundational component."
    3 -> "Priority: MEDIUM-HIGH - Core feature implementation."
    2 -> "Priority: MEDIUM - Important but not blocking."
    1 -> "Priority: LOW - Nice to have."
    _ -> "Priority: " <> int.to_string(bead.priority)
  }

  let label_context = case bead.labels {
    [] -> ""
    labels -> "Labels: " <> string.join(labels, ", ")
  }

  [type_context, priority_context, label_context]
  |> list.filter(fn(s) { !string.is_empty(s) })
  |> string.join("\n")
}

// =============================================================================
// OUTPUT FORMATTING
// =============================================================================

/// Format a prompt for human-readable output
pub fn format_prompt_human(prompt: Prompt) -> String {
  let separator =
    "=============================================================================="
  let divider =
    "------------------------------------------------------------------------------"

  let header = separator <> "\nPROMPT: " <> prompt.id <> "\n" <> separator

  let role_section = "\n## Role\n" <> prompt.role

  let task_section = "\n## Task\n" <> prompt.task

  let context_section = "\n## Context\n" <> prompt.context

  let instructions_section =
    "\n## Instructions\n" <> string.join(prompt.instructions, "\n")

  let criteria_section =
    "\n## Acceptance Criteria\n"
    <> list.map(prompt.acceptance_criteria, fn(c) { "- [ ] " <> c })
    |> string.join("\n")

  let hints_section = case prompt.hints {
    "" -> ""
    h -> "\n## AI Hints\n" <> h
  }

  let guardrail_section = "\n## Guardrail\n" <> prompt.guardrail

  let deps_section = case prompt.dependencies {
    [] -> ""
    deps ->
      "\n## Dependencies\n"
      <> list.map(deps, fn(d) { "- " <> d })
      |> string.join("\n")
  }

  [
    header,
    role_section,
    task_section,
    context_section,
    instructions_section,
    criteria_section,
    hints_section,
    guardrail_section,
    deps_section,
    "\n" <> divider,
  ]
  |> list.filter(fn(s) { !string.is_empty(s) })
  |> string.join("\n")
}

/// Format a prompt for Claude-optimized output (structured sections)
pub fn format_prompt_claude(prompt: Prompt) -> String {
  let task_block = "<task>\n" <> prompt.task <> "\n</task>"

  let context_block = "<context>\n" <> prompt.context <> "\n</context>"

  let instructions_block =
    "<instructions>\n"
    <> string.join(prompt.instructions, "\n")
    <> "\n</instructions>"

  let criteria_block =
    "<acceptance_criteria>\n"
    <> list.map(prompt.acceptance_criteria, fn(c) { "- " <> c })
    |> string.join("\n")
    <> "\n</acceptance_criteria>"

  let hints_block = case prompt.hints {
    "" -> ""
    h -> "<hints>\n" <> h <> "\n</hints>"
  }

  let guardrail_block_output =
    "<guardrail_output>\n" <> prompt.guardrail <> "\n</guardrail_output>"

  let deps_block = case prompt.dependencies {
    [] -> ""
    deps -> "<dependencies>\n" <> string.join(deps, "\n") <> "\n</dependencies>"
  }

  [
    prompt.role,
    "",
    task_block,
    "",
    context_block,
    "",
    instructions_block,
    "",
    criteria_block,
    hints_block,
    guardrail_block_output,
    deps_block,
  ]
  |> list.filter(fn(s) { !string.is_empty(s) })
  |> string.join("\n")
}

/// Format a prompt as minimal text
pub fn format_prompt_minimal(prompt: Prompt) -> String {
  let criteria_text =
    prompt.acceptance_criteria
    |> list.map(fn(c) { "- " <> c })
    |> string.join("\n")

  prompt.task
  <> "\n\nAcceptance Criteria:\n"
  <> criteria_text
  <> case prompt.hints {
    "" -> ""
    h -> "\n\nHints: " <> h
  }
  <> "\n\n## Guardrail\n"
  <> "Safety: No secrets, security bypasses, or vulnerabilities\n"
  <> "Scope: Only implement described feature, no unplanned changes\n"
  <> "Constraints: Follow patterns, test, document, compile clean"
}

/// Format prompt using specified template
pub fn format_prompt(prompt: Prompt, template: PromptTemplate) -> String {
  case template {
    Human -> format_prompt_human(prompt)
    AI -> format_prompt_human(prompt)
    Claude -> format_prompt_claude(prompt)
    Minimal -> format_prompt_minimal(prompt)
  }
}

/// Format all prompts using specified template
pub fn format_prompts(prompts: List(Prompt), template: PromptTemplate) -> String {
  prompts
  |> list.map(fn(p) { format_prompt(p, template) })
  |> string.join("\n\n")
}

// =============================================================================
// JSON OUTPUT
// =============================================================================

/// Convert a single prompt to JSON
pub fn prompt_to_json(prompt: Prompt) -> Json {
  json.object([
    #("id", json.string(prompt.id)),
    #("role", json.string(prompt.role)),
    #("task", json.string(prompt.task)),
    #("context", json.string(prompt.context)),
    #("instructions", json.array(prompt.instructions, json.string)),
    #(
      "acceptance_criteria",
      json.array(prompt.acceptance_criteria, json.string),
    ),
    #("hints", json.string(prompt.hints)),
    #("guardrail", json.string(prompt.guardrail)),
    #("dependencies", json.array(prompt.dependencies, json.string)),
    #(
      "metadata",
      json.object([
        #("profile_type", json.string(prompt.metadata.profile_type)),
        #("issue_type", json.string(prompt.metadata.issue_type)),
        #("priority", json.int(prompt.metadata.priority)),
        #("labels", json.array(prompt.metadata.labels, json.string)),
      ]),
    ),
  ])
}

/// Convert prompts to action-based JSON for AI consumption
pub fn prompts_to_action_json(
  prompts: List(Prompt),
  session_id: String,
  template: PromptTemplate,
) -> Json {
  let template_str = case template {
    Human -> "human"
    AI -> "ai"
    Claude -> "claude"
    Minimal -> "minimal"
  }

  let data =
    json.object([
      #("session_id", json.string(session_id)),
      #("template", json.string(template_str)),
      #("prompt_count", json.int(list.length(prompts))),
      #("prompts", json.array(prompts, prompt_to_json)),
    ])

  json_output.create_response("prompts_generated", "bead-prompt", data, None, 0)
  |> json_output.to_json
}

// =============================================================================
// TEMPLATE PARSING
// =============================================================================

/// Parse template string to PromptTemplate type
pub fn parse_template(template_str: String) -> Result(PromptTemplate, String) {
  case string.lowercase(template_str) {
    "human" -> Ok(Human)
    "ai" -> Ok(AI)
    "claude" -> Ok(Claude)
    "minimal" -> Ok(Minimal)
    _ ->
      Error(
        "Unknown template '"
        <> template_str
        <> "'. Valid templates: human, ai, claude, minimal",
      )
  }
}

// =============================================================================
// HELPERS
// =============================================================================

/// Truncate a string to max length with ellipsis
fn truncate(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len - 3) <> "..."
    False -> s
  }
}

// =============================================================================
// EXPORT FUNCTIONS
// =============================================================================

/// Error type for prompt export operations
pub type PromptExportError {
  WriteError(String)
  CreateDirError(String)
  InvalidPath(String)
}

/// Export prompts to a single file with specified template
pub fn export_prompts_to_file(
  prompts: List(Prompt),
  file_path: String,
  template: PromptTemplate,
) -> Result(Nil, PromptExportError) {
  let content = format_prompts(prompts, template)
  case simplifile.write(file_path, content) {
    Ok(Nil) -> Ok(Nil)
    Error(err) ->
      Error(WriteError("Failed to write to " <> file_path <> ": " <> string.inspect(err)))
  }
}

/// Export each prompt to a separate file in a directory
/// Files are named: prompt-001.txt, prompt-002.txt, etc.
pub fn export_prompts_to_directory(
  prompts: List(Prompt),
  dir_path: String,
  template: PromptTemplate,
) -> Result(Nil, PromptExportError) {
  case simplifile.create_directory_all(dir_path) {
    Ok(Nil) -> {
      let results =
        list.index_map(prompts, fn(prompt, index) {
          let index_str = pad_number(index + 1, 3)
          let file_name = "prompt-" <> index_str <> ".txt"
          let file_path = dir_path <> "/" <> file_name
          let content = format_prompt(prompt, template)
          case simplifile.write(file_path, content) {
            Ok(Nil) -> Ok(Nil)
            Error(err) ->
              Error(WriteError(
                "Failed to write " <> file_path <> ": " <> string.inspect(err),
              ))
          }
        })
      case result.all(results) {
        Ok(_) -> Ok(Nil)
        Error(err) -> Error(err)
      }
    }
    Error(err) ->
      Error(CreateDirError(
        "Failed to create directory " <> dir_path <> ": " <> string.inspect(err),
      ))
  }
}

/// Pad number with leading zeros
fn pad_number(n: Int, width: Int) -> String {
  let num_str = int.to_string(n)
  case string.length(num_str) >= width {
    True -> num_str
    False ->
      list.fold(list.range(0, width - string.length(num_str)), num_str, fn(acc, _) {
        "0" <> acc
      })
  }
}

/// Export prompts to JSONL format (one JSON object per line)
pub fn export_prompts_to_jsonl(
  prompts: List(Prompt),
  file_path: String,
) -> Result(Nil, PromptExportError) {
  let json_lines = list.map(prompts, fn(p) {
    prompt_to_json(p) |> json.to_string
  })
  let content = string.join(json_lines, "\n")
  case simplifile.write(file_path, content) {
    Ok(Nil) -> Ok(Nil)
    Error(err) ->
      Error(WriteError("Failed to write to " <> file_path <> ": " <> string.inspect(err)))
  }
}

/// Export a single prompt to a file
pub fn export_single_prompt(
  prompt: Prompt,
  file_path: String,
  template: PromptTemplate,
) -> Result(Nil, PromptExportError) {
  let content = format_prompt(prompt, template)
  case simplifile.write(file_path, content) {
    Ok(Nil) -> Ok(Nil)
    Error(err) ->
      Error(WriteError("Failed to write to " <> file_path <> ": " <> string.inspect(err)))
  }
}

/// Export summary of prompts (metadata only) to JSON
pub fn export_prompt_summary(
  prompts: List(Prompt),
  file_path: String,
) -> Result(Nil, PromptExportError) {
  let summary =
    json.object([
      #("count", json.int(list.length(prompts))),
      #(
        "by_type",
        json.object(
          list.fold(prompts, dict.new(), fn(acc, p) {
            let current = dict.get(acc, p.metadata.issue_type) |> result.unwrap(0)
            dict.insert(acc, p.metadata.issue_type, current + 1)
          })
          |> dict.to_list
          |> list.map(fn(p) { #(p.0, json.int(p.1)) }),
        ),
      ),
      #(
        "by_priority",
        json.object(
          list.fold(prompts, dict.new(), fn(acc, p) {
            let key = int.to_string(p.metadata.priority)
            let current = dict.get(acc, key) |> result.unwrap(0)
            dict.insert(acc, key, current + 1)
          })
          |> dict.to_list
          |> list.map(fn(p) { #(p.0, json.int(p.1)) }),
        ),
      ),
    ])
    |> json.to_string
  case simplifile.write(file_path, summary) {
    Ok(Nil) -> Ok(Nil)
    Error(err) ->
      Error(WriteError("Failed to write to " <> file_path <> ": " <> string.inspect(err)))
  }
}
