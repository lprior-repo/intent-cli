//// Implementation Prompt Generator with Codebase Context
////
//// Generates AI-friendly implementation prompts that include relevant codebase
//// context for efficient task execution. Designed for AI agents to understand
//// both the task requirements and the existing code patterns to follow.
////
//// Architecture: Functional Core / Imperative Shell
//// - All functions in this module are pure (no I/O)
//// - Context gathering is done externally and passed in
//// - JSON output follows the action-based schema from json_output module

import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/bead_templates.{type BeadRecord}
import intent/json_output

/// Represents a file in the codebase that's relevant to implementation
pub type FileContext {
  FileContext(
    path: String,
    language: String,
    purpose: String,
    content_snippet: Option(String),
    relevant_lines: Option(List(LineReference)),
  )
}

/// A reference to a specific line in a file
pub type LineReference {
  LineReference(line_number: Int, content: String, reason: String)
}

/// Represents the overall codebase context for an implementation task
pub type CodebaseContext {
  CodebaseContext(
    project_name: String,
    tech_stack: List(String),
    directory_structure: List(String),
    entry_points: List(FileContext),
    related_files: List(FileContext),
    patterns_to_follow: List(String),
    constraints: List(String),
    existing_tests: List(String),
  )
}

/// The generated implementation prompt with all context
pub type ImplementationPrompt {
  ImplementationPrompt(
    bead_id: String,
    task_summary: String,
    context_section: String,
    requirements: List(String),
    acceptance_criteria: List(String),
    relevant_code: List(FileContext),
    suggested_approach: String,
    pitfalls_to_avoid: List(String),
    guardrail_block: String,
    verification_steps: List(String),
  )
}

// =============================================================================
// CONSTRUCTORS: Create context and prompt types
// =============================================================================

/// Create a new empty codebase context
pub fn new_codebase_context(project_name: String) -> CodebaseContext {
  CodebaseContext(
    project_name: project_name,
    tech_stack: [],
    directory_structure: [],
    entry_points: [],
    related_files: [],
    patterns_to_follow: [],
    constraints: [],
    existing_tests: [],
  )
}

/// Create a file context with basic information
pub fn new_file_context(
  path: String,
  language: String,
  purpose: String,
) -> FileContext {
  FileContext(
    path: path,
    language: language,
    purpose: purpose,
    content_snippet: None,
    relevant_lines: None,
  )
}

/// Create a line reference
pub fn new_line_reference(
  line_number: Int,
  content: String,
  reason: String,
) -> LineReference {
  LineReference(line_number: line_number, content: content, reason: reason)
}

// =============================================================================
// GUARDRAIL BLOCK
// =============================================================================

const guardrail_block = "## GUARDRAIL\n\n### Safety Instructions\n- Do not modify code unrelated to the task\n- Do not commit secrets, credentials, or API keys\n- Do not bypass security checks or validation\n- Do not introduce SQL injection, XSS, or other vulnerabilities\n- Do not remove or disable logging, monitoring, or alerting\n- Always validate inputs and handle errors gracefully\n\n### Scope Limitations\n- Only implement the specific feature described in this task\n- Do not add unplanned features or functionality\n- Do not refactor unrelated code or change coding style\n- Do not update dependencies unless explicitly required\n- Do not change public APIs unless specified in acceptance criteria\n\n### Constraints\n- Follow existing code patterns and conventions\n- Maintain backward compatibility where applicable\n- Write tests for new functionality\n- Document your implementation\n- Ensure code compiles without warnings\n- Use the tech stack and libraries already in use"

// =============================================================================
// BUILDERS: Fluent API for building context
// =============================================================================

/// Add tech stack items to context
pub fn with_tech_stack(
  ctx: CodebaseContext,
  stack: List(String),
) -> CodebaseContext {
  CodebaseContext(..ctx, tech_stack: list.append(ctx.tech_stack, stack))
}

/// Add directory structure to context
pub fn with_directories(
  ctx: CodebaseContext,
  dirs: List(String),
) -> CodebaseContext {
  CodebaseContext(
    ..ctx,
    directory_structure: list.append(ctx.directory_structure, dirs),
  )
}

/// Add entry points to context
pub fn with_entry_points(
  ctx: CodebaseContext,
  files: List(FileContext),
) -> CodebaseContext {
  CodebaseContext(..ctx, entry_points: list.append(ctx.entry_points, files))
}

/// Add related files to context
pub fn with_related_files(
  ctx: CodebaseContext,
  files: List(FileContext),
) -> CodebaseContext {
  CodebaseContext(..ctx, related_files: list.append(ctx.related_files, files))
}

/// Add patterns to follow
pub fn with_patterns(
  ctx: CodebaseContext,
  patterns: List(String),
) -> CodebaseContext {
  CodebaseContext(
    ..ctx,
    patterns_to_follow: list.append(ctx.patterns_to_follow, patterns),
  )
}

/// Add constraints
pub fn with_constraints(
  ctx: CodebaseContext,
  constraints: List(String),
) -> CodebaseContext {
  CodebaseContext(..ctx, constraints: list.append(ctx.constraints, constraints))
}

/// Add existing test paths
pub fn with_tests(ctx: CodebaseContext, tests: List(String)) -> CodebaseContext {
  CodebaseContext(..ctx, existing_tests: list.append(ctx.existing_tests, tests))
}

/// Add a content snippet to a file context
pub fn file_with_snippet(file: FileContext, snippet: String) -> FileContext {
  FileContext(..file, content_snippet: Some(snippet))
}

/// Add relevant lines to a file context
pub fn file_with_lines(
  file: FileContext,
  lines: List(LineReference),
) -> FileContext {
  FileContext(..file, relevant_lines: Some(lines))
}

// =============================================================================
// PROMPT GENERATION: Create implementation prompts
// =============================================================================

/// Generate an implementation prompt from a bead and codebase context
pub fn generate_implementation_prompt(
  bead: BeadRecord,
  context: CodebaseContext,
) -> ImplementationPrompt {
  let task_summary = build_task_summary(bead)
  let context_section = build_context_section(context)
  let requirements = build_requirements(bead, context)
  let suggested_approach = build_suggested_approach(bead, context)
  let pitfalls = build_pitfalls(bead, context)
  let verification = build_verification_steps(bead)

  ImplementationPrompt(
    bead_id: bead.title <> "-" <> int.to_string(bead.priority),
    task_summary: task_summary,
    context_section: context_section,
    requirements: requirements,
    acceptance_criteria: bead.acceptance_criteria,
    relevant_code: list.append(context.entry_points, context.related_files),
    suggested_approach: suggested_approach,
    pitfalls_to_avoid: pitfalls,
    guardrail_block: guardrail_block,
    verification_steps: verification,
  )
}

/// Build the task summary from bead information
fn build_task_summary(bead: BeadRecord) -> String {
  let type_str = case bead.issue_type {
    "api_endpoint" -> "Implement API endpoint"
    "cli_command" -> "Implement CLI command"
    "event" -> "Implement event handler"
    "data_model" -> "Implement data model"
    "workflow" -> "Implement workflow step"
    "ui_screen" -> "Implement UI screen"
    _ -> "Implement feature"
  }

  type_str <> ": " <> bead.title <> "\n\n" <> bead.description
}

/// Build the context section describing the codebase
fn build_context_section(context: CodebaseContext) -> String {
  let stack_str = case context.tech_stack {
    [] -> ""
    stack -> "Tech Stack: " <> string.join(stack, ", ") <> "\n"
  }

  let patterns_str = case context.patterns_to_follow {
    [] -> ""
    patterns ->
      "Patterns to Follow:\n"
      <> string.join(list.map(patterns, fn(p) { "- " <> p }), "\n")
      <> "\n"
  }

  let constraints_str = case context.constraints {
    [] -> ""
    constraints ->
      "Constraints:\n"
      <> string.join(list.map(constraints, fn(c) { "- " <> c }), "\n")
      <> "\n"
  }

  "Project: "
  <> context.project_name
  <> "\n"
  <> stack_str
  <> patterns_str
  <> constraints_str
}

/// Build requirements list from bead and context
fn build_requirements(
  bead: BeadRecord,
  context: CodebaseContext,
) -> List(String) {
  let base_requirements = [
    "Follow existing code patterns in the codebase",
    "Maintain consistency with project conventions",
  ]

  let type_requirements = case bead.issue_type {
    "api_endpoint" -> [
      "Implement proper error handling",
      "Add input validation",
      "Return appropriate status codes",
    ]
    "cli_command" -> [
      "Parse command-line arguments correctly",
      "Provide helpful error messages",
      "Add help text documentation",
    ]
    "event" -> [
      "Define event schema",
      "Implement producer and consumer",
      "Handle event routing",
    ]
    "data_model" -> [
      "Define schema with proper types",
      "Add validation rules",
      "Create repository/access layer",
    ]
    "workflow" -> [
      "Implement state machine",
      "Handle error cases and retries",
      "Add logging and monitoring",
    ]
    "ui_screen" -> [
      "Follow responsive design",
      "Meet accessibility standards",
      "Match existing UI patterns",
    ]
    _ -> []
  }

  let test_requirements = case context.existing_tests {
    [] -> []
    _ -> ["Add tests following existing test patterns"]
  }

  list.flatten([base_requirements, type_requirements, test_requirements])
}

/// Build suggested approach based on bead type and context
fn build_suggested_approach(
  bead: BeadRecord,
  context: CodebaseContext,
) -> String {
  let entry_point_hint = case context.entry_points {
    [] -> ""
    [first, ..] ->
      "Start by examining " <> first.path <> " (" <> first.purpose <> ").\n"
  }

  let related_hint = case context.related_files {
    [] -> ""
    files ->
      "Review related files for patterns:\n"
      <> string.join(
        list.map(files, fn(f) { "- " <> f.path <> ": " <> f.purpose }),
        "\n",
      )
      <> "\n"
  }

  let ai_hint = case bead.ai_hints {
    "" -> ""
    hint -> "\nAI Implementation Hint: " <> hint <> "\n"
  }

  entry_point_hint <> related_hint <> ai_hint
}

/// Build pitfalls to avoid list
fn build_pitfalls(bead: BeadRecord, context: CodebaseContext) -> List(String) {
  let common_pitfalls = [
    "Don't break existing functionality",
    "Don't introduce security vulnerabilities",
    "Don't skip error handling",
  ]

  let type_pitfalls = case bead.issue_type {
    "api_endpoint" -> [
      "Don't expose internal error details to clients",
      "Don't forget rate limiting considerations",
    ]
    "cli_command" -> [
      "Don't use hardcoded paths",
      "Don't ignore platform differences",
    ]
    "data_model" -> [
      "Don't use nullable fields without reason",
      "Don't skip migration scripts",
    ]
    _ -> []
  }

  let constraint_pitfalls =
    list.map(context.constraints, fn(c) { "Constraint: " <> c })

  list.flatten([common_pitfalls, type_pitfalls, constraint_pitfalls])
}

/// Build verification steps from acceptance criteria
fn build_verification_steps(bead: BeadRecord) -> List(String) {
  let criteria_steps =
    list.map(bead.acceptance_criteria, fn(c) { "Verify: " <> c })

  let standard_steps = [
    "Run existing tests to ensure no regressions",
    "Test edge cases and error conditions",
    "Review code for consistency with project style",
  ]

  list.append(criteria_steps, standard_steps)
}

// =============================================================================
// JSON OUTPUT: Convert prompts to JSON for AI consumption
// =============================================================================

/// Convert an implementation prompt to JSON
pub fn prompt_to_json(prompt: ImplementationPrompt) -> Json {
  json.object([
    #("bead_id", json.string(prompt.bead_id)),
    #("task_summary", json.string(prompt.task_summary)),
    #("context", json.string(prompt.context_section)),
    #("requirements", json.array(prompt.requirements, json.string)),
    #(
      "acceptance_criteria",
      json.array(prompt.acceptance_criteria, json.string),
    ),
    #("relevant_code", json.array(prompt.relevant_code, file_context_to_json)),
    #("suggested_approach", json.string(prompt.suggested_approach)),
    #("pitfalls_to_avoid", json.array(prompt.pitfalls_to_avoid, json.string)),
    #("guardrail_block", json.string(prompt.guardrail_block)),
    #("verification_steps", json.array(prompt.verification_steps, json.string)),
  ])
}

/// Convert a file context to JSON
fn file_context_to_json(file: FileContext) -> Json {
  let base = [
    #("path", json.string(file.path)),
    #("language", json.string(file.language)),
    #("purpose", json.string(file.purpose)),
  ]

  let with_snippet = case file.content_snippet {
    None -> base
    Some(snippet) -> list.append(base, [#("snippet", json.string(snippet))])
  }

  let with_lines = case file.relevant_lines {
    None -> with_snippet
    Some(lines) ->
      list.append(with_snippet, [
        #("relevant_lines", json.array(lines, line_reference_to_json)),
      ])
  }

  json.object(with_lines)
}

/// Convert a line reference to JSON
fn line_reference_to_json(line: LineReference) -> Json {
  json.object([
    #("line", json.int(line.line_number)),
    #("content", json.string(line.content)),
    #("reason", json.string(line.reason)),
  ])
}

/// Convert implementation prompt to action-based JSON response
pub fn prompt_to_action_json(prompt: ImplementationPrompt) -> Json {
  let data = prompt_to_json(prompt)
  json_output.create_response(
    "implementation_prompt",
    "prompt-generate",
    data,
    None,
    0,
  )
  |> json_output.to_json
}

// =============================================================================
// TEXT OUTPUT: Human-readable prompt format
// =============================================================================

/// Convert an implementation prompt to formatted text
pub fn prompt_to_text(prompt: ImplementationPrompt) -> String {
  let divider =
    "═══════════════════════════════════════════════════════════════════\n"
  let section_divider =
    "───────────────────────────────────────────────────────────────────\n"

  let header =
    divider
    <> "IMPLEMENTATION PROMPT: "
    <> prompt.bead_id
    <> "\n"
    <> divider
    <> "\n"

  let task =
    "TASK SUMMARY\n" <> section_divider <> prompt.task_summary <> "\n\n"

  let context =
    "CODEBASE CONTEXT\n" <> section_divider <> prompt.context_section <> "\n"

  let requirements =
    "REQUIREMENTS\n"
    <> section_divider
    <> format_numbered_list(prompt.requirements)
    <> "\n"

  let criteria =
    "ACCEPTANCE CRITERIA\n"
    <> section_divider
    <> format_numbered_list(prompt.acceptance_criteria)
    <> "\n"

  let code =
    "RELEVANT CODE\n"
    <> section_divider
    <> format_file_contexts(prompt.relevant_code)
    <> "\n"

  let approach =
    "SUGGESTED APPROACH\n"
    <> section_divider
    <> prompt.suggested_approach
    <> "\n"

  let pitfalls =
    "PITFALLS TO AVOID\n"
    <> section_divider
    <> format_bullet_list(prompt.pitfalls_to_avoid)
    <> "\n"

  let guardrail =
    "GUARDRAIL\n" <> section_divider <> prompt.guardrail_block <> "\n"

  let verification =
    "VERIFICATION STEPS\n"
    <> section_divider
    <> format_numbered_list(prompt.verification_steps)
    <> "\n"

  header
  <> task
  <> context
  <> requirements
  <> criteria
  <> code
  <> approach
  <> pitfalls
  <> guardrail
  <> verification
  <> divider
}

/// Format a numbered list
fn format_numbered_list(items: List(String)) -> String {
  items
  |> list.index_map(fn(item, i) { int.to_string(i + 1) <> ". " <> item })
  |> string.join("\n")
}

/// Format a bullet list
fn format_bullet_list(items: List(String)) -> String {
  items
  |> list.map(fn(item) { "- " <> item })
  |> string.join("\n")
}

/// Format file contexts for display
fn format_file_contexts(files: List(FileContext)) -> String {
  files
  |> list.map(fn(file) {
    let header = "  " <> file.path <> " [" <> file.language <> "]\n"
    let purpose = "    Purpose: " <> file.purpose <> "\n"

    let snippet = case file.content_snippet {
      None -> ""
      Some(s) ->
        "    Snippet:\n      " <> string.replace(s, "\n", "\n      ") <> "\n"
    }

    let lines = case file.relevant_lines {
      None -> ""
      Some(refs) ->
        "    Key Lines:\n"
        <> string.join(
          list.map(refs, fn(r) {
            "      L"
            <> int.to_string(r.line_number)
            <> ": "
            <> r.content
            <> " ("
            <> r.reason
            <> ")"
          }),
          "\n",
        )
        <> "\n"
    }

    header <> purpose <> snippet <> lines
  })
  |> string.join("\n")
}

// =============================================================================
// CONVENIENCE: Quick prompt generation
// =============================================================================

/// Generate a minimal implementation prompt from just a bead
/// Uses default context with basic patterns
pub fn generate_minimal_prompt(bead: BeadRecord) -> ImplementationPrompt {
  let context =
    new_codebase_context("Unknown Project")
    |> with_patterns([
      "Follow existing code style",
      "Add appropriate error handling",
      "Include documentation comments",
    ])
    |> with_constraints(["Maintain backward compatibility"])

  generate_implementation_prompt(bead, context)
}

/// Generate a Gleam-specific implementation prompt
pub fn generate_gleam_prompt(
  bead: BeadRecord,
  project_name: String,
) -> ImplementationPrompt {
  let context =
    new_codebase_context(project_name)
    |> with_tech_stack(["Gleam", "Erlang/OTP", "BEAM VM"])
    |> with_patterns([
      "Use Result types for error handling (no exceptions)",
      "Prefer pure functions (Functional Core / Imperative Shell)",
      "Use exhaustive pattern matching",
      "Follow Gleam formatting conventions (gleam format)",
      "Use labeled arguments for clarity",
      "Prefer pipelines for data transformation",
    ])
    |> with_constraints([
      "No mutable state",
      "No null values (use Option)",
      "No panics or crashes in production code",
      "Type safety must be maintained",
    ])

  generate_implementation_prompt(bead, context)
}
