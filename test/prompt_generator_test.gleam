//// Tests for the Implementation Prompt Generator
////
//// Tests cover:
//// - Context building
//// - Prompt generation from beads
//// - JSON output format
//// - Text output format

import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import intent/bead_templates.{type BeadRecord, BeadRecord}
import intent/prompt_generator.{
  CodebaseContext, FileContext, ImplementationPrompt, LineReference,
  file_with_lines, file_with_snippet, generate_gleam_prompt,
  generate_implementation_prompt, generate_minimal_prompt, new_codebase_context,
  new_file_context, new_line_reference, prompt_to_action_json, prompt_to_json,
  prompt_to_text, with_constraints, with_directories, with_entry_points,
  with_patterns, with_related_files, with_tech_stack, with_tests,
}

// =============================================================================
// CONTEXT BUILDING TESTS
// =============================================================================

pub fn new_codebase_context_test() {
  let ctx = new_codebase_context("TestProject")

  ctx.project_name
  |> should.equal("TestProject")

  ctx.tech_stack
  |> should.equal([])

  ctx.patterns_to_follow
  |> should.equal([])
}

pub fn with_tech_stack_test() {
  let ctx =
    new_codebase_context("TestProject")
    |> with_tech_stack(["Gleam", "Erlang"])
    |> with_tech_stack(["OTP"])

  ctx.tech_stack
  |> should.equal(["Gleam", "Erlang", "OTP"])
}

pub fn with_patterns_test() {
  let ctx =
    new_codebase_context("TestProject")
    |> with_patterns(["Pattern1"])
    |> with_patterns(["Pattern2", "Pattern3"])

  ctx.patterns_to_follow
  |> should.equal(["Pattern1", "Pattern2", "Pattern3"])
}

pub fn with_constraints_test() {
  let ctx =
    new_codebase_context("TestProject")
    |> with_constraints(["No mutable state"])

  ctx.constraints
  |> should.equal(["No mutable state"])
}

pub fn new_file_context_test() {
  let file = new_file_context("src/main.gleam", "gleam", "Entry point")

  file.path
  |> should.equal("src/main.gleam")

  file.language
  |> should.equal("gleam")

  file.purpose
  |> should.equal("Entry point")

  file.content_snippet
  |> should.equal(None)

  file.relevant_lines
  |> should.equal(None)
}

pub fn file_with_snippet_test() {
  let file =
    new_file_context("src/main.gleam", "gleam", "Entry point")
    |> file_with_snippet("pub fn main() { ... }")

  file.content_snippet
  |> should.equal(Some("pub fn main() { ... }"))
}

pub fn file_with_lines_test() {
  let line1 = new_line_reference(10, "pub fn main()", "Entry function")
  let line2 = new_line_reference(20, "import gleam/io", "I/O module")

  let file =
    new_file_context("src/main.gleam", "gleam", "Entry point")
    |> file_with_lines([line1, line2])

  case file.relevant_lines {
    Some(lines) -> {
      list.length(lines)
      |> should.equal(2)
    }
    None -> should.fail()
  }
}

pub fn with_entry_points_test() {
  let file = new_file_context("src/main.gleam", "gleam", "Entry point")

  let ctx =
    new_codebase_context("TestProject")
    |> with_entry_points([file])

  list.length(ctx.entry_points)
  |> should.equal(1)
}

pub fn with_related_files_test() {
  let file1 = new_file_context("src/lib.gleam", "gleam", "Library")
  let file2 = new_file_context("src/utils.gleam", "gleam", "Utilities")

  let ctx =
    new_codebase_context("TestProject")
    |> with_related_files([file1, file2])

  list.length(ctx.related_files)
  |> should.equal(2)
}

pub fn with_tests_test() {
  let ctx =
    new_codebase_context("TestProject")
    |> with_tests(["test/main_test.gleam", "test/lib_test.gleam"])

  ctx.existing_tests
  |> should.equal(["test/main_test.gleam", "test/lib_test.gleam"])
}

pub fn with_directories_test() {
  let ctx =
    new_codebase_context("TestProject")
    |> with_directories(["src/", "test/", "lib/"])

  ctx.directory_structure
  |> should.equal(["src/", "test/", "lib/"])
}

// =============================================================================
// PROMPT GENERATION TESTS
// =============================================================================

fn create_test_bead() -> BeadRecord {
  BeadRecord(
    title: "Implement user authentication",
    description: "Add JWT-based authentication to the API",
    profile_type: "api",
    priority: 1,
    issue_type: "api_endpoint",
    labels: ["auth", "api", "security"],
    ai_hints: "Use existing JWT library for token generation",
    acceptance_criteria: [
      "JWT tokens are generated on login",
      "Tokens expire after 24 hours",
      "Invalid tokens return 401",
    ],
    dependencies: [],
  )
}

pub fn generate_implementation_prompt_test() {
  let bead = create_test_bead()
  let ctx =
    new_codebase_context("AuthAPI")
    |> with_tech_stack(["Gleam", "Erlang"])
    |> with_patterns(["Use Result types"])

  let prompt = generate_implementation_prompt(bead, ctx)

  // Check bead_id is set
  string.contains(prompt.bead_id, "1")
  |> should.be_true

  // Check task summary includes title
  string.contains(prompt.task_summary, "authentication")
  |> should.be_true

  // Check context includes project name
  string.contains(prompt.context_section, "AuthAPI")
  |> should.be_true

  // Check acceptance criteria is preserved
  list.length(prompt.acceptance_criteria)
  |> should.equal(3)

  // Check requirements are generated
  { list.length(prompt.requirements) > 0 }
  |> should.be_true
  // At least some requirements

  // Check pitfalls are generated
  { list.length(prompt.pitfalls_to_avoid) > 0 }
  |> should.be_true
}

pub fn generate_minimal_prompt_test() {
  let bead = create_test_bead()
  let prompt = generate_minimal_prompt(bead)

  // Check context section is present
  string.contains(prompt.context_section, "Unknown Project")
  |> should.be_true

  // Check patterns are included
  { list.length(prompt.pitfalls_to_avoid) > 0 }
  |> should.be_true
}

pub fn generate_gleam_prompt_test() {
  let bead = create_test_bead()
  let prompt = generate_gleam_prompt(bead, "IntentCLI")

  // Check Gleam-specific tech stack
  string.contains(prompt.context_section, "Gleam")
  |> should.be_true

  // Check Gleam-specific constraints in pitfalls
  let has_immutability =
    list.any(prompt.pitfalls_to_avoid, fn(p) {
      string.contains(p, "mutable") || string.contains(p, "Constraint")
    })
  has_immutability
  |> should.be_true
}

// =============================================================================
// JSON OUTPUT TESTS
// =============================================================================

pub fn prompt_to_json_test() {
  let bead = create_test_bead()
  let ctx = new_codebase_context("TestProject")
  let prompt = generate_implementation_prompt(bead, ctx)

  let json_output = prompt_to_json(prompt)
  let json_str = json.to_string(json_output)

  // Check required fields are present
  string.contains(json_str, "bead_id")
  |> should.be_true

  string.contains(json_str, "task_summary")
  |> should.be_true

  string.contains(json_str, "requirements")
  |> should.be_true

  string.contains(json_str, "acceptance_criteria")
  |> should.be_true

  string.contains(json_str, "pitfalls_to_avoid")
  |> should.be_true
}

pub fn prompt_to_action_json_test() {
  let bead = create_test_bead()
  let ctx = new_codebase_context("TestProject")
  let prompt = generate_implementation_prompt(bead, ctx)

  let json_output = prompt_to_action_json(prompt)
  let json_str = json.to_string(json_output)

  // Check action-based format
  string.contains(json_str, "\"action\"")
  |> should.be_true

  string.contains(json_str, "implementation_prompt")
  |> should.be_true

  string.contains(json_str, "\"command\"")
  |> should.be_true

  string.contains(json_str, "\"metadata\"")
  |> should.be_true
}

pub fn file_context_json_includes_lines_test() {
  let line = new_line_reference(42, "pub fn main()", "Entry point")
  let file =
    new_file_context("src/main.gleam", "gleam", "Entry point")
    |> file_with_lines([line])
    |> file_with_snippet("pub fn main() { io.println(\"Hello\") }")

  let ctx =
    new_codebase_context("TestProject")
    |> with_entry_points([file])

  let bead = create_test_bead()
  let prompt = generate_implementation_prompt(bead, ctx)
  let json_output = prompt_to_json(prompt)
  let json_str = json.to_string(json_output)

  // Check file context includes path
  string.contains(json_str, "src/main.gleam")
  |> should.be_true

  // Check snippet is included
  string.contains(json_str, "snippet")
  |> should.be_true

  // Check line references are included
  string.contains(json_str, "relevant_lines")
  |> should.be_true

  string.contains(json_str, "42")
  |> should.be_true
}

// =============================================================================
// TEXT OUTPUT TESTS
// =============================================================================

pub fn prompt_to_text_test() {
  let bead = create_test_bead()
  let ctx =
    new_codebase_context("TestProject")
    |> with_tech_stack(["Gleam"])
    |> with_patterns(["Use Result types"])

  let prompt = generate_implementation_prompt(bead, ctx)
  let text = prompt_to_text(prompt)

  // Check sections are present
  string.contains(text, "IMPLEMENTATION PROMPT")
  |> should.be_true

  string.contains(text, "TASK SUMMARY")
  |> should.be_true

  string.contains(text, "CODEBASE CONTEXT")
  |> should.be_true

  string.contains(text, "REQUIREMENTS")
  |> should.be_true

  string.contains(text, "ACCEPTANCE CRITERIA")
  |> should.be_true

  string.contains(text, "PITFALLS TO AVOID")
  |> should.be_true

  string.contains(text, "VERIFICATION STEPS")
  |> should.be_true
}

pub fn text_output_includes_dividers_test() {
  let bead = create_test_bead()
  let ctx = new_codebase_context("TestProject")
  let prompt = generate_implementation_prompt(bead, ctx)
  let text = prompt_to_text(prompt)

  // Check dividers are present
  string.contains(text, "═══")
  |> should.be_true

  string.contains(text, "───")
  |> should.be_true
}

pub fn text_output_numbered_lists_test() {
  let bead = create_test_bead()
  let ctx = new_codebase_context("TestProject")
  let prompt = generate_implementation_prompt(bead, ctx)
  let text = prompt_to_text(prompt)

  // Check numbered lists in requirements
  string.contains(text, "1. ")
  |> should.be_true

  string.contains(text, "2. ")
  |> should.be_true
}

pub fn text_output_bullet_lists_test() {
  let bead = create_test_bead()
  let ctx = new_codebase_context("TestProject")
  let prompt = generate_implementation_prompt(bead, ctx)
  let text = prompt_to_text(prompt)

  // Check bullet lists in pitfalls
  string.contains(text, "- ")
  |> should.be_true
}

// =============================================================================
// ISSUE TYPE SPECIFIC TESTS
// =============================================================================

pub fn cli_command_requirements_test() {
  let bead =
    BeadRecord(
      title: "Add list command",
      description: "List all items",
      profile_type: "cli",
      priority: 2,
      issue_type: "cli_command",
      labels: ["cli"],
      ai_hints: "Use glint for argument parsing",
      acceptance_criteria: ["Command outputs JSON"],
      dependencies: [],
    )

  let ctx = new_codebase_context("MyCLI")
  let prompt = generate_implementation_prompt(bead, ctx)

  // Check CLI-specific requirements are included
  let has_cli_requirement =
    list.any(prompt.requirements, fn(r) {
      string.contains(r, "argument") || string.contains(r, "help")
    })
  has_cli_requirement
  |> should.be_true
}

pub fn data_model_requirements_test() {
  let bead =
    BeadRecord(
      title: "User entity",
      description: "Define user data model",
      profile_type: "data",
      priority: 3,
      issue_type: "data_model",
      labels: ["data"],
      ai_hints: "Use UUID for IDs",
      acceptance_criteria: ["Schema validates email"],
      dependencies: [],
    )

  let ctx = new_codebase_context("MyDB")
  let prompt = generate_implementation_prompt(bead, ctx)

  // Check data model-specific requirements
  let has_data_requirement =
    list.any(prompt.requirements, fn(r) {
      string.contains(r, "schema") || string.contains(r, "validation")
    })
  has_data_requirement
  |> should.be_true
}
