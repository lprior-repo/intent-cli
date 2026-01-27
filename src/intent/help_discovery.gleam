/// Help Discovery Module
/// Provides improved command discovery with grouping and workflow guidance
/// Bead: intent-cli-tmx
import gleam/list
import gleam/string

// =============================================================================
// Public Types
// =============================================================================

/// Command group with name, description, and list of commands
pub type CommandGroup {
  CommandGroup(name: String, description: String, commands: List(String))
}

/// Common workflow with name, description, and step-by-step commands
pub type Workflow {
  Workflow(name: String, description: String, steps: List(String))
}

// =============================================================================
// Command Groups
// =============================================================================

/// Get all command groups organized by functionality
pub fn get_command_groups() -> List(CommandGroup) {
  [
    CommandGroup(
      name: "Core Spec Operations",
      description: "Basic spec validation, display, and improvement commands",
      commands: [
        "validate",
        "show",
        "export",
        "lint",
        "analyze",
        "improve",
        "doctor",
        "diff",
      ],
    ),
    CommandGroup(
      name: "KIRK Analysis",
      description: "Advanced analysis using KIRK methodology (quality, coverage, gaps, etc.)",
      commands: [
        "quality",
        "coverage",
        "gaps",
        "invert",
        "effects",
        "ears",
        "parse",
      ],
    ),
    CommandGroup(
      name: "Interview Workflow",
      description: "Interactive spec discovery through guided interviews",
      commands: ["interview", "sessions", "history", "export"],
    ),
    CommandGroup(
      name: "Planning & Beads",
      description: "Work decomposition, planning, and bead generation",
      commands: [
        "plan",
        "plan-approve",
        "beads",
        "beads-regenerate",
        "bead-status",
        "prompt",
        "feedback",
      ],
    ),
    CommandGroup(
      name: "Vision Phase",
      description: "Early-phase spec critique and alignment",
      commands: [
        "vision start",
        "vision check",
        "vision critique",
        "vision respond",
        "vision agree",
      ],
    ),
    CommandGroup(
      name: "Spec Phase",
      description: "Spec phase commands for detailed specification",
      commands: [
        "spec start",
        "spec check",
        "spec critique",
        "spec respond",
        "spec agree",
      ],
    ),
    CommandGroup(
      name: "Shape Phase",
      description: "Shape phase commands for final agreement",
      commands: [
        "shape start",
        "shape check",
        "shape critique",
        "shape respond",
        "shape agree",
      ],
    ),
    CommandGroup(
      name: "AI Commands",
      description: "AI-specific utilities and schema generation",
      commands: ["ai schema", "ai aggregate"],
    ),
  ]
}

// =============================================================================
// Workflow Hints
// =============================================================================

/// Get workflow hint for a specific command
/// Returns contextual guidance about what to do next
pub fn get_workflow_hint(command: String) -> String {
  case command {
    "interview" ->
      "Next: List sessions with 'intent sessions' or export spec with 'intent export <session-id>'"

    "sessions" ->
      "Next: Start new interview with 'intent interview --profile api' or resume a session"

    "export" ->
      "Next: Validate spec with 'intent validate <spec>' or analyze quality with 'intent quality <spec>'"

    "validate" ->
      "Next: Show spec details with 'intent show <spec>' or check quality with 'intent quality <spec>'"

    "quality" ->
      "Next: Check coverage with 'intent coverage <spec>' or find gaps with 'intent gaps <spec>'"

    "coverage" ->
      "Next: Find gaps with 'intent gaps <spec>' or analyze failure modes with 'intent invert <spec>'"

    "gaps" ->
      "Next: Analyze failure modes with 'intent invert <spec>' or check second-order effects with 'intent effects <spec>'"

    "invert" ->
      "Next: Check effects with 'intent effects <spec>' or generate beads with 'intent beads <session-id>'"

    "effects" ->
      "Next: Generate beads with 'intent beads <session-id>' or create plan with 'intent plan <session-id>'"

    "beads" ->
      "Next: Create plan with 'intent plan <session-id>' or generate AI prompts with 'intent prompt <session-id>'"

    "plan" ->
      "Next: Approve plan with 'intent plan-approve <session-id>' or generate implementation prompts with 'intent prompt <session-id>'"

    "prompt" ->
      "Next: Use prompts with AI implementation tools or provide feedback with 'intent feedback --results <output.json>'"

    "feedback" ->
      "Next: Regenerate beads with 'intent beads-regenerate <spec>' or validate fixes with 'intent validate <spec>'"

    _ -> ""
  }
}

// =============================================================================
// Common Workflows
// =============================================================================

/// Get common workflow examples
/// Returns step-by-step workflows for typical use cases
pub fn get_common_workflows() -> List(Workflow) {
  [
    Workflow(
      name: "New Spec from Interview",
      description: "Create a new spec from scratch using guided interview",
      steps: [
        "intent interview --profile api",
        "intent sessions",
        "intent export <session-id> --output spec.cue",
        "intent validate spec.cue",
        "intent show spec.cue",
      ],
    ),
    Workflow(
      name: "Spec Analysis",
      description: "Analyze existing spec for quality and coverage",
      steps: [
        "intent validate spec.cue",
        "intent quality spec.cue",
        "intent coverage spec.cue",
        "intent gaps spec.cue",
        "intent invert spec.cue",
        "intent effects spec.cue",
      ],
    ),
    Workflow(
      name: "Planning & Beads",
      description: "Generate work items and plan from spec",
      steps: [
        "intent interview --profile api",
        "intent beads <session-id>",
        "intent plan <session-id>",
        "intent plan-approve <session-id>",
        "intent prompt <session-id>",
      ],
    ),
    Workflow(
      name: "Quality Improvement",
      description: "Improve spec quality based on analysis",
      steps: [
        "intent doctor spec.cue",
        "intent improve spec.cue",
        "intent lint spec.cue",
        "intent validate spec.cue",
      ],
    ),
  ]
}

// =============================================================================
// Help Text Formatting
// =============================================================================

/// Format help text with command groups and workflows
pub fn format_help_text() -> String {
  let groups = get_command_groups()
  let workflows = get_common_workflows()

  let header_lines = [
    "Intent CLI - Contract-Driven API Testing",
    "",
    "Usage:",
    "  intent <command> [options]",
    "  intent <command> --help  # Show detailed help for a command",
    "",
    "Command Groups:",
  ]

  let group_lines = format_command_groups(groups)

  let workflow_header = ["", "Common Workflows:"]

  let workflow_lines = format_workflows(workflows)

  let footer_lines = [
    "",
    "For more information on a specific command:",
    "  intent help <command>",
    "  intent <command> --help",
  ]

  list.concat([
    header_lines,
    group_lines,
    workflow_header,
    workflow_lines,
    footer_lines,
  ])
  |> string.join("\n")
}

/// Format command groups for display
fn format_command_groups(groups: List(CommandGroup)) -> List(String) {
  list.flat_map(groups, fn(group) {
    let command_list =
      group.commands
      |> list.map(fn(cmd) { "    " <> cmd })
      |> string.join("\n")

    [
      "",
      "  " <> group.name <> ":",
      "    " <> group.description,
      command_list,
    ]
  })
}

/// Format workflows for display
fn format_workflows(workflows: List(Workflow)) -> List(String) {
  list.flat_map(workflows, fn(workflow) {
    let step_list =
      workflow.steps
      |> list.map(fn(step) { "    " <> step })
      |> string.join("\n")

    [
      "",
      "  " <> workflow.name <> ":",
      "    " <> workflow.description,
      step_list,
    ]
  })
}
