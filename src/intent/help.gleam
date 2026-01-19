/// Help text template system for Intent CLI
/// Provides consistent, structured help text for all 24 commands
///
/// Template structure:
/// 1. Short description (one line)
/// 2. Usage syntax (command + args + flags)
/// 3. Arguments (positional params)
/// 4. Options/flags (with types, defaults)
/// 5. Examples (realistic usage)
/// 6. Related commands (cross-references)
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// =============================================================================
// TYPES
// =============================================================================

/// Complete help information for a command
pub type CommandHelp {
  CommandHelp(
    name: String,
    short_desc: String,
    long_desc: String,
    usage: String,
    arguments: List(Argument),
    flags: List(Flag),
    examples: List(Example),
    related: List(String),
    category: CommandCategory,
  )
}

/// Positional argument definition
pub type Argument {
  Argument(name: String, description: String, required: Bool)
}

/// Flag/option definition
pub type Flag {
  Flag(
    name: String,
    short: Option(String),
    description: String,
    value_type: ValueType,
    default_value: Option(String),
    required: Bool,
  )
}

/// Type of flag value
pub type ValueType {
  StringType
  BoolType
  IntType
}

/// Usage example
pub type Example {
  Example(command: String, description: String)
}

/// Command category for grouping in help output
pub type CommandCategory {
  Core
  Analysis
  Interview
  Kirk
  Execution
}

// =============================================================================
// BUILDERS (Functional construction helpers)
// =============================================================================

/// Create a new command help with required fields
pub fn command(
  name name: String,
  short_desc short_desc: String,
  category category: CommandCategory,
) -> CommandHelp {
  CommandHelp(
    name: name,
    short_desc: short_desc,
    long_desc: "",
    usage: "intent " <> name,
    arguments: [],
    flags: [],
    examples: [],
    related: [],
    category: category,
  )
}

/// Add a longer description
pub fn with_long_desc(help: CommandHelp, desc: String) -> CommandHelp {
  CommandHelp(..help, long_desc: desc)
}

/// Set the usage string
pub fn with_usage(help: CommandHelp, usage: String) -> CommandHelp {
  CommandHelp(..help, usage: usage)
}

/// Add a required argument
pub fn with_arg(help: CommandHelp, name: String, desc: String) -> CommandHelp {
  let arg = Argument(name: name, description: desc, required: True)
  CommandHelp(..help, arguments: list.append(help.arguments, [arg]))
}

/// Add an optional argument
pub fn with_optional_arg(
  help: CommandHelp,
  name: String,
  desc: String,
) -> CommandHelp {
  let arg = Argument(name: name, description: desc, required: False)
  CommandHelp(..help, arguments: list.append(help.arguments, [arg]))
}

/// Add a string flag
pub fn with_string_flag(
  help: CommandHelp,
  name: String,
  desc: String,
  default: Option(String),
) -> CommandHelp {
  let flag =
    Flag(
      name: name,
      short: None,
      description: desc,
      value_type: StringType,
      default_value: default,
      required: False,
    )
  CommandHelp(..help, flags: list.append(help.flags, [flag]))
}

/// Add a required string flag
pub fn with_required_string_flag(
  help: CommandHelp,
  name: String,
  desc: String,
) -> CommandHelp {
  let flag =
    Flag(
      name: name,
      short: None,
      description: desc,
      value_type: StringType,
      default_value: None,
      required: True,
    )
  CommandHelp(..help, flags: list.append(help.flags, [flag]))
}

/// Add a boolean flag
pub fn with_bool_flag(
  help: CommandHelp,
  name: String,
  desc: String,
) -> CommandHelp {
  let flag =
    Flag(
      name: name,
      short: None,
      description: desc,
      value_type: BoolType,
      default_value: Some("false"),
      required: False,
    )
  CommandHelp(..help, flags: list.append(help.flags, [flag]))
}

/// Add an example
pub fn with_example(help: CommandHelp, cmd: String, desc: String) -> CommandHelp {
  let example = Example(command: cmd, description: desc)
  CommandHelp(..help, examples: list.append(help.examples, [example]))
}

/// Add related commands
pub fn with_related(help: CommandHelp, commands: List(String)) -> CommandHelp {
  CommandHelp(..help, related: commands)
}

// =============================================================================
// FORMATTING (Human-readable output)
// =============================================================================

/// Format command help as human-readable text
pub fn format_help(help: CommandHelp) -> String {
  let sections = [
    format_name_section(help),
    format_synopsis_section(help),
    format_description_section(help),
    format_arguments_section(help),
    format_options_section(help),
    format_examples_section(help),
    format_see_also_section(help),
  ]

  sections
  |> list.filter(fn(s) { !string.is_empty(s) })
  |> string.join("\n")
}

fn format_name_section(help: CommandHelp) -> String {
  "NAME\n    intent " <> help.name <> " - " <> help.short_desc <> "\n"
}

fn format_synopsis_section(help: CommandHelp) -> String {
  "SYNOPSIS\n    " <> help.usage <> "\n"
}

fn format_description_section(help: CommandHelp) -> String {
  case help.long_desc {
    "" -> ""
    desc -> "DESCRIPTION\n" <> wrap_text(desc, 4, 72) <> "\n"
  }
}

fn format_arguments_section(help: CommandHelp) -> String {
  case help.arguments {
    [] -> ""
    args -> {
      let formatted =
        args
        |> list.map(format_argument)
        |> string.join("\n")
      "ARGUMENTS\n" <> formatted <> "\n"
    }
  }
}

fn format_argument(arg: Argument) -> String {
  let required_marker = case arg.required {
    True -> ""
    False -> " (optional)"
  }
  "    <"
  <> arg.name
  <> ">"
  <> required_marker
  <> "\n        "
  <> arg.description
}

fn format_options_section(help: CommandHelp) -> String {
  case help.flags {
    [] -> ""
    flags -> {
      let formatted =
        flags
        |> list.map(format_flag)
        |> string.join("\n\n")
      "OPTIONS\n" <> formatted <> "\n"
    }
  }
}

fn format_flag(flag: Flag) -> String {
  let flag_syntax = case flag.value_type {
    BoolType -> "    --" <> flag.name
    StringType -> "    --" <> flag.name <> " <value>"
    IntType -> "    --" <> flag.name <> " <n>"
  }

  let markers = []
  let markers = case flag.required {
    True -> list.append(markers, ["required"])
    False -> markers
  }
  let markers = case flag.default_value {
    Some(d) -> list.append(markers, ["default: " <> d])
    None -> markers
  }

  let marker_str = case markers {
    [] -> ""
    m -> " (" <> string.join(m, ", ") <> ")"
  }

  flag_syntax <> marker_str <> "\n        " <> flag.description
}

fn format_examples_section(help: CommandHelp) -> String {
  case help.examples {
    [] -> ""
    examples -> {
      let formatted =
        examples
        |> list.map(format_example)
        |> string.join("\n\n")
      "EXAMPLES\n" <> formatted <> "\n"
    }
  }
}

fn format_example(example: Example) -> String {
  "    # " <> example.description <> "\n    " <> example.command
}

fn format_see_also_section(help: CommandHelp) -> String {
  case help.related {
    [] -> ""
    related -> "SEE ALSO\n    " <> string.join(related, ", ") <> "\n"
  }
}

/// Wrap text to a specified width with indentation
fn wrap_text(text: String, indent: Int, width: Int) -> String {
  let padding = string.repeat(" ", indent)
  let words = string.split(text, " ")
  let target_width = width - indent

  wrap_words(words, padding, target_width, padding, 0)
}

fn wrap_words(
  words: List(String),
  padding: String,
  width: Int,
  current_line: String,
  current_len: Int,
) -> String {
  case words {
    [] -> current_line
    [word, ..rest] -> {
      let word_len = string.length(word)
      case current_len + word_len + 1 > width {
        True -> {
          // Start new line
          current_line
          <> "\n"
          <> wrap_words(rest, padding, width, padding <> word, word_len)
        }
        False -> {
          // Add to current line
          let separator = case current_len {
            0 -> ""
            _ -> " "
          }
          wrap_words(
            rest,
            padding,
            width,
            current_line <> separator <> word,
            current_len + word_len + 1,
          )
        }
      }
    }
  }
}

// =============================================================================
// JSON OUTPUT (Machine-readable)
// =============================================================================

/// Convert command help to JSON
pub fn help_to_json(help: CommandHelp) -> json.Json {
  json.object([
    #("name", json.string(help.name)),
    #("short_desc", json.string(help.short_desc)),
    #("long_desc", json.string(help.long_desc)),
    #("usage", json.string(help.usage)),
    #("arguments", json.array(help.arguments, argument_to_json)),
    #("flags", json.array(help.flags, flag_to_json)),
    #("examples", json.array(help.examples, example_to_json)),
    #("related", json.array(help.related, json.string)),
    #("category", json.string(category_to_string(help.category))),
  ])
}

fn argument_to_json(arg: Argument) -> json.Json {
  json.object([
    #("name", json.string(arg.name)),
    #("description", json.string(arg.description)),
    #("required", json.bool(arg.required)),
  ])
}

fn flag_to_json(flag: Flag) -> json.Json {
  json.object([
    #("name", json.string(flag.name)),
    #("short", case flag.short {
      Some(s) -> json.string(s)
      None -> json.null()
    }),
    #("description", json.string(flag.description)),
    #("value_type", json.string(value_type_to_string(flag.value_type))),
    #("default", case flag.default_value {
      Some(d) -> json.string(d)
      None -> json.null()
    }),
    #("required", json.bool(flag.required)),
  ])
}

fn example_to_json(example: Example) -> json.Json {
  json.object([
    #("command", json.string(example.command)),
    #("description", json.string(example.description)),
  ])
}

fn value_type_to_string(vt: ValueType) -> String {
  case vt {
    StringType -> "string"
    BoolType -> "bool"
    IntType -> "int"
  }
}

pub fn category_to_string(cat: CommandCategory) -> String {
  case cat {
    Core -> "core"
    Analysis -> "analysis"
    Interview -> "interview"
    Kirk -> "kirk"
    Execution -> "execution"
  }
}

// =============================================================================
// COMMAND HELP DEFINITIONS
// =============================================================================

/// Get help for a specific command by name
pub fn get_command_help(name: String) -> Option(CommandHelp) {
  case name {
    "check" -> Some(check_help())
    "validate" -> Some(validate_help())
    "show" -> Some(show_help())
    "export" -> Some(export_help())
    "lint" -> Some(lint_help())
    "analyze" -> Some(analyze_help())
    "improve" -> Some(improve_help())
    "doctor" -> Some(doctor_help())
    "interview" -> Some(interview_help())
    "beads" -> Some(beads_help())
    "bead-status" -> Some(bead_status_help())
    "history" -> Some(history_help())
    "diff" -> Some(diff_help())
    "sessions" -> Some(sessions_help())
    "quality" -> Some(quality_help())
    "invert" -> Some(invert_help())
    "coverage" -> Some(coverage_help())
    "gaps" -> Some(gaps_help())
    "ears" -> Some(ears_help())
    "parse" -> Some(parse_help())
    "effects" -> Some(effects_help())
    "plan" -> Some(plan_help())
    "plan-approve" -> Some(plan_approve_help())
    "beads-regenerate" -> Some(beads_regenerate_help())
    _ -> None
  }
}

/// Get all command helps
pub fn all_commands() -> List(CommandHelp) {
  [
    check_help(),
    validate_help(),
    show_help(),
    export_help(),
    lint_help(),
    analyze_help(),
    improve_help(),
    doctor_help(),
    interview_help(),
    beads_help(),
    bead_status_help(),
    history_help(),
    diff_help(),
    sessions_help(),
    quality_help(),
    invert_help(),
    coverage_help(),
    gaps_help(),
    ears_help(),
    parse_help(),
    effects_help(),
    plan_help(),
    plan_approve_help(),
    beads_regenerate_help(),
  ]
}

/// Get commands by category
pub fn commands_by_category(cat: CommandCategory) -> List(CommandHelp) {
  all_commands()
  |> list.filter(fn(h) { h.category == cat })
}

// =============================================================================
// CORE COMMANDS
// =============================================================================

pub fn check_help() -> CommandHelp {
  command(
    name: "check",
    short_desc: "Run spec against a target URL and verify behaviors",
    category: Core,
  )
  |> with_long_desc(
    "Loads a CUE specification file and executes all behaviors against the specified target URL, verifying that the API conforms to the spec. Returns exit code 0 on success, 1 on test failures, 2 if behaviors are blocked, 3 for invalid spec, 4 for errors.",
  )
  |> with_usage("intent check <spec.cue> --target <url> [OPTIONS]")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_required_string_flag("target", "Target base URL to test against")
  |> with_bool_flag("json", "Output results as JSON")
  |> with_string_flag("feature", "Filter to a specific feature", None)
  |> with_string_flag("only", "Run only a specific behavior", None)
  |> with_bool_flag("verbose", "Verbose output with request/response details")
  |> with_bool_flag("quiet", "Quiet output (errors only)")
  |> with_bool_flag(
    "allow-localhost",
    "Allow localhost URLs for development (bypasses SSRF protection)",
  )
  |> with_example(
    "intent check api.cue --target https://api.example.com",
    "Run all behaviors in a spec",
  )
  |> with_example(
    "intent check api.cue --target https://api.example.com --feature auth",
    "Check only the auth feature",
  )
  |> with_example(
    "intent check api.cue --target https://api.example.com --json",
    "Get JSON output for CI pipelines",
  )
  |> with_example(
    "intent check api.cue --target http://localhost:8080 --allow-localhost",
    "Test against local development server",
  )
  |> with_related(["validate", "lint", "doctor"])
}

pub fn validate_help() -> CommandHelp {
  command(
    name: "validate",
    short_desc: "Validate a CUE spec file (syntax and structure)",
    category: Core,
  )
  |> with_long_desc(
    "Validates both CUE syntax and Intent spec structure. Checks that the file is valid CUE and conforms to the Intent specification schema. Does not execute any behaviors.",
  )
  |> with_usage("intent validate <spec.cue>")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_example("intent validate api.cue", "Validate a spec file")
  |> with_example("intent validate specs/*.cue", "Validate multiple specs")
  |> with_related(["check", "lint", "show"])
}

pub fn show_help() -> CommandHelp {
  command(
    name: "show",
    short_desc: "Pretty print a parsed spec",
    category: Core,
  )
  |> with_long_desc(
    "Loads and parses a CUE specification file, then displays it in a human-readable format showing features, behaviors, rules, and anti-patterns.",
  )
  |> with_usage("intent show <spec.cue> [--json]")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_bool_flag("json", "Output as JSON instead of formatted text")
  |> with_example("intent show api.cue", "Display spec in readable format")
  |> with_example("intent show api.cue --json", "Output spec as JSON")
  |> with_related(["export", "validate"])
}

pub fn export_help() -> CommandHelp {
  command(
    name: "export",
    short_desc: "Export spec to JSON format",
    category: Core,
  )
  |> with_long_desc(
    "Exports the CUE specification to JSON format. Useful for integration with other tools or for debugging spec structure.",
  )
  |> with_usage("intent export <spec.cue>")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_example(
    "intent export api.cue > api.json",
    "Export spec to JSON file",
  )
  |> with_example(
    "intent export api.cue | jq '.features'",
    "Pipe to jq for processing",
  )
  |> with_related(["show", "validate"])
}

// =============================================================================
// ANALYSIS COMMANDS
// =============================================================================

pub fn lint_help() -> CommandHelp {
  command(
    name: "lint",
    short_desc: "Check spec for anti-patterns and quality issues",
    category: Analysis,
  )
  |> with_long_desc(
    "Analyzes the specification for common anti-patterns and quality issues. Reports warnings for vague intents, missing error cases, overly broad rules, and other specification smells.",
  )
  |> with_usage("intent lint <spec.cue>")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_example("intent lint api.cue", "Check spec for issues")
  |> with_related(["analyze", "doctor", "improve"])
}

pub fn analyze_help() -> CommandHelp {
  command(
    name: "analyze",
    short_desc: "Analyze spec quality and provide improvement suggestions",
    category: Analysis,
  )
  |> with_long_desc(
    "Performs deep quality analysis of the specification, scoring it across multiple dimensions including completeness, testability, and clarity. Provides actionable suggestions for improvement.",
  )
  |> with_usage("intent analyze <spec.cue>")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_example("intent analyze api.cue", "Analyze spec quality")
  |> with_related(["lint", "improve", "doctor", "quality"])
}

pub fn improve_help() -> CommandHelp {
  command(
    name: "improve",
    short_desc: "Suggest improvements based on quality analysis and linting",
    category: Analysis,
  )
  |> with_long_desc(
    "Combines quality analysis and linting results to generate prioritized improvement suggestions. Shows what to fix first for maximum impact.",
  )
  |> with_usage("intent improve <spec.cue>")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_example("intent improve api.cue", "Get improvement suggestions")
  |> with_related(["lint", "analyze", "doctor"])
}

pub fn doctor_help() -> CommandHelp {
  command(
    name: "doctor",
    short_desc: "Analyze spec health and generate prioritized improvement report",
    category: Analysis,
  )
  |> with_long_desc(
    "Comprehensive health check that combines all analysis tools to produce a single, prioritized report. Shows overall health score and top issues to address.",
  )
  |> with_usage("intent doctor <spec.cue> [--json]")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_bool_flag("json", "Output as JSON for programmatic use")
  |> with_example("intent doctor api.cue", "Get health report")
  |> with_example("intent doctor api.cue --json", "Get JSON health report")
  |> with_related(["lint", "analyze", "improve", "quality"])
}

// =============================================================================
// INTERVIEW COMMANDS
// =============================================================================

pub fn interview_help() -> CommandHelp {
  command(
    name: "interview",
    short_desc: "Guided specification discovery through structured interview",
    category: Interview,
  )
  |> with_long_desc(
    "Interactive interview process that guides you through discovering and refining your API specification. Asks questions across 5 rounds covering core intent, scope, error cases, security, and operations. Supports both interactive mode and AI agent mode via CUE directives.",
  )
  |> with_usage("intent interview [OPTIONS]")
  |> with_string_flag(
    "profile",
    "System profile: api, cli, event, data, workflow, or ui",
    Some("api"),
  )
  |> with_string_flag("resume", "Resume existing interview session by ID", None)
  |> with_string_flag(
    "answers",
    "Path to CUE file with pre-filled answers for non-interactive mode",
    None,
  )
  |> with_bool_flag(
    "strict",
    "Strict mode: fail if answers file is missing required answers",
  )
  |> with_string_flag("export", "Export completed interview to spec file", None)
  |> with_bool_flag(
    "cue",
    "Output CUE directives for AI agents (non-interactive)",
  )
  |> with_string_flag("session", "Session ID for CUE mode", None)
  |> with_string_flag(
    "answer",
    "Submit answer to current question (use with --cue --session)",
    None,
  )
  |> with_bool_flag("dry-run", "Preview interview questions without saving")
  |> with_example(
    "intent interview --profile api",
    "Start new API interview interactively",
  )
  |> with_example(
    "intent interview --resume interview-abc123",
    "Resume existing session",
  )
  |> with_example(
    "intent interview --cue --profile api",
    "Start interview in AI agent mode",
  )
  |> with_example(
    "intent interview --cue --session abc123 --answer \"THE SYSTEM SHALL...\"",
    "Submit answer in AI mode",
  )
  |> with_related(["sessions", "beads", "history"])
}

pub fn beads_help() -> CommandHelp {
  command(
    name: "beads",
    short_desc: "Generate work items (beads) from an interview session",
    category: Interview,
  )
  |> with_long_desc(
    "Converts interview session answers into actionable work items (beads) that can be tracked and executed. Exports to .beads/issues.jsonl for integration with the bd issue tracker.",
  )
  |> with_usage("intent beads <session-id> [--json]")
  |> with_arg("session-id", "Interview session ID (e.g., interview-abc123)")
  |> with_bool_flag("json", "Output JSON for machine consumption")
  |> with_example(
    "intent beads interview-abc123",
    "Generate beads from session",
  )
  |> with_example(
    "intent beads interview-abc123 --json",
    "Get JSON output for automation",
  )
  |> with_related(["interview", "bead-status", "plan"])
}

pub fn bead_status_help() -> CommandHelp {
  command(
    name: "bead-status",
    short_desc: "Mark bead execution status (success/failed/blocked)",
    category: Interview,
  )
  |> with_long_desc(
    "Updates the execution status of a bead. Use to track progress through generated work items.",
  )
  |> with_usage("intent bead-status --bead-id <id> --status <status> [OPTIONS]")
  |> with_required_string_flag("bead-id", "Bead ID to update")
  |> with_required_string_flag("status", "Status: success, failed, or blocked")
  |> with_string_flag(
    "reason",
    "Reason for status (required for blocked)",
    None,
  )
  |> with_string_flag("session", "Session ID", None)
  |> with_example(
    "intent bead-status --bead-id bead-001 --status success",
    "Mark bead as completed",
  )
  |> with_example(
    "intent bead-status --bead-id bead-002 --status blocked --reason \"Waiting for API access\"",
    "Mark bead as blocked with reason",
  )
  |> with_related(["beads", "beads-regenerate", "plan"])
}

pub fn history_help() -> CommandHelp {
  command(
    name: "history",
    short_desc: "View snapshot history for an interview session",
    category: Interview,
  )
  |> with_long_desc(
    "Shows the history of snapshots taken during an interview session, including timestamps, stages, and answer counts.",
  )
  |> with_usage("intent history <session-id>")
  |> with_arg("session-id", "Interview session ID")
  |> with_example("intent history interview-abc123", "View session history")
  |> with_related(["sessions", "diff", "interview"])
}

pub fn diff_help() -> CommandHelp {
  command(
    name: "diff",
    short_desc: "Compare two interview sessions and show differences",
    category: Interview,
  )
  |> with_long_desc(
    "Compares two interview sessions and displays the differences in answers, gaps, conflicts, and stage progression.",
  )
  |> with_usage("intent diff <from-session> <to-session>")
  |> with_arg("from-session", "First session ID to compare")
  |> with_arg("to-session", "Second session ID to compare")
  |> with_example(
    "intent diff interview-abc123 interview-def456",
    "Compare two sessions",
  )
  |> with_related(["sessions", "history"])
}

pub fn sessions_help() -> CommandHelp {
  command(
    name: "sessions",
    short_desc: "List all interview sessions",
    category: Interview,
  )
  |> with_long_desc(
    "Lists all interview sessions stored in .interview/sessions.jsonl with their status, profile, and progress.",
  )
  |> with_usage("intent sessions [OPTIONS]")
  |> with_bool_flag("json", "Output as JSON")
  |> with_string_flag(
    "profile",
    "Filter by profile (api, cli, event, etc.)",
    None,
  )
  |> with_bool_flag("incomplete", "Show only incomplete sessions")
  |> with_example("intent sessions", "List all sessions")
  |> with_example("intent sessions --incomplete", "List incomplete sessions")
  |> with_example(
    "intent sessions --profile api --json",
    "List API sessions as JSON",
  )
  |> with_related(["interview", "history", "diff"])
}

// =============================================================================
// KIRK COMMANDS (Knowledge-Informed Requirement Kritiquer)
// =============================================================================

pub fn quality_help() -> CommandHelp {
  command(
    name: "quality",
    short_desc: "KIRK: Analyze spec quality across multiple dimensions",
    category: Kirk,
  )
  |> with_long_desc(
    "KIRK quality analysis scores your specification across completeness, consistency, testability, clarity, and security dimensions. Identifies specific issues with severity levels.",
  )
  |> with_usage("intent quality <spec.cue> [--json]")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_bool_flag("json", "Output as JSON")
  |> with_example("intent quality api.cue", "Analyze spec quality")
  |> with_example("intent quality api.cue --json", "Get JSON quality report")
  |> with_related(["analyze", "doctor", "gaps", "invert"])
}

pub fn invert_help() -> CommandHelp {
  command(
    name: "invert",
    short_desc: "KIRK: Inversion analysis - what failure cases are missing?",
    category: Kirk,
  )
  |> with_long_desc(
    "Applies inversion thinking to identify missing failure cases. Finds security gaps, usability issues, and integration problems by asking 'what could go wrong?' for each behavior.",
  )
  |> with_usage("intent invert <spec.cue> [--json]")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_bool_flag("json", "Output as JSON")
  |> with_example("intent invert api.cue", "Find missing failure cases")
  |> with_example("intent invert api.cue --json", "Get JSON inversion report")
  |> with_related(["gaps", "effects", "quality"])
}

pub fn coverage_help() -> CommandHelp {
  command(
    name: "coverage",
    short_desc: "KIRK: Coverage analysis including OWASP Top 10",
    category: Kirk,
  )
  |> with_long_desc(
    "Analyzes spec coverage across HTTP methods, status codes, and security concerns. Includes OWASP Top 10 coverage assessment.",
  )
  |> with_usage("intent coverage <spec.cue> [--json]")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_bool_flag("json", "Output as JSON")
  |> with_example("intent coverage api.cue", "Analyze coverage")
  |> with_example("intent coverage api.cue --json", "Get JSON coverage report")
  |> with_related(["gaps", "quality", "invert"])
}

pub fn gaps_help() -> CommandHelp {
  command(
    name: "gaps",
    short_desc: "KIRK: Detect gaps using mental models",
    category: Kirk,
  )
  |> with_long_desc(
    "Comprehensive gap detection using multiple mental models including inversion, second-order effects, checklists, and security analysis. Prioritizes gaps by severity.",
  )
  |> with_usage("intent gaps <spec.cue> [--json]")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_bool_flag("json", "Output as JSON")
  |> with_example("intent gaps api.cue", "Detect specification gaps")
  |> with_example("intent gaps api.cue --json", "Get JSON gap report")
  |> with_related(["invert", "effects", "coverage", "quality"])
}

pub fn ears_help() -> CommandHelp {
  command(
    name: "ears",
    short_desc: "KIRK: Parse EARS requirements to Intent behaviors",
    category: Kirk,
  )
  |> with_long_desc(
    "Parses requirements written in EARS (Easy Approach to Requirements Syntax) format and converts them to Intent behaviors. Supports all EARS patterns: Ubiquitous, Event-Driven, State-Driven, Optional, Unwanted, and Complex.",
  )
  |> with_usage("intent ears <requirements.md> [OPTIONS]")
  |> with_arg("requirements.md", "Path to requirements file in EARS format")
  |> with_string_flag("output", "Output format: text, cue, json", Some("text"))
  |> with_string_flag("out", "Output file path", None)
  |> with_string_flag("name", "Spec name for CUE output", Some("GeneratedSpec"))
  |> with_example("intent ears requirements.md", "Parse EARS requirements")
  |> with_example(
    "intent ears requirements.md --output cue --out spec.cue",
    "Convert to CUE spec",
  )
  |> with_example(
    "intent ears requirements.md --output json",
    "Get JSON output",
  )
  |> with_related(["parse", "validate"])
}

pub fn parse_help() -> CommandHelp {
  command(
    name: "parse",
    short_desc: "Parse EARS requirements to spec",
    category: Kirk,
  )
  |> with_long_desc(
    "Parses EARS requirements and optionally outputs a CUE spec. Shows parsing progress and any errors encountered.",
  )
  |> with_usage("intent parse <requirements.ears.md> [OPTIONS]")
  |> with_arg(
    "requirements.ears.md",
    "Path to requirements file in EARS format",
  )
  |> with_string_flag("o", "Output CUE spec file path", None)
  |> with_bool_flag("json", "Output as JSON")
  |> with_example("intent parse requirements.ears.md", "Parse and show results")
  |> with_example(
    "intent parse requirements.ears.md -o spec.cue",
    "Parse and export to CUE",
  )
  |> with_related(["ears", "validate"])
}

pub fn effects_help() -> CommandHelp {
  command(
    name: "effects",
    short_desc: "KIRK: Analyze second-order effects (consequence tracing)",
    category: Kirk,
  )
  |> with_long_desc(
    "Traces the consequences of each behavior to identify second-order effects. Helps find unintended consequences and cascading impacts.",
  )
  |> with_usage("intent effects <spec.cue> [--json]")
  |> with_arg("spec.cue", "Path to the CUE specification file")
  |> with_bool_flag("json", "Output JSON for machine consumption")
  |> with_example("intent effects api.cue", "Analyze second-order effects")
  |> with_example("intent effects api.cue --json", "Get JSON effects report")
  |> with_related(["invert", "gaps", "quality"])
}

// =============================================================================
// EXECUTION COMMANDS
// =============================================================================

pub fn plan_help() -> CommandHelp {
  command(
    name: "plan",
    short_desc: "Display execution plan from session beads",
    category: Execution,
  )
  |> with_long_desc(
    "Generates and displays an execution plan from session beads, organized into phases with effort estimates and risk assessment.",
  )
  |> with_usage("intent plan <session-id> [--format human|json]")
  |> with_arg("session-id", "Session ID to generate plan from")
  |> with_string_flag("format", "Output format: human or json", Some("human"))
  |> with_example("intent plan abc123", "Display human-readable execution plan")
  |> with_example(
    "intent plan abc123 --format json",
    "Get JSON plan for automation",
  )
  |> with_related(["plan-approve", "beads", "beads-regenerate"])
}

pub fn plan_approve_help() -> CommandHelp {
  command(
    name: "plan-approve",
    short_desc: "Approve execution plan for session",
    category: Execution,
  )
  |> with_long_desc(
    "Approves an execution plan for a session, optionally with notes. Supports interactive approval or auto-approval for CI pipelines.",
  )
  |> with_usage("intent plan-approve <session-id> [OPTIONS]")
  |> with_arg("session-id", "Session ID to approve plan for")
  |> with_bool_flag("yes", "Auto-approve for CI (non-interactive)")
  |> with_string_flag("notes", "Approval notes", None)
  |> with_example("intent plan-approve abc123", "Interactive approval")
  |> with_example(
    "intent plan-approve abc123 --yes",
    "Auto-approve for CI pipeline",
  )
  |> with_example(
    "intent plan-approve abc123 --yes --notes \"Approved by release team\"",
    "Auto-approve with notes",
  )
  |> with_related(["plan", "beads"])
}

pub fn beads_regenerate_help() -> CommandHelp {
  command(
    name: "beads-regenerate",
    short_desc: "Regenerate failed/blocked beads with adjusted approach",
    category: Execution,
  )
  |> with_long_desc(
    "Analyzes failed or blocked beads and generates regeneration entries with adjusted approaches. Supports different regeneration strategies.",
  )
  |> with_usage("intent beads-regenerate <session-id> [OPTIONS]")
  |> with_arg("session-id", "Session ID with failed/blocked beads")
  |> with_string_flag(
    "strategy",
    "Regeneration strategy: hybrid, inversion, or premortem",
    Some("hybrid"),
  )
  |> with_example(
    "intent beads-regenerate abc123",
    "Regenerate with hybrid strategy",
  )
  |> with_example(
    "intent beads-regenerate abc123 --strategy inversion",
    "Use inversion strategy",
  )
  |> with_related(["bead-status", "plan", "beads"])
}

// =============================================================================
// FULL HELP OUTPUT
// =============================================================================

/// Format complete CLI help with all commands grouped by category
pub fn format_full_help() -> String {
  let header =
    "INTENT - Contract-driven API testing framework
Human-writes, AI-verifies, AI-implements

USAGE
    intent <command> [OPTIONS]

"

  let categories = [
    #(Core, "CORE COMMANDS"),
    #(Analysis, "ANALYSIS COMMANDS"),
    #(Interview, "INTERVIEW COMMANDS"),
    #(Kirk, "KIRK COMMANDS (Knowledge-Informed Requirement Kritiquer)"),
    #(Execution, "EXECUTION COMMANDS"),
  ]

  let sections =
    categories
    |> list.map(fn(pair) {
      let #(cat, title) = pair
      let commands = commands_by_category(cat)
      format_category_section(title, commands)
    })
    |> string.join("\n")

  let footer =
    "
Run 'intent <command> --help' for detailed information on a specific command.

For more information: https://github.com/your-org/intent-cli
"

  header <> sections <> footer
}

fn format_category_section(title: String, commands: List(CommandHelp)) -> String {
  let header = title <> "\n"
  let items =
    commands
    |> list.map(fn(h) { "    " <> pad_right(h.name, 20) <> h.short_desc })
    |> string.join("\n")
  header <> items <> "\n"
}

fn pad_right(s: String, width: Int) -> String {
  let len = string.length(s)
  case len < width {
    True -> s <> string.repeat(" ", width - len)
    False -> s
  }
}
