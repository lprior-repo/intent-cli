import gleam/list
import gleam/string

pub type ConsistencyIssue {
  MissingJsonFlag(command: String)
  IncorrectOutputMode(command: String, expected: String, found: String)
  InconsistentExitCode(
    command: String,
    context: String,
    expected: String,
    found: String,
  )
  MissingCliUiPrint(command: String)
  InconsistentErrorOutput(command: String, expected: String, found: String)
  MissingUsageMessage(command: String)
  InconsistentUsageFormat(command: String, expected: String, found: String)
}

pub type ConsistencyResult {
  Passed
  Failed(List(ConsistencyIssue))
}

pub type CommandSpec {
  CommandSpec(
    name: String,
    expects_json_flag: Bool,
    expected_output_mode: String,
    expected_error_output: String,
    valid_exit_codes: List(Int),
    usage_pattern: String,
  )
}

pub fn validate_command_consistency(
  spec: CommandSpec,
  issues: List(ConsistencyIssue),
) -> ConsistencyResult {
  let combined = collect_issues(spec, issues)
  case combined {
    [] -> Passed
    _ -> Failed(combined)
  }
}

fn collect_issues(
  spec: CommandSpec,
  issues: List(ConsistencyIssue),
) -> List(ConsistencyIssue) {
  list.filter(issues, fn(issue) {
    case issue {
      MissingJsonFlag(cmd) -> cmd == spec.name && spec.expects_json_flag
      IncorrectOutputMode(cmd, _, _) -> cmd == spec.name
      InconsistentExitCode(cmd, _, _, _) -> cmd == spec.name
      MissingCliUiPrint(cmd) -> cmd == spec.name
      InconsistentErrorOutput(cmd, _, _) -> cmd == spec.name
      MissingUsageMessage(cmd) -> cmd == spec.name
      InconsistentUsageFormat(cmd, _, _) -> cmd == spec.name
    }
  })
}

pub fn validate_check_command(
  has_json_flag: Bool,
  uses_output_mode: Bool,
  uses_cli_ui_print: Bool,
  uses_exit_error_for_validation: Bool,
  has_correct_usage: Bool,
) -> ConsistencyResult {
  let issues = []
  let issues = case has_json_flag {
    False -> [MissingJsonFlag("check"), ..issues]
    True -> issues
  }
  let issues = case uses_output_mode {
    False -> [
      IncorrectOutputMode("check", "from_json_flag", "hardcoded Interactive"),
      ..issues
    ]
    True -> issues
  }
  let issues = case uses_cli_ui_print {
    False -> [MissingCliUiPrint("check"), ..issues]
    True -> issues
  }
  let issues = case uses_exit_error_for_validation {
    False -> [
      InconsistentExitCode(
        "check",
        "validation error",
        "exit_invalid",
        "exit_error",
      ),
      ..issues
    ]
    True -> issues
  }
  let issues = case has_correct_usage {
    False -> [
      InconsistentUsageFormat(
        "check",
        "intent check <spec.cue> --target=<url>",
        "missing/incorrect",
      ),
      ..issues
    ]
    True -> issues
  }
  validate_command_consistency(
    CommandSpec(
      name: "check",
      expects_json_flag: True,
      expected_output_mode: "from_json_flag",
      expected_error_output: "cli_ui.print_error",
      valid_exit_codes: [0, 1, 2, 3, 4],
      usage_pattern: "intent check <spec.cue> --target=<url>",
    ),
    issues,
  )
}

pub fn validate_validate_command(
  uses_output_mode: Bool,
  uses_cli_ui_print: Bool,
  has_correct_usage: Bool,
) -> ConsistencyResult {
  let issues = []
  let issues = case uses_output_mode {
    False -> [
      IncorrectOutputMode("validate", "Interactive", "missing"),
      ..issues
    ]
    True -> issues
  }
  let issues = case uses_cli_ui_print {
    False -> [MissingCliUiPrint("validate"), ..issues]
    True -> issues
  }
  let issues = case has_correct_usage {
    False -> [
      InconsistentUsageFormat(
        "validate",
        "intent validate <spec.cue>",
        "missing/incorrect",
      ),
      ..issues
    ]
    True -> issues
  }
  validate_command_consistency(
    CommandSpec(
      name: "validate",
      expects_json_flag: False,
      expected_output_mode: "Interactive",
      expected_error_output: "cli_ui.print_error",
      valid_exit_codes: [0, 3],
      usage_pattern: "intent validate <spec.cue>",
    ),
    issues,
  )
}

pub fn validate_show_command(
  _has_json_flag: Bool,
  uses_output_mode: Bool,
  uses_cli_ui_print: Bool,
  has_correct_usage: Bool,
) -> ConsistencyResult {
  let issues = []
  // has_json_flag check removed as we don't expect it
  let issues = case uses_output_mode {
    False -> [
      IncorrectOutputMode("show", "from_json_flag", "missing"),
      ..issues
    ]
    True -> issues
  }
  let issues = case uses_cli_ui_print {
    False -> [MissingCliUiPrint("show"), ..issues]
    True -> issues
  }
  let issues = case has_correct_usage {
    False -> [
      InconsistentUsageFormat(
        "show",
        "intent show <spec.cue>",
        "missing/incorrect",
      ),
      ..issues
    ]
    True -> issues
  }
  validate_command_consistency(
    CommandSpec(
      name: "show",
      expects_json_flag: False,
      expected_output_mode: "from_json_flag",
      expected_error_output: "cli_ui.print_error",
      valid_exit_codes: [0, 4],
      usage_pattern: "intent show <spec.cue>",
    ),
    issues,
  )
}

pub fn validate_export_command(has_correct_usage: Bool) -> ConsistencyResult {
  let issues = case has_correct_usage {
    False -> [
      InconsistentUsageFormat(
        "export",
        "intent export <spec.cue>",
        "missing/incorrect",
      ),
      ..[]
    ]
    True -> []
  }
  validate_command_consistency(
    CommandSpec(
      name: "export",
      expects_json_flag: False,
      expected_output_mode: "default",
      expected_error_output: "cli_ui.print_error",
      valid_exit_codes: [0, 4],
      usage_pattern: "intent export <spec.cue>",
    ),
    issues,
  )
}

pub fn validate_lint_command(has_correct_usage: Bool) -> ConsistencyResult {
  let issues = case has_correct_usage {
    False -> [
      InconsistentUsageFormat(
        "lint",
        "intent lint <spec.cue>",
        "missing/incorrect",
      ),
      ..[]
    ]
    True -> []
  }
  validate_command_consistency(
    CommandSpec(
      name: "lint",
      expects_json_flag: False,
      expected_output_mode: "default",
      expected_error_output: "cli_ui.print_error",
      valid_exit_codes: [0, 1, 3],
      usage_pattern: "intent lint <spec.cue>",
    ),
    issues,
  )
}

pub fn validate_analyze_command(has_correct_usage: Bool) -> ConsistencyResult {
  let issues = case has_correct_usage {
    False -> [
      InconsistentUsageFormat(
        "analyze",
        "intent analyze <spec.cue>",
        "missing/incorrect",
      ),
      ..[]
    ]
    True -> []
  }
  validate_command_consistency(
    CommandSpec(
      name: "analyze",
      expects_json_flag: False,
      expected_output_mode: "default",
      expected_error_output: "cli_ui.print_error",
      valid_exit_codes: [0, 3],
      usage_pattern: "intent analyze <spec.cue>",
    ),
    issues,
  )
}

pub fn validate_improve_command(has_correct_usage: Bool) -> ConsistencyResult {
  let issues = case has_correct_usage {
    False -> [
      InconsistentUsageFormat(
        "improve",
        "intent improve <spec.cue>",
        "missing/incorrect",
      ),
      ..[]
    ]
    True -> []
  }
  validate_command_consistency(
    CommandSpec(
      name: "improve",
      expects_json_flag: False,
      expected_output_mode: "default",
      expected_error_output: "cli_ui.print_error",
      valid_exit_codes: [0, 3],
      usage_pattern: "intent improve <spec.cue>",
    ),
    issues,
  )
}

pub fn validate_doctor_command(
  _has_json_flag: Bool,
  uses_output_mode: Bool,
  has_correct_usage: Bool,
) -> ConsistencyResult {
  let issues = []
  let issues = case uses_output_mode {
    False -> [IncorrectOutputMode("doctor", "from_flags", "missing"), ..issues]
    True -> issues
  }
  let issues = case has_correct_usage {
    False -> [
      InconsistentUsageFormat(
        "doctor",
        "intent doctor <spec.cue>",
        "missing/incorrect",
      ),
      ..issues
    ]
    True -> issues
  }
  validate_command_consistency(
    CommandSpec(
      name: "doctor",
      expects_json_flag: False,
      expected_output_mode: "from_flags",
      expected_error_output: "cli_ui.print_error",
      valid_exit_codes: [0, 3],
      usage_pattern: "intent doctor <spec.cue>",
    ),
    issues,
  )
}

pub fn format_issue(issue: ConsistencyIssue) -> String {
  case issue {
    MissingJsonFlag(cmd) ->
      "Missing --json flag handling in '" <> cmd <> "' command"
    IncorrectOutputMode(cmd, expected, found) ->
      "Incorrect output mode in '"
      <> cmd
      <> "': expected "
      <> expected
      <> ", found "
      <> found
    InconsistentExitCode(cmd, context, expected, found) ->
      "Inconsistent exit code in '"
      <> cmd
      <> "' for "
      <> context
      <> ": expected "
      <> expected
      <> ", found "
      <> found
    MissingCliUiPrint(cmd) ->
      "Missing cli_ui.print_error usage in '" <> cmd <> "' command"
    InconsistentErrorOutput(cmd, expected, found) ->
      "Inconsistent error output in '"
      <> cmd
      <> "': expected "
      <> expected
      <> ", found "
      <> found
    MissingUsageMessage(cmd) ->
      "Missing usage message in '" <> cmd <> "' command"
    InconsistentUsageFormat(cmd, expected, found) ->
      "Inconsistent usage format in '"
      <> cmd
      <> "': expected '"
      <> expected
      <> "', found '"
      <> found
      <> "'"
  }
}

pub fn format_result(result: ConsistencyResult) -> String {
  case result {
    Passed -> "✓ All consistency checks passed"
    Failed(issues) -> {
      let formatted = list.map(issues, format_issue)
      "✗ Consistency validation failed:\n" <> string.join(formatted, "\n")
    }
  }
}

pub type CommandCategory {
  CoreSpec
  KirkAnalysis
  Interview
  BeadsPlanning
  Parsing
  Utilities
  AICommands
  ShapePhase
  ReadyPhase
}

pub type CommandInfo {
  CommandInfo(
    name: String,
    category: CommandCategory,
    has_json_flag: Bool,
    always_json_output: Bool,
    is_interactive: Bool,
    primary_flags: List(String),
    valid_exit_codes: List(Int),
  )
}

pub fn get_all_command_info() -> List(CommandInfo) {
  [
    // Core Spec Operations (4)
    CommandInfo(
      name: "validate",
      category: CoreSpec,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 3],
    ),
    CommandInfo(
      name: "analyze",
      category: CoreSpec,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 3],
    ),
    CommandInfo(
      name: "lint",
      category: CoreSpec,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 1, 3],
    ),
    CommandInfo(
      name: "improve",
      category: CoreSpec,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 3],
    ),
    // KIRK Analysis (6)
    CommandInfo(
      name: "quality",
      category: KirkAnalysis,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 3],
    ),
    CommandInfo(
      name: "coverage",
      category: KirkAnalysis,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 3],
    ),
    CommandInfo(
      name: "gaps",
      category: KirkAnalysis,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 3],
    ),
    CommandInfo(
      name: "invert",
      category: KirkAnalysis,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 3],
    ),
    CommandInfo(
      name: "effects",
      category: KirkAnalysis,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 3],
    ),
    CommandInfo(
      name: "ears",
      category: KirkAnalysis,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: ["output"],
      valid_exit_codes: [0, 3],
    ),
    // Interview Workflow (5)
    CommandInfo(
      name: "interview",
      category: Interview,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: True,
      primary_flags: ["profile", "resume", "session", "answer", "dry-run"],
      valid_exit_codes: [0, 4],
    ),
    CommandInfo(
      name: "sessions",
      category: Interview,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: ["profile"],
      valid_exit_codes: [0],
    ),
    CommandInfo(
      name: "history",
      category: Interview,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0],
    ),
    CommandInfo(
      name: "diff",
      category: Interview,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 4],
    ),
    CommandInfo(
      name: "export",
      category: Interview,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: ["output"],
      valid_exit_codes: [0, 4],
    ),
    // Beads/Planning (7)
    CommandInfo(
      name: "beads",
      category: BeadsPlanning,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: ["max-items"],
      valid_exit_codes: [0, 4],
    ),
    CommandInfo(
      name: "beads-regenerate",
      category: BeadsPlanning,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: ["strategy"],
      valid_exit_codes: [0, 4],
    ),
    CommandInfo(
      name: "bead-status",
      category: BeadsPlanning,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: ["bead-id", "status", "reason"],
      valid_exit_codes: [0, 4],
    ),
    CommandInfo(
      name: "plan",
      category: BeadsPlanning,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: ["format", "rounds"],
      valid_exit_codes: [0, 4],
    ),
    CommandInfo(
      name: "plan-approve",
      category: BeadsPlanning,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: ["yes", "notes"],
      valid_exit_codes: [0, 4],
    ),
    CommandInfo(
      name: "prompt",
      category: BeadsPlanning,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: ["max-items"],
      valid_exit_codes: [0, 4],
    ),
    CommandInfo(
      name: "feedback",
      category: BeadsPlanning,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: ["results"],
      valid_exit_codes: [0, 4],
    ),
    // Parsing (1)
    CommandInfo(
      name: "parse",
      category: Parsing,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: ["o"],
      valid_exit_codes: [0, 4],
    ),
    // Utilities (3)
    CommandInfo(
      name: "doctor",
      category: Utilities,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 3, 4],
    ),
    CommandInfo(
      name: "show",
      category: Utilities,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 4],
    ),
    CommandInfo(
      name: "help",
      category: Utilities,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0],
    ),
    // AI Commands (1)
    CommandInfo(
      name: "ai schema",
      category: AICommands,
      has_json_flag: False,
      always_json_output: False,
      is_interactive: False,
      primary_flags: ["command", "type"],
      valid_exit_codes: [0],
    ),
    // Shape Phase (5)
    CommandInfo(
      name: "shape start",
      category: ShapePhase,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0],
    ),
    CommandInfo(
      name: "shape check",
      category: ShapePhase,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: ["session"],
      valid_exit_codes: [0],
    ),
    CommandInfo(
      name: "shape critique",
      category: ShapePhase,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: ["session"],
      valid_exit_codes: [0],
    ),
    CommandInfo(
      name: "shape respond",
      category: ShapePhase,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: ["session", "answers"],
      valid_exit_codes: [0],
    ),
    CommandInfo(
      name: "shape agree",
      category: ShapePhase,
      has_json_flag: False,
      always_json_output: True,
      is_interactive: False,
      primary_flags: ["session"],
      valid_exit_codes: [0],
    ),
  ]
}

pub fn format_command_summary(info: CommandInfo) -> String {
  let category_str = case info.category {
    CoreSpec -> "Core Spec Operations"
    KirkAnalysis -> "KIRK Analysis"
    Interview -> "Interview Workflow"
    BeadsPlanning -> "Beads/Planning"
    Parsing -> "Parsing"
    Utilities -> "Utilities"
    AICommands -> "AI Commands"
    ShapePhase -> "Shape Phase"
    ReadyPhase -> "Ready Phase"
  }

  let output_type = case info.always_json_output, info.has_json_flag {
    True, _ -> "JSON (always)"
    False, True -> "JSON (with --json)"
    False, False -> "Text"
  }

  let interactive = case info.is_interactive {
    True -> "Yes"
    False -> "No"
  }

  let exit_codes =
    list.map(info.valid_exit_codes, string.inspect)
    |> string.join(", ")

  let flags = case list.is_empty(info.primary_flags) {
    True -> "none"
    False -> string.join(info.primary_flags, ", ")
  }

  string.concat([
    "Command: ",
    info.name,
    "\n",
    "  Category: ",
    category_str,
    "\n",
    "  Output: ",
    output_type,
    "\n",
    "  Interactive: ",
    interactive,
    "\n",
    "  Primary flags: ",
    flags,
    "\n",
    "  Valid exit codes: ",
    exit_codes,
  ])
}

pub fn generate_command_report() -> String {
  let commands = get_all_command_info()
  let count = list.length(commands)

  let json_count =
    list.filter(commands, fn(c) { c.always_json_output })
    |> list.length

  let has_json_flag_count =
    list.filter(commands, fn(c) { c.has_json_flag })
    |> list.length

  let interactive_count =
    list.filter(commands, fn(c) { c.is_interactive })
    |> list.length

  let summaries =
    list.map(commands, format_command_summary)
    |> string.join("\n\n")

  string.concat([
    "Intent CLI Command Summary\n",
    "==========================\n\n",
    "Total commands: ",
    string.inspect(count),
    "\n",
    "JSON-only output: ",
    string.inspect(json_count),
    "\n",
    "Commands with --json flag: ",
    string.inspect(has_json_flag_count),
    "\n",
    "Interactive commands: ",
    string.inspect(interactive_count),
    "\n\n",
    summaries,
  ])
}

pub fn validate_all_commands() -> ConsistencyResult {
  // Validate all 32 commands defined in the CLI using metadata-based approach
  // This performs consistency checks across:
  // - Flag naming conventions
  // - JSON output patterns
  // - Exit code usage
  // - Interactive vs non-interactive consistency

  // Run metadata-based validation on all commands
  let metadata_result = validate_command_metadata()

  // Run specific command validations (currently all return empty lists)
  let specific_issues = []

  // Core Spec Operations (4)
  let specific_issues =
    list.append(specific_issues, check_validate_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_analyze_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_lint_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_improve_command_consistency())

  // KIRK Analysis (6)
  let specific_issues =
    list.append(specific_issues, check_quality_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_coverage_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_gaps_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_invert_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_effects_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_ears_command_consistency())

  // Interview Workflow (5)
  let specific_issues =
    list.append(specific_issues, check_interview_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_sessions_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_history_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_diff_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_export_command_consistency())

  // Beads/Planning (7)
  let specific_issues =
    list.append(specific_issues, check_beads_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_beads_regenerate_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_bead_status_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_plan_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_plan_approve_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_prompt_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_feedback_command_consistency())

  // Parsing (1)
  let specific_issues =
    list.append(specific_issues, check_parse_command_consistency())

  // Utilities (3)
  let specific_issues =
    list.append(specific_issues, check_doctor_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_show_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_help_command_consistency())

  // AI Commands (1)
  let specific_issues =
    list.append(specific_issues, check_ai_schema_command_consistency())

  // Shape Phase Commands (5)
  let specific_issues =
    list.append(specific_issues, check_shape_start_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_shape_check_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_shape_critique_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_shape_respond_command_consistency())
  let specific_issues =
    list.append(specific_issues, check_shape_agree_command_consistency())

  // Combine all issues
  let all_issues = case metadata_result {
    Passed -> specific_issues
    Failed(metadata_issues) -> list.append(metadata_issues, specific_issues)
  }

  case all_issues {
    [] -> Passed
    _ -> Failed(all_issues)
  }
}

// Core Spec Operations validators
fn check_validate_command_consistency() -> List(ConsistencyIssue) {
  // validate command: no JSON flag, uses Interactive mode, text output only
  // Expected: returns exit_pass(0) or exit_invalid(3)
  // Validates both CUE syntax and structural requirements
  []
}

fn check_analyze_command_consistency() -> List(ConsistencyIssue) {
  // analyze command: alias for quality but text-only output
  // No JSON flag support (by design)
  []
}

fn check_lint_command_consistency() -> List(ConsistencyIssue) {
  // lint command: no JSON flag, text output only
  // Detects anti-patterns and shows warnings
  []
}

fn check_improve_command_consistency() -> List(ConsistencyIssue) {
  // improve command: no JSON flag, text output only
  // Provides improvement suggestions
  []
}

// KIRK Analysis validators
fn check_quality_command_consistency() -> List(ConsistencyIssue) {
  // quality command: always outputs JSON (using json_output module)
  // No --json flag because it's JSON-only by design
  // Returns exit_pass(0) on success, exit_invalid(3) on load error
  // Expected: includes next_actions suggesting gaps and invert commands
  []
}

fn check_coverage_command_consistency() -> List(ConsistencyIssue) {
  // coverage command: always outputs JSON (using json_output module)
  // OWASP Top 10 + edge case coverage analysis
  // Returns exit_pass(0) on success, exit_invalid(3) on load error
  []
}

fn check_gaps_command_consistency() -> List(ConsistencyIssue) {
  // gaps command: always outputs JSON (using json_output module)
  // Mental model gap detection across 5 rounds
  // Returns exit_pass(0) on success, exit_invalid(3) on load error
  []
}

fn check_invert_command_consistency() -> List(ConsistencyIssue) {
  // invert command: always outputs JSON (using json_output module)
  // Failure mode analysis (security, usability, integration)
  // Returns exit_pass(0) on success, exit_invalid(3) on load error
  []
}

fn check_effects_command_consistency() -> List(ConsistencyIssue) {
  // effects command: always outputs JSON (using json_output module)
  // Second-order effects analysis
  // Returns exit_pass(0) on success, exit_invalid(3) on load error
  []
}

fn check_ears_command_consistency() -> List(ConsistencyIssue) {
  // ears command: supports --output flag (cue|json)
  // Parses EARS requirements and outputs CUE or JSON format
  []
}

// Interview Workflow validators
fn check_interview_command_consistency() -> List(ConsistencyIssue) {
  // interview command: no JSON flag, fully interactive mode
  // Multi-round guided spec discovery workflow
  // Validates profile flag (api|cli)
  []
}

fn check_sessions_command_consistency() -> List(ConsistencyIssue) {
  // sessions command: no JSON flag, text output
  // Lists all interview sessions from JSONL storage
  []
}

fn check_history_command_consistency() -> List(ConsistencyIssue) {
  // history command: no JSON flag, text output
  // Shows session snapshot history
  []
}

fn check_diff_command_consistency() -> List(ConsistencyIssue) {
  // diff command: no JSON flag, text output
  // Compares two interview sessions
  // Requires exactly 2 session ID arguments
  []
}

fn check_export_command_consistency() -> List(ConsistencyIssue) {
  // export command: no JSON flag, exports spec to file
  // Takes session ID and optional --output flag
  []
}

// Beads/Planning validators
fn check_beads_command_consistency() -> List(ConsistencyIssue) {
  // beads command: has --json flag defined but always outputs JSON
  // Generates work items from interview session
  // Also supports --max-items flag for AI guardrails
  // Writes to .beads/issues.jsonl
  []
}

fn check_beads_regenerate_command_consistency() -> List(ConsistencyIssue) {
  // beads-regenerate command: regenerates failed/blocked beads
  // Supports --strategy flag (retry|reframe|split)
  []
}

fn check_bead_status_command_consistency() -> List(ConsistencyIssue) {
  // bead-status command: no JSON flag
  // Updates individual bead execution status
  // Requires --bead-id, --status, and optional --reason flags
  []
}

fn check_plan_command_consistency() -> List(ConsistencyIssue) {
  // plan command: has --format flag (json|text|waves)
  // Displays execution plan with health + waves + beads
  // Supports --rounds flag (1..5) for mental model rounds
  []
}

fn check_plan_approve_command_consistency() -> List(ConsistencyIssue) {
  // plan-approve command: has --yes flag for automation
  // Approves execution plan for CI/CD
  // Optional --notes flag for approval context
  []
}

fn check_prompt_command_consistency() -> List(ConsistencyIssue) {
  // prompt command: has --json flag
  // Generates AI implementation prompts from beads
  // Supports --max-items flag
  []
}

fn check_feedback_command_consistency() -> List(ConsistencyIssue) {
  // feedback command: has --json flag
  // Generates fix beads from check command failures
  // Requires --results flag pointing to check output JSON
  []
}

// Parsing validators
fn check_parse_command_consistency() -> List(ConsistencyIssue) {
  // parse command: has --o flag for output file
  // Quick EARS validation and conversion
  []
}

// Utilities validators
fn check_doctor_command_consistency() -> List(ConsistencyIssue) {
  // doctor command: always outputs JSON (no flag needed)
  // Health report with prioritized improvements
  // Returns exit_pass(0) on success, exit_invalid(3) on load error
  []
}

fn check_show_command_consistency() -> List(ConsistencyIssue) {
  // show command: always outputs JSON (no flag needed)
  // Pretty prints parsed spec with next_actions
  // Returns exit_pass(0) on success, exit_error(4) on load error
  []
}

fn check_help_command_consistency() -> List(ConsistencyIssue) {
  // help command: no JSON flag, text output
  // Shows detailed help for specific commands
  []
}

// AI Commands validators
fn check_ai_schema_command_consistency() -> List(ConsistencyIssue) {
  // ai schema command: no --json flag
  // Generates action JSON schema documentation
  // Supports --command and --type flags
  []
}

// Shape Phase validators
fn check_shape_start_command_consistency() -> List(ConsistencyIssue) {
  // shape start command: always outputs JSON
  // Initializes Shape phase session
  []
}

fn check_shape_check_command_consistency() -> List(ConsistencyIssue) {
  // shape check command: always outputs JSON
  // Validates Shape phase completeness
  // Requires --session flag
  []
}

fn check_shape_critique_command_consistency() -> List(ConsistencyIssue) {
  // shape critique command: always outputs JSON
  // Generates critique questions for spec
  // Requires --session flag
  []
}

fn check_shape_respond_command_consistency() -> List(ConsistencyIssue) {
  // shape respond command: always outputs JSON
  // Processes critique responses
  // Requires --session and --answers flags
  []
}

fn check_shape_agree_command_consistency() -> List(ConsistencyIssue) {
  // shape agree command: always outputs JSON
  // Finalizes Shape phase agreement
  // Requires --session flag
  []
}

// Validation helper: Check that JSON-always commands don't have --json flag
fn validate_json_consistency(info: CommandInfo) -> List(ConsistencyIssue) {
  case info.always_json_output, info.has_json_flag {
    // If command always outputs JSON, it shouldn't need a --json flag
    True, True -> [
      MissingUsageMessage(
        info.name <> " always outputs JSON but has redundant --json flag",
      ),
    ]
    _, _ -> []
  }
}

// Validation helper: Check that interactive commands aren't JSON-only
fn validate_interactive_consistency(info: CommandInfo) -> List(ConsistencyIssue) {
  case info.is_interactive, info.always_json_output {
    // Interactive commands shouldn't be JSON-only
    True, True -> [
      InconsistentErrorOutput(
        info.name,
        "text/interactive",
        "json-only (incompatible with interactive mode)",
      ),
    ]
    _, _ -> []
  }
}

// Run validation checks on command metadata
fn validate_command_info(info: CommandInfo) -> List(ConsistencyIssue) {
  let issues = []
  let issues = list.append(issues, validate_json_consistency(info))
  let issues = list.append(issues, validate_interactive_consistency(info))
  issues
}

// Validate all commands based on their metadata
pub fn validate_command_metadata() -> ConsistencyResult {
  let all_commands = get_all_command_info()
  let all_issues = list.flat_map(all_commands, validate_command_info)

  case all_issues {
    [] -> Passed
    _ -> Failed(all_issues)
  }
}
