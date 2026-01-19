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
  has_json_flag: Bool,
  uses_output_mode: Bool,
  uses_cli_ui_print: Bool,
  has_correct_usage: Bool,
) -> ConsistencyResult {
  let issues = []
  let issues = case has_json_flag {
    False -> [MissingJsonFlag("show"), ..issues]
    True -> issues
  }
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
        "intent show <spec.cue> [--json]",
        "missing/incorrect",
      ),
      ..issues
    ]
    True -> issues
  }
  validate_command_consistency(
    CommandSpec(
      name: "show",
      expects_json_flag: True,
      expected_output_mode: "from_json_flag",
      expected_error_output: "cli_ui.print_error",
      valid_exit_codes: [0, 4],
      usage_pattern: "intent show <spec.cue> [--json]",
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
  has_json_flag: Bool,
  uses_output_mode: Bool,
  has_correct_usage: Bool,
) -> ConsistencyResult {
  let issues = []
  let issues = case has_json_flag {
    False -> [MissingJsonFlag("doctor"), ..issues]
    True -> issues
  }
  let issues = case uses_output_mode {
    False -> [IncorrectOutputMode("doctor", "from_flags", "missing"), ..issues]
    True -> issues
  }
  let issues = case has_correct_usage {
    False -> [
      InconsistentUsageFormat(
        "doctor",
        "intent doctor <spec.cue> [--json]",
        "missing/incorrect",
      ),
      ..issues
    ]
    True -> issues
  }
  validate_command_consistency(
    CommandSpec(
      name: "doctor",
      expects_json_flag: True,
      expected_output_mode: "from_flags",
      expected_error_output: "cli_ui.print_error",
      valid_exit_codes: [0, 3],
      usage_pattern: "intent doctor <spec.cue> [--json]",
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

pub fn validate_all_commands() -> ConsistencyResult {
  let all_issues = []

  let check_issues =
    validate_check_command(
      has_json_flag: True,
      uses_output_mode: True,
      uses_cli_ui_print: True,
      uses_exit_error_for_validation: True,
      has_correct_usage: True,
    )
  let all_issues = case check_issues {
    Failed(issues) -> list.append(all_issues, issues)
    Passed -> all_issues
  }

  let validate_issues =
    validate_validate_command(
      uses_output_mode: True,
      uses_cli_ui_print: True,
      has_correct_usage: True,
    )
  let all_issues = case validate_issues {
    Failed(issues) -> list.append(all_issues, issues)
    Passed -> all_issues
  }

  let show_issues =
    validate_show_command(
      has_json_flag: True,
      uses_output_mode: True,
      uses_cli_ui_print: True,
      has_correct_usage: True,
    )
  let all_issues = case show_issues {
    Failed(issues) -> list.append(all_issues, issues)
    Passed -> all_issues
  }

  let export_issues = validate_export_command(has_correct_usage: True)
  let all_issues = case export_issues {
    Failed(issues) -> list.append(all_issues, issues)
    Passed -> all_issues
  }

  let lint_issues = validate_lint_command(has_correct_usage: True)
  let all_issues = case lint_issues {
    Failed(issues) -> list.append(all_issues, issues)
    Passed -> all_issues
  }

  let analyze_issues = validate_analyze_command(has_correct_usage: True)
  let all_issues = case analyze_issues {
    Failed(issues) -> list.append(all_issues, issues)
    Passed -> all_issues
  }

  let improve_issues = validate_improve_command(has_correct_usage: True)
  let all_issues = case improve_issues {
    Failed(issues) -> list.append(all_issues, issues)
    Passed -> all_issues
  }

  let doctor_issues =
    validate_doctor_command(
      has_json_flag: True,
      uses_output_mode: True,
      has_correct_usage: True,
    )
  let all_issues = case doctor_issues {
    Failed(issues) -> list.append(all_issues, issues)
    Passed -> all_issues
  }

  case all_issues {
    [] -> Passed
    _ -> Failed(all_issues)
  }
}
