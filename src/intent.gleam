/// Intent CLI - Human-writes, AI-verifies, AI-implements
/// Contract-driven API testing tool
///
/// ## Module Overview (for AI agents)
///
/// This is the main CLI entry point. All commands are organized into submodules:
///
/// **Extracted Modules**:
/// - cli/check.gleam: Core spec commands (check, validate, show, export, lint, analyze, improve)
/// - cli/kirk.gleam: KIRK analysis commands (quality, invert, coverage, gaps, effects, compact, prototext, ears, parse)
/// - cli/interview.gleam: Interview commands (interview, sessions, history, diff, beads, bead-status)
/// - cli/plan.gleam: Plan mode commands (plan, plan-approve, beads-regenerate)
/// - cli/common.gleam: Shared utilities (ExitCode, halt, exit)
///
/// ## Key Dependencies
/// - loader.gleam: CUE file loading and parsing
/// - runner.gleam: Spec execution engine
/// - interview.gleam: Interview domain logic
/// - output.gleam: Result formatting
import argv
import gleam/io
import glint
import intent/cli/check as cli_check
import intent/cli/interview as cli_interview
import intent/cli/kirk as cli_kirk
import intent/cli/plan as cli_plan

pub fn main() {
  let cli =
    glint.new()
    |> glint.with_name("intent")
    |> glint.with_pretty_help(glint.default_pretty_help())
    // Core commands (from cli/check.gleam)
    |> glint.add(at: ["check"], do: cli_check.check_command())
    |> glint.add(at: ["validate"], do: cli_check.validate_command())
    |> glint.add(at: ["show"], do: cli_check.show_command())
    |> glint.add(at: ["export"], do: cli_check.export_command())
    |> glint.add(at: ["lint"], do: cli_check.lint_command())
    |> glint.add(at: ["analyze"], do: cli_check.analyze_command())
    |> glint.add(at: ["improve"], do: cli_check.improve_command())
    // Interview commands (from cli/interview.gleam)
    |> glint.add(at: ["interview"], do: cli_interview.interview_command())
    |> glint.add(at: ["beads"], do: cli_interview.beads_command())
    |> glint.add(at: ["bead-status"], do: cli_interview.bead_status_command())
    |> glint.add(at: ["history"], do: cli_interview.history_command())
    |> glint.add(at: ["diff"], do: cli_interview.diff_command())
    |> glint.add(at: ["sessions"], do: cli_interview.sessions_command())
    // KIRK commands (from cli/kirk.gleam)
    |> glint.add(at: ["quality"], do: cli_kirk.quality_command())
    |> glint.add(at: ["invert"], do: cli_kirk.invert_command())
    |> glint.add(at: ["coverage"], do: cli_kirk.coverage_command())
    |> glint.add(at: ["gaps"], do: cli_kirk.gaps_command())
    |> glint.add(at: ["compact"], do: cli_kirk.compact_command())
    |> glint.add(at: ["prototext"], do: cli_kirk.prototext_command())
    |> glint.add(at: ["ears"], do: cli_kirk.ears_command())
    |> glint.add(at: ["parse"], do: cli_kirk.parse_command())
    |> glint.add(at: ["effects"], do: cli_kirk.effects_command())
    // Plan commands (from cli/plan.gleam)
    |> glint.add(at: ["plan"], do: cli_plan.plan_command())
    |> glint.add(at: ["plan-approve"], do: cli_plan.plan_approve_command())
    |> glint.add(at: ["beads-regenerate"], do: cli_plan.beads_regenerate_command())

  // Execute and handle errors properly (glint.run doesn't set exit codes)
  case glint.execute(cli, argv.load().arguments) {
    // Error from glint (e.g., invalid flag syntax, unknown command)
    Error(error_message) -> {
      io.println(error_message)
      halt(1)
    }
    // Help was requested - print it and exit 0
    Ok(glint.Help(help_text)) -> {
      io.println(help_text)
      Nil
    }
    // Command executed successfully
    Ok(glint.Out(_)) -> Nil
  }
}

@external(erlang, "intent_ffi", "halt")
fn halt(code: Int) -> Nil
