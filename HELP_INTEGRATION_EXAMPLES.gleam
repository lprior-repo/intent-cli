/// EXAMPLE CODE: How to Use Extended Help Texts in Intent CLI
///
/// This file shows production-ready integration examples for displaying
/// the extended help text constants defined in cli_text_constants.gleam
///
/// The following constants are available and ready to use:
/// - cli_text_constants.check_extended_help
/// - cli_text_constants.validate_extended_help
/// - cli_text_constants.show_extended_help
/// - cli_text_constants.export_extended_help

// =============================================================================
// OPTION 1: Dedicated Help Command (RECOMMENDED)
// =============================================================================
//
// This is the cleanest approach: create a single `help` command that displays
// extended help for testing commands.
//
// Add to your CLI:
//
//   let cli =
//     glint.new()
//     |> glint.add(["help"], help_command())
//     |> glint.add(["check"], check_command())
//     |> glint.add(["validate"], validate_command())
//     |> glint.add(["show"], show_command())
//     |> glint.add(["export"], export_command())
//
// Usage: intent help check

fn help_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      ["check"] -> {
        io.println(cli_text_constants.check_extended_help)
        halt(exit_pass)
      }
      ["validate"] -> {
        io.println(cli_text_constants.validate_extended_help)
        halt(exit_pass)
      }
      ["show"] -> {
        io.println(cli_text_constants.show_extended_help)
        halt(exit_pass)
      }
      ["export"] -> {
        io.println(cli_text_constants.export_extended_help)
        halt(exit_pass)
      }
      [] -> {
        io.println("Intent CLI - Extended Help")
        io.println("")
        io.println("Usage: intent help <command>")
        io.println("")
        io.println("Testing Commands:")
        io.println("  check     - Execute spec tests against target URL")
        io.println("  validate  - Validate CUE spec file syntax and structure")
        io.println("  show      - Display parsed spec with formatted output")
        io.println("  export    - Export spec to JSON format")
        io.println("")
        io.println("Examples:")
        io.println("  intent help check")
        io.println("  intent help validate")
        io.println("  intent help show")
        io.println("  intent help export")
        halt(exit_pass)
      }
      cmd -> {
        io.println("Unknown command: " <> list.head(cmd) |> result.unwrap(""))
        io.println("Run 'intent help' for available commands")
        halt(exit_fail)
      }
    }
  })
  |> glint.description("Show extended help for a command")
}

// =============================================================================
// OPTION 2: Global --help-extended Flag
// =============================================================================
//
// Add a global flag that displays extended help instead of executing the
// command. Minimal changes to each command.
//
// Add to global flags:
//   |> glint.global_flag("help-extended", flag.no_arg(False))
//
// Then in each command, before executing:
//
//   case flag.get_bool(input.flags, "help-extended") {
//     Ok(True) -> {
//       io.println(cli_text_constants.check_extended_help)
//       halt(exit_pass)
//     }
//     _ -> {
//       // ... normal execution
//     }
//   }

fn check_command_with_extended_help() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // Check for --help-extended flag first
    case flag.get_bool(input.flags, "help-extended") {
      Ok(True) -> {
        io.println(cli_text_constants.check_extended_help)
        halt(exit_pass)
      }
      _ -> {
        // Normal check command implementation here
        case input.args {
          [spec_path, ..] -> {
            // ... existing check logic
            Nil
          }
          [] -> {
            let error =
              error_handler.usage_error(
                "check",
                "intent check <spec.cue> --target <url>",
              )
            halt(error_handler.output_error(error, False))
          }
        }
      }
    }
  })
  |> glint.description(cli_text_constants.cmd_check_desc)
  |> glint.flag("target", cli_flags.target_flag())
  |> glint.flag("json", cli_flags.json_flag())
  |> glint.flag("help-extended", flag.no_arg(False))
}

// =============================================================================
// OPTION 3: Print Help in Error Conditions
// =============================================================================
//
// When a command fails due to incorrect usage, show a truncated version of
// extended help with the error.

fn check_command_with_help_on_error() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [spec_path, ..] -> {
        // ... existing implementation
        Nil
      }
      [] -> {
        // Instead of just printing usage, show helpful context
        io.println_error("Missing required argument: spec file path")
        io.println_error("")
        io.println_error("QUICK START:")
        io.println_error("  intent check api.cue --target http://localhost:8080 --allow-localhost")
        io.println_error("")
        io.println_error("For more examples:")
        io.println_error("  intent help check")
        io.println_error("")
        halt(exit_fail)
      }
    }
  })
  |> glint.description(cli_text_constants.cmd_check_desc)
}

// =============================================================================
// OPTION 4: Integrate with Man Pages (For Distribution)
// =============================================================================
//
// Convert the extended help texts to markdown and generate man pages.
// This is useful for installed CLI tools.
//
// Steps:
// 1. Extract constants to markdown files:
//    check_extended_help -> man/intent-check.1.md
//    validate_extended_help -> man/intent-validate.1.md
//    show_extended_help -> man/intent-show.1.md
//    export_extended_help -> man/intent-export.1.md
//
// 2. Use pandoc to generate man pages:
//    pandoc man/intent-check.1.md -s -t man -o man/intent-check.1
//
// 3. Install with the CLI:
//    cp man/*.1 /usr/share/man/man1/
//
// 4. Access with: man intent-check

// =============================================================================
// OPTION 5: TUI Help Browser (Advanced)
// =============================================================================
//
// Use a terminal UI library (e.g., ratatui) to create an interactive help
// browser with navigation between commands.
//
// Features:
// - Keyboard navigation (arrow keys, vim keys)
// - Search within help text
// - Jump to examples
// - Copy commands to clipboard
//
// This is more complex but provides excellent UX.

// =============================================================================
// RECOMMENDED IMPLEMENTATION STEPS
// =============================================================================
//
// 1. IMMEDIATE (15-30 minutes)
//    - Add help_command() function (Option 1)
//    - Register with glint.add()
//    - Test: intent help check
//
// 2. SHORT TERM (next day)
//    - Add --help-extended flag to check/validate/show/export
//    - Update documentation with new help system
//
// 3. MEDIUM TERM (next week)
//    - Generate man pages from constants
//    - Update README with examples
//
// 4. LONG TERM (as needed)
//    - Monitor glint for native long_help() support
//    - Consider TUI help browser for interactive guide

// =============================================================================
// TESTING THE INTEGRATION
// =============================================================================
//
// Basic command tests:
//
//   gleam build
//   # Should compile with zero errors
//
//   ./build/dev/erlang/intent/priv/intent help
//   # Shows available help topics
//
//   ./build/dev/erlang/intent/priv/intent help check
//   # Shows check_extended_help
//
//   ./build/dev/erlang/intent/priv/intent help validate
//   # Shows validate_extended_help
//
//   ./build/dev/erlang/intent/priv/intent help show
//   # Shows show_extended_help
//
//   ./build/dev/erlang/intent/priv/intent help export
//   # Shows export_extended_help

// =============================================================================
// QUICK COPY-PASTE: Help Command
// =============================================================================
//
// Copy this entire function into src/intent.gleam if using Option 1:

/*
fn help_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      ["check"] -> {
        io.println(cli_text_constants.check_extended_help)
        halt(exit_pass)
      }
      ["validate"] -> {
        io.println(cli_text_constants.validate_extended_help)
        halt(exit_pass)
      }
      ["show"] -> {
        io.println(cli_text_constants.show_extended_help)
        halt(exit_pass)
      }
      ["export"] -> {
        io.println(cli_text_constants.export_extended_help)
        halt(exit_pass)
      }
      [] -> {
        io.println("Intent CLI - Extended Help")
        io.println("")
        io.println("Usage: intent help <command>")
        io.println("")
        io.println("Testing Commands:")
        io.println("  check     - Execute spec tests against target URL")
        io.println("  validate  - Validate CUE spec file syntax and structure")
        io.println("  show      - Display parsed spec with formatted output")
        io.println("  export    - Export spec to JSON format")
        io.println("")
        io.println("Examples:")
        io.println("  intent help check")
        io.println("  intent help validate")
        io.println("  intent help show")
        io.println("  intent help export")
        halt(exit_pass)
      }
      _ -> {
        io.println("Unknown command: " <> string.join(input.args, " "))
        io.println("Run 'intent help' for available commands")
        halt(exit_fail)
      }
    }
  })
  |> glint.description("Show extended help for a command")
}
*/

// Then register it in main() CLI setup:
/*
  glint.new()
  |> glint.add(["help"], help_command())
  |> glint.add(["check"], check_command())
  // ... other commands
*/

// =============================================================================
// SUMMARY: EXTENDED HELP CONSTANTS
// =============================================================================
//
// Location: src/intent/cli_text_constants.gleam, lines 246-705
// Total content: 448 lines of comprehensive help text
//
// Available constants:
// - check_extended_help (85 lines, lines 251-336)
// - validate_extended_help (109 lines, lines 338-447)
// - show_extended_help (113 lines, lines 449-562)
// - export_extended_help (141 lines, lines 564-705)
//
// Status: Compiled and ready to use
// Build: gleam build (succeeds)
// Next: Choose integration approach and implement
