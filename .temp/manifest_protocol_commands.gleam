// ADD THESE LINES TO main() AFTER show_command():
//   |> glint.add(at: ["manifest"], do: manifest_command())
//   |> glint.add(at: ["protocol"], do: protocol_command())

// ADD THESE FUNCTIONS AFTER show_command() definition:

/// The `manifest` command - machine-readable CLI capabilities
fn manifest_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let format =
      flag.get_string(input.flags, "format")
      |> result.unwrap("json")

    case command_metadata.load_metadata() {
      Ok(cli) -> {
        let output = case format {
          "cue" -> {
            // For CUE, just use cue export directly
            io.println_error(
              "Hint: Use 'cue export schema/command.cue data/commands.cue' for CUE output",
            )
            halt(exit_error)
          }
          "json" -> command_metadata.format_cli_as_json(cli)
          _ -> {
            io.println_error("Error: format must be 'json' or 'cue'")
            halt(exit_error)
          }
        }
        io.println(output)
        halt(exit_pass)
      }
      Error(e) -> {
        io.println_error("Error loading manifest: " <> e)
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "🤖 AI-FRIENDLY: Machine-readable CLI capabilities and command metadata\n\n"
    <> "PURPOSE: Discover all commands, flags, and protocols programmatically\n\n"
    <> "Output includes:\n"
    <> "  • All commands with descriptions and categories\n"
    <> "  • Flag definitions (type, default, required, validation)\n"
    <> "  • Output formats and schemas (CUE/JSON/text)\n"
    <> "  • AI protocol information (input/output formats, determinism)\n"
    <> "  • Exit codes and error handling\n"
    <> "  • Related commands and workflows\n\n"
    <> "Use this to:\n"
    <> "  • Discover available commands programmatically\n"
    <> "  • Validate command invocations before execution\n"
    <> "  • Generate documentation or CLI wrappers\n"
    <> "  • Understand AI-friendly vs interactive commands\n\n"
    <> "Examples:\n"
    <> "  intent manifest --format=json\n"
    <> "  intent manifest --format=cue",
  )
  |> glint.flag(
    "format",
    flag.string()
      |> flag.default("json")
      |> flag.description("Output format: json or cue"),
  )
}

/// The `protocol` command - get structured metadata for a specific command
fn protocol_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [command_name, ..] -> {
        case command_metadata.get_command(command_name) {
          Ok(cmd) -> {
            let output = command_metadata.format_command_as_json(cmd)
            io.println(output)
            halt(exit_pass)
          }
          Error(e) -> {
            io.println_error("Error: " <> e)
            halt(exit_error)
          }
        }
      }
      [] -> {
        io.println_error("Error: command name required")
        io.println_error("Usage: intent protocol <command>")
        io.println_error("Example: intent protocol interview")
        halt(exit_error)
      }
    }
  })
  |> glint.description(
    "🤖 AI-FRIENDLY: Get structured metadata for a specific command\n\n"
    <> "PURPOSE: Get structured contract defining command's behavior\n\n"
    <> "Output includes:\n"
    <> "  • Command signature (arguments, flags, types)\n"
    <> "  • Input validation rules\n"
    <> "  • Output format schemas\n"
    <> "  • AI protocol specification (if applicable)\n"
    <> "  • Exit codes and error conditions\n"
    <> "  • Usage examples\n\n"
    <> "Use this to:\n"
    <> "  • Understand command contracts before invoking\n"
    <> "  • Validate inputs against schema\n"
    <> "  • Parse outputs deterministically\n"
    <> "  • Generate type-safe wrappers\n\n"
    <> "Examples:\n"
    <> "  intent protocol interview\n"
    <> "  intent protocol check\n"
    <> "  intent protocol beads",
  )
}
