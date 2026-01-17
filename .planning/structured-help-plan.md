# Structured Help Implementation Plan

## Phase 0: Research ✅ COMPLETE
- Analyzed current glint help system
- Identified 4 major CUE schemas (intent.cue, interview.cue, kirk.cue, ai_interview.cue)
- Found existing structured output patterns (--cue, --json flags)
- Documented 27 commands with their flags and capabilities

## Phase 1: CUE Schema Design (CURRENT)

### Create schema/command.cue

Define the authoritative command metadata schema following Intent's patterns:

```cue
// Command metadata schema - source of truth for CLI structure
package command

#CLI: {
	name:        "intent"
	version:     string
	description: string
	commands: [string]: #Command
}

#Command: {
	name:        string
	description: string
	category:    #Category

	// Usage
	usage:    string
	examples: [...string]

	// Arguments and flags
	arguments: [...#Argument]
	flags: [string]: #Flag

	// Output capabilities
	outputs: {
		formats: [...#OutputFormat]
		schema?: string  // Reference to CUE schema (e.g., "intent.cue#Spec")
	}

	// AI protocol info
	ai_protocol?: #AIProtocol

	// Exit codes
	exit_codes: [#ExitCode, ...#ExitCode]

	// Related commands
	related?: [...string]
}

#Category: "core" | "interview" | "beads" | "kirk" | "planning" | "review" | "utility"

#Argument: {
	name:        string
	description: string
	required:    bool | *true
	type:        "path" | "string" | "id"
	examples:    [...string]
}

#Flag: {
	name:        string
	short?:      string
	description: string
	type:        "bool" | "string" | "int"
	required:    bool | *false
	default?:    string | bool | int
	values?:     [...string]  // For enum flags
	env_var?:    string       // Environment variable fallback
}

#OutputFormat: "json" | "cue" | "text" | "xml" | "tap" | "sarif" | "junit"

#AIProtocol: {
	non_interactive: bool  // Can run without human interaction
	deterministic:   bool  // Same input -> same output
	input_format:    string  // "ears_pattern", "spec_path", etc.
	output_format:   string  // "cue_directive", "json_result", etc.
	validation:      string  // "reject_if_vague", "ears_conformance", etc.
}

#ExitCode: {
	code:    int
	meaning: string
	when:    string  // When this exit code occurs
}
```

### Create data/commands.cue

Populate actual command metadata:

```cue
package data

import "command"

cli: command.#CLI & {
	name:        "intent"
	version:     "0.1.0"
	description: "Contract-driven requirements engineering and API testing"

	commands: {
		about: {
			name:        "about"
			description: "Comprehensive explanation of Intent's purpose and AI-first design"
			category:    "core"
			usage:       "intent about"
			examples: ["intent about"]
			arguments: []
			flags: {}
			outputs: formats: ["text"]
			exit_codes: [{code: 0, meaning: "success", when: "always"}]
		}

		interview: {
			name:        "interview"
			description: "Guided specification discovery through structured interview"
			category:    "interview"
			usage:       "intent interview [flags]"
			examples: [
				"intent interview --cue --profile api",
				"intent interview --cue --session interview-abc123 --answer \"THE SYSTEM SHALL...\"",
				"intent interview --profile cli --export spec.cue",
			]
			arguments: []
			flags: {
				profile: {
					name:        "profile"
					description: "System profile: api, cli, event, data, workflow, or ui"
					type:        "string"
					default:     "api"
					values: ["api", "cli", "event", "data", "workflow", "ui"]
				}
				cue: {
					name:        "cue"
					description: "AI AGENT MODE - Output CUE (no interactive prompts)"
					type:        "bool"
					default:     false
				}
				session: {
					name:        "session"
					description: "Session ID for resuming Q&A loop (requires --cue)"
					type:        "string"
				}
				answer: {
					name:        "answer"
					description: "Answer text in EARS format (requires --cue --session)"
					type:        "string"
				}
			}
			outputs: {
				formats: ["cue", "text"]
				schema:  "ai_interview.cue#AIDirective"
			}
			ai_protocol: {
				non_interactive: true
				deterministic:   true
				input_format:    "ears_pattern"
				output_format:   "cue_directive"
				validation:      "ears_conformance"
			}
			exit_codes: [
				{code: 0, meaning: "success", when: "interview completed or question asked"},
				{code: 4, meaning: "error", when: "validation failed or invalid session"},
			]
			related: ["beads", "quality", "gaps"]
		}

		// ... more commands
	}
}
```

## Phase 2: Gleam Implementation

### Create src/intent/command_metadata.gleam

Module to load and expose command metadata:

```gleam
import gleam/dict.{type Dict}
import gleam/result
import intent/loader
import intent/types

pub type CLI {
  CLI(name: String, version: String, description: String, commands: Dict(String, Command))
}

pub type Command {
  Command(
    name: String,
    description: String,
    category: Category,
    usage: String,
    examples: List(String),
    arguments: List(Argument),
    flags: Dict(String, Flag),
    outputs: OutputInfo,
    ai_protocol: Option(AIProtocol),
    exit_codes: List(ExitCode),
    related: List(String),
  )
}

pub type Category {
  Core
  Interview
  Beads
  Kirk
  Planning
  Review
  Utility
}

// ... other types

/// Load command metadata from data/commands.cue
pub fn load_metadata() -> Result(CLI, String) {
  // Use existing loader.gleam pattern
  loader.load_cue_file("data/commands.cue")
  |> result.then(parse_cli_metadata)
}

/// Get metadata for a specific command
pub fn get_command(name: String) -> Result(Command, String) {
  use cli <- result.try(load_metadata())
  dict.get(cli.commands, name)
  |> result.replace_error("Command not found: " <> name)
}

/// Format command metadata as CUE
pub fn format_as_cue(cmd: Command) -> String {
  // Generate CUE representation
}

/// Format command metadata as JSON
pub fn format_as_json(cmd: Command) -> String {
  // Generate JSON using gleam/json
}
```

### Add manifest command to src/intent.gleam

```gleam
/// The `manifest` command - machine-readable CLI capabilities
fn manifest_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let format =
      flag.get_string(input.flags, "format")
      |> result.unwrap("json")

    case command_metadata.load_metadata() {
      Ok(cli) -> {
        let output = case format {
          "cue" -> command_metadata.format_cli_as_cue(cli)
          "json" -> command_metadata.format_cli_as_json(cli)
          _ -> {
            io.println_error("Error: format must be 'cue' or 'json'")
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
    <> "  • All 27 commands with descriptions and categories\n"
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
```

### Add protocol command to src/intent.gleam

```gleam
/// The `protocol` command - CUE schema for specific command
fn protocol_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [command_name, ..] -> {
        case command_metadata.get_command(command_name) {
          Ok(cmd) -> {
            let output = command_metadata.format_as_cue(cmd)
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
    "🤖 AI-FRIENDLY: CUE schema and protocol for a specific command\n\n"
    <> "PURPOSE: Get structured contract defining command's behavior\n\n"
    <> "Output is a CUE schema including:\n"
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
```

### Enhanced help command

```gleam
/// The `help` command - enhanced with format support
fn help_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let format =
      flag.get_string(input.flags, "format")
      |> result.unwrap("text")

    let command_name = case input.args {
      [name, ..] -> name
      [] -> ""
    }

    case command_name {
      "" -> {
        // Show all commands
        case format {
          "json" -> show_all_commands_json()
          "cue" -> show_all_commands_cue()
          _ -> show_all_commands_text()  // Default glint help
        }
      }
      name -> {
        // Show specific command
        case command_metadata.get_command(name) {
          Ok(cmd) -> {
            let output = case format {
              "json" -> command_metadata.format_as_json(cmd)
              "cue" -> command_metadata.format_as_cue(cmd)
              _ -> format_command_text(cmd)
            }
            io.println(output)
            halt(exit_pass)
          }
          Error(_) -> {
            io.println_error("Unknown command: " <> name)
            halt(exit_error)
          }
        }
      }
    }
  })
  |> glint.description("Get help for commands (supports --format=json|cue|text)")
  |> glint.flag(
    "format",
    flag.string()
      |> flag.default("text")
      |> flag.description("Output format: text, json, or cue"),
  )
}
```

## Phase 3: Data Population

### Populate data/commands.cue with all 27 commands

Extract metadata from existing help text in src/intent.gleam:
- about, check, validate, show, version, export, lint, analyze, improve
- interview, beads, execute-beads, bead-status, history, diff, sessions
- quality, invert, coverage, gaps, compact, prototext, ears
- ears-interview, lattice-analyze, generate-contract, plan-structure, generate-beads
- review-requirements, review-contracts, review-structure, review-beads
- parse, effects, plan, plan-approve, beads-regenerate

## Phase 4: Testing

### Test scenarios

```bash
# Manifest command
intent manifest --format=json
intent manifest --format=cue

# Protocol command
intent protocol interview
intent protocol check
intent protocol beads

# Enhanced help
intent help interview --format=json
intent help --format=cue
```

### Validation checks

1. CUE validation: `cue vet schema/command.cue data/commands.cue`
2. Schema completeness: All 27 commands defined
3. JSON output valid: Parseable by `jq`
4. CUE output valid: Parseable by `cue eval`
5. Text output backward compatible: Existing help still works

## Phase 5: Verification

### Contract-driven philosophy alignment

✅ CUE as source of truth (schema/command.cue defines structure)
✅ Multiple output formats (JSON, CUE, text)
✅ AI-first design (manifest and protocol for programmatic discovery)
✅ Type safety (Gleam types mirror CUE schemas)
✅ Required fields (no defaults in command definitions)
✅ Structured errors (follows ai_errors.gleam pattern)

### Success criteria

- [ ] `intent manifest --format=json` returns all command metadata as valid JSON
- [ ] `intent manifest --format=cue` returns valid CUE that passes `cue vet`
- [ ] `intent protocol interview` returns interview command schema as CUE
- [ ] `intent help interview --format=json` returns JSON metadata
- [ ] All 27 commands documented in data/commands.cue
- [ ] CUE validation passes: `cue vet schema/command.cue data/commands.cue`
- [ ] Gleam compilation passes with new modules
- [ ] Tests pass (moon run :test)
- [ ] Documentation updated (CLAUDE.md references new commands)

## Implementation Order

1. ✅ Research (COMPLETE)
2. ⏳ Design CUE schemas (schema/command.cue) - CURRENT
3. Create sample data (data/commands.cue with 3-5 commands)
4. Implement Gleam module (src/intent/command_metadata.gleam)
5. Add manifest command
6. Add protocol command
7. Test with sample commands
8. Populate all 27 commands
9. Add help command enhancement
10. Full testing and verification
11. Documentation update

## Risks and Mitigations

**Risk**: CUE schema too complex, hard to maintain
**Mitigation**: Start minimal, iterate based on actual needs

**Risk**: Help text drift (glint.description vs data/commands.cue)
**Mitigation**: Eventually generate glint.description from CUE (future work)

**Risk**: Performance impact of loading CUE on every command
**Mitigation**: Only load for manifest/protocol/help commands, not general CLI

**Risk**: Breaking existing help text
**Mitigation**: Keep glint.description as-is, add new commands alongside

## Future Enhancements

- Generate glint help text from CUE (DRY)
- Add command composition protocol (chaining commands)
- Extend AI protocol with example request/response pairs
- Add schema versioning for backward compatibility
- Create interactive command builder for AI agents
