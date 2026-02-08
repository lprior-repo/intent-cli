/// Intent CLI - Planning and bead generation tool
import argv
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/cli_ui
import intent/validation
import intent/plan_emit_beads

/// Exit codes
const exit_pass = 0

const exit_fail = 1

pub fn main() {
  let args = argv.load().arguments

  // Check for --version flag (before normalization)
  case
    list.find(args, fn(arg) {
      arg == "--version"
      || arg == "-v"
      || arg == "--version=true"
      || arg == "--version=True"
    })
  {
    Ok(_) -> {
      io.println("intent v0.1.0")
      exit(exit_pass)
    }
    Error(_) -> Nil
  }

  // Handle 'help' command specially - convert to --help for the target command
  let processed_args = case args {
    ["help", ..] -> {
      // Convert "help <command>" to "<command> --help"
      case args {
        ["help"] -> ["--help"]
        ["help", command, ..rest] -> [command, "--help", ..rest]
        _ -> ["--help"]
      }
    }
    _ -> normalize_cli_args(args)
  }

  let app =
    glint.new()
    |> glint.with_name("intent")
    |> glint.with_pretty_help(glint.default_pretty_help())
    |> glint.add(at: ["interview"], do: interview_command())
    |> glint.add(at: ["beads"], do: beads_command())
    |> glint.add(at: ["bead-status"], do: bead_status_command())
    |> glint.add(at: ["history"], do: history_command())
    |> glint.add(at: ["diff"], do: diff_command())
    |> glint.add(at: ["sessions"], do: sessions_command())
    // Plan commands
    |> glint.add(at: ["plan"], do: plan_command())
    |> glint.add(at: ["plan-next"], do: plan_next_command())
    |> glint.add(at: ["plan-approve"], do: plan_approve_command())
    |> glint.add(at: ["plan-emit-beads"], do: plan_emit_beads_command())
    |> glint.add(at: ["beads-regenerate"], do: beads_regenerate_command())
    // Vision and Ready commands
    |> glint.add(at: ["vision"], do: vision_command())
    |> glint.add(at: ["ready"], do: ready_command())
    // KIRK commands
    |> glint.add(at: ["effects"], do: effects_command())

  case glint.execute(app, processed_args) {
    Ok(glint.Out(_)) -> {
      io.println_error(
        "error: failed to run command
cause:
  0: command not found",
      )
      exit(exit_fail)
    }
    Ok(glint.Help(help_text)) -> {
      io.println(help_text)
      exit(exit_pass)
    }
    Error(err) -> {
      io.println_error(err)
      exit(exit_fail)
    }
  }
}

pub fn normalize_cli_args(args: List(String)) -> List(String) {
  case args {
    [arg, next, ..rest] -> {
      case classify_flag(arg) {
        BoolFlag -> {
          case is_bool_literal(next) {
            True -> [
              arg <> "=" <> string.lowercase(next),
              ..normalize_cli_args(rest)
            ]
            False -> [arg <> "=true", ..normalize_cli_args([next, ..rest])]
          }
        }
        ValueFlag -> {
          case is_flag_token(next) {
            True -> [arg, ..normalize_cli_args([next, ..rest])]
            False -> [arg <> "=" <> next, ..normalize_cli_args(rest)]
          }
        }
        UnknownFlag -> [arg, ..normalize_cli_args([next, ..rest])]
      }
    }
    [arg] -> {
      case classify_flag(arg) {
        BoolFlag -> [arg <> "=true"]
        _ -> [arg]
      }
    }
    [] -> []
  }
}

type FlagKind {
  BoolFlag
  ValueFlag
  UnknownFlag
}

fn classify_flag(arg: String) -> FlagKind {
  let is_candidate = string.starts_with(arg, "--") && !string.contains(arg, "=")
  bool_to_flag_kind(is_candidate, string.drop_left(arg, 2))
}

fn bool_to_flag_kind(is_candidate: Bool, flag_name: String) -> FlagKind {
  case is_candidate {
    False -> UnknownFlag
    True -> {
      case is_known_bool_flag(flag_name) {
        True -> BoolFlag
        False -> {
          case is_known_value_flag(flag_name) {
            True -> ValueFlag
            False -> UnknownFlag
          }
        }
      }
    }
  }
}

fn is_known_bool_flag(flag_name: String) -> Bool {
  case flag_name {
    "json" -> True
    "verbose" -> True
    "quiet" -> True
    "yes" -> True
    "draft" -> True
    "confirm" -> True
    "dry-run" -> True
    "execute" -> True
    _ -> False
  }
}

fn is_known_value_flag(flag_name: String) -> Bool {
  case flag_name {
    "only" -> True
    "profile" -> True
    "resume" -> True
    "answer" -> True
    "bead-id" -> True
    "status" -> True
    "reason" -> True
    "session" -> True
    "format" -> True
    "notes" -> True
    "strategy" -> True
    "output" -> True
    "out" -> True
    "name" -> True
    "target" -> True
    _ -> False
  }
}

fn is_flag_token(value: String) -> Bool {
  string.starts_with(value, "--")
}

fn is_bool_literal(value: String) -> Bool {
  case string.lowercase(value) {
    "true" -> True
    "false" -> True
    _ -> False
  }
}

/// ============================================================================
/// INTERVIEW COMMAND
/// ============================================================================

fn interview_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let profile =
      flag.get_string(input.flags, "profile")
      |> result.unwrap("")

    let resume_session =
      flag.get_string(input.flags, "resume")
      |> result.unwrap("")

    // Validate no extra arguments
    case validation.validate_no_args(input.args, "interview") {
      Ok(Nil) -> {
        case resume_session {
          "" -> {
            // Validate profile when not resuming
            case validation.validate_profile(profile) {
              Ok(valid_profile) -> run_interview(valid_profile, "")
              Error(err) -> {
                cli_ui.print_error(err)
                exit(exit_fail)
              }
            }
          }
          session_id -> run_interview(profile, session_id)
        }
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Run interactive interview session to capture requirements")
  |> glint.flag(
    "profile",
    flag.string()
      |> flag.default("")
      |> flag.description("Profile type: api, cli, event, data, workflow, ui"),
  )
  |> glint.flag(
    "resume",
    flag.string()
      |> flag.default("")
      |> flag.description("Resume a previous session by ID"),
  )
}

fn run_interview(profile: String, session_id: String) -> Nil {
  // TODO: Implement interview functionality
  cli_ui.print_header("Interview")
  io.println("Profile: " <> profile)
  case session_id {
    "" -> Nil
    _ -> io.println("Resume session: " <> session_id)
  }
  cli_ui.print_success("Interview command - implementation needed")
  exit(exit_pass)
}

/// ============================================================================
/// BEADS COMMAND
/// ============================================================================

fn beads_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let format =
      flag.get_string(input.flags, "format")
      |> result.unwrap("")

    let output_dir =
      flag.get_string(input.flags, "out")
      |> result.unwrap("")

    let session_id =
      flag.get_string(input.flags, "session")
      |> result.unwrap("")

    // Validate no extra arguments
    case validation.validate_no_args(input.args, "beads") {
      Ok(Nil) -> {
        // Validate session_id is required
        case validation.validate_required_flag("session", session_id) {
          Ok(valid_session) -> {
            // Validate format
            case validation.validate_format(format) {
              Ok(valid_format) -> generate_beads(valid_session, valid_format, output_dir)
              Error(err) -> {
                cli_ui.print_error(err)
                exit(exit_fail)
              }
            }
          }
          Error(err) -> {
            cli_ui.print_error(err)
            exit(exit_fail)
          }
        }
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Generate beads from interview session")
  |> glint.flag(
    "session",
    flag.string()
      |> flag.default("")
      |> flag.description("Session ID to generate beads from"),
  )
  |> glint.flag(
    "format",
    flag.string()
      |> flag.default("json")
      |> flag.description("Output format: json, jsonl, markdown"),
  )
  |> glint.flag(
    "out",
    flag.string()
      |> flag.default("")
      |> flag.description("Output directory (default: current directory)"),
  )
}

fn generate_beads(session_id: String, format: String, _output_dir: String) -> Nil {
  // TODO: Implement bead generation
  cli_ui.print_header("Generate Beads")
  io.println("Session: " <> session_id)
  io.println("Format: " <> format)
  cli_ui.print_success("Beads command - implementation needed")
  exit(exit_pass)
}

/// ============================================================================
/// BEAD STATUS COMMAND
/// ============================================================================

fn bead_status_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let bead_id =
      flag.get_string(input.flags, "bead-id")
      |> result.unwrap("")

    // Validate no extra arguments
    case validation.validate_no_args(input.args, "bead-status") {
      Ok(Nil) -> {
        // Validate bead_id is required
        case validation.validate_required_flag("bead-id", bead_id) {
          Ok(valid_bead_id) -> check_bead_status(valid_bead_id)
          Error(err) -> {
            cli_ui.print_error(err)
            exit(exit_fail)
          }
        }
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Check status of a bead")
  |> glint.flag(
    "bead-id",
    flag.string()
      |> flag.default("")
      |> flag.description("Bead ID to check"),
  )
}

fn check_bead_status(bead_id: String) -> Nil {
  // TODO: Implement bead status check
  cli_ui.print_header("Bead Status")
  io.println("Bead ID: " <> bead_id)
  cli_ui.print_success("Bead status command - implementation needed")
  exit(exit_pass)
}

/// ============================================================================
/// HISTORY COMMAND
/// ============================================================================

fn history_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // Validate no extra arguments
    case validation.validate_no_args(input.args, "history") {
      Ok(Nil) -> {
        // TODO: Implement history listing
        cli_ui.print_header("Interview History")
        io.println("No sessions found")
        exit(exit_pass)
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("List all interview sessions")
}

/// ============================================================================
/// DIFF COMMAND
/// ============================================================================

fn diff_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session")
      |> result.unwrap("")

    // Validate no extra arguments
    case validation.validate_no_args(input.args, "diff") {
      Ok(Nil) -> {
        // Validate session_id is required
        case validation.validate_required_flag("session", session_id) {
          Ok(valid_session) -> show_session_diff(valid_session)
          Error(err) -> {
            cli_ui.print_error(err)
            exit(exit_fail)
          }
        }
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Show diff for session changes")
  |> glint.flag(
    "session",
    flag.string()
      |> flag.default("")
      |> flag.description("Session ID to show diff for"),
  )
}

fn show_session_diff(session_id: String) -> Nil {
  // TODO: Implement diff
  cli_ui.print_header("Session Diff")
  io.println("Session: " <> session_id)
  cli_ui.print_success("Diff command - implementation needed")
  exit(exit_pass)
}

/// ============================================================================
/// SESSIONS COMMAND
/// ============================================================================

fn sessions_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let profile =
      flag.get_string(input.flags, "profile")
      |> result.unwrap("")

    // Validate no extra arguments
    case validation.validate_no_args(input.args, "sessions") {
      Ok(Nil) -> list_sessions(profile)
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("List interview sessions")
  |> glint.flag(
    "profile",
    flag.string()
      |> flag.default("")
      |> flag.description("Filter by profile type"),
  )
}

fn list_sessions(profile_filter: String) -> Nil {
  // TODO: Implement session listing
  cli_ui.print_header("Sessions")
  case profile_filter {
    "" -> Nil
    _ -> io.println("Filter: " <> profile_filter)
  }
  io.println("No sessions found")
  exit(exit_pass)
}

/// ============================================================================
/// PLAN COMMAND
/// ============================================================================

fn plan_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let notes =
      flag.get_string(input.flags, "notes")
      |> result.unwrap("")

    // Validate no extra arguments
    case validation.validate_no_args(input.args, "plan") {
      Ok(Nil) -> {
        case notes {
          "" -> io.println("Plan generation - notes: none")
          _ -> io.println("Plan generation - notes: " <> notes)
        }
        cli_ui.print_success("Plan command - implementation needed")
        exit(exit_pass)
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Generate plan from current context")
  |> glint.flag(
    "notes",
    flag.string()
      |> flag.default("")
      |> flag.description("Additional notes for plan generation"),
  )
}

/// ============================================================================
/// PLAN NEXT COMMAND
/// ============================================================================

fn plan_next_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let strategy =
      flag.get_string(input.flags, "strategy")
      |> result.unwrap("")

    // Validate no extra arguments
    case validation.validate_no_args(input.args, "plan-next") {
      Ok(Nil) -> {
        // Validate strategy
        case validation.validate_strategy(strategy) {
          Ok(valid_strategy) -> {
            case valid_strategy {
              "" -> io.println("Suggest next task")
              _ -> io.println("Suggest next task - strategy: " <> valid_strategy)
            }
            cli_ui.print_success("Plan next command - implementation needed")
            exit(exit_pass)
          }
          Error(err) -> {
            cli_ui.print_error(err)
            exit(exit_fail)
          }
        }
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Suggest next task to work on")
  |> glint.flag(
    "strategy",
    flag.string()
      |> flag.default("")
      |> flag.description("Selection strategy: page_rank, critical_path, shortest, risk_first"),
  )
}

/// ============================================================================
/// PLAN APPROVE COMMAND
/// ============================================================================

fn plan_approve_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // Validate single argument
    case validation.validate_single_arg(input.args, "plan-approve") {
      Ok(plan_id) -> {
        cli_ui.print_success("Plan approved: " <> plan_id)
        exit(exit_pass)
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Approve a generated plan")
}

/// ============================================================================
/// BEADS REGENERATE COMMAND
/// ============================================================================

fn beads_regenerate_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let session_id =
      flag.get_string(input.flags, "session")
      |> result.unwrap("")

    // Validate no extra arguments
    case validation.validate_no_args(input.args, "beads-regenerate") {
      Ok(Nil) -> {
        // Validate session_id is required
        case validation.validate_required_flag("session", session_id) {
          Ok(valid_session) -> regenerate_beads(valid_session)
          Error(err) -> {
            cli_ui.print_error(err)
            exit(exit_fail)
          }
        }
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Regenerate beads from session")
  |> glint.flag(
    "session",
    flag.string()
      |> flag.default("")
      |> flag.description("Session ID to regenerate beads from"),
  )
}

fn regenerate_beads(session_id: String) -> Nil {
  // TODO: Implement bead regeneration
  cli_ui.print_header("Regenerate Beads")
  io.println("Session: " <> session_id)
  cli_ui.print_success("Beads regenerate command - implementation needed")
  exit(exit_pass)
}

/// ============================================================================
/// PLAN EMIT BEADS COMMAND
/// ============================================================================

fn plan_emit_beads_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let dry_run =
      flag.get_bool(input.flags, "dry-run")
      |> result.unwrap(False)

    let execute =
      flag.get_bool(input.flags, "execute")
      |> result.unwrap(False)

    let force =
      flag.get_bool(input.flags, "force")
      |> result.unwrap(False)

    let target =
      flag.get_string(input.flags, "target")
      |> result.unwrap("br")

    // Validate single argument (session_id)
    case input.args {
      [] -> {
        cli_ui.print_error("Error: session ID required")
        io.println("\nUsage: intent plan-emit-beads <session-id> [--dry-run] [--execute] [--force] [--target br]")
        exit(exit_fail)
      }
      [session_id] -> {
        // Validate target is "br" (only supported target for now)
        case target {
          "br" -> emit_beads_to_br(session_id, dry_run, execute, force)
          _ -> {
            cli_ui.print_error("Error: unsupported target '" <> target <> "'")
            io.println("Supported targets: br")
            exit(exit_fail)
          }
        }
      }
      _ -> {
        cli_ui.print_error("Error: plan-emit-beads takes exactly one argument (session-id)")
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Emit beads from session to br (idempotent - won't create duplicates)")
  |> glint.flag(
    "dry-run",
    flag.bool()
      |> flag.default(True)
      |> flag.description("Show what would be created without creating beads (default: true)"),
  )
  |> glint.flag(
    "execute",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Actually create beads in br (requires explicit confirmation)"),
  )
  |> glint.flag(
    "force",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Bypass idempotency checks and create all beads (use with caution)"),
  )
  |> glint.flag(
    "target",
    flag.string()
      |> flag.default("br")
      |> flag.description("Target system (default: br)"),
  )
}

fn emit_beads_to_br(session_id: String, dry_run: Bool, execute: Bool, force: Bool) -> Nil {
  cli_ui.print_header("Emit Beads to br")

  // Safety check: require --execute flag to actually create beads
  case !dry_run && !execute {
    True -> {
      cli_ui.print_error("Error: --execute flag required to create beads")
      io.println("\nThis command will create beads in br using the session: " <> session_id)
      io.println("\nTo see what would be created (dry run):")
      io.println("  intent plan-emit-beads " <> session_id)
      io.println("\nTo actually create beads:")
      io.println("  intent plan-emit-beads " <> session_id <> " --execute")
      io.println("\nTo bypass idempotency checks (force recreation):")
      io.println("  intent plan-emit-beads " <> session_id <> " --execute --force")
      exit(exit_fail)
    }
    False -> {
      case plan_emit_beads.emit_beads(session_id, dry_run, execute, force) {
        Ok(result) -> {
          io.println(plan_emit_beads.format_result(result))
          exit(exit_pass)
        }
        Error(err) -> {
          cli_ui.print_error(plan_emit_beads.format_error(err))
          exit(exit_fail)
        }
      }
    }
  }
}

/// ============================================================================
/// VISION COMMAND
/// ============================================================================

fn vision_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let output_dir =
      flag.get_string(input.flags, "out")
      |> result.unwrap("")

    // Validate no extra arguments
    case validation.validate_no_args(input.args, "vision") {
      Ok(Nil) -> {
        case output_dir {
          "" -> io.println("Generate vision document")
          _ -> io.println("Generate vision document to: " <> output_dir)
        }
        cli_ui.print_success("Vision command - implementation needed")
        exit(exit_pass)
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Generate vision document")
  |> glint.flag(
    "out",
    flag.string()
      |> flag.default("")
      |> flag.description("Output directory"),
  )
}

/// ============================================================================
/// READY COMMAND
/// ============================================================================

fn ready_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let output_dir =
      flag.get_string(input.flags, "out")
      |> result.unwrap("")

    // Validate no extra arguments
    case validation.validate_no_args(input.args, "ready") {
      Ok(Nil) -> {
        case output_dir {
          "" -> io.println("Generate ready document")
          _ -> io.println("Generate ready document to: " <> output_dir)
        }
        cli_ui.print_success("Ready command - implementation needed")
        exit(exit_pass)
      }
      Error(err) -> {
        cli_ui.print_error(err)
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Generate ready document")
  |> glint.flag(
    "out",
    flag.string()
      |> flag.default("")
      |> flag.description("Output directory"),
  )
}

/// ============================================================================
/// EFFECTS COMMAND
/// ============================================================================

fn effects_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)

    let behavior =
      flag.get_string(input.flags, "behavior")
      |> result.unwrap("")

    // Validate argument count (0 or 1)
    case input.args {
      [] -> {
        cli_ui.print_error("Error: spec file required")
        exit(exit_fail)
      }
      [spec_file] -> {
        analyze_effects(spec_file, behavior, json)
      }
      _ -> {
        cli_ui.print_error("Error: effects command takes at most one argument")
        exit(exit_fail)
      }
    }
  })
  |> glint.description("Analyze behaviors for second-order effects")
  |> glint.flag(
    "behavior",
    flag.string()
      |> flag.default("")
      |> flag.description("Analyze specific behavior only"),
  )
  |> glint.flag(
    "json",
    flag.bool()
      |> flag.default(False)
      |> flag.description("Output as JSON"),
  )
}

fn analyze_effects(spec_file: String, behavior_filter: String, as_json: Bool) -> Nil {
  cli_ui.print_header("Second-Order Effects Analysis")
  io.println("Spec file: " <> spec_file)

  case behavior_filter {
    "" -> io.println("Analyzing all behaviors")
    _ -> io.println("Analyzing behavior: " <> behavior_filter)
  }

  case as_json {
    True -> io.println("\nOutput format: JSON")
    False -> io.println("\nOutput format: CLI")
  }

  // TODO: Implement actual CUE parsing and effects analysis
  // For now, show a demo
  io.println("\nDemo effects analysis:")
  io.println("📝 State Change: Creates new resource")
  io.println("📧 Notification: May trigger events")
  io.println("🔗 Cascade: May affect related records")
  io.println("⚠️  Race Condition: Concurrent access possible")
  io.println("🔄 Rollback Required: Operation should be reversible")

  cli_ui.print_success("Effects analysis complete")
  exit(exit_pass)
}

@external(erlang, "erlang", "halt")
fn exit(code: Int) -> Nil
