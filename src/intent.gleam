/// Intent CLI - Planning and bead generation tool
import argv
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import glint
import glint/flag
import intent/cli_ui

/// Exit codes
const exit_pass = 0

const exit_fail = 1

const exit_error = 4

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
    |> glint.add(at: ["beads-regenerate"], do: beads_regenerate_command())
    // Vision and Ready commands
    |> glint.add(at: ["vision"], do: vision_command())
    |> glint.add(at: ["ready"], do: ready_command())

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

    case input.args {
      [] -> {
        case resume_session {
          "" -> {
            case profile {
              "" -> {
                cli_ui.print_error("Error: --profile required when not resuming")
                exit(exit_error)
              }
              _ -> run_interview(profile, "")
            }
          }
          session_id -> run_interview(profile, session_id)
        }
      }
      _ -> {
        cli_ui.print_error("Error: interview command takes no arguments")
        exit(exit_error)
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

    case input.args {
      [] -> {
        case session_id {
          "" -> {
            cli_ui.print_error("Error: --session required")
            exit(exit_error)
          }
          _ -> generate_beads(session_id, format, output_dir)
        }
      }
      _ -> {
        cli_ui.print_error("Error: beads command takes no arguments")
        exit(exit_error)
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

    case input.args {
      [] -> {
        case bead_id {
          "" -> {
            cli_ui.print_error("Error: --bead-id required")
            exit(exit_error)
          }
          _ -> check_bead_status(bead_id)
        }
      }
      _ -> {
        cli_ui.print_error("Error: bead-status command takes no arguments")
        exit(exit_error)
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
  glint.command(fn(_) {
    // TODO: Implement history listing
    cli_ui.print_header("Interview History")
    io.println("No sessions found")
    exit(exit_pass)
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

    case input.args {
      [] -> {
        case session_id {
          "" -> {
            cli_ui.print_error("Error: --session required")
            exit(exit_error)
          }
          _ -> show_session_diff(session_id)
        }
      }
      _ -> {
        cli_ui.print_error("Error: diff command takes no arguments")
        exit(exit_error)
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

    case input.args {
      [] -> list_sessions(profile)
      _ -> {
        cli_ui.print_error("Error: sessions command takes no arguments")
        exit(exit_error)
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

    case input.args {
      [] -> {
        case notes {
          "" -> io.println("Plan generation - notes: none")
          _ -> io.println("Plan generation - notes: " <> notes)
        }
        cli_ui.print_success("Plan command - implementation needed")
        exit(exit_pass)
      }
      _ -> {
        cli_ui.print_error("Error: plan command takes no arguments")
        exit(exit_error)
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

    case input.args {
      [] -> {
        case strategy {
          "" -> io.println("Suggest next task")
          _ -> io.println("Suggest next task - strategy: " <> strategy)
        }
        cli_ui.print_success("Plan next command - implementation needed")
        exit(exit_pass)
      }
      _ -> {
        cli_ui.print_error("Error: plan-next command takes no arguments")
        exit(exit_error)
      }
    }
  })
  |> glint.description("Suggest next task to work on")
  |> glint.flag(
    "strategy",
    flag.string()
      |> flag.default("")
      |> flag.description("Selection strategy"),
  )
}

/// ============================================================================
/// PLAN APPROVE COMMAND
/// ============================================================================

fn plan_approve_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      [plan_id, ..] -> {
        cli_ui.print_success("Plan approved: " <> plan_id)
        exit(exit_pass)
      }
      [] -> {
        cli_ui.print_error("Error: plan ID required")
        exit(exit_error)
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

    case input.args {
      [] -> {
        case session_id {
          "" -> {
            cli_ui.print_error("Error: --session required")
            exit(exit_error)
          }
          _ -> regenerate_beads(session_id)
        }
      }
      _ -> {
        cli_ui.print_error("Error: beads-regenerate command takes no arguments")
        exit(exit_error)
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
/// VISION COMMAND
/// ============================================================================

fn vision_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let output_dir =
      flag.get_string(input.flags, "out")
      |> result.unwrap("")

    case input.args {
      [] -> {
        case output_dir {
          "" -> io.println("Generate vision document")
          _ -> io.println("Generate vision document to: " <> output_dir)
        }
        cli_ui.print_success("Vision command - implementation needed")
        exit(exit_pass)
      }
      _ -> {
        cli_ui.print_error("Error: vision command takes no arguments")
        exit(exit_error)
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

    case input.args {
      [] -> {
        case output_dir {
          "" -> io.println("Generate ready document")
          _ -> io.println("Generate ready document to: " <> output_dir)
        }
        cli_ui.print_success("Ready command - implementation needed")
        exit(exit_pass)
      }
      _ -> {
        cli_ui.print_error("Error: ready command takes no arguments")
        exit(exit_error)
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

@external(erlang, "erlang", "halt")
fn exit(code: Int) -> Nil
