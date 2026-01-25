/// Output mode control for AI-friendly vs interactive output
///
/// This module provides the OutputMode type that controls how the CLI
/// presents information. When in Json mode, all interactive UI elements
/// (spinners, colors, progress bars) are suppressed to ensure clean,
/// parseable JSON output.
pub type OutputMode {
  /// Full interactive UI with colors, spinners, and progress bars
  Interactive

  /// Machine-readable JSON on stdout only, no UI noise
  Json

  /// Minimal output for piping/scripting
  Quiet

  /// Explicit AI/robot consumption mode with action metadata
  Robot
}

/// Check if interactive UI should be shown
pub fn is_interactive(mode: OutputMode) -> Bool {
  case mode {
    Interactive -> True
    _ -> False
  }
}

/// Check if JSON output mode is active
/// Robot mode is JSON-compatible for backward compatibility
pub fn is_json(mode: OutputMode) -> Bool {
  case mode {
    Json -> True
    Robot -> True
    _ -> False
  }
}

/// Check if robot mode is active
pub fn is_robot(mode: OutputMode) -> Bool {
  case mode {
    Robot -> True
    _ -> False
  }
}

/// Check if spinners should be shown
pub fn should_show_spinner(mode: OutputMode) -> Bool {
  case mode {
    Interactive -> True
    _ -> False
  }
}

/// Check if colors should be shown
pub fn should_show_colors(mode: OutputMode) -> Bool {
  case mode {
    Interactive -> True
    _ -> False
  }
}

/// Convert from --json flag to OutputMode
pub fn from_json_flag(is_json: Bool) -> OutputMode {
  case is_json {
    True -> Json
    False -> Interactive
  }
}

/// Convert from --quiet flag to OutputMode
pub fn from_quiet_flag(is_quiet: Bool) -> OutputMode {
  case is_quiet {
    True -> Quiet
    False -> Interactive
  }
}

/// Convert from --robot flag to OutputMode
pub fn from_robot_flag(is_robot: Bool) -> OutputMode {
  case is_robot {
    True -> Robot
    False -> Interactive
  }
}

/// Get the appropriate mode from both flags (JSON takes precedence)
pub fn from_flags(is_json: Bool, is_quiet: Bool) -> OutputMode {
  case is_json, is_quiet {
    True, _ -> Json
    False, True -> Quiet
    False, False -> Interactive
  }
}

/// Get the appropriate mode from all flags (Robot takes precedence)
pub fn from_flags_with_robot(
  is_robot is_robot: Bool,
  is_json is_json: Bool,
  is_quiet is_quiet: Bool,
) -> OutputMode {
  case is_robot, is_json, is_quiet {
    True, _, _ -> Robot
    False, True, _ -> Json
    False, False, True -> Quiet
    False, False, False -> Interactive
  }
}
