/// Centralized error handling and formatting for Intent CLI
///
/// This module provides:
/// - Standardized error message formatting with visual markers
/// - Consistent exit code mapping (0=pass, 1=fail, 2=blocked, 3=invalid, 4=error)
/// - Error severity classification
/// - Context and recovery step generation
/// - JSON and text output formatting
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/json.{type Json}
import gleam/list
import gleam/string
import gleam_community/ansi

// ============================================================================
// Error Severity
// ============================================================================

/// Error severity levels for prioritization and display
pub type ErrorSeverity {
  Critical
  High
  Medium
  Low
}

/// Visual markers for each severity level
pub fn severity_marker(severity: ErrorSeverity) -> String {
  case severity {
    Critical -> "[CRITICAL]"
    High -> "[ERROR]"
    Medium -> "[WARNING]"
    Low -> "[INFO]"
  }
}

/// Color function for each severity
fn severity_color(severity: ErrorSeverity) -> fn(String) -> String {
  case severity {
    Critical -> ansi.red
    High -> ansi.red
    Medium -> ansi.yellow
    Low -> ansi.blue
  }
}

// ============================================================================
// Standard Error Template
// ============================================================================

/// Standard error message structure
pub type ErrorMessage {
  ErrorMessage(
    severity: ErrorSeverity,
    message: String,
    context: Dict(String, String),
    suggestion: String,
    recovery_steps: List(String),
    exit_code: Int,
  )
}

/// Format error message for text output
pub fn format_error_text(error: ErrorMessage) -> String {
  let color_fn = severity_color(error.severity)
  let marker = severity_marker(error.severity)

  // Build header
  let header = color_fn(marker <> ": " <> error.message)

  // Build context section
  let context_lines = case dict.size(error.context) {
    0 -> []
    _ -> {
      let context_items =
        dict.to_list(error.context)
        |> list.map(fn(pair) {
          let #(key, value) = pair
          "  " <> key <> ": " <> value
        })
      [ansi.dim("Context:"), ..context_items]
    }
  }

  // Build suggestion section
  let suggestion_line = case string.is_empty(error.suggestion) {
    True -> []
    False -> [ansi.bold("Suggestion:"), "  " <> error.suggestion]
  }

  // Build recovery steps section
  let recovery_lines = case error.recovery_steps {
    [] -> []
    steps -> {
      let step_items =
        list.index_map(steps, fn(step, idx) {
          "  " <> int.to_string(idx + 1) <> ". " <> step
        })
      [ansi.bold("Recovery Steps:"), ..step_items]
    }
  }

  // Build exit code footer
  let footer = ansi.dim("Exit code: " <> int.to_string(error.exit_code))

  // Combine all sections
  [header]
  |> list.append(context_lines)
  |> list.append(suggestion_line)
  |> list.append(recovery_lines)
  |> list.append([footer])
  |> string.join("\n")
}

/// Format error message for JSON output
pub fn format_error_json(error: ErrorMessage) -> Json {
  json.object([
    #("action", json.string("error")),
    #(
      "error",
      json.object([
        #(
          "severity",
          json.string(case error.severity {
            Critical -> "critical"
            High -> "high"
            Medium -> "medium"
            Low -> "low"
          }),
        ),
        #("message", json.string(error.message)),
        #(
          "context",
          json.object(
            dict.to_list(error.context)
            |> list.map(fn(p) { #(p.0, json.string(p.1)) }),
          ),
        ),
        #("suggestion", json.string(error.suggestion)),
        #("recovery_steps", json.array(error.recovery_steps, json.string)),
        #("exit_code", json.int(error.exit_code)),
      ]),
    ),
  ])
}

// ============================================================================
// Output Functions
// ============================================================================

/// Output error to stderr and return exit code
pub fn output_error(error: ErrorMessage, is_json: Bool) -> Int {
  case is_json {
    True -> {
      error
      |> format_error_json
      |> json.to_string
      |> io.println_error
    }
    False -> {
      error
      |> format_error_text
      |> io.println_error
    }
  }
  error.exit_code
}

/// Create a generic error message
pub fn generic_error(
  message: String,
  suggestion: String,
  recovery_steps: List(String),
) -> ErrorMessage {
  ErrorMessage(
    severity: High,
    message: message,
    context: dict.new(),
    suggestion: suggestion,
    recovery_steps: recovery_steps,
    exit_code: 4,
  )
}

/// Create a usage error (for CLI argument errors)
pub fn usage_error(command: String, usage: String) -> ErrorMessage {
  ErrorMessage(
    severity: Medium,
    message: "Invalid usage of " <> command,
    context: dict.new(),
    suggestion: usage,
    recovery_steps: ["Use --help for more information"],
    exit_code: 3,
  )
}

/// Create a simple error with just message and exit code
pub fn simple_error(message: String, exit_code: Int) -> ErrorMessage {
  ErrorMessage(
    severity: High,
    message: message,
    context: dict.new(),
    suggestion: "",
    recovery_steps: [],
    exit_code: exit_code,
  )
}
