import gleam/dict
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import intent/error_handler

pub fn main() {
  gleeunit.main()
}

pub fn severity_marker_critical_test() {
  error_handler.severity_marker(error_handler.Critical)
  |> should.equal("[CRITICAL]")
}

pub fn severity_marker_high_test() {
  error_handler.severity_marker(error_handler.High)
  |> should.equal("[ERROR]")
}

pub fn severity_marker_medium_test() {
  error_handler.severity_marker(error_handler.Medium)
  |> should.equal("[WARNING]")
}

pub fn severity_marker_low_test() {
  error_handler.severity_marker(error_handler.Low)
  |> should.equal("[INFO]")
}

pub fn format_error_text_basic_test() {
  let error =
    error_handler.ErrorMessage(
      severity: error_handler.High,
      message: "Test error",
      context: dict.new(),
      suggestion: "Try this",
      recovery_steps: [],
      exit_code: 1,
    )

  let formatted = error_handler.format_error_text(error)
  let is_valid = string.contains(formatted, "Test error") && string.contains(formatted, "Try this")
  is_valid |> should.equal(True)
}

pub fn format_error_text_with_context_test() {
  let error =
    error_handler.ErrorMessage(
      severity: error_handler.High,
      message: "Connection failed",
      context: dict.from_list([#("url", "http://localhost:8080")]),
      suggestion: "Check if service is running",
      recovery_steps: ["Restart service", "Check logs"],
      exit_code: 4,
    )

  let formatted = error_handler.format_error_text(error)
  let contains_all = string.contains(formatted, "Connection failed") && string.contains(formatted, "http://localhost:8080") && string.contains(formatted, "Check if service is running") && string.contains(formatted, "Restart service")
  contains_all |> should.equal(True)
}

pub fn simple_error_test() {
  let error =
    error_handler.simple_error("Something went wrong", 1)

  error.message |> should.equal("Something went wrong")
  error.exit_code |> should.equal(1)
}

pub fn generic_error_test() {
  let error =
    error_handler.generic_error(
      "Invalid input",
      "Please provide valid data",
      ["Check input format", "Consult documentation"],
    )

  error.message |> should.equal("Invalid input")
  error.suggestion |> should.equal("Please provide valid data")
  list.length(error.recovery_steps) |> should.equal(2)
}
