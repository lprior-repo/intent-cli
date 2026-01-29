import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import intent/cli_consistency

pub fn main() {
  gleeunit.main()
}

pub fn validate_all_commands_test() {
  let result = cli_consistency.validate_all_commands()
  case result {
    cli_consistency.Passed -> should.be_ok(Ok(Nil))
    cli_consistency.Failed(_issues) -> {
      // Print issues for debugging
      let _formatted = cli_consistency.format_result(result)
      should.fail()
    }
  }
}

pub fn validate_command_metadata_test() {
  let result = cli_consistency.validate_command_metadata()
  // This should pass since our metadata is consistent
  result
  |> should.equal(cli_consistency.Passed)
}

pub fn get_all_command_info_test() {
  let commands = cli_consistency.get_all_command_info()
  // Should have 32 commands
  commands
  |> list.length
  |> should.equal(32)
}

pub fn generate_command_report_test() {
  let report = cli_consistency.generate_command_report()
  // Should contain basic structure
  report
  |> string.contains("Intent CLI Command Summary")
  |> should.be_true()

  report
  |> string.contains("Total commands: 32")
  |> should.be_true()
}

pub fn format_command_summary_test() {
  let info =
    cli_consistency.CommandInfo(
      name: "validate",
      category: cli_consistency.CoreSpec,
      always_json_output: False,
      is_interactive: False,
      primary_flags: [],
      valid_exit_codes: [0, 3],
    )

  let summary = cli_consistency.format_command_summary(info)

  summary
  |> string.contains("Command: validate")
  |> should.be_true()

  summary
  |> string.contains("Category: Core Spec Operations")
  |> should.be_true()

  summary
  |> string.contains("Output: Text")
  |> should.be_true()
}

pub fn validate_check_command_test() {
  // Test check command validation
  let result = cli_consistency.validate_check_command(True, True, True, True)
  result
  |> should.equal(cli_consistency.Passed)
}

pub fn validate_show_command_test() {
  // Test show command validation
  let result = cli_consistency.validate_show_command(True, True, True)
  result
  |> should.equal(cli_consistency.Passed)
}

pub fn validate_doctor_command_test() {
  // Test doctor command validation
  let result = cli_consistency.validate_doctor_command(True, True)
  result
  |> should.equal(cli_consistency.Passed)
}
