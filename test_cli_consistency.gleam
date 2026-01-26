import gleam/io
import gleam/list
import intent/cli_consistency

pub fn main() {
  io.println("Testing CLI Consistency Module")
  io.println("==============================\n")

  // Test 1: Validate all commands
  io.println("Test 1: validate_all_commands()")
  let result = cli_consistency.validate_all_commands()
  let formatted = cli_consistency.format_result(result)
  io.println(formatted)
  io.println("")

  // Test 2: Validate command metadata
  io.println("Test 2: validate_command_metadata()")
  let metadata_result = cli_consistency.validate_command_metadata()
  let metadata_formatted = cli_consistency.format_result(metadata_result)
  io.println(metadata_formatted)
  io.println("")

  // Test 3: Get all command info
  io.println("Test 3: get_all_command_info()")
  let commands = cli_consistency.get_all_command_info()
  let count = list.length(commands)
  io.println("Total commands: " <> int_to_string(count))
  io.println("")

  // Test 4: Generate command report
  io.println("Test 4: generate_command_report()")
  let report = cli_consistency.generate_command_report()
  io.println(report)
  io.println("")

  // Test 5: Validate specific commands
  io.println("Test 5: Validate specific commands")

  io.println("  - validate_check_command (all correct):")
  let check_result =
    cli_consistency.validate_check_command(True, True, True, True, True)
  io.println("    " <> cli_consistency.format_result(check_result))

  io.println("  - validate_check_command (missing json flag):")
  let check_result2 =
    cli_consistency.validate_check_command(False, True, True, True, True)
  io.println("    " <> cli_consistency.format_result(check_result2))

  io.println("  - validate_show_command (all correct):")
  let show_result = cli_consistency.validate_show_command(True, True, True, True)
  io.println("    " <> cli_consistency.format_result(show_result))

  io.println("  - validate_doctor_command (all correct):")
  let doctor_result =
    cli_consistency.validate_doctor_command(True, True, True)
  io.println("    " <> cli_consistency.format_result(doctor_result))

  io.println("\nAll tests completed!")
}

fn int_to_string(i: Int) -> String {
  case i {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "10"
    11 -> "11"
    12 -> "12"
    13 -> "13"
    14 -> "14"
    15 -> "15"
    16 -> "16"
    17 -> "17"
    18 -> "18"
    19 -> "19"
    20 -> "20"
    21 -> "21"
    22 -> "22"
    23 -> "23"
    24 -> "24"
    25 -> "25"
    26 -> "26"
    27 -> "27"
    28 -> "28"
    29 -> "29"
    30 -> "30"
    31 -> "31"
    32 -> "32"
    _ -> "many"
  }
}
