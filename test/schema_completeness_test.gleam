import gleam/list
import gleam/string
import gleeunit/should
import intent/schema_validator

/// Verify every command in the commands list has a corresponding JSON Schema file
pub fn all_commands_have_schemas_test() {
  let commands = schema_validator.all_commands()
  let missing =
    commands
    |> list.filter(fn(cmd) { !schema_validator.has_schema(cmd) })

  case missing {
    [] -> should.be_true(True)
    cmds -> {
      let msg = "Missing schemas for: " <> string.join(cmds, ", ")
      panic as msg
    }
  }
}

/// Verify the base schema exists
pub fn base_schema_exists_test() {
  schema_validator.has_schema("base")
  |> should.be_true
}

/// Verify schema files are valid JSON (can be loaded without error)
pub fn all_schemas_are_valid_json_test() {
  let commands = schema_validator.all_commands()
  let invalid =
    commands
    |> list.filter(fn(cmd) {
      case schema_validator.load_schema(cmd) {
        Ok(_) -> False
        Error(_) -> True
      }
    })

  case invalid {
    [] -> should.be_true(True)
    cmds -> {
      let msg = "Invalid schema files for: " <> string.join(cmds, ", ")
      panic as msg
    }
  }
}
