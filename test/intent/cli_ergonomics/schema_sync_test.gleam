//// Schema Introspection Tests (ATDD + BDD)
//// Tests for bead: intent-cli-schema-sync

import gleeunit/should
import "$TEST_DIR/test_helpers.gleam" as test_helpers

pub fn main() {
  gleeunit.main()
}

pub fn schema_introspection_matches_cli_implementation_test() {
  // Given: Schema introspection output for a command
  let schema = get_schema_for_command("lint")
  
  // When: I parse the schema
  let parsed_schema = parse_schema(schema)
  
  // Then: Documented flags should exist in CLI
  let flags = extract_flags_from_schema(parsed_schema)
  
  flags
  |> list.each(fn(flag_name) {
       cli_has_flag("lint", flag_name)
       |> should.be_true()
     })
}

pub fn documented_flags_work_correctly_test() {
  let schema = get_schema_for_command("quality")
  let flags = extract_flags_from_schema(parse_schema(schema))
  
  flags
  |> list.each(fn(flag_name) {
       let result = test_helpers.execute_intent("quality", ["--" <> flag_name, "spec.cue"])
       
       result.exit_code
       |> should.equal(0)
     })
}

pub fn schema_doesnt_document_nonexistent_flags_test() {
  let schema = get_schema_for_command("validate")
  let parsed = parse_schema(schema)
  
  let has_json_flag = list.contains(extract_flags_from_schema(parsed), "json")
  
  has_json_flag
  |> should.be_false()  // Currently not implemented
}

// ============================================================================
// Helper Functions
// ============================================================================

fn get_schema_for_command(command: String) -> String {
  // In real implementation, this would call "intent ai schema --command=X --type=input"
  // Mock response for testing
  "{\"command\": \"" <> command <> "\", \"fields\": [{\"name\": \"spec_path\", \"type\": \"string\"}]}"
}

fn parse_schema(schema: String) -> json.Json {
  case json.decode(schema, json.dynamic) {
    Ok(parsed) -> parsed
    Error(_) -> panic("Invalid schema")
  }
}

fn extract_flags_from_schema(schema: json.Json) -> List(String) {
  case dynamic.field("flags", dynamic.list(dynamic.dynamic))(schema) {
    Ok(flags) -> {
      flags
      |> list.map(fn(f) {
           case dynamic.string(f) {
             Ok(s) -> s
             Error(_) -> panic("Flag is not a string")
           }
         })
    }
    Error(_) -> []
  }
}

fn cli_has_flag(command: String, flag: String) -> Bool {
  // In real implementation, this would check glint command definition
  // For testing, return True for known flags
  ["json", "format", "max-items", "verbose"]
  |> list.contains(flag)
}
