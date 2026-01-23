/// Tests for the help system integration
/// Verifies that CommandHelp templates render correctly for CLI use
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import intent/help

// =============================================================================
// format_help tests
// =============================================================================

pub fn format_help_includes_name_section_test() {
  let help_text = help.check_help() |> help.format_help()

  // Should include NAME section
  string.contains(help_text, "NAME") |> should.be_true()
  string.contains(help_text, "intent check") |> should.be_true()
}

pub fn format_help_includes_synopsis_test() {
  let help_text = help.check_help() |> help.format_help()

  // Should include SYNOPSIS section
  string.contains(help_text, "SYNOPSIS") |> should.be_true()
  string.contains(help_text, "<spec.cue>") |> should.be_true()
}

pub fn format_help_includes_examples_test() {
  let help_text = help.check_help() |> help.format_help()

  // Should include EXAMPLES section
  string.contains(help_text, "EXAMPLES") |> should.be_true()
  // Should include actual example commands
  string.contains(help_text, "intent check api.cue") |> should.be_true()
}

pub fn format_help_includes_see_also_test() {
  let help_text = help.check_help() |> help.format_help()

  // Should include SEE ALSO section with related commands
  string.contains(help_text, "SEE ALSO") |> should.be_true()
  string.contains(help_text, "validate") |> should.be_true()
}

pub fn format_help_includes_options_test() {
  let help_text = help.check_help() |> help.format_help()

  // Should include OPTIONS section
  string.contains(help_text, "OPTIONS") |> should.be_true()
  string.contains(help_text, "--target") |> should.be_true()
  string.contains(help_text, "--json") |> should.be_true()
}

// =============================================================================
// format_for_glint tests - the new function for glint integration
// =============================================================================

pub fn format_for_glint_starts_with_description_test() {
  // Glint descriptions should start with the short description
  let description = help.format_for_glint(help.check_help())

  // Should start with the short description
  string.starts_with(
    description,
    "Run spec against a target URL and verify behaviors",
  )
  |> should.be_true()
}

pub fn format_for_glint_includes_examples_test() {
  let description = help.format_for_glint(help.check_help())

  // Should include examples section
  string.contains(description, "Examples:") |> should.be_true()
  string.contains(description, "intent check api.cue") |> should.be_true()
}

pub fn format_for_glint_includes_related_commands_test() {
  let description = help.format_for_glint(help.check_help())

  // Should include related commands
  string.contains(description, "Related:") |> should.be_true()
  string.contains(description, "validate") |> should.be_true()
}

pub fn format_for_glint_doctor_test() {
  let description = help.format_for_glint(help.doctor_help())

  // Should include short description
  string.contains(description, "health") |> should.be_true()
  // Should include examples
  string.contains(description, "Examples:") |> should.be_true()
  string.contains(description, "intent doctor") |> should.be_true()
}

pub fn format_for_glint_validate_test() {
  let description = help.format_for_glint(help.validate_help())

  // Should include examples
  string.contains(description, "Examples:") |> should.be_true()
  string.contains(description, "intent validate") |> should.be_true()
  // Should include related
  string.contains(description, "Related:") |> should.be_true()
}

// =============================================================================
// get_command_help tests
// =============================================================================

pub fn get_command_help_returns_correct_help_test() {
  // Test that we can retrieve help for known commands
  case help.get_command_help("check") {
    Some(h) -> h.name |> should.equal("check")
    None -> should.fail()
  }

  case help.get_command_help("validate") {
    Some(h) -> h.name |> should.equal("validate")
    None -> should.fail()
  }

  case help.get_command_help("doctor") {
    Some(h) -> h.name |> should.equal("doctor")
    None -> should.fail()
  }
}

pub fn get_command_help_returns_none_for_unknown_test() {
  case help.get_command_help("nonexistent") {
    None -> True |> should.be_true()
    Some(_) -> should.fail()
  }
}

// =============================================================================
// Validate help completeness for priority commands
// =============================================================================

pub fn check_help_has_required_fields_test() {
  let h = help.check_help()

  // Name and description
  h.name |> should.equal("check")
  { h.short_desc != "" } |> should.be_true()

  // Should have examples
  { h.examples != [] } |> should.be_true()

  // Should have related commands
  { h.related != [] } |> should.be_true()

  // Should have flags
  { h.flags != [] } |> should.be_true()
}

pub fn doctor_help_has_required_fields_test() {
  let h = help.doctor_help()

  h.name |> should.equal("doctor")
  { h.short_desc != "" } |> should.be_true()
  { h.examples != [] } |> should.be_true()
  { h.related != [] } |> should.be_true()
}

pub fn validate_help_has_required_fields_test() {
  let h = help.validate_help()

  h.name |> should.equal("validate")
  { h.short_desc != "" } |> should.be_true()
  { h.examples != [] } |> should.be_true()
  { h.related != [] } |> should.be_true()
}

// =============================================================================
// Test all commands have help defined
// =============================================================================

pub fn all_commands_list_is_not_empty_test() {
  let commands = help.all_commands()
  { commands != [] } |> should.be_true()
}

pub fn all_commands_have_names_test() {
  let all_have_names =
    help.all_commands()
    |> list.all(fn(h) { h.name != "" })
  all_have_names |> should.be_true()
}
