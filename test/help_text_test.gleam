import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import intent/cli_text_constants as text

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// TEST HELPERS
// =============================================================================

fn is_valid_description(desc: String) -> Bool {
  let min_length = 30
  let max_length = 100
  let length = string.length(desc)
  length >= min_length && length <= max_length
}

fn has_positive_length(s: String) -> Bool {
  string.length(s) > 0
}

fn has_substantial_length(s: String) -> Bool {
  string.length(s) > 100
}

fn has_what_why_when(help_text: String) -> Bool {
  string.contains(help_text, "WHAT")
  && string.contains(help_text, "WHY")
  && string.contains(help_text, "WHEN")
}

// =============================================================================
// COMMAND DESCRIPTION TESTS - All 24 Commands
// =============================================================================

pub fn check_command_description_exists_test() {
  has_positive_length(text.cmd_check_desc)
  |> should.equal(True)
}

pub fn check_description_valid_length_test() {
  is_valid_description(text.cmd_check_desc)
  |> should.equal(True)
}

pub fn validate_command_description_exists_test() {
  has_positive_length(text.cmd_validate_desc)
  |> should.equal(True)
}

pub fn validate_description_valid_length_test() {
  is_valid_description(text.cmd_validate_desc)
  |> should.equal(True)
}

pub fn show_command_description_exists_test() {
  has_positive_length(text.cmd_show_desc)
  |> should.equal(True)
}

pub fn show_description_valid_length_test() {
  is_valid_description(text.cmd_show_desc)
  |> should.equal(True)
}

pub fn export_command_description_exists_test() {
  has_positive_length(text.cmd_export_desc)
  |> should.equal(True)
}

pub fn export_description_valid_length_test() {
  is_valid_description(text.cmd_export_desc)
  |> should.equal(True)
}

pub fn lint_command_description_exists_test() {
  has_positive_length(text.cmd_lint_desc)
  |> should.equal(True)
}

pub fn lint_description_valid_length_test() {
  is_valid_description(text.cmd_lint_desc)
  |> should.equal(True)
}

pub fn analyze_command_description_exists_test() {
  has_positive_length(text.cmd_analyze_desc)
  |> should.equal(True)
}

pub fn analyze_description_valid_length_test() {
  is_valid_description(text.cmd_analyze_desc)
  |> should.equal(True)
}

pub fn improve_command_description_exists_test() {
  has_positive_length(text.cmd_improve_desc)
  |> should.equal(True)
}

pub fn improve_description_valid_length_test() {
  is_valid_description(text.cmd_improve_desc)
  |> should.equal(True)
}

pub fn doctor_command_description_exists_test() {
  has_positive_length(text.cmd_doctor_desc)
  |> should.equal(True)
}

pub fn doctor_description_valid_length_test() {
  is_valid_description(text.cmd_doctor_desc)
  |> should.equal(True)
}

pub fn interview_command_description_exists_test() {
  has_positive_length(text.cmd_interview_desc)
  |> should.equal(True)
}

pub fn interview_description_valid_length_test() {
  is_valid_description(text.cmd_interview_desc)
  |> should.equal(True)
}

pub fn beads_command_description_exists_test() {
  has_positive_length(text.cmd_beads_desc)
  |> should.equal(True)
}

pub fn beads_description_valid_length_test() {
  is_valid_description(text.cmd_beads_desc)
  |> should.equal(True)
}

pub fn bead_status_command_description_exists_test() {
  has_positive_length(text.cmd_bead_status_desc)
  |> should.equal(True)
}

pub fn bead_status_description_valid_length_test() {
  is_valid_description(text.cmd_bead_status_desc)
  |> should.equal(True)
}

pub fn history_command_description_exists_test() {
  has_positive_length(text.cmd_history_desc)
  |> should.equal(True)
}

pub fn history_description_valid_length_test() {
  is_valid_description(text.cmd_history_desc)
  |> should.equal(True)
}

pub fn diff_command_description_exists_test() {
  has_positive_length(text.cmd_diff_desc)
  |> should.equal(True)
}

pub fn diff_description_valid_length_test() {
  is_valid_description(text.cmd_diff_desc)
  |> should.equal(True)
}

pub fn sessions_command_description_exists_test() {
  has_positive_length(text.cmd_sessions_desc)
  |> should.equal(True)
}

pub fn sessions_description_valid_length_test() {
  is_valid_description(text.cmd_sessions_desc)
  |> should.equal(True)
}

pub fn quality_command_description_exists_test() {
  has_positive_length(text.cmd_quality_desc)
  |> should.equal(True)
}

pub fn quality_description_valid_length_test() {
  is_valid_description(text.cmd_quality_desc)
  |> should.equal(True)
}

pub fn quality_description_includes_kirk_prefix_test() {
  string.contains(text.cmd_quality_desc, "KIRK:")
  |> should.equal(True)
}

pub fn invert_command_description_exists_test() {
  has_positive_length(text.cmd_invert_desc)
  |> should.equal(True)
}

pub fn invert_description_valid_length_test() {
  is_valid_description(text.cmd_invert_desc)
  |> should.equal(True)
}

pub fn invert_description_includes_kirk_prefix_test() {
  string.contains(text.cmd_invert_desc, "KIRK:")
  |> should.equal(True)
}

pub fn coverage_command_description_exists_test() {
  has_positive_length(text.cmd_coverage_desc)
  |> should.equal(True)
}

pub fn coverage_description_valid_length_test() {
  is_valid_description(text.cmd_coverage_desc)
  |> should.equal(True)
}

pub fn coverage_description_includes_kirk_prefix_test() {
  string.contains(text.cmd_coverage_desc, "KIRK:")
  |> should.equal(True)
}

pub fn gaps_command_description_exists_test() {
  has_positive_length(text.cmd_gaps_desc)
  |> should.equal(True)
}

pub fn gaps_description_valid_length_test() {
  is_valid_description(text.cmd_gaps_desc)
  |> should.equal(True)
}

pub fn gaps_description_includes_kirk_prefix_test() {
  string.contains(text.cmd_gaps_desc, "KIRK:")
  |> should.equal(True)
}

pub fn effects_command_description_exists_test() {
  has_positive_length(text.cmd_effects_desc)
  |> should.equal(True)
}

pub fn effects_description_valid_length_test() {
  is_valid_description(text.cmd_effects_desc)
  |> should.equal(True)
}

pub fn effects_description_includes_kirk_prefix_test() {
  string.contains(text.cmd_effects_desc, "KIRK:")
  |> should.equal(True)
}

pub fn ears_command_description_exists_test() {
  has_positive_length(text.cmd_ears_desc)
  |> should.equal(True)
}

pub fn ears_description_valid_length_test() {
  is_valid_description(text.cmd_ears_desc)
  |> should.equal(True)
}

pub fn ears_description_includes_kirk_prefix_test() {
  string.contains(text.cmd_ears_desc, "KIRK:")
  |> should.equal(True)
}

pub fn parse_command_description_exists_test() {
  has_positive_length(text.cmd_parse_desc)
  |> should.equal(True)
}

pub fn parse_description_valid_length_test() {
  is_valid_description(text.cmd_parse_desc)
  |> should.equal(True)
}

pub fn plan_command_description_exists_test() {
  has_positive_length(text.cmd_plan_desc)
  |> should.equal(True)
}

pub fn plan_description_valid_length_test() {
  is_valid_description(text.cmd_plan_desc)
  |> should.equal(True)
}

pub fn plan_approve_command_description_exists_test() {
  has_positive_length(text.cmd_plan_approve_desc)
  |> should.equal(True)
}

pub fn plan_approve_description_valid_length_test() {
  is_valid_description(text.cmd_plan_approve_desc)
  |> should.equal(True)
}

pub fn beads_regenerate_command_description_exists_test() {
  has_positive_length(text.cmd_beads_regenerate_desc)
  |> should.equal(True)
}

pub fn beads_regenerate_description_valid_length_test() {
  is_valid_description(text.cmd_beads_regenerate_desc)
  |> should.equal(True)
}

// =============================================================================
// EXTENDED HELP TEXT - All 24 Commands (Existence & Key Sections)
// =============================================================================

pub fn check_extended_help_exists_test() {
  has_substantial_length(text.check_extended_help)
  |> should.equal(True)
}

pub fn check_extended_help_has_key_sections_test() {
  has_what_why_when(text.check_extended_help)
  |> should.equal(True)
}

pub fn validate_extended_help_exists_test() {
  has_substantial_length(text.validate_extended_help)
  |> should.equal(True)
}

pub fn validate_extended_help_has_key_sections_test() {
  has_what_why_when(text.validate_extended_help)
  |> should.equal(True)
}

pub fn show_extended_help_exists_test() {
  has_substantial_length(text.show_extended_help)
  |> should.equal(True)
}

pub fn show_extended_help_has_key_sections_test() {
  has_what_why_when(text.show_extended_help)
  |> should.equal(True)
}

pub fn export_extended_help_exists_test() {
  has_substantial_length(text.export_extended_help)
  |> should.equal(True)
}

pub fn export_extended_help_has_key_sections_test() {
  has_what_why_when(text.export_extended_help)
  |> should.equal(True)
}

pub fn lint_extended_help_exists_test() {
  has_substantial_length(text.lint_extended_help)
  |> should.equal(True)
}

pub fn lint_extended_help_has_key_sections_test() {
  has_what_why_when(text.lint_extended_help)
  |> should.equal(True)
}

pub fn analyze_extended_help_exists_test() {
  has_substantial_length(text.analyze_extended_help)
  |> should.equal(True)
}

pub fn analyze_extended_help_has_key_sections_test() {
  has_what_why_when(text.analyze_extended_help)
  |> should.equal(True)
}

pub fn improve_extended_help_exists_test() {
  has_substantial_length(text.improve_extended_help)
  |> should.equal(True)
}

pub fn improve_extended_help_has_key_sections_test() {
  has_what_why_when(text.improve_extended_help)
  |> should.equal(True)
}

pub fn doctor_extended_help_exists_test() {
  has_substantial_length(text.doctor_extended_help)
  |> should.equal(True)
}

pub fn doctor_extended_help_has_key_sections_test() {
  has_what_why_when(text.doctor_extended_help)
  |> should.equal(True)
}

pub fn plan_extended_help_exists_test() {
  has_substantial_length(text.plan_extended_help)
  |> should.equal(True)
}

pub fn plan_extended_help_has_key_sections_test() {
  has_what_why_when(text.plan_extended_help)
  |> should.equal(True)
}

pub fn plan_approve_extended_help_exists_test() {
  has_substantial_length(text.plan_approve_extended_help)
  |> should.equal(True)
}

pub fn plan_approve_extended_help_has_key_sections_test() {
  has_what_why_when(text.plan_approve_extended_help)
  |> should.equal(True)
}

pub fn beads_regenerate_extended_help_exists_test() {
  has_substantial_length(text.beads_regenerate_extended_help)
  |> should.equal(True)
}

pub fn beads_regenerate_extended_help_has_key_sections_test() {
  has_what_why_when(text.beads_regenerate_extended_help)
  |> should.equal(True)
}

pub fn interview_extended_help_exists_test() {
  has_substantial_length(text.interview_extended_help)
  |> should.equal(True)
}

pub fn interview_extended_help_has_key_sections_test() {
  has_what_why_when(text.interview_extended_help)
  |> should.equal(True)
}

pub fn beads_extended_help_exists_test() {
  has_substantial_length(text.beads_extended_help)
  |> should.equal(True)
}

pub fn beads_extended_help_has_key_sections_test() {
  has_what_why_when(text.beads_extended_help)
  |> should.equal(True)
}

pub fn bead_status_extended_help_exists_test() {
  has_substantial_length(text.bead_status_extended_help)
  |> should.equal(True)
}

pub fn bead_status_extended_help_has_key_sections_test() {
  has_what_why_when(text.bead_status_extended_help)
  |> should.equal(True)
}

pub fn history_extended_help_exists_test() {
  has_substantial_length(text.history_extended_help)
  |> should.equal(True)
}

pub fn history_extended_help_has_key_sections_test() {
  has_what_why_when(text.history_extended_help)
  |> should.equal(True)
}

pub fn diff_extended_help_exists_test() {
  has_substantial_length(text.diff_extended_help)
  |> should.equal(True)
}

pub fn diff_extended_help_has_key_sections_test() {
  has_what_why_when(text.diff_extended_help)
  |> should.equal(True)
}

pub fn sessions_extended_help_exists_test() {
  has_substantial_length(text.sessions_extended_help)
  |> should.equal(True)
}

pub fn sessions_extended_help_has_key_sections_test() {
  has_what_why_when(text.sessions_extended_help)
  |> should.equal(True)
}

pub fn kirk_quality_extended_help_exists_test() {
  has_substantial_length(text.kirk_quality_extended_help)
  |> should.equal(True)
}

pub fn kirk_quality_extended_help_has_key_sections_test() {
  has_what_why_when(text.kirk_quality_extended_help)
  |> should.equal(True)
}

pub fn kirk_invert_extended_help_exists_test() {
  has_substantial_length(text.kirk_invert_extended_help)
  |> should.equal(True)
}

pub fn kirk_invert_extended_help_has_key_sections_test() {
  has_what_why_when(text.kirk_invert_extended_help)
  |> should.equal(True)
}

pub fn kirk_coverage_extended_help_exists_test() {
  has_substantial_length(text.kirk_coverage_extended_help)
  |> should.equal(True)
}

pub fn kirk_coverage_extended_help_has_key_sections_test() {
  has_what_why_when(text.kirk_coverage_extended_help)
  |> should.equal(True)
}

pub fn kirk_gaps_extended_help_exists_test() {
  has_substantial_length(text.kirk_gaps_extended_help)
  |> should.equal(True)
}

pub fn kirk_gaps_extended_help_has_key_sections_test() {
  has_what_why_when(text.kirk_gaps_extended_help)
  |> should.equal(True)
}

pub fn kirk_effects_extended_help_exists_test() {
  has_substantial_length(text.kirk_effects_extended_help)
  |> should.equal(True)
}

pub fn kirk_effects_extended_help_has_key_sections_test() {
  has_what_why_when(text.kirk_effects_extended_help)
  |> should.equal(True)
}

pub fn kirk_ears_extended_help_exists_test() {
  has_substantial_length(text.kirk_ears_extended_help)
  |> should.equal(True)
}

pub fn kirk_ears_extended_help_has_key_sections_test() {
  has_what_why_when(text.kirk_ears_extended_help)
  |> should.equal(True)
}

pub fn parse_extended_help_exists_test() {
  has_substantial_length(text.parse_extended_help)
  |> should.equal(True)
}

pub fn parse_extended_help_has_key_sections_test() {
  has_what_why_when(text.parse_extended_help)
  |> should.equal(True)
}

// =============================================================================
// UNIQUENESS TEST - No Duplicated Help Text
// =============================================================================

pub fn all_command_descriptions_unique_test() {
  let descriptions = [
    text.cmd_check_desc,
    text.cmd_validate_desc,
    text.cmd_show_desc,
    text.cmd_export_desc,
    text.cmd_lint_desc,
    text.cmd_analyze_desc,
    text.cmd_improve_desc,
    text.cmd_doctor_desc,
    text.cmd_interview_desc,
    text.cmd_beads_desc,
    text.cmd_bead_status_desc,
    text.cmd_history_desc,
    text.cmd_diff_desc,
    text.cmd_sessions_desc,
    text.cmd_quality_desc,
    text.cmd_invert_desc,
    text.cmd_coverage_desc,
    text.cmd_gaps_desc,
    text.cmd_effects_desc,
    text.cmd_ears_desc,
    text.cmd_parse_desc,
    text.cmd_plan_desc,
    text.cmd_plan_approve_desc,
    text.cmd_beads_regenerate_desc,
  ]

  let unique_count = list.length(list.unique(descriptions))
  let total_count = list.length(descriptions)

  unique_count |> should.equal(total_count)
}

// =============================================================================
// FLAG DESCRIPTION TESTS
// =============================================================================

pub fn flag_json_description_exists_test() {
  has_positive_length(text.flag_json_desc)
  |> should.equal(True)
}

pub fn flag_target_description_exists_test() {
  has_positive_length(text.flag_target_desc)
  |> should.equal(True)
}

pub fn flag_verbose_description_exists_test() {
  has_positive_length(text.flag_verbose_desc)
  |> should.equal(True)
}

pub fn flag_quiet_description_exists_test() {
  has_positive_length(text.flag_quiet_desc)
  |> should.equal(True)
}

pub fn flag_profile_description_exists_test() {
  has_positive_length(text.flag_profile_desc)
  |> should.equal(True)
}

pub fn flag_output_description_exists_test() {
  has_positive_length(text.flag_output_desc)
  |> should.equal(True)
}

// =============================================================================
// HELPER FUNCTION TESTS
// =============================================================================

pub fn with_default_helper_works_test() {
  let result = text.with_default("Some flag", "default_value")
  let valid = string.contains(result, "Some flag") && string.contains(result, "default: default_value")
  valid |> should.equal(True)
}

pub fn required_helper_works_test() {
  let result = text.required("Some flag")
  let valid = string.contains(result, "Some flag") && string.contains(result, "(required)")
  valid |> should.equal(True)
}

pub fn with_env_helper_works_test() {
  let result = text.with_env("Some flag", "INTENT_VAR")
  let valid = string.contains(result, "Some flag") && string.contains(result, "[env: INTENT_VAR]")
  valid |> should.equal(True)
}

pub fn with_default_and_env_helper_works_test() {
  let result = text.with_default_and_env("Some flag", "default_val", "INTENT_VAR")
  let valid =
    string.contains(result, "Some flag")
    && string.contains(result, "default: default_val")
    && string.contains(result, "[env: INTENT_VAR]")
  valid |> should.equal(True)
}

// =============================================================================
// SUMMARY / COMPLETENESS TESTS
// =============================================================================

pub fn total_command_count_test() {
  let descriptions = [
    text.cmd_check_desc,
    text.cmd_validate_desc,
    text.cmd_show_desc,
    text.cmd_export_desc,
    text.cmd_lint_desc,
    text.cmd_analyze_desc,
    text.cmd_improve_desc,
    text.cmd_doctor_desc,
    text.cmd_interview_desc,
    text.cmd_beads_desc,
    text.cmd_bead_status_desc,
    text.cmd_history_desc,
    text.cmd_diff_desc,
    text.cmd_sessions_desc,
    text.cmd_quality_desc,
    text.cmd_invert_desc,
    text.cmd_coverage_desc,
    text.cmd_gaps_desc,
    text.cmd_effects_desc,
    text.cmd_ears_desc,
    text.cmd_parse_desc,
    text.cmd_plan_desc,
    text.cmd_plan_approve_desc,
    text.cmd_beads_regenerate_desc,
  ]

  list.length(descriptions) |> should.equal(24)
}

pub fn total_extended_help_count_test() {
  let helps = [
    text.check_extended_help,
    text.validate_extended_help,
    text.show_extended_help,
    text.export_extended_help,
    text.lint_extended_help,
    text.analyze_extended_help,
    text.improve_extended_help,
    text.doctor_extended_help,
    text.interview_extended_help,
    text.beads_extended_help,
    text.bead_status_extended_help,
    text.history_extended_help,
    text.diff_extended_help,
    text.sessions_extended_help,
    text.kirk_quality_extended_help,
    text.kirk_invert_extended_help,
    text.kirk_coverage_extended_help,
    text.kirk_gaps_extended_help,
    text.kirk_effects_extended_help,
    text.kirk_ears_extended_help,
    text.parse_extended_help,
    text.plan_extended_help,
    text.plan_approve_extended_help,
    text.beads_regenerate_extended_help,
  ]

  list.length(helps) |> should.equal(24)
}

pub fn all_extended_help_nonempty_test() {
  let helps = [
    text.check_extended_help,
    text.validate_extended_help,
    text.show_extended_help,
    text.export_extended_help,
    text.lint_extended_help,
    text.analyze_extended_help,
    text.improve_extended_help,
    text.doctor_extended_help,
    text.interview_extended_help,
    text.beads_extended_help,
    text.bead_status_extended_help,
    text.history_extended_help,
    text.diff_extended_help,
    text.sessions_extended_help,
    text.kirk_quality_extended_help,
    text.kirk_invert_extended_help,
    text.kirk_coverage_extended_help,
    text.kirk_gaps_extended_help,
    text.kirk_effects_extended_help,
    text.kirk_ears_extended_help,
    text.parse_extended_help,
    text.plan_extended_help,
    text.plan_approve_extended_help,
    text.beads_regenerate_extended_help,
  ]

  let all_valid = list.all(helps, fn(h) { has_substantial_length(h) })
  all_valid |> should.equal(True)
}
