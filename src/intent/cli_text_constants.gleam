/// CLI Help Text Constants
///
/// Centralized help text for all Intent CLI commands following the Help Text Standard:
/// - Command descriptions: 50-100 chars, start with action verb
/// - KIRK commands: prefixed with "KIRK:"
/// - Required flags: marked with (required)
/// - Optional flags: show (default: X)
/// - Environment variables: marked with [env: VAR]
///
/// This module provides consistent help text across all 24 Intent CLI commands.

// =============================================================================
// COMMAND DESCRIPTIONS (24 commands across 5 categories)
// =============================================================================

// --- Core Testing Commands ---

/// Execute spec tests against target URL and verify behaviors
pub const cmd_check_desc = "Execute spec tests against target URL and verify behaviors"

/// Validate CUE spec file syntax and structure
pub const cmd_validate_desc = "Validate CUE spec file syntax and structure"

/// Display parsed spec with formatted output
pub const cmd_show_desc = "Display parsed spec with formatted output"

/// Export spec to JSON format for external tools
pub const cmd_export_desc = "Export spec to JSON format for external tools"

// --- Quality Analysis Commands ---

/// Detect anti-patterns and quality issues in spec
pub const cmd_lint_desc = "Detect anti-patterns and quality issues in spec"

/// Analyze spec quality across multiple dimensions
pub const cmd_analyze_desc = "Analyze spec quality across multiple dimensions"

/// Generate improvement suggestions from quality analysis
pub const cmd_improve_desc = "Generate improvement suggestions from quality analysis"

/// Generate health report with prioritized improvements
pub const cmd_doctor_desc = "Generate health report with prioritized improvements"

// --- Interview & Workflow Commands ---

/// Start guided specification discovery interview
pub const cmd_interview_desc = "Start guided specification discovery interview"

/// Generate work items (beads) from interview session
pub const cmd_beads_desc = "Generate work items (beads) from interview session"

/// Mark bead execution status (success/failed/blocked)
pub const cmd_bead_status_desc = "Mark bead execution status (success/failed/blocked)"

/// View snapshot history for interview session
pub const cmd_history_desc = "View snapshot history for interview session"

/// Compare two interview sessions and show differences
pub const cmd_diff_desc = "Compare two interview sessions and show differences"

/// List all interview sessions with metadata
pub const cmd_sessions_desc = "List all interview sessions with metadata"

// --- KIRK Analysis Commands ---

/// KIRK: Analyze spec quality across coverage, clarity, testability
pub const cmd_quality_desc = "KIRK: Analyze spec quality across coverage, clarity, testability"

/// KIRK: Identify missing failure cases through inversion analysis
pub const cmd_invert_desc = "KIRK: Identify missing failure cases through inversion analysis"

/// KIRK: Analyze coverage including OWASP Top 10 and edge cases
pub const cmd_coverage_desc = "KIRK: Analyze coverage including OWASP Top 10 and edge cases"

/// KIRK: Detect specification gaps using mental models
pub const cmd_gaps_desc = "KIRK: Detect specification gaps using mental models"

/// KIRK: Trace second-order effects and consequence chains
pub const cmd_effects_desc = "KIRK: Trace second-order effects and consequence chains"

/// KIRK: Parse EARS requirements into Intent behaviors
pub const cmd_ears_desc = "KIRK: Parse EARS requirements into Intent behaviors"

/// Parse EARS requirements to structured spec
pub const cmd_parse_desc = "Parse EARS requirements to structured spec"

// --- Planning Commands ---

/// Display execution plan with waves and dependencies
pub const cmd_plan_desc = "Display execution plan with waves and dependencies"

/// Approve execution plan for session (CI/automation ready)
pub const cmd_plan_approve_desc = "Approve execution plan for session (CI/automation ready)"

/// Regenerate failed/blocked beads with adjusted approach
pub const cmd_beads_regenerate_desc = "Regenerate failed/blocked beads with adjusted approach"

// =============================================================================
// FLAG DESCRIPTIONS
// =============================================================================

// --- Common Flags ---

/// Output results as JSON
pub const flag_json_desc = "Output results as JSON"

/// Write output to file instead of stdout
pub const flag_output_desc = "Write output to file instead of stdout"

/// Suppress non-error output
pub const flag_quiet_desc = "Suppress non-error output"

/// Enable verbose diagnostic output
pub const flag_verbose_desc = "Enable verbose diagnostic output"

// --- Check Command Flags ---

/// Target base URL to test against
pub const flag_target_desc = "Target base URL to test against"

/// Filter to specific feature by name
pub const flag_feature_desc = "Filter to specific feature by name"

/// Run only specific behavior by name
pub const flag_only_desc = "Run only specific behavior by name"

/// Allow localhost URLs, bypassing SSRF protection
pub const flag_allow_localhost_desc = "Allow localhost URLs, bypassing SSRF protection"

// --- Interview Command Flags ---

/// System profile type: api, cli, event, data, workflow, ui
pub const flag_profile_desc = "System profile type: api, cli, event, data, workflow, ui"

/// Resume existing interview session by ID
pub const flag_resume_desc = "Resume existing interview session by ID"

/// Path to CUE file with pre-filled answers (non-interactive mode)
pub const flag_answers_desc = "Path to CUE file with pre-filled answers (non-interactive mode)"

/// Fail if answers file missing required fields
pub const flag_strict_desc = "Fail if answers file missing required fields"

/// Export completed session to CUE spec file
pub const flag_export_desc = "Export completed session to CUE spec file"

/// Session ID for operations
pub const flag_session_desc = "Session ID for operations"

/// Provide answer text for CUE mode (requires --session)
pub const flag_answer_desc = "Provide answer text for CUE mode (requires --session)"

/// Show CUE directives without executing
pub const flag_dry_run_desc = "Show CUE directives without executing"

/// Output CUE directives for AI agents
pub const flag_cue_desc = "Output CUE directives for AI agents"

// --- EARS/Parse Command Flags ---

/// Output format: text, cue, json
pub const flag_output_format_desc = "Output format: text, cue, json"

/// Output file path
pub const flag_out_desc = "Output file path"

/// Spec name for CUE output
pub const flag_name_desc = "Spec name for CUE output"

// --- Plan Command Flags ---

/// Output format: human, json
pub const flag_format_desc = "Output format: human, json"

/// Auto-approve for CI pipelines (non-interactive)
pub const flag_yes_desc = "Auto-approve for CI pipelines (non-interactive)"

/// Optional approval notes for plan approval
pub const flag_notes_desc = "Optional approval notes for plan approval"

// --- Bead Status Command Flags ---

/// Bead ID to update status
pub const flag_bead_id_desc = "Bead ID to update status"

/// Execution status: success, failed, blocked
pub const flag_status_desc = "Execution status: success, failed, blocked"

/// Reason for status (required for failed/blocked)
pub const flag_reason_desc = "Reason for status (required for failed/blocked)"

// =============================================================================
// FLAG DESCRIPTION HELPERS
// =============================================================================

/// Add (default: X) suffix to flag description
pub fn with_default(desc: String, default: String) -> String {
  desc <> " (default: " <> default <> ")"
}

/// Add (required) marker to flag description
pub fn required(desc: String) -> String {
  desc <> " (required)"
}

/// Add [env: VAR] marker to flag description
pub fn with_env(desc: String, env_var: String) -> String {
  desc <> " [env: " <> env_var <> "]"
}

/// Combine default and env markers
pub fn with_default_and_env(
  desc: String,
  default: String,
  env_var: String,
) -> String {
  desc <> " (default: " <> default <> ") [env: " <> env_var <> "]"
}

// =============================================================================
// COMMAND-SPECIFIC ERROR MESSAGES
// =============================================================================

pub const check_missing_spec_error = "Error: spec file path required
Usage: intent check <spec.cue> --target <url>

Examples:
  intent check api.cue --target https://api.example.com
  intent check api.cue -t http://localhost:8080 --allow-localhost
  intent check api.cue -t https://staging.api.com --feature Authentication"

pub const validate_missing_spec_error = "Error: spec file path required
Usage: intent validate <spec.cue>

Examples:
  intent validate api.cue
  intent validate specs/user-api.cue"

pub const lint_missing_spec_error = "Error: spec file path required
Usage: intent lint <spec.cue>

Examples:
  intent lint api.cue
  intent lint specs/user-api.cue"
