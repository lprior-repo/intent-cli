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

// =============================================================================
// EXTENDED HELP TEXT FOR TESTING COMMANDS
// =============================================================================

/// Extended help text for the `check` command
pub const check_extended_help = "Execute spec tests against target URL and verify behaviors

WHAT IT DOES
  Execute all behaviors defined in a spec against a target HTTP API, verifying
  that responses match expected status codes, headers, and validation rules.

WHY YOU'D USE IT
  During development and testing to confirm your API implementation matches the
  contract-driven specification. Catch behavioral regressions early and ensure
  consistency across features.

WHEN TO USE IT
  After implementing features, before committing to version control, or as part
  of CI/CD pipeline validation. Use with --target to point to dev/staging/prod
  APIs for environment-specific testing.

PREREQUISITES
  - A valid Intent CUE spec file (validate with: intent validate spec.cue)
  - Target API running and accessible at the specified --target URL
  - Network access from the current machine to the target

USAGE EXAMPLES

  Simple execution against localhost:
    intent check api.cue --target http://localhost:8080 --allow-localhost

  Test production API with JSON output for tooling:
    intent check api.cue --target https://api.example.com --json

  Run only authentication-related behaviors:
    intent check api.cue --target http://localhost:3000 --feature Authentication --allow-localhost

  Test a single behavior in verbose mode:
    intent check api.cue --target https://api.example.com --only CreateUser --verbose

  Suppress output noise in CI pipelines:
    intent check api.cue --target https://staging.example.com --quiet

FLAG DETAILS
  --target URL (required)
    Base URL of the API to test (e.g., https://api.example.com or http://localhost:8080)
    Can also be set via INTENT_TARGET environment variable
    Must include protocol (http:// or https://)

  --json
    Output structured JSON instead of human-readable text
    Suitable for integration with other tools and CI systems
    Each behavior execution includes status, duration, and failure details

  --feature FEATURE_NAME
    Filter execution to only test a specific feature by exact name match
    Useful when focusing on a single area during development
    Behavior count and execution time scales accordingly

  --only BEHAVIOR_NAME
    Run a single behavior by exact name match
    Fastest feedback loop when troubleshooting specific endpoints
    Pairs well with --verbose for detailed request/response analysis

  --verbose
    Show HTTP request/response details, including headers and bodies
    Helps debug mismatches between spec expectations and actual responses
    Cannot be combined with --quiet

  --quiet
    Suppress all non-error output, show only final exit code
    Ideal for scripts and CI where you only care about pass/fail status
    Cannot be combined with --verbose

  --allow-localhost
    Bypass SSRF protection to test against localhost/127.0.0.1
    Required for testing local development servers
    Security: Only use with development APIs, never in production

EXIT CODES
  0 = All behaviors passed
  1 = One or more behaviors failed
  2 = Behaviors blocked (missing prerequisites or dependencies)
  3 = Invalid spec or configuration
  4 = Runtime error (network, timeout, etc.)

SEE ALSO
  intent validate  - Check spec file syntax before running
  intent show     - Preview spec contents without execution
  intent lint     - Detect specification quality issues
  intent plan     - View execution plan with wave structure"

/// Extended help text for the `validate` command
pub const validate_extended_help = "Validate CUE spec file syntax and structure

WHAT IT DOES
  Parses CUE syntax and validates that the spec file conforms to Intent's
  required structure: name, description, version, audience, success_criteria,
  config, features, rules, anti_patterns, and ai_hints fields.

WHY YOU'D USE IT
  Catch specification errors early before running expensive test execution.
  Ensures your spec is parseable and structurally sound, preventing downstream
  errors when running check, plan, or other commands.

WHEN TO USE IT
  After editing a CUE spec file, before committing changes, or in pre-commit
  hooks. Always validate before running check/plan commands to get clear error
  messages about what's wrong.

PREREQUISITES
  - A CUE spec file in the working directory or specified path
  - Valid CUE syntax with proper structure (see examples below)

USAGE EXAMPLES

  Validate a single spec file:
    intent validate api.cue

  Validate from a subdirectory:
    intent validate specs/user-api.cue

  Validate before running checks:
    intent validate api.cue && intent check api.cue --target http://localhost:8080 --allow-localhost

  Quick validation in a script:
    intent validate api.cue || echo \"Fix the spec and try again\"

  Batch validate multiple specs:
    for spec in specs/*.cue; do intent validate \"$spec\" || exit 1; done

FLAG DETAILS
  None - validate takes only the spec file path as argument

SPEC STRUCTURE REQUIREMENTS

  Required top-level fields:
    name: String
      Identifier for the spec (e.g., \"UserAPI\")

    description: String
      Human-readable overview of what the spec covers

    version: String
      Version identifier (e.g., \"v1.0\", \"2024-01-15\")

    audience: String
      Target users/systems for this spec (e.g., \"Frontend developers\", \"Mobile clients\")

    success_criteria: List(String)
      Measurable outcomes the spec is designed to achieve

    config: {...}
      Execution configuration with base_url, timeout_ms, headers

    features: List(Feature)
      Behaviors grouped by logical feature (non-empty)

    rules: List(Rule)
      Global validation rules applied across all behaviors

    anti_patterns: List(AntiPattern)
      Known bad practices to avoid in implementation

    ai_hints: {...}
      AI-focused metadata: implementation, entities, security, pitfalls

  Each Feature requires:
    name: String
    description: String
    behaviors: List(Behavior)  # Must have at least one behavior

  Each Behavior requires:
    name: String
    intent: String
    request: {...method, path, headers, query, body}
    response: {...status, example, checks, headers}

EXIT CODES
  0 = Spec is valid and well-formed
  3 = CUE syntax error or structure mismatch
  4 = File not found or read error

ERROR EXAMPLES

  Missing required field:
    ✗ Error: missing required field \"features\" in spec
    Fix: Add a features: [{...}] section

  Invalid CUE syntax:
    ✗ Error: CUE parse error on line 42: unexpected token \"{\"
    Fix: Review syntax around line 42, check brackets and quotes

  Empty behaviors list:
    ✗ Error: Feature \"Authentication\" has no behaviors
    Fix: Add at least one behavior to each feature

SEE ALSO
  intent show     - Display parsed spec contents
  intent check    - Run tests against a target API
  intent lint     - Check for specification quality issues
  intent improve  - Get suggestions to enhance your spec"

/// Extended help text for the `show` command
pub const show_extended_help = "Display parsed spec with formatted output

WHAT IT DOES
  Parses and displays the contents of an Intent spec file in human-readable
  format (or JSON with --json). Shows metadata, features, behaviors, rules,
  anti-patterns, and AI hints without executing any tests.

WHY YOU'D USE IT
  Preview spec contents before running tests, verify structure changes,
  share spec overview with team members, or extract structured data for
  reporting/documentation purposes.

WHEN TO USE IT
  When reviewing spec files, troubleshooting test failures, communicating
  spec coverage to stakeholders, or preparing specs for external tools.
  JSON mode is useful for programmatic analysis and tooling integration.

PREREQUISITES
  - A valid Intent CUE spec file (previously validated with: intent validate spec.cue)
  - File must be readable from the current working directory or specified path

USAGE EXAMPLES

  Display human-readable spec summary:
    intent show api.cue

  Show spec from subdirectory:
    intent show specs/user-api.cue

  Export spec as JSON for analysis tools:
    intent show api.cue --json

  Pretty-print JSON output:
    intent show api.cue --json | jq .

  Extract just the features section:
    intent show api.cue --json | jq '.features'

  Count total behaviors in a spec:
    intent show api.cue --json | jq '[.features[].behaviors] | length'

  Compare two spec versions:
    diff <(intent show api-v1.cue --json) <(intent show api-v2.cue --json)

FLAG DETAILS
  --json
    Output parsed spec as formatted JSON instead of human-readable text
    Suitable for piping to jq, grep, or other text processing tools
    Full spec structure including all metadata and validation rules

OUTPUT SECTIONS (Human-Readable Mode)

  Spec metadata:
    - Name: Identifier for the specification
    - Version: Version number or date
    - Description: Human-readable overview
    - Audience: Target users or systems

  Success Criteria:
    List of measurable outcomes the spec should achieve

  Features:
    Logical groupings of related behaviors
    Each feature shows:
      - Name and description
      - Count of behaviors
      - List of behavior names and intents

  Global Rules:
    Validation rules that apply across all behaviors

  Anti-Patterns:
    Known bad practices to avoid during implementation

JSON OUTPUT STRUCTURE

  {
    \"name\": \"UserAPI\",
    \"version\": \"v1.0\",
    \"description\": \"...\",
    \"audience\": \"...\",
    \"success_criteria\": [...],
    \"config\": {...},
    \"features\": [
      {
        \"name\": \"Authentication\",
        \"description\": \"...\",
        \"behaviors\": [
          {
            \"name\": \"LoginUser\",
            \"intent\": \"...\",
            \"request\": {...},
            \"response\": {...},
            ...
          }
        ]
      }
    ],
    \"rules\": [...],
    \"anti_patterns\": [...],
    \"ai_hints\": {...}
  }

EXIT CODES
  0 = Successfully displayed spec
  3 = CUE syntax error or spec structure invalid
  4 = File not found or read error

SEE ALSO
  intent validate - Check spec syntax before showing
  intent export   - Export spec to JSON (same as show --json)
  intent check    - Run tests against a target API
  intent lint     - Check for specification quality issues"

/// Extended help text for the `export` command
pub const export_extended_help = "Export spec to JSON format for external tools

WHAT IT DOES
  Parses an Intent spec and exports it as minified JSON to stdout. Suitable
  for programmatic processing, external tooling, CI/CD pipelines, and spec
  distribution across teams.

WHY YOU'D USE IT
  Embed Intent specs in larger systems, share specs via APIs, generate
  documentation from structured data, or perform automated analysis without
  needing the Intent CLI present on receiving systems.

WHEN TO USE IT
  Before publishing specs to a registry, for spec version control in git,
  when integrating with test frameworks, or when sharing specs across
  teams/organizations. Export is deterministic and suitable for inclusion
  in documentation, APIs, and generated files.

PREREQUISITES
  - A valid Intent CUE spec file (previously validated with: intent validate spec.cue)
  - File must be readable from the current working directory or specified path

USAGE EXAMPLES

  Export spec as compact JSON:
    intent export api.cue

  Save exported spec to file:
    intent export api.cue > api.json

  Pretty-print exported JSON:
    intent export api.cue | jq .

  Validate exported JSON:
    intent export api.cue | jq empty && echo \"Valid JSON\"

  Extract and analyze features:
    intent export api.cue | jq '.features | length'

  Compare exported specs:
    diff <(intent export api-v1.cue) <(intent export api-v2.cue)

  Embed in a shell script for CI:
    SPEC_JSON=$(intent export api.cue)
    echo \"Spec version: $(echo $SPEC_JSON | jq -r .version)\"

  Upload to API registry:
    curl -X POST https://api.example.com/specs \\
      -H \"Content-Type: application/json\" \\
      -d \"$(intent export api.cue)\"

FLAG DETAILS
  None - export takes only the spec file path as argument

OUTPUT FORMAT

  Minified JSON with complete spec structure:
  {
    \"name\":\"UserAPI\",
    \"version\":\"v1.0\",
    \"description\":\"User account management API\",
    \"audience\":\"Mobile and web clients\",
    \"success_criteria\":[...],
    \"config\":{...},
    \"features\":[{...}],
    \"rules\":[...],
    \"anti_patterns\":[...],
    \"ai_hints\":{...}
  }

  To pretty-print:
    intent export api.cue | jq .

  To minify further for distribution:
    intent export api.cue | jq -c .

INTEGRATION PATTERNS

  Store in version control:
    intent export api.cue > spec.json
    git add spec.json
    git commit -m \"Update API spec\"

  CI/CD pipeline step:
    #!/bin/bash
    set -e
    intent validate api.cue
    intent export api.cue > /tmp/spec.json
    # Upload, validate, or process the spec...

  Automated documentation:
    SPEC_JSON=$(intent export api.cue)
    cat > API_SPEC.md << EOF
    # API Specification
    Version: $(echo $SPEC_JSON | jq -r .version)
    Audience: $(echo $SPEC_JSON | jq -r .audience)
    Features: $(echo $SPEC_JSON | jq '.features | length')
    EOF

  Testing framework integration:
    const spec = JSON.parse(fs.readFileSync('spec.json'));
    const behaviors = spec.features.flatMap(f => f.behaviors);
    // Use behaviors to drive test generation...

COMPARISON WITH SHOW

  show --json:
    - Parses and displays spec
    - Same JSON output as export
    - Supports human-readable mode without --json
    - Good for interactive review

  export:
    - Always outputs JSON
    - Minimal/no overhead
    - Optimized for tooling and scripting
    - Equivalent to: show --json (but more direct)

EXIT CODES
  0 = Successfully exported spec as JSON
  3 = CUE syntax error or spec structure invalid
  4 = File not found or read error

ERROR HANDLING

  Parse error in spec:
    ✗ Error: Failed to export spec
    Fix: Run 'intent validate api.cue' to identify syntax issues

  File not found:
    ✗ Error: Failed to export spec: file not found
    Fix: Check the file path and verify file exists

  Invalid structure:
    ✗ Error: Failed to export spec: missing required fields
    Fix: Ensure all required spec fields are present

SEE ALSO
  intent show     - Display spec with optional JSON output (same output)
  intent validate - Check spec syntax before exporting
  intent check    - Run tests against a target API"

// =============================================================================
// EXTENDED HELP TEXT FOR PLANNING COMMANDS
// =============================================================================

/// Extended help text for the `plan` command
pub const plan_extended_help = "Display execution plan with waves and dependencies

WHAT IT DOES
  Computes a topological execution plan from a session's beads, organizing
  them into parallel waves by dependency depth and calculating total effort,
  risk level, and blockers.

WHY YOU'D USE IT
  Understand the scope, complexity, and sequencing of work before starting
  execution. Identifies dependencies, parallel opportunities, and risk factors.

WHEN TO USE IT
  After running 'intent interview' and 'intent beads' to see the planned work.
  Use before 'intent plan-approve' to review and validate the plan. Reference
  during execution to track progress and adjust strategies.

PREREQUISITES
  - Valid session ID from: intent interview + intent beads
  - Beads already generated and stored in session

USAGE EXAMPLES

  View plan for a session:
    intent plan abc-123

  Export plan as JSON for CI/CD automation:
    intent plan abc-123 --json

  Check plan quality before approval:
    intent plan abc-123 --json | jq '.risk_level'

  Integrate with GitHub Actions:
    intent plan ${{ env.SESSION_ID }} --json | jq '.risk_level'

FLAG DETAILS
  --json
    Output execution plan as structured JSON
    Includes: waves[], dependencies[], effort, risk_level

EXIT CODES
  0 = Plan generated successfully
  1 = Session not found
  3 = Invalid session data
  4 = System error

SEE ALSO
  intent interview    - Create session
  intent beads        - Generate work items
  intent plan-approve - Approve plan for execution"

/// Extended help text for the `plan-approve` command
pub const plan_approve_extended_help = "Approve execution plan for session (CI/automation ready)

WHAT IT DOES
  Reviews session execution plan and records approval (human or automated).
  Validates plan quality, presents risk summary, stores approval timestamp
  and metadata in session file.

WHY YOU'D USE IT
  Explicit approval gates prevent accidental execution of large/risky plans.
  Creates audit trail for compliance tracking. Enable CI/CD automation with
  --yes flag for fully automated pipelines.

WHEN TO USE IT
  After reviewing plan output with 'intent plan'. Before executing beads with
  'bd ready'. In CI/CD pipelines to gate automated work execution.

PREREQUISITES
  - Session with valid plan (from: intent plan)
  - Review plan risk/scope before approval

USAGE EXAMPLES

  Interactive approval (human gatekeeper):
    intent plan-approve session-abc-123

  Automated approval in CI (non-interactive):
    intent plan-approve session-abc-123 --yes

  Approval with audit documentation:
    intent plan-approve session-abc-123 --yes --notes \"Approved by release manager\"

  GitHub Actions example:
    - name: Approve execution plan
      run: |
        intent plan-approve ${{ env.SESSION_ID }} --yes \\
          --notes \"Auto-approved by CI/${{ github.actor }}\"

  GitLab CI example:
    approve_plan:
      script:
        - intent plan-approve $SESSION_ID --yes
        - intent beads $SESSION_ID > beads.json

FLAG DETAILS
  --yes
    Approve automatically without interactive prompt
    For CI/CD automation and non-interactive pipelines

  --notes TEXT
    Document approval reason for audit trail
    Example: --notes \"Passed security review\"

EXIT CODES
  0 = Plan approved successfully
  1 = Plan rejected (user selected no)
  2 = Plan blocked (has critical blockers)
  3 = Invalid plan (failed validation)
  4 = System error (file I/O, etc.)

SEE ALSO
  intent plan             - View execution plan
  intent beads            - View work items
  intent beads-regenerate - Regenerate failed beads"

/// Extended help text for the `beads-regenerate` command
pub const beads_regenerate_extended_help = "Regenerate failed/blocked beads with adjusted approach

WHAT IT DOES
  Analyzes execution feedback, identifies failed/blocked work, and generates
  new beads with alternative strategies (inversion, effects, pre-mortem).
  Replaces broken beads with improved alternatives using mental model shifts.

WHY YOU'D USE IT
  When beads fail during execution, regeneration provides alternative approaches
  rather than repeating the same failed strategy. Uses mental model shifts
  to find new solutions.

WHEN TO USE IT
  After executing beads and gathering feedback. When some beads failed (crashed,
  timeout) or blocked (dependency issues). Before rerun to avoid repeating
  failed patterns.

PREREQUISITES
  - Completed execution with failed/blocked beads
  - Feedback data captured (from: bd ready, intent bead-status)

USAGE EXAMPLES

  Regenerate with default hybrid strategy (recommended):
    intent beads-regenerate session-abc-123

  Regenerate using inversion analysis (for logic failures):
    intent beads-regenerate session-abc-123 --strategy inversion

  Regenerate using effects analysis (for dependency failures):
    intent beads-regenerate session-abc-123 --strategy effects

  Regenerate using pre-mortem (for robustness):
    intent beads-regenerate session-abc-123 --strategy premortem

  Full CI/CD workflow with regeneration:
    $ intent beads-regenerate $SESSION_ID --strategy hybrid
    $ intent plan $SESSION_ID

FLAG DETAILS
  --strategy STRATEGY
    Regeneration strategy: hybrid (default), inversion, effects, premortem
    - hybrid: Combines all models (recommended)
    - inversion: Flips failed behavior to find root causes
    - effects: Analyzes second-order dependencies
    - premortem: Post-mortem analysis for robustness

  --session SESSION_ID (or positional)
    Session ID with failed/blocked beads

EXIT CODES
  0 = Regeneration complete
  1 = No failed beads found
  3 = Invalid session
  4 = System error

SEE ALSO
  intent plan      - View execution plan
  intent beads     - View work items
  intent invert    - Inversion analysis
  intent effects   - Second-order effects"

// =============================================================================
// EXTENDED HELP TEXT FOR QUALITY ANALYSIS COMMANDS
// =============================================================================

/// Extended help text for the `lint` command
pub const lint_extended_help = "Detect anti-patterns and quality issues in spec

WHAT IT DOES
  Scans spec for anti-patterns, missing error cases, untested paths, and
  other quality issues. Reports findings with severity levels (warning/error)
  and actionable recommendations.

WHY YOU'D USE IT
  Catch quality issues early before test execution or spec distribution.
  Improves spec clarity, testability, and maintainability. Identifies gaps
  in coverage and suggests corrections.

WHEN TO USE IT
  During spec authoring, before committing to version control, or as part
  of CI/CD validation. Run before `intent check` to fix issues proactively.

PREREQUISITES
  - A valid Intent CUE spec file (validate with: intent validate spec.cue)

USAGE EXAMPLES

  Lint a single spec file:
    intent lint api.cue

  Output issues as JSON for tooling:
    intent lint api.cue --json

  Lint all specs in a directory:
    for f in specs/*.cue; do intent lint \"$f\"; done

FLAG DETAILS
  --json
    Output structured JSON instead of human-readable text
    Each issue includes: code, severity, line, message, suggestion

EXIT CODES
  0 = No issues found
  1 = Warnings found (non-blocking)
  2 = Errors found (should fix before testing)
  3 = Invalid spec file
  4 = Runtime error

SEE ALSO
  intent validate - Check spec syntax
  intent analyze - Detailed quality scoring
  intent doctor  - Full health report with prioritized fixes"

/// Extended help text for the `analyze` command
pub const analyze_extended_help = "Analyze spec quality across multiple dimensions

WHAT IT DOES
  Scores spec across coverage (required cases), clarity (structure/docs),
  testability (executability), and AI-readiness (LLM compatibility).
  Returns 0-100 score per dimension and overall quality rating.

WHY YOU'D USE IT
  Understand spec quality in quantitative terms. Identify weakest areas for
  improvement. Track quality metrics over time as spec evolves.

WHEN TO USE IT
  After writing or modifying specs, before major releases, or when planning
  quality improvement efforts. Compare scores across spec versions.

PREREQUISITES
  - A valid Intent CUE spec file

USAGE EXAMPLES

  Analyze a spec:
    intent analyze api.cue

  Get JSON output with detailed scores:
    intent analyze api.cue --json

  Analyze and save results:
    intent analyze api.cue --json > quality-metrics.json

FLAG DETAILS
  --json
    Output structured JSON with per-dimension scores
    Includes: coverage%, clarity%, testability%, ai_readiness%, overall%

EXIT CODES
  0 = Analysis complete (any quality level)
  3 = Invalid spec file
  4 = Runtime error

SEE ALSO
  intent lint    - Detect specific issues
  intent improve - Get improvement suggestions
  intent doctor  - Full health report"

/// Extended help text for the `improve` command
pub const improve_extended_help = "Generate improvement suggestions from quality analysis

WHAT IT DOES
  Analyzes spec and suggests improvements ranked by impact. Covers coverage
  gaps, clarity issues, testability enhancements, and AI-readiness improvements.
  Prioritizes high-impact suggestions first.

WHY YOU'D USE IT
  Get concrete, ranked suggestions for making specs better. Focus effort on
  highest-impact improvements first. Understand why each suggestion matters.

WHEN TO USE IT
  After analyzing spec quality (intent analyze). Use suggestions to guide
  spec refinement and improvement efforts.

PREREQUISITES
  - A valid Intent CUE spec file
  - Recommended: Run `intent analyze` first for context

USAGE EXAMPLES

  Get improvement suggestions:
    intent improve api.cue

  Get as JSON with impact scores:
    intent improve api.cue --json

  Get top 5 suggestions:
    intent improve api.cue --json | jq '.suggestions[0:5]'

FLAG DETAILS
  --json
    Output structured JSON with ranked suggestions
    Each suggestion includes: category, description, impact (1-10), effort (1-5)

EXIT CODES
  0 = Suggestions generated
  1 = No suggestions found (spec already excellent)
  3 = Invalid spec file
  4 = Runtime error

SEE ALSO
  intent analyze - Get quality scores
  intent lint    - Detect issues
  intent doctor  - Combined health + suggestions"

/// Extended help text for the `doctor` command
pub const doctor_extended_help = "Generate health report with prioritized improvements

WHAT IT DOES
  Comprehensive spec health check combining validation, linting, analysis,
  and improvement suggestions. Returns overall health status (green/yellow/red)
  with prioritized action list.

WHY YOU'D USE IT
  One-stop command for understanding spec health. Get all issues and
  suggestions in priority order with effort/impact estimates. Ideal for
  CI/CD gates and spec reviews.

WHEN TO USE IT
  Before committing specs, before test execution, during spec review cycles,
  or as part of CI/CD validation. Run periodically to track health trends.

PREREQUISITES
  - A valid Intent CUE spec file

USAGE EXAMPLES

  Full health check:
    intent doctor api.cue

  Health check with JSON output:
    intent doctor api.cue --json

  Extract prioritized action items:
    intent doctor api.cue --json | jq '.actions | sort_by(.priority)'

FLAG DETAILS
  --json
    Output structured JSON with detailed health metrics
    Includes: status (green/yellow/red), issues[], suggestions[], actions[]

EXIT CODES
  0 = Health check complete (any status level)
  3 = Invalid spec file
  4 = Runtime error

SEE ALSO
  intent validate - Check syntax only
  intent lint     - Detect issues
  intent analyze  - Get quality scores
  intent improve  - Get suggestions"

// =============================================================================
// EXTENDED HELP TEXT FOR WORKFLOW COMMANDS
// =============================================================================

/// Extended help text for the `interview` command
pub const interview_extended_help = "Start guided specification discovery interview

WHAT IT DOES
  Interactive interview session that walks through specification discovery
  following a chosen profile (api, cli, event, data, workflow, or ui).
  Collects answers and builds a CUE specification or generates beads.

WHY YOU'D USE IT
  Structured way to capture API contract details without manually writing CUE.
  Profile-based guidance ensures you cover all relevant aspects for your use case.
  Generates beads as work items for implementation teams.

WHEN TO USE IT
  Starting a new spec project, during API design reviews, or when onboarding
  new services. Resume interrupted sessions with --resume flag.

PREREQUISITES
  - Choose a profile: api, cli, event, data, workflow, or ui
  - Optional: CUE mode (--cue) for AI-friendly structured output

USAGE EXAMPLES

  Start API interview for new backend service:
    intent interview api

  Resume an interrupted interview:
    intent interview api --resume session-abc-123

  Export spec to CUE file during interview:
    intent interview api --export myapi.cue

  Interview in CUE-compatible format (for AI agents):
    intent interview api --cue

  Event-driven system interview:
    intent interview event

  Generate beads instead of spec:
    intent interview api --beads

FLAG DETAILS
  --profile PROFILE
    Profile type: api, cli, event, data, workflow, or ui
    Guides interview questions for the specified architecture style
    Default: api

  --resume SESSION_ID
    Resume existing interview session by ID
    Preserves previous answers and continues from last question

  --export FILE.cue
    Export completed spec to CUE format file
    Can be used with `intent check`, `intent validate`, etc.

  --beads
    Generate beads (work items) from interview answers

  --cue
    Output in CUE-compatible structured format (for AI parsing)

  --json
    Export answers as JSON instead of CUE format

EXIT CODES
  0 = Interview completed successfully
  1 = User cancelled interview
  3 = Invalid profile specified
  4 = Session storage error

SEE ALSO
  intent beads             - Generate work items from spec
  intent plan              - View execution plan
  intent beads-regenerate  - Regenerate beads with new strategy"

/// Extended help text for the `beads` command
pub const beads_extended_help = "Generate work items (beads) from interview session

WHAT IT DOES
  Generates atomic work items (beads) from a completed interview session or
  spec file. Each bead represents 5-30 minutes of focused work. Organizes
  beads into waves based on dependencies and mental model phases.

WHY YOU'D USE IT
  Break down spec into concrete work items for project planning and execution.
  Understand task dependencies and execution order. Track progress through
  implementation phases.

WHEN TO USE IT
  After completing interview (intent interview) or when starting spec execution.
  Run again after feedback loop (intent feedback) to generate additional beads
  for failed/blocked items.

PREREQUISITES
  - Completed interview session ID or valid spec file
  - Session must have been saved (from: intent interview --export)

USAGE EXAMPLES

  Generate beads from interview session:
    intent beads session-abc-123

  Generate beads from saved spec:
    intent beads --spec myapi.cue

  Generate beads and export as JSON:
    intent beads session-abc-123 --json

  View beads for a specific feature:
    intent beads session-abc-123 --feature Authentication

FLAG DETAILS
  --session SESSION_ID or positional: SESSION_ID
    Interview session ID to generate beads from
    Required unless --spec is provided

  --spec FILE.cue
    Generate beads from spec file instead of session
    Alternative to --session flag

  --json
    Output beads as JSON with full metadata
    Includes: id, title, description, requires[], tags[]

  --feature FEATURE_NAME
    Generate beads only for specified feature
    Filters results by feature_id match

EXIT CODES
  0 = Beads generated successfully
  1 = Session not found
  3 = Invalid spec file
  4 = Generation error

SEE ALSO
  intent interview         - Create spec via guided interview
  intent bead-status       - Mark bead execution status
  intent beads-regenerate  - Regenerate failed/blocked beads
  intent plan              - View beads in wave structure"

/// Extended help text for the `bead-status` command
pub const bead_status_extended_help = "Mark bead execution status (success/failed/blocked)

WHAT IT DOES
  Records execution status for individual beads (work items). Status changes
  propagate to dependent beads and affect execution waves. Triggers regeneration
  when beads fail or are blocked.

WHY YOU'D USE IT
  Track progress through implementation tasks. Signal completion to downstream
  tasks. Flag failures for analysis and regeneration.

WHEN TO USE IT
  After completing each bead during implementation. Also used in CI/CD to
  automatically record test/execution results.

PREREQUISITES
  - Valid session ID
  - Bead ID exists in the session
  - Status value: success, failed, or blocked

USAGE EXAMPLES

  Mark bead as complete:
    intent bead-status --bead-id auth-service-login --status success

  Mark bead as failed:
    intent bead-status --bead-id auth-service-login --status failed

  Mark bead as blocked (dependency issue):
    intent bead-status --bead-id auth-service-login --status blocked

  From CI/CD - mark all beads in group:
    intent bead-status --bead-id \"$BEAD_ID\" --status success

FLAG DETAILS
  --bead-id BEAD_ID (required)
    Identifier of the bead to update
    Format: feature-name-description

  --status STATUS (required)
    New status: success, failed, or blocked
    - success: Bead completed successfully
    - failed: Bead execution halted (error/crash/assertion)
    - blocked: Bead blocked (dependency unsatisfied)

  --session SESSION_ID
    Session ID (optional, inferred from context if available)

EXIT CODES
  0 = Status recorded successfully
  1 = Bead not found
  2 = Status change blocked (dependency issue)
  3 = Invalid status value
  4 = Session error

SEE ALSO
  intent beads             - Generate beads
  intent history           - View status changes over time
  intent beads-regenerate  - Regenerate failed/blocked beads"

/// Extended help text for the `history` command
pub const history_extended_help = "View snapshot history for interview session

WHAT IT DOES
  Shows chronological history of all snapshots (saved states) for a session.
  Each snapshot captures beads, answers, and progress at a point in time.
  Compare snapshots with `intent diff` to see what changed.

WHY YOU'D USE IT
  Understand how spec and beads evolved. Track when decisions were made.
  Find previous versions for comparison or rollback.

WHEN TO USE IT
  Reviewing session progression, investigating change history, or finding
  an earlier snapshot state to diff against current.

PREREQUISITES
  - Valid session ID
  - Session must have multiple snapshots (created over time)

USAGE EXAMPLES

  View session history:
    intent history session-abc-123

  View history with JSON output:
    intent history session-abc-123 --json

  Get most recent 5 snapshots:
    intent history session-abc-123 --json | jq '.snapshots[-5:]'

FLAG DETAILS
  --session SESSION_ID or positional: SESSION_ID
    Session ID to show history for
    Required parameter

  --json
    Output history as JSON with all snapshots
    Each includes: timestamp, snapshot_id, beads_count, changes[]

  --limit N
    Show most recent N snapshots (default: all)

EXIT CODES
  0 = History displayed
  1 = Session not found
  4 = Read error

SEE ALSO
  intent sessions  - List all sessions
  intent diff      - Compare two snapshots
  intent beads     - View current beads"

/// Extended help text for the `diff` command
pub const diff_extended_help = "Compare two interview sessions and show differences

WHAT IT DOES
  Shows differences between two session snapshots or sessions. Highlights
  added/modified/removed beads, answer changes, and metadata differences.
  Useful for understanding spec evolution.

WHY YOU'D USE IT
  Review what changed between iterations. Understand impact of decisions.
  Validate changes before committing session to version control.

WHEN TO USE IT
  After significant changes (interviews, regeneration), or when comparing
  different spec versions for analysis.

PREREQUISITES
  - Two valid session IDs or snapshot IDs
  - Both sessions/snapshots must exist

USAGE EXAMPLES

  Compare two sessions:
    intent diff session-abc-123 session-def-456

  Compare session snapshots:
    intent diff session-abc-123#snapshot-1 session-abc-123#snapshot-2

  Get diff as JSON:
    intent diff session-abc-123 session-def-456 --json

  Show only added beads:
    intent diff session-abc-123 session-def-456 --only added

FLAG DETAILS
  Positional args: SESSION_1 SESSION_2
    Two session or snapshot IDs to compare
    Format: session-id or session-id#snapshot-id

  --json
    Output differences as structured JSON
    Includes: added[], modified[], removed[]

  --only CHANGE_TYPE
    Show only specific change type: added, modified, removed

EXIT CODES
  0 = Diff complete
  1 = Session not found
  4 = Comparison error

SEE ALSO
  intent history   - View all snapshots for a session
  intent sessions  - List all sessions
  intent beads     - View current beads"

/// Extended help text for the `sessions` command
pub const sessions_extended_help = "List all interview sessions with metadata

WHAT IT DOES
  Lists all saved interview sessions with creation date, profile, bead count,
  and last modified time. Shows status of each session (active/completed/archived).

WHY YOU'D USE IT
  Find session IDs for resuming work. Track multiple sessions in progress.
  Understand scope and timeline of spec development efforts.

WHEN TO USE IT
  When looking for a session ID to resume, or when organizing multiple
  spec development efforts.

PREREQUISITES
  - At least one saved session

USAGE EXAMPLES

  List all sessions:
    intent sessions

  List sessions with JSON output:
    intent sessions --json

  Filter sessions by profile:
    intent sessions --profile api --json

  Find active sessions:
    intent sessions --json | jq '.[] | select(.status==\"active\")'

FLAG DETAILS
  --json
    Output sessions as structured JSON
    Each includes: id, profile, created, modified, bead_count, status

  --profile PROFILE
    Filter by profile: api, cli, event, data, workflow, ui

  --status STATUS
    Filter by status: active, completed, archived

EXIT CODES
  0 = Sessions listed
  4 = Storage error

SEE ALSO
  intent interview - Start new session
  intent history   - View snapshots for a session
  intent beads     - View beads in a session"

// =============================================================================
// EXTENDED HELP TEXT FOR KIRK ANALYSIS COMMANDS
// =============================================================================

/// Extended help text for the `quality` command (KIRK)
pub const kirk_quality_extended_help = "KIRK: Analyze spec quality across coverage, clarity, testability

WHAT IT DOES
  KIRK (Komprehensive Intent Review Kit) quality analysis scores spec across
  4 dimensions: coverage (required cases), clarity (documentation/structure),
  testability (executability/assertions), and AI-readiness (LLM compatibility).
  Returns individual scores plus overall health rating.

WHY YOU'D USE IT
  Quantitative understanding of spec quality. Identify weakest dimension for
  targeted improvement. Track quality metrics across spec versions and time.

WHEN TO USE IT
  During spec creation/refinement, before releases, in code review processes,
  or when establishing quality baselines.

PREREQUISITES
  - Valid Intent CUE spec file

USAGE EXAMPLES

  Analyze spec quality:
    intent quality api.cue

  Get detailed JSON scores:
    intent quality api.cue --json

  Export quality metrics:
    intent quality api.cue --json > quality.json

  Monitor quality over time:
    for v in v1 v2 v3; do intent quality spec-$v.cue --json; done

FLAG DETAILS
  --json
    Output structured JSON with per-dimension scores
    Each dimension 0-100, includes: coverage%, clarity%, testability%, ai_readiness%

EXIT CODES
  0 = Analysis complete (any quality level)
  3 = Invalid spec file
  4 = Runtime error

SEE ALSO
  intent analyze - Quick quality overview
  intent improve - Get specific suggestions
  intent lint    - Find specific issues"

/// Extended help text for the `invert` command (KIRK)
pub const kirk_invert_extended_help = "KIRK: Identify missing failure cases through inversion analysis

WHAT IT DOES
  Inversion analysis flips normal behavior: what could go wrong, what would
  break this system, what's missing. Generates alternative/error behaviors
  and anti-patterns spec may not cover. Helps identify blind spots.

WHY YOU'D USE IT
  Discover failure modes and edge cases not explicitly specified. Improve
  robustness by understanding what could break the system. Better anticipate
  production issues.

WHEN TO USE IT
  During spec review, before security/compliance audits, when building fault-
  tolerant systems, or during pre-mortem analysis.

PREREQUISITES
  - Valid Intent CUE spec file

USAGE EXAMPLES

  Run inversion analysis:
    intent invert api.cue

  Get potential failures as JSON:
    intent invert api.cue --json

  Export failure modes:
    intent invert api.cue --json > failure-modes.json

  Generate test cases from inversions:
    intent invert api.cue --json | jq '.failures[] | .suggested_test'

FLAG DETAILS
  --json
    Output structured JSON with identified failure cases
    Each includes: pattern, description, severity, suggested_behavior

EXIT CODES
  0 = Inversion analysis complete
  3 = Invalid spec file
  4 = Runtime error

SEE ALSO
  intent coverage - OWASP coverage analysis
  intent gaps     - Gap detection
  intent effects  - Second-order effects"

/// Extended help text for the `coverage` command (KIRK)
pub const kirk_coverage_extended_help = "KIRK: Analyze coverage including OWASP Top 10 and edge cases

WHAT IT DOES
  Coverage analysis checks spec against OWASP Top 10 security categories,
  common edge cases (empty lists, null values, boundary conditions), and
  architectural patterns. Identifies coverage gaps and recommendations.

WHY YOU'D USE IT
  Ensure security best practices are covered. Reduce blind spots for common
  edge cases. Improve spec robustness and production readiness.

WHEN TO USE IT
  Security reviews, before production deployments, when building public APIs,
  or during threat modeling sessions.

PREREQUISITES
  - Valid Intent CUE spec file

USAGE EXAMPLES

  Check coverage:
    intent coverage api.cue

  Get detailed coverage report:
    intent coverage api.cue --json

  Find OWASP gaps:
    intent coverage api.cue --json | jq '.owasp_gaps[]'

  List uncovered edge cases:
    intent coverage api.cue --json | jq '.edge_cases | map(select(.covered==false))'

FLAG DETAILS
  --json
    Output structured JSON with coverage analysis
    Includes: owasp[], edge_cases[], coverage_percent

EXIT CODES
  0 = Coverage analysis complete
  3 = Invalid spec file
  4 = Runtime error

SEE ALSO
  intent invert   - Failure mode analysis
  intent gaps     - Gap detection
  intent effects  - Second-order effects"

/// Extended help text for the `gaps` command (KIRK)
pub const kirk_gaps_extended_help = "KIRK: Detect specification gaps using mental models

WHAT IT DOES
  Gap detection analyzes spec through multiple mental model lenses: inversion
  (what could break), 2nd-order effects (consequences), checklist gaps (standard
  best practices), coverage gaps (untested cases), and security gaps. Identifies
  missing requirements and behaviors.

WHY YOU'D USE IT
  Comprehensive gap analysis beyond what lint/analyze cover. Discover hidden
  requirements. Improve spec completeness using proven mental models.

WHEN TO USE IT
  During spec finalization, before execution, or in code reviews when you want
  comprehensive coverage verification.

PREREQUISITES
  - Valid Intent CUE spec file

USAGE EXAMPLES

  Find specification gaps:
    intent gaps api.cue

  Get gaps as JSON with priorities:
    intent gaps api.cue --json

  Find high-priority gaps:
    intent gaps api.cue --json | jq '.gaps | map(select(.priority==\"high\"))'

FLAG DETAILS
  --json
    Output structured JSON with detected gaps
    Each gap includes: type, description, priority, suggested_fix

EXIT CODES
  0 = Gap detection complete
  3 = Invalid spec file
  4 = Runtime error

SEE ALSO
  intent invert   - Failure mode analysis
  intent coverage - OWASP/edge case coverage
  intent effects  - Second-order effects"

/// Extended help text for the `effects` command (KIRK)
pub const kirk_effects_extended_help = "KIRK: Trace second-order effects and consequence chains

WHAT IT DOES
  Effects analysis traces consequence chains: what happens when a behavior
  executes, what other behaviors depend on it, what state changes propagate.
  Identifies orphaned behaviors and missing consequence handlers.

WHY YOU'D USE IT
  Understand system behavior chains and dependencies. Discover missing error
  handlers and recovery paths. Ensure all state changes have consequences.

WHEN TO USE IT
  When designing systems with state changes, during complex workflow specs,
  or when building event-driven architectures.

PREREQUISITES
  - Valid Intent CUE spec file

USAGE EXAMPLES

  Analyze second-order effects:
    intent effects api.cue

  Get effects as JSON:
    intent effects api.cue --json

  Find missing consequence handlers:
    intent effects api.cue --json | jq '.orphans[]'

  Trace effect chain for specific behavior:
    intent effects api.cue --json | jq '.effects[] | select(.trigger==\"CreateUser\")'

FLAG DETAILS
  --json
    Output structured JSON with effect chains
    Includes: effects[], consequences[], orphaned_states[]

EXIT CODES
  0 = Effects analysis complete
  3 = Invalid spec file
  4 = Runtime error

SEE ALSO
  intent invert   - Failure mode analysis
  intent gaps     - Gap detection
  intent coverage - Coverage analysis"

/// Extended help text for the `ears` command (KIRK)
pub const kirk_ears_extended_help = "KIRK: Parse EARS requirements into Intent behaviors

WHAT IT DOES
  EARS (Easy Approach to Requirements Syntax) parser converts natural language
  requirements into structured Intent spec behaviors. Recognizes 5 patterns:
  THE SYSTEM SHALL (ubiquitous), GIVEN/WHEN/THEN (scenario-based), IF/THEN
  (conditional), and state-based patterns. Generates complete spec from
  requirements document.

WHY YOU'D USE IT
  Convert requirements documents to executable specs. Ensure all requirements
  have corresponding test cases. Bridge business requirements to test automation.

WHEN TO USE IT
  When starting from requirements document, during requirements review, or
  when migrating from manual testing to contract-driven approach.

PREREQUISITES
  - Requirements file in EARS format (natural language or markdown)

USAGE EXAMPLES

  Parse requirements to spec:
    intent ears requirements.md

  Parse and export to CUE:
    intent ears requirements.md --output cue > api.cue

  Parse and export to JSON:
    intent ears requirements.md --output json > requirements.json

  Parse and save to file:
    intent ears requirements.md --output cue --out spec.cue

FLAG DETAILS
  --output FORMAT
    Output format: cue (default), json, text
    Determines spec representation

  --out FILE
    Write output to file instead of stdout
    If omitted, output to console

  --json
    Alias for --output json

EXIT CODES
  0 = Parsing complete
  1 = No EARS patterns found
  3 = Invalid requirements file
  4 = Parsing error

SEE ALSO
  intent validate - Validate generated spec
  intent show     - Preview parsed spec"

/// Extended help text for the `parse` command
pub const parse_extended_help = "Parse EARS requirements to structured spec

WHAT IT DOES
  Utility command that parses EARS (Easy Approach to Requirements Syntax)
  requirements into structured Intent spec format. Same as `intent ears` but
  provided as alternative command for discoverability.

WHY YOU'D USE IT
  Convert requirements documents to machine-readable specs. Build specs from
  existing requirements without manual spec writing. Automate spec generation
  from requirement documents.

WHEN TO USE IT
  Starting new spec from requirements, migrating from document-based to
  contract-driven testing, or batch-processing requirements files.

PREREQUISITES
  - Requirements file in EARS format

USAGE EXAMPLES

  Parse requirements file:
    intent parse requirements.md

  Convert to CUE spec:
    intent parse requirements.md --output cue > api.cue

  Parse to JSON and save:
    intent parse requirements.md --output json --out requirements.json

  Batch process multiple files:
    for f in reqs/*.md; do intent parse \"$f\" --output cue --out \"specs/$(basename $f .md).cue\"; done

FLAG DETAILS
  --output FORMAT
    Output format: cue (default), json, text
    cue: Intent CUE specification format
    json: Structured JSON representation
    text: Human-readable text format

  --out FILE
    Write to file instead of stdout
    Creates file if it doesn't exist, overwrites if it does

  --json
    Shorthand for --output json

EXIT CODES
  0 = Parsing complete
  1 = No parseable patterns found
  3 = Invalid input file
  4 = Runtime error

SEE ALSO
  intent ears     - EARS parsing (same functionality)
  intent validate - Validate generated spec
  intent show     - Preview spec contents"
