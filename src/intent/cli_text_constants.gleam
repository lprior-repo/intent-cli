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

WHAT IT DOES:
  Computes a topological execution plan from a session's beads, organizing
  them into parallel waves by dependency depth and calculating total effort,
  risk level, and blockers.

WHY YOU'D USE IT:
  To understand the scope, complexity, and sequencing of work before starting
  execution. Identifies dependencies, parallel opportunities, and risk factors.

WHEN TO USE IT:
  After running 'intent interview' and 'intent beads' to see the planned work.
  Use before 'intent plan-approve' to review and validate the plan. Reference
  during execution to track progress and adjust strategies.

PLANNING WORKFLOW:
  1. Run interview & generate beads:
     $ intent interview myprofile --export myspec.cue
     $ intent beads myspec.cue > session.id

  2. View the plan to understand scope:
     $ intent plan <session_id>

  3. If satisfied, approve for execution:
     $ intent plan-approve <session_id>

  4. Execute beads according to wave order, then gather feedback:
     $ bd ready --json | xargs -I {} bash -c '...'
     $ intent beads-regenerate <session_id> --strategy hybrid

PLAN OUTPUTS:
  - Wave structure: Lists which beads can run in parallel (same depth)
  - Effort breakdown: Total time, complexity estimates per feature
  - Risk assessment: Critical path, blocker analysis, dependency depth
  - Dependencies: Shows requires[] relationships between beads
  - Example output (human format):
    Phase 1 (5 beads, 2.5 hours)
      Wave 1: auth-setup, db-init [parallel]
      Wave 2: seed-users [depends on Wave 1]

FORMAT OPTIONS:
  --format human  (default) Pretty-printed with wave structure and risk info
  --format json   Structured data for CI automation and tool integration

EXAMPLES:
  View plan for a session:
    intent plan abc-123

  Export plan as JSON for CI/CD automation:
    intent plan abc-123 --format json > plan.json

  Integrate with GitHub Actions (check if plan is valid):
    intent plan ${{ env.SESSION_ID }} --format json | jq '.risk == \"low\"'
"

/// Extended help text for the `plan-approve` command
pub const plan_approve_extended_help = "Approve execution plan for session (CI/automation ready)

WHAT IT DOES:
  Reviews the session's execution plan and records approval (human or automated).
  Validates plan quality, presents risk summary, and stores approval timestamp
  and metadata in the session file.

WHY YOU'D USE IT:
  Explicit approval gates prevent accidental execution of large/risky plans.
  Creates audit trail for compliance, tracks who approved what and when.
  Enables CI/CD automation with --yes flag for fully automated pipelines.

WHEN TO USE IT:
  After reviewing plan output with 'intent plan'. Before executing beads with
  'bd ready'. In CI/CD pipelines to gate automated work execution.

APPROVAL WORKFLOW:
  1. Manual approval (human gatekeeper):
     $ intent plan <session_id>        # Review scope & risk
     $ intent plan-approve <session_id> # Interactive prompt
     # Enters approval when satisfied

  2. Automated approval (CI/CD):
     $ intent plan-approve <session_id> --yes # Non-interactive
     # Perfect for GitHub Actions, GitLab CI, etc.

  3. Approval with audit trail:
     $ intent plan-approve <session_id> --yes \\
       --notes \"Approved by security review, passed OWASP checks\"
     # Documents who/why approved for compliance

APPROVAL GATES:
  - Risk level check: Warns if High or Critical
  - Blocker detection: Halts if blockers present (requires manual override)
  - Dependency validation: Ensures all requires[] are satisfied
  - Approval metadata: Captures timestamp, approver, notes

INTERACTIVE MODE (default):
  Displays plan summary with risk/blockers, prompts \"Approve this plan? (yes/no)\"
  Good for human review, prevents accidental approvals.

CI/CD MODE (--yes flag):
  Skips prompt, approves automatically. For non-interactive pipelines.
  Pair with --notes to document approval reason.

OUTPUT:
  Text: Success/error message with approval status
  Exit codes:
    0 = Plan approved successfully
    1 = Plan rejected (user selected 'no')
    2 = Plan blocked (has critical blockers)
    3 = Invalid plan (failed validation)
    4 = System error (file I/O, etc.)

EXAMPLES:
  Interactive approval (human gatekeeper):
    intent plan-approve session-abc-123

  Automated approval in CI (non-interactive):
    intent plan-approve session-abc-123 --yes

  Approval with audit documentation:
    intent plan-approve session-abc-123 --yes \\
      --notes \"Approved by release manager, ready for staging\"

  GitLab CI example:
    approve_plan:
      script:
        - intent plan-approve $SESSION_ID --yes
        - intent beads $SESSION_ID > beads.json
      only:
        - main

  GitHub Actions example:
    - name: Approve execution plan
      run: |
        intent plan-approve ${{ env.SESSION_ID }} --yes \\
          --notes \"Auto-approved by CI/${{ github.actor }}\"
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
"

/// Extended help text for the `beads-regenerate` command
pub const beads_regenerate_extended_help = "Regenerate failed/blocked beads with adjusted approach

WHAT IT DOES:
  Analyzes execution feedback from completed beads, identifies failed/blocked
  work, and generates new beads with alternative strategies (inversion, effects,
  pre-mortem analysis). Replaces broken beads with improved alternatives.

WHY YOU'D USE IT:
  When beads fail during execution, regeneration provides alternative approaches
  rather than repeating the same failed strategy. Uses mental model shifts
  (inversion, effects, pre-mortem) to find new solutions.

WHEN TO USE IT:
  After executing a session's beads and gathering feedback. When some beads
  Failed (crashed, timeout) or Blocked (dependency issues). Before rerun to
  avoid repeating failed patterns.

REGENERATION WORKFLOW:
  1. Execute initial beads and capture feedback:
     $ bd ready --json | xargs -I {} intent bead-status {} --status failed
     $ intent beads <session_id> > beads.json

  2. View which beads failed:
     $ cat beads.json | jq '.[] | select(.result == \"failed\")'

  3. Regenerate with alternative strategies:
     $ intent beads-regenerate <session_id> --strategy hybrid

  4. Review generated alternatives and plan next wave:
     $ intent plan <session_id>

  5. Execute regenerated beads:
     $ bd ready --json | xargs -I {} bash -c '...'

REGENERATION STRATEGIES:
  --strategy hybrid (default)
    Combines multiple mental models for best coverage. Uses inversion first,
    then effects, then pre-mortem to find different approaches. Recommended.

  --strategy inversion
    Inverts the failed behavior: \"if X failed, try NOT-X\". Identifies root
    cause assumptions and challenges them. Good for logic/sequence failures.

  --strategy effects
    Analyzes second-order effects and requires[] relationships. Regenerates
    beads that were blocked on dependencies. Good for integration failures.

  --strategy premortem
    Imagines the failure post-mortem and works backward. Identifies weaknesses
    in error handling and edge case coverage. Good for robustness.

BEAD REGENERATION OUTPUTS:
  - Updated session file with regen_metadata[]
  - New beads with alternative approaches
  - Strategy explanation for each regenerated bead
  - Example feedback -> regeneration:
    Failed: auth-service-deploy [timeout]
    → Regenerated with: inversion (add retry logic), effects (check dependencies)
    → New beads: auth-check-deps, auth-deploy-with-fallback

FAILURE HANDLING:
  Failed beads: Execution halted (crash, timeout, assertion). Try alternative
               approach or deeper investigation.
  Blocked beads: Dependency unsatisfied. Check requires[], regenerate parent
               beads, or change execution order.
  Skipped beads: Intentionally skipped. No regeneration needed.

INTEGRATION WITH FEEDBACK:
  $ intent feedback <session_id> --results feedback.json
  $ intent beads-regenerate <session_id> --strategy hybrid
  Feedback loop creates beads from both check failures AND regeneration.

EXAMPLES:
  Regenerate with default hybrid strategy (recommended):
    intent beads-regenerate session-abc-123

  Regenerate using inversion analysis (for logic failures):
    intent beads-regenerate session-abc-123 --strategy inversion

  Regenerate using effects analysis (for dependency failures):
    intent beads-regenerate session-abc-123 --strategy effects

  Regenerate using pre-mortem (for robustness):
    intent beads-regenerate session-abc-123 --strategy premortem

  Full CI/CD workflow with regeneration:
    $ SESSION_ID=$(intent interview myprofile --export myspec.cue)
    $ intent plan $SESSION_ID
    $ intent plan-approve $SESSION_ID --yes
    $ bd ready --json | xargs ./execute-bead.sh
    $ intent feedback $SESSION_ID --results results.json
    $ intent beads-regenerate $SESSION_ID --strategy hybrid
    $ intent plan $SESSION_ID  # See updated plan with regen beads

  Conditional regeneration in GitHub Actions:
    - name: Regenerate failed beads
      if: failure()  # Only on failure
      run: |
        intent beads-regenerate ${{ env.SESSION_ID }} \\
          --strategy hybrid
        intent plan ${{ env.SESSION_ID }} --format json > plan-v2.json
      continue-on-error: true  # Don't block if no failures to regen"
