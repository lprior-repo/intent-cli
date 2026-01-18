/// KIRK Analysis Commands - Extended Help Text & Examples
///
/// This module provides production-ready Gleam code to enhance the seven KIRK analysis
/// commands (quality, invert, coverage, gaps, effects, ears, parse) with comprehensive
/// help text and realistic usage examples.
///
/// Integration: Replace the existing command definitions (lines 2774-3513 in src/intent.gleam)
/// with the updated function signatures shown below. All functions follow the pattern:
///   |> glint.description(...)          // Short 50-100 char description
///   |> glint.long_help("""...""")      // Extended help with examples
///   |> glint.flag(...)                 // Flag definitions

/// =============================================================================
/// QUALITY COMMAND - Extended Help & Examples
/// =============================================================================

/// The `quality` command - KIRK quality analysis
///
/// Pattern integration:
/// ```gleam
/// fn kirk_quality_command() -> glint.Command(Nil) {
///   glint.command(fn(input: glint.CommandInput) {
///     // ... existing implementation (lines 2775-2831) ...
///   })
///   |> glint.description(cli_text_constants.cmd_quality_desc)
///   |> glint.long_help(quality_long_help())
///   |> glint.flag("json", cli_flags.json_flag())
/// }
/// ```

pub fn quality_long_help() -> String {
  """
KIRK: Analyze spec quality across coverage, clarity, testability, consistency, and security

What it does:
  Evaluates your Intent spec against five quality dimensions with detailed scoring
  and issue categorization (completeness, consistency, testability, clarity, security).

Why you'd use it:
  Before running tests or planning implementation, understand spec gaps and quality
  issues that could impact development velocity and test coverage.

When to use it:
  • Early in spec authoring to validate completeness
  • After major spec revisions to measure improvement
  • As a gate before marking spec "ready for implementation"
  • To identify which dimensions need focus (clarity vs. testability vs. security)

Mental Model:
  4-Dimensional Quality Scoring:
    • Completeness: Do all required fields have content?
    • Consistency: Are naming patterns, field types, and status codes consistent?
    • Testability: Can behaviors be verified? Do checks exist for assertions?
    • Clarity: Is language unambiguous? Are descriptions sufficient?
    • Security: Are auth behaviors present? Are error cases defined?

EXAMPLES:

  Basic quality analysis:
    intent quality examples/user-api.cue

  Output as JSON for tooling:
    intent quality examples/user-api.cue --json

  Pipe to file for archival:
    intent quality api.cue | tee quality-report.txt

  Integration with doctor workflow:
    intent quality api.cue && intent doctor api.cue

INTERPRETING RESULTS:

  Overall Score (0-100):
    ≥90% ✓  Ready for implementation
    70-89% ⚠  Address medium-severity issues first
    <70%  ✗  Significant gaps; resolve before testing

  Per-Dimension Scores:
    Each dimension (completeness, consistency, testability, clarity, security)
    scored independently. Use individual scores to prioritize improvements.

  Issues List:
    Severity levels:
      • Critical: Blocks implementation (no success criteria, empty feature list)
      • High: Degrades testing (missing checks, inconsistent status codes)
      • Medium: Reduces clarity (vague descriptions, missing notes)
      • Low: Style suggestions (naming patterns, minor improvements)

ADVANCED USAGE:

  Monitor quality over time:
    for version in v1 v2 v3; do
      echo "=== $version ===" >> quality-trends.txt
      intent quality specs/$version.cue --json >> quality-trends.txt
    done

  Combine with other KIRK analysis:
    # Full spec audit pipeline
    intent quality api.cue && \\
    intent coverage api.cue && \\
    intent gaps api.cue && \\
    intent invert api.cue
"""
}

/// =============================================================================
/// INVERT COMMAND - Extended Help & Examples
/// =============================================================================

/// The `invert` command - KIRK inversion analysis
///
/// Pattern integration:
/// ```gleam
/// fn kirk_invert_command() -> glint.Command(Nil) {
///   glint.command(fn(input: glint.CommandInput) {
///     // ... existing implementation (lines 2838-2900) ...
///   })
///   |> glint.description(cli_text_constants.cmd_invert_desc)
///   |> glint.long_help(invert_long_help())
///   |> glint.flag("json", cli_flags.json_flag())
/// }
/// ```

pub fn invert_long_help() -> String {
  """
KIRK: Identify missing failure cases through inversion analysis

What it does:
  Applies failure mode analysis to spec behaviors, identifying what's NOT explicitly
  tested or handled. Categorizes gaps into security, usability, and integration issues.

Why you'd use it:
  Positive testing (happy path) is natural. Inversion forces you to think: "What could
  go wrong?" Discovers unhandled edge cases before they become production bugs.

When to use it:
  • Before check command to ensure error behaviors are defined
  • After defining success behaviors, add corresponding failure modes
  • During security review to validate auth and input validation
  • To identify missing error codes and status handling

Mental Model:
  Inversion = thinking in reverse. For each behavior:
    • Normal case: "User authenticates → 200 OK"
    • Inverted: "User authenticates badly → 401/403?"
                "User is banned → what code?"
                "User loses connection → timeout + retry?"

  Gap Categories:
    • Security: Missing auth, encryption, validation, rate-limiting
    • Usability: Missing error messages, unclear status codes, no retry guidance
    • Integration: Missing dependency behaviors, cascade failures, race conditions

EXAMPLES:

  Basic inversion analysis:
    intent invert examples/user-api.cue

  JSON output for test generation:
    intent invert api.cue --json | jq '.suggested_behaviors'

  Find security gaps:
    intent invert api.cue | grep -A 5 "Security"

  Integration with quality review:
    intent quality api.cue
    intent invert api.cue
    intent improve api.cue

INTERPRETING RESULTS:

  Inversion Score (0-100):
    ≥80% ✓  Most failure modes identified
    60-79% ⚠  Review suggested behaviors before implementation
    <60%  ✗  Significant gaps; add more error behaviors

  Gap Types:
    1. Security gaps: Authentication, authorization, input validation, encryption
    2. Usability gaps: Error messages, status clarity, retry guidance
    3. Integration gaps: Dependency failures, cascading effects, race conditions

  Suggested Behaviors:
    Each suggested behavior includes:
      • Name: Descriptive failure scenario
      • Intent: Why this behavior matters
      • Expected status: HTTP status code for this error
      • Category: Security | Usability | Integration

ADVANCED USAGE:

  Add all suggested behaviors to spec:
    # Step 1: Generate JSON with suggestions
    intent invert api.cue --json > inversion-report.json

    # Step 2: Review suggestions (manual step)
    cat inversion-report.json | jq '.suggested_behaviors[] | .intent'

    # Step 3: Integrate into behaviors section

  Security-focused inversion:
    intent invert api.cue | grep -E "(auth|encrypt|validate|csrf)"

  Combine with gaps analysis:
    intent invert api.cue && \\
    intent gaps api.cue
"""
}

/// =============================================================================
/// COVERAGE COMMAND - Extended Help & Examples
/// =============================================================================

/// The `coverage` command - KIRK coverage analysis
///
/// Pattern integration:
/// ```gleam
/// fn kirk_coverage_command() -> glint.Command(Nil) {
///   glint.command(fn(input: glint.CommandInput) {
///     // ... existing implementation (lines 2916-2976) ...
///   })
///   |> glint.description(cli_text_constants.cmd_coverage_desc)
///   |> glint.long_help(coverage_long_help())
///   |> glint.flag("json", cli_flags.json_flag())
/// }
/// ```

pub fn coverage_long_help() -> String {
  """
KIRK: Analyze coverage including OWASP Top 10 and edge cases

What it does:
  Measures three orthogonal coverage dimensions:
    1. HTTP Methods: Does spec cover GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS?
    2. Status Codes: Range of HTTP responses (1xx/2xx/3xx/4xx/5xx)?
    3. OWASP Top 10: Does spec address security vulnerabilities?

Why you'd use it:
  Complete API specs must handle multiple methods and edge case responses. Security
  coverage prevents "that vulnerability wasn't tested" surprises.

When to use it:
  • When designing new API endpoints to ensure method completeness
  • After integration with auth/validation layers
  • Pre-release to verify OWASP Top 10 mitigations are tested
  • To identify which HTTP status codes need explicit behaviors

Mental Model:
  Coverage = breadth across multiple dimensions:
    • Methods: Does spec exercise REST CRUD patterns (POST create, GET read, etc.)?
    • Status Codes: 2xx (success), 4xx (client error), 5xx (server error) buckets
    • OWASP: A01-Injection, A02-Broken Auth, A03-Sensitive Data, etc.

  Each dimension scored independently. Low scores signal incomplete spec.

EXAMPLES:

  Basic coverage analysis:
    intent coverage examples/user-api.cue

  JSON output for dashboards:
    intent coverage api.cue --json | jq '.owasp_missing'

  Find missing methods:
    intent coverage api.cue | grep -A 10 "HTTP Methods"

  Check OWASP compliance:
    intent coverage api.cue | grep -E "OWASP|Missing"

  Full spec audit:
    intent coverage api.cue
    intent invert api.cue
    intent gaps api.cue

INTERPRETING RESULTS:

  Overall Score (0-100):
    ≥85% ✓  Strong coverage; ready for testing
    70-84% ⚠  Review missing status codes and OWASP items
    <70%  ✗  Significant gaps; add more behaviors and error cases

  Methods Coverage:
    Shows count of behaviors per HTTP method (GET, POST, PUT, DELETE, PATCH).
    Unimplemented methods marked as missing.

  Status Codes Coverage:
    Distribution across code ranges:
      • 2xx: Success responses (200, 201, 204, 206)
      • 3xx: Redirects (301, 302, 304, 307)
      • 4xx: Client errors (400, 401, 403, 404, 409, 422, etc.)
      • 5xx: Server errors (500, 502, 503, 504)
    Most specs should have 2xx and 4xx; 5xx optional per design.

  OWASP Top 10 Missing:
    Lists which of the Top 10 vulnerabilities aren't explicitly tested:
      A01-Injection: SQL injection, command injection, etc.
      A02-Broken Auth: Missing auth tests, token expiry, etc.
      A03-Sensitive Data: Missing encryption, PII handling, etc.
      A04-XML External Entities (XXE)
      A05-Broken Access Control: Missing authz tests
      A06-Security Misconfiguration: Default passwords, debug endpoints
      A07-XSS (Cross-Site Scripting)
      A08-Insecure Deserialization
      A09-Using Components With Known Vulnerabilities
      A10-Insufficient Logging & Monitoring

ADVANCED USAGE:

  Add missing behaviors by status code:
    # Identify missing status codes
    intent coverage api.cue --json | jq '.status_codes | keys'

    # Manually add behaviors for gaps (e.g., 429 Rate Limit)

  Track coverage over releases:
    for tag in v1.0 v1.1 v2.0; do
      git checkout $tag
      echo "=== $tag ===" >> coverage-trends.csv
      intent coverage api.cue --json >> coverage-trends.csv
    done

  Combine with OWASP vulnerability scanner:
    # Intent checks spec completeness
    intent coverage api.cue

    # Pair with external tools (OWASP ZAP, Burp, etc.) for runtime verification
"""
}

/// =============================================================================
/// GAPS COMMAND - Extended Help & Examples
/// =============================================================================

/// The `gaps` command - KIRK gap detection
///
/// Pattern integration:
/// ```gleam
/// fn kirk_gaps_command() -> glint.Command(Nil) {
///   glint.command(fn(input: glint.CommandInput) {
///     // ... existing implementation (lines 2980-3051) ...
///   })
///   |> glint.description(cli_text_constants.cmd_gaps_desc)
///   |> glint.long_help(gaps_long_help())
///   |> glint.flag("json", cli_flags.json_flag())
/// }
/// ```

pub fn gaps_long_help() -> String {
  """
KIRK: Detect specification gaps using mental models

What it does:
  Applies five gap detection mental models to identify missing requirements:
    1. Inversion: "What could fail?"
    2. Second-order effects: "What happens next?"
    3. Checklist: "Did we check all req items?"
    4. Coverage: "Are all HTTP methods/codes covered?"
    5. Security: "Are auth/validation behaviors defined?"

Why you'd use it:
  Gaps compound. One missing validation check becomes a security vulnerability in
  production. Gaps analysis catches oversights before implementation.

When to use it:
  • Mid-spec development to validate completeness
  • Before handing spec to implementation team
  • During security review alongside quality analysis
  • To educate team on missing mental models

Mental Model:
  5-Round Mental Model System (from CLAUDE.md):
    Round 1 (EARS): Ubiquitous/Event/State/Unwanted patterns
    Round 2 (Contracts): Response checks with rule+why
    Round 3 (Inversion): Anti-patterns + error behaviors
    Round 4 (Effects): requires[] + verification behaviors
    Round 5 (Pre-mortem): ai_hints.pitfalls

  Gap = missing requirement per mental model

EXAMPLES:

  Basic gap detection:
    intent gaps examples/user-api.cue

  JSON output for reporting:
    intent gaps api.cue --json | jq '.inversion_gaps'

  List critical gaps only:
    intent gaps api.cue | grep -E "Critical|High"

  Integration test planning:
    intent gaps api.cue
    intent effects api.cue

  Full audit workflow:
    intent quality api.cue
    intent gaps api.cue
    intent invert api.cue
    intent coverage api.cue

INTERPRETING RESULTS:

  Total Gaps: Aggregate count across all five gap types.

  Severity Breakdown:
    Critical: Blocks implementation or introduces severe risk
    High: Degrades functionality or introduces moderate risk
    Medium: Reduces clarity or introduces minor risk
    Low: Style/documentation suggestions

  Gap Types:

    1. Inversion Gaps: Missing error behaviors
       → For each success behavior, is error handling defined?
       → Example: "Forgot to test invalid_token response"

    2. Second-Order Gaps: Missing consequence behaviors
       → When this behavior fires, what else must change?
       → Example: "User deletion should cascade to remove tokens"

    3. Checklist Gaps: Missing spec requirements
       → Required fields missing: config, ai_hints, anti_patterns, etc.
       → Example: "Spec has no anti_patterns section"

    4. Coverage Gaps: Incomplete method/code coverage
       → Missing HTTP methods: PATCH, DELETE
       → Missing status codes: 429, 504
       → Example: "No behaviors for rate-limiting (429)"

    5. Security Gaps: Missing security behaviors
       → No authentication checks
       → No validation errors
       → No OWASP Top 10 coverage
       → Example: "No SQL injection test for search endpoint"

ADVANCED USAGE:

  Prioritize gaps by severity:
    intent gaps api.cue --json | jq -s '.[0].critical + .[0].high' | length

  Workflow: Find → Fix → Verify
    # Step 1: Identify gaps
    intent gaps api.cue > gaps-report.txt

    # Step 2: Update spec (manual)
    # Edit api.cue to address gaps

    # Step 3: Verify improvement
    intent gaps api.cue > gaps-report-v2.txt
    diff gaps-report.txt gaps-report-v2.txt

  Combine gap detection with other mental models:
    intent gaps api.cue
    intent invert api.cue
    intent effects api.cue
    intent coverage api.cue
"""
}

/// =============================================================================
/// EFFECTS COMMAND - Extended Help & Examples
/// =============================================================================

/// The `effects` command - KIRK second-order effects analysis
///
/// Pattern integration:
/// ```gleam
/// fn kirk_effects_command() -> glint.Command(Nil) {
///   glint.command(fn(input: glint.CommandInput) {
///     // ... existing implementation (lines 3065-3109) ...
///   })
///   |> glint.description(cli_text_constants.cmd_effects_desc)
///   |> glint.long_help(effects_long_help())
///   |> glint.flag("json", cli_flags.json_flag())
/// }
/// ```

pub fn effects_long_help() -> String {
  """
KIRK: Trace second-order effects and consequence chains

What it does:
  Identifies cascading effects: "If behavior X happens, what else must happen?"
  Traces requirement dependencies and unintended consequence chains.

Why you'd use it:
  APIs rarely operate in isolation. User deletion should cascade (revoke tokens,
  cleanup sessions). Missing effects cause test-to-production surprises.

When to use it:
  • After defining core behaviors, before finalizing spec
  • When integrating with external services (payments, auth, etc.)
  • During code review to validate cascading updates
  • To explain dependency chains to implementation team

Mental Model:
  Second-Order Effects = consequence analysis:
    • Direct effect: "GET /user returns 200"
    • First-order consequence: "Response must include auth_token"
    • Second-order consequence: "Token must be validated on next request"
    • Third-order consequence: "Expired token → 401 + new login required"

  Common patterns:
    • State transitions: Active → Inactive → Deleted
    • Cascades: Delete user → delete sessions → invalidate tokens
    • Notifications: Order placed → payment processed → confirmation email
    • Consistency: Update account → update permissions → revoke old tokens

EXAMPLES:

  Basic effects analysis:
    intent effects examples/user-api.cue

  JSON output for workflow visualization:
    intent effects api.cue --json | jq '.consequences'

  Find orphaned behaviors:
    intent effects api.cue | grep "Orphan"

  Plan implementation order:
    intent effects api.cue | head -20

  Combine with gaps analysis:
    intent gaps api.cue
    intent effects api.cue
    intent invert api.cue

INTERPRETING RESULTS:

  Consequence Chains:
    Chains show dependency order. Implement behaviors in this order:
      Level 1: Independent behaviors (no requires[])
      Level 2: Depend on Level 1 (requires [Level1])
      Level 3: Depend on Level 2, etc.

  Orphaned Behaviors:
    Behaviors that require non-existent behaviors.
    → Fix: Either add missing behavior or remove dependency

  Coverage:
    Percentage of behaviors with defined consequences.
    ≥90% = well-modeled spec
    <70% = likely missing verification behaviors

  Effects Map:
    Shows which behaviors trigger which other behaviors.
    Useful for:
      • Implementation planning (topological sort)
      • Test ordering (dependencies first)
      • Rollback planning (reverse order)

ADVANCED USAGE:

  Export dependency graph for visualization:
    intent effects api.cue --json | jq '.effects_map' > effects.json
    # Use with tools like Graphviz or D3.js for visualization

  Validate cascade completeness:
    # Step 1: List all "delete" behaviors
    intent effects api.cue | grep -i delete

    # Step 2: For each, verify cascade behaviors are defined
    # Example: delete-user should have requires [delete-sessions, delete-tokens]

  Implementation scheduling:
    # Effects output shows dependency depth
    # Implement depth-0 (independent), then depth-1, then depth-2, etc.
    # Useful for sprint planning and parallel work allocation

  Detect circular dependencies:
    intent effects api.cue | grep "Circular"
    # If found, break cycle by removing one dependency or adding intermediate behavior
"""
}

/// =============================================================================
/// EARS COMMAND - Extended Help & Examples
/// =============================================================================

/// The `ears` command - KIRK EARS requirements parser
///
/// Pattern integration:
/// ```gleam
/// fn kirk_ears_command() -> glint.Command(Nil) {
///   glint.command(fn(input: glint.CommandInput) {
///     // ... existing implementation (lines 3124-3258) ...
///   })
///   |> glint.description(cli_text_constants.cmd_ears_desc)
///   |> glint.long_help(ears_long_help())
///   |> glint.flag("output", flag_output_format_flag())
///   |> glint.flag("out", flag_output_file_flag())
///   |> glint.flag("name", flag_spec_name_flag())
/// }
/// ```

pub fn ears_long_help() -> String {
  """
KIRK: Parse EARS requirements into Intent behaviors

What it does:
  Converts Easily Achievable Requirements Syntax (EARS) formatted requirements
  files into Intent behaviors. EARS patterns map to ubiquitous, event, state,
  and exception-based requirements.

Why you'd use it:
  EARS structure maps well to API behaviors:
    • Ubiquitous: Always-true behaviors
    • Event-Driven: Trigger → action patterns
    • State-Driven: Context-dependent behaviors
    • Unwanted: Error/exception behaviors

When to use it:
  • Converting requirements documents to specs
  • Validating EARS format before importing to Intent
  • Understanding which requirements map to which behaviors
  • Identifying unparseable or malformed requirements

EARS Format:

  Five patterns. Write each requirement as a complete sentence:

    1. Ubiquitous (always true)
       THE SYSTEM SHALL [behavior]
       Example:
         THE SYSTEM SHALL validate all API requests have valid JSON

    2. Event-Driven (when trigger, then behavior)
       WHEN [trigger] THE SYSTEM SHALL [behavior]
       Example:
         WHEN user submits login form THE SYSTEM SHALL verify credentials

    3. State-Driven (in context, then behavior)
       WHILE [state] THE SYSTEM SHALL [behavior]
       Example:
         WHILE user is authenticated THE SYSTEM SHALL allow token refresh

    4. Optional (conditional behavior)
       WHERE [condition] THE SYSTEM SHALL [behavior]
       Example:
         WHERE user role is admin THE SYSTEM SHALL expose /admin endpoints

    5. Unwanted (exception, rejection)
       IF [condition] THEN THE SYSTEM SHALL NOT [behavior]
       Example:
         IF user is blocked THEN THE SYSTEM SHALL NOT allow login

    6. Complex (combine patterns)
       WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]
       Example:
         WHILE session is active WHEN user clicks logout THE SYSTEM SHALL
         invalidate session

EXAMPLES:

  Parse requirements file:
    intent ears requirements.md

  Convert to CUE spec:
    intent ears requirements.md --output cue --out api.cue

  Export as JSON for tooling:
    intent ears requirements.md --output json

  Validate EARS format (catches syntax errors):
    intent ears requirements.md
    # Shows parsing errors and suggestions

  Review parsed behaviors:
    intent ears requirements.md | head -30

  Full workflow: requirements → behaviors → testing
    intent ears requirements.md --output cue --out api.cue
    intent validate api.cue
    intent check api.cue --target http://localhost:8080

INTERPRETING RESULTS:

  Text Output:
    Shows requirements parsed, grouped by pattern type:
      • Ubiquitous (N parsed)
      • Event-Driven (N parsed)
      • State-Driven (N parsed)
      • Optional (N parsed)
      • Unwanted (N parsed)
      • Complex (N parsed)
    Lists any parsing errors with line numbers and suggestions.

  CUE Output:
    Generates partial CUE spec with behaviors extracted from requirements.
    Must manually add: name, description, version, config, ai_hints, etc.
    But behaviors, features are auto-generated.

  JSON Output:
    Structured data suitable for API consumption:
      {
        "requirements": [...],  // Original parsed requirements
        "behaviors": [...],     // Extracted behaviors
        "errors": [...],        // Parsing failures
        "warnings": [...]       // Style suggestions
      }

ERROR HANDLING:

  Parsing Errors:
    Lines that don't match any EARS pattern.
    → Review suggestion and reformat to valid pattern.
    → Example: "THE USER LOGS IN" → "THE SYSTEM SHALL authenticate user"

  Warnings:
    Style suggestions that don't block parsing:
    → Inconsistent capitalization
    → Missing commas
    → Ambiguous phrasing
    → Vague action verbs (use specific verbs: validates, transforms, deletes)

ADVANCED USAGE:

  Batch convert multiple requirements files:
    for file in requirements/*.md; do
      intent ears "$file" --output cue --out "specs/$(basename $file .md).cue"
    done

  Quality gate: parse requirements, validate output
    intent ears requirements.md --output cue --out api.cue && \\
    intent validate api.cue && \\
    echo "✓ Requirements converted successfully"

  Audit trail: preserve original EARS format + parsed behaviors
    intent ears requirements.md --output json > audit-trail.json
    # Allows tracing: requirement → behavior → test

BEST PRACTICES:

  • Use consistent phrasing (THE SYSTEM SHALL, not "The system should")
  • Use specific verbs (validates, returns, deletes) vs. vague (handles, manages)
  • Each requirement = one behavior (no AND-chaining)
  • Use WHERE for role-based behaviors (Where user is admin)
  • Use IF...SHALL NOT for security/validation behaviors
  • Complex patterns rare; prefer simple WHEN/WHILE
"""
}

/// =============================================================================
/// PARSE COMMAND - Extended Help & Examples
/// =============================================================================

/// The `parse` command - parse EARS requirements to spec
///
/// Pattern integration:
/// ```gleam
/// fn parse_command() -> glint.Command(Nil) {
///   glint.command(fn(input: glint.CommandInput) {
///     // ... existing implementation (lines 3266-3513) ...
///   })
///   |> glint.description(cli_text_constants.cmd_parse_desc)
///   |> glint.long_help(parse_long_help())
///   |> glint.flag("o", flag_output_file_flag())
///   |> glint.flag("json", cli_flags.json_flag())
/// }
/// ```

pub fn parse_long_help() -> String {
  """
Parse EARS requirements to structured spec

What it does:
  Full-pipeline requirement parsing: EARS format → Intent CUE spec.
  Combines EARS parsing with spec generation, ready for validation and testing.

Why you'd use it:
  Automate requirements-to-tests workflow. Reduce manual transcription errors.
  Creates spec foundation that engineers can build on.

When to use it:
  • Converting legacy requirements documents to specs
  • CI/CD pipeline: requirements commit → test generation
  • During product discovery to lock in behavioral contracts
  • Validating requirements capture before development

Mental Model:
  Requirements → Spec Generation Pipeline:
    1. Parse EARS patterns from requirements
    2. Extract behaviors (name, intent, method, path, status)
    3. Generate CUE spec with features and behaviors
    4. Validate CUE syntax
    5. Ready for testing and implementation

EXAMPLES:

  Convert requirements to spec:
    intent parse requirements.md -o api.cue

  Validate requirements are well-formed:
    intent parse requirements.md

  JSON output for downstream tools:
    intent parse requirements.md --json

  Full workflow: capture → parse → validate → test
    intent parse requirements.md -o api.cue
    intent validate api.cue
    intent check api.cue --target http://localhost:8080

  CI/CD integration (fail if requirements invalid):
    intent parse requirements.md -o generated-spec.cue || exit 1
    intent validate generated-spec.cue || exit 1
    git add generated-spec.cue
    # Commit auto-generated spec alongside requirements

INTERPRETING RESULTS:

  Text Output:
    Shows parsing progress with counts by pattern type:
      ✓ Parsed 5 ubiquitous requirements
      ✓ Parsed 8 event-driven requirements
      ✓ Parsed 3 state-driven requirements
      ✗ Error parsing line 42: ...
      ✗ Error parsing line 55: ...
    Lists failures (if any) with line numbers and fix suggestions.

  CUE Output (-o flag):
    Generated spec file with:
      ✓ Features and behaviors extracted from requirements
      ✓ HTTP methods and paths inferred
      ✗ TODO: name, description, version (add manually)
      ✗ TODO: config, ai_hints, success_criteria (add manually)
    Ready for `intent validate` and `intent check`.

  JSON Output (--json flag):
    Structured results for programmatic consumption:
      {
        "requirements": 16,            // Total parsed
        "behaviors": 12,               // Extracted behaviors
        "errors": 2,                   // Failed lines
        "requirements": [              // Details
          {
            "id": "REQ-001",
            "pattern": "EventDriven",
            "system_shall": "verify credentials",
            "raw_text": "WHEN user submits login..."
          },
          ...
        ],
        "behaviors": [
          {
            "name": "verify_credentials",
            "intent": "Check username and password",
            "method": "POST",
            "path": "/login",
            "status": 200
          },
          ...
        ],
        "errors": [
          {
            "line": 42,
            "message": "Invalid EARS pattern",
            "suggestion": "Use 'WHEN ... THE SYSTEM SHALL'"
          }
        ]
      }

ERROR HANDLING:

  Parse Errors:
    Lines that don't match EARS syntax.
    → Fix requirement text to match one of five patterns
    → Retry parsing

  Warnings:
    Parsing succeeds but quality issues detected:
    → Inconsistent language (use present tense)
    → Vague action verbs (use validates, returns, deletes)
    → Ambiguous conditions (be specific about context)

  Exit Codes:
    0 = All requirements parsed successfully
    1 = Some requirements failed to parse
      Review errors and fix requirements file, then rerun

ADVANCED USAGE:

  Validate and commit generated spec:
    # Step 1: Parse requirements
    intent parse requirements.md -o api.cue

    # Step 2: Validate CUE syntax and structure
    intent validate api.cue

    # Step 3: Manual polish (add name, version, success criteria)
    # Edit api.cue

    # Step 4: Validate again, then commit
    intent validate api.cue && git add api.cue

  Track requirements changes:
    # Parse current requirements
    intent parse current-req.md -o current-spec.cue --json > current.json

    # Parse previous requirements
    intent parse previous-req.md -o previous-spec.cue --json > previous.json

    # Diff to see what changed
    jq '.behaviors' current.json previous.json | diff

  Multi-format export:
    # Generate spec for multiple environments
    for env in dev staging prod; do
      intent parse "requirements/$env.md" -o "specs/$env.cue"
    done

  CI/CD gate: parse → validate → quality check
    #!/bin/bash
    intent parse requirements.md -o api.cue || exit 1
    intent validate api.cue || exit 1
    intent quality api.cue || exit 1
    echo "✓ Requirements → Spec pipeline successful"

BEST PRACTICES:

  • Keep requirements.md in version control alongside specs
  • Regenerate spec when requirements change
  • Use intent validate to catch syntax errors
  • Use intent quality to measure spec completeness
  • Pair with intent check to validate against running API
  • Review generated spec; add name, version, config manually
  • Test generated spec immediately (catch issues early)
"""
}

/// =============================================================================
/// FLAG HELPER FUNCTIONS (for integration)
/// =============================================================================

pub fn flag_output_format_flag() -> glint.flag.FlagBuilder(String) {
  glint.flag.string()
    |> glint.flag.default("text")
    |> glint.flag.description("Output format: text, cue, json")
}

pub fn flag_output_file_flag() -> glint.flag.FlagBuilder(String) {
  glint.flag.string()
    |> glint.flag.default("")
    |> glint.flag.description("Output file path")
}

pub fn flag_spec_name_flag() -> glint.flag.FlagBuilder(String) {
  glint.flag.string()
    |> glint.flag.default("GeneratedSpec")
    |> glint.flag.description("Spec name for CUE output")
}
