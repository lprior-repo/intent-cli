/// Example: How to integrate KIRK help text into src/intent.gleam
///
/// This file shows the exact pattern for integrating the help text functions
/// from KIRK_HELP_TEXT_IMPLEMENTATION.gleam into command definitions.

import glint
import glint/flag
import intent/cli_flags
import intent/cli_text_constants

// =============================================================================
// PATTERN 1: Simple Command (Quality)
// =============================================================================

/// Quality command with extended help text
fn kirk_quality_command_example() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... existing implementation from lines 2775-2831 ...
    let is_json =
      flag.get_bool(input.flags, "json")
      |> result.unwrap(False)
    // Process...
  })
  |> glint.description(cli_text_constants.cmd_quality_desc)
  // ADD THIS LINE:
  |> glint.long_help(quality_long_help())
  |> glint.flag("json", cli_flags.json_flag())
}

// =============================================================================
// PATTERN 2: Command with Multiple Flags (EARS)
// =============================================================================

/// EARS command with refactored flag builders and help text
fn kirk_ears_command_example() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... existing implementation from lines 3124-3213 ...
    let output_format =
      flag.get_string(input.flags, "output")
      |> result.unwrap("text")
    // Process...
  })
  |> glint.description(cli_text_constants.cmd_ears_desc)
  // ADD THIS LINE:
  |> glint.long_help(ears_long_help())
  // REPLACE old flag definitions with new helpers:
  |> glint.flag("output", flag_output_format_flag())
  |> glint.flag("out", flag_output_file_flag())
  |> glint.flag("name", flag_spec_name_flag())
}

// =============================================================================
// HELPER FUNCTIONS (from KIRK_HELP_TEXT_IMPLEMENTATION.gleam)
// =============================================================================

/// Quality analysis help text
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
  • Before marking spec "ready for implementation"
  • To identify which dimensions need focus

Mental Model:
  4-Dimensional Quality Scoring:
    • Completeness: All required fields populated?
    • Consistency: Naming, types, status codes uniform?
    • Testability: Behaviors verifiable with checks?
    • Clarity: Language unambiguous, sufficient description?
    • Security: Auth behaviors present, error cases defined?

EXAMPLES:

  Basic quality analysis:
    intent quality examples/user-api.cue

  JSON output for tooling:
    intent quality examples/user-api.cue --json

  Integration with doctor workflow:
    intent quality api.cue && intent doctor api.cue

INTERPRETING RESULTS:

  Overall Score (0-100):
    ≥90% ✓  Ready for implementation
    70-89% ⚠  Address medium-severity issues first
    <70%  ✗  Significant gaps; resolve before testing

  Per-Dimension Scores:
    Each dimension scored independently.
    Use individual scores to prioritize improvements.

ADVANCED USAGE:

  Monitor quality over time:
    for version in v1 v2 v3; do
      intent quality specs/$version.cue --json >> trends.txt
    done

  Full spec audit pipeline:
    intent quality api.cue && \\
    intent coverage api.cue && \\
    intent gaps api.cue && \\
    intent invert api.cue
"""
}

/// EARS parsing help text
pub fn ears_long_help() -> String {
  """
KIRK: Parse EARS requirements into Intent behaviors

What it does:
  Converts EARS formatted requirements into Intent behaviors.
  Maps patterns: ubiquitous, event-driven, state-driven, optional, unwanted, complex.

Why you'd use it:
  EARS structure maps well to API behaviors. Automate requirements capture.

When to use it:
  • Converting requirements documents to specs
  • Validating EARS format before importing
  • Understanding which requirements map to which behaviors
  • Identifying unparseable or malformed requirements

EARS Format:

  1. Ubiquitous:
     THE SYSTEM SHALL [behavior]

  2. Event-Driven:
     WHEN [trigger] THE SYSTEM SHALL [behavior]

  3. State-Driven:
     WHILE [state] THE SYSTEM SHALL [behavior]

  4. Optional:
     WHERE [condition] THE SYSTEM SHALL [behavior]

  5. Unwanted:
     IF [condition] THEN THE SYSTEM SHALL NOT [behavior]

  6. Complex:
     WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]

EXAMPLES:

  Parse requirements file:
    intent ears requirements.md

  Convert to CUE spec:
    intent ears requirements.md --output cue --out api.cue

  Export as JSON:
    intent ears requirements.md --output json

  Full workflow:
    intent ears requirements.md --output cue --out api.cue
    intent validate api.cue
    intent check api.cue --target http://localhost:8080

INTERPRETING RESULTS:

  Text Output:
    Shows counts by pattern type and any parsing errors.

  CUE Output:
    Generated spec with behaviors extracted from requirements.

  JSON Output:
    Structured data for programmatic consumption.
"""
}

/// Parse requirements to spec help text
pub fn parse_long_help() -> String {
  """
Parse EARS requirements to structured spec

What it does:
  Full-pipeline requirement parsing: EARS format → Intent CUE spec.

Why you'd use it:
  Automate requirements-to-tests workflow. Reduce manual transcription errors.

When to use it:
  • Converting legacy requirements documents to specs
  • CI/CD pipeline: requirements commit → test generation
  • During product discovery to lock in behavioral contracts

EXAMPLES:

  Convert requirements to spec:
    intent parse requirements.md -o api.cue

  Validate requirements are well-formed:
    intent parse requirements.md

  JSON output for downstream tools:
    intent parse requirements.md --json

  Full workflow:
    intent parse requirements.md -o api.cue
    intent validate api.cue
    intent check api.cue --target http://localhost:8080

INTERPRETING RESULTS:

  Text Output:
    Shows parsing progress with counts by pattern type.

  CUE Output:
    Generated spec file ready for validation and testing.

  JSON Output:
    Structured results: requirements, behaviors, errors, warnings.
"""
}

// =============================================================================
// FLAG HELPER FUNCTIONS
// =============================================================================

/// Output format flag (text, cue, json)
pub fn flag_output_format_flag() -> glint.flag.FlagBuilder(String) {
  glint.flag.string()
    |> glint.flag.default("text")
    |> glint.flag.description("Output format: text, cue, json")
}

/// Output file path flag
pub fn flag_output_file_flag() -> glint.flag.FlagBuilder(String) {
  glint.flag.string()
    |> glint.flag.default("")
    |> glint.flag.description("Output file path")
}

/// Spec name for CUE output flag
pub fn flag_spec_name_flag() -> glint.flag.FlagBuilder(String) {
  glint.flag.string()
    |> glint.flag.default("GeneratedSpec")
    |> glint.flag.description("Spec name for CUE output")
}

// =============================================================================
// INTEGRATION INSTRUCTIONS
// =============================================================================

// To integrate into src/intent.gleam:
//
// 1. Copy all help text functions (quality_long_help, invert_long_help, etc.)
//    from KIRK_HELP_TEXT_IMPLEMENTATION.gleam
//
// 2. For each of the 7 commands, add |> glint.long_help(...) between
//    |> glint.description(...) and |> glint.flag(...)
//
//    Before:
//      fn kirk_quality_command() -> glint.Command(Nil) {
//        glint.command(...)
//        |> glint.description(...)
//        |> glint.flag(...)
//      }
//
//    After:
//      fn kirk_quality_command() -> glint.Command(Nil) {
//        glint.command(...)
//        |> glint.description(...)
//        |> glint.long_help(quality_long_help())
//        |> glint.flag(...)
//      }
//
// 3. For EARS and PARSE commands, replace flag definitions with helpers:
//
//    Before:
//      |> glint.flag("output", flag.string() |> flag.default("text") |> ...)
//
//    After:
//      |> glint.flag("output", flag_output_format_flag())
//
// 4. Build and test:
//    gleam build && gleam test
//
// 5. Verify help text displays:
//    gleam run -- quality --help
//    gleam run -- invert --help
//    ... (repeat for all 7 commands)
