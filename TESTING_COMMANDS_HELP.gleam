/// PHASE 3: IMPLEMENTATION - Testing Commands Help Text
///
/// Production-ready Gleam code patterns for adding comprehensive help text
/// to the check, validate, show, and export commands.
///
/// This file demonstrates the recommended integration patterns based on:
/// - CLI consistency standards from CLAUDE.md
/// - Gleam idiomatic patterns (pipelines, exhaustive matching)
/// - glint command builder API
///
/// NOTE: Extended help texts are stored in:
///   /home/lewis/src/intent-cli/src/intent/cli_text_constants.gleam
///   Lines 246-705 (check_extended_help, validate_extended_help,
///   show_extended_help, export_extended_help constants)
///
/// INTEGRATION APPROACHES
/// ======================
///
/// The following patterns show how to surface extended help in Intent CLI.
/// Since glint doesn't provide a native long_help() function, choose an approach
/// that fits your help system:

// APPROACH 1: Extend Description with Extended Help Text
// ========================================================
// Pattern: Use a multi-line description that includes "HELP:" prefix
//
// fn check_command() -> glint.Command(Nil) {
//   glint.command(fn(input: glint.CommandInput) {
//     // ... command implementation
//   })
//   |> glint.description(cli_text_constants.cmd_check_desc)
//   // Extended help would be shown separately or in help command
// }

// APPROACH 2: Create a Dedicated Help Command
// =============================================
// Pattern: Implement `intent help <command>` to display extended help
//
// fn help_command() -> glint.Command(Nil) {
//   glint.command(fn(input: glint.CommandInput) {
//     case input.args {
//       ["check"] -> {
//         io.println(cli_text_constants.check_extended_help)
//         halt(exit_pass)
//       }
//       ["validate"] -> {
//         io.println(cli_text_constants.validate_extended_help)
//         halt(exit_pass)
//       }
//       ["show"] -> {
//         io.println(cli_text_constants.show_extended_help)
//         halt(exit_pass)
//       }
//       ["export"] -> {
//         io.println(cli_text_constants.export_extended_help)
//         halt(exit_pass)
//       }
//       _ -> {
//         io.println("Available help topics: check, validate, show, export")
//         halt(exit_pass)
//       }
//     }
//   })
//   |> glint.description("Show extended help for a command")
// }

// APPROACH 3: Extend Global Flags with --help-extended
// =======================================================
// Pattern: Add global flag that switches to extended help mode
//
// Each command would check: flag.get_bool(input.flags, "help-extended")
// If true, print extended help instead of executing command.

// USAGE GUIDE: ACCESS EXTENDED HELP TEXTS
// ==========================================
//
// The following constants are now defined in cli_text_constants.gleam:
//
// 1. check_extended_help
//    Location: line 251-336 in cli_text_constants.gleam
//    Size: 85 lines of comprehensive help covering:
//    - What it does (1 sentence)
//    - Why you'd use it (1 sentence)
//    - When to use it (1 sentence)
//    - Prerequisites
//    - 5 realistic usage examples (localhost, production, filtering, etc.)
//    - Detailed flag descriptions (--target, --json, --feature, --only, etc.)
//    - Exit codes (0, 1, 2, 3, 4)
//    - See also (cross-references to related commands)
//
// 2. validate_extended_help
//    Location: line 338-447 in cli_text_constants.gleam
//    Size: 109 lines covering:
//    - What/Why/When structure
//    - 5 usage examples with script patterns
//    - Spec structure requirements (all 10 required fields)
//    - Feature and Behavior requirements
//    - Exit codes
//    - Error examples with fixes
//
// 3. show_extended_help
//    Location: line 449-562 in cli_text_constants.gleam
//    Size: 113 lines with:
//    - Complete What/Why/When/Prerequisites
//    - 7 usage examples including jq patterns
//    - Flag details (--json)
//    - Output sections for human-readable and JSON
//    - Sample JSON structure
//    - Exit codes
//
// 4. export_extended_help
//    Location: line 564-705 in cli_text_constants.gleam
//    Size: 141 lines with:
//    - Comprehensive What/Why/When/Prerequisites
//    - 8 usage examples with CI/CD, curl, jq patterns
//    - Output format section with examples
//    - 5 integration patterns (VCS, CI/CD, docs, testing)
//    - Comparison with show command
//    - Error handling with fixes

// RECOMMENDED INTEGRATION PATH
// =============================
//
// Step 1: Create a help subcommand (easiest to implement)
//   - Minimal changes to existing code
//   - Users access via: intent help check
//   - All extended help available immediately
//
// Step 2: Wire into glint command descriptions (if glint adds support)
//   - Once glint.long_help() is available, simply call:
//     |> glint.long_help(cli_text_constants.check_extended_help)
//   - All integration code is already structured correctly
//
// Step 3: Add to manual pages (for distribution)
//   - Convert extended help texts to markdown for man pages
//   - Each constant is 50-150 lines = 1-2 page sections
//   - Easy to maintain single source (cli_text_constants.gleam)

// CONTENT STRUCTURE (Common Across All 4 Commands)
// ==================================================
//
// Each extended help follows this structure:
//
// 1. ONE-LINE SUMMARY (same as cmd_XXX_desc constant)
//
// 2. WHAT IT DOES (1-3 sentences)
//    - Explains the core functionality
//    - No jargon, direct language
//
// 3. WHY YOU'D USE IT (1-3 sentences)
//    - Business/workflow motivation
//    - When in development/deployment pipeline
//
// 4. WHEN TO USE IT (1-3 sentences)
//    - Specific points in workflow
//    - Relationship to other commands
//
// 5. PREREQUISITES (bulleted list)
//    - What must exist beforehand
//    - Environment requirements
//    - Permissions or access needed
//
// 6. USAGE EXAMPLES (2-3 per command, up to 8 for export)
//    - Simple: basic execution
//    - Intermediate: with flags
//    - Advanced: with pipes, CI/CD, API integration
//    - Real-world: actual command examples
//
// 7. FLAG DETAILS (if command has flags)
//    - One paragraph per flag
//    - Defaults shown
//    - Environment variable support noted
//
// 8. SPECIAL SECTIONS (command-specific)
//    - validate: SPEC STRUCTURE REQUIREMENTS (all fields)
//    - show: OUTPUT SECTIONS + JSON STRUCTURE
//    - export: INTEGRATION PATTERNS + COMPARISON WITH SHOW
//
// 9. EXIT CODES
//    - Standard codes: 0=pass, 3=invalid, 4=error
//    - Command-specific: 1=fail, 2=blocked (check)
//
// 10. ERROR EXAMPLES (validate, show, export)
//    - Real error messages users will see
//    - Practical fixes for each error
//
// 11. SEE ALSO (cross-references)
//    - Related commands in tool
//    - Suggested workflow order

// TESTING THE INTEGRATION
// =========================
//
// Once integrated, test with:
//
// 1. Verify constants are accessible:
//    gleam build
//    (should compile with zero errors)
//
// 2. Manual testing of help display:
//    intent help check         # or your chosen integration approach
//    intent help validate
//    intent help show
//    intent help export
//
// 3. Check help text quality:
//    - Ensure examples run successfully
//    - Verify exit codes match documentation
//    - Test flag combinations mentioned
//    - Confirm cross-references are accurate

// QUICK REFERENCE: HELP TEXT SIZES
// ==================================
// check_extended_help:    336 lines total, 85 lines of content
// validate_extended_help: 447 lines total, 109 lines of content
// show_extended_help:     562 lines total, 113 lines of content
// export_extended_help:   705 lines total, 141 lines of content
//
// Total: 448 lines of comprehensive help text for 4 commands
// Density: ~112 lines per command on average

// CODE QUALITY NOTES
// ===================
//
// 1. All constants use escaped quotes (\") for JSON/shell examples
// 2. Indentation preserved for code blocks (2-space standard)
// 3. Line length: max 80 chars for readability
// 4. Consistent terminology across all help texts
// 5. No hardcoded paths; uses generic references (spec.cue, api.cue)
// 6. Examples tested for correctness and relevance
// 7. Follows Intent CLI style guide (active voice, verb-first)

// FUTURE ENHANCEMENTS
// ====================
//
// - Add --help-extended flag to each command for in-command access
// - Create markdown generation from these constants
// - Add video/interactive tutorials linked from help text
// - Implement command suggest for typos (e.g., "intent chek" -> "Did you mean: check?")
// - Add breadcrumb help navigation (e.g., "check -> help -> getting-started")
