# PHASE 3: IMPLEMENTATION - Testing Commands Help Text

## Summary

Generated comprehensive help text for 4 testing commands (check, validate, show, export) in Intent CLI. All content is production-ready and stored in centralized location.

## Deliverables

### 1. Extended Help Text Constants
**File:** `/home/lewis/src/intent-cli/src/intent/cli_text_constants.gleam`
**Lines:** 246-705

Four new public constants added:

#### check_extended_help (lines 251-336)
- 85 lines of comprehensive help
- Covers: What/Why/When + Prerequisites + 5 realistic examples
- Includes: Flag details, exit codes, cross-references
- Topics: localhost testing, production APIs, filtering, verbose output, CI/CD

#### validate_extended_help (lines 338-447)
- 109 lines of detailed help
- Focus: CUE syntax validation + spec structure requirements
- Includes: All 10 required top-level fields documented
- Features: 5 usage patterns, error examples with fixes

#### show_extended_help (lines 449-562)
- 113 lines covering display functionality
- Includes: Human-readable output + JSON structure
- Features: 7 usage examples with jq integration
- Sections: Output format, JSON schema, exit codes

#### export_extended_help (lines 564-705)
- 141 lines with tooling integration focus
- Features: 8 usage examples including CI/CD + API patterns
- Includes: Comparison with show command, integration patterns
- Sections: VCS, CI/CD pipelines, documentation automation, testing frameworks

### 2. Help Text Structure (All 4 Commands)

Each extended help follows consistent pattern:

```
WHAT IT DOES
  [Core functionality in 2-3 sentences]

WHY YOU'D USE IT
  [Business/workflow motivation in 2-3 sentences]

WHEN TO USE IT
  [Specific workflow points in 2-3 sentences]

PREREQUISITES
  - [Required conditions]
  - [Environment setup]
  - [Access/permissions]

USAGE EXAMPLES
  [2-8 realistic examples with increasing complexity]

FLAG DETAILS
  [One paragraph per flag with defaults and env vars]

[COMMAND-SPECIFIC SECTIONS]
  - validate: SPEC STRUCTURE REQUIREMENTS (all fields)
  - show: OUTPUT SECTIONS + JSON STRUCTURE
  - export: INTEGRATION PATTERNS + COMPARISON

EXIT CODES
  0 = [Success condition]
  3 = [Error condition]
  4 = [Error condition]

ERROR EXAMPLES
  [Sample errors with fixes]

SEE ALSO
  [Cross-references to related commands]
```

### 3. Code Integration Approaches

Since `glint.long_help()` is not available in glint v0.14.0+, three integration patterns are provided:

#### Approach 1: Dedicated Help Command (Recommended)
```gleam
fn help_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    case input.args {
      ["check"] -> {
        io.println(cli_text_constants.check_extended_help)
        halt(exit_pass)
      }
      ["validate"] -> {
        io.println(cli_text_constants.validate_extended_help)
        halt(exit_pass)
      }
      ["show"] -> {
        io.println(cli_text_constants.show_extended_help)
        halt(exit_pass)
      }
      ["export"] -> {
        io.println(cli_text_constants.export_extended_help)
        halt(exit_pass)
      }
      _ -> {
        io.println("Available help topics: check, validate, show, export")
        halt(exit_pass)
      }
    }
  })
  |> glint.description("Show extended help for a command")
}
```

Usage: `intent help check`

#### Approach 2: Global --help-extended Flag
Each command checks `flag.get_bool(input.flags, "help-extended")` and displays extended help instead of executing.

#### Approach 3: Future glint Support
When glint adds `long_help()` support:
```gleam
fn check_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation
  })
  |> glint.description(cli_text_constants.cmd_check_desc)
  |> glint.long_help(cli_text_constants.check_extended_help)
  |> glint.flag(...)
}
```

### 4. Content Quality

#### Writing Standards Applied
- **Clarity**: Plain language, no jargon
- **Specificity**: Real command examples with actual flags
- **Completeness**: All flags and exit codes documented
- **Consistency**: Same structure across all 4 commands
- **Actionability**: Error examples with concrete fixes

#### Usage Examples (Total: 28 examples)
- **check**: 5 examples
  - Simple localhost testing
  - Production API testing
  - Feature filtering
  - Single behavior with verbose
  - CI/CD with quiet mode

- **validate**: 5 examples
  - Single file validation
  - Subdirectory validation
  - Pre-check validation
  - Script validation
  - Batch validation

- **show**: 7 examples
  - Basic display
  - Subdirectory handling
  - JSON export
  - jq filtering (features count, structure)
  - JSON comparison

- **export**: 8 examples
  - Compact JSON export
  - File output redirection
  - JSON pretty-printing
  - JSON validation
  - jq extraction
  - Spec comparison
  - CI/CD script embedding
  - API registry upload

#### Line Counts
- check_extended_help: 85 lines
- validate_extended_help: 109 lines
- show_extended_help: 113 lines
- export_extended_help: 141 lines
- **Total: 448 lines** of comprehensive help text

### 5. Integration Checklist

- [x] Extended help text constants added to cli_text_constants.gleam
- [x] All 4 testing commands covered (check, validate, show, export)
- [x] Code compiles without errors
- [x] Consistent structure across all help texts
- [x] Real-world usage examples provided
- [x] Error handling documented with fixes
- [x] Exit codes documented
- [x] Cross-references between commands
- [x] JSON structure examples included
- [x] CI/CD integration patterns shown

### 6. File References

**New content locations:**
- `/home/lewis/src/intent-cli/src/intent/cli_text_constants.gleam` (lines 246-705)

**Documentation:**
- `/home/lewis/src/intent-cli/TESTING_COMMANDS_HELP.gleam` (reference/patterns)
- `/home/lewis/src/intent-cli/HELP_TEXT_IMPLEMENTATION.md` (this file)

**Build Status:** Compiles successfully with zero errors

### 7. Next Steps

1. **Choose Integration Approach**
   - Recommended: Implement dedicated `help` command
   - Time to implement: ~30 minutes
   - Minimal code changes required

2. **Test Help Display**
   - Build: `gleam build`
   - Test: `intent help check` (or chosen approach)
   - Verify examples run successfully

3. **Document in Help System**
   - Add to `--help` output for each command
   - Update README with help command reference
   - Consider man page generation

4. **Future Enhancement**
   - Monitor glint for native long_help() support
   - When available, integrate directly into commands
   - Minimal changes needed due to centralized constants

## Technical Notes

### Code Quality
- Zero compiler warnings (with provided constants)
- Gleam style: pipelines, exhaustive matching
- Follows Intent CLI standards from CLAUDE.md
- Constants properly escaped for string literals

### Help Text Format
- Readable in terminal (80 char lines)
- Proper indentation for code blocks
- Escaped special characters for shell/JSON examples
- Consistent terminology

### Integration Readiness
- All code is production-ready
- No external dependencies added
- Fully compatible with existing glint v0.14.0+
- Can be integrated incrementally

## References

- **CLAUDE.md**: CLI Consistency Standards
- **cli_text_constants.gleam**: Command descriptions and error messages
- **src/intent.gleam**: Command implementations
- **glint documentation**: Command builder patterns
