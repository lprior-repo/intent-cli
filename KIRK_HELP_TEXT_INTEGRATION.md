# KIRK Help Text Integration Guide

This guide explains how to integrate the comprehensive help text and examples for KIRK analysis commands (quality, invert, coverage, gaps, effects, ears, parse) into `src/intent.gleam`.

## Overview

Seven KIRK commands need enhanced help text following the pattern:
```gleam
|> glint.description(...)          // Short 50-100 char description
|> glint.long_help("""...""")       // Extended help with What/Why/When + Examples
|> glint.flag(...)                 // Flag definitions
```

## Implementation Details

### File Structure

The production-ready code is in `KIRK_HELP_TEXT_IMPLEMENTATION.gleam`:

```
/home/lewis/src/intent-cli/KIRK_HELP_TEXT_IMPLEMENTATION.gleam
├── quality_long_help()
├── invert_long_help()
├── coverage_long_help()
├── gaps_long_help()
├── effects_long_help()
├── ears_long_help()
├── parse_long_help()
└── Flag helpers (3 functions)
```

### Integration Strategy

**Option A: Inline (Recommended)**
Copy help text functions directly into `src/intent.gleam` near the command definitions.

**Option B: Separate Module**
Create `src/intent/kirk_help_text.gleam` with help functions, then import.

**Option C: Constants in CLI Text Module**
Add to `src/intent/cli_text_constants.gleam` for centralization.

## Step-by-Step Integration

### 1. Copy Help Text Functions

Add these functions to `src/intent.gleam` (or separate module):

```gleam
// Copy from KIRK_HELP_TEXT_IMPLEMENTATION.gleam
// quality_long_help() - 150 lines
// invert_long_help() - 140 lines
// coverage_long_help() - 160 lines
// gaps_long_help() - 180 lines
// effects_long_help() - 160 lines
// ears_long_help() - 170 lines
// parse_long_help() - 180 lines
```

### 2. Update Command Definitions

For each of the 7 commands, add `|> glint.long_help(...)` between `|> glint.description(...)` and `|> glint.flag(...)`.

#### Quality Command (Line ~2832)

**Before:**
```gleam
fn kirk_quality_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_quality_desc)
  |> glint.flag("json", cli_flags.json_flag())
}
```

**After:**
```gleam
fn kirk_quality_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_quality_desc)
  |> glint.long_help(quality_long_help())
  |> glint.flag("json", cli_flags.json_flag())
}
```

#### Invert Command (Line ~2898)

**Before:**
```gleam
fn kirk_invert_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_invert_desc)
  |> glint.flag("json", cli_flags.json_flag())
}
```

**After:**
```gleam
fn kirk_invert_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_invert_desc)
  |> glint.long_help(invert_long_help())
  |> glint.flag("json", cli_flags.json_flag())
}
```

#### Coverage Command (Line ~2974)

**Before:**
```gleam
fn kirk_coverage_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_coverage_desc)
  |> glint.flag("json", cli_flags.json_flag())
}
```

**After:**
```gleam
fn kirk_coverage_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_coverage_desc)
  |> glint.long_help(coverage_long_help())
  |> glint.flag("json", cli_flags.json_flag())
}
```

#### Gaps Command (Line ~3049)

**Before:**
```gleam
fn kirk_gaps_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_gaps_desc)
  |> glint.flag("json", cli_flags.json_flag())
}
```

**After:**
```gleam
fn kirk_gaps_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_gaps_desc)
  |> glint.long_help(gaps_long_help())
  |> glint.flag("json", cli_flags.json_flag())
}
```

#### Effects Command (Line ~3107)

**Before:**
```gleam
fn kirk_effects_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_effects_desc)
  |> glint.flag("json", cli_flags.json_flag())
}
```

**After:**
```gleam
fn kirk_effects_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_effects_desc)
  |> glint.long_help(effects_long_help())
  |> glint.flag("json", cli_flags.json_flag())
}
```

#### EARS Command (Line ~3241)

**Before:**
```gleam
fn kirk_ears_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_ears_desc)
  |> glint.flag(
    "output",
    flag.string()
      |> flag.default("text")
      |> flag.description("Output format: text, cue, json"),
  )
  |> glint.flag(
    "out",
    flag.string() |> flag.default("") |> flag.description("Output file path"),
  )
  |> glint.flag(
    "name",
    flag.string()
      |> flag.default("GeneratedSpec")
      |> flag.description("Spec name for CUE output"),
  )
}
```

**After:**
```gleam
fn kirk_ears_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_ears_desc)
  |> glint.long_help(ears_long_help())
  |> glint.flag("output", flag_output_format_flag())
  |> glint.flag("out", flag_output_file_flag())
  |> glint.flag("name", flag_spec_name_flag())
}
```

#### Parse Command (Line ~3505)

**Before:**
```gleam
fn parse_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_parse_desc)
  |> glint.flag(
    "o",
    flag.string()
      |> flag.default("")
      |> flag.description("Output spec file path"),
  )
  |> glint.flag("json", cli_flags.json_flag())
}
```

**After:**
```gleam
fn parse_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    // ... implementation ...
  })
  |> glint.description(cli_text_constants.cmd_parse_desc)
  |> glint.long_help(parse_long_help())
  |> glint.flag("o", flag_output_file_flag())
  |> glint.flag("json", cli_flags.json_flag())
}
```

### 3. Add Flag Helper Functions

The EARS and PARSE commands benefit from refactored flag definitions. Add to `src/intent.gleam`:

```gleam
fn flag_output_format_flag() -> glint.flag.FlagBuilder(String) {
  glint.flag.string()
    |> glint.flag.default("text")
    |> glint.flag.description("Output format: text, cue, json")
}

fn flag_output_file_flag() -> glint.flag.FlagBuilder(String) {
  glint.flag.string()
    |> glint.flag.default("")
    |> glint.flag.description("Output file path")
}

fn flag_spec_name_flag() -> glint.flag.FlagBuilder(String) {
  glint.flag.string()
    |> glint.flag.default("GeneratedSpec")
    |> glint.flag.description("Spec name for CUE output")
}
```

## Help Text Structure

Each `*_long_help()` function follows this format:

### Header
```
KIRK: [Analysis Type] [One-line description]
```

### Sections

1. **What it does** (1 sentence)
   - Clear, concrete description of analysis

2. **Why you'd use it** (1 sentence)
   - Business/development value

3. **When to use it** (3-4 bullets)
   - Specific workflow points

4. **Mental Model** (1-2 paragraphs)
   - Underlying thinking framework
   - Reference to CLAUDE.md where applicable

5. **EXAMPLES** (2-4 real-world usage)
   - Basic usage
   - Advanced usage with flags
   - Integration with other commands

6. **INTERPRETING RESULTS** (variable by command)
   - Output explanation
   - Score ranges and interpretation
   - Common patterns

7. **ADVANCED USAGE** (2-4 patterns)
   - CI/CD integration
   - Scripting patterns
   - Cross-command workflows

8. **Error Handling** or **Best Practices** (command-specific)

## Testing Integration

After integration, test help text display:

```bash
# Build first
gleam build

# Test individual help text
gleam run -- quality --help
gleam run -- invert --help
gleam run -- coverage --help
gleam run -- gaps --help
gleam run -- effects --help
gleam run -- ears --help
gleam run -- parse --help

# Verify long_help renders in terminal
# Should show full extended help text
```

## Validation Checklist

- [ ] All 7 commands have `|> glint.long_help(...)` added
- [ ] Help text functions are defined and accessible
- [ ] Flag helpers refactored for consistency
- [ ] All flag descriptions match cli_text_constants patterns
- [ ] Long help text renders without truncation (test in terminal)
- [ ] Examples are accurate and runnable
- [ ] Mental model references to CLAUDE.md align
- [ ] No hardcoded emoji in help text (use emoji_constants where appropriate)
- [ ] JSON output examples are valid (test with `jq`)
- [ ] Cross-command workflows documented
- [ ] Build succeeds: `gleam build && gleam test`

## Performance Considerations

- Help text strings are ~150-180 lines each
- Total size: ~1100 lines of string literals
- No runtime cost (strings allocated once at startup)
- Can be extracted to separate module if src/intent.gleam grows too large

## Documentation Alignment

Help text references to mental models align with:
- `CLAUDE.md`: 5-Round Mental Model System
- `CLAUDE.md`: KIRK module descriptions
- Existing command help in cli_text_constants.gleam
- Intent CLI design patterns (Contract-driven API testing)

## Future Enhancements

1. **Localization**: Extract help strings to translation module
2. **Interactive Examples**: Shell script snippets in help text
3. **Links**: Reference to full KIRK documentation
4. **Video**: Embed demo video links in help
5. **Accessibility**: ARIA labels, high-contrast output options

## Integration Completion

When complete:
- `gleam build` passes without errors
- `gleam test` passes all tests
- `gleam run -- <cmd> --help` shows extended help text
- All 7 KIRK commands have comprehensive, consistent help
- Ready for user documentation and onboarding
