# KIRK Help Text Implementation Checklist

## Files Provided

- [x] **KIRK_HELP_TEXT_IMPLEMENTATION.gleam** (1,100+ lines)
  - Production-ready code for all 7 help functions + 3 flag helpers
  - Ready to copy into src/intent.gleam or separate module

- [x] **KIRK_HELP_TEXT_INTEGRATION.md**
  - Step-by-step integration guide with before/after code
  - Line numbers for each command in src/intent.gleam
  - Flag refactoring patterns

- [x] **KIRK_HELP_TEXT_SUMMARY.md**
  - Overview with mental models explained
  - Output examples for each command
  - Cross-command workflow documentation

- [x] **KIRK_HELP_TEXT_EXAMPLE.gleam**
  - Actual Gleam code showing integration pattern
  - All 7 help functions (abridged versions for clarity)
  - Flag helpers ready to use

- [x] **KIRK_HELP_TEXT_CHECKLIST.md** (this file)
  - Quick reference and implementation checklist

---

## Quick Reference: Commands

| # | Command | File Lines | Focus | Time |
|---|---------|-----------|-------|------|
| 1 | `quality` | 2774-2834 | 4D scoring | 15 min |
| 2 | `invert` | 2836-2900 | Failure modes | 15 min |
| 3 | `coverage` | 2914-2976 | Breadth (methods, codes, OWASP) | 15 min |
| 4 | `gaps` | 2978-3051 | 5-round mental model gaps | 15 min |
| 5 | `effects` | 3063-3109 | Cascading consequences | 15 min |
| 6 | `ears` | 3122-3258 | EARS pattern parsing | 20 min |
| 7 | `parse` | 3264-3513 | Requirements → Spec pipeline | 20 min |

**Total Integration Time: ~100 minutes (~2 hours)**

---

## Step-by-Step Integration

### Phase 1: Setup (5 min)

- [ ] Read KIRK_HELP_TEXT_INTEGRATION.md (overview)
- [ ] Open src/intent.gleam in editor
- [ ] Have KIRK_HELP_TEXT_IMPLEMENTATION.gleam open for reference

### Phase 2: Copy Help Functions (10 min)

- [ ] Copy `quality_long_help()` (150 lines)
- [ ] Copy `invert_long_help()` (140 lines)
- [ ] Copy `coverage_long_help()` (160 lines)
- [ ] Copy `gaps_long_help()` (180 lines)
- [ ] Copy `effects_long_help()` (160 lines)
- [ ] Copy `ears_long_help()` (170 lines)
- [ ] Copy `parse_long_help()` (180 lines)
- [ ] Copy flag helper functions (3 functions, ~20 lines total)

**Place in src/intent.gleam**: Near end of file before `halt()` definition (around line 3530)

### Phase 3: Update Quality Command (5 min)

Location: Line ~2832

**Change:**
```diff
  fn kirk_quality_command() -> glint.Command(Nil) {
    glint.command(fn(input: glint.CommandInput) {
      // ... implementation ...
    })
    |> glint.description(cli_text_constants.cmd_quality_desc)
+   |> glint.long_help(quality_long_help())
    |> glint.flag("json", cli_flags.json_flag())
  }
```

- [ ] Add `|> glint.long_help(quality_long_help())` line
- [ ] Save file

### Phase 4: Update Invert Command (5 min)

Location: Line ~2898

**Change:**
```diff
  fn kirk_invert_command() -> glint.Command(Nil) {
    glint.command(fn(input: glint.CommandInput) {
      // ... implementation ...
    })
    |> glint.description(cli_text_constants.cmd_invert_desc)
+   |> glint.long_help(invert_long_help())
    |> glint.flag("json", cli_flags.json_flag())
  }
```

- [ ] Add `|> glint.long_help(invert_long_help())` line
- [ ] Save file

### Phase 5: Update Coverage Command (5 min)

Location: Line ~2974

**Change:**
```diff
  fn kirk_coverage_command() -> glint.Command(Nil) {
    glint.command(fn(input: glint.CommandInput) {
      // ... implementation ...
    })
    |> glint.description(cli_text_constants.cmd_coverage_desc)
+   |> glint.long_help(coverage_long_help())
    |> glint.flag("json", cli_flags.json_flag())
  }
```

- [ ] Add `|> glint.long_help(coverage_long_help())` line
- [ ] Save file

### Phase 6: Update Gaps Command (5 min)

Location: Line ~3049

**Change:**
```diff
  fn kirk_gaps_command() -> glint.Command(Nil) {
    glint.command(fn(input: glint.CommandInput) {
      // ... implementation ...
    })
    |> glint.description(cli_text_constants.cmd_gaps_desc)
+   |> glint.long_help(gaps_long_help())
    |> glint.flag("json", cli_flags.json_flag())
  }
```

- [ ] Add `|> glint.long_help(gaps_long_help())` line
- [ ] Save file

### Phase 7: Update Effects Command (5 min)

Location: Line ~3107

**Change:**
```diff
  fn kirk_effects_command() -> glint.Command(Nil) {
    glint.command(fn(input: glint.CommandInput) {
      // ... implementation ...
    })
    |> glint.description(cli_text_constants.cmd_effects_desc)
+   |> glint.long_help(effects_long_help())
    |> glint.flag("json", cli_flags.json_flag())
  }
```

- [ ] Add `|> glint.long_help(effects_long_help())` line
- [ ] Save file

### Phase 8: Update EARS Command (10 min)

Location: Line ~3241

**Change:**
```diff
  fn kirk_ears_command() -> glint.Command(Nil) {
    glint.command(fn(input: glint.CommandInput) {
      // ... implementation ...
    })
    |> glint.description(cli_text_constants.cmd_ears_desc)
+   |> glint.long_help(ears_long_help())
-   |> glint.flag(
-     "output",
-     flag.string()
-       |> flag.default("text")
-       |> flag.description("Output format: text, cue, json"),
-   )
+   |> glint.flag("output", flag_output_format_flag())
-   |> glint.flag(
-     "out",
-     flag.string() |> flag.default("") |> flag.description("Output file path"),
-   )
+   |> glint.flag("out", flag_output_file_flag())
-   |> glint.flag(
-     "name",
-     flag.string()
-       |> flag.default("GeneratedSpec")
-       |> flag.description("Spec name for CUE output"),
-   )
+   |> glint.flag("name", flag_spec_name_flag())
  }
```

- [ ] Add `|> glint.long_help(ears_long_help())` line
- [ ] Replace flag "output" definition with `flag_output_format_flag()`
- [ ] Replace flag "out" definition with `flag_output_file_flag()`
- [ ] Replace flag "name" definition with `flag_spec_name_flag()`
- [ ] Save file

### Phase 9: Update Parse Command (10 min)

Location: Line ~3505

**Change:**
```diff
  fn parse_command() -> glint.Command(Nil) {
    glint.command(fn(input: glint.CommandInput) {
      // ... implementation ...
    })
    |> glint.description(cli_text_constants.cmd_parse_desc)
+   |> glint.long_help(parse_long_help())
-   |> glint.flag(
-     "o",
-     flag.string()
-       |> flag.default("")
-       |> flag.description("Output spec file path"),
-   )
+   |> glint.flag("o", flag_output_file_flag())
    |> glint.flag("json", cli_flags.json_flag())
  }
```

- [ ] Add `|> glint.long_help(parse_long_help())` line
- [ ] Replace flag "o" definition with `flag_output_file_flag()`
- [ ] Save file

### Phase 10: Compile & Test (15 min)

- [ ] Run `gleam build` (should complete without errors)
- [ ] Run `gleam test` (should pass all tests)
- [ ] If errors occur, check line numbers match current version
- [ ] Adjust line numbers if src/intent.gleam has changed

### Phase 11: Verify Help Text Display (15 min)

For each command, run and verify help text displays:

```bash
gleam run -- quality --help
gleam run -- invert --help
gleam run -- coverage --help
gleam run -- gaps --help
gleam run -- effects --help
gleam run -- ears --help
gleam run -- parse --help
```

For each command:
- [ ] Help text displays without truncation
- [ ] Text wraps at terminal width
- [ ] Examples are clearly formatted
- [ ] Mental model section is readable
- [ ] INTERPRETING RESULTS section is clear

### Phase 12: Validate Examples (20 min)

Test sample examples from help text:

```bash
# Quality
gleam run -- quality examples/user-api.cue
gleam run -- quality examples/user-api.cue --json | jq . > /dev/null

# Invert
gleam run -- invert examples/user-api.cue
gleam run -- invert examples/user-api.cue --json | jq . > /dev/null

# Coverage
gleam run -- coverage examples/user-api.cue
gleam run -- coverage examples/user-api.cue --json | jq . > /dev/null

# Gaps
gleam run -- gaps examples/user-api.cue
gleam run -- gaps examples/user-api.cue --json | jq . > /dev/null

# Effects
gleam run -- effects examples/user-api.cue
gleam run -- effects examples/user-api.cue --json | jq . > /dev/null
```

For each:
- [ ] Command runs without error
- [ ] JSON output is valid (jq parse succeeds)
- [ ] Output matches expected format from help text

### Phase 13: Commit Changes (5 min)

```bash
git add src/intent.gleam
git commit -m "feat: Add comprehensive help text for all 7 KIRK analysis commands

- quality: 4D scoring analysis with completeness, consistency, testability, clarity, security
- invert: Failure mode analysis with security, usability, integration gap detection
- coverage: HTTP method and status code breadth with OWASP Top 10 coverage
- gaps: 5-round mental model gap detection (inversion, 2nd-order, checklist, coverage, security)
- effects: Cascading consequence tracing and dependency chain analysis
- ears: EARS requirement format parsing (ubiquitous, event, state, optional, unwanted, complex)
- parse: Full requirements-to-spec pipeline with JSON output

Includes:
- Extended help text (150-180 lines per command)
- Realistic usage examples with JSON output
- Mental model explanations with CLAUDE.md references
- Cross-command workflow documentation
- INTERPRETING RESULTS sections for output analysis
- Refactored flag helpers for consistency

All examples tested and verified."
```

- [ ] Git commit message complete
- [ ] All changes staged with `git add`
- [ ] Commit created successfully

---

## Validation Checklist

### Code Quality
- [ ] `gleam build` completes without errors
- [ ] `gleam test` passes all tests
- [ ] No compiler warnings
- [ ] Gleam formatting is consistent (run `gleam format`)

### Help Text Display
- [ ] All 7 commands have `--help` implemented
- [ ] Help text renders without truncation
- [ ] Text wraps at 80-char terminal width (approximately)
- [ ] No hardcoded emoji (use emoji_constants)
- [ ] Links/references use proper markdown format

### Examples Accuracy
- [ ] Examples are syntactically correct
- [ ] Examples are runnable (no fictional flags)
- [ ] JSON output examples are valid
- [ ] Flags match actual command implementation
- [ ] File paths in examples exist or are realistic placeholders

### Documentation Alignment
- [ ] Help text references CLAUDE.md where appropriate
- [ ] Mental models match documented systems
- [ ] Cross-command workflows documented
- [ ] Error handling sections are accurate
- [ ] Best practices align with Intent CLI philosophy

### User Experience
- [ ] Help text is clear and jargon-minimal
- [ ] Examples progress from basic to advanced
- [ ] Output interpretation sections are helpful
- [ ] Cross-command integration workflows shown
- [ ] Mental models explained before use

---

## Troubleshooting

### Issue: "unknown label 'long_help'"

**Cause**: glint library doesn't support `long_help()` in current version

**Solution**:
1. Check glint version in gleam.toml
2. Update glint if needed: `gleam deps upgrade glint`
3. Verify glint documentation for equivalent function

### Issue: Compiler errors on help text strings

**Cause**: Malformed triple-quoted strings (missing closing `"""`)

**Solution**:
1. Verify all help functions end with `""")`
2. Check for unescaped quotes in strings
3. Use raw strings if needed: `r#"..."#`

### Issue: Help text truncates in terminal

**Cause**: Terminal width too narrow or help text too long

**Solution**:
1. Resize terminal to 120+ chars width
2. Pipe output to pager: `gleam run -- <cmd> --help | less`
3. Review help text for excessive line length
4. Consider breaking into sections with headers

### Issue: Examples don't run

**Cause**: Example files don't exist or flags incorrect

**Solution**:
1. Verify example files exist: `ls examples/`
2. Check actual command flags: `gleam run -- <cmd> --help`
3. Update examples to match actual implementation
4. Use realistic file paths (user will substitute)

### Issue: Cross-command workflows fail

**Cause**: Pipe output format changed or flag compatibility

**Solution**:
1. Test workflow manually before documenting
2. Verify output format matches next command input
3. Add `--json` flag where needed for parsing
4. Document any intermediate processing steps

---

## Performance Notes

- Help text is allocated once at CLI startup
- Total additional code: ~1,100 lines
- No runtime performance impact
- Improves user experience significantly

---

## Success Criteria

After integration, verify:

1. **Functionality**: All commands run with `--help` flag
2. **Content**: Help text displays complete (all 7 commands)
3. **Quality**: Examples are runnable and accurate
4. **Build**: `gleam build && gleam test` succeeds
5. **User Experience**: Help text is clear and helpful
6. **Documentation**: Aligned with CLAUDE.md and existing standards

---

## Post-Integration Tasks (Optional)

- [ ] Add help text to online documentation
- [ ] Create tutorial using help examples
- [ ] Generate help text PDF for offline reference
- [ ] Add help text to GitHub wiki/docs
- [ ] Create video tutorials showing help text + examples
- [ ] Localize help text to other languages
- [ ] Auto-generate man pages from help text

---

## File Summary

All files are in `/home/lewis/src/intent-cli/`:

| File | Lines | Purpose |
|------|-------|---------|
| KIRK_HELP_TEXT_IMPLEMENTATION.gleam | 1,100+ | Production code (7 functions + 3 helpers) |
| KIRK_HELP_TEXT_INTEGRATION.md | 300+ | Step-by-step integration guide |
| KIRK_HELP_TEXT_SUMMARY.md | 400+ | Overview, patterns, mental models |
| KIRK_HELP_TEXT_EXAMPLE.gleam | 350+ | Actual Gleam code patterns |
| KIRK_HELP_TEXT_CHECKLIST.md | 500+ | Implementation checklist (this file) |

**Total**: ~2,600 lines of documentation + code

---

## Quick Start (TL;DR)

1. Copy help functions from `KIRK_HELP_TEXT_IMPLEMENTATION.gleam`
2. Add `|> glint.long_help(<cmd>_long_help())` to 7 command definitions
3. Refactor EARS/PARSE flags with 3 helper functions
4. Run `gleam build && gleam test`
5. Verify: `gleam run -- quality --help` (and 6 others)
6. Commit with descriptive message

**Estimated Time**: 1.5-2 hours

---

## Questions?

Refer to:
- **Integration Guide**: KIRK_HELP_TEXT_INTEGRATION.md (step-by-step)
- **Code Patterns**: KIRK_HELP_TEXT_EXAMPLE.gleam (actual Gleam code)
- **Overview**: KIRK_HELP_TEXT_SUMMARY.md (mental models, outputs)
- **Implementation**: KIRK_HELP_TEXT_IMPLEMENTATION.gleam (complete code)
