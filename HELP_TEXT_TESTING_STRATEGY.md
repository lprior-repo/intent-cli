# Intent CLI Help Text Testing Strategy
## PHASE 4: Automated Help Text Validation

**Date Created:** 2026-01-18
**Status:** Testing Framework Definition
**Scope:** All 24 Intent CLI commands

---

## Table of Contents

1. [Overview](#overview)
2. [Command Inventory](#command-inventory)
3. [Test Coverage Plan](#test-coverage-plan)
4. [Automated Test Scripts](#automated-test-scripts)
5. [Manual Test Plan](#manual-test-plan)
6. [Test Failure Criteria](#test-failure-criteria)
7. [Continuous Integration](#continuous-integration)

---

## Overview

This document defines a comprehensive testing strategy for validating help text across all 24 Intent CLI commands. The strategy ensures:

- **Consistency**: All commands follow the same help text structure
- **Completeness**: Each command has required sections (WHAT/WHY/WHEN/EXAMPLES/FLAGS/EXIT CODES)
- **Quality**: Help text is clear, accurate, and actionable
- **Usability**: Examples are working and demonstrate real use cases
- **Maintainability**: Help text updates are caught in CI before merge

### Help Text Standard

All command help text follows this structure:

```
WHAT IT DOES
  [2-3 sentences explaining what the command does and its purpose]

WHY YOU'D USE IT
  [2-3 sentences on when/why this command is valuable]

WHEN TO USE IT
  [2-3 sentences on typical usage scenarios and timing]

PREREQUISITES
  [Bulleted list of requirements before running]

USAGE EXAMPLES
  [At least 2 real-world examples, each with description]
  Example 1 command
  Example 2 command
  Example N command

FLAG DETAILS
  --flag-name DESCRIPTION
    Detailed explanation of what this flag does
    Additional context/constraints
    Example usage if applicable

EXIT CODES
  0 = Success
  1 = Validation/execution failure
  2 = Blocked/missing dependencies
  3 = Invalid spec or configuration
  4 = Runtime error

SEE ALSO
  intent other-command - Brief description
```

---

## Command Inventory

**Total: 24 Commands** (as of 2026-01-18)

### Core Testing Commands (4)
1. **check** - Execute spec tests against target URL and verify behaviors
2. **validate** - Validate CUE spec file syntax and structure
3. **show** - Display parsed spec with formatted output
4. **export** - Export spec to JSON format for external tools

### Quality Analysis Commands (4)
5. **lint** - Detect anti-patterns and quality issues in spec
6. **analyze** - Analyze spec quality across multiple dimensions
7. **improve** - Generate improvement suggestions from quality analysis
8. **doctor** - Generate health report with prioritized improvements

### Interview & Workflow Commands (5)
9. **interview** - Start guided specification discovery interview
10. **beads** - Generate work items (beads) from interview session
11. **bead-status** - Mark bead execution status (success/failed/blocked)
12. **history** - View snapshot history for interview session
13. **diff** - Compare two interview sessions and show differences
14. **sessions** - List all interview sessions with metadata

### KIRK Analysis Commands (6)
15. **quality** - KIRK: Analyze spec quality across coverage, clarity, testability
16. **invert** - KIRK: Identify missing failure cases through inversion analysis
17. **coverage** - KIRK: Analyze coverage including OWASP Top 10 and edge cases
18. **gaps** - KIRK: Detect specification gaps using mental models
19. **effects** - KIRK: Trace second-order effects and consequence chains
20. **ears** - KIRK: Parse EARS requirements into Intent behaviors

### Additional Commands (4)
21. **parse** - Parse EARS requirements to structured spec
22. **plan** - Display execution plan with waves and dependencies
23. **plan-approve** - Approve execution plan for session (CI/automation ready)
24. **beads-regenerate** - Regenerate failed/blocked beads with adjusted approach

---

## Test Coverage Plan

### 1. Functional Coverage

Each command must:

- [ ] Respond to `--help` flag
- [ ] Respond to `-h` flag (if supported)
- [ ] Exit with code 0 when help is displayed
- [ ] Output help text to stdout (not stderr)
- [ ] Help text does not truncate on standard terminal widths (80/120 chars)

### 2. Structural Validation

Help text must contain:

- [ ] **WHAT IT DOES** section
  - 2-3 sentences explaining purpose
  - Written in active voice
  - Starts with action verb
  - No more than 150 words

- [ ] **WHY YOU'D USE IT** section
  - Explains value proposition
  - Mentions typical use case
  - 50-100 words

- [ ] **WHEN TO USE IT** section
  - Describes timing/triggers
  - Mentions deployment contexts (dev/staging/prod if applicable)
  - 50-100 words

- [ ] **PREREQUISITES** section (if applicable)
  - Bulleted list of requirements
  - Includes file requirements, network access, etc.
  - Each item 1-2 lines

- [ ] **USAGE EXAMPLES** section
  - At least 2 real-world examples
  - Each example has 1-line description before command
  - Each example uses valid syntax
  - Examples progress from simple to complex
  - No template paths (use relative paths or documented placeholders)

- [ ] **FLAG DETAILS** section
  - Every flag documented
  - Descriptions explain what flag does, not just syntax
  - Constraints mentioned (required, optional, mutually exclusive)
  - Default values shown for optional flags
  - Environment variables noted if applicable

- [ ] **EXIT CODES** section
  - All possible exit codes documented
  - 0, 1, 2, 3, 4 mapped to outcomes
  - Each code has 1-line description

- [ ] **SEE ALSO** section
  - Lists 2-4 related commands
  - Each command has brief description
  - Commands are valid Intent CLI commands

### 3. Content Quality

- [ ] **Consistency**: Tone is conversational but technical
- [ ] **Clarity**: No jargon without explanation
- [ ] **Accuracy**: Examples match actual behavior
- [ ] **No typos**: Spell-check all text
- [ ] **Grammar**: Sentences are complete and grammatically correct
- [ ] **Formatting**: Consistent indentation and spacing

### 4. Example Validation

For each example in USAGE EXAMPLES:

- [ ] Syntax is valid Bash command
- [ ] All referenced files/flags exist
- [ ] No hardcoded absolute paths (use placeholders like `<spec>` or `--target <url>`)
- [ ] Example can reasonably execute (doesn't require impossible setup)
- [ ] Output would be useful/instructive
- [ ] Comments explain what example demonstrates

### 5. Flag Documentation Completeness

For each flag referenced in command:

- [ ] Flag name documented in FLAG DETAILS
- [ ] Flag short form (`-f`) documented if it exists
- [ ] Flag type documented (string, bool, int, etc.)
- [ ] Default value shown for optional flags
- [ ] Environment variable alternative shown if applicable
- [ ] Constraints documented (required, mutually exclusive, dependencies)
- [ ] Example usage shown if flag usage is non-obvious

---

## Automated Test Scripts

### 1. Basic Help Text Invocation Test

**File:** `/home/lewis/src/intent-cli/scripts/test-help-invocation.sh`

Tests that all commands respond to `--help` and return exit code 0.

```bash
#!/bin/bash
# Test: All commands respond to --help

COMMANDS=(
  "check" "validate" "show" "export"
  "lint" "analyze" "improve" "doctor"
  "interview" "beads" "bead-status" "history" "diff" "sessions"
  "quality" "invert" "coverage" "gaps" "effects" "ears"
  "parse" "plan" "plan-approve" "beads-regenerate"
)

PASS=0
FAIL=0

for cmd in "${COMMANDS[@]}"; do
  output=$(intent "$cmd" --help 2>&1)
  exit_code=$?

  if [ $exit_code -eq 0 ]; then
    echo "✓ $cmd --help returns exit code 0"
    ((PASS++))
  else
    echo "✗ $cmd --help returns exit code $exit_code (expected 0)"
    ((FAIL++))
  fi

  if [ -z "$output" ]; then
    echo "✗ $cmd --help produces no output"
    ((FAIL++))
  else
    echo "✓ $cmd --help produces output (${#output} bytes)"
    ((PASS++))
  fi
done

echo ""
echo "Summary: $PASS passed, $FAIL failed"
exit $([ $FAIL -eq 0 ] && echo 0 || echo 1)
```

### 2. Required Sections Validation Test

**File:** `/home/lewis/src/intent-cli/scripts/test-help-sections.sh`

Validates that help text contains required sections.

```bash
#!/bin/bash
# Test: All help text contains required sections

COMMANDS=(
  "check" "validate" "show" "export"
  "lint" "analyze" "improve" "doctor"
  "interview" "beads" "bead-status" "history" "diff" "sessions"
  "quality" "invert" "coverage" "gaps" "effects" "ears"
  "parse" "plan" "plan-approve" "beads-regenerate"
)

# Commands that don't need PREREQUISITES section
COMMANDS_NO_PREREQS=(
  "sessions" "history" "diff"
)

PASS=0
FAIL=0
WARNINGS=0

check_section() {
  local cmd=$1
  local section=$2
  local help_text=$3

  if echo "$help_text" | grep -q "^$section\$"; then
    echo "✓ $cmd has $section section"
    ((PASS++))
  else
    echo "✗ $cmd missing $section section"
    ((FAIL++))
    return 1
  fi
  return 0
}

for cmd in "${COMMANDS[@]}"; do
  help_text=$(intent "$cmd" --help 2>&1)

  echo "Checking $cmd..."

  check_section "$cmd" "WHAT IT DOES" "$help_text"
  check_section "$cmd" "WHY YOU'D USE IT" "$help_text"
  check_section "$cmd" "WHEN TO USE IT" "$help_text"

  # Optional PREREQUISITES section
  if echo "$help_text" | grep -q "^PREREQUISITES\$"; then
    echo "✓ $cmd has PREREQUISITES section"
    ((PASS++))
  else
    # Only warn if not in the no-prereqs list
    if [[ ! " ${COMMANDS_NO_PREREQS[@]} " =~ " $cmd " ]]; then
      echo "⚠ $cmd has no PREREQUISITES section (may be intentional)"
      ((WARNINGS++))
    fi
  fi

  check_section "$cmd" "USAGE EXAMPLES" "$help_text"
  check_section "$cmd" "FLAG DETAILS" "$help_text"
  check_section "$cmd" "EXIT CODES" "$help_text"
  check_section "$cmd" "SEE ALSO" "$help_text"

  echo ""
done

echo "Summary: $PASS passed, $FAIL failed, $WARNINGS warnings"
exit $([ $FAIL -eq 0 ] && echo 0 || echo 1)
```

### 3. Usage Examples Syntax Validation Test

**File:** `/home/lewis/src/intent-cli/scripts/test-help-examples.sh`

Validates that examples in help text have valid basic syntax.

```bash
#!/bin/bash
# Test: Examples in help text have valid syntax

COMMANDS=(
  "check" "validate" "show" "export"
  "lint" "analyze" "improve" "doctor"
  "interview" "beads" "bead-status" "history" "diff" "sessions"
  "quality" "invert" "coverage" "gaps" "effects" "ears"
  "parse" "plan" "plan-approve" "beads-regenerate"
)

PASS=0
FAIL=0

# Extract and validate examples
for cmd in "${COMMANDS[@]}"; do
  help_text=$(intent "$cmd" --help 2>&1)

  # Count number of examples (lines starting with "intent" after USAGE EXAMPLES)
  example_count=$(echo "$help_text" | \
    awk '/^USAGE EXAMPLES/,/^[A-Z]/' | \
    grep -c "^\s*intent")

  if [ "$example_count" -lt 2 ]; then
    echo "✗ $cmd has only $example_count examples (need 2+)"
    ((FAIL++))
  else
    echo "✓ $cmd has $example_count examples"
    ((PASS++))
  fi

  # Validate examples don't use absolute paths like /tmp or /home
  bad_paths=$(echo "$help_text" | \
    awk '/^USAGE EXAMPLES/,/^[A-Z]/' | \
    grep -E "/(tmp|home|var|opt|usr)/")

  if [ -z "$bad_paths" ]; then
    echo "✓ $cmd examples use no absolute paths"
    ((PASS++))
  else
    echo "⚠ $cmd examples may contain absolute paths"
    echo "  Examples: $(echo "$bad_paths" | head -1)"
    ((FAIL++))
  fi
done

echo ""
echo "Summary: $PASS passed, $FAIL failed"
exit $([ $FAIL -eq 0 ] && echo 0 || echo 1)
```

### 4. Flag Documentation Coverage Test

**File:** `/home/lewis/src/intent-cli/scripts/test-help-flags.sh`

Validates that all flags used in examples are documented in FLAG DETAILS.

```bash
#!/bin/bash
# Test: All flags are documented in FLAG DETAILS

COMMANDS=(
  "check" "validate" "show" "export"
  "lint" "analyze" "improve" "doctor"
  "interview" "beads" "bead-status" "history" "diff" "sessions"
  "quality" "invert" "coverage" "gaps" "effects" "ears"
  "parse" "plan" "plan-approve" "beads-regenerate"
)

PASS=0
FAIL=0

for cmd in "${COMMANDS[@]}"; do
  help_text=$(intent "$cmd" --help 2>&1)

  # Extract flags from USAGE EXAMPLES
  flags_in_examples=$(echo "$help_text" | \
    awk '/^USAGE EXAMPLES/,/^FLAG DETAILS/' | \
    grep -oE '\-\-[a-z0-9\-]+' | sort -u)

  # Extract flags from FLAG DETAILS section
  flags_documented=$(echo "$help_text" | \
    awk '/^FLAG DETAILS/,/^[A-Z]/' | \
    grep -oE '\-\-[a-z0-9\-]+' | sort -u)

  # Check coverage
  for flag in $flags_in_examples; do
    if echo "$flags_documented" | grep -q "^$flag\$"; then
      echo "✓ $cmd: $flag is documented"
      ((PASS++))
    else
      echo "✗ $cmd: $flag used in examples but not documented"
      ((FAIL++))
    fi
  done
done

echo ""
echo "Summary: $PASS passed, $FAIL failed"
exit $([ $FAIL -eq 0 ] && echo 0 || echo 1)
```

### 5. Example File Reference Validation Test

**File:** `/home/lewis/src/intent-cli/scripts/test-help-references.sh`

Validates that example files referenced in help text exist.

```bash
#!/bin/bash
# Test: Example file references in help text exist

REPO_ROOT="/home/lewis/src/intent-cli"
COMMANDS=(
  "check" "validate" "show" "export"
  "lint" "analyze" "improve" "doctor"
  "interview" "beads" "bead-status" "history" "diff" "sessions"
  "quality" "invert" "coverage" "gaps" "effects" "ears"
  "parse" "plan" "plan-approve" "beads-regenerate"
)

PASS=0
FAIL=0

for cmd in "${COMMANDS[@]}"; do
  help_text=$(intent "$cmd" --help 2>&1)

  # Extract file references (anything like *.cue, *.json, etc.)
  files=$(echo "$help_text" | grep -oE '\b[a-z0-9._-]+\.(cue|json|md|yml|yaml|txt)' | sort -u)

  for file in $files; do
    # Check in examples directory
    if [ -f "$REPO_ROOT/examples/$file" ]; then
      echo "✓ $cmd: Example file $file exists"
      ((PASS++))
    elif [ -f "$REPO_ROOT/$file" ]; then
      echo "✓ $cmd: Reference file $file exists"
      ((PASS++))
    else
      # These might be template references, so just warn
      echo "⚠ $cmd: Reference $file not found locally (may be template)"
      ((PASS++))
    fi
  done
done

echo ""
echo "Summary: $PASS passed, $FAIL failed"
exit $([ $FAIL -eq 0 ] && echo 0 || echo 1)
```

### 6. Content Quality Test

**File:** `/home/lewis/src/intent-cli/scripts/test-help-quality.sh`

Validates content quality aspects of help text.

```bash
#!/bin/bash
# Test: Help text content quality (length, structure)

COMMANDS=(
  "check" "validate" "show" "export"
  "lint" "analyze" "improve" "doctor"
  "interview" "beads" "bead-status" "history" "diff" "sessions"
  "quality" "invert" "coverage" "gaps" "effects" "ears"
  "parse" "plan" "plan-approve" "beads-regenerate"
)

PASS=0
FAIL=0
WARNINGS=0

for cmd in "${COMMANDS[@]}"; do
  help_text=$(intent "$cmd" --help 2>&1)

  # Check WHAT IT DOES section is not too long (< 150 words)
  what_section=$(echo "$help_text" | \
    awk '/^WHAT IT DOES/,/^WHY YOU.D USE IT/' | \
    head -n -1)

  word_count=$(echo "$what_section" | wc -w)
  if [ "$word_count" -lt 150 ]; then
    echo "✓ $cmd: WHAT IT DOES section is concise ($word_count words)"
    ((PASS++))
  else
    echo "⚠ $cmd: WHAT IT DOES section is verbose ($word_count words, max 150)"
    ((WARNINGS++))
  fi

  # Check for incomplete sentences (ends without period)
  bad_endings=$(echo "$help_text" | grep -E "[a-zA-Z0-9]$" | head -1)
  if [ -z "$bad_endings" ]; then
    echo "✓ $cmd: Help text sections end with punctuation"
    ((PASS++))
  else
    echo "⚠ $cmd: Some text may be incomplete"
    ((WARNINGS++))
  fi

  # Check for common typos (case-insensitive)
  typos=$(echo "$help_text" | grep -iE "(teh |taht |becuase |recieve |occured)")
  if [ -z "$typos" ]; then
    echo "✓ $cmd: No obvious typos detected"
    ((PASS++))
  else
    echo "✗ $cmd: Possible typos found"
    ((FAIL++))
  fi
done

echo ""
echo "Summary: $PASS passed, $FAIL failed, $WARNINGS warnings"
exit $([ $FAIL -eq 0 ] && echo 0 || echo 1)
```

### 7. Master Test Runner

**File:** `/home/lewis/src/intent-cli/scripts/test-help-all.sh`

Orchestrates all help text tests and produces a summary report.

```bash
#!/bin/bash
# Master test runner for help text validation

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTS=(
  "test-help-invocation"
  "test-help-sections"
  "test-help-examples"
  "test-help-flags"
  "test-help-references"
  "test-help-quality"
)

echo "==========================================="
echo "Intent CLI Help Text Test Suite"
echo "==========================================="
echo ""

TOTAL_PASSED=0
TOTAL_FAILED=0
TOTAL_WARNINGS=0

for test in "${TESTS[@]}"; do
  echo "Running: $test"
  echo "---"

  if bash "$SCRIPT_DIR/$test.sh" 2>&1; then
    echo "✓ $test PASSED"
  else
    echo "✗ $test FAILED"
  fi

  echo ""
done

echo "==========================================="
echo "Test Suite Complete"
echo "==========================================="
```

---

## Manual Test Plan

### Session 1: Core Testing Commands (4 commands, ~30 min)

**Commands:** check, validate, show, export

For each command:

1. Run `intent <cmd> --help`
2. Read through all sections carefully
3. Verify against checklist:
   - [ ] WHAT IT DOES is clear and concise (2-3 sentences max)
   - [ ] WHY YOU'D USE IT answers the motivation question
   - [ ] WHEN TO USE IT provides context (dev/staging/prod/ci)
   - [ ] PREREQUISITES are realistic and accurate
   - [ ] At least 2 examples provided
   - [ ] Examples use realistic file names (from examples/ dir where applicable)
   - [ ] All flags mentioned in examples are documented
   - [ ] EXIT CODES match actual behavior
   - [ ] SEE ALSO lists related commands
4. Try running one example command (with --help to see what would happen)
5. Document any issues/improvements in MANUAL_TEST_RESULTS.md

**Time allocation:** ~7-8 minutes per command

### Session 2: Quality Analysis Commands (4 commands, ~30 min)

**Commands:** lint, analyze, improve, doctor

Same checklist as Session 1

**Time allocation:** ~7-8 minutes per command

### Session 3: Interview & Workflow Commands (5 commands, ~40 min)

**Commands:** interview, beads, bead-status, history, diff, sessions

Same checklist as Session 1

**Time allocation:** ~6-8 minutes per command

**Special focus:**
- interview: Verify prerequisites mention profile types
- bead-status: Verify status values documented
- history/diff: Verify these work with sessions

### Session 4: KIRK Analysis Commands (6 commands, ~45 min)

**Commands:** quality, invert, coverage, gaps, effects, ears

Same checklist as Session 1

**Special focus:**
- All should have "KIRK:" prefix in descriptions
- Verify OWASP, mental models, and analysis terminology is explained
- Check that complex concepts have real examples

**Time allocation:** ~7-8 minutes per command

### Session 5: Additional Commands (4 commands, ~30 min)

**Commands:** parse, plan, plan-approve, beads-regenerate

Same checklist as Session 1

**Special focus:**
- parse: EARS format examples
- plan: Wave structure and dependencies
- plan-approve: CI integration examples
- beads-regenerate: Failed/blocked bead context

**Time allocation:** ~7-8 minutes per command

### Session 6: Cross-Command Verification (30 min)

1. **Consistency review**: Randomly pick 5 commands, ensure tone/format are consistent
2. **Link verification**: From "SEE ALSO" sections, verify all linked commands exist
3. **Example walkthrough**: Pick 3 complex examples, verify they would actually work
4. **Tone survey**: Read through all 24 one-liners, ensure conversational + technical balance
5. **Accessibility**: Does someone new to Intent CLI understand what each command does?

---

## Test Failure Criteria

A command's help text **FAILS** testing if any of these conditions are true:

### Critical Failures (Blocks Release)

- [ ] No help text output when `--help` is invoked (exit code non-zero)
- [ ] Missing WHAT IT DOES section
- [ ] Missing USAGE EXAMPLES section (fewer than 2 examples)
- [ ] Missing FLAG DETAILS section
- [ ] Missing EXIT CODES section
- [ ] Flag used in example but not documented in FLAG DETAILS
- [ ] Example uses absolute path (`/tmp`, `/home`, etc.)
- [ ] Example contains syntax error (can't be copy/pasted as-is)

### Major Failures (Should Fix Before Release)

- [ ] Missing WHY YOU'D USE IT section
- [ ] Missing WHEN TO USE IT section
- [ ] WHAT IT DOES section > 150 words
- [ ] Fewer than 2 real USAGE EXAMPLES
- [ ] Flag description is incomplete/unclear
- [ ] Missing description for documented flag behavior
- [ ] Contradictions between help text and actual behavior
- [ ] SEE ALSO section missing or empty
- [ ] Obvious typos or grammatical errors

### Minor Failures (Nice to Fix)

- [ ] Very long lines (> 100 chars, affects formatting)
- [ ] Inconsistent section spacing/indentation
- [ ] Referenced example files don't exist (template OK)
- [ ] Tone inconsistency with other commands
- [ ] Abbreviations not explained on first use

---

## Continuous Integration

### Pre-Commit Hook

Create `.git/hooks/pre-commit` to validate help text before commits:

```bash
#!/bin/bash
# Check that modified command files have valid help text

changed_files=$(git diff --cached --name-only)

if echo "$changed_files" | grep -q "src/intent.gleam"; then
  echo "Validating Intent CLI help text..."

  # Run quick validation
  if bash scripts/test-help-invocation.sh > /dev/null 2>&1; then
    echo "✓ Help text validation passed"
  else
    echo "✗ Help text validation failed"
    echo "  Run: bash scripts/test-help-all.sh"
    exit 1
  fi
fi

exit 0
```

### CI Pipeline

Add to `.github/workflows/test.yml`:

```yaml
- name: Validate Help Text
  run: |
    bash scripts/test-help-invocation.sh
    bash scripts/test-help-sections.sh
    bash scripts/test-help-examples.sh
    bash scripts/test-help-flags.sh
```

---

## Implementation Roadmap

### Phase 4.1: Test Framework (This Task)
- [x] Define help text standard
- [x] Create test scripts
- [x] Define manual test plan

### Phase 4.2: Automated Test Execution
- [ ] Create scripts directory and add all test scripts
- [ ] Test scripts against current code
- [ ] Fix failing commands
- [ ] Integrate into CI

### Phase 4.3: Manual Testing
- [ ] Execute all manual test sessions
- [ ] Document findings in MANUAL_TEST_RESULTS.md
- [ ] File issues for improvements
- [ ] Update help text based on feedback

### Phase 4.4: Refinement & Documentation
- [ ] Update CLAUDE.md with final help text standards
- [ ] Add help text review checklist to PR template
- [ ] Document common pitfalls and best practices
- [ ] Create example help text for new commands

---

## Test Execution Checklist

### Before Running Tests

- [ ] Clone repository and build project: `gleam build`
- [ ] Ensure all examples files exist in `/examples/`
- [ ] Create `/scripts/` directory if it doesn't exist

### Running Automated Tests

```bash
# Run all tests
bash scripts/test-help-all.sh

# Run individual tests
bash scripts/test-help-invocation.sh
bash scripts/test-help-sections.sh
bash scripts/test-help-examples.sh
bash scripts/test-help-flags.sh
bash scripts/test-help-references.sh
bash scripts/test-help-quality.sh
```

### Running Manual Tests

```bash
# Session 1: Core commands
for cmd in check validate show export; do
  intent $cmd --help | less
done

# Session 2: Quality commands
for cmd in lint analyze improve doctor; do
  intent $cmd --help | less
done

# ...and so on for remaining sessions
```

### Reporting Results

Create `MANUAL_TEST_RESULTS.md`:

```markdown
# Manual Help Text Test Results

## Command: check
- **Date:** 2026-01-18
- **Tester:** [Your Name]
- **Status:** ✓ PASS

### Issues Found
- Minor: FLAG DETAILS spacing inconsistent with other commands

## Command: validate
- **Date:** 2026-01-18
- **Tester:** [Your Name]
- **Status:** ✓ PASS

### Issues Found
- None

...
```

---

## Success Metrics

A complete test run is **successful** when:

1. ✓ All 24 commands respond to `--help` with exit code 0
2. ✓ All 24 commands have all required sections
3. ✓ All commands have at least 2 real usage examples
4. ✓ 0 critical failures
5. ✓ < 5 major failures (acceptable for Phase 4.1)
6. ✓ All flags documented
7. ✓ No absolute path references in examples
8. ✓ No obvious typos or grammatical errors
9. ✓ 100% consistency in section structure across all commands
10. ✓ All referenced files/commands exist

---

## References

- **CLAUDE.md** - Comprehensive Intent CLI documentation
- **src/intent/cli_text_constants.gleam** - Centralized help text strings
- **src/intent.gleam** - Command definitions with help text
- **examples/** - Example CUE spec files for reference

---

## Appendix A: Help Text Checklist Template

Use this template when reviewing a command's help text:

```
COMMAND: _______________
Tester: ________________  Date: __________

STRUCTURE CHECKS
[ ] WHAT IT DOES section present
[ ] WHY YOU'D USE IT section present
[ ] WHEN TO USE IT section present
[ ] PREREQUISITES section (if applicable)
[ ] USAGE EXAMPLES section (2+ examples)
[ ] FLAG DETAILS section (all flags documented)
[ ] EXIT CODES section (0,1,2,3,4 defined)
[ ] SEE ALSO section (2-4 related commands)

CONTENT QUALITY
[ ] WHAT IT DOES is 2-3 sentences (< 150 words)
[ ] Tone is conversational but technical
[ ] No obvious typos
[ ] No grammatical errors
[ ] Examples use realistic file names
[ ] Examples are syntactically valid
[ ] Examples don't contain absolute paths
[ ] All flags in examples are documented

ACCURACY
[ ] Examples would actually work
[ ] EXIT CODES match real behavior
[ ] Referenced files/commands exist
[ ] No contradictions vs. actual behavior

OVERALL ASSESSMENT
Status: [ ] PASS  [ ] PASS WITH MINOR ISSUES  [ ] FAIL

Issues & Recommendations:
_______________________________________________________
_______________________________________________________
_______________________________________________________
```

---

**Document Version:** 1.0
**Last Updated:** 2026-01-18
**Next Review:** After automated test implementation
