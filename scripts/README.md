# Intent CLI Help Text Test Suite

Automated testing framework for validating help text across all 24 Intent CLI commands.

## Overview

This test suite ensures that Intent CLI help text meets quality standards for:

- **Completeness**: All required sections present (WHAT/WHY/WHEN/etc.)
- **Consistency**: Uniform structure and tone across all commands
- **Accuracy**: Examples are realistic and would actually work
- **Usability**: Help text is clear and actionable
- **Quality**: No typos, grammatical errors, or formatting issues

## Quick Start

### Run All Tests

```bash
# Build Intent CLI first
cd /home/lewis/src/intent-cli
gleam build

# Run full test suite
bash scripts/test-help-all.sh

# Or specify custom binary path
bash scripts/test-help-all.sh /path/to/intent
```

### Run Individual Tests

```bash
# Test invocation (all commands respond to --help)
bash scripts/test-help-invocation.sh

# Test required sections
bash scripts/test-help-sections.sh

# Test usage examples
bash scripts/test-help-examples.sh

# Test flag documentation
bash scripts/test-help-flags.sh

# Test content quality
bash scripts/test-help-quality.sh
```

## Test Scripts

### 1. test-help-invocation.sh

**Purpose:** Validates that all 24 commands respond to `--help` flag

**Checks:**
- Command exits with code 0
- Produces non-empty output (> 500 bytes)
- Output is substantial

**Usage:**
```bash
bash scripts/test-help-invocation.sh [binary]
```

**Example Output:**
```
✓ check                exit code 0
✓ check                output: 4250 bytes
✓ check                output length: substantial (4250 bytes)
✓ validate             exit code 0
...
Summary: 72 passed, 0 failed
```

---

### 2. test-help-sections.sh

**Purpose:** Validates help text contains all required sections

**Checks:**
- WHAT IT DOES section
- WHY YOU'D USE IT section
- WHEN TO USE IT section
- PREREQUISITES section (optional for some commands)
- USAGE EXAMPLES section
- FLAG DETAILS section
- EXIT CODES section
- SEE ALSO section

**Usage:**
```bash
bash scripts/test-help-sections.sh [binary]
```

**Example Output:**
```
Checking: check
✓ check              has WHAT IT DOES
✓ check              has WHY YOU'D USE IT
✓ check              has WHEN TO USE IT
✓ check              has PREREQUISITES
✓ check              has usage examples section
...
Summary: 180 passed, 3 failed, 2 warnings
```

---

### 3. test-help-examples.sh

**Purpose:** Validates usage examples are present and have valid syntax

**Checks:**
- At least 2 examples per command
- Examples don't use absolute paths (`/tmp`, `/home`, etc.)
- Examples are concrete (no placeholder text)
- Examples follow standard format

**Usage:**
```bash
bash scripts/test-help-examples.sh [binary]
```

**Example Output:**
```
✓ check              has 4 examples
✓ check              examples avoid absolute paths
✓ check              examples are concrete
✓ check              examples follow standard format
...
Summary: 96 passed, 0 failed, 0 warnings
```

---

### 4. test-help-flags.sh

**Purpose:** Validates all flags used in examples are documented

**Checks:**
- Flags in examples section are documented in FLAG DETAILS
- Flag count is reasonable
- Common flags (--json, --verbose, etc.) are documented

**Usage:**
```bash
bash scripts/test-help-flags.sh [binary]
```

**Example Output:**
```
✓ check              flag --target is documented
✓ check              flag --json is documented
✓ check              flag --feature is documented
✓ check              3 flags documented
...
Summary: 85 passed, 2 failed, 1 warning
```

---

### 5. test-help-quality.sh

**Purpose:** Validates content quality (spelling, grammar, formatting)

**Checks:**
- No common typos
- Sentences properly punctuated
- Lines don't exceed 100 characters
- Consistent capitalization
- Proper indentation
- Description doesn't just repeat command name

**Usage:**
```bash
bash scripts/test-help-quality.sh [binary]
```

**Example Output:**
```
✓ check              no common typos detected
✓ check              all sentences properly punctuated
✓ check              all lines reasonable length
✓ check              consistent 'Intent' capitalization
✓ check              proper flag indentation
✓ check              description is distinct from command name
...
Summary: 144 passed, 0 failed, 1 warning
```

---

### 6. test-help-all.sh

**Purpose:** Master test runner - executes all tests and produces summary

**Checks:**
- Runs all 5 test scripts
- Produces comprehensive summary
- Determines overall pass/fail status

**Usage:**
```bash
bash scripts/test-help-all.sh [binary]
```

**Example Output:**
```
Intent CLI Help Text Test Suite

Binary: intent
Tests: 5

Running: test-help-invocation
✓ test-help-invocation PASSED

Running: test-help-sections
✓ test-help-sections PASSED

Running: test-help-examples
✓ test-help-examples PASSED

Running: test-help-flags
✓ test-help-flags PASSED

Running: test-help-quality
✓ test-help-quality PASSED

Test Suite Summary
Tests Passed:  5/5
Tests Failed:  0/5

✓ All tests passed!
Help text quality status: ACCEPTABLE
```

---

## Manual Testing

For comprehensive manual testing, follow the process documented in:
**`MANUAL_TEST_CHECKLIST.md`**

Quick summary:
- 6 testing sessions covering all 24 commands
- ~30-45 minutes per session
- Detailed checklist for each command
- Cross-command consistency review

---

## Understanding Test Output

### Color Codes

- **Green (✓)** - Test passed
- **Red (✗)** - Test failed (critical issue)
- **Yellow (⚠)** - Warning (non-critical issue)

### Exit Codes

```
0 = All tests passed (help text is acceptable)
1 = Some tests failed (help text needs fixes)
```

## Continuous Integration

### GitHub Actions Example

```yaml
- name: Test Help Text
  run: |
    gleam build
    bash scripts/test-help-invocation.sh
    bash scripts/test-help-sections.sh
    bash scripts/test-help-examples.sh
    bash scripts/test-help-flags.sh
    bash scripts/test-help-quality.sh
```

### Pre-Commit Hook

Create `.git/hooks/pre-commit`:

```bash
#!/bin/bash
if git diff --cached --name-only | grep -q "src/intent.gleam"; then
  echo "Validating help text..."
  bash scripts/test-help-invocation.sh > /dev/null || exit 1
  bash scripts/test-help-sections.sh > /dev/null || exit 1
fi
exit 0
```

Make executable:
```bash
chmod +x .git/hooks/pre-commit
```

---

## Interpreting Results

### All Tests Pass ✓

Help text is acceptable for release. Standard quality met:
- All commands have help text
- All required sections present
- Examples are realistic
- No critical issues found

### Some Tests Fail ✗

Review failures and fix critical issues before release:

1. **Missing Sections** → Add to help text
2. **Bad Examples** → Use concrete, working examples
3. **Undocumented Flags** → Document in FLAG DETAILS
4. **Typos/Grammar** → Spell-check and fix

### Warnings (⚠)

Non-critical issues that should be addressed:
- Optional sections missing (assess if needed)
- Line length warnings (may affect display)
- Formatting inconsistencies

---

## Test Coverage Map

| Test | check | validate | show | export | lint | analyze | improve | doctor | interview | beads | bead-status | history | diff | sessions | quality | invert | coverage | gaps | effects | ears | parse | plan | plan-approve | beads-regenerate |
|------|-------|----------|------|--------|------|---------|---------|--------|-----------|-------|-------------|---------|------|----------|---------|--------|----------|------|---------|------|-------|------|--------------|-----------------|
| Invocation | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Sections | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Examples | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Flags | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Quality | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |

---

## Troubleshooting

### Error: "Binary not found"

```bash
# Build Intent CLI first
cd /home/lewis/src/intent-cli
gleam build

# Then run tests with built binary
bash scripts/test-help-all.sh
```

### Error: "Command not found: intent"

```bash
# If intent is not in PATH, specify full path
bash scripts/test-help-all.sh /home/lewis/src/intent-cli/dist/bin/intent

# Or add to PATH
export PATH="/home/lewis/src/intent-cli/dist/bin:$PATH"
bash scripts/test-help-all.sh
```

### Test Failure: "Missing WHAT IT DOES"

The help text for a command doesn't have the required structure. Edit the command in `src/intent.gleam` and add the missing section. Use the `check` command as a reference.

### Test Failure: "No examples"

Add at least 2 realistic examples to the `long_help()` section. Examples should be copy-pasteable and demonstrate real use cases.

---

## Adding New Commands

When adding a new Intent CLI command, follow this checklist:

1. **Add to COMMANDS array** in all test scripts
2. **Implement help text** with all required sections:
   - WHAT IT DOES
   - WHY YOU'D USE IT
   - WHEN TO USE IT
   - PREREQUISITES (if applicable)
   - USAGE EXAMPLES (2+)
   - FLAG DETAILS
   - EXIT CODES
   - SEE ALSO

3. **Run tests** to validate:
   ```bash
   bash scripts/test-help-all.sh
   ```

4. **Fix any failures** before merging

---

## Resources

- **Full Strategy Document:** `HELP_TEXT_TESTING_STRATEGY.md`
- **Manual Test Checklist:** `MANUAL_TEST_CHECKLIST.md`
- **Command Implementation:** `src/intent.gleam`
- **Help Text Constants:** `src/intent/cli_text_constants.gleam`

---

## Support

For issues or improvements to the test suite:

1. Review the test scripts and strategy document
2. Check help text in `src/intent.gleam` for examples
3. Run individual tests to isolate issues
4. Update scripts or strategy as needed

---

**Last Updated:** 2026-01-18
**Version:** 1.0
**Maintainer:** Intent CLI Team
