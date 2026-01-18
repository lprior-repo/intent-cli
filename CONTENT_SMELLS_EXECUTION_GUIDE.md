# Content Smells Beads - Execution Guide

Quick reference for executing the 5 content smell beads.

## Quick Start

```bash
# View all beads
cat CONTENT_SMELLS_BEADS.jsonl | jq .

# Import to bead system
bd create --from CONTENT_SMELLS_BEADS.jsonl --batch content-smells

# Track progress
bd ready --filter tag:code-smell --json
```

## Individual Bead Execution

### Bead 1: Parse Command KIRK Prefix (5 min)
**ID**: `cs-001-parse-kirk-prefix`
**Priority**: Critical
**Impact**: 1 line change

```bash
# Location
src/intent/cli_text_constants.gleam:84

# Change
- pub const cmd_parse_desc = "Parse EARS requirements to structured spec"
+ pub const cmd_parse_desc = "KIRK: Parse EARS requirements to structured spec"

# Verify
grep "cmd_parse_desc" src/intent/cli_text_constants.gleam
# Should show: pub const cmd_parse_desc = "KIRK: Parse EARS requirements to structured spec"
```

**Before**:
```gleam
pub const cmd_parse_desc = "Parse EARS requirements to structured spec"
```

**After**:
```gleam
pub const cmd_parse_desc = "KIRK: Parse EARS requirements to structured spec"
```

---

### Bead 2: EARS Pattern Count (10 min)
**ID**: `cs-002-ears-pattern-count`
**Priority**: Critical
**Impact**: 1-2 line changes

```bash
# Location
src/intent/cli_text_constants.gleam:1695 (in kirk_ears_extended_help)

# Identify the issue
grep -n "Recognizes 5 patterns" src/intent/cli_text_constants.gleam

# Action: Change "5 patterns" to "4 patterns"
sed -i 's/Recognizes 5 patterns:/Recognizes 4 patterns:/' src/intent/cli_text_constants.gleam

# Verify
grep -A 3 "Recognizes.*patterns:" src/intent/cli_text_constants.gleam
```

**Before**:
```
Recognizes 5 patterns:
THE SYSTEM SHALL (ubiquitous), GIVEN/WHEN/THEN (scenario-based), IF/THEN
(conditional), and state-based patterns.
```

**After**:
```
Recognizes 4 patterns:
THE SYSTEM SHALL (ubiquitous), GIVEN/WHEN/THEN (scenario-based), IF/THEN
(conditional), and state-based patterns.
```

---

### Bead 3: Quality AI-Readiness Dimension (20 min)
**ID**: `cs-003-quality-ai-readiness-dimension`
**Priority**: Critical
**Impact**: 4-5 line changes

```bash
# Location
src/intent/cli_text_constants.gleam:1451 (cmd_quality_desc + kirk_quality_extended_help)

# Step 1: Update command description (line ~66)
# Find: pub const cmd_quality_desc = "KIRK: Analyze spec quality across coverage, clarity, testability"
# Replace with: pub const cmd_quality_desc = "KIRK: Analyze spec quality across 4 dimensions: coverage, clarity, testability, AI-readiness"

# Step 2: Update extended help WHAT IT DOES section (line ~1454-1457)
# Find the 4 dimensions text and make them explicit with numbering
```

**Before** (line ~66):
```gleam
pub const cmd_quality_desc = "KIRK: Analyze spec quality across coverage, clarity, testability"
```

**After**:
```gleam
pub const cmd_quality_desc = "KIRK: Analyze spec quality across 4 dimensions: coverage, clarity, testability, AI-readiness"
```

**Before** (line ~1454-1457, WHAT IT DOES section):
```
KIRK (Komprehensive Intent Review Kit) quality analysis scores spec across
4 dimensions: coverage (required cases), clarity (documentation/structure),
testability (executability/assertions), and AI-readiness (LLM compatibility).
Returns individual scores plus overall health rating.
```

**After**:
```
KIRK (Komprehensive Intent Review Kit) quality analysis scores spec across 4 dimensions:
1. Coverage - required test cases and behavior coverage
2. Clarity - documentation quality and structural organization
3. Testability - executability of behaviors and assertion completeness
4. AI-readiness - LLM compatibility and implementation hints
Returns individual scores (0-100 each) plus overall health rating.
```

---

### Bead 4: Beads-Regenerate Mental Models (30 min)
**ID**: `cs-004-beads-regenerate-mental-models`
**Priority**: High
**Impact**: 4 function descriptions in FLAG DETAILS

```bash
# Location
src/intent/cli_text_constants.gleam:867-872 (beads_regenerate_extended_help FLAG DETAILS section)

# Strategy descriptions to update:
# - hybrid (add: combines inversion + effects + premortem)
# - inversion (add: mental model context + use case)
# - effects (add: consequence chain context + use case)
# - premortem (add: scenario thinking + use case)
```

**Before** (lines 867-872):
```
FLAG DETAILS
  --strategy STRATEGY
    Regeneration strategy: hybrid (default), inversion, effects, premortem
    - hybrid: Combines all models (recommended)
    - inversion: Flips failed behavior to find root causes
    - effects: Analyzes second-order dependencies
    - premortem: Post-mortem analysis for robustness
```

**After**:
```
FLAG DETAILS
  --strategy STRATEGY
    Regeneration strategy: hybrid (default), inversion, effects, premortem
    - hybrid (default): Combines inversion (find what breaks), effects (trace consequences),
      and premortem (imagine failure) for comprehensive new approaches. Recommended for most cases.
    - inversion: Mental model that flips failure: if behavior X failed, what's the opposite?
      Useful for logic errors and missing validation. Finds root causes by contradiction.
    - effects: Analyzes second-order effects and consequence chains. Use for dependency failures
      where consequences were missed or state changes didn't trigger expected behaviors.
    - premortem: Imagine spec deployed and failures occur. What would break? What's missing?
      Best for robustness checks and discovering unknown unknowns before execution.
```

---

### Bead 5: Effects Orphaned Behaviors (20 min)
**ID**: `cs-005-effects-orphaned-behaviors`
**Priority**: High
**Impact**: 1-2 line changes

```bash
# Location
src/intent/cli_text_constants.gleam:1648 (kirk_effects_extended_help WHAT IT DOES section)

# Find: "Identifies orphaned behaviors and missing consequence handlers."
# Replace with detailed definitions in parentheses
```

**Before** (lines 1645-1648):
```
Effects analysis traces consequence chains: what happens when a behavior
executes, what other behaviors depend on it, what state changes propagate.
Identifies orphaned behaviors and missing consequence handlers.
```

**After**:
```
Effects analysis traces consequence chains: what happens when a behavior
executes, what other behaviors depend on it, what state changes propagate.
Identifies orphaned behaviors (behaviors that don't trigger expected consequences)
and missing consequence handlers (state changes with no recovery or follow-up path).
```

---

## Verification Checklist

After executing each bead:

### Bead 1 (cs-001)
```bash
grep "cmd_parse_desc" src/intent/cli_text_constants.gleam | grep "KIRK:"
# ✓ Should show: pub const cmd_parse_desc = "KIRK: Parse EARS requirements..."
```

### Bead 2 (cs-002)
```bash
grep -A 3 "Recognizes.*patterns:" src/intent/cli_text_constants.gleam | head -4
# ✓ Should show: "Recognizes 4 patterns:" (not 5)
# ✓ Should list exactly 4 patterns
```

### Bead 3 (cs-003)
```bash
grep "cmd_quality_desc" src/intent/cli_text_constants.gleam | grep "AI-readiness"
# ✓ Should show AI-readiness in command description
grep -A 5 "4 dimensions:" src/intent/cli_text_constants.gleam | grep -n "."
# ✓ Should list numbered dimensions including ai_readiness
```

### Bead 4 (cs-004)
```bash
grep -A 8 "strategy STRATEGY" src/intent/cli_text_constants.gleam | grep -i "mental model"
# ✓ Should find "mental model" references
grep -A 8 "strategy STRATEGY" src/intent/cli_text_constants.gleam | grep -i "use case"
# ✓ Should find use case descriptions for each strategy
```

### Bead 5 (cs-005)
```bash
grep "orphaned behaviors" src/intent/cli_text_constants.gleam
# ✓ Should show definition in parentheses
# ✓ Should show: "(behaviors that don't trigger expected consequences)"
```

---

## Testing Commands

Run after executing all beads:

```bash
# Build and test
gleam build && gleam test

# Verify help text displays correctly
intent parse --help | grep -i "kirk"
intent quality --help | grep -i "ai-readiness"
intent effects --help | grep -i "orphaned"
intent beads-regenerate --help | grep -i "mental model"
intent ears --help | grep -i "pattern"

# Check for syntax errors
gleam format --check src/intent/cli_text_constants.gleam

# Run linter
gleam lint
```

---

## Rollback Strategy

If issues occur, rollback is easy since all changes are in one file:

```bash
# Save current version
cp src/intent/cli_text_constants.gleam src/intent/cli_text_constants.gleam.backup

# Rollback to main
git checkout src/intent/cli_text_constants.gleam

# Or rollback specific bead
git show HEAD:src/intent/cli_text_constants.gleam | head -100
```

---

## Batch Execution

To execute all beads as a batch:

```bash
# Start workflow
bd update cs-001-parse-kirk-prefix --status in_progress
# ... make change for bead 1 ...
bd close cs-001-parse-kirk-prefix --reason "Added KIRK: prefix to parse_desc"

bd update cs-002-ears-pattern-count --status in_progress
# ... make change for bead 2 ...
bd close cs-002-ears-pattern-count --reason "Fixed pattern count from 5 to 4"

# ... repeat for beads 3, 4, 5 ...

# Verify all closed
bd list --filter bead_group:content-smells --status closed
```

---

## Success Criteria

All beads successfully executed when:

✓ All 5 beads marked as `closed` with `success` status
✓ No syntax errors in `src/intent/cli_text_constants.gleam`
✓ `gleam build` completes successfully
✓ `gleam test` passes all tests
✓ Help text displays correctly with all fixes applied
✓ Code review approved by project lead

---

## Time Estimate

| Bead | Time | Notes |
|------|------|-------|
| cs-001 | 5 min | Single line change, straightforward |
| cs-002 | 10 min | Verify documentation, single change |
| cs-003 | 20 min | Multiple locations, needs coordination |
| cs-004 | 30 min | Longest, requires detailed writing |
| cs-005 | 20 min | Add definitions, moderate scope |
| **Total** | **85 min** | ~1.5 hours full execution |

---

**Generated**: 2026-01-18
**Format**: Markdown
**For**: Manual and automated bead execution
