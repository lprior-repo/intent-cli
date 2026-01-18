# Beads System Quick Start Guide

## What is a Bead?

A **bead** is an atomic 5-30 minute work unit from an auditor finding. All 21 beads are in `BEADS.jsonl` (JSONL format - one JSON object per line).

## Where to Start?

### Phase A (Critical) - Execute First ⚠️
**Why:** Production bugs & documentation errors. Blocking all other work.
**Effort:** ~2 hours
**When to start:** NOW

```bash
# View Phase A beads
cat BEADS.jsonl | jq 'select(.priority == 0)' | head -20

# Pretty view
cat BEADS.jsonl | jq 'select(.priority == 0) | {id, title, effort, requires}'
```

**Execution order:**
1. CRITICAL-004 (5 min) - Add "KIRK: " prefix
2. CRITICAL-005 (10 min) - Fix EARS pattern count
3. CRITICAL-006 (5 min) - Add ai_readiness dimension
4. CRITICAL-001 (15 min) - Fix center_in_box bounds
5. CRITICAL-002 (15 min) - Fix progress_bar_with_width DOS
6. CRITICAL-003 (10 min) - Fix config timeout merge
7. CRITICAL-007 (20 min) - Test bounds for center_in_box
8. CRITICAL-008 (20 min) - Test validation for progress_bar

## Files Reference

| File | Purpose |
|------|---------|
| **BEADS.jsonl** | All 21 bead definitions (machine-readable) |
| **BEADS_INDEX.md** | Quick catalog of all beads |
| **BEADS_EXECUTION_PLAN.md** | Detailed timeline, dependencies, strategy |
| **BEADS_QUICK_START.md** | This file - quick reference |

## Understanding Bead Structure

Each bead in BEADS.jsonl has:
```json
{
  "id": "CRITICAL-001",           # Unique identifier
  "title": "Fix center_in_box...",  # What to do
  "description": "...",            # Why & how
  "file": "src/intent/...",        # What file to change
  "lines": [39, 49],               # Line range
  "priority": 0,                   # 0=critical, 1=high, 2=medium, 4=backlog
  "effort": 15,                    # Minutes
  "tags": ["critical", "..."],     # Categorization
  "requires": [],                  # Dependencies (other bead IDs)
  "success_criteria": ["...", "..."]  # How to verify completion
}
```

## Common Commands

### View all beads
```bash
cat BEADS.jsonl | jq .
```

### Filter by phase
```bash
cat BEADS.jsonl | jq 'select(.priority == 0)'    # CRITICAL
cat BEADS.jsonl | jq 'select(.priority == 1)'    # HIGH
cat BEADS.jsonl | jq 'select(.priority == 2)'    # MEDIUM
cat BEADS.jsonl | jq 'select(.priority == 4)'    # BACKLOG
```

### View specific bead
```bash
cat BEADS.jsonl | jq 'select(.id == "CRITICAL-001")'
```

### View bead summary
```bash
cat BEADS.jsonl | jq '{id, title, effort, priority}'
```

### Check dependencies
```bash
# What does CRITICAL-007 depend on?
cat BEADS.jsonl | jq 'select(.id == "CRITICAL-007") | .requires'

# What depends on CRITICAL-001?
cat BEADS.jsonl | jq '.[] | select(.requires[] == "CRITICAL-001")'
```

## Workflow: Claiming & Completing a Bead

### 1. Claim a bead
```bash
# Get next unclaimed bead
cat BEADS.jsonl | jq 'select(.priority == 0) | first | .id'

# Example: CRITICAL-004
echo "Working on: CRITICAL-004"
```

### 2. Read the bead details
```bash
cat BEADS.jsonl | jq 'select(.id == "CRITICAL-004")'
```

Output:
```json
{
  "id": "CRITICAL-004",
  "title": "Add KIRK: prefix to parse command description",
  "description": "cmd_parse_desc at line 84 in cli_text_constants.gleam missing 'KIRK: ' prefix...",
  "file": "src/intent/cli_text_constants.gleam",
  "lines": [84, 84],
  "priority": 0,
  "effort": 5,
  "tags": ["critical", "help-text", "consistency", "documentation"],
  "requires": [],
  "success_criteria": [
    "parse command description starts with 'KIRK: '",
    "Consistency check passes for all KIRK commands"
  ]
}
```

### 3. Make changes
```bash
# Read the file
cat src/intent/cli_text_constants.gleam | sed -n '84p'
# Output: pub const cmd_parse_desc = "Parse EARS requirements to structured spec"

# Edit to add "KIRK: " prefix
# Change to: pub const cmd_parse_desc = "KIRK: Parse EARS requirements to structured spec"
```

### 4. Verify success criteria
```bash
# Run tests
gleam test

# Check build
gleam build

# Verify the change
cat src/intent/cli_text_constants.gleam | sed -n '84p'
# Should show: pub const cmd_parse_desc = "KIRK: Parse EARS requirements to structured spec"
```

### 5. Commit
```bash
git add src/intent/cli_text_constants.gleam
git commit -m "CRITICAL-004: Add KIRK: prefix to parse command description"
```

## Tracking Progress

### Phase A Progress
```bash
# Count completed CRITICAL beads
git log --oneline | grep CRITICAL | wc -l

# Example tracking:
# 0/8 complete
# 1/8 complete (CRITICAL-004)
# 2/8 complete (CRITICAL-004, CRITICAL-005)
# ...
# 8/8 complete - Phase A gates open, Phase B can start
```

### Full Project Progress
```bash
# Beads by phase
echo "Phase A (Critical):"; cat BEADS.jsonl | jq -r 'select(.priority == 0) | .id' | sort
echo "Phase B (High):"; cat BEADS.jsonl | jq -r 'select(.priority == 1) | .id' | sort
echo "Phase C (Medium):"; cat BEADS.jsonl | jq -r 'select(.priority == 2) | .id' | sort
echo "Phase D (Backlog):"; cat BEADS.jsonl | jq -r 'select(.priority == 4) | .id' | sort
```

## Dependencies: When Can I Start?

### Phase A Beads (No dependencies ✓)
- CRITICAL-001: Start anytime
- CRITICAL-002: Start anytime (parallel with CRITICAL-001)
- CRITICAL-003: Start anytime
- CRITICAL-004: Start anytime
- CRITICAL-005: Start anytime
- CRITICAL-006: Start anytime
- CRITICAL-007: After CRITICAL-001 ← depends on
- CRITICAL-008: After CRITICAL-002 ← depends on

### Phase B Beads (Wait for Phase A)
- HIGH-001: After Phase A ← blocks on all CRITICAL
- HIGH-002: After Phase A
- HIGH-003: After Phase A
- HIGH-004: After Phase A
- HIGH-005: After Phase A

### Phase C Beads (Independent)
- MEDIUM-001: Start anytime
- MEDIUM-002: Start anytime (or after Phase A)
- MEDIUM-003: Start anytime
- MEDIUM-004: Start anytime

### Phase D Beads (Backlog)
- BACKLOG-001: Start anytime (after CRITICAL-007, CRITICAL-008)
- BACKLOG-002: Start anytime
- BACKLOG-003: Start anytime

## Files You'll Modify

### Phase A
```
src/intent/cli_text_constants.gleam    ← CRITICAL-004, 005, 006 (text edits)
src/intent/formatter_utils.gleam       ← CRITICAL-001, 002 (code fixes)
src/intent/config.gleam                ← CRITICAL-003 (code fix)
test/formatter_utils_test.gleam        ← CRITICAL-007, 008 (new tests)
```

### Phase B
```
test/help_text_test.gleam              ← HIGH-001 (refactor)
test/output_test.gleam                 ← HIGH-002 (new tests)
test/flag_normalization_test.gleam     ← HIGH-003 (new tests)
scripts/validate-phase-gates.sh        ← HIGH-004 (new script)
scripts/automation/                    ← HIGH-004 (automation)
.planning/parallelization-strategy.md  ← HIGH-005 (design doc)
```

### Phase C
```
.planning/plan-module-split.md         ← MEDIUM-001 (design doc)
.planning/error-handler-standardization.md ← MEDIUM-003 (design doc)
src/intent/emoji_constants.gleam       ← MEDIUM-004 (reorganization)
scripts/validate-phase-gates.sh        ← MEDIUM-002 (script)
```

### Phase D
```
src/intent/formatter_utils.gleam       ← BACKLOG-001 (performance)
test/cli_standards_fixtures.gleam      ← BACKLOG-002 (new fixtures)
docs/CLI_CONSISTENCY_HANDBOOK.md       ← BACKLOG-003 (documentation)
```

## Success = All Tests Pass

After each bead, verify:
```bash
gleam test    # All tests pass
gleam build   # No build errors
```

## Help & Questions

**What's the execution order?**
→ See "Execution Sequence" in BEADS_EXECUTION_PLAN.md

**Can I parallelize beads?**
→ Yes! Check "Dependency Graph" in BEADS_EXECUTION_PLAN.md

**What if I get stuck?**
→ Check the bead's `description` field for detailed context and `success_criteria` for validation

**How long should each bead take?**
→ Check `effort` field (in minutes). CRITICAL beads: 5-20 minutes each

**When is Phase B?**
→ After Phase A completes (gate: all tests pass). See BEADS_EXECUTION_PLAN.md

## Timeline Summary

```
TODAY        → Start Phase A (CRITICAL-004 first)
<1 day       → Phase A complete, gates open
Week 2-4     → Phase B (testing & automation)
Month 2      → Phase C (optimization)
v1.2+        → Phase D (backlog)
```

## One More Thing

**Phase A is blocking.** Everything else depends on it completing successfully.
Start with CRITICAL-004 (5-minute text fix) - get a quick win, then tackle the rest.

---

**Need details?** Check BEADS.jsonl for full bead definitions or BEADS_EXECUTION_PLAN.md for strategy.

**Ready to start?**
```bash
cat BEADS.jsonl | jq 'select(.id == "CRITICAL-004")'
```

Go! 🚀
