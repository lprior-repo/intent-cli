# Content Audit Code Smells - Complete Deliverables

**Audit**: Quality Auditor v1.0 (Content Smells Detection)
**Score**: 84.5/100
**Generated**: 2026-01-18
**Status**: Ready for Execution

## Overview

This package contains 5 executable beads derived from quality audit findings. The audit identified content smells in the Intent CLI help text - inconsistencies, missing definitions, and unclear explanations that reduce API clarity and usability.

**Critical Finding**: All issues are in a single file (`src/intent/cli_text_constants.gleam`), making fixes efficient and low-risk.

## What Are Code Smells?

Code smells detected:
- **Inconsistent Naming**: `parse` command lacks "KIRK:" prefix (Bead 1)
- **Documentation Mismatch**: EARS claims 5 patterns, lists 4 (Bead 2)
- **Missing Required Field**: ai_readiness not explicit in quality command (Bead 3)
- **Unexplained Concepts**: Beads-regenerate strategies lack mental model explanations (Bead 4)
- **Undefined Jargon**: Effects command doesn't define "orphaned behaviors" (Bead 5)

## Deliverables

### 1. `CONTENT_SMELLS_BEADS.jsonl`
**Machine-readable bead definitions** - Import to bead system for execution tracking.

```bash
bd create --from CONTENT_SMELLS_BEADS.jsonl --batch content-smells
```

**Contains**:
- 5 beads (cs-001 through cs-005)
- Bead ID, priority, effort estimate
- Exact file locations and line numbers
- Before/after code examples
- Success criteria for verification
- Tags for filtering and tracking

**Format**: Newline-delimited JSON (5 beads, 1 per line)
**Size**: 8.4 KB

### 2. `CONTENT_SMELLS_ANALYSIS.md`
**Comprehensive analysis document** - Detailed explanation of each code smell.

**Contains**:
- Problem description for each smell
- Root cause analysis
- Impact assessment
- Before/after code examples
- Verification procedures
- Summary table of all 5 smells
- Quality gates checklist
- Additional improvements (future work)

**Audience**: Project leads, code reviewers, auditors
**Size**: 14 KB (385 lines)

### 3. `CONTENT_SMELLS_EXECUTION_GUIDE.md`
**Step-by-step execution guide** - How to fix each code smell.

**Contains**:
- Individual bead execution instructions
- Specific file paths and line numbers
- Exact code changes needed
- Verification commands for each bead
- Batch execution workflow
- Testing strategy
- Rollback procedures
- Time estimates

**Audience**: Developers executing the beads
**Size**: 9.8 KB (339 lines)

### 4. `CONTENT_SMELLS_SUMMARY.txt`
**Executive summary** - Quick reference and status overview.

**Contains**:
- Executive summary of all findings
- Quick stats (5 beads, 85 min effort)
- Content smell descriptions (digest format)
- Quality gates checklist
- Quick start instructions
- File locations index
- Validation commands
- Next steps workflow

**Audience**: Everyone - start here
**Size**: 9.7 KB (284 lines)

### 5. This File (`CONTENT_SMELLS_README.md`)
**Navigation and index** - How to use these deliverables.

## Quick Start

### For Busy Readers (5 minutes)
1. Read `CONTENT_SMELLS_SUMMARY.txt`
2. Skim "Content Smells" section for overview
3. Check "Quick Start" for execution steps

### For Implementers (30 minutes)
1. Read `CONTENT_SMELLS_EXECUTION_GUIDE.md`
2. Pick a bead and execute step-by-step
3. Run verification commands
4. Move to next bead

### For Auditors (60 minutes)
1. Read `CONTENT_SMELLS_ANALYSIS.md` in full
2. Review `CONTENT_SMELLS_BEADS.jsonl` for data structure
3. Verify each bead success criteria
4. Assess quality gate completeness

### For Automation (5 seconds)
```bash
# Import beads directly
bd create --from CONTENT_SMELLS_BEADS.jsonl --batch content-smells

# Execute workflow
bd ready --filter tag:code-smell --json
```

## Bead Summary

| ID | Title | Priority | Effort | Lines Changed |
|---|---|---|---|---|
| cs-001 | Add KIRK prefix to parse | **Critical** (0) | 5 min | 1 |
| cs-002 | Fix EARS pattern count | **Critical** (0) | 10 min | 1 |
| cs-003 | Make ai_readiness explicit | **Critical** (0) | 20 min | 4 |
| cs-004 | Document mental models | High (1) | 30 min | 5 |
| cs-005 | Define orphaned behaviors | High (1) | 20 min | 2 |
| **TOTAL** | | **3 Critical + 2 High** | **85 min** | **~13 lines** |

## File Locations

All changes are in **one file**:
```
src/intent/cli_text_constants.gleam
```

Specific line numbers:
- Line 84: `cmd_parse_desc` (Bead 1)
- Line 1695: EARS pattern count (Bead 2)
- Line 66: `cmd_quality_desc` (Bead 3)
- Line 1454-1457: Quality dimensions (Bead 3)
- Line 867-872: Beads-regenerate strategies (Bead 4)
- Line 1648: Effects orphaned behaviors (Bead 5)

## Execution Workflow

### Phase 1: Pre-Execution (5 minutes)
```bash
# Review deliverables
cat CONTENT_SMELLS_SUMMARY.txt
cat CONTENT_SMELLS_EXECUTION_GUIDE.md

# Verify bead format
jq . CONTENT_SMELLS_BEADS.jsonl | head -50
```

### Phase 2: Execution (85 minutes)
```bash
# Import beads to system
bd create --from CONTENT_SMELLS_BEADS.jsonl --batch content-smells

# Execute each bead (example for Bead 1)
bd update cs-001-parse-kirk-prefix --status in_progress
# ... make code change ...
gleam format src/intent/cli_text_constants.gleam
bd close cs-001-parse-kirk-prefix --reason "Added KIRK: prefix"

# Repeat for beads 2-5
```

### Phase 3: Verification (30 minutes)
```bash
# Verify each fix
grep "cmd_parse_desc" src/intent/cli_text_constants.gleam | grep "KIRK:"
grep -A 3 "Recognizes.*patterns:" src/intent/cli_text_constants.gleam
grep "cmd_quality_desc" src/intent/cli_text_constants.gleam | grep "AI-readiness"
grep -A 10 "strategy STRATEGY" src/intent/cli_text_constants.gleam | grep -i "mental"
grep "orphaned behaviors" src/intent/cli_text_constants.gleam

# Build and test
gleam build && gleam test
```

### Phase 4: Quality Gates (15 minutes)
```bash
# Syntax check
gleam format --check src/intent/cli_text_constants.gleam

# Lint check
gleam lint

# Help text verification
intent parse --help | grep -i "kirk"
intent quality --help | grep -i "ai-readiness"
intent effects --help | grep -i "orphaned"
```

**Total Time**: ~2 hours (85 min execution + 35 min verification/gates)

## Quality Gates - Before Merge

- [ ] All 5 beads executed and closed with `success` status
- [ ] No syntax errors in `src/intent/cli_text_constants.gleam`
- [ ] `gleam build` completes successfully
- [ ] `gleam test` passes all tests
- [ ] Lint passes: `gleam lint`
- [ ] Help text displays correctly: `intent <cmd> --help`
- [ ] No regressions in other commands
- [ ] Code review approved

## Success Criteria

✓ **Bead 1** (cs-001): `parse` description includes "KIRK:" prefix
✓ **Bead 2** (cs-002): EARS pattern count matches documentation
✓ **Bead 3** (cs-003): ai_readiness is explicit 4th dimension
✓ **Bead 4** (cs-004): Each strategy has mental model explanation
✓ **Bead 5** (cs-005): "Orphaned behaviors" is defined in context

## Risk Assessment

**Risk Level**: LOW ✓

**Reasons**:
- Single file affected (no cross-file dependencies)
- Documentation-only changes (no logic changes)
- Simple text updates (low complexity)
- No breaking API changes
- Existing tests unaffected
- Easy rollback available

**Mitigation**:
- Execute in single session for consistency
- Run full test suite after changes
- Code review before merge
- Easy git rollback if needed

## Related Commands

Once beads are executed, verify with these commands:

```bash
# View parse command
intent parse --help

# View quality command
intent quality --help

# View effects command
intent effects --help

# View beads-regenerate command
intent beads-regenerate --help

# View ears command
intent ears --help
```

## Rollback Strategy

If issues occur:

```bash
# Rollback to main branch version
git checkout src/intent/cli_text_constants.gleam

# Or restore from backup
cp src/intent/cli_text_constants.gleam.backup src/intent/cli_text_constants.gleam

# Or view specific changes
git show HEAD:src/intent/cli_text_constants.gleam | head -100
```

## Estimated Impact

**Positive**:
- Improved API consistency
- Clearer command descriptions
- Better user guidance
- Defined technical terms
- Complete documentation

**Negative**: None identified

**Performance**: No impact (documentation-only)

**Breaking Changes**: None

## Next Steps

### Immediate (Today)
1. [ ] Read this README
2. [ ] Review `CONTENT_SMELLS_SUMMARY.txt`
3. [ ] Understand scope of changes

### Short Term (This Week)
1. [ ] Schedule execution session
2. [ ] Execute 5 beads following guide
3. [ ] Verify all fixes
4. [ ] Code review
5. [ ] Merge to main

### Long Term (Future)
- [ ] Review 6-11 additional content issues (2-3 hours)
- [ ] Establish documentation review checklist
- [ ] Create CLI consistency audit process
- [ ] Monitor for new content smells

## Support & Questions

For questions about:
- **What to do**: See `CONTENT_SMELLS_EXECUTION_GUIDE.md`
- **Why it matters**: See `CONTENT_SMELLS_ANALYSIS.md`
- **Quick reference**: See `CONTENT_SMELLS_SUMMARY.txt`
- **Automation**: See `CONTENT_SMELLS_BEADS.jsonl`

## Files Index

```
CONTENT_SMELLS_BEADS.jsonl           Machine-readable beads (5 items)
CONTENT_SMELLS_ANALYSIS.md           Comprehensive analysis (385 lines)
CONTENT_SMELLS_EXECUTION_GUIDE.md   How-to guide (339 lines)
CONTENT_SMELLS_SUMMARY.txt           Executive summary (284 lines)
CONTENT_SMELLS_README.md             This file (navigation)
```

**Total Documentation**: ~1,300 lines, 42 KB

## Version History

| Version | Date | Status |
|---|---|---|
| 1.0 | 2026-01-18 | Initial Release - Ready for Execution |

## Audit Attribution

**Source**: Quality Auditor v1.0 (Content Smells Detection)
**Audit Score**: 84.5/100
**Issues Found**: 5 (3 Critical, 2 High)
**Recommendation**: Execute all beads before next release

---

**Start Here**: Read `CONTENT_SMELLS_SUMMARY.txt` (5 min) → Execute from `CONTENT_SMELLS_EXECUTION_GUIDE.md` → Verify using quality gates above

**Status**: Ready for execution
**Last Updated**: 2026-01-18
