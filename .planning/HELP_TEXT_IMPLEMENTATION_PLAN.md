# Help Text Implementation Plan - 8 Phase Delivery

## Executive Summary

Transform Intent CLI from partial (7/24 commands complete) to comprehensive AI-friendly help system.

**Status**: Phase 1 - Planning (IN PROGRESS)

---

## Phase 1: Planning Phase ✓ COMPLETE

### 1.1 Commands Needing Extended Help (17 total)

**Category A: Quality Analysis Commands (4)**
- `lint` - Anti-pattern detection
- `analyze` - Quality scoring
- `improve` - Improvement suggestions
- `doctor` - Health report + prioritized fixes

**Category B: Workflow Commands (6)**
- `interview` - Guided specification discovery
- `beads` - Generate work items from spec
- `bead-status` - Mark bead completion status
- `history` - View session history
- `diff` - Compare session changes
- `sessions` - List all sessions

**Category C: KIRK Analysis Commands (6)**
- `quality` - KIRK quality analysis
- `invert` - KIRK failure mode analysis
- `coverage` - KIRK coverage analysis (OWASP)
- `gaps` - KIRK gap detection
- `effects` - KIRK second-order effects
- `ears` - EARS requirement parsing

**Category D: Other Commands (1)**
- `parse` - Parse JSON/CUE files

### 1.2 Extended Help Template (for all 24 commands)

```
WHAT IT DOES
  [Single paragraph explaining core functionality]

WHY YOU'D USE IT
  [2-3 use cases or workflow scenarios]

WHEN TO USE IT
  [When this command fits in the workflow]

PREREQUISITES
  [Required setup or dependencies]

USAGE EXAMPLES
  [6-8 realistic examples with brief description]
  Example 1: Simple case
    $ intent COMMAND <spec.cue>

  Example 2: With flags
    $ intent COMMAND <spec.cue> --flag value

FLAG DETAILS
  --flag-name Description
    - Supports X, Y, Z
    - Cannot combine with: --other-flag
    - Default: value
    - Environment: INTENT_FLAG_NAME

EXIT CODES
  0 = Success
  1 = Spec validation failed
  2 = Test execution blocked
  3 = Invalid arguments
  4 = Runtime error

SEE ALSO
  intent other-command - Brief description of related command
```

### 1.3 Implementation Order (Dependencies First)

**Phase 2 Priority Order**:
1. Quality commands (lint, analyze, improve, doctor) - foundational
2. Workflow commands (interview, beads, bead-status) - core operations
3. History/diff/sessions - workflow context
4. KIRK commands (6 total) - analysis layer
5. Parse command - utility

### 1.4 File Modification Strategy

**cli_text_constants.gleam** expansion:
```
Current:  ~950 lines (7 extended help texts)
Target:   ~2,200 lines (24 extended help texts)
Growth:   +250 lines per command average
```

**Split into commits**:
- Commit 1: Quality commands (lint, analyze, improve, doctor) + tests
- Commit 2: Workflow commands (interview, beads, bead-status, history) + tests
- Commit 3: Workflow continued (diff, sessions) + KIRK intro
- Commit 4: KIRK commands (quality, invert, coverage, gaps, effects, ears) + parse
- Commit 5: help_sections module creation + refactoring

### 1.5 Testing Strategy

**Test files to create**:
- `test/help_text_test.gleam` - Unit tests for help availability
- `scripts/test-help-coverage.sh` - All 24 commands respond to --help
- `scripts/test-help-sections.sh` - All sections present in extended help
- `scripts/test-help-examples.sh` - Example syntax validation
- `scripts/test-help-consistency.sh` - Consistency checks across commands

### 1.6 Validation Checklist

- [ ] 24/24 commands have 1-line descriptions (already done)
- [ ] 24/24 commands have extended help in cli_text_constants.gleam
- [ ] All extended help follows template structure
- [ ] All 30+ flags documented with descriptions
- [ ] All flags include examples in descriptions
- [ ] No inline io.println("Usage:") in intent.gleam
- [ ] All tests passing
- [ ] LLM assessment score ≥85/100

---

## Phase 2: Implementation Phase (READY TO START)

**Current Status**: Not started

**What needs to happen**:
1. Expand cli_text_constants.gleam with 17 new extended help texts
2. Migrate inline usage messages to centralized system
3. Create help_sections module for formatting
4. Enhance flag descriptions with examples
5. Update intent.gleam commands to use new help text

**Files to modify**:
- src/intent/cli_text_constants.gleam (add 2,100+ lines)
- src/intent/formatter_utils.gleam (add help section functions)
- src/intent/cli_flags.gleam (enhance flag descriptions)
- src/intent.gleam (update command definitions)

**Estimated size**: 2,500+ new lines of help text across 4 commits

---

## Phase 3: Testing Phase (READY AFTER PHASE 2)

**What needs to happen**:
1. Create test/help_text_test.gleam with:
   - Test all 24 commands have descriptions
   - Test extended help exists for all
   - Test no inline usage messages
2. Create scripts/test-help-*.sh test suite
3. Run manual testing: `intent COMMAND --help`

---

## Phase 4: Code Review Phase (READY AFTER PHASE 3)

**What needs to happen**:
1. Style consistency review across all 24 commands
2. Grammar and technical accuracy checks
3. Consistency verification (terminology, patterns, formatting)
4. Architecture verification (no duplication, DRY principle)

---

## Phase 5: Deep Interrogation Phase (READY AFTER PHASE 4)

**What needs to happen**:
1. Adversarial testing of all commands
2. Edge case testing (many flags, zero flags, etc.)
3. Cross-command consistency verification
4. Terminal width testing (40, 80, 120+ chars)

---

## Phase 6: Validation Phase (READY AFTER PHASE 5)

**What needs to happen**:
1. Completeness validation against checklist
2. Consistency validation across all commands
3. Generate VALIDATION_REPORT.md with metrics

---

## Phase 7: LLM Quality Assessment (READY AFTER PHASE 6)

**What needs to happen**:
1. Use Claude Opus to evaluate all 24 commands
2. Score: clarity, completeness, consistency, AI-friendliness, accuracy, usability
3. Produce LLM_QUALITY_ASSESSMENT.md with recommendations

---

## Phase 8: Iteration & Polish (READY AFTER PHASE 7)

**What needs to happen**:
1. Fix issues identified in LLM assessment
2. Final validation and testing
3. Generate final implementation report

---

## Success Metrics

| Metric | Target | Current |
|--------|--------|---------|
| Commands with extended help | 24/24 | 7/24 |
| Flag descriptions with examples | 30+/30+ | ~10/30 |
| Inline usage messages removed | 0 | ~5 |
| Test coverage | 100% | ~50% |
| LLM assessment score | ≥85/100 | TBD |

---

## Next Steps

1. ✓ Phase 1 Planning complete
2. → Begin Phase 2: Implementation (add extended help for 17 commands)
3. → Phase 3: Test all 24 commands
4. → Phase 4: Code review and fixes
5. → Phase 5: Interrogation testing
6. → Phase 6: Validation report
7. → Phase 7: LLM assessment
8. → Phase 8: Final polish

**Ready to proceed with Phase 2**
