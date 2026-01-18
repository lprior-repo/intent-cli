# Intent CLI Beads Execution Plan

**Generated:** 2026-01-18
**Total Beads:** 21
**Critical Path:** 8 beads (Phase A: must execute first)
**High Priority:** 5 beads (Phase B: v1.1 timeline)
**Medium Priority:** 4 beads (Phase C: optimization)
**Backlog:** 4 beads (Phase D: future)

## Phase A: CRITICAL PRODUCTION FIXES (Immediate)
**Effort:** ~2 hours total
**Blocking:** All production work must wait on Phase A completion

### Why Critical
1. **Safety & Security:** Negative padding (silent failures) and DOS vector in progress bar
2. **Data Integrity:** Timeout configuration being lost in merge operations
3. **Documentation Accuracy:** Help text inconsistencies and documentation errors confuse users
4. **User Experience:** Inconsistent KIRK command naming violates CLI standards

### Execution Order (Sequential)
1. **CRITICAL-004** - Add "KIRK: " prefix to parse command (5 min)
   - Smallest fix first for quick confidence boost
   - No dependencies

2. **CRITICAL-005** - Fix EARS pattern count documentation (10 min)
   - Updates help text accuracy
   - Simple text edit
   - Requires: nothing

3. **CRITICAL-006** - Add ai_readiness dimension to quality help (5 min)
   - Completes documentation fix sequence
   - Simple addition to help text
   - Requires: nothing

4. **CRITICAL-001** - Fix center_in_box negative padding (15 min)
   - First code change (bounds check)
   - No dependencies
   - Prevents silent layout failures

5. **CRITICAL-002** - Fix progress_bar_with_width DOS vector (15 min)
   - Parallel with CRITICAL-001 (independent modules)
   - Width validation + clamping
   - Prevents resource exhaustion

6. **CRITICAL-003** - Fix config timeout merge data loss (10 min)
   - Config module fix
   - Critical logic correction
   - Requires: nothing

7. **CRITICAL-007** - Add bounds validation tests for center_in_box (20 min)
   - Depends on: CRITICAL-001
   - Validates oversized text handling
   - Unit test coverage

8. **CRITICAL-008** - Add validation tests for progress_bar_with_width (20 min)
   - Depends on: CRITICAL-002
   - Boundary testing (0, 1, 100, 101)
   - DOS vector validation

**Total Phase A Effort:** ~1.75 hours (105 minutes)
**Gate Criteria:** All CRITICAL beads must pass tests before Phase B starts

---

## Phase B: HIGH-PRIORITY v1.1 IMPROVEMENTS (Weeks 2-4)
**Effort:** 12-15 hours total
**Team:** Requires 1-2 developers, potential test specialist
**Blocking:** None (can run in parallel with other work)

### Focus Areas
1. **Test Suite Quality** (240 minutes total)
   - HIGH-001: Parametrize help_text_test (120 min) - eliminates duplication
   - HIGH-002: Add integration tests for --help (180 min) - comprehensive coverage
   - HIGH-003: Flag coverage tests for 17 missing flags (120 min) - reach 100%

2. **Workflow Automation** (480 minutes total)
   - HIGH-004: Phase gate automation framework (480 min)
   - HIGH-005: Parallelization strategy (240 min planning)

### Execution Strategy
```
Wave 1 (Parallel):
  ├─ HIGH-001: Parametrize help_text_test (120 min)
  ├─ HIGH-005: Design parallelization strategy (240 min planning)
  └─ MEDIUM-002: Phase gate validation script (90 min)

Wave 2 (After Wave 1):
  ├─ HIGH-002: Integration tests for --help (180 min)
  └─ HIGH-004: Phase gate automation (480 min)

Wave 3 (After Wave 2):
  └─ HIGH-003: Flag coverage tests (120 min)
```

### Success Criteria
- [x] Help text test duplication reduced from 95% to < 10%
- [x] All 24 commands have --help integration tests
- [x] Flag coverage reaches 100% (30+ flags tested)
- [x] Phase gate automation scripts working
- [x] Parallelization strategy documented with impact estimates

---

## Phase C: OPTIMIZATION & REFACTORING (Month 2)
**Effort:** 6.5 hours total
**Priority:** Medium (nice-to-have, improves maintainability)
**Blocking:** None

### Tasks
1. **MEDIUM-001** - Plan plan.gleam module split (240 min)
   - Design: wave_calculator, dependency_resolver, risk_analyzer, effort_estimator
   - Address scalability as LOC approaches 3.5K
   - No code changes yet, design document only

2. **MEDIUM-003** - Error handler exit code standardization (120 min)
   - Audit all 24 commands
   - Document standard exit codes: 0=success, 1=fail, 2=blocked, 3=invalid, 4=error
   - Create compliance checklist

3. **MEDIUM-004** - Organize emoji_constants with grouping (60 min)
   - Group 40+ constants into: status_icons, severity_icons, ui_elements, etc.
   - Add documentation comments per group
   - Improve discoverability

### Execution
Wave 1 (Parallel):
- MEDIUM-001: Plan split strategy
- MEDIUM-003: Audit exit codes
- MEDIUM-004: Organize emoji constants

---

## Phase D: BACKLOG (v1.2+)
**Effort:** 9 hours total
**Priority:** Low (nice-to-have improvements)

1. **BACKLOG-001** - Performance profiling formatter_utils (120 min)
   - After bounds fixes complete
   - Benchmark string.repeat() performance
   - Optimize if > 5ms

2. **BACKLOG-002** - Test fixtures for CLI standards (90 min)
   - Reusable fixtures for emoji validation
   - Formatter output fixtures
   - Help text verification fixtures

3. **BACKLOG-003** - CLI Consistency Standards handbook (150 min)
   - Comprehensive documentation
   - Examples for all 6 modules
   - Integration patterns guide
   - New command checklist

---

## Dependency Graph

```
CRITICAL-004 ──┐
CRITICAL-005 ──┤
CRITICAL-006 ──┤
CRITICAL-001 ──┼──→ CRITICAL-007 ─┐
CRITICAL-002 ──┼──→ CRITICAL-008 ─┤
CRITICAL-003 ──┘                    └──→ [Phase B start]

HIGH-001 ────┐
HIGH-005 ────┤──→ HIGH-004 ──→ [Testing & Automation Complete]
MEDIUM-002 ──┘

HIGH-002 ────┐
HIGH-003 ────┴──→ [Integration Complete]

[Phase A Complete] ──→ [Phase B Parallel Execution] ──→ [Phase C Optimization]
```

---

## Rollout Timeline

### Immediate (Today)
- Commit: CRITICAL-004, CRITICAL-005, CRITICAL-006 (documentation fixes)
- Commit: CRITICAL-001, CRITICAL-002, CRITICAL-003 (code fixes)
- Commit: CRITICAL-007, CRITICAL-008 (test coverage)

### v1.0.1 Release (Next week)
- All CRITICAL beads merged and tested
- Test suite reports updated
- Exit code: 0 (all tests pass)

### v1.1 Planning (Weeks 2-4)
- HIGH-001: Parametrized test suite
- HIGH-002: Integration tests for --help
- HIGH-003: 100% flag coverage
- HIGH-004: Phase gate automation
- HIGH-005: Parallelization strategy designed

### v1.1 Release (End of month)
- All Phase B beads complete
- Test coverage: 95%+
- Automation: Phase gates enforced
- Documentation: Updated

### v1.2 (Future)
- Phase C & D beads as time permits
- Performance optimizations
- Handbook and fixtures

---

## Quality Metrics

### Before Phase A
- Bug Count: 6 (3 critical, 3 documentation)
- Test Coverage: 58%
- Code Health: 82/100 (architecture)

### After Phase A
- Bug Count: 0
- Test Coverage: 65-70% (after critical-007, critical-008)
- Code Health: 90/100

### After Phase B
- Bug Count: 0
- Test Coverage: 95%+
- Code Health: 92/100

---

## Risk Analysis

### Phase A Risks
- **Low Risk:** All changes are isolated bug fixes
- **Mitigation:** Each fix has focused test coverage
- **Rollback Plan:** Simple git revert of each commit

### Phase B Risks
- **Medium Risk:** Test parametrization could introduce new test failures
- **Mitigation:** Run full test suite after each HIGH bead
- **Rollback Plan:** Revert to previous test framework if issues arise

### Phase C Risks
- **Low Risk:** Design and refactoring, no user-facing changes
- **Mitigation:** Design review before implementation
- **Rollback Plan:** Keep old modules until new ones proven stable

---

## Metrics & Success Criteria

### Phase A Success
✓ All 8 critical beads passing tests
✓ No regression in existing functionality
✓ Help text consistency verified
✓ Exit code: 0 for all tests

### Phase B Success
✓ Help text test duplication < 10%
✓ Integration tests for all 24 commands
✓ Flag coverage 100%
✓ Phase gate automation working
✓ Parallelization strategy documented

### Phase C Success
✓ plan.gleam split strategy documented
✓ Exit code audit complete
✓ emoji_constants organized

### Phase D Success
✓ Performance profiled and optimized
✓ Test fixtures published
✓ Handbook available

---

## Notes for Execution

1. **CRITICAL beads must complete before Phase B** - Non-negotiable gate
2. **Run full test suite after each Phase A bead** - Ensure no regressions
3. **Phase B beads can execute in parallel** (see Wave structure)
4. **Phase C beads are planning/design only** - No breaking changes
5. **Backlog beads execute as bandwidth allows** - Nice-to-have improvements

---

## Questions Before Starting?

- Priority disputes? All priorities set by auditor findings + security concerns
- Estimate adjustments? Times based on auditor experience reports
- Dependency questions? See Dependency Graph section above
- Risk concerns? See Risk Analysis section above

**Ready to execute Phase A? Start with CRITICAL-004 (5-min documentation fix)**
