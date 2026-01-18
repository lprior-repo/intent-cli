# Beads Execution Guide
## Multi-Agent Evaluation Findings to Executable Work Units

**Created**: 2026-01-18
**Source**: Comprehensive Multi-Agent Evaluation Report (6 agents)
**Overall Status**: APPROVED FOR PRODUCTION (85.4/100)
**Critical Timeline**: 2 hours CRITICAL FIXES → 9-15 hours HIGH PRIORITY → v1.1-v1.2 MEDIUM PRIORITY

---

## Executive Summary

The multi-agent evaluation identified **21 actionable beads** across 3 priority levels:

| Phase | Beads | Effort | Timeline | Status |
|-------|-------|--------|----------|--------|
| **PHASE A: CRITICAL** | 8 beads | 95 min | **2 hours** | BLOCKING DEPLOYMENT |
| **PHASE B: HIGH** | 10 beads | 610 min | **9-15 hours** | v1.0→v1.1 release |
| **PHASE C: MEDIUM** | 3 beads | 315 min | **5-8 hours** | v1.1→v1.2 planning |
| **TOTAL** | **21 beads** | **1020 min** | **16-25 hours** | |

---

## PHASE A: CRITICAL PRODUCTION FIXES (2 hours)

These 8 beads BLOCK production deployment. Execute immediately.

### Dependency Graph

```
critical-fix-center-box-negative-padding          ┐
critical-fix-progress-bar-width-validation         │
critical-fix-config-timeout-merge-logic            ├─→ critical-rebuild-and-verify-fixes
critical-fix-parse-command-kirk-prefix             │
critical-fix-ears-pattern-count-doc                │
critical-fix-quality-command-ai-readiness-dimension┘
```

### Beads Detail

#### 1. Fix center_in_box negative padding (10 min) ⚠️ SECURITY
- **File**: `src/intent/formatter_utils.gleam`
- **Issue**: `center_in_box()` silently fails when text longer than box width, creating negative padding
- **Fix**: Add validation to handle oversized text (truncate or return error)
- **Tests**: Verify negative padding produces valid output
- **Risk**: MEDIUM - affects box rendering in multiple commands

#### 2. Fix progress_bar_with_width unvalidated width (10 min) ⚠️ SECURITY
- **File**: `src/intent/formatter_utils.gleam`
- **Issue**: Extremely large width values cause DOS via string allocation
- **Fix**: Clamp width to range 1-200 or validate before use
- **Tests**: Test with 0, -1, 1000000 width values
- **Risk**: MEDIUM - potential DOS vector (low probability due to internal use only)

#### 3. Fix config timeout merge logic (15 min) 🐛 BUG
- **File**: `src/intent/config.gleam`
- **Issue**: `merge_with_flags()` always uses override timeout_ms, ignores env config
- **Current Code (line 65)**:
  ```gleam
  timeout_ms: overrides.timeout_ms,  // BUG: Always overrides, no fallback
  ```
- **Fix**: Implement fallback semantics like other fields:
  ```gleam
  timeout_ms: case overrides.timeout_ms == 30_000 {
    True -> base.timeout_ms   // Use env if override is default
    False -> overrides.timeout_ms
  }
  ```
- **Tests**: Test env→flag override priority, default fallback behavior
- **Risk**: MEDIUM - affects configuration for all commands

#### 4. Add KIRK: prefix to parse command (5 min) 📝 CONTENT
- **File**: `src/intent/cli_text_constants.gleam`
- **Issue**: Parse command missing "KIRK:" prefix (unlike ears command)
- **Current**: `pub const cmd_parse_desc = "Parse EARS requirements to structured spec"`
- **Fix**: Change to `pub const cmd_parse_desc = "KIRK: Parse EARS requirements to structured spec"`
- **Rationale**: Parse is part of KIRK analysis suite; inconsistent with ears command
- **Tests**: help_text_test verifies prefix presence
- **Risk**: LOW - documentation only

#### 5. Fix ears pattern count documentation (5 min) 📝 CONTENT
- **File**: `src/intent/cli_text_constants.gleam` + `src/intent/kirk/ears_parser.gleam`
- **Issue**: Documentation lists 4 patterns but system supports 5: Ubiquitous, Event-Driven, State-Driven, Optional, Unwanted (+ Complex)
- **Current Confusion**: Help text may say "4 patterns" but code implements 5
- **Fix Options**:
  - Option A: Update help text to accurately list all 5 patterns
  - Option B: Add note explaining Unwanted vs Optional distinction
  - Recommended: Option A (clear is better)
- **Code Reference**: `ears_parser.gleam` lines 30-42 define 6 types (including Complex)
- **Tests**: Verify help text matches pattern count
- **Risk**: LOW - documentation only

#### 6. Add ai_readiness dimension to quality command (20 min) ✨ FEATURE
- **File**: `src/intent/kirk/quality_analyzer.gleam`
- **Issue**: Quality command currently outputs only 3 dimensions (coverage, clarity, testability) but should include ai_readiness
- **Current Dimensions**: Coverage, Clarity, Testability
- **New Dimension**: AI Readiness (measures spec's readiness for AI implementation)
- **Implementation**:
  - Add `ai_readiness` field to quality analysis output
  - Implement scoring logic (similar to other dimensions)
  - Update output formatting to display 4 scores
  - Update tests to validate dimension calculation
- **Tests**: Verify all 4 dimensions calculated correctly, formatted properly
- **Risk**: LOW - additive feature, doesn't break existing functionality

#### 7. Rebuild and verify fixes (10 min) ✅ VERIFICATION
- **Trigger**: After all 6 fixes above complete
- **Actions**:
  ```bash
  gleam build          # Should show 0 warnings
  gleam test           # Should pass 1485/1485
  ```
- **Success Criteria**:
  - ✅ Build compiles with 0 warnings
  - ✅ All 1485 tests pass
  - ✅ No regressions in existing features
  - ✅ Help text system functional
- **Verification**: Run integration smoke test for all 24 commands

### PHASE A Timeline

```
Fix center_in_box             [10 min]
Fix progress_bar_width        [10 min]  (parallel)
Fix config timeout merge      [15 min]  (parallel)
Fix parse KIRK prefix         [5 min]   (parallel)
Fix ears pattern docs         [5 min]   (parallel)
Add quality ai_readiness      [20 min]  (parallel, may need longer)
Rebuild & verify              [10 min]  (sequential, after all above)
─────────────────────────────────────
Total: ~95 minutes (2 hours worst case)
Critical Path: 20 min (ai_readiness feature is longest single item)
Parallelizable: 5 items can run in parallel = 20 min
Actual Estimate: 20 + 10 (rebuild) = **30 minutes parallel execution**
```

---

## PHASE B: HIGH PRIORITY v1.0→v1.1 (9-15 hours)

Complete these 10 beads in v1.0 patch release. Start after PHASE A deployment.

### Dependency Graph

```
high-analyze-help-text-test-structure
    ↓
high-design-parametrization-approach
    ↓
high-refactor-duplicate-test-patterns (120 min)

high-add-integration-test-framework ────→ high-add-integration-tests-all-24-commands (90 min)

high-flag-coverage-test-framework
    ↓
high-complete-flag-coverage-100-percent (120 min)

high-implement-8-content-improvements (120 min)

high-implement-phase-gate-automation (120 min)
```

### Test Suite Improvements (4 beads, 245 min / ~4 hours)

#### 1. Analyze help_text_test.gleam duplication (15 min)
- **File**: `test/help_text_test.gleam`
- **Current State**: 116 tests with 95% duplication
- **Pattern**: Each command gets identical 4-test suite:
  - Test exists (e.g., `check_command_description_exists_test`)
  - Test valid length (e.g., `check_description_valid_length_test`)
  - Test positive length helper
  - Test substantial length helper
- **Deliverable**: Quantified duplication %, proposed consolidation (116→25-30 tests)

#### 2. Design parametrization approach (10 min)
- **Input**: Analysis from bead #1
- **Output**: Design document (.planning/test-parametrization-design.md)
- **Content**:
  - Data structure for test cases (command, desc, flags, etc.)
  - Parametrization algorithm
  - Migration path (116→25-30)
  - Estimated time savings

#### 3. Refactor tests to eliminate duplication (120 min)
- **Refactor**: 116 repetitive tests → 25-30 parametrized tests
- **Maintain**: 100% test pass rate, 100% command coverage
- **Result**: Net LOC reduction, easier maintenance
- **Parallelizable**: No (depends on design)

#### 4. Design integration test framework (60 min)
- **Challenge**: Tests currently don't verify actual `--help` output
- **Gap**: Help text could be missing without detection
- **Solution**: Framework to invoke CLI and capture/validate `--help` output
- **Components**:
  - CLI invocation wrapper
  - Output capture mechanism
  - Validation helpers (check text presence, structure)
  - Error handling
- **Deliverable**: Runnable framework + 1 sample test for proof-of-concept

#### 5. Add integration tests for all 24 commands (90 min)
- **Test Each Command**: Invoke `intent <command> --help` and verify:
  - Help text present
  - WHAT/WHY/WHEN structure in output
  - Flag descriptions displayed
  - Examples present (if applicable)
- **Success**: 24/24 commands have working integration tests
- **Pass Rate**: 100% (1485+ total tests)

#### 6. Design flag coverage framework (20 min)
- **Current**: 6/23 flags tested (26% coverage)
- **Gap**: 17 untested flags (target_url, profile, timeout_ms, etc.)
- **Design**: Test structure for all flag types (string, bool, int, enum)
- **Coverage Plan**: Environment vars, parsing, defaults, validation, merging
- **Deliverable**: Design document

#### 7. Complete flag coverage to 100% (120 min)
- **Add tests for**: All 17 missing flags
- **Test scenarios**:
  - Flag parsing from command line
  - Environment variable override
  - Default values
  - Validation (e.g., profile must be one of 6 options)
  - Flag merge behavior (env + flags)
- **Result**: 26% → 100% flag coverage

### Content & Documentation (2 beads, 240 min / ~4 hours)

#### 8. Implement 8 Quality Auditor improvements (120 min)
- **Issue 1**: Beads-Regenerate strategies not explained
- **Issue 2**: Effects: "Orphaned behaviors" term undefined
- **Issue 3**: Interview: Profile selection guide missing
- **Issue 4**: Gaps: Gap types not enumerated
- **Issue 5**: EARS/Parse: Pattern examples missing
- **Issue 6**: Sessions: Storage location undocumented
- **Issue 7**: Bead-Status: State machine not visualized
- **Issue 8**: Diff: Output format not shown

**Actions for each**:
- Add clear explanation to cli_text_constants help text
- Include examples where helpful
- Define undefined terms
- Document configuration/output format
- Add visual diagrams or structured examples

#### 9. Implement phase gate automation (120 min)
- **Gates to Automate** (7 total):
  1. Build compilation (gleam build → 0 warnings)
  2. Test execution (gleam test → 100% pass)
  3. Code review approval
  4. Regression detection
  5. Consistency validation
  6. LLM assessment (optional)
  7. Production readiness sign-off
- **Implementation**: Bash script or CI/CD pipeline
- **Deliverable**: `scripts/phase-gates.sh` with clear pass/fail criteria
- **Goal**: 33% time savings through automation

### PHASE B Timeline

```
Analysis & Design (sequential):
  Analyze test duplication              [15 min]
  Design parametrization                [10 min]
  Design flag coverage framework        [20 min]
  Design integration framework          [60 min]
Total design: 105 min (~2 hours)

Implementation (can parallelize after design):
  Refactor tests (duplicate elimination)   [120 min] A
  Add integration tests (all 24 commands)  [90 min]  B
  Complete flag coverage                    [120 min] C
  Implement 8 content improvements          [120 min] D
  Implement phase gate automation           [120 min] E

Parallel execution:
  A, B, C, D, E can run in parallel after design
  Longest item: 120 min (tie between A, C, D, E)
  Actual time: 2 hours design + 2 hours parallel = **~4 hours minimum**

Realistic estimate (accounting for review/testing): **9-15 hours total**
- Design & planning: 2 hours
- Implementation (parallel): 2-4 hours
- Testing & refinement: 2-3 hours
- Integration & buffer: 3-5 hours
```

---

## PHASE C: MEDIUM PRIORITY v1.1→v1.2 (Backlog Planning)

These 3 beads are planning/architecture work. No hard deadline but recommend v1.1 release cycle.

### 1. Plan architecture scaling for 50+ commands (60 min)
- **Current State**: ~240 LOC in formatter_utils.gleam, 2,847 LOC total
- **Architect Finding**: Will need split at ~3,500 LOC for 50+ commands
- **Planning Tasks**:
  - Identify module split points
  - Design new module organization
  - Define boundaries & dependencies
  - Growth projections
  - Risk assessment
- **Deliverable**: `.planning/architecture-scaling-50-commands.md`

### 2. Design parallelization strategy (45 min)
- **Opportunity**: 33% time savings identified
- **Areas to Parallelize**:
  - Parallel command execution
  - Concurrent test runs
  - Parallel module compilation
  - Distributed documentation building
- **Deliverable**: `.planning/parallelization-strategy.md` with critical path analysis

### 3. Design quality dashboard (90 min)
- **Purpose**: Real-time monitoring of quality metrics
- **Metrics to Track**:
  - Test pass rate (target: 100%)
  - Code coverage
  - Quality scores (by dimension)
  - Build time
  - Help text completeness (24/24 commands, 23/23 flags)
  - Module LOC growth tracking
- **Deliverable**: `.planning/quality-dashboard-design.md` with visualization mockups

### Optional Enhancements

#### Add semantic validation to tests (120 min)
- Currently tests use brittle substring matching
- Replace with semantic validation:
  - Command structure verification
  - Description semantic validation (action verbs, appropriate length)
  - Cross-reference validation
  - Example syntax checking
- **Result**: Robustness score 45→75+

#### Improve test assertions (60 min)
- Add custom assertion functions with better error messages
- Context-specific debugging output
- Detailed failure diffs
- Clearer test failure diagnostics

---

## Execution Order & Dependencies

### Critical Path Analysis

```
┌─ PHASE A (2 hrs)
│  ├─ 6 critical fixes (parallelizable: 30 min actual)
│  └─ Rebuild & verify (10 min)
│     └─ GATE: Production ready ✓
│
└─ PHASE B (9-15 hrs)
   ├─ Analysis (2 hrs sequential)
   │  ├─ Test duplication analysis
   │  ├─ Flag coverage design
   │  └─ Integration framework design
   │
   ├─ Implementation (2-4 hrs parallel)
   │  ├─ Refactor tests (120 min) → 100% test pass ✓
   │  ├─ Add integration tests (90 min) → Coverage verification ✓
   │  ├─ Complete flag coverage (120 min) → 26%→100% ✓
   │  ├─ Content improvements (120 min) → Quality audit re-pass ✓
   │  └─ Phase gates (120 min) → Workflow automation ✓
   │
   └─ Testing & Integration (3-5 hrs)
      └─ GATE: v1.1 release ready ✓

PHASE C (planning only, ~3-5 hours per item)
   └─ Architecture & infrastructure planning (can start anytime)
```

### Recommended Execution Sequence

**Day 1: PHASE A DEPLOYMENT** (2-3 hours including testing)
```
Morning:
  1. Pull latest code
  2. Fix center_in_box negative padding (10 min)
  3. Fix progress_bar_with_width validation (10 min)
  4. Fix config timeout merge logic (15 min)
  5. Add KIRK: prefix to parse command (5 min)
  6. Fix ears pattern documentation (5 min)
  7. Add ai_readiness dimension (20 min)

Afternoon:
  8. Rebuild & verify (10 min)
  9. Integration smoke test (15 min)
  10. Commit critical fixes
  11. Deploy to production ✓

Review gate before deployment:
  - ✅ All 6 code/doc fixes applied
  - ✅ Build passes with 0 warnings
  - ✅ All 1485 tests pass
  - ✅ No regressions
  - ✅ Help text system functional on all 24 commands
```

**Weeks 2-3: PHASE B v1.1 PREPARATION** (9-15 hours total)
```
Sprint Planning:
  Day 1: Detailed analysis & design (2-3 hours)
    - Analyze test duplication patterns
    - Design parametrization approach
    - Design integration framework
    - Design flag coverage approach

  Days 2-4: Implementation (parallel tracks)
    Track A: Test infrastructure (4-5 hours)
      - Refactor tests (eliminate 95% duplication)
      - Implement integration framework
      - Add integration tests for 24 commands
      - Complete flag coverage tests

    Track B: Documentation & Process (3-4 hours)
      - Implement 8 content improvements
      - Implement phase gate automation
      - Documentation updates

    Track C: QA & Integration (2-3 hours)
      - Verify all new tests pass
      - Integration testing
      - Performance baseline

  Day 5: Release preparation
    - Final smoke tests
    - v1.1 tag & release notes
    - Deploy v1.1 to staging → production
```

**Week 4+: PHASE C PLANNING** (Optional, can start anytime)
```
  - Architecture scaling design
  - Parallelization strategy
  - Quality dashboard mockups
  - Semantic validation enhancement (optional)
  - Advanced test improvements (optional)
```

---

## Success Metrics & Quality Gates

### PHASE A Completion Gate
- ✅ 6 critical fixes applied
- ✅ Build: 0 warnings, 0 errors
- ✅ Tests: 1485/1485 passing
- ✅ Regressions: None detected
- ✅ Help text: All 24 commands functional
- ✅ Auditor review: Approved for production

### PHASE B Completion Gate
- ✅ Test duplication: 116→25-30 tests (77% reduction)
- ✅ Integration tests: 24/24 commands
- ✅ Flag coverage: 26%→100%
- ✅ Content: 8/8 improvements implemented
- ✅ Process: Phase gates automated
- ✅ Tests: 1485+/1485+ passing (new tests added)
- ✅ Quality Auditor: Re-audit shows improvements
- ✅ Test Auditor: Coverage score 58→78+ (target)

### PHASE C Completion Gate (Optional)
- ✅ Architecture scaling plan documented
- ✅ Parallelization strategy with ROI analysis
- ✅ Quality dashboard design complete
- ✅ Implementation roadmap for v1.2+

---

## Risk Assessment

### PHASE A Risks (CRITICAL)
| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| center_in_box fix breaks other commands | Medium | High | Test all box rendering in commands |
| progress_bar width clamping too aggressive | Low | Medium | Test with realistic widths (10-80) |
| Config timeout merge breaks env vars | Low | Medium | Test all config scenarios |
| Quality dimension incomplete | Medium | Medium | Add tests for ai_readiness scoring |

**Risk Mitigation**: Comprehensive regression testing after each fix

### PHASE B Risks (HIGH)
| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| Test refactoring introduces bugs | Medium | High | Maintain test pass rate to 100% |
| Integration framework fragile | Medium | High | Design with extensibility in mind |
| Flag tests incomplete | Low | Medium | Use framework to auto-generate coverage |
| Phase gates too strict | Low | Medium | Start with warning-only gates |

**Risk Mitigation**: Extensive test coverage, gradual gate rollout

---

## Effort Breakdown by Category

| Category | Beads | Total Minutes | % of Total |
|----------|-------|---------------|-----------|
| **Code Fixes** | 3 | 35 | 3% |
| **Documentation** | 3 | 30 | 3% |
| **Tests** | 10 | 565 | 55% |
| **Infrastructure** | 4 | 255 | 25% |
| **Planning** | 1 | 135 | 13% |
| **TOTAL** | **21** | **1020** | **100%** |

---

## Resource Allocation

### Team Composition Recommended

**For PHASE A** (2 hours, can be 1 person):
- 1 Senior Developer (QA + leadership)

**For PHASE B** (9-15 hours, best with 2 people):
- 1 Full-stack Developer (code fixes, tests)
- 1 QA/Test Engineer (test framework, integration tests)
- Part-time: Technical Writer (content improvements)

**For PHASE C** (3-5 hours per component, planning only):
- 1 Architect (design reviews)
- 1 Technical Lead (process improvements)

---

## Questions & Escalation

### Before PHASE A Deployment
- [ ] All 6 critical fixes reviewed by senior dev?
- [ ] Build environment tested (gleam build && gleam test)?
- [ ] Staging deployment ready?
- [ ] Rollback plan if regressions detected?

### Before PHASE B Start
- [ ] Team capacity available for 9-15 hours?
- [ ] Test framework design approved?
- [ ] Integration test approach validated?
- [ ] Phase gates tool/platform selected?

### Before PHASE C
- [ ] Is 50+ command scaling needed (roadmap)?
- [ ] Priority: parallelization vs. quality dashboard?
- [ ] Budget for architecture redesign?

---

## Appendix: Bead Manifest

**Total Beads**: 21 (executable work units)
**Format**: BEADS.jsonl (one JSON per line)
**Fields**: id, title, category, priority, effort_minutes, description, success_criteria, file_location, agent_finding, requires[], tags[]

### By Priority
- **Priority 0 (Critical)**: 8 beads (PHASE A)
- **Priority 1 (High)**: 10 beads (PHASE B)
- **Priority 2 (Medium)**: 3 beads (PHASE C + optional)

### By Category
- **code**: 4 beads
- **docs**: 3 beads
- **test**: 9 beads
- **infra**: 4 beads
- **process**: 1 bead

### By Agent Source
- **Auditor**: 4 beads (code quality, security)
- **Quality Auditor**: 5 beads (content, documentation)
- **Test Auditor**: 9 beads (test coverage, integration)
- **Process Auditor**: 2 beads (automation, workflow)
- **Architect**: 1 bead (scaling)

---

**Document Version**: 1.0
**Last Updated**: 2026-01-18
**Status**: READY FOR EXECUTION
**Approval**: Multi-Agent Evaluation Complete ✓
