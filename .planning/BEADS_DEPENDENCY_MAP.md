# Beads Dependency & Execution Map

**Status**: Ready for parallel execution planning
**Total Beads**: 21
**Total Effort**: 1020 minutes (17 hours)
**Critical Path**: 30 minutes (PHASE A) + 240 minutes (PHASE B longest dependency chain)

---

## PHASE A: CRITICAL FIXES DEPENDENCY GRAPH

```
┌──────────────────────────────────────────────────────────────────────┐
│                         PHASE A: CRITICAL FIXES                      │
│                           (2 hours total)                            │
└──────────────────────────────────────────────────────────────────────┘

INDEPENDENT FIXES (can run in parallel):
┌─────────────────────────────┐
│ center_in_box negative pad   │  10 min  ─┐
│ (security issue)            │           │
└─────────────────────────────┘           │
                                          │
┌─────────────────────────────┐           │
│ progress_bar width valid    │  10 min  ─┼─→ critical-rebuild-and-verify-fixes
│ (security issue)            │           │     (10 min)
└─────────────────────────────┘           │
                                          │
┌─────────────────────────────┐           │
│ config timeout merge logic  │  15 min  ─┼─→ PRODUCTION READY ✓
│ (bug fix)                   │           │
└─────────────────────────────┘           │
                                          │
┌─────────────────────────────┐           │
│ parse KIRK: prefix          │  5 min   ─┤
│ (content fix)               │           │
└─────────────────────────────┘           │
                                          │
┌─────────────────────────────┐           │
│ ears pattern documentation  │  5 min   ─┤
│ (content fix)               │           │
└─────────────────────────────┘           │
                                          │
┌─────────────────────────────┐           │
│ quality ai_readiness dim    │  20 min  ─┘
│ (feature addition)          │
└─────────────────────────────┘

CRITICAL PATH (Sequential):
  1. Fix 6 items (longest: 20 min)
  2. Rebuild & verify (10 min)
  ───────────────
  = 30 minutes parallel execution

GATE: Deploy to production ✓
```

---

## PHASE B: HIGH PRIORITY DEPENDENCY GRAPH

```
┌──────────────────────────────────────────────────────────────────────┐
│               PHASE B: HIGH PRIORITY v1.0→v1.1                      │
│                      (9-15 hours total)                              │
└──────────────────────────────────────────────────────────────────────┘

TEST TRACK A: Test Infrastructure Improvements (4 items, 245 min)
┌─────────────────────────────────────┐
│ high-analyze-help-text-test-structure│  15 min
└──────────┬──────────────────────────┘
           │
           ↓
┌─────────────────────────────────────┐
│ high-design-parametrization-approach │  10 min
└──────────┬──────────────────────────┘
           │
           ↓
┌─────────────────────────────────────┐
│ high-refactor-duplicate-test-patterns│  120 min ━┐
└─────────────────────────────────────┘           │
                                                  │
FLAG TRACK B: Flag Coverage (2 items, 140 min)    │
┌─────────────────────────────────────┐           │
│ high-flag-coverage-test-framework   │  20 min   │
└──────────┬──────────────────────────┘           │
           │                                       │
           ↓                                       │
┌─────────────────────────────────────┐           │
│ high-complete-flag-coverage-100pct  │  120 min ━┼→ Merge test suites
└─────────────────────────────────────┘           │  (weekly)
                                                  │
INTEGRATION TRACK C: Integration Tests (2 items, 150 min)
┌─────────────────────────────────────┐           │
│ high-add-integration-test-framework │  60 min   │
└──────────┬──────────────────────────┘           │
           │                                       │
           ↓                                       │
┌─────────────────────────────────────┐           │
│ high-add-integration-tests-all-24   │  90 min ━─┘
└─────────────────────────────────────┘

CONTENT TRACK D: Documentation (1 item, 120 min)
┌─────────────────────────────────────┐
│ high-implement-8-content-improvements│  120 min ━→ Merge test suites
└─────────────────────────────────────┘

PROCESS TRACK E: Workflow (1 item, 120 min)
┌─────────────────────────────────────┐
│ high-implement-phase-gate-automation │  120 min ━→ Merge test suites
└─────────────────────────────────────┘

PARALLEL EXECUTION AFTER DESIGN PHASE:
  Design: 2 hrs sequential (analysis, framework design)
  Impl:   All tracks (A, B, C, D, E) in parallel
          Max(120, 120, 90, 120, 120) = 120 min per track
          Actual parallel: 120 min (4th quarter execution)

  Total PHASE B: 2 hrs design + 2 hrs parallel + 1-2 hrs integration = 5-6 hrs
  Realistic: 9-15 hrs (includes testing, review, refinement cycles)

GATE: v1.1 release candidate ready ✓
```

---

## PHASE C: MEDIUM PRIORITY PLANNING GRAPH

```
┌──────────────────────────────────────────────────────────────────────┐
│          PHASE C: MEDIUM PRIORITY v1.1→v1.2 (Planning)              │
│                      (5-15 hours optional)                           │
└──────────────────────────────────────────────────────────────────────┘

ARCHITECTURE PLANNING (Independent, 60 min)
┌─────────────────────────────────────┐
│ medium-plan-architecture-scaling    │  60 min ━→ v1.1 or v1.2 release
│ 50+ commands, module split at 3.5k  │
└─────────────────────────────────────┘

PERFORMANCE PLANNING (Independent, 45 min)
┌─────────────────────────────────────┐
│ medium-design-parallelization       │  45 min ━→ v1.1 or v1.2 release
│ 33% time savings, critical path     │           (33% sprint reduction)
└─────────────────────────────────────┘

MONITORING PLANNING (Independent, 90 min)
┌─────────────────────────────────────┐
│ medium-design-quality-dashboard     │  90 min ━→ v1.2+ infrastructure
│ Real-time metrics, alerting         │
└─────────────────────────────────────┘

OPTIONAL ENHANCEMENTS (Low priority):
┌─────────────────────────────────────┐
│ Add semantic validation to tests    │  120 min
│ Improve test assertions             │  60 min
└─────────────────────────────────────┘

PARALLEL EXECUTION:
  All 3 planning tasks can run in parallel
  Max(60, 45, 90) = 90 min per planning track
  True parallel: 90 min (2-3 people working)

GATE: v1.2 roadmap defined and planned ✓
```

---

## COMPLETE EXECUTION TIMELINE

### Week 1: PHASE A Deployment (2-3 hours)

```
Day 1 Morning (30-45 min):
  critical-fix-center-box-negative-padding         [10 min]
  critical-fix-progress-bar-width-validation       [10 min]  ┐
  critical-fix-config-timeout-merge-logic          [15 min]  ├─ PARALLEL
  critical-fix-parse-command-kirk-prefix           [5 min]   │ (pick longest)
  critical-fix-ears-pattern-count-doc              [5 min]   │
  critical-fix-quality-command-ai-readiness-dim    [20 min]  ┘

Day 1 Afternoon (20 min):
  critical-rebuild-and-verify-fixes                [10 min]
  Integration smoke test                           [10 min]

Day 1 Evening:
  Code review & approval
  Deploy to production ✓

Total: 2-3 hours including testing & review
```

### Weeks 2-3: PHASE B Implementation (9-15 hours)

```
Day 1 (2-3 hours): Analysis & Design
  high-analyze-help-text-test-structure            [15 min]
  high-flag-coverage-test-framework                [20 min]
  high-add-integration-test-framework              [60 min]
  high-design-parametrization-approach             [10 min]
  Document all findings                            [30-60 min]

Days 2-4 (6-8 hours): Implementation (parallel tracks)
  Track A: Test Infrastructure (can be 1 person)
    high-refactor-duplicate-test-patterns          [120 min]
    high-add-integration-tests-all-24-commands     [90 min]
    high-complete-flag-coverage-100-percent       [120 min]
    Subtotal: 330 min (5.5 hours)

  Track B: Documentation & Process (parallel)
    high-implement-8-content-improvements          [120 min]
    high-implement-phase-gate-automation           [120 min]
    Subtotal: 240 min (4 hours)

  Track C: QA & Integration
    Run full test suite                            [10 min]
    Regression testing                             [30 min]
    Performance baseline                           [30 min]
    Subtotal: 70 min (1 hour)

Day 5 (1-2 hours): Release Preparation
  Final testing & smoke tests                      [30 min]
  Documentation updates                            [30 min]
  v1.1 release tag & notes                         [30 min]
  Deploy staging → production                      [30-60 min]

Total PHASE B: 2-3 hours design + 6-8 hours implementation = 9-15 hours
Depends on: concurrent vs. sequential resource availability
```

### Weeks 4+: PHASE C Planning (Optional)

```
Week 4 (4-6 hours): Planning Only
  medium-plan-architecture-scaling-50-commands     [60 min]
  medium-design-parallelization-strategy           [45 min]
  medium-design-quality-dashboard                  [90 min]
  Optional: semantic validation design             [30-60 min]

Execution Decision Point:
  - If critical path would benefit from parallelization: prioritize
  - If code bloat is concern: prioritize architecture planning
  - Otherwise: backlog for v1.2+ planning

No code changes in PHASE C (planning only)
```

---

## Resource Allocation Matrix

### PHASE A (2-3 hours)

| Role | Tasks | Duration |
|------|-------|----------|
| Senior Dev | All 6 fixes + rebuild | 30 min (parallel) |
| QA/Tester | Integration smoke test | 15 min |
| Tech Lead | Code review & approval | 15 min |
| **Total Person-Hours** | | **1 hour** |

### PHASE B (9-15 hours)

| Role | Tasks | Duration |
|------|-------|----------|
| Full-stack Dev | Tests, flags, integration framework | 6-8 hours |
| QA/Test Engineer | Test parametrization, integration tests, content | 6-8 hours |
| Technical Writer | Content improvements, documentation | 2-3 hours |
| DevOps/Infra | Phase gates, automation | 2-3 hours |
| Tech Lead | Design review, code review | 2-3 hours |
| **Total Person-Hours** | | **18-30 hours** |

### PHASE C (5-15 hours, planning only)

| Role | Tasks | Duration |
|------|-------|----------|
| Architect | Architecture scaling, design review | 3-4 hours |
| Tech Lead | Parallelization strategy, critical path | 2-3 hours |
| DevOps/Infra | Dashboard design, monitoring | 2-4 hours |
| **Total Person-Hours** | | **7-11 hours** |

---

## Critical Assumptions & Constraints

### PHASE A Assumptions
- All 6 fixes are low-risk, localized changes
- No refactoring needed (surgical fixes only)
- Gleam compiler available & tested
- All existing tests will pass

### PHASE B Assumptions
- Test framework can be implemented independently
- Integration testing doesn't require external services
- Phase gates can be automated (not manual)
- Team capacity available for parallel tracks

### PHASE C Assumptions
- Planning only (no implementation)
- Roadmap not finalized (50+ commands not urgent)
- Quality dashboard is nice-to-have

### Risks That Could Extend Timeline
| Risk | Mitigation |
|------|-----------|
| Design parallelization failures | Use sequential for PHASE B if needed |
| Integration test framework too complex | Start with simpler verification approach |
| Phase gates too strict | Implement as warnings initially |
| Content improvements scope creep | Stick to 8 agreed items only |

---

## Success Criteria Summary

### PHASE A (Production Ready)
- ✅ All 6 critical fixes applied
- ✅ Build: 0 warnings
- ✅ Tests: 1485/1485 passing
- ✅ Deployment: Zero issues on prod

### PHASE B (v1.1 Ready)
- ✅ Test suite: 95% duplication eliminated (116→25-30 tests)
- ✅ Integration: 24/24 command tests working
- ✅ Flag coverage: 26%→100%
- ✅ Content: 8/8 improvements implemented
- ✅ Process: Phase gates automated & working
- ✅ Quality: Overall score 85.4%→90%+

### PHASE C (v1.2 Roadmap)
- ✅ Architecture plan documented
- ✅ Parallelization strategy with ROI
- ✅ Quality dashboard designed
- ✅ Scaling plan to 50+ commands

---

## Dependency Resolution

### Bead Dependency Chain (Critical Path)

```
Phase A Critical Path (30 min):
  ├─ 6 fixes in parallel (20 min max)
  └─ Rebuild & verify (10 min)
    └─ GATE: Production Ready

Phase B Critical Path (240 min):
  ├─ Analysis Phase (2 hrs sequential)
  │  ├─ high-analyze-help-text-test-structure
  │  ├─ high-flag-coverage-test-framework
  │  ├─ high-add-integration-test-framework (longest: 60 min)
  │  └─ high-design-parametrization-approach
  │
  └─ Implementation Phase (parallel, 120 min max)
     ├─ high-refactor-duplicate-test-patterns (120 min)
     ├─ high-add-integration-tests-all-24-commands (90 min)
     ├─ high-complete-flag-coverage-100-percent (120 min)
     ├─ high-implement-8-content-improvements (120 min)
     └─ high-implement-phase-gate-automation (120 min)

    MAX(implementation) = 120 min per parallel track
    TOTAL = 120 min (design already done)

Total Phase B: 120 min design + 120 min impl = 240 min = 4 hours minimum
Realistic with testing/review: 9-15 hours
```

### No External Dependencies
- ✅ All beads are internal to Intent CLI
- ✅ No third-party service dependencies
- ✅ No environment-specific constraints
- ✅ Can be executed in any order within Phase groups

---

## Execution Checklist

### Pre-Deployment (PHASE A)
- [ ] All 6 fixes reviewed by senior developer
- [ ] Build tested locally (0 warnings)
- [ ] Full test suite runs (1485/1485)
- [ ] Integration smoke test passed
- [ ] Staging deployment successful
- [ ] Rollback plan documented
- [ ] Change log prepared
- [ ] Approval from tech lead

### Pre-v1.1 Release (PHASE B)
- [ ] All design docs completed
- [ ] Test framework implemented & working
- [ ] 116 tests refactored to 25-30 (verify count)
- [ ] Integration tests for all 24 commands passing
- [ ] Flag coverage 26%→100% verified
- [ ] 8 content improvements verified
- [ ] Phase gates implemented & tested
- [ ] Full regression testing passed
- [ ] Performance baseline established
- [ ] All 1485+ tests passing
- [ ] Release notes prepared
- [ ] v1.1 tag created

### v1.2 Planning Complete (PHASE C)
- [ ] Architecture scaling plan documented
- [ ] Parallelization strategy approved
- [ ] Quality dashboard design finalized
- [ ] Roadmap created for v1.2+

---

**Document Status**: READY FOR EXECUTION
**Last Updated**: 2026-01-18
**Critical Path**: 30 min (PHASE A) + 240 min (PHASE B longest chain) = 4.5 hours minimum
**Realistic Timeline**: 2-3 hours (Phase A) + 9-15 hours (Phase B) + 5-15 hours (Phase C optional)
