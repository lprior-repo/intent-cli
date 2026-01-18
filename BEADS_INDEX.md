# Intent CLI Beads System Index

**Date Generated:** 2026-01-18
**Source:** Auditor Findings (Security, Quality, Test Coverage, Process, Architecture, Integration)
**Status:** Ready for Execution

## Quick Reference

| Phase | Beads | Effort | Priority | Status |
|-------|-------|--------|----------|--------|
| **A: Critical** | 8 | ~2h | Blocking | Ready |
| **B: High** | 5 | ~15h | Urgent | Planning |
| **C: Medium** | 4 | ~6.5h | Medium | Backlog |
| **D: Backlog** | 4 | ~9h | Low | Backlog |
| **TOTAL** | 21 | ~32.5h | Mixed | Distributed |

---

## File Structure

```
Intent CLI Project
├── BEADS.jsonl                    ← Complete bead definitions (21 beads)
├── BEADS_EXECUTION_PLAN.md        ← Detailed execution strategy & timeline
├── BEADS_INDEX.md                 ← This file
│
├── Phase A: CRITICAL (execute first)
│   ├── CRITICAL-001 to CRITICAL-008
│   ├── Effort: ~2 hours
│   ├── Focus: Production bugs & documentation fixes
│   └── Gate: Must complete before Phase B
│
├── Phase B: HIGH (v1.1 improvements)
│   ├── HIGH-001 to HIGH-005
│   ├── Effort: ~15 hours
│   ├── Focus: Test suite quality & automation
│   └── Can run parallel to other work
│
├── Phase C: MEDIUM (optimization)
│   ├── MEDIUM-001 to MEDIUM-004
│   ├── Effort: ~6.5 hours
│   ├── Focus: Refactoring & standardization
│   └── Nice-to-have improvements
│
└── Phase D: BACKLOG (v1.2+)
    ├── BACKLOG-001 to BACKLOG-003
    ├── Effort: ~9 hours
    ├── Focus: Performance & documentation
    └── Execute as time permits
```

---

## Phase A: CRITICAL (Execute Now)

**Total Effort:** ~2 hours | **Blocking:** Yes | **Can Parallelize:** Partial

### Execution Sequence

#### 1. Documentation Fixes (20 minutes total)
```bash
CRITICAL-004  Add "KIRK: " prefix to parse command desc         5 min
CRITICAL-005  Fix EARS pattern count (5 patterns → 4)         10 min
CRITICAL-006  Add ai_readiness to quality help text             5 min
```

#### 2. Code Safety Fixes (40 minutes total)
```bash
CRITICAL-001  Fix center_in_box negative padding              15 min
CRITICAL-002  Fix progress_bar_with_width DOS vector          15 min
CRITICAL-003  Fix config timeout merge data loss              10 min
```

#### 3. Test Coverage (40 minutes total)
```bash
CRITICAL-007  Add bounds validation tests for center_in_box    20 min
CRITICAL-008  Add validation tests for progress_bar_width      20 min
```

### Files Modified

| File | Beads | Type | Impact |
|------|-------|------|--------|
| `src/intent/cli_text_constants.gleam` | CRITICAL-004, 005, 006 | Text | High |
| `src/intent/formatter_utils.gleam` | CRITICAL-001, 002, 007, 008 | Code | Critical |
| `src/intent/config.gleam` | CRITICAL-003 | Code | Critical |
| `test/formatter_utils_test.gleam` | CRITICAL-007, 008 | Tests | New |

### Success Criteria ✓
- [ ] All 8 beads passing their tests
- [ ] No regressions in existing test suite
- [ ] formatter_utils exports validated bounds-safe functions
- [ ] config.merge_with_flags preserves base values correctly
- [ ] Help text consistent (all KIRK commands prefixed)

### Gate to Phase B
```
All CRITICAL beads passing tests → Phase B can begin
```

---

## Phase B: HIGH PRIORITY (v1.1 Sprint)

**Total Effort:** ~15 hours | **Team:** 1-2 developers | **Timeline:** Weeks 2-4

### Wave Structure

**Wave 1 (Parallel, ~450 min)**
```
HIGH-001  Parametrize help_text_test (eliminate 95% duplication)    120 min
HIGH-005  Design parallelization strategy                            240 min
MEDIUM-002 Phase gate validation script                               90 min
```

**Wave 2 (After Wave 1, ~660 min)**
```
HIGH-002  Integration tests for all 24 commands --help output        180 min
HIGH-004  Implement phase gate automation framework                  480 min
```

**Wave 3 (After Wave 2, ~120 min)**
```
HIGH-003  Add flag coverage tests (17 missing flags → 100%)         120 min
```

### Quality Improvements

| Area | Current | Target | Bead |
|------|---------|--------|------|
| **Test Duplication** | 95% | < 10% | HIGH-001 |
| **Command --help Tests** | 0/24 | 24/24 | HIGH-002 |
| **Flag Test Coverage** | 26% | 100% | HIGH-003 |
| **Phase Gates** | Manual | Automated | HIGH-004 |
| **Parallelization** | Ad-hoc | Strategic | HIGH-005 |

### Success Criteria ✓
- [ ] help_text_test.gleam reduced from 240+ lines to ~40 (parametrized)
- [ ] All 24 commands have integration tests for --help output
- [ ] All 30+ flags covered by test cases (coverage: 100%)
- [ ] Phase gate validation script working
- [ ] Parallelization strategy documented with performance estimates

---

## Phase C: MEDIUM PRIORITY (Optimization)

**Total Effort:** ~6.5 hours | **Team:** 1 developer | **Timeline:** Month 2

### Tasks

```
MEDIUM-001  Plan plan.gleam module split (3.5K LOC → 4 modules)    240 min
MEDIUM-003  Standardize exit codes across all 24 commands          120 min
MEDIUM-004  Organize emoji_constants with category groups           60 min
```

### Design Outcomes

| Bead | Deliverable | Purpose |
|------|-------------|---------|
| MEDIUM-001 | `.planning/plan-module-split.md` | Scalability planning |
| MEDIUM-003 | `.planning/error-handler-standardization.md` | CLI consistency |
| MEDIUM-004 | Reorganized `emoji_constants.gleam` | Code clarity |

### Success Criteria ✓
- [ ] plan.gleam split strategy designed (no implementation yet)
- [ ] Exit code audit complete, all 24 commands assessed
- [ ] emoji_constants grouped logically with documentation

---

## Phase D: BACKLOG (v1.2+)

**Total Effort:** ~9 hours | **Team:** 1 developer | **Timeline:** As bandwidth allows

### Tasks

```
BACKLOG-001  Performance profiling formatter_utils                 120 min
BACKLOG-002  Test fixtures for CLI standards validation             90 min
BACKLOG-003  CLI Consistency Standards handbook & guide            150 min
```

### Outcomes

| Bead | Deliverable | Impact |
|------|-------------|--------|
| BACKLOG-001 | Performance baseline & optimizations | 5ms target |
| BACKLOG-002 | `test/cli_standards_fixtures.gleam` | Better testing |
| BACKLOG-003 | `docs/CLI_CONSISTENCY_HANDBOOK.md` | Developer guide |

---

## Dependencies

### Critical Path
```
CRITICAL-001 ─→ CRITICAL-007 ─┐
CRITICAL-002 ─→ CRITICAL-008 ─┼─→ Phase B Start
CRITICAL-003 ──────────────────┤
CRITICAL-004 ──────────────────┘
CRITICAL-005
CRITICAL-006
```

### Phase B Dependencies
```
[Phase A Complete] ──→ HIGH-001 ──→ HIGH-004 ──→ [Phase B Complete]
[Phase A Complete] ──→ HIGH-002 ──→ [Testing Done]
[Phase A Complete] ──→ HIGH-003 ──→ [Coverage 100%]
[Phase A Complete] ──→ HIGH-005 ──→ [Strategy Ready]
```

### Cross-Phase
```
CRITICAL-001, CRITICAL-002, CRITICAL-003 ──→ MEDIUM-002 ──→ Phase Gate Validation
BACKLOG-002 ──→ Depends on CRITICAL-007, CRITICAL-008 (needs test patterns)
```

---

## Bead Catalog

### All 21 Beads by ID

**Phase A: CRITICAL (8 beads)**
| ID | Title | Effort | File |
|----|-------|--------|------|
| CRITICAL-001 | Fix center_in_box negative padding | 15m | formatter_utils.gleam |
| CRITICAL-002 | Fix progress_bar_with_width DOS vector | 15m | formatter_utils.gleam |
| CRITICAL-003 | Fix config timeout merge data loss | 10m | config.gleam |
| CRITICAL-004 | Add KIRK: prefix to parse command | 5m | cli_text_constants.gleam |
| CRITICAL-005 | Fix EARS pattern count documentation | 10m | cli_text_constants.gleam |
| CRITICAL-006 | Add ai_readiness to quality help | 5m | cli_text_constants.gleam |
| CRITICAL-007 | Add bounds tests for center_in_box | 20m | formatter_utils_test.gleam |
| CRITICAL-008 | Add validation tests for progress_bar | 20m | formatter_utils_test.gleam |

**Phase B: HIGH (5 beads)**
| ID | Title | Effort | File |
|----|-------|--------|------|
| HIGH-001 | Parametrize help_text_test | 120m | help_text_test.gleam |
| HIGH-002 | Add --help integration tests | 180m | output_test.gleam |
| HIGH-003 | Implement flag coverage tests | 120m | flag_normalization_test.gleam |
| HIGH-004 | Phase gate automation framework | 480m | scripts/ |
| HIGH-005 | Design parallelization strategy | 240m | .planning/ |

**Phase C: MEDIUM (4 beads)**
| ID | Title | Effort | File |
|----|-------|--------|------|
| MEDIUM-001 | Plan plan.gleam module split | 240m | .planning/ |
| MEDIUM-002 | Phase gate validation script | 90m | scripts/ |
| MEDIUM-003 | Error handler exit code standardization | 120m | .planning/ |
| MEDIUM-004 | Organize emoji_constants | 60m | emoji_constants.gleam |

**Phase D: BACKLOG (4 beads)**
| ID | Title | Effort | File |
|----|-------|--------|------|
| BACKLOG-001 | Performance profiling formatter_utils | 120m | src/intent/ |
| BACKLOG-002 | Test fixtures for CLI standards | 90m | test/ |
| BACKLOG-003 | CLI Consistency Standards handbook | 150m | docs/ |
| BACKLOG-004 | *Future optimization beads* | TBD | TBD |

---

## How to Use This System

### For Project Managers
1. Read **BEADS_EXECUTION_PLAN.md** for timeline & resource planning
2. Use Phase breakdown to allocate team capacity
3. Track completion via bead status in BEADS.jsonl
4. Monitor gates between phases

### For Developers
1. Read **BEADS.jsonl** for detailed task specifications
2. Check **BEADS_EXECUTION_PLAN.md** "Execution Order" for sequencing
3. Each bead lists: effort, requirements, success criteria, tags
4. Use `bd` commands to track bead status

### For Architects
1. Review dependency graph in **BEADS_EXECUTION_PLAN.md**
2. Check Phase C (MEDIUM) for architectural planning
3. MEDIUM-001 details plan.gleam split strategy
4. BACKLOG beads inform v1.2 roadmap

### For QA/Testing
1. Focus on Phase B beads (HIGH-001, 002, 003)
2. Each CRITICAL bead includes test success criteria
3. HIGH-005 parallelization affects test execution
4. BACKLOG-002 provides testing infrastructure

---

## Metrics & Tracking

### Progress Tracking
```bash
# View all beads
cat BEADS.jsonl | jq .

# Filter by phase
cat BEADS.jsonl | jq 'select(.priority == 0)'    # CRITICAL
cat BEADS.jsonl | jq 'select(.priority == 1)'    # HIGH
cat BEADS.jsonl | jq 'select(.priority == 2)'    # MEDIUM
cat BEADS.jsonl | jq 'select(.priority == 4)'    # BACKLOG

# View specific bead
cat BEADS.jsonl | jq 'select(.id == "CRITICAL-001")'
```

### Status Dashboard
- **Phase A:** 0/8 complete (0%)
- **Phase B:** 0/5 complete (0%)
- **Phase C:** 0/4 complete (0%)
- **Phase D:** 0/4 complete (0%)
- **Total:** 0/21 complete (0%)

---

## Approval & Sign-Off

### Phase A Gate
**Status:** Ready to Execute
**Approval Required:** Tech Lead (safety review) + QA (test coverage review)
**Owner:** TBD
**Target Start:** Immediately
**Target Complete:** <1 day

### Phase B Gate
**Status:** Awaiting Phase A completion
**Approval Required:** Tech Lead + Product Manager
**Owner:** TBD
**Target Start:** Week 2
**Target Complete:** Week 4

### Phase C Gate
**Status:** Awaiting Phase B completion
**Approval Required:** Tech Lead + Architecture Review
**Owner:** TBD
**Target Start:** Month 2
**Target Complete:** Month 2-3

### Phase D Gate
**Status:** Backlog (as bandwidth allows)
**Approval Required:** None (optional improvements)
**Owner:** TBD
**Target Start:** v1.2 planning
**Target Complete:** TBD

---

## References

**Source Documents:**
- CLAUDE.md - Project instructions & CLI standards
- Auditor findings from: Security, Quality, Test, Process, Architecture reviews

**Related Files:**
- BEADS.jsonl - Complete bead definitions
- BEADS_EXECUTION_PLAN.md - Detailed execution strategy

**Commands:**
```bash
# View beads
cat BEADS.jsonl | jq .

# Pretty print execution plan
cat BEADS_EXECUTION_PLAN.md | less

# Check current metrics
gleam test           # Test coverage
gleam build          # Build health
```

---

## Questions?

**About specific beads:** Check BEADS.jsonl `success_criteria` field
**About execution order:** See BEADS_EXECUTION_PLAN.md "Execution Order" section
**About dependencies:** See "Dependency Graph" in BEADS_EXECUTION_PLAN.md
**About timelines:** See "Rollout Timeline" in BEADS_EXECUTION_PLAN.md
**About risks:** See "Risk Analysis" in BEADS_EXECUTION_PLAN.md

---

**Created:** 2026-01-18 | **Format:** JSONL (beads) + Markdown (plans) | **Status:** Ready for Execution
