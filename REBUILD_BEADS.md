# Intent CLI 4.0: Implementation Beads

## Wave 0: Foundation (Port Existing Core)

### WAVE0-01: Port Core Types
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port types.gleam with new vision/shape/ready blocks
- **Tests to port:** parser_test.gleam type assertions
- **Acceptance:**
  - [ ] All existing Spec fields preserved
  - [ ] New Vision block added
  - [ ] New Shape block added
  - [ ] New Ready block added
  - [ ] Compiles without errors

### WAVE0-02: Port Checker Rules
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port checker/rules.gleam (602 lines, 40+ rule types)
- **Tests to port:** rule_test.gleam (200+ tests)
- **Acceptance:**
  - [ ] All 40+ RuleExpr variants work
  - [ ] Regex caching functional
  - [ ] All rule_test.gleam tests pass

### WAVE0-03: Port Interpolation
- **Priority:** 1 (Critical)
- **Effort:** 20min
- **Task:** Port interpolate.gleam (335 lines)
- **Tests to port:** interpolate_test.gleam (100+ tests)
- **Acceptance:**
  - [ ] Variable substitution works
  - [ ] Depth limiting functional
  - [ ] Circular reference detection works
  - [ ] All interpolate_test.gleam tests pass

### WAVE0-04: Port Array Indexing
- **Priority:** 1 (Critical)
- **Effort:** 15min
- **Task:** Port array_indexing.gleam (288 lines)
- **Tests to port:** array_indexing_test.gleam (72 tests)
- **Acceptance:**
  - [ ] JSON path navigation works
  - [ ] Positive/negative indices work
  - [ ] Wildcard support works
  - [ ] All array_indexing_test.gleam tests pass

### WAVE0-05: Port Security Module
- **Priority:** 1 (Critical)
- **Effort:** 20min
- **Task:** Port security.gleam (367 lines)
- **Tests to port:** security_test.gleam
- **Acceptance:**
  - [ ] SSRF protection functional
  - [ ] Path traversal detection works
  - [ ] URL validation works
  - [ ] All security tests pass

### WAVE0-06: Port FFI Layer
- **Priority:** 1 (Critical)
- **Effort:** 15min
- **Task:** Port intent_ffi.erl, intent_http_ffi.erl, intent_checker.erl
- **Acceptance:**
  - [ ] HTTP execution works
  - [ ] Regex caching works
  - [ ] UUID generation works
  - [ ] Timestamp generation works

### WAVE0-07: Port Checker Core
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port checker.gleam, checker/json.gleam, checker/headers.gleam
- **Tests to port:** Related checker tests
- **Acceptance:**
  - [ ] Response validation works
  - [ ] Field navigation works
  - [ ] Header checking works
  - [ ] All checker tests pass

### WAVE0-08: Port HTTP Client
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port http_client.gleam (560 lines)
- **Acceptance:**
  - [ ] HTTP execution with timeout
  - [ ] SSRF protection integrated
  - [ ] Error classification works
  - [ ] Request interpolation works

### WAVE0-09: Port Runner
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port runner.gleam (627 lines)
- **Tests to port:** runner_test.gleam, runner_executor_test.gleam
- **Acceptance:**
  - [ ] Spec execution works
  - [ ] Behavior dependency resolution works
  - [ ] Context capture works
  - [ ] All runner tests pass

### WAVE0-10: Port Output System
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port output.gleam, json_output.gleam, output_mode.gleam, cli_ui.gleam
- **Tests to port:** output_test.gleam (100+ tests)
- **Acceptance:**
  - [ ] JSON output schema preserved
  - [ ] next_actions field works
  - [ ] Interactive mode works
  - [ ] All output tests pass

---

## Wave 1: Phase 1 - Vision (DDD)

### WAVE1-01: Vision Types
- **Priority:** 1 (Critical)
- **Effort:** 20min
- **Task:** Create phase1_vision/vision_types.gleam
- **Acceptance:**
  - [ ] VisionSession type defined
  - [ ] VisionDoc type defined
  - [ ] Scenario type defined
  - [ ] ScenarioStep type defined

### WAVE1-02: Vision Storage
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create phase1_vision/vision_storage.gleam
- **Pattern:** Follow interview_storage.gleam JSONL pattern
- **Acceptance:**
  - [ ] JSONL persistence works
  - [ ] Session roundtrip works
  - [ ] History/snapshots work
  - [ ] Tests written and passing

### WAVE1-03: Vision Questions
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create phase1_vision/vision_questions.gleam
- **Questions:**
  - Q1: Press release (what & why)
  - Q2: Persona (who specifically)
  - Q3: Non-personas (who NOT)
  - Q4: Replaces (current solution)
  - Q5: VORP (why switch)
  - Q6: North star (ideal journey)
  - Q7: More scenarios
  - Q8: Out of scope
- **Acceptance:**
  - [ ] All 8 questions defined
  - [ ] Extraction logic for each
  - [ ] Field mapping correct

### WAVE1-04: Vision Critique Protocol
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase1_vision/vision_critique.gleam
- **Persona:** Skeptical PM
- **Critique areas:**
  - Press release (compelling? differentiated?)
  - Persona (specific? validated?)
  - North star (complete? end-to-end?)
  - VORP (10x better? validated?)
- **Acceptance:**
  - [ ] Critique generation works
  - [ ] Blocking questions identified
  - [ ] Alignment check works
  - [ ] JSON output correct

### WAVE1-05: Vision Commands
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Add vision commands to main.gleam
- **Commands:**
  - vision start
  - vision parse
  - vision check
  - vision critique
  - vision respond
  - vision agree
  - vision export
- **Acceptance:**
  - [ ] All 7 commands work
  - [ ] JSON output correct
  - [ ] Gate logic works
  - [ ] next_actions guide to Phase 2

### WAVE1-06: Vision Tests
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Create test/phase1_vision_test.gleam
- **Acceptance:**
  - [ ] Storage roundtrip tests
  - [ ] Question extraction tests
  - [ ] Critique generation tests
  - [ ] Gate logic tests

---

## Wave 2: Phase 2 - Shape (MVP)

### WAVE2-01: Shape Types
- **Priority:** 1 (Critical)
- **Effort:** 20min
- **Task:** Create phase2_shape/shape_types.gleam
- **Acceptance:**
  - [ ] ShapeSession type defined
  - [ ] FeatureShape type defined
  - [ ] MVPSlice type defined
  - [ ] ValidationMoment type defined

### WAVE2-02: Shape Storage
- **Priority:** 1 (Critical)
- **Effort:** 20min
- **Task:** Create phase2_shape/shape_storage.gleam
- **Pattern:** Follow vision_storage.gleam
- **Acceptance:**
  - [ ] JSONL persistence works
  - [ ] Links to vision session
  - [ ] Tests written

### WAVE2-03: MVP Analyzer
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create phase2_shape/mvp_analyzer.gleam
- **Logic:**
  - Identify critical path features
  - Detect what can be faked/hardcoded
  - Calculate minimal scope
- **Acceptance:**
  - [ ] Critical path detection works
  - [ ] Shortcut suggestions work
  - [ ] MVP slice generated correctly

### WAVE2-04: Shape Critique Protocol
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase2_shape/shape_critique.gleam
- **Persona:** Pragmatic Tech Lead
- **Critique areas:**
  - MVP slice (actually minimum?)
  - Critical path (really critical?)
  - Validation moment (proves concept?)
  - Post-MVP (acceptable to defer?)
- **Acceptance:**
  - [ ] Critique generation works
  - [ ] Scope reduction suggestions
  - [ ] Alignment check works

### WAVE2-05: Shape Commands
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Add shape commands to main.gleam
- **Commands:**
  - shape start --vision=<id>
  - shape check
  - shape critique
  - shape respond
  - shape agree
  - shape beads
- **Acceptance:**
  - [ ] All 6 commands work
  - [ ] Vision linking works
  - [ ] MVP beads generated
  - [ ] next_actions guide to Phase 3

### WAVE2-06: Shape Tests
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Create test/phase2_shape_test.gleam
- **Acceptance:**
  - [ ] MVP analyzer tests
  - [ ] Critique tests
  - [ ] Vision linking tests

---

## Wave 3: Phase 3 - Spec (Enhance KIRK)

### WAVE3-01: Enhanced Interview
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Enhance interview.gleam to link vision/shape
- **Changes:**
  - Accept --shape=<id> flag
  - Pre-populate from vision/shape data
  - Validate against vision context
- **Acceptance:**
  - [ ] Shape linking works
  - [ ] Vision data accessible
  - [ ] Pre-population works

### WAVE3-02: Enhanced Quality Analyzer
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Add ready_score dimension to quality_analyzer.gleam
- **New dimension:**
  - ready_score: 0-100 based on READY checks
- **Acceptance:**
  - [ ] 5th dimension added
  - [ ] Scoring logic correct
  - [ ] Tests updated

### WAVE3-03: Enhanced Gap Detector
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Add product gaps to gap_detector.gleam
- **New gap types:**
  - discovery_gap
  - understanding_gap
  - empathy_gap
  - vorp_gap
- **Acceptance:**
  - [ ] 4 new gap types added
  - [ ] Detection logic works
  - [ ] Tests updated

### WAVE3-04: Spec Critique Protocol
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase3_spec/spec_critique.gleam
- **Persona:** Adversarial QA Engineer
- **Critique areas:**
  - EARS completeness
  - Contract coverage
  - Inversion coverage
  - Effects coverage
  - Pre-mortem pitfalls
- **Acceptance:**
  - [ ] Per-round critique works
  - [ ] RCS validation
  - [ ] Alignment check works

### WAVE3-05: Spec Commands Enhancement
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Add spec critique commands
- **Commands:**
  - spec critique
  - spec respond
  - spec agree
- **Acceptance:**
  - [ ] Commands work
  - [ ] Links to existing KIRK commands
  - [ ] Gate to Phase 4 works

### WAVE3-06: Spec Tests
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Update/add tests for enhanced KIRK
- **Acceptance:**
  - [ ] ready_score tests
  - [ ] Product gap tests
  - [ ] Critique tests

---

## Wave 4: Phase 4 - Ready (Ship Decision)

### WAVE4-01: READY Checker
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase4_ready/ready.gleam
- **Checks:**
  - R: Replacement value (VORP analysis)
  - E: Empathy validated (friction simulation)
  - A: Actionable errors (fix suggestions)
  - D: Discoverable (feature access)
  - Y: Yet complete (north star achievable)
- **Acceptance:**
  - [ ] All 5 checks implemented
  - [ ] Score calculation works
  - [ ] Blocking issues identified

### WAVE4-02: Vision Alignment Checker
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create phase4_ready/vision_alignment.gleam
- **Logic:**
  - Compare spec to original vision
  - Detect drift
  - Flag deviations
- **Acceptance:**
  - [ ] Alignment detection works
  - [ ] Drift reporting works
  - [ ] Recommendations generated

### WAVE4-03: Empathy Simulator
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase4_ready/empathy_simulator.gleam
- **Logic:**
  - Simulate persona through north star
  - Generate friction log
  - Score friction points
- **Acceptance:**
  - [ ] Persona simulation works
  - [ ] Friction log generated
  - [ ] Severity classification works

### WAVE4-04: VORP Analyzer
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create phase4_ready/vorp_analyzer.gleam
- **Logic:**
  - Compare to replaces
  - Calculate improvement delta
  - 4 Brutal Truths audit
- **Acceptance:**
  - [ ] VORP calculation works
  - [ ] 4 Truths audit works
  - [ ] Recommendations generated

### WAVE4-05: Ready Critique Protocol
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase4_ready/ready_critique.gleam
- **Persona:** Pre-Launch Auditor
- **Critique areas:**
  - Vision alignment (drift?)
  - READY checks (all pass?)
  - Friction resolved?
  - Ship recommendation
- **Acceptance:**
  - [ ] Final critique works
  - [ ] Ship/no-ship decision
  - [ ] Blocking beads generated

### WAVE4-06: Ready Commands
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Add ready commands to main.gleam
- **Commands:**
  - ready
  - ready critique
  - ready respond
  - ready agree
- **Acceptance:**
  - [ ] All 4 commands work
  - [ ] READY score output
  - [ ] Beads generated with ready: tags

### WAVE4-07: Ready Tests
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Create test/phase4_ready_test.gleam
- **Acceptance:**
  - [ ] READY check tests
  - [ ] Vision alignment tests
  - [ ] Empathy simulator tests
  - [ ] VORP analyzer tests

---

## Wave 5: Integration

### WAVE5-01: Unified CLI Entry
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Refactor main.gleam for 4-phase flow
- **Acceptance:**
  - [ ] All phase commands registered
  - [ ] Help text updated
  - [ ] Phase state tracked

### WAVE5-02: Phase State Machine
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create phase_state.gleam
- **Logic:**
  - Track current phase
  - Enforce gate progression
  - Block out-of-order commands
- **Acceptance:**
  - [ ] State tracking works
  - [ ] Gate enforcement works
  - [ ] Override flag for power users

### WAVE5-03: Enhanced Bead Generation
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Update bead_templates.gleam
- **Changes:**
  - Add ready: tags (R, E, A, D, Y)
  - Link to blocking READY issues
  - Include vision/shape context
- **Acceptance:**
  - [ ] Tags added correctly
  - [ ] Traceability works
  - [ ] Context included

### WAVE5-04: Documentation Update
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Update CLAUDE.md with new commands
- **Acceptance:**
  - [ ] All new commands documented
  - [ ] 4-phase flow explained
  - [ ] Glossary updated

### WAVE5-05: Integration Tests
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create test/integration_test.gleam
- **Tests:**
  - Full 4-phase flow
  - Gate progression
  - Vision to READY journey
- **Acceptance:**
  - [ ] End-to-end test works
  - [ ] All phases exercised
  - [ ] Gate logic validated

### WAVE5-06: Example Specs Update
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Update examples/ with 4-phase specs
- **Acceptance:**
  - [ ] At least 2 complete examples
  - [ ] Vision block populated
  - [ ] Shape block populated
  - [ ] Ready block populated

---

## Summary

| Wave | Beads | Total Effort |
|------|-------|--------------|
| Wave 0: Foundation | 10 | ~4h 30min |
| Wave 1: Vision | 6 | ~3h 20min |
| Wave 2: Shape | 6 | ~3h 10min |
| Wave 3: Spec | 6 | ~3h 30min |
| Wave 4: Ready | 7 | ~4h 30min |
| Wave 5: Integration | 6 | ~3h 30min |
| **TOTAL** | **41** | **~22h 30min** |

---

## Dependencies

```
WAVE0 (all) → WAVE1, WAVE2, WAVE3, WAVE4, WAVE5
WAVE1 → WAVE2 (vision required for shape)
WAVE2 → WAVE3 (shape required for spec linking)
WAVE3 → WAVE4 (spec required for READY)
WAVE1, WAVE2, WAVE3, WAVE4 → WAVE5 (all phases for integration)
```

---

## Critical Path

```
WAVE0-01 → WAVE0-02 → WAVE0-07 → WAVE0-08 → WAVE0-09
                                              ↓
WAVE1-01 → WAVE1-02 → WAVE1-03 → WAVE1-04 → WAVE1-05
                                              ↓
WAVE2-01 → WAVE2-02 → WAVE2-03 → WAVE2-04 → WAVE2-05
                                              ↓
WAVE3-01 → WAVE3-02 → WAVE3-03 → WAVE3-04 → WAVE3-05
                                              ↓
WAVE4-01 → WAVE4-02 → WAVE4-03 → WAVE4-04 → WAVE4-05 → WAVE4-06
                                                          ↓
                                                       WAVE5-01 → WAVE5-02 → WAVE5-05
```
