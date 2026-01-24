# Intent CLI 4.0: Implementation Beads

**Intent CLI is a planning tool. No HTTP. No API testing. Pure thinking framework.**

---

## Wave 0: Foundation

### WAVE0-01: Core Types
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create core/types.gleam with Plan, Vision, Shape, Spec, Ready types
- **Acceptance:**
  - [ ] Plan type with vision/shape/spec/ready blocks
  - [ ] Scenario and ScenarioStep types
  - [ ] Feature and FeatureSpec types
  - [ ] Behavior with given/when/then
  - [ ] ReadyCheck type
  - [ ] Compiles without errors

### WAVE0-02: JSONL Storage Pattern
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create core/storage.gleam with session persistence
- **Pattern:** Port from interview_storage.gleam
- **Acceptance:**
  - [ ] Session roundtrip works
  - [ ] JSONL append works
  - [ ] History/snapshots work
  - [ ] Tests written

### WAVE0-03: Output Formatting
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create output/output.gleam, json_output.gleam
- **Pattern:** Port unified JSON schema with next_actions
- **Acceptance:**
  - [ ] JSON output format correct
  - [ ] next_actions field populated
  - [ ] Human-readable format works
  - [ ] OutputMode abstraction works

### WAVE0-04: FFI Utilities
- **Priority:** 1 (Critical)
- **Effort:** 15min
- **Task:** Create minimal intent_ffi.erl
- **Keep only:**
  - UUID generation
  - Timestamp generation
- **Delete:**
  - HTTP execution
  - Regex caching (not needed)
- **Acceptance:**
  - [ ] UUID works
  - [ ] Timestamps work

### WAVE0-05: Dependency Resolver
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port core/resolver.gleam (topological sort)
- **Acceptance:**
  - [ ] Kahn's algorithm works
  - [ ] Cycle detection works
  - [ ] Wave grouping works
  - [ ] Tests pass

### WAVE0-06: CUE Loader
- **Priority:** 1 (Critical)
- **Effort:** 20min
- **Task:** Port core/loader.gleam for Plan CUE files
- **Acceptance:**
  - [ ] CUE validation works
  - [ ] CUE export works
  - [ ] Plan parsing works

---

## Wave 1: Phase 1 - Vision (DDD)

### WAVE1-01: Vision Types
- **Priority:** 1 (Critical)
- **Effort:** 15min
- **Task:** Create phase1_vision/vision_types.gleam
- **Types:**
  - VisionSession
  - VisionDoc
  - VisionAnswer
- **Acceptance:**
  - [ ] All types defined
  - [ ] Links to core types

### WAVE1-02: Vision Storage
- **Priority:** 1 (Critical)
- **Effort:** 20min
- **Task:** Create phase1_vision/vision_storage.gleam
- **Acceptance:**
  - [ ] JSONL persistence
  - [ ] Session roundtrip
  - [ ] History tracking

### WAVE1-03: Vision Questions
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create phase1_vision/vision_questions.gleam
- **Questions:**
  1. Press release (what & why)
  2. Persona (who specifically)
  3. Non-personas (who NOT)
  4. Replaces (current solution)
  5. VORP (why switch)
  6. North star (ideal journey)
  7. More scenarios (2-3 additional)
  8. Out of scope (boundaries)
- **Acceptance:**
  - [ ] All 8 questions defined
  - [ ] Extraction logic for each
  - [ ] Field mapping to VisionDoc

### WAVE1-04: Vision Critique Protocol
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase1_vision/vision_critique.gleam
- **Persona:** Skeptical PM
- **Critique areas:**
  - Press release: Compelling? Differentiated?
  - Persona: Specific? Validated?
  - North star: Complete? End-to-end?
  - VORP: 10x better? Validated?
  - Scope: Intentional boundaries?
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
  - vision start [--profile]
  - vision parse <file.md>
  - vision check
  - vision critique
  - vision respond '<text>'
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
- **Effort:** 15min
- **Task:** Create phase2_shape/shape_types.gleam
- **Types:**
  - ShapeSession
  - FeatureShape
  - MVPSlice
  - ValidationMoment
- **Acceptance:**
  - [ ] All types defined
  - [ ] Links to vision session

### WAVE2-02: Shape Storage
- **Priority:** 1 (Critical)
- **Effort:** 20min
- **Task:** Create phase2_shape/shape_storage.gleam
- **Acceptance:**
  - [ ] JSONL persistence
  - [ ] Links to vision session
  - [ ] History tracking

### WAVE2-03: Shape Questions
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create phase2_shape/shape_questions.gleam
- **Questions:**
  1. What features are needed? (from north star)
  2. Which are critical for north star?
  3. What's the absolute minimum to see it work?
  4. What can we fake/hardcode?
  5. What can wait until after validation?
  6. What's the validation moment?
- **Acceptance:**
  - [ ] All 6 questions defined
  - [ ] Feature extraction works
  - [ ] MVP slice identified

### WAVE2-04: MVP Analyzer
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create phase2_shape/mvp_analyzer.gleam
- **Logic:**
  - Identify critical path from north star
  - Suggest shortcuts (fake, hardcode, defer)
  - Calculate minimal scope
- **Acceptance:**
  - [ ] Critical path detection
  - [ ] Shortcut suggestions
  - [ ] MVP slice generation

### WAVE2-05: Shape Critique Protocol
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase2_shape/shape_critique.gleam
- **Persona:** Pragmatic Tech Lead
- **Critique areas:**
  - MVP slice: Actually minimum?
  - Critical path: Really critical?
  - Validation moment: Proves concept?
  - Post-MVP: Acceptable to defer?
- **Acceptance:**
  - [ ] Critique generation works
  - [ ] Scope reduction suggestions
  - [ ] Alignment check works

### WAVE2-06: Shape Commands
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Add shape commands to main.gleam
- **Commands:**
  - shape start --vision=<id>
  - shape check
  - shape critique
  - shape respond '<text>'
  - shape agree
  - shape beads
- **Acceptance:**
  - [ ] All 6 commands work
  - [ ] Vision linking works
  - [ ] MVP beads generated
  - [ ] next_actions guide to Phase 3

### WAVE2-07: Shape Tests
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Create test/phase2_shape_test.gleam
- **Acceptance:**
  - [ ] MVP analyzer tests
  - [ ] Critique tests
  - [ ] Vision linking tests

---

## Wave 3: Phase 3 - Spec (KIRK)

### WAVE3-01: Port KIRK Quality Analyzer
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port kirk/quality_analyzer.gleam
- **Adapt for Plan schema (not API spec)
- **Acceptance:**
  - [ ] 5-dimension scoring works
  - [ ] Works with new Plan type
  - [ ] Tests pass

### WAVE3-02: Port KIRK Coverage Analyzer
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port kirk/coverage_analyzer.gleam
- **Adapt:** Check feature coverage, not OWASP
- **Acceptance:**
  - [ ] Feature coverage works
  - [ ] Edge case detection works
  - [ ] Tests pass

### WAVE3-03: Port KIRK Gap Detector
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port kirk/gap_detector.gleam
- **Add product gaps:**
  - discovery_gap
  - understanding_gap
  - empathy_gap
  - vorp_gap
- **Acceptance:**
  - [ ] Gap detection works
  - [ ] Product gaps included
  - [ ] Tests pass

### WAVE3-04: Port KIRK Inversion Checker
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port kirk/inversion_checker.gleam
- **Focus:** Failure modes in behaviors, not HTTP errors
- **Acceptance:**
  - [ ] Failure mode analysis works
  - [ ] Anti-pattern detection works
  - [ ] Tests pass

### WAVE3-05: Port KIRK Effects Analyzer
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port kirk/effects_analyzer.gleam
- **Focus:** Feature dependencies, side effects
- **Acceptance:**
  - [ ] Dependency analysis works
  - [ ] Orphan detection works
  - [ ] Tests pass

### WAVE3-06: Port EARS Parser
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port kirk/ears_parser.gleam
- **Acceptance:**
  - [ ] 5 EARS patterns parse
  - [ ] Behavior extraction works
  - [ ] Tests pass

### WAVE3-07: Spec Interview
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase3_spec/interview.gleam
- **5 rounds:**
  1. EARS (requirements patterns)
  2. Contracts (given/when/then)
  3. Inversion (failure modes)
  4. Effects (dependencies)
  5. Pre-mortem (pitfalls)
- **Acceptance:**
  - [ ] 5-round flow works
  - [ ] Links to shape session
  - [ ] RCS calculation works

### WAVE3-08: Spec Critique Protocol
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase3_spec/spec_critique.gleam
- **Persona:** Adversarial QA Engineer
- **Critique areas:**
  - EARS completeness
  - Behavior coverage
  - Failure mode coverage
  - Dependency clarity
  - Pitfall documentation
- **Acceptance:**
  - [ ] Per-round critique works
  - [ ] RCS validation
  - [ ] Alignment check works

### WAVE3-09: Spec Commands
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Add spec commands to main.gleam
- **Commands:**
  - spec start --shape=<id>
  - quality <plan>
  - coverage <plan>
  - gaps <plan>
  - invert <plan>
  - effects <plan>
  - ears <file>
  - spec critique
  - spec respond '<text>'
  - spec agree
- **Acceptance:**
  - [ ] All commands work
  - [ ] Links to shape session
  - [ ] Gate to Phase 4 works

### WAVE3-10: Spec Tests
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Create test/phase3_spec_test.gleam
- **Acceptance:**
  - [ ] KIRK analyzer tests
  - [ ] Interview flow tests
  - [ ] Critique tests

---

## Wave 4: Phase 4 - Ready (Ship)

### WAVE4-01: READY Checker
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create phase4_ready/ready.gleam
- **Checks:**
  - R: Replacement value (VORP still valid?)
  - E: Empathy validated (friction simulated?)
  - A: Actionable errors (error paths defined?)
  - D: Discoverable (features findable?)
  - Y: Yet complete (north star achievable?)
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
  - Detect drift from persona/north_star
  - Flag scope creep or reduction
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
  - Walk through scenarios
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
  - Validate replaces is still true
  - Check VORP claim against spec
  - Apply 4 Brutal Truths audit
- **Acceptance:**
  - [ ] VORP validation works
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
  - 4 Brutal Truths satisfied?
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
  - ready <plan>
  - ready critique
  - ready respond '<text>'
  - ready agree
- **Acceptance:**
  - [ ] All 4 commands work
  - [ ] READY score output
  - [ ] Vision alignment shown

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
- **Task:** Create main.gleam with all phase commands
- **Acceptance:**
  - [ ] All phase commands registered
  - [ ] Help text complete
  - [ ] Version info correct

### WAVE5-02: Phase State Machine
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Create core/phase_state.gleam
- **Logic:**
  - Track current phase
  - Enforce gate progression
  - Block out-of-order commands
  - Override flag for power users
- **Acceptance:**
  - [ ] State tracking works
  - [ ] Gate enforcement works
  - [ ] Override flag works

### WAVE5-03: Bead Generator
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create beads/bead_generator.gleam
- **Generate beads from:**
  - MVP slice (Phase 2)
  - Spec features (Phase 3)
  - READY blockers (Phase 4)
- **Tag beads with:**
  - phase: 1|2|3|4
  - ready: R|E|A|D|Y (if from READY blockers)
- **Acceptance:**
  - [ ] Bead generation works
  - [ ] Tags applied correctly
  - [ ] Traceability maintained

### WAVE5-04: Wave Calculator
- **Priority:** 1 (Critical)
- **Effort:** 30min
- **Task:** Port beads/plan_mode.gleam
- **Acceptance:**
  - [ ] Topological sort works
  - [ ] Wave grouping works
  - [ ] Effort calculation works

### WAVE5-05: Prompt Generator
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Create beads/prompt_generator.gleam
- **Generate AI implementation prompts from beads
- **Acceptance:**
  - [ ] Context generation works
  - [ ] Guardrails included
  - [ ] Plan context included

### WAVE5-06: Documentation
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Update CLAUDE.md with new commands
- **Acceptance:**
  - [ ] All commands documented
  - [ ] 4-phase flow explained
  - [ ] Glossary complete

### WAVE5-07: Integration Tests
- **Priority:** 1 (Critical)
- **Effort:** 45min
- **Task:** Create test/integration_test.gleam
- **Tests:**
  - Full 4-phase flow
  - Gate progression
  - Vision → Shape → Spec → Ready journey
- **Acceptance:**
  - [ ] End-to-end test works
  - [ ] All phases exercised
  - [ ] Gate logic validated

### WAVE5-08: Example Plans
- **Priority:** 2 (Important)
- **Effort:** 30min
- **Task:** Create examples/ with complete plans
- **Acceptance:**
  - [ ] At least 2 complete examples
  - [ ] Vision block populated
  - [ ] Shape block populated
  - [ ] Spec block populated
  - [ ] Ready block populated

---

## Summary

| Wave | Beads | Total Effort |
|------|-------|--------------|
| Wave 0: Foundation | 6 | ~2h 35min |
| Wave 1: Vision | 6 | ~3h 5min |
| Wave 2: Shape | 7 | ~3h 35min |
| Wave 3: Spec | 10 | ~5h 45min |
| Wave 4: Ready | 7 | ~4h 30min |
| Wave 5: Integration | 8 | ~4h 45min |
| **TOTAL** | **44** | **~24h 15min** |

---

## Dependencies

```
Wave 0 → All other waves

Wave 1 (Vision) → Wave 2 (Shape)
Wave 2 (Shape) → Wave 3 (Spec)
Wave 3 (Spec) → Wave 4 (Ready)

Waves 1-4 → Wave 5 (Integration)
```

---

## Critical Path

```
WAVE0-01 (types) → WAVE0-02 (storage) → WAVE0-03 (output)
                                              ↓
WAVE1-01 → WAVE1-02 → WAVE1-03 → WAVE1-04 → WAVE1-05
                                              ↓
WAVE2-01 → WAVE2-02 → WAVE2-03 → WAVE2-04 → WAVE2-05 → WAVE2-06
                                                          ↓
WAVE3-01..06 (KIRK) → WAVE3-07 → WAVE3-08 → WAVE3-09
                                              ↓
WAVE4-01 → WAVE4-02 → WAVE4-03 → WAVE4-04 → WAVE4-05 → WAVE4-06
                                                          ↓
WAVE5-01 → WAVE5-02 → WAVE5-03 → WAVE5-07
```

---

## What We're NOT Building

- No HTTP client
- No API testing
- No SSRF protection
- No response validation
- No checker/rules engine
- No contract verification

**This is a pure planning tool.**
