# Intent CLI 4.0: Complete Implementation Plan

> **Status**: Research Complete, Ready for Review
> **Total Beads**: 59 (44 original + 15 AI-native)
> **Estimated Work**: ~31 hours

---

## Executive Summary

Intent CLI transforms from an API testing tool into a **pure planning framework** with:

1. **4-Phase System**: Vision → Shape → Spec → Ready (with AI critique at each gate)
2. **AI-Native Interface**: JSONL in/out, CUE schemas for everything
3. **Zero-Ambiguity Beads**: Complete "Block of Context" for deterministic AI implementation

### What Gets Deleted (~2,165 LOC)
- `runner.gleam`, `http_client.gleam`, `checker/*`
- HTTP FFI (`intent_checker.erl`, `intent_runner_ffi.erl`)
- All HTTP-related tests

### What Gets Kept (~22,800 LOC)
- KIRK analyzers (coverage, gaps, inversion, effects, ears_parser)
- Interview system (session, storage, questions)
- Bead generation (templates, prompts, plan_mode)
- Utilities (interpolate, resolver, diff, output)
- FFI (`intent_ffi.erl` - UUID, timestamps only)

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           INTENT CLI 4.0                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────────────────────────────────────────────────────────────┐   │
│  │                        AI-NATIVE INTERFACE                            │   │
│  │  JSONL In → Command Router → Execute → JSONL Out                     │   │
│  │  CUE Schemas: input/*.cue → validate → output/*.cue                  │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│                                     │                                        │
│  ┌──────────────────────────────────┴───────────────────────────────────┐   │
│  │                         4-PHASE SYSTEM                                │   │
│  │                                                                       │   │
│  │  ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐           │   │
│  │  │ VISION  │───▶│  SHAPE  │───▶│  SPEC   │───▶│  READY  │           │   │
│  │  │  (DDD)  │    │  (MVP)  │    │ (KIRK)  │    │ (Ship)  │           │   │
│  │  └────┬────┘    └────┬────┘    └────┬────┘    └────┬────┘           │   │
│  │       │              │              │              │                  │   │
│  │   Skeptical      Pragmatic     Adversarial    Pre-Launch             │   │
│  │      PM          Tech Lead         QA          Auditor               │   │
│  │       │              │              │              │                  │   │
│  │       └──────────────┴──────────────┴──────────────┘                 │   │
│  │                         CRITIQUE SYSTEM                               │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│                                     │                                        │
│  ┌──────────────────────────────────┴───────────────────────────────────┐   │
│  │                       BEAD GENERATION                                 │   │
│  │  Plan Spec → Enhanced Beads → Waves → AI Prompts                     │   │
│  │  (EARS + Contracts + Types + Tests + Boundaries)                     │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Phase 1: VISION (DDD) - "What are we building and why?"

### Types
```gleam
pub type VisionSection {
  VisionSection(
    press_release: String,      // What & why
    persona: String,            // Who specifically
    non_personas: List(String), // Who NOT
    north_star: String,         // Ideal journey
    scenarios: List(Scenario),  // User stories
    replaces: Option(String),   // Current solution
    vorp: String,               // Value Over Replacement
    out_of_scope: List(String), // Boundaries
  )
}
```

### Questions (8 Core)
1. What problem are we solving? (Press release)
2. Who specifically needs this? (Persona)
3. Who is this NOT for? (Non-personas)
4. What do they currently use? (Replaces)
5. What would make them switch? (VORP)
6. What is their ideal journey? (North star)
7. What scenarios demonstrate this? (2-3 scenarios)
8. What is explicitly out of scope? (Boundaries)

### Critique: Skeptical PM
- "Is this a real problem or wishful thinking?"
- "Is the persona validated or assumed?"
- "Is the VORP 10x better, not 10% better?"

### Gate: Vision Agreement
- All 8 questions answered
- PM critique addressed
- Human + AI alignment confirmed

---

## Phase 2: SHAPE (MVP) - "What's the smallest thing that works?"

### Types
```gleam
pub type ShapeSection {
  ShapeSection(
    features: List(FeatureShape),
    critical_path: List(String),    // Must-have for north star
    mvp_slice: MVPSlice,
    post_mvp: List(String),         // Explicitly deferred
    validation_moment: String,       // The "aha" moment
  )
}

pub type MVPSlice {
  MVPSlice(
    description: String,
    features: List(String),
    shortcuts: List(String),        // What we can fake/defer
  )
}
```

### Questions (6 Core)
1. What features are needed? (From north star)
2. Which are critical for north star?
3. What's the absolute minimum to see it work?
4. What can we fake/hardcode?
5. What can wait until after validation?
6. What's the validation moment?

### Critique: Pragmatic Tech Lead
- "Can we cut more?"
- "Will this actually validate the concept?"
- "Is this scope achievable?"

### Gate: Shape Agreement
- MVP slice defined
- Shortcuts documented
- Tech Lead alignment confirmed

---

## Phase 3: SPEC (KIRK) - "How does it work exactly?"

### 5-Round Mental Model System

| Round | Model | Output | Gate |
|-------|-------|--------|------|
| 1 | EARS | Requirements with patterns | RCS₁=100% |
| 2 | Contracts | Preconditions/postconditions/invariants | RCS₂=100% |
| 3 | Inversion | Failure modes + anti-patterns | RCS₃=100% |
| 4 | Effects | Second-order consequences + requires[] | RCS₄=100% |
| 5 | Pre-mortem | Pitfalls + risk mitigation | RCS₅≥80% |

### Interactive Questioning (6 Categories)
1. **Clarification**: Resolve ambiguous EARS
2. **Edge Cases**: Confirm unusual inputs
3. **Business Logic**: Domain-specific rules
4. **Security**: Critical security choices
5. **API Design**: Structure and conventions
6. **Integration**: External system behavior

### Critique: Adversarial QA
- "What's NOT tested?"
- "What edge cases break this?"
- "What's the blast radius of failures?"

### Gate: Spec Agreement
- All 5 rounds complete (RCS gates met)
- All P0 blocking questions answered
- QA alignment confirmed

---

## Phase 4: READY (Ship) - "Is this actually ready to build?"

### READY Framework (5 Dimensions)

```gleam
pub type ReadyReport {
  ReadyReport(
    replacement: DimensionScore,    // R: VORP still valid?
    empathy: DimensionScore,        // E: Friction simulated?
    actionable: DimensionScore,     // A: Errors guide users?
    discoverable: DimensionScore,   // D: Features findable?
    yet_complete: DimensionScore,   // Y: North star achievable?
    overall_readiness: Int,         // 0-100
    blockers: List(Blocker),
    recommendations: List(Recommendation),
  )
}
```

### Readiness Classification
- ≥90: Launch Ready
- ≥80: Production with caution
- ≥70: Beta/Experimental
- <70: Needs rework

### Vision Alignment Check
- Compare Phase 4 spec to Phase 1 vision
- Detect drift from persona/north_star
- Flag scope creep or reduction

### Critique: Pre-Launch Auditor
- "Did we stay true to the vision?"
- "Are all success criteria met?"
- "What's the rollback plan?"

### Gate: Ready Agreement
- READY score ≥80
- Vision alignment confirmed
- All Critical blockers resolved

---

## AI-Native Interface

### JSONL Protocol

**Request Format:**
```jsonl
{"id":"req-001","command":"quality","args":{"spec_path":"api.cue"}}
{"id":"req-002","command":"coverage","args":{"spec_path":"api.cue"}}
```

**Response Format:**
```jsonl
{"id":"req-001","success":true,"command":"quality","data":{...},"next_actions":[...]}
{"id":"req-002","success":true,"command":"coverage","data":{...},"next_actions":[...]}
```

### AI Commands

```bash
# Batch processing
cat requests.jsonl | intent ai batch > responses.jsonl

# Schema introspection
intent ai schema --command=quality --type=input
intent ai schema --all

# Streaming REPL
intent ai stream

# Validation
intent ai validate --schema=input < requests.jsonl

# Context aggregation
intent ai aggregate --from=responses.jsonl --max-tokens=8000
```

### Error Taxonomy

```cue
#ErrorCode:
    // Validation (user fixable)
    "SPEC_NOT_FOUND" | "SPEC_INVALID_CUE" | "SESSION_NOT_FOUND" | "INVALID_ARGS" |
    // Execution (logic failures)
    "CHECK_FAILED" | "GATE_NOT_MET" | "DEPENDENCY_CYCLE" | "BLOCKER_EXISTS" |
    // System
    "IO_ERROR" | "TIMEOUT" | "CUE_COMMAND_FAILED" |
    // AI-specific
    "CONTEXT_LIMIT" | "BATCH_PARTIAL_FAILURE"
```

### CUE Schema Structure

```
schema/
├── intent.cue              # Existing spec
├── plan.cue                # Plan (Vision+Shape+Spec+Ready)
├── bead.cue                # Enhanced bead
│
└── ai/
    ├── envelope.cue        # Request/Response
    ├── errors.cue          # Error taxonomy
    ├── input/*.cue         # Per-command input
    └── output/*.cue        # Per-command output
```

---

## Enhanced Bead Structure

```gleam
pub type EnhancedBeadRecord {
  EnhancedBeadRecord(
    // Identity
    id: String,
    title: String,

    // Context
    description: String,
    profile_type: String,
    issue_type: String,
    labels: List(String),

    // Priority & Effort
    priority: Int,                    // 1-5
    effort: BeadEffort,               // 5-30min

    // EARS Requirements
    ears_requirements: List(EarsContext),

    // Design by Contract
    contracts: ContractContext,

    // Type Definitions
    type_definitions: List(TypeDefinition),

    // Test Suite
    test_cases: TestSuiteDefinition,

    // Schema & Validation
    input_schema: SchemaDefinition,
    output_schema: SchemaDefinition,
    edge_cases: List(EdgeCase),

    // Boundaries
    boundaries: ScopeBoundaries,     // will_do / will_not_do

    // Dependencies
    dependencies: List(DependencyRef),
    blocks: List(String),

    // AI Context
    ai_context: AIImplementationContext,

    // Source
    spec_source: Option(SpecSource),
  )
}
```

---

## Implementation Waves

### Wave 0: Foundation (~2h 35min)

| ID | Title | Effort | Dependencies |
|----|-------|--------|--------------|
| WAVE0-01 | Core Types (Plan, Vision, Shape, Spec, Ready) | 30min | - |
| WAVE0-02 | JSONL Storage Pattern | 30min | WAVE0-01 |
| WAVE0-03 | Output Formatting (JSON + next_actions) | 30min | WAVE0-01 |
| WAVE0-04 | FFI Utilities (UUID, timestamp) | 15min | - |
| WAVE0-05 | Dependency Resolver (Kahn's toposort) | 30min | WAVE0-01 |
| WAVE0-06 | CUE Loader (Plan schema) | 20min | WAVE0-01 |

### Wave 0.5: AI-Native Foundation (~2h)

| ID | Title | Effort | Dependencies |
|----|-------|--------|--------------|
| AI-01 | schema/ai/envelope.cue | 15min | - |
| AI-02 | schema/ai/errors.cue | 15min | - |
| AI-03 | Extend json_output with correlation ID | 20min | WAVE0-03 |
| AI-04 | jsonl_input.gleam (JSONL stdin parser) | 30min | - |
| AI-05 | command_router.gleam (JSONL dispatch) | 30min | AI-04 |

### Wave 0.6: AI Commands (~2h 30min)

| ID | Title | Effort | Dependencies |
|----|-------|--------|--------------|
| AI-06 | `intent ai batch` command | 45min | AI-05 |
| AI-07 | `intent ai schema` command | 30min | AI-01, AI-02 |
| AI-08 | `intent ai validate` command | 20min | AI-01 |
| AI-09 | `intent ai stream` command | 30min | AI-06 |
| AI-10 | schema/ai/input/*.cue (all commands) | 45min | AI-01 |
| AI-11 | schema/ai/output/*.cue (all commands) | 45min | AI-02 |

### Wave 1: Phase 1 - Vision (~3h 5min)

| ID | Title | Effort | Dependencies |
|----|-------|--------|--------------|
| WAVE1-01 | Vision Types | 15min | WAVE0-01 |
| WAVE1-02 | Vision Storage | 20min | WAVE0-02 |
| WAVE1-03 | Vision Questions (8 core) | 30min | WAVE1-01 |
| WAVE1-04 | Vision Critique (Skeptical PM) | 45min | WAVE1-03 |
| WAVE1-05 | Vision Commands | 45min | WAVE1-04 |
| WAVE1-06 | Vision Tests | 30min | WAVE1-05 |

### Wave 2: Phase 2 - Shape (~3h 35min)

| ID | Title | Effort | Dependencies |
|----|-------|--------|--------------|
| WAVE2-01 | Shape Types | 15min | WAVE0-01 |
| WAVE2-02 | Shape Storage | 20min | WAVE0-02 |
| WAVE2-03 | Shape Questions (6 core) | 30min | WAVE2-01 |
| WAVE2-04 | MVP Analyzer | 30min | WAVE2-03 |
| WAVE2-05 | Shape Critique (Pragmatic Tech Lead) | 45min | WAVE2-04 |
| WAVE2-06 | Shape Commands | 45min | WAVE2-05 |
| WAVE2-07 | Shape Tests | 30min | WAVE2-06 |

### Wave 3: Phase 3 - Spec (KIRK) (~5h 45min)

| ID | Title | Effort | Dependencies |
|----|-------|--------|--------------|
| WAVE3-01 | Port KIRK Quality Analyzer | 30min | WAVE0-01 |
| WAVE3-02 | Port KIRK Coverage Analyzer | 30min | WAVE0-01 |
| WAVE3-03 | Port KIRK Gap Detector | 30min | WAVE0-01 |
| WAVE3-04 | Port KIRK Inversion Checker | 30min | WAVE0-01 |
| WAVE3-05 | Port KIRK Effects Analyzer | 30min | WAVE0-01 |
| WAVE3-06 | Port EARS Parser | 30min | WAVE0-01 |
| WAVE3-07 | Spec Interview (5-round flow) | 45min | WAVE3-01..06 |
| WAVE3-08 | Spec Critique (Adversarial QA) | 45min | WAVE3-07 |
| WAVE3-09 | Spec Commands | 45min | WAVE3-08 |
| WAVE3-10 | Spec Tests | 30min | WAVE3-09 |

### Wave 4: Phase 4 - Ready (~4h 30min)

| ID | Title | Effort | Dependencies |
|----|-------|--------|--------------|
| WAVE4-01 | READY Checker (5 dimensions) | 45min | WAVE3-09 |
| WAVE4-02 | Vision Alignment Checker | 30min | WAVE1-05, WAVE4-01 |
| WAVE4-03 | Empathy Simulator | 45min | WAVE4-01 |
| WAVE4-04 | VORP Analyzer | 30min | WAVE4-01 |
| WAVE4-05 | Ready Critique (Pre-Launch Auditor) | 45min | WAVE4-04 |
| WAVE4-06 | Ready Commands | 45min | WAVE4-05 |
| WAVE4-07 | Ready Tests | 30min | WAVE4-06 |

### Wave 5: Integration (~4h 45min)

| ID | Title | Effort | Dependencies |
|----|-------|--------|--------------|
| WAVE5-01 | Unified CLI Entry | 45min | All phases |
| WAVE5-02 | Phase State Machine | 30min | WAVE5-01 |
| WAVE5-03 | Enhanced Bead Generator | 45min | WAVE4-06 |
| WAVE5-04 | Wave Calculator | 30min | WAVE5-03 |
| WAVE5-05 | Prompt Generator | 30min | WAVE5-03 |
| WAVE5-06 | Documentation Update | 30min | All |
| WAVE5-07 | Integration Tests | 45min | WAVE5-05 |
| WAVE5-08 | Example Plans | 30min | WAVE5-07 |

### Wave 5.5: AI Polish (~2h)

| ID | Title | Effort | Dependencies |
|----|-------|--------|--------------|
| AI-12 | `intent ai aggregate` command | 30min | AI-06 |
| AI-13 | Add --parallel flag to batch | 30min | AI-06 |
| AI-14 | Add --progress flag for stderr | 20min | AI-06 |
| AI-15 | AI pipeline integration tests | 45min | AI-12..14 |

---

## Summary

| Wave | Beads | Effort |
|------|-------|--------|
| Wave 0: Foundation | 6 | ~2h 35min |
| Wave 0.5: AI Foundation | 5 | ~2h |
| Wave 0.6: AI Commands | 6 | ~2h 30min |
| Wave 1: Vision | 6 | ~3h 5min |
| Wave 2: Shape | 7 | ~3h 35min |
| Wave 3: Spec | 10 | ~5h 45min |
| Wave 4: Ready | 7 | ~4h 30min |
| Wave 5: Integration | 8 | ~4h 45min |
| Wave 5.5: AI Polish | 4 | ~2h |
| **TOTAL** | **59** | **~31h** |

---

## Critical Path

```
WAVE0 (Foundation)
    ↓
WAVE0.5 (AI Foundation) ─────────────────────────────────────────┐
    ↓                                                             │
WAVE0.6 (AI Commands) ─────────────────────────────────────────┐ │
    ↓                                                           │ │
WAVE1 (Vision) → WAVE2 (Shape) → WAVE3 (Spec) → WAVE4 (Ready)  │ │
                                                    ↓           │ │
                                              WAVE5 (Integration)│ │
                                                    ↓           │ │
                                              WAVE5.5 (AI Polish)←┘
```

---

## Files to Create

### New Gleam Modules
```
src/intent/
├── core/
│   ├── plan_types.gleam          # Plan, Vision, Shape, Spec, Ready
│   ├── phase_state.gleam         # Phase state machine
│   └── storage.gleam             # JSONL persistence pattern
│
├── phase1_vision/
│   ├── vision.gleam
│   ├── vision_storage.gleam
│   ├── vision_questions.gleam
│   └── vision_critique.gleam
│
├── phase2_shape/
│   ├── shape.gleam
│   ├── shape_storage.gleam
│   ├── shape_questions.gleam
│   ├── mvp_analyzer.gleam
│   └── shape_critique.gleam
│
├── phase3_spec/
│   ├── spec_interview.gleam
│   ├── spec_critique.gleam
│   └── questioning/
│       ├── clarification.gleam
│       ├── edge_cases.gleam
│       ├── business_logic.gleam
│       ├── security.gleam
│       ├── api_design.gleam
│       └── integration.gleam
│
├── phase4_ready/
│   ├── ready.gleam               # READY checker
│   ├── vision_alignment.gleam
│   ├── empathy_simulator.gleam
│   ├── vorp_analyzer.gleam
│   └── ready_critique.gleam
│
├── beads/
│   ├── enhanced_bead.gleam       # New bead types
│   └── bead_generator.gleam      # Generate from Plan
│
└── ai/
    ├── jsonl_input.gleam         # JSONL stdin parser
    ├── command_router.gleam      # Command dispatch
    └── batch_executor.gleam      # Batch processing
```

### New CUE Schemas
```
schema/
├── plan.cue                      # Plan schema
├── bead.cue                      # Enhanced bead schema
│
└── ai/
    ├── envelope.cue              # Request/Response
    ├── errors.cue                # Error taxonomy
    ├── input/
    │   ├── quality.cue
    │   ├── coverage.cue
    │   ├── gaps.cue
    │   ├── vision.cue
    │   ├── shape.cue
    │   ├── spec.cue
    │   ├── ready.cue
    │   └── ... (all commands)
    └── output/
        ├── quality.cue
        ├── coverage.cue
        ├── gaps.cue
        ├── vision.cue
        ├── shape.cue
        ├── spec.cue
        ├── ready.cue
        └── ... (all commands)
```

---

## Files to Delete

```
src/intent/
├── runner.gleam              # HTTP orchestration
├── http_client.gleam         # HTTP client
├── checker.gleam             # Response validation
├── checker/
│   ├── rules.gleam
│   ├── json.gleam
│   ├── headers.gleam
│   └── types.gleam
│
└── security.gleam            # SSRF protection (keep localhost logic)

src/
├── intent_checker.erl        # Regex caching FFI
└── intent_runner_ffi.erl     # Spinner FFI

test/
├── runner_test.gleam
├── runner_executor_test.gleam
├── timeout_test.gleam
└── exit_code_test.gleam
```

---

## Success Criteria

### Core Requirements
- [ ] All 4 phases implemented with interview + critique + gate
- [ ] Pure planning tool - no HTTP, no external requests
- [ ] AI-native JSON output for all commands with next_actions
- [ ] Vision alignment check ensures no drift

### AI-Native Requirements
- [ ] JSONL input via stdin for all commands
- [ ] JSONL output with correlation IDs
- [ ] CUE schemas for all input/output
- [ ] `intent ai batch` processes multiple requests
- [ ] `intent ai schema` enables introspection

### Engineering Depth
- [ ] Enhanced beads with EARS + Contracts + Types + Tests
- [ ] Design by Contract - preconditions/postconditions/invariants
- [ ] Zero ambiguity - Interactive Questioning resolves all unclear requirements
- [ ] Complete test cases - edge cases enumerated

### Quality Validation
- [ ] 5-dimension READY scoring
- [ ] Mental Lattice complete (all 5 rounds)
- [ ] 4 critique personas implemented

---

## Next Steps

1. **Review this plan** - Confirm scope and priorities
2. **Create beads in bd** - Track all 59 work items
3. **Start Wave 0** - Foundation types and storage
4. **Parallel track** - Wave 0.5/0.6 can run alongside Wave 0
5. **Sequential phases** - Waves 1-4 build on each other
6. **Integration** - Wave 5 ties everything together

---

*Generated from comprehensive codebase research on 2026-01-25*
