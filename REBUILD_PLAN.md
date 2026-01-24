# Intent CLI 4.0: Complete Rebuild Plan

## Executive Summary

Intent CLI is a **planning tool** for software development. It guides humans and AI through a structured 4-phase process to ensure complete understanding before building.

**What it is:**
- A thinking framework encoded as a CLI
- A 4-phase planning system with AI critique at each gate
- A bead generator for work breakdown

**What it is NOT:**
- An API testing framework
- An HTTP client
- A contract verification tool

The tool integrates:
- **PME** (Product-Minded Engineer) - Scenarios, Journey Phases, Diagnostics
- **DDD** (Documentation-Driven Development) - Vision capture before specification
- **Empathy Engine** - Cognitive Limitation Protocol + 4 Brutal Truths
- **READY Framework** - Ship readiness validation

---

## The 4 Phases

```
PHASE 1: VISION (DDD)     → "What are we building and why?"
PHASE 2: SHAPE (MVP)      → "What's the smallest thing that works?"
PHASE 3: SPEC (KIRK)      → "How does it work exactly?"
PHASE 4: READY (Ship)     → "Is this actually ready to build?"
```

Each phase includes:
- User input/interview
- AI Critique (devil's advocate)
- Dialogue until alignment
- Gate check before proceeding

---

## Architecture Patterns to Preserve

| Pattern | Why Keep It |
|---------|-------------|
| Railway-Oriented Programming | Clean error propagation with Result types |
| Functional Core / Imperative Shell | Testable pure functions |
| Output Mode Abstraction | JSON vs interactive switching |
| Unified JSON Schema | `next_actions` for AI guidance |
| Kahn's Algorithm | Topological sort for bead waves |
| JSONL Storage | Session persistence |

---

## Code to Port (Minimal Core)

| Module | Lines | Purpose |
|--------|-------|---------|
| `intent_ffi.erl` | ~50 | UUID, timestamps |
| `interpolate.gleam` | 335 | Variable substitution in specs |
| `resolver.gleam` | ~400 | Dependency resolution (toposort) |
| `plan_mode.gleam` | ~400 | Wave calculation |
| `output.gleam` | ~300 | JSON/text formatting |

**Total: ~1,500 lines** (down from 2,200)

### What Gets Deleted

| Module | Reason |
|--------|--------|
| `checker/` | API response validation - not needed |
| `runner.gleam` | HTTP execution - not needed |
| `http_client.gleam` | HTTP client - not needed |
| `security.gleam` | SSRF protection - no HTTP means no SSRF |
| `intent_http_ffi.erl` | HTTP FFI - not needed |
| `checker/rules.gleam` | Response validation rules - not needed |

---

## New Spec Schema (Planning-Focused)

```cue
#Plan: {
    // === PHASE 1: VISION ===
    vision: {
        press_release: string      // What is this? Why care?
        persona: string            // Who specifically needs this?
        non_personas: [...string]  // Who is this NOT for?
        north_star: string         // The ideal journey
        scenarios: [...#Scenario]  // Detailed user stories
        replaces: string           // Current solution
        vorp: string               // Why they'll switch
        out_of_scope: [...string]  // Explicit boundaries
    }

    // === PHASE 2: SHAPE ===
    shape: {
        features: [...#Feature]    // All capabilities needed
        critical_path: [...string] // Must-have for north star
        mvp_slice: {
            description: string
            features: [...string]
            shortcuts: [...string] // What we can fake/defer
        }
        post_mvp: [...string]      // Explicitly deferred
        validation_moment: string  // The "aha" moment
    }

    // === PHASE 3: SPEC ===
    spec: {
        name: string
        description: string
        audience: string
        success_criteria: [...string]
        features: [...#FeatureSpec]
        rules: [...#Rule]              // Business rules
        anti_patterns: [...#AntiPattern]
        ai_hints: #AIHints
    }

    // === PHASE 4: READY ===
    ready: {
        score: int                     // 0-100
        checks: {
            replacement: #ReadyCheck   // R: Better than current?
            empathy: #ReadyCheck       // E: Friction simulated?
            actionable: #ReadyCheck    // A: Errors guide users?
            discoverable: #ReadyCheck  // D: Features findable?
            complete: #ReadyCheck      // Y: North star achievable?
        }
        vision_alignment: bool         // Spec matches vision?
        approved: bool
        approved_at?: string
        approved_by?: string
    }
}

#Scenario: {
    id: string
    title: string
    persona: string
    motivation: string             // The "I want" moment
    steps: [...#ScenarioStep]
    success_looks_like: string
    failure_modes: [...string]
}

#ScenarioStep: {
    action: string                 // What user does
    sees: string                   // What they see
    thinks: string                 // What they're thinking
    risk?: string                  // What could go wrong
}

#Feature: {
    name: string
    description: string
    behaviors: [...string]         // What it does
    requires: [...string]          // Dependencies
}

#FeatureSpec: {
    name: string
    description: string
    behaviors: [...#Behavior]
    acceptance_criteria: [...string]
}

#Behavior: {
    name: string
    intent: string                 // Why this exists
    given: string                  // Preconditions
    when: string                   // Trigger
    then: string                   // Outcome
    notes?: string
    requires: [...string]
    tags: [...string]
}

#Rule: {
    name: string
    description: string
    type: "business" | "validation" | "constraint"
}

#AntiPattern: {
    name: string
    description: string
    why_bad: string
    alternative: string
}

#AIHints: {
    implementation: [...string]
    entities: [...string]
    pitfalls: [...string]
}

#ReadyCheck: {
    pass: bool
    details: string
    blockers?: [...string]
}
```

---

## AI Critique Personas

| Phase | Persona | Core Challenge |
|-------|---------|----------------|
| 1 VISION | Skeptical PM | "Is this real or wishful thinking?" |
| 2 SHAPE | Pragmatic Tech Lead | "Can we cut more? Will this validate?" |
| 3 SPEC | Adversarial QA | "What breaks? What's missing?" |
| 4 READY | Pre-Launch Auditor | "Did we stay true? Are we ready?" |

Each critique includes:
- Structured questions per area
- Blocking questions that must be answered
- Alignment check (both sides agree)
- Gate unlock on agreement

---

## Module Structure

```
src/intent/
├── core/
│   ├── types.gleam           # Plan, Vision, Shape, Spec, Ready types
│   ├── parser.gleam          # CUE/JSON parsing
│   ├── loader.gleam          # File loading
│   ├── interpolate.gleam     # Variable substitution
│   └── resolver.gleam        # Dependency resolution
│
├── phase1_vision/
│   ├── vision.gleam          # Vision state machine
│   ├── vision_storage.gleam  # JSONL persistence
│   ├── vision_questions.gleam
│   ├── vision_critique.gleam # Skeptical PM
│   └── vision_export.gleam
│
├── phase2_shape/
│   ├── shape.gleam           # Feature decomposition
│   ├── shape_storage.gleam
│   ├── mvp_analyzer.gleam    # MVP detection
│   ├── shape_critique.gleam  # Pragmatic Tech Lead
│   └── shape_beads.gleam
│
├── phase3_spec/
│   ├── interview.gleam       # 5-round KIRK interview
│   ├── interview_storage.gleam
│   ├── kirk/
│   │   ├── quality_analyzer.gleam
│   │   ├── coverage_analyzer.gleam
│   │   ├── gap_detector.gleam
│   │   ├── inversion_checker.gleam
│   │   ├── effects_analyzer.gleam
│   │   └── ears_parser.gleam
│   ├── spec_critique.gleam   # Adversarial QA
│   └── spec_builder.gleam
│
├── phase4_ready/
│   ├── ready.gleam           # READY checker
│   ├── ready_critique.gleam  # Pre-Launch Auditor
│   ├── vision_alignment.gleam
│   ├── empathy_simulator.gleam
│   └── vorp_analyzer.gleam
│
├── beads/
│   ├── bead_types.gleam
│   ├── bead_generator.gleam  # Generate from plan
│   └── plan_mode.gleam       # Waves, dependencies
│
├── output/
│   ├── output.gleam
│   ├── json_output.gleam
│   └── output_mode.gleam
│
└── main.gleam                # CLI entry
```

---

## Command Structure

### Phase 1: Vision
```
vision start [--profile=api|cli|ui]
vision parse <file.md> [--json]
vision check [--json]
vision critique [--json]
vision respond '<text>'
vision agree
vision export <session-id>
```

### Phase 2: Shape
```
shape start --vision=<id>
shape check [--json]
shape critique [--json]
shape respond '<text>'
shape agree
shape beads [--json]
```

### Phase 3: Spec
```
spec start --shape=<id>
quality <plan> [--json]
coverage <plan> [--json]
gaps <plan> [--json]
invert <plan> [--json]
effects <plan> [--json]
ears <file> [--output=cue|json]
spec critique [--json]
spec respond '<text>'
spec agree
```

### Phase 4: Ready
```
ready <plan> [--json]
ready critique [--json]
ready respond '<text>'
ready agree
beads <plan> [--json]
plan <plan> [--json]
prompt <plan> [--json]
```

### Utility
```
sessions [--phase=1|2|3|4]
history <session-id>
diff <session-id1> <session-id2>
export <session-id> [--output=plan.cue]
```

---

## Implementation Waves

### Wave 0: Foundation
- Core types (Plan, Vision, Shape, Spec, Ready)
- JSONL storage pattern
- Output formatting (JSON + text)
- FFI utilities (UUID, timestamps)
- Resolver (topological sort)

### Wave 1: Phase 1 - Vision
- Vision types and storage
- Vision interview questions (8 questions)
- Vision critique (Skeptical PM)
- Vision commands

### Wave 2: Phase 2 - Shape
- Shape types and storage
- MVP analyzer
- Shape critique (Pragmatic Tech Lead)
- Shape commands

### Wave 3: Phase 3 - Spec
- Port KIRK analyzers (quality, coverage, gaps, invert, effects, ears)
- Spec interview (5 rounds)
- Spec critique (Adversarial QA)
- Spec commands

### Wave 4: Phase 4 - Ready
- READY checker (R, E, A, D, Y)
- Vision alignment checker
- Empathy simulator
- VORP analyzer
- Ready critique (Pre-Launch Auditor)

### Wave 5: Integration
- Unified CLI entry
- Phase state machine
- Gate enforcement
- Bead generation
- Documentation

---

## Success Criteria

1. **All 4 phases implemented** with interview + critique + gate
2. **Pure planning tool** - no HTTP, no external requests
3. **AI-native JSON output** for all commands
4. **`next_actions`** field guides agents through phases
5. **Vision alignment check** ensures no drift
6. **READY score** includes all 5 dimensions
7. **Beads generated** with phase traceability

---

## Glossary

| Term | Definition |
|------|------------|
| Phase 1: VISION | DDD - big picture understanding before technical work |
| Phase 2: SHAPE | Feature decomposition and MVP definition |
| Phase 3: SPEC | KIRK 5-round deep technical specification |
| Phase 4: READY | Review, validation, and ship decision |
| Plan | The complete output: vision + shape + spec + ready |
| Vision Doc | Press release + persona + north star + VORP + boundaries |
| Feature Map | All features with critical path and MVP slice |
| MVP Slice | Smallest subset that delivers validation moment |
| Validation Moment | The one thing that proves the concept works |
| Critical Path | Features required for north star to be achievable |
| KIRK | 5-round mental model specification system |
| EARS | Easy Approach to Requirements Syntax |
| READY | Replacement, Empathy, Actionable, Discoverable, Yet-complete |
| Vision Alignment | Phase 4 check that spec still matches Phase 1 vision |
| Critique | AI devil's advocate challenge at each phase |
| Gate | Checkpoint requiring AI + human agreement |
| Bead | Atomic work unit generated from the plan |
| Wave | Parallel bead group (same dependency depth) |
| Persona | Specific user with background, means, motivation |
| North Star | The ideal user journey from trigger to success |
| Replaces | Current solution user will abandon |
| VORP | Value Over Replacement Product |
| Friction Log | Step-by-step narrative of user confusion points |
| Mental Lattice | DDD → KIRK → READY validation structure |
