# Intent CLI 4.0: Complete Rebuild Plan

## Executive Summary

Intent CLI is being rebuilt from the ground up to implement a 4-phase Mental Lattice system that integrates:
- **PME** (Product-Minded Engineer) - Scenarios, Journey Phases, Diagnostics
- **DDD** (Documentation-Driven Development) - Vision capture before specification
- **Empathy Engine** - Cognitive Limitation Protocol + 4 Brutal Truths
- **READY Framework** - Ship readiness validation

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

## Lessons Preserved from Existing Codebase

### Architecture Patterns
| Pattern | Source | Preserve |
|---------|--------|----------|
| Railway-Oriented Programming | All modules | Result types + `use` |
| Functional Core / Imperative Shell | loader.gleam | Pure functions, injected I/O |
| Dependency Injection | CommandExecutor | Testable mocks |
| Output Mode Abstraction | OutputMode | JSON vs interactive |
| Unified JSON Schema | json_output.gleam | `next_actions` for AI |
| Kahn's Algorithm | resolver.gleam | Topological sort |

### Code to Port Directly (~2,200 lines)
- `checker/rules.gleam` (602 lines) - 40+ rule types
- `interpolate.gleam` (335 lines) - Variable substitution
- `array_indexing.gleam` (288 lines) - JSON path navigation
- `security.gleam` (367 lines) - SSRF protection
- `intent_ffi.erl` (59 lines) - Core FFI
- `intent_http_ffi.erl` (68 lines) - HTTP execution
- `intent_checker.erl` (48 lines) - Regex caching

### Tests to Preserve (~1,400 test functions)
- `rule_test.gleam` (200+ tests)
- `resolver_test.gleam` (80+ tests)
- `interpolate_test.gleam` (100+ tests)
- `array_indexing_test.gleam` (72 tests)
- `output_test.gleam` (100+ tests)
- All KIRK analyzer tests (300+ tests)

---

## New Spec Schema

```cue
#Spec: {
    // === PHASE 1: VISION ===
    vision: {
        press_release: string
        persona: string
        non_personas: [...string]
        north_star: string
        scenarios: [...#Scenario]
        replaces: string
        vorp: string
        out_of_scope: [...string]
    }

    // === PHASE 2: SHAPE ===
    shape: {
        features: [...#FeatureShape]
        critical_path: [...string]
        mvp_slice: {
            description: string
            features: [...string]
            shortcuts: [...string]
        }
        post_mvp: [...string]
        validation_moment: string
    }

    // === PHASE 3: SPEC (existing, enhanced) ===
    name: string
    description: string
    audience: string
    version: string
    success_criteria: [...string]
    config: #Config
    features: [...#Feature]
    rules: [...#Rule]
    anti_patterns: [...#AntiPattern]
    ai_hints: #AIHints

    // === PHASE 4: READY ===
    ready: {
        score: int
        checks: {
            replacement: #ReadyCheck
            empathy: #ReadyCheck
            actionable: #ReadyCheck
            discoverable: #ReadyCheck
            complete: #ReadyCheck
        }
        vision_alignment: bool
        approved: bool
        approved_at?: string
        approved_by?: string
    }
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

## Implementation Waves

### Wave 0: Foundation (Port Existing)
- Port core types
- Port checker/rules
- Port interpolation
- Port security
- Port FFI layer
- Port tests

### Wave 1: Phase 1 - Vision
- Vision types
- Vision storage (JSONL)
- Vision interview questions
- Vision critique protocol
- Vision commands

### Wave 2: Phase 2 - Shape
- Shape types
- Shape storage
- MVP analyzer
- Shape critique protocol
- Shape commands

### Wave 3: Phase 3 - Spec (Enhance KIRK)
- Enhance interview with vision/shape linking
- Add ready_score to quality
- Add product gaps to gap_detector
- Spec critique protocol
- Link to Phase 1/2 data

### Wave 4: Phase 4 - Ready
- READY checker (R, E, A, D, Y)
- Vision alignment checker
- Empathy simulator
- VORP analyzer
- Ready critique protocol
- Ready commands

### Wave 5: Integration
- Unified CLI entry point
- Phase state machine
- Gate enforcement
- Bead generation with ready: tags
- Documentation

---

## Command Structure

### Phase 1: Vision
```
vision start [--profile=api|cli]
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
interview start --shape=<id>
quality <spec> [--json]
coverage <spec> [--json]
gaps <spec> [--json]
invert <spec> [--json]
effects <spec> [--json]
ears <file> [--output=cue|json]
spec critique [--json]
spec respond '<text>'
spec agree
```

### Phase 4: Ready
```
ready <spec> [--json]
ready critique [--json]
ready respond '<text>'
ready agree
beads <spec> [--json]
plan <spec> [--json]
check <spec> --target=URL [--json]
feedback --results <file> [--json]
```

---

## Success Criteria

1. **All 4 phases implemented** with interview + critique + gate
2. **All existing tests pass** after port
3. **New tests for each phase** (critique protocol, gate logic)
4. **AI-native JSON output** for all commands
5. **`next_actions`** field guides agents through phases
6. **Vision alignment check** in Phase 4
7. **READY score** includes all 5 dimensions
8. **Beads tagged** with `ready:R/E/A/D/Y` for traceability

---

## Glossary

| Term | Definition |
|------|------------|
| Phase 1: VISION | DDD - big picture understanding before technical work |
| Phase 2: SHAPE | Feature decomposition and MVP definition |
| Phase 3: SPEC | KIRK 5-round deep technical specification |
| Phase 4: READY | Review, validation, and ship decision |
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
| Persona | Specific user with background, means, motivation |
| North Star | The ideal user journey from trigger to success |
| Replaces | Current solution user will abandon |
| VORP | Value Over Replacement Product |
| Friction Log | Step-by-step narrative of user confusion points |
| Mental Lattice | DDD -> KIRK -> READY validation structure |
