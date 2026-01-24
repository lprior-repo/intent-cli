# Intent CLI 4.0: Complete Engineering Specification System

## Executive Summary

Intent CLI is a **complete engineering specification system** that takes you from "I want to build X" all the way down to database schemas, algorithms, security configurations, test cases, and atomic work items. It guides humans and AI through a structured 4-phase process ensuring ZERO ambiguity before implementation.

**What it produces:**
- Vertical slice architecture with small files (<200 lines) AI can work with
- Functional programming structure (Result types, pure core, pipelines)
- Complete test cases with edge cases and property-based tests
- Implementation patterns and anti-patterns with examples
- Atomic work items (beads) with all ambiguities resolved

**What it is:**
- A thinking framework encoded as a CLI
- A 4-phase planning system with AI critique at each gate
- A Design by Contract specification generator
- A zero-ambiguity questioning system
- A bead generator for deterministic AI implementation

**What it is NOT:**
- An API testing framework
- An HTTP client
- A runtime verification tool

The tool integrates:
- **Mental Lattice** - 5 layers: EARS → Contracts → Inversion → Second-Order → Pre-mortem
- **PME** (Product-Minded Engineer) - Scenarios, Journey Phases, Diagnostics
- **DDD** (Documentation-Driven Development) - Vision capture before specification
- **KIRK** (Knowledge-Informed Requirements & Kontract) - Design by Contract with preconditions/postconditions/invariants
- **EARS** (Easy Approach to Requirements Syntax) - 6 structured requirement patterns
- **Interactive Questioning** - 6 categories for zero ambiguity
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

## The 5 Mental Lattices

Each phase builds on structured mental models that eliminate ambiguity:

```
┌─────────────────────────────────────────────────────────────────────────┐
│  LATTICE 1: EARS (Requirements Syntax)                                   │
│  - Ubiquitous:    THE SYSTEM SHALL [behavior]                           │
│  - Event-Driven:  WHEN [trigger] THE SYSTEM SHALL [behavior]            │
│  - State-Driven:  WHILE [state] THE SYSTEM SHALL [behavior]             │
│  - Optional:      WHERE [condition] THE SYSTEM SHALL [behavior]         │
│  - Unwanted:      IF [condition] THEN THE SYSTEM SHALL NOT [behavior]   │
│  - Complex:       WHILE [state] WHEN [trigger] THE SYSTEM SHALL ...     │
├─────────────────────────────────────────────────────────────────────────┤
│  LATTICE 2: KIRK Contracts (Design by Contract)                         │
│  - Preconditions:  What must be true BEFORE                             │
│  - Postconditions: What will be true AFTER                              │
│  - Invariants:     What is ALWAYS true                                  │
├─────────────────────────────────────────────────────────────────────────┤
│  LATTICE 3: Inversion Thinking (Failure Analysis)                       │
│  - Security:    auth-bypass, expired-token, privilege-escalation        │
│  - Usability:   not-found, invalid-format, missing-required             │
│  - Integration: idempotency, timeout-handling, version-mismatch         │
├─────────────────────────────────────────────────────────────────────────┤
│  LATTICE 4: Second-Order Thinking (Consequence Tracing)                 │
│  - First-order:  The immediate effect                                   │
│  - Second-order: Cascade consequences (orphaned data, invalidated sessions) │
│  - Consequence checks: Behaviors that verify cascades handled           │
├─────────────────────────────────────────────────────────────────────────┤
│  LATTICE 5: Pre-Mortem Analysis (Risk Prediction)                       │
│  - Assumed failure: "The launch failed catastrophically after 1 week"   │
│  - Likely causes with probability and mitigation                        │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Interactive Questioning System (Zero Ambiguity)

After EARS parsing, 6 question categories resolve ALL remaining ambiguity:

| Category | Purpose | Example |
|----------|---------|---------|
| **1. Clarification** | Resolve ambiguous EARS | "What does 'handle appropriately' mean?" |
| **2. Edge Cases** | Confirm unusual inputs | "Accept unicode email user@例子.中国?" |
| **3. Business Logic** | Domain-specific rules | "Hard delete or soft delete on account removal?" |
| **4. Security** | Critical security choices | "bcrypt cost 10, 12, or Argon2id?" |
| **5. API Design** | Structure and conventions | "RFC 7807 error format or simple JSON?" |
| **6. Integration** | External system behavior | "SMTP timeout: 5s, 30s, or 60s?" |

**Key Insight**: By the time a bead reaches the AI, every possible question has been answered, every edge case enumerated, and implementation is purely mechanical.

---

## Complete Spec Schema (Engineering-Depth)

```cue
#Plan: {
    // === PHASE 1: VISION (DDD) ===
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

    // === PHASE 2: SHAPE (MVP) ===
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

    // === PHASE 3: SPEC (KIRK + Mental Lattices) ===
    spec: {
        name: string
        description: string
        audience: string
        success_criteria: [...string]

        // EARS Requirements
        requirements: [...#EARSRequirement]

        // Feature Specifications with Contracts
        features: [...#FeatureSpec]

        // Mental Lattice Outputs
        inversions: #Inversions           // Lattice 3
        second_order_effects: [...#SecondOrderEffect]  // Lattice 4
        pre_mortem: #PreMortem            // Lattice 5

        // Business Rules & Anti-Patterns
        rules: [...#Rule]
        anti_patterns: [...#AntiPattern]

        // Deep AI Implementation Hints
        ai_hints: #AIHints

        // Interactive Questioning Answers
        answers: #Answers

        // Quality Scores
        quality: #QualityScore
    }

    // === PHASE 4: READY (Ship Decision) ===
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

// === EARS REQUIREMENTS ===
#EARSRequirement: {
    id: string
    type: "ubiquitous" | "event_driven" | "state_driven" | "optional" | "unwanted" | "complex"
    trigger?: string              // WHEN clause
    state?: string                // WHILE clause
    condition?: string            // WHERE/IF clause
    behavior: string              // SHALL/SHALL NOT clause
    rationale: string             // Why this requirement exists
}

// === SCENARIOS ===
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

// === FEATURES ===
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

// === BEHAVIORS WITH DESIGN BY CONTRACT ===
#Behavior: {
    name: string
    intent: string                 // Why this exists

    // DESIGN BY CONTRACT (Lattice 2)
    preconditions: {
        auth_required: bool
        required_fields: [...string]
        field_constraints: {[string]: string}
    }

    postconditions: {
        state_changes: [...string]
        response_guarantees: {[string]: string}
    }

    invariants: [...string]        // Always true for this object

    // Behavior Definition (Given/When/Then)
    given: string                  // Preconditions narrative
    when: string                   // Trigger
    then: string                   // Outcome

    // Second-Order Effects (Lattice 4)
    second_order_effects?: [...string]
    consequence_checks?: [...{behavior: string, expect: string}]

    notes?: string
    requires: [...string]
    tags: [...string]
}

// === MENTAL LATTICE 3: INVERSION ===
#Inversions: {
    security_failures: [...{
        name: string               // e.g., "auth-bypass"
        description: string
        expected_status: int       // e.g., 401
        test_scenario: string
    }]
    usability_failures: [...{
        name: string               // e.g., "not-found"
        description: string
        expected_status: int       // e.g., 404
        test_scenario: string
    }]
    integration_failures: [...{
        name: string               // e.g., "idempotency"
        description: string
        expected_status: int
        test_scenario: string
    }]
}

// === MENTAL LATTICE 4: SECOND-ORDER EFFECTS ===
#SecondOrderEffect: {
    trigger_behavior: string       // e.g., "delete-user"
    first_order: string            // Immediate effect
    consequences: [...string]      // Cascade effects
    verification_behaviors: [...string]  // How to verify handled
}

// === MENTAL LATTICE 5: PRE-MORTEM ===
#PreMortem: {
    assumed_failure: string        // "The launch failed catastrophically after 1 week"
    likely_causes: [...{
        cause: string
        probability: "high" | "medium" | "low"
        mitigation: string
    }]
}

// === RULES & ANTI-PATTERNS ===
#Rule: {
    name: string
    description: string
    type: "business" | "validation" | "constraint" | "invariant"
}

#AntiPattern: {
    name: string
    description: string
    bad_example: string            // Code/JSON that demonstrates the problem
    good_example: string           // Correct alternative
    why_bad: string
}

// === DEEP AI IMPLEMENTATION HINTS ===
#AIHints: {
    // Architecture Principles
    architecture: {
        style: "vertical_slice" | "clean" | "hexagonal"

        // Vertical Slice: Each feature is self-contained
        slices: [...{
            name: string           // "create-user", "delete-item"
            files: [...string]     // All files for this slice
            public_api: string     // Single entry point
        }]

        // File Size Constraints (AI-friendly)
        constraints: {
            max_file_lines: int    // Default: 200 lines
            max_function_lines: int // Default: 30 lines
            max_parameters: int    // Default: 4
        }
    }

    // Functional Programming Principles
    functional: {
        // Result types for all fallible operations
        error_handling: "result_type" | "option_type" | "either"

        // Pure functions at the core
        patterns: {
            pure_core: bool        // Business logic is pure
            imperative_shell: bool // IO at the edges only
            immutable_data: bool   // No mutation
            pipeline_style: bool   // |> composition
        }

        // Type system usage
        types: {
            exhaustive_matching: bool  // All cases handled
            newtype_wrappers: bool     // UserId not String
            phantom_types: bool        // Compile-time state
        }
    }

    // Data Models (structure, not SQL)
    entities: {[string]: {
        fields: {[string]: string} // field_name: "Type + constraints description"
        validations: [...string]   // Business rules for this entity
        relations: [...{
            to: string
            cardinality: "one" | "many"
        }]
    }}

    // Test Strategy
    testing: {
        unit: {
            coverage_target: int   // e.g., 80
            patterns: [...string]  // "property-based", "example-based"
        }
        integration: {
            scope: string          // "vertical slice boundaries"
            patterns: [...string]
        }
        edge_cases: [...{
            input: string
            expected: string
            rationale: string
        }]
    }

    // Explicit Anti-Patterns (What NOT to do)
    pitfalls: [...{
        mistake: string            // "God object with 50 methods"
        consequence: string        // "Impossible to test in isolation"
        prevention: string         // "Split into vertical slices"
    }]

    // Code Patterns to Follow
    code_patterns: [...{
        name: string
        description: string
        example: string            // Actual code snippet
        when_to_use: string
    }]
}

// === INTERACTIVE QUESTIONING ANSWERS ===
#Answers: {
    clarifications: {[string]: string}
    edge_cases: {[string]: {
        accept: bool
        response: int
        message?: string
    }}
    business: {[string]: string}
    security: {[string]: string | {[string]: _}}
    api: {[string]: string}
    integration: {[string]: string}
}

// === QUALITY SCORES ===
#QualityScore: {
    completeness: {score: int, issues: [...string]}
    consistency: {score: int, issues: [...string]}
    testability: {score: int, behaviors_with_checks: int, total_behaviors: int}
    clarity: {score: int, missing_why: [...string]}
    security: {score: int, patterns_tested: [...string], patterns_missing: [...string]}
    overall: int
}

// === READY CHECKS ===
#ReadyCheck: {
    pass: bool
    details: string
    blockers?: [...string]
}
```

---

## Example: Deep ai_hints for User Management Feature

```cue
ai_hints: {
    architecture: {
        style: "vertical_slice"

        slices: [
            {
                name: "create-user"
                files: [
                    "src/features/user/create/handler.gleam",    // ~50 lines
                    "src/features/user/create/validator.gleam",  // ~30 lines
                    "src/features/user/create/repository.gleam", // ~40 lines
                    "src/features/user/create/types.gleam",      // ~20 lines
                ]
                public_api: "create_user(input: CreateUserInput) -> Result(User, CreateUserError)"
            },
            {
                name: "get-user"
                files: [
                    "src/features/user/get/handler.gleam",
                    "src/features/user/get/repository.gleam",
                ]
                public_api: "get_user(id: UserId) -> Result(User, GetUserError)"
            }
        ]

        constraints: {
            max_file_lines: 200
            max_function_lines: 30
            max_parameters: 4
        }
    }

    functional: {
        error_handling: "result_type"

        patterns: {
            pure_core: true         // All business logic is pure
            imperative_shell: true  // IO only at edges (main, handlers)
            immutable_data: true    // No let mut, no reassignment
            pipeline_style: true    // input |> validate |> transform |> persist
        }

        types: {
            exhaustive_matching: true  // Every Result/Option case handled
            newtype_wrappers: true     // UserId(String), Email(String)
            phantom_types: false       // Not needed for this scope
        }
    }

    entities: {
        User: {
            fields: {
                id: "UserId - prefixed random string, immutable after creation"
                email: "Email - validated format, unique across system"
                name: "String - 1-100 chars, trimmed"
                created_at: "DateTime - set once at creation"
            }
            validations: [
                "Email must be valid format",
                "Name must be 1-100 characters",
                "Email must be unique (check before insert)"
            ]
            relations: [
                { to: "Session", cardinality: "many" }
            ]
        }
    }

    testing: {
        unit: {
            coverage_target: 90
            patterns: ["property-based for validators", "example-based for transformers"]
        }
        integration: {
            scope: "Each vertical slice tested end-to-end"
            patterns: ["in-memory repository for speed", "real DB for CI"]
        }
        edge_cases: [
            {
                input: "email with unicode: user@例子.中国"
                expected: "accepted if unicode emails enabled, rejected otherwise"
                rationale: "Unicode domain names are valid per RFC 6531"
            },
            {
                input: "empty string email"
                expected: "Error(ValidationError(EmptyEmail))"
                rationale: "Email is required, empty is not valid format"
            },
            {
                input: "name with only whitespace"
                expected: "Error(ValidationError(BlankName))"
                rationale: "Trimmed length must be >= 1"
            }
        ]
    }

    pitfalls: [
        {
            mistake: "God module with all user operations"
            consequence: "500+ line file, impossible to test in isolation, merge conflicts"
            prevention: "Split into vertical slices: create/, get/, update/, delete/"
        },
        {
            mistake: "Business logic mixed with IO"
            consequence: "Can't unit test without mocking everything"
            prevention: "Pure core: validate(input) -> Result, then persist at edge"
        },
        {
            mistake: "Stringly-typed IDs and emails"
            consequence: "Easy to mix up UserId with SessionId, no compile-time safety"
            prevention: "Newtype wrappers: type UserId { UserId(String) }"
        },
        {
            mistake: "Ignoring error cases, using unwrap/expect"
            consequence: "Runtime panics, unclear error messages"
            prevention: "Exhaustive pattern matching on all Result/Option"
        }
    ]

    code_patterns: [
        {
            name: "Railway-Oriented Pipeline"
            description: "Chain operations that can fail, short-circuit on first error"
            when_to_use: "Any multi-step operation where each step can fail"
            example: """
                pub fn create_user(input: CreateUserInput, repo: Repository) -> Result(User, CreateUserError) {
                    input
                    |> validate_email
                    |> result.try(validate_name)
                    |> result.try(check_email_unique(_, repo))
                    |> result.try(build_user)
                    |> result.try(persist(_, repo))
                }
            """
        },
        {
            name: "Newtype Wrapper"
            description: "Type-safe wrapper to prevent mixing up String IDs"
            when_to_use: "Any identifier that could be confused with another"
            example: """
                pub type UserId { UserId(String) }
                pub type SessionId { SessionId(String) }

                // Compile error: can't pass SessionId where UserId expected
                pub fn get_user(id: UserId) -> Result(User, Error)
            """
        },
        {
            name: "Pure Validator"
            description: "Validation as pure function returning Result"
            when_to_use: "All input validation"
            example: """
                pub fn validate_email(input: String) -> Result(Email, ValidationError) {
                    case string.contains(input, "@") {
                        True -> Ok(Email(string.trim(input)))
                        False -> Error(InvalidEmailFormat)
                    }
                }
            """
        }
    ]
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
│   └── resolver.gleam        # Dependency resolution (Kahn's toposort)
│
├── phase1_vision/
│   ├── vision.gleam          # Vision state machine
│   ├── vision_storage.gleam  # JSONL persistence
│   ├── vision_questions.gleam # 8 core questions
│   ├── vision_critique.gleam # Skeptical PM persona
│   └── vision_export.gleam
│
├── phase2_shape/
│   ├── shape.gleam           # Feature decomposition
│   ├── shape_storage.gleam
│   ├── mvp_analyzer.gleam    # MVP detection
│   ├── shape_critique.gleam  # Pragmatic Tech Lead persona
│   └── shape_beads.gleam
│
├── phase3_spec/
│   ├── interview.gleam       # 5x5 KIRK interview matrix
│   ├── interview_storage.gleam
│   ├── questioning/
│   │   ├── clarification.gleam     # Category 1: Ambiguous EARS
│   │   ├── edge_cases.gleam        # Category 2: Unusual inputs
│   │   ├── business_logic.gleam    # Category 3: Domain rules
│   │   ├── security.gleam          # Category 4: Critical security
│   │   ├── api_design.gleam        # Category 5: Structure/conventions
│   │   └── integration.gleam       # Category 6: External systems
│   ├── kirk/
│   │   ├── ears_parser.gleam       # Lattice 1: EARS patterns
│   │   ├── contract_builder.gleam  # Lattice 2: Design by Contract
│   │   ├── inversion_checker.gleam # Lattice 3: Failure analysis
│   │   ├── effects_analyzer.gleam  # Lattice 4: Second-order effects
│   │   ├── premortem.gleam         # Lattice 5: Risk prediction
│   │   ├── quality_analyzer.gleam  # 5-dimension quality scoring
│   │   ├── coverage_analyzer.gleam # OWASP + edge case coverage
│   │   └── gap_detector.gleam      # Missing requirements
│   ├── spec_critique.gleam   # Adversarial QA persona
│   ├── spec_builder.gleam    # Build complete spec from answers
│   └── compact_format.gleam  # CIN for 50% token reduction
│
├── phase4_ready/
│   ├── ready.gleam           # READY checker (R,E,A,D,Y)
│   ├── ready_critique.gleam  # Pre-Launch Auditor persona
│   ├── vision_alignment.gleam # Check spec matches vision
│   ├── empathy_simulator.gleam # Cognitive Limitation Protocol
│   └── vorp_analyzer.gleam   # Value Over Replacement Product
│
├── beads/
│   ├── bead_types.gleam
│   ├── bead_generator.gleam  # Generate from plan with full context
│   ├── plan_mode.gleam       # Waves, dependencies (toposort)
│   └── test_generator.gleam  # Generate test cases from spec
│
├── output/
│   ├── output.gleam
│   ├── json_output.gleam     # AI-native JSON with next_actions
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

### Wave 0: Foundation (~800 lines)
- Core types (Plan, Vision, Shape, Spec, Ready, all nested types)
- Design by Contract types (Preconditions, Postconditions, Invariants)
- JSONL storage pattern (session persistence)
- Output formatting (JSON + text with `next_actions`)
- FFI utilities (UUID, timestamps)
- Resolver (Kahn's algorithm for topological sort)

### Wave 1: Phase 1 - Vision (~600 lines)
- Vision types and storage
- Vision interview questions (8 core questions):
  1. What problem are we solving?
  2. Who specifically needs this?
  3. What do they currently use?
  4. What is their ideal journey?
  5. What would make them switch?
  6. What is explicitly out of scope?
  7. What does success look like?
  8. What could go wrong?
- Vision critique (Skeptical PM persona)
- Vision commands (start, parse, check, critique, respond, agree, export)

### Wave 2: Phase 2 - Shape (~500 lines)
- Shape types and storage
- MVP analyzer (critical path detection)
- Feature decomposition
- Validation moment identification
- Shape critique (Pragmatic Tech Lead persona)
- Shape commands (start, check, critique, respond, agree, beads)

### Wave 3: Phase 3 - Spec (KIRK + Mental Lattices) (~2000 lines)
**Lattice 1: EARS Parser** (port existing ~600 lines)
- Parse 6 EARS patterns from natural language
- Generate structured requirements

**Lattice 2: Contract Builder** (~400 lines)
- Extract preconditions from requirements
- Generate postconditions for each behavior
- Identify invariants across the system

**Lattice 3: Inversion Checker** (port existing ~500 lines)
- Security failure analysis (24+ patterns)
- Usability failure analysis
- Integration failure analysis

**Lattice 4: Effects Analyzer** (port existing ~300 lines)
- Second-order consequence tracing
- Cascade effect enumeration
- Verification behavior generation

**Lattice 5: Pre-mortem** (~200 lines)
- Risk prediction framework
- Mitigation generation

**Interactive Questioning System** (~500 lines)
- 6 question categories
- Answer file format (CUE)
- Question priority (P0 blocking → P2 defaults)

**Quality & Coverage** (port existing ~800 lines)
- 5-dimension quality scoring
- OWASP Top 10 coverage
- Gap detection

**Spec Critique** (Adversarial QA persona)
- Spec commands

### Wave 4: Phase 4 - Ready (~600 lines)
- READY checker (5 dimensions):
  - **R**eplacement: Better than current solution?
  - **E**mpathy: Friction simulated with Cognitive Limitation Protocol?
  - **A**ctionable: Errors guide users to success?
  - **D**iscoverable: Features findable without documentation?
  - **Y**et-complete: North star journey achievable?
- Vision alignment checker (spec matches Phase 1 vision)
- Empathy simulator (4 Brutal Truths validation)
- VORP analyzer (Value Over Replacement Product)
- Ready critique (Pre-Launch Auditor persona)

### Wave 5: Bead Generation (~500 lines)
- Bead generator from complete plan
- Test case generator from spec
- Wave calculation (dependency depth)
- Full context embedding in each bead:
  - All relevant answers from questioning
  - Database schema from ai_hints
  - Security configuration
  - Anti-patterns to avoid
  - Test cases to implement

### Wave 6: Integration (~400 lines)
- Unified CLI entry
- Phase state machine with gate enforcement
- Session management across phases
- Export to CUE/JSON
- Documentation generation

**Total Estimated: ~5,400 lines** (of which ~2,200 ported from existing KIRK)

---

## Success Criteria

### Core Requirements
1. **All 4 phases implemented** with interview + critique + gate
2. **Pure planning tool** - no HTTP, no external requests, no runtime verification
3. **AI-native JSON output** for all commands with `next_actions` field
4. **Vision alignment check** ensures no drift from Phase 1 to Phase 4

### Engineering Depth
5. **Vertical slice architecture** - each feature self-contained with small files (<200 lines)
6. **Functional programming principles** - pure core, Result types, exhaustive matching, pipelines
7. **Design by Contract** - every behavior has preconditions, postconditions, invariants
8. **Zero ambiguity** - Interactive Questioning resolves ALL unclear requirements
9. **Complete test cases** - edge cases enumerated, property-based tests where applicable

### Quality Validation
10. **5-dimension quality scoring** - Completeness, Consistency, Testability, Clarity, Security
11. **OWASP Top 10 coverage** - security failure modes analyzed
12. **Mental Lattice complete** - all 5 lattices applied (EARS → Contract → Inversion → 2nd Order → Pre-mortem)
13. **READY score** includes all 5 dimensions (R, E, A, D, Y)

### Output Quality
14. **Beads contain full context** - each bead includes:
    - All relevant questioning answers
    - Database schema excerpts
    - Security configuration
    - Anti-patterns to avoid
    - Test cases to implement
    - Edge cases confirmed
15. **Mechanical implementation** - AI can implement from bead without asking clarifying questions

---

## What Makes This World-Class

### 1. Deterministic Planning
- **EARS** eliminates natural language ambiguity
- **KIRK contracts** define machine-checkable success/failure
- **Interactive Questioning** resolves every edge case BEFORE implementation
- **Beads** are self-contained - all context embedded

### 2. AI-Friendly Architecture
- **Small files** (<200 lines) - fits in AI context window
- **Vertical slices** - each feature self-contained, easy to understand
- **Pure functions** - predictable, testable, no hidden state
- **Explicit types** - UserId not String, compile-time safety

### 3. Functional Programming Rigor
- **Result types** for all fallible operations (no exceptions)
- **Exhaustive matching** - every case handled at compile time
- **Pipeline style** - `input |> validate |> transform |> persist`
- **Pure core / Imperative shell** - IO only at the edges

### 4. Mental Model Integration
- **Inversion**: "What could fail?" → comprehensive failure modes
- **Pre-mortem**: "Why did this fail?" → proactive risk mitigation
- **Second-order**: "What happens after?" → cascade effects handled
- **Empathy Engine**: "What confuses users?" → friction eliminated

### 5. Test-First Quality
- **Property-based tests** for validators and transformers
- **Edge cases enumerated** in spec before implementation
- **Vertical slice integration tests** - each feature tested end-to-end
- **Anti-patterns documented** with bad/good examples

### 6. AI-Native Design
- **Token efficiency**: CIN format for 50% reduction
- **Structured prompts**: Beads → implementation without hallucination
- **next_actions**: AI knows exactly what to do next
- **Zero guessing**: Everything specified, nothing assumed

---

## Glossary

### Phases
| Term | Definition |
|------|------------|
| Phase 1: VISION | DDD - big picture understanding before technical work |
| Phase 2: SHAPE | Feature decomposition and MVP definition |
| Phase 3: SPEC | KIRK + Mental Lattices deep technical specification |
| Phase 4: READY | Review, validation, and ship decision |
| Plan | The complete output: vision + shape + spec + ready + beads |

### Mental Lattices
| Term | Definition |
|------|------------|
| Mental Lattice | 5-layer thinking framework: EARS → Contracts → Inversion → 2nd Order → Pre-mortem |
| EARS | Easy Approach to Requirements Syntax - 6 patterns eliminating ambiguity |
| KIRK | Knowledge-Informed Requirements & Kontract - Design by Contract for APIs |
| Design by Contract | Preconditions + Postconditions + Invariants for each behavior |
| Precondition | What must be true BEFORE a function executes |
| Postcondition | What will be true AFTER a function executes |
| Invariant | What must ALWAYS be true for an object/system |
| Inversion | "What could fail?" - systematic failure mode analysis |
| Second-Order Effects | Cascade consequences beyond the immediate effect |
| Pre-mortem | "Why did this fail?" - prospective hindsight risk analysis |

### Interactive Questioning
| Term | Definition |
|------|------------|
| Clarification Question | Resolves ambiguous EARS requirements |
| Edge Case Question | Confirms behavior for unusual inputs |
| Business Logic Question | Clarifies domain-specific rules |
| Security Question | Confirms critical security choices |
| API Design Question | Confirms structure and conventions |
| Integration Question | Confirms external system behavior |
| P0 Blocking | Question that must be answered to proceed |
| P1 Important | Significantly affects implementation |
| P2 Nice-to-have | Can use sensible defaults |

### Vision & PME
| Term | Definition |
|------|------------|
| Vision Doc | Press release + persona + north star + VORP + boundaries |
| Persona | Specific user with background, means, motivation |
| Non-Personas | Who this is explicitly NOT for |
| North Star | The ideal user journey from trigger to success |
| Replaces | Current solution user will abandon |
| VORP | Value Over Replacement Product - why they'll switch |
| Scenario | Character (persona + motivation) + Simulation (the plot) |
| Friction Log | Step-by-step narrative of user confusion points |
| 4 Brutal Truths | Scale is hard, Value is back-loaded, VORP matters, Sustaining is hard |

### Shape & MVP
| Term | Definition |
|------|------------|
| Feature Map | All features with critical path and MVP slice |
| Critical Path | Features required for north star to be achievable |
| MVP Slice | Smallest subset that delivers validation moment |
| Validation Moment | The one thing that proves the concept works |
| Post-MVP | Features explicitly deferred to later phases |

### Quality & Validation
| Term | Definition |
|------|------------|
| READY | Replacement, Empathy, Actionable, Discoverable, Yet-complete |
| Vision Alignment | Phase 4 check that spec still matches Phase 1 vision |
| Quality Score | 5-dimension score: Completeness, Consistency, Testability, Clarity, Security |
| Empathy Simulator | Cognitive Limitation Protocol - simulates user confusion |
| Gap Detection | Finding missing requirements across all lattices |

### Critique System
| Term | Definition |
|------|------------|
| Critique | AI devil's advocate challenge at each phase |
| Skeptical PM | Phase 1 critique persona - "Is this real or wishful thinking?" |
| Pragmatic Tech Lead | Phase 2 critique persona - "Can we cut more?" |
| Adversarial QA | Phase 3 critique persona - "What breaks? What's missing?" |
| Pre-Launch Auditor | Phase 4 critique persona - "Did we stay true? Are we ready?" |
| Gate | Checkpoint requiring AI + human agreement to proceed |

### Beads & Execution
| Term | Definition |
|------|------------|
| Bead | Atomic 5-30 min work unit with full context embedded |
| Wave | Parallel bead group (same dependency depth) |
| Toposort | Kahn's algorithm for dependency ordering |
| CIN | Compact Intent Notation - 50% token reduction format |

### AI Hints (Architecture & Code Quality)
| Term | Definition |
|------|------------|
| ai_hints.architecture | Vertical slice structure, file constraints, public APIs |
| ai_hints.functional | FP principles - Result types, pure core, pipeline style |
| ai_hints.entities | Data models with fields, validations, relations |
| ai_hints.testing | Coverage targets, patterns, edge cases |
| ai_hints.pitfalls | Architectural mistake + consequence + prevention |
| ai_hints.code_patterns | Named patterns with examples and when to use |

### Functional Programming
| Term | Definition |
|------|------------|
| Result Type | `Result(Ok, Error)` - explicit success/failure, no exceptions |
| Pure Function | Same input always produces same output, no side effects |
| Pure Core / Imperative Shell | Business logic pure, IO only at edges |
| Pipeline Style | `input \|> step1 \|> step2 \|> step3` composition |
| Exhaustive Matching | Every case in sum type must be handled |
| Newtype Wrapper | `type UserId { UserId(String) }` - compile-time safety |
| Railway-Oriented | Chain Result-returning functions, short-circuit on error |

### Architecture
| Term | Definition |
|------|------------|
| Vertical Slice | Feature-organized code - all files for one feature together |
| Small Files | <200 lines per file - AI-friendly, easy to understand |
| Public API | Single entry point per slice - clean boundaries |
| Feature Boundary | Clear separation between features, minimal coupling |
