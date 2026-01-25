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

// === DEEP AI IMPLEMENTATION HINTS (Fowler-Style Domain Modeling) ===
#AIHints: {
    // Domain Model Structure
    domain: {
        // Bounded Context
        context_name: string       // "UserManagement", "Ordering"
        ubiquitous_language: {[string]: string}  // Term → Definition

        // Value Objects (immutable, equality by value)
        value_objects: [...{
            name: string           // "Email", "UserId", "Money"
            inner_type: string     // "String", "i64", "{currency: Currency, amount: Decimal}"
            validations: [...string]
            derives: [...string]   // ["Clone", "PartialEq", "Eq", "Hash"]
        }]

        // Entities (identity-based, mutable lifecycle)
        entities: [...{
            name: string
            id_type: string        // Value object used as ID
            fields: {[string]: string}
            invariants: [...string]  // "email must be unique", "balance >= 0"
        }]

        // Aggregates (consistency boundaries)
        aggregates: [...{
            root: string           // Entity that owns the aggregate
            members: [...string]   // Other entities/value objects
            invariants: [...string]
        }]

        // Domain Events (capture what happened)
        events: [...{
            name: string           // "UserCreated", "OrderPlaced"
            fields: {[string]: string}
            triggered_by: string   // Which command/behavior
        }]
    }

    // Module Organization (Fowler's layered architecture)
    architecture: {
        style: "domain_driven"

        layers: {
            domain: {
                path: "src/domain/"
                contains: ["entities", "value_objects", "events", "repository_traits"]
                dependencies: []   // Domain has NO dependencies
            }
            application: {
                path: "src/application/"
                contains: ["use_cases", "command_handlers", "query_handlers"]
                dependencies: ["domain"]
            }
            infrastructure: {
                path: "src/infrastructure/"
                contains: ["repository_impls", "external_services", "persistence"]
                dependencies: ["domain", "application"]
            }
        }

        // File constraints
        constraints: {
            max_file_lines: int
            max_function_lines: int
            one_aggregate_per_module: bool
        }
    }

    // Type System Usage (Make Illegal States Unrepresentable)
    types: {
        // Parse, Don't Validate
        parsing_strategy: "constructor_validation"  // Validation in ::new(), not runtime checks

        // State Machines as Types
        state_machines: [...{
            name: string           // "OrderState"
            states: [...string]    // ["Draft", "Submitted", "Paid", "Shipped"]
            transitions: [...{from: string, to: string, via: string}]
        }]

        // Error Types (one per aggregate)
        error_types: [...{
            aggregate: string
            variants: [...{name: string, fields: {[string]: string}}]
        }]
    }

    // Functional Patterns
    functional: {
        // Result everywhere, no panics
        error_handling: {
            style: "result_type"
            never_panic: bool
            error_conversion: "impl From<X> for DomainError"
        }

        // Pure core, impure shell
        purity: {
            domain_logic: "pure"           // No IO in domain
            use_cases: "orchestration"     // Coordinate pure + impure
            infrastructure: "impure"       // All IO here
        }

        // Traits for abstraction
        traits: {
            repository: "trait UserRepository { fn find(&self, id: UserId) -> Result<Option<User>, RepoError> }"
            domain_service: "Pure functions in impl blocks"
        }
    }

    // Testing Strategy (Fowler's test pyramid)
    testing: {
        unit: {
            scope: "Value objects and domain logic"
            patterns: ["property-based for value objects", "example-based for entities"]
            mocking: "None - domain is pure"
        }
        integration: {
            scope: "Use cases with real repositories"
            patterns: ["in-memory repo for fast tests", "testcontainers for slow/CI"]
        }
        edge_cases: [...{
            input: string
            expected: string
            rationale: string
        }]
    }

    // Anti-Patterns (What NOT to do)
    pitfalls: [...{
        mistake: string
        consequence: string
        prevention: string
        fowler_reference?: string  // Link to relevant Fowler article
    }]

    // Code Patterns
    code_patterns: [...{
        name: string
        description: string
        example: string
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

## Example: Fowler-Style Domain Model for User Management

```cue
ai_hints: {
    domain: {
        context_name: "Identity"
        ubiquitous_language: {
            "User": "A registered identity in the system"
            "Email": "Verified communication address, globally unique"
            "Registration": "The act of creating a new User"
            "Deactivation": "Soft-removal of a User, preserving audit trail"
        }

        value_objects: [
            {
                name: "UserId"
                inner_type: "String"
                validations: ["Must start with 'usr_'", "Must be exactly 20 chars"]
                derives: ["Clone", "PartialEq", "Eq", "Hash", "Debug"]
            },
            {
                name: "Email"
                inner_type: "String"
                validations: ["Must contain '@'", "Must have valid domain", "Max 255 chars"]
                derives: ["Clone", "PartialEq", "Eq", "Hash"]
            },
            {
                name: "UserName"
                inner_type: "String"
                validations: ["1-100 chars after trim", "No control characters"]
                derives: ["Clone", "PartialEq", "Eq"]
            }
        ]

        entities: [
            {
                name: "User"
                id_type: "UserId"
                fields: {
                    email: "Email"
                    name: "UserName"
                    status: "UserStatus"
                    created_at: "DateTime<Utc>"
                    updated_at: "DateTime<Utc>"
                }
                invariants: [
                    "email is immutable after creation",
                    "status transitions: Active -> Deactivated (one-way)"
                ]
            }
        ]

        aggregates: [
            {
                root: "User"
                members: []  // User is a simple aggregate
                invariants: ["One user per email address"]
            }
        ]

        events: [
            {
                name: "UserRegistered"
                fields: { user_id: "UserId", email: "Email", registered_at: "DateTime<Utc>" }
                triggered_by: "register_user"
            },
            {
                name: "UserDeactivated"
                fields: { user_id: "UserId", reason: "DeactivationReason", deactivated_at: "DateTime<Utc>" }
                triggered_by: "deactivate_user"
            }
        ]
    }

    architecture: {
        style: "domain_driven"

        layers: {
            domain: {
                path: "src/domain/identity/"
                contains: ["user.rs", "value_objects.rs", "events.rs", "repository.rs"]
                dependencies: []
            }
            application: {
                path: "src/application/identity/"
                contains: ["register_user.rs", "deactivate_user.rs", "find_user.rs"]
                dependencies: ["domain"]
            }
            infrastructure: {
                path: "src/infrastructure/identity/"
                contains: ["postgres_user_repository.rs"]
                dependencies: ["domain", "application"]
            }
        }

        constraints: {
            max_file_lines: 150
            max_function_lines: 25
            one_aggregate_per_module: true
        }
    }

    types: {
        parsing_strategy: "constructor_validation"

        state_machines: [
            {
                name: "UserStatus"
                states: ["Active", "Deactivated"]
                transitions: [
                    { from: "Active", to: "Deactivated", via: "deactivate()" }
                ]
            }
        ]

        error_types: [
            {
                aggregate: "User"
                variants: [
                    { name: "InvalidEmail", fields: { value: "String", reason: "String" } },
                    { name: "InvalidName", fields: { value: "String", reason: "String" } },
                    { name: "EmailAlreadyExists", fields: { email: "Email" } },
                    { name: "UserNotFound", fields: { id: "UserId" } },
                    { name: "AlreadyDeactivated", fields: { id: "UserId" } }
                ]
            }
        ]
    }

    functional: {
        error_handling: {
            style: "result_type"
            never_panic: true
            error_conversion: "impl From<RepoError> for UserError"
        }

        purity: {
            domain_logic: "pure"
            use_cases: "orchestration"
            infrastructure: "impure"
        }

        traits: {
            repository: """
                pub trait UserRepository {
                    fn next_id(&self) -> UserId;
                    fn find_by_id(&self, id: &UserId) -> Result<Option<User>, RepoError>;
                    fn find_by_email(&self, email: &Email) -> Result<Option<User>, RepoError>;
                    fn save(&self, user: &User) -> Result<(), RepoError>;
                }
            """
            domain_service: "Free functions in domain module, no traits needed"
        }
    }

    testing: {
        unit: {
            scope: "Value objects and domain logic"
            patterns: [
                "property-based: Email::new arbitrary strings",
                "example-based: User state transitions"
            ]
            mocking: "None - domain is pure"
        }
        integration: {
            scope: "Use cases with real repositories"
            patterns: [
                "InMemoryUserRepository for fast unit-like tests",
                "testcontainers postgres for CI"
            ]
        }
        edge_cases: [
            {
                input: "Email::new(\"user@例子.中国\")"
                expected: "Ok(Email) if unicode enabled, Err(InvalidEmail) otherwise"
                rationale: "RFC 6531 internationalized email"
            },
            {
                input: "Email::new(\"\")"
                expected: "Err(InvalidEmail { reason: \"empty\" })"
                rationale: "Empty string is not a valid email"
            },
            {
                input: "UserName::new(\"   \")"
                expected: "Err(InvalidName { reason: \"blank after trim\" })"
                rationale: "Whitespace-only is not a valid name"
            },
            {
                input: "user.deactivate() when already Deactivated"
                expected: "Err(AlreadyDeactivated)"
                rationale: "State machine prevents invalid transition"
            }
        ]
    }

    pitfalls: [
        {
            mistake: "Primitive obsession - using String for Email, UserId"
            consequence: "No compile-time safety, can mix up user_id with session_id"
            prevention: "Value objects with validation in constructor"
            fowler_reference: "https://martinfowler.com/bliki/ValueObject.html"
        },
        {
            mistake: "Anemic domain model - all logic in services"
            consequence: "Domain objects become data bags, logic scattered"
            prevention: "Put behavior on entities: user.deactivate() not deactivate_user(user)"
            fowler_reference: "https://martinfowler.com/bliki/AnemicDomainModel.html"
        },
        {
            mistake: "Leaking infrastructure into domain"
            consequence: "Can't test domain without database, tight coupling"
            prevention: "Repository trait in domain, impl in infrastructure"
            fowler_reference: "https://martinfowler.com/eaaCatalog/repository.html"
        },
        {
            mistake: "Using unwrap()/expect() in domain code"
            consequence: "Panics in production, unclear error handling"
            prevention: "Return Result<T, DomainError> everywhere, ? operator"
        },
        {
            mistake: "Mutable state in domain objects"
            consequence: "Race conditions, hard to reason about"
            prevention: "Return new instance: fn with_name(self, name: UserName) -> Self"
        }
    ]

    code_patterns: [
        {
            name: "Value Object with Parse-Don't-Validate"
            description: "Validation happens once at construction, type guarantees validity"
            when_to_use: "Any domain concept that should be validated"
            example: """
                #[derive(Clone, PartialEq, Eq, Hash)]
                pub struct Email(String);

                impl Email {
                    pub fn new(value: impl Into<String>) -> Result<Self, InvalidEmail> {
                        let value = value.into();
                        if value.is_empty() {
                            return Err(InvalidEmail::empty());
                        }
                        if !value.contains('@') {
                            return Err(InvalidEmail::missing_at(&value));
                        }
                        Ok(Self(value))
                    }

                    pub fn as_str(&self) -> &str {
                        &self.0
                    }
                }
            """
        },
        {
            name: "Entity with State Machine"
            description: "Encode valid state transitions in the type system"
            when_to_use: "Entities with lifecycle states"
            example: """
                pub struct User {
                    id: UserId,
                    email: Email,
                    name: UserName,
                    status: UserStatus,
                }

                impl User {
                    pub fn deactivate(self, reason: DeactivationReason) -> Result<(Self, UserDeactivated), UserError> {
                        match self.status {
                            UserStatus::Active => {
                                let event = UserDeactivated::new(self.id.clone(), reason);
                                let user = Self { status: UserStatus::Deactivated, ..self };
                                Ok((user, event))
                            }
                            UserStatus::Deactivated => Err(UserError::AlreadyDeactivated { id: self.id })
                        }
                    }
                }
            """
        },
        {
            name: "Repository Trait"
            description: "Abstract persistence, domain doesn't know about DB"
            when_to_use: "Any aggregate that needs persistence"
            example: """
                // In domain layer - no DB imports
                pub trait UserRepository: Send + Sync {
                    fn next_id(&self) -> UserId;
                    fn find_by_id(&self, id: &UserId) -> Result<Option<User>, RepoError>;
                    fn save(&self, user: &User) -> Result<(), RepoError>;
                }

                // In infrastructure layer
                pub struct PostgresUserRepository { pool: PgPool }

                impl UserRepository for PostgresUserRepository {
                    fn find_by_id(&self, id: &UserId) -> Result<Option<User>, RepoError> {
                        // SQL here
                    }
                }
            """
        },
        {
            name: "Use Case / Command Handler"
            description: "Orchestrate domain logic and infrastructure"
            when_to_use: "Application layer entry points"
            example: """
                pub struct RegisterUser<R: UserRepository> {
                    repo: R,
                }

                impl<R: UserRepository> RegisterUser<R> {
                    pub fn execute(&self, cmd: RegisterUserCommand) -> Result<UserId, UserError> {
                        // 1. Parse input into value objects (validation)
                        let email = Email::new(&cmd.email)?;
                        let name = UserName::new(&cmd.name)?;

                        // 2. Check business rules
                        if self.repo.find_by_email(&email)?.is_some() {
                            return Err(UserError::EmailAlreadyExists { email });
                        }

                        // 3. Create aggregate
                        let id = self.repo.next_id();
                        let user = User::new(id.clone(), email, name);

                        // 4. Persist
                        self.repo.save(&user)?;

                        Ok(id)
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
14. **Beads contain full context** - each bead is a "Block of Context" so precise a primitive AI can fix it in one shot without hallucinations
15. **Mechanical implementation** - AI can implement from bead without asking clarifying questions

---

## Bead Template (MANDATORY for Every Bead)

Every bead MUST follow this template. This is Jira in CLI form - complete work breakdown with dependencies, priorities, and full implementation context.

### Bead Schema

```cue
#Bead: {
    // === METADATA (Jira-style) ===
    id: string                      // "BEAD-001", auto-generated
    epic_id?: string                // Parent epic for grouping
    title: string                   // Short, action-oriented
    status: "open" | "in_progress" | "blocked" | "closed"
    priority: 1 | 2 | 3 | 4 | 5     // 5 = critical, 1 = nice-to-have
    issue_type: "feature" | "bug" | "task" | "spike"
    estimated_minutes: 5..30        // Atomic work unit
    labels: [...string]

    // === DEPENDENCIES (Critical for ordering) ===
    depends_on: [...string]         // Bead IDs that must complete first
    blocks: [...string]             // Bead IDs waiting on this one
    related_to: [...string]         // Non-blocking relationships

    // === CONTEXT BLOCK ===
    context: {
        file_path: string           // "src/domain/user/value_objects.rs"
        function_or_type?: string   // "Email::new" or "struct User"
        smell: string               // "The code assumes X but Y happens..."
        why_now: string             // Why this bead matters in the sequence
    }

    // === SPECIFICATION BLOCK (The "One-Shot" Instructions) ===
    spec: #BeadSpec

    // === AI REVIEW ===
    ai_review: {
        completeness_check: string  // Self-review: "Does this cover everything?"
        context_references: [...{   // Where to look in codebase
            file: string
            lines?: string          // "45-67"
            why: string
        }]
        ambiguity_flags: [...string] // Any remaining questions?
    }
}

#BeadSpec: {
    // 1. EARS (Easy Approach to Requirements Syntax)
    ears: [...{
        pattern: "ubiquitous" | "event_driven" | "state_driven" | "optional" | "unwanted"
        trigger?: string            // WHEN clause
        state?: string              // WHILE clause
        condition?: string          // WHERE/IF clause
        behavior: string            // SHALL/SHALL NOT clause
    }]

    // 2. Design by Contract
    contract: {
        preconditions: [...string]  // What must be true BEFORE
        postconditions: [...string] // What must be true AFTER
        invariants: [...string]     // What must ALWAYS be true
    }

    // 3. Test-Driven Design (Kent Beck style)
    tests: {
        happy_path: [...{
            name: string
            given: string
            when: string
            then: string
            code_sketch: string     // Actual test code
        }]
        unhappy_path: [...{
            name: string
            given: string
            when: string
            then: string
            code_sketch: string
        }]
        property_based?: [...{
            name: string
            property: string        // "forall x: valid_email(x) => Email::new(x).is_ok()"
            generator: string       // How to generate test data
        }]
    }

    // 4. Design by Type
    types: {
        interfaces: [...{
            name: string
            definition: string      // Actual code
        }]
        value_objects: [...{
            name: string
            inner_type: string
            validations: [...string]
            derives: [...string]
        }]
        error_types: [...{
            name: string
            variants: [...string]
        }]
    }

    // 5. Schema & Edge Cases
    schema: {
        input_type: string          // JSON Schema or Rust type
        output_type: string
        edge_cases: [...{
            input: string
            expected_output: string
            rationale: string
        }]
    }

    // 6. Invariants & Variants
    boundaries: {
        will_do: [...{
            behavior: string
            code_example: string
        }]
        will_not_do: [...{
            behavior: string
            reason: string
        }]
    }
}
```

### Example Bead: Implement Email Value Object

```cue
{
    id: "BEAD-003"
    epic_id: "EPIC-001-USER-DOMAIN"
    title: "Implement Email value object with Parse-Don't-Validate"
    status: "open"
    priority: 4
    issue_type: "feature"
    estimated_minutes: 20
    labels: ["domain", "value-object", "validation"]

    depends_on: []
    blocks: ["BEAD-004", "BEAD-005"]  // User entity and RegisterUser use case
    related_to: ["BEAD-002"]          // UserId value object

    context: {
        file_path: "src/domain/identity/value_objects.rs"
        function_or_type: "struct Email"
        smell: "Raw String used for email throughout codebase - no validation, can mix up with other strings, no compile-time safety"
        why_now: "Foundation for User aggregate - all user operations need validated Email"
    }

    spec: {
        ears: [
            {
                pattern: "ubiquitous"
                behavior: "The system SHALL reject email addresses without @ symbol"
            },
            {
                pattern: "ubiquitous"
                behavior: "The system SHALL reject email addresses longer than 255 characters"
            },
            {
                pattern: "event_driven"
                trigger: "Email::new() is called with valid input"
                behavior: "The system SHALL return Ok(Email) with normalized lowercase"
            },
            {
                pattern: "unwanted"
                condition: "input is empty string"
                behavior: "The system SHALL NOT create Email, SHALL return Err(EmptyEmail)"
            }
        ]

        contract: {
            preconditions: [
                "Input is a String or &str",
                "No preconditions on content (validation is the job)"
            ]
            postconditions: [
                "If Ok: email contains exactly one @",
                "If Ok: email is lowercase (normalized)",
                "If Ok: email.len() <= 255",
                "If Err: no Email instance created",
                "Email is immutable after creation"
            ]
            invariants: [
                "Email always contains valid format (enforced by private field)",
                "Email can only be created through ::new() constructor",
                "Two emails with same string are equal (PartialEq)"
            ]
        }

        tests: {
            happy_path: [
                {
                    name: "creates_email_from_valid_input"
                    given: "A valid email string 'user@example.com'"
                    when: "Email::new is called"
                    then: "Returns Ok(Email) with normalized value"
                    code_sketch: """
                        #[test]
                        fn creates_email_from_valid_input() {
                            let result = Email::new("User@Example.COM");
                            assert!(result.is_ok());
                            assert_eq!(result.unwrap().as_str(), "user@example.com");
                        }
                    """
                },
                {
                    name: "emails_with_same_value_are_equal"
                    given: "Two Email instances with same normalized value"
                    when: "Compared with =="
                    then: "Returns true"
                    code_sketch: """
                        #[test]
                        fn emails_with_same_value_are_equal() {
                            let e1 = Email::new("test@example.com").unwrap();
                            let e2 = Email::new("TEST@EXAMPLE.COM").unwrap();
                            assert_eq!(e1, e2);
                        }
                    """
                }
            ]
            unhappy_path: [
                {
                    name: "rejects_empty_string"
                    given: "Empty string input"
                    when: "Email::new is called"
                    then: "Returns Err(InvalidEmail::Empty)"
                    code_sketch: """
                        #[test]
                        fn rejects_empty_string() {
                            let result = Email::new("");
                            assert!(matches!(result, Err(InvalidEmail::Empty)));
                        }
                    """
                },
                {
                    name: "rejects_missing_at_symbol"
                    given: "String without @ symbol"
                    when: "Email::new is called"
                    then: "Returns Err(InvalidEmail::MissingAt)"
                    code_sketch: """
                        #[test]
                        fn rejects_missing_at_symbol() {
                            let result = Email::new("userexample.com");
                            assert!(matches!(result, Err(InvalidEmail::MissingAt { .. })));
                        }
                    """
                },
                {
                    name: "rejects_too_long"
                    given: "String longer than 255 chars"
                    when: "Email::new is called"
                    then: "Returns Err(InvalidEmail::TooLong)"
                    code_sketch: """
                        #[test]
                        fn rejects_too_long() {
                            let long_email = format!("{}@example.com", "a".repeat(250));
                            let result = Email::new(&long_email);
                            assert!(matches!(result, Err(InvalidEmail::TooLong { .. })));
                        }
                    """
                }
            ]
            property_based: [
                {
                    name: "valid_emails_always_contain_at"
                    property: "forall e: Email::new(e).is_ok() => e.contains('@')"
                    generator: "Arbitrary strings, filtered"
                },
                {
                    name: "created_emails_are_normalized"
                    property: "forall e: Email::new(e).is_ok() => Email::new(e).unwrap().as_str() == e.to_lowercase()"
                    generator: "Valid email strings with mixed case"
                }
            ]
        }

        types: {
            interfaces: []
            value_objects: [
                {
                    name: "Email"
                    inner_type: "String"
                    validations: ["contains @", "len <= 255", "not empty"]
                    derives: ["Clone", "PartialEq", "Eq", "Hash", "Debug"]
                }
            ]
            error_types: [
                {
                    name: "InvalidEmail"
                    variants: [
                        "Empty",
                        "MissingAt { value: String }",
                        "TooLong { len: usize, max: usize }",
                        "InvalidFormat { value: String, reason: String }"
                    ]
                }
            ]
        }

        schema: {
            input_type: "impl Into<String>"
            output_type: "Result<Email, InvalidEmail>"
            edge_cases: [
                {
                    input: "\"\""
                    expected_output: "Err(InvalidEmail::Empty)"
                    rationale: "Empty string is not a valid email"
                },
                {
                    input: "\"   \""
                    expected_output: "Err(InvalidEmail::Empty)"
                    rationale: "Whitespace-only should be treated as empty"
                },
                {
                    input: "\"user@例子.中国\""
                    expected_output: "Ok(Email) if unicode support enabled"
                    rationale: "RFC 6531 internationalized email addresses"
                },
                {
                    input: "\"user@localhost\""
                    expected_output: "Ok(Email)"
                    rationale: "localhost is valid for development"
                },
                {
                    input: "\"user@@example.com\""
                    expected_output: "Err(InvalidEmail::InvalidFormat)"
                    rationale: "Multiple @ symbols invalid"
                }
            ]
        }

        boundaries: {
            will_do: [
                {
                    behavior: "Validate @ presence"
                    code_example: "if !value.contains('@') { return Err(InvalidEmail::MissingAt { value }) }"
                },
                {
                    behavior: "Normalize to lowercase"
                    code_example: "let normalized = value.trim().to_lowercase();"
                },
                {
                    behavior: "Enforce max length"
                    code_example: "if normalized.len() > 255 { return Err(InvalidEmail::TooLong { len: normalized.len(), max: 255 }) }"
                }
            ]
            will_not_do: [
                {
                    behavior: "DNS lookup to verify domain exists"
                    reason: "IO operation - belongs in infrastructure layer"
                },
                {
                    behavior: "Send verification email"
                    reason: "Side effect - belongs in use case layer"
                },
                {
                    behavior: "Full RFC 5322 regex validation"
                    reason: "Overly strict, rejects valid emails users expect to work"
                },
                {
                    behavior: "Store in database"
                    reason: "Persistence is repository concern"
                }
            ]
        }
    }

    ai_review: {
        completeness_check: "This bead covers: type definition, error type, constructor validation, equality, Display impl. Missing: Serialize/Deserialize derives if needed for API layer - add if User API requires JSON."
        context_references: [
            {
                file: "src/domain/identity/mod.rs"
                why: "Module exports - add pub use value_objects::Email"
            },
            {
                file: "src/domain/identity/user.rs"
                lines: "15-20"
                why: "User entity will use Email type for email field"
            },
            {
                file: "BEAD-002"
                why: "Similar pattern to UserId - follow same structure"
            }
        ]
        ambiguity_flags: [
            "Unicode support: decide yes/no before implementation",
            "Serde derives: check if needed for this layer"
        ]
    }
}
```

### Bead Workflow (Jira-style)

```
┌─────────────────────────────────────────────────────────────────────────┐
│  EPIC: EPIC-001-USER-DOMAIN                                             │
│  "Implement User aggregate with value objects and repository"            │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  Wave 1 (Foundation - no dependencies):                                  │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐                              │
│  │ BEAD-001 │  │ BEAD-002 │  │ BEAD-003 │                              │
│  │ Error    │  │ UserId   │  │ Email    │                              │
│  │ Types    │  │ Value Obj│  │ Value Obj│                              │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘                              │
│       │             │             │                                      │
│       └─────────────┼─────────────┘                                      │
│                     ▼                                                    │
│  Wave 2 (Depends on Wave 1):                                            │
│  ┌──────────────────────────────┐                                       │
│  │         BEAD-004             │                                       │
│  │     User Entity              │                                       │
│  │  depends_on: [001,002,003]   │                                       │
│  └──────────────┬───────────────┘                                       │
│                 │                                                        │
│                 ▼                                                        │
│  Wave 3 (Depends on Wave 2):                                            │
│  ┌──────────────────────────────┐  ┌──────────────────────────────┐    │
│  │         BEAD-005             │  │         BEAD-006             │    │
│  │   UserRepository Trait       │  │    Domain Events             │    │
│  │  depends_on: [004]           │  │  depends_on: [004]           │    │
│  └──────────────┬───────────────┘  └──────────────┬───────────────┘    │
│                 │                                  │                     │
│                 └──────────────┬──────────────────┘                     │
│                                ▼                                        │
│  Wave 4 (Depends on Wave 3):                                            │
│  ┌──────────────────────────────┐                                       │
│  │         BEAD-007             │                                       │
│  │   RegisterUser Use Case      │                                       │
│  │  depends_on: [004,005,006]   │                                       │
│  └──────────────────────────────┘                                       │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Bead CLI Commands

```bash
# View ready beads (no blockers)
bd ready --json

# Claim a bead (move to in_progress)
bd update BEAD-003 --status in_progress --json

# Close a bead with reason
bd close BEAD-003 --reason "All tests passing, code reviewed" --json

# View dependencies
bv --robot-graph --graph-format=json

# Get AI triage (which bead to work on next)
bv --robot-next

# View parallel tracks (beads that can be worked simultaneously)
bv --robot-plan

# Get critical path
bv --robot-insights
```

### 1. Deterministic Planning (Mental Lattices)
- **EARS** eliminates natural language ambiguity
- **KIRK contracts** define machine-checkable success/failure
- **Interactive Questioning** resolves every edge case BEFORE implementation
- **Inversion** + **Pre-mortem** + **Second-order** thinking catches failures early
- **Beads** are self-contained - all context embedded

### 2. Fowler-Style Domain Modeling
- **Ubiquitous Language** - code reads like the domain
- **Value Objects** - Email, UserId, Money with validated constructors
- **Entities** - identity-based with clear lifecycle
- **Aggregates** - consistency boundaries, one per module
- **Domain Events** - capture what happened for audit/replay

### 3. Functional Programming Rigor
- **Parse, Don't Validate** - validation in constructor, type guarantees validity
- **Result types** - no exceptions, no panics
- **Pure domain** - business logic has zero IO
- **Make Illegal States Unrepresentable** - type system prevents bugs

### 4. Clean Architecture (Dependency Inversion)
- **Domain layer** - entities, value objects, repository traits (NO dependencies)
- **Application layer** - use cases orchestrate domain + infrastructure
- **Infrastructure layer** - implements repository traits, owns all IO
- **Small files** (<150 lines) - AI-friendly, focused modules

### 5. Test-First Quality
- **Property-based tests** for value objects
- **Edge cases enumerated** in spec with input/expected/rationale
- **In-memory repositories** for fast unit-like integration tests
- **Anti-patterns documented** with Fowler references

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

### Domain-Driven Design (Fowler)
| Term | Definition |
|------|------------|
| Bounded Context | Linguistic boundary where terms have precise meaning |
| Ubiquitous Language | Shared vocabulary between code and domain experts |
| Value Object | Immutable, equality by value (Email, UserId, Money) |
| Entity | Identity-based object with lifecycle (User, Order) |
| Aggregate | Consistency boundary, accessed through root entity |
| Domain Event | Record of something that happened (UserRegistered) |
| Repository | Collection-like interface for aggregate persistence |
| Anemic Domain Model | Anti-pattern: logic in services, entities are data bags |

### Functional Programming
| Term | Definition |
|------|------------|
| Result Type | `Result<T, E>` - explicit success/failure, no exceptions |
| Parse Don't Validate | Validation in constructor, type guarantees validity |
| Pure Core / Impure Shell | Domain logic pure, IO in infrastructure |
| Make Illegal States Unrepresentable | Type system prevents invalid data |
| Exhaustive Matching | Every enum variant must be handled |
| Newtype Wrapper | `struct Email(String)` - compile-time safety |

### Architecture Layers
| Term | Definition |
|------|------------|
| Domain Layer | Entities, value objects, events, repository traits - NO dependencies |
| Application Layer | Use cases, command handlers - depends on domain only |
| Infrastructure Layer | Repository impls, DB, external services - depends on all |
| Dependency Inversion | Domain defines traits, infrastructure implements them |

### Code Quality
| Term | Definition |
|------|------------|
| Small Files | <150 lines per file - AI-friendly, focused |
| One Aggregate Per Module | Clear boundaries, easy to find code |
| Never Panic | Return Result, no unwrap()/expect() in domain |
| Fowler Reference | Link to martinfowler.com article for deeper reading |
