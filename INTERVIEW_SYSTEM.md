# Intent Interview System

## Overview

The Intent Interview System is a **structured interrogation engine** that discovers and refines specifications through systematic questioning. It directly competes with Kiro, OpenSpec, and SpecKit by being fundamentally better at extracting real requirements from humans.

### The Problem

Current tools assume you know what you want:
- Kiro: "Write your spec, we'll help you implement"
- OpenSpec: "Format your requirements, we'll validate"
- SpecKit: "Fill out this questionnaire"

### The Intent Solution

```
Vague Idea → 5-Round Interview → Complete Spec → Bead Templates → Implementation
```

## System Architecture

### 1. Interview Engine (`interview.gleam`)

Core types and logic:

```gleam
pub type Profile { Api | Cli | Event | Data | Workflow | UI }
pub type InterviewStage { Discovery | Refinement | Validation | Complete | Paused }
pub type Perspective { User | Developer | Ops | Security | Business }

// Core functions:
pub fn extract_from_answer(question_id, response, fields) -> Dict(String, String)
pub fn detect_gaps(profile, answers) -> List(Gap)
pub fn detect_conflicts(answers) -> List(Conflict)
pub fn calculate_confidence(question_id, response, extracted) -> Float
```

### 2. Questions Library (`interview_questions.gleam`)

25+ hardcoded questions across 5 rounds and 6 profiles:

**Round 1: Core Intent** (5 user-perspective questions)
- What should this do?
- Who will use it?
- Walk me through the happy path
- What's most critical?
- What would make this a failure?

**Round 2: Error Cases** (5 developer-perspective questions)
- What's the most common error?
- What info should NEVER leak?
- What are the HTTP status codes?
- How should errors be displayed?
- What's the fallback strategy?

**Round 3: Edge Cases** (3 QA-perspective questions)
- What's the min/max input?
- What happens with zero/one/many items?
- Special characters, unicode, emojis?

**Round 4: Security & Compliance** (2 security-perspective questions)
- What data is sensitive?
- What are the compliance requirements?

**Round 5: Operations** (3 ops-perspective questions)
- Where does it run?
- What's your uptime requirement?
- How will you monitor this?

### 3. Session Controller (`interview_session.gleam`)

Orchestrates the multi-round interrogation:

```gleam
pub fn start_interview(profile) -> InterviewSession
pub fn get_first_question_for_round(session, round) -> Result(CurrentQuestion, String)
pub fn get_next_question(session, current, answer) -> Result(NextAction, String)
pub fn resolve_conflict(conflict, chosen_index) -> Result(String, String)

pub type NextAction {
  NextQuestion(question)
  BlockingGap(gap, clarifying_question)
  ConflictFound(conflict)
  RoundComplete(round, summary)
  InterviewComplete(extracted_spec)
}
```

Key behaviors:
- **Gap Detection**: Identifies missing critical fields and blocks progress
- **Conflict Resolution**: Detects contradictions (CAP theorem, privacy paradoxes) and offers options
- **Progressive Refinement**: Each round adds more specificity
- **Confidence Scoring**: Tracks how certain we are about extracted fields

### 4. Spec Builder (`spec_builder.gleam`)

Converts answers into typed CUE specifications:

```gleam
pub fn build_spec_from_session(session) -> String
// Outputs valid CUE with:
// - name, description, audience
// - features, behaviors, constraints
// - security requirements, non-functional requirements
// - AI hints for implementation
```

Process:
1. Extract key fields from answers
2. Group behaviors into features
3. Consolidate constraints and requirements
4. Generate CUE spec file

### 5. Bead Templates (`bead_templates.gleam`)

Generates work items from complete specs:

```gleam
pub type Bead {
  Bead(
    id: String,
    title: String,
    description: String,
    acceptance_criteria: List(String),
    labels: List(String),
    blocked_by: List(String),
    ai_hints: String,
    priority: String,
  )
}

pub fn generate_beads_from_spec(profile, spec_name, behaviors, constraints, security) -> List(Bead)
pub fn export_beads_to_jsonl(beads) -> String
```

Profile-specific templates:
- **API**: Behavior → endpoint implementation bead
- **CLI**: Command → CLI implementation bead
- **Event**: Event type → emitter bead
- **Data**: Entity → schema bead
- **Workflow**: Transition → state machine bead
- **UI**: Screen → component bead

## The 5-Round Interview Flow

### Round 1: Core Intent (Discovery)

```
Q: In one sentence, what should this do?
A: Users can register and log in

Extracts:
  ✓ Feature: Authentication
  ✓ Audience: Users
  ✓ Happy path: register → login
```

### Round 2: Error Cases (Validation)

```
Q: What's the most common error users will hit?
A: Wrong password

Extracts:
  ✓ Error case: invalid_password
  ✓ Response format: {"error": "..."}

Detects gap:
  ⚠️  What should the error message say?
```

### Round 3: Edge Cases (Specification)

```
Q: What if someone tries 100 passwords per second?
A: We should rate limit them

Extracts:
  ✓ Constraint: max 5 attempts per minute

Detects conflict:
  ⚠️  You said "no latency impact" but also "rate limit aggressively"?
      (Resolution options offered)
```

### Round 4: Security & Compliance

```
Q: What data is sensitive?
A: Passwords, credit cards

Extracts:
  ✓ Sensitive fields: [password, card_number]
  ✓ Encryption requirement: encrypt at rest + in transit
```

### Round 5: Operations

```
Q: What's your uptime requirement?
A: 99.9% for paying customers

Extracts:
  ✓ SLA: 99.9%
  ✓ Acceptable downtime: 8.76 hours/year
  ✓ Failover strategy: required
```

## Gap Detection Strategy

### Blocking Gaps

Prevent spec generation until answered:
- `base_url` (for APIs)
- `auth_method` (for anything secure)
- `happy_path` (core intent)
- `error_cases` (resilience)

When detected:
1. User is notified
2. Clarifying question asked
3. Progress blocked until answered

### Non-Blocking Gaps

Recommended but don't block:
- Rate limiting policy
- Retry strategy
- Monitoring approach

## Conflict Detection

### CAP Theorem

```
"I want fast response times" + "I want strong consistency"
↓
Impossible at scale (CAP theorem)
↓
Offer options:
  1. Accept eventual consistency, optimize latency
  2. Accept latency, optimize consistency
```

### Privacy Paradox

```
"I want anonymous users" + "I want full audit trail"
↓
Can't audit anonymous users without tracking
↓
Offer options:
  1. Use pseudonymous IDs (session-scoped tracking)
  2. Aggregate logging only (no per-user audit)
  3. Accept need for user consent + opt-in tracking
```

### Scope Creep

```
"Keep it simple" + 20 requirements listed
↓
Scope creep detected
↓
Ask: "Which 3-5 are MVP? Which are nice-to-have?"
```

## From Interview to Implementation

### Phase 1: Interview (Complete)
- ✅ 5-round structured interrogation
- ✅ Gap detection and blocking
- ✅ Conflict detection and resolution
- ✅ Progressive spec building
- ✅ 25+ tests passing

### Phase 2: Spec Generation (Complete)
- ✅ CUE spec generation from answers
- ✅ Schema validation
- ✅ AI hints embedded

### Phase 3: Bead Generation (Complete)
- ✅ Profile-specific templates
- ✅ Acceptance criteria generation
- ✅ Dependency tracking
- ✅ JSONL export for `bd` tool

### Phase 4: Persistence (Next)
- [ ] JSONL session storage (git-tracked)
- [ ] SQLite cache for queries
- [ ] Session resume capability

### Phase 5: Interactive TUI (Next)
- [ ] Real Q&A loop
- [ ] Progress indicator
- [ ] Gap/conflict UI
- [ ] Pause/resume/export commands

### Phase 6: Integration (Future)
- [ ] `bd` command integration
- [ ] Automatic bead creation
- [ ] Verification workflow
- [ ] AI implementation hints

## Usage

```bash
# Start new interview for API
intent interview --profile api

# Resume session
intent interview --resume session_abc123

# Non-interactive mode (testing)
intent interview --profile api --answers answers.yaml

# Export spec from completed session
intent interview --export session_abc123 > spec.cue

# Generate beads from spec
intent beads spec.cue --template @api-standard

# Verify implementation against spec
intent check spec.cue --target http://localhost:8080
```

## Example Session Transcript

```
╔══════════════════════════════════════════════════════════════════╗
║                    INTENT INTERVIEW                              ║
║              Round 1 of 5: Core Intent (USER PERSPECTIVE)        ║
╚══════════════════════════════════════════════════════════════════╝

─────────────────────────────────────────────────────────────────────
Question 1/5 [CRITICAL]
─────────────────────────────────────────────────────────────────────

In one sentence, what should this API do?

Why: We're starting with the core intent. Be specific, not vague.

Example answer: "Allow users to manage their shopping cart"

> Users can register an account and log in with email/password

Extracted:
  ✓ Feature: User Registration
  ✓ Feature: Authentication
  ✓ Audience: Users
  ✓ Success criteria: "register and log in"

Confidence: 92%

─────────────────────────────────────────────────────────────────────
Question 2/5 [CRITICAL]
─────────────────────────────────────────────────────────────────────

Who will use this? What are they trying to accomplish?

> Mobile app users who need quick onboarding

Extracted:
  ✓ Audience: Mobile users
  ✓ Goal: Quick onboarding

Confidence: 85%

─────────────────────────────────────────────────────────────────────
Question 3/5 [CRITICAL]
─────────────────────────────────────────────────────────────────────

Walk me through registration step by step.

> User enters email and password, we validate them, create account,
> return their profile (without password)

Extracted:
  ✓ Behavior: successful-registration
    - Input: email, password
    - Output: profile (no sensitive fields)
    - Status: 201 Created

  ⚠️  Gap detected: Password strength requirements?

> At least 8 chars, one uppercase, one digit, one special

Extracted:
  ✓ Constraint: password complexity rules

Confidence: 88%

─────────────────────────────────────────────────────────────────────
Question 4/5 [CRITICAL]
─────────────────────────────────────────────────────────────────────

What's the most critical thing this MUST do correctly?

> NEVER expose passwords in responses. Ever.

Extracted:
  ✓ Anti-pattern: password-in-response
  ✓ Global rule: body_must_not_contain["password"]

Confidence: 98%

─────────────────────────────────────────────────────────────────────
Question 5/5 [CRITICAL]
─────────────────────────────────────────────────────────────────────

What would make this a failure even if it technically works?

> If login errors reveal whether an email exists. That's a security hole.

Extracted:
  ✓ Anti-pattern: user-enumeration
  ✓ Error response rule: Return same error for invalid email/password

═══════════════════════════════════════════════════════════════════

Round 1 Complete ✓

Extracted:
  • 2 features (Registration, Authentication)
  • 1 behavior (Register)
  • 3 constraints (Password complexity, No password exposure, No enumeration)
  • 1 audience (Mobile users)

Gaps remaining (3 critical):
  ⚠️  What happens on invalid email format?
  ⚠️  What's the rate limit on registration?
  ⚠️  What's the email verification flow?

Proceeding to Round 2: Error Cases (DEVELOPER PERSPECTIVE)
```

## Key Differences from Competitors

| Feature | Kiro | OpenSpec | SpecKit | Intent |
|---------|------|----------|---------|--------|
| Spec format | Markdown | YAML | JSON | **CUE (typed)** |
| Interview process | Basic | None | Wizard | **5-round multi-perspective** |
| Gap detection | ❌ | ❌ | Partial | **✅ With blocking** |
| Conflict detection | ❌ | ❌ | ❌ | **✅ With resolution** |
| Anti-patterns | ❌ | ❌ | ❌ | **✅ Explicit** |
| Bead/Issue generation | ❌ | ❌ | ❌ | **✅ Templated** |
| Multi-profile support | ❌ | ❌ | ❌ | **✅ 6 profiles** |
| Verification built-in | ❌ | External | External | **✅ `intent check`** |

## The Moat

**Intent's competitive advantage is the interview engine itself.**

Everyone else assumes you already know what you want. Intent helps you discover it through structured interrogation by multiple perspectives (user, developer, ops, security, business).

This creates a **funnel advantage**:
1. Users come for the better interview experience
2. They get better specs as output
3. Which leads to better beads
4. Which leads to better implementations
5. Which leads to faster shipping

The interview engine is the moat. Everything else flows from that.

## Next Steps

1. ✅ Core interview logic (complete)
2. ✅ Question library (complete)
3. ✅ Spec generation (complete)
4. ✅ Bead templates (complete)
5. ⏳ Interactive TUI (in progress)
6. ⏳ Session persistence (next)
7. 🔮 Full integration (future)
