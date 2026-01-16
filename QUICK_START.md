# Intent CLI - Quick Start Guide

**Transform vague ideas into deterministic beads in 6 commands.**

---

## Installation

```bash
npx intent-cli
```

Verify:
```
/intent:help
```

---

## The 6-Step Workflow

### Step 1: Initialize Project

```
/intent:init my-awesome-project
```

**What happens:**
- Creates `.intent/` directory
- Initializes PROJECT.md template
- Sets up session state

**Output:**
```
✓ Initialized .intent/
✓ Created PROJECT.md template

Next: /intent:interview
```

---

### Step 2: EARS Interview

```
/intent:interview
```

**What happens:**
- Agent asks systematic questions using 6 EARS patterns
- You answer each question
- Agent formats answers into EARS requirements

**Sample questions:**

**Round 1 - Ubiquitous (always true):**
```
Q: What must ALWAYS be true, regardless of context?

Your answer: "The system must validate all inputs"

Agent formats: "THE SYSTEM SHALL validate all inputs against schema"
```

**Round 2 - Event-Driven (when X, do Y):**
```
Q: What user actions trigger system responses?

Your answer: "When user submits registration, validate email format"

Agent formats: "WHEN user submits registration THE SYSTEM SHALL validate email format"
```

**Round 3 - State-Driven (while X, do Y):**
```
Q: While in a certain state, what behavior should occur?

Your answer: "While user is authenticated, allow access to dashboard"

Agent formats: "WHILE user is authenticated THE SYSTEM SHALL allow access to dashboard"
```

... continues through all 6 patterns

**Output:**
```
✓ Interview complete
✓ Created .intent/REQUIREMENTS.md (24 requirements)

EARS breakdown:
- Ubiquitous: 6 requirements
- Event-Driven: 10 requirements
- State-Driven: 4 requirements
- Optional: 2 requirements
- Unwanted: 2 requirements
- Complex: 0 requirements

Next: /intent:analyze
```

---

### Step 3: Mental Lattice Analysis

```
/intent:analyze
```

**What happens:**
- Agent applies 5 thinking models to your requirements
- Discovers edge cases, failure modes, consequences

**Lattice 1 - Inversion (what could fail?):**
```
Analyzing: "WHEN user submits login THE SYSTEM SHALL authenticate credentials"

Inversions found:
✗ auth-bypass: User accesses without authentication
✗ expired-token: User uses expired JWT
✗ sql-injection: Malicious input in email field
✗ rate-limit-bypass: Brute force attack

Generated requirements:
- "IF authentication fails THE SYSTEM SHALL NOT grant access"
- "IF token is expired THE SYSTEM SHALL NOT accept token"
- "IF input contains SQL patterns THE SYSTEM SHALL NOT execute query"
- "WHEN login attempts exceed 5 in 1 minute THE SYSTEM SHALL NOT accept further attempts"
```

**Lattice 2 - Second-Order (what are consequences?):**
```
Analyzing: "WHEN user is deleted THE SYSTEM SHALL remove user record"

Second-order effects:
→ User's items become orphaned
→ Active sessions must be invalidated
→ Audit logs reference missing user
→ Shared resources need ownership transfer

Generated requirements:
- "WHEN user is deleted THE SYSTEM SHALL invalidate all user sessions"
- "WHEN user is deleted THE SYSTEM SHALL transfer ownership of shared resources"
```

... continues through all 5 lattices

**Output:**
```
✓ Analysis complete
✓ Created .intent/ANALYSIS.md

Discovered:
- 18 edge cases
- 12 failure modes
- 8 second-order consequences
- 3 pre-mortem scenarios

Added 41 new requirements to REQUIREMENTS.md

Next: /intent:contract
```

---

### Step 4: KIRK Contract Generation

```
/intent:contract
```

**What happens:**
- Agent transforms EARS requirements into formal contracts
- Each requirement becomes a behavior with preconditions, postconditions, invariants

**Example transformation:**

**EARS requirement:**
```
WHEN user submits registration THE SYSTEM SHALL validate email format
```

**KIRK contract:**
```cue
behavior: "validate-registration-email"
intent: "Ensure all registered users have valid email addresses"

kirk: {
  preconditions: {
    fields: {
      required: ["email"]

      email: {
        type: "string"
        constraints: ["non-empty", "valid email format"]
      }
    }
  }

  postconditions: {
    response: {
      status: 200  // or 400 if invalid

      guarantees: {
        if_valid: "Email accepted, user created"
        if_invalid: "400 with clear error message"
      }
    }
  }

  invariants: [
    "Emails are unique (case-insensitive)",
    "All stored emails are valid format"
  ]
}

ears_pattern: "event_driven"
ears_formatted: "WHEN user submits registration THE SYSTEM SHALL validate email format"
```

**Output:**
```
✓ Contract generation complete
✓ Created .intent/CONTRACTS.cue
✓ CUE validation passed

Contracts created: 65
- From original requirements: 24
- From analysis discoveries: 41

Next: /intent:plan-phase 1
```

---

### Step 5: Phase Planning

```
/intent:plan-phase 1
```

**What happens:**
- Agent groups contracts by feature/domain
- Identifies dependencies
- Calculates parallel execution waves
- Creates GSD-style PLAN.md files

**Example phase breakdown:**

**Phase 1: Foundation**
```
Contracts in this phase:
- validate-input-schema (ubiquitous)
- log-all-requests (ubiquitous)
- use-https (ubiquitous)
- reject-large-payloads (ubiquitous)

Wave: 1 (all parallel - no dependencies)
```

**Phase 2: Authentication**
```
Contracts in this phase:
- validate-registration-email (event-driven)
- hash-password (event-driven)
- create-user-record (event-driven)
- return-jwt-token (event-driven)
- invalidate-expired-tokens (unwanted)

Wave 1: validate-registration-email, hash-password (parallel)
Wave 2: create-user-record (depends on Wave 1)
Wave 3: return-jwt-token (depends on Wave 2)
```

**Output:**
```
✓ Phase 1 planning complete
✓ Created .intent/phases/01-foundation/PLAN.md

Phase breakdown:
- Total contracts: 12
- Wave 1 (parallel): 8 contracts
- Wave 2 (sequential): 3 contracts
- Wave 3 (sequential): 1 contract

Parallelization: 67%

Next: /intent:plan-phase 2
(or /intent:plan-all to plan all phases at once)
```

---

### Step 6: Generate Beads

```
/intent:generate-beads
```

**What happens:**
- Agent creates atomic beads from contracts
- Each bead includes: what, why, done_when, preconditions, postconditions, invariants, edge_cases, test_cases
- Beads created in bd database

**Example bead generation:**

**From contract:**
```cue
behavior: "validate-registration-email"
```

**Generated bead:**
```yaml
id: AUTH-001
title: "Validate email format on registration"
type: feature
priority: 1

what: "Implement email validation that checks format against RFC 5322 standard"
why: "Ensure all registered users have valid email addresses to prevent fake accounts"

done_when:
  - "Valid emails pass validation"
  - "Invalid emails return 400 with clear error"
  - "All test cases pass"
  - "Edge cases handled"

preconditions:
  - "Request includes email field"
  - "Email is non-empty string"

postconditions:
  - "Valid email accepted (continues to user creation)"
  - "Invalid email rejected with 400"
  - "Error message explains format requirement"

invariants:
  - "Emails are unique (case-insensitive)"
  - "All stored emails are valid format"

edge_cases:
  - "Email with unicode characters"
  - "Email with multiple @ symbols"
  - "Email with spaces"
  - "Very long email (>320 chars)"
  - "Email with SQL injection payload"
  - "Empty string"
  - "Null value"

failure_modes:
  - "Regex validation library failure"
  - "Database uniqueness check timeout"

test_cases:
  - given: "Valid email: user@example.com"
    when: "POST /register with valid email"
    then: "Returns 200, user created"
    edge_case: false

  - given: "Invalid email: not-an-email"
    when: "POST /register with invalid email"
    then: "Returns 400 with error message"
    edge_case: true

  - given: "SQL injection: admin'--@example.com"
    when: "POST /register with malicious email"
    then: "Returns 400 (pattern rejected)"
    edge_case: true

dependencies: []
wave: 1
estimate_minutes: 45

context_files:
  - "schema/users.sql"
  - "src/types/user.ts"

output_files:
  - "src/validators/email.ts"
  - "test/validators/email.test.ts"
```

**Bead created in bd:**
```bash
bd create "Validate email format on registration" \
  -t feature -p 1 \
  --json
```

**Output:**
```
✓ Bead generation complete

Beads created: 65
- Wave 1 (parallel): 32 beads
- Wave 2 (sequential): 20 beads
- Wave 3 (sequential): 10 beads
- Wave 4 (sequential): 3 beads

Total estimated time: 84 hours
Parallelization efficiency: 49%

Dependency graph: .intent/DEPENDENCY_GRAPH.mermaid

Execute with:
  bd ready --json
  bv --robot-next
```

---

## Verification: Quality Score

```
/intent:quality
```

**What happens:**
- Agent scores your plan on 5 dimensions
- Provides recommendations for improvements

**Output:**
```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  QUALITY SCORE: 94/100 ✓
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Dimension Breakdown:

  Completeness:  100/100 ✓
  ├─ All required fields present
  ├─ All beads have test cases
  └─ All edge cases covered

  Consistency:   100/100 ✓
  ├─ No circular dependencies
  ├─ No contradicting requirements
  └─ Naming conventions followed

  Testability:   100/100 ✓
  ├─ Every bead has test cases
  ├─ Every test has given/when/then
  └─ Test coverage > 80%

  Clarity:       95/100 ⚠
  ├─ All beads have 'why' field
  ├─ 95% of tests have clear descriptions
  └─ Recommendation: Clarify test AUTH-012.test_case_3

  Security:      85/100 ✓
  ├─ SQL injection covered
  ├─ XSS covered
  ├─ Auth bypass covered
  ├─ Rate limiting covered
  └─ Recommendation: Add CSRF protection

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Recommendations:
1. Add CSRF token validation to all state-changing operations
2. Clarify test case AUTH-012.test_case_3 description

Gaps:
- CSRF protection not explicitly mentioned in contracts
```

---

## Execution (Outside Intent CLI)

Intent CLI generates beads. You execute them with your tools of choice:

### Option 1: View with Beads Viewer

```bash
bv --robot-triage
```

Shows:
- Top priority beads
- Dependency graph
- Parallel execution tracks
- Critical path analysis

### Option 2: Claim and Execute

```bash
bd ready --json        # See ready beads
bd update AUTH-001 --status in_progress
# ... implement AUTH-001 ...
bd close AUTH-001 --reason "Completed"
```

### Option 3: Use with GSD

Generate beads with Intent, execute with GSD:

```bash
# Intent generates the beads
/intent:generate-beads

# GSD executes them (if you have GSD installed)
/gsd:execute-plan .intent/phases/01-foundation/PLAN.md
```

---

## Complete Example Session

```bash
# 1. Initialize
/intent:init user-api

# 2. Interview (answers 18 questions across 6 EARS patterns)
/intent:interview

# 3. Analyze (discovers 41 additional requirements)
/intent:analyze

# 4. Generate contracts (65 KIRK contracts)
/intent:contract

# 5. Plan phases (4 phases, wave-based)
/intent:plan-phase 1
/intent:plan-phase 2
/intent:plan-phase 3
/intent:plan-phase 4

# 6. Generate beads (65 beads in bd database)
/intent:generate-beads

# 7. Verify quality (94/100 score)
/intent:quality

# 8. Execute beads
bd ready --json
bv --robot-next
```

**Total time: ~30 minutes of interview + analysis**

**Output: 65 crystal-clear, deterministic beads ready for AI execution**

---

## Tips for Best Results

### During Interview
- ✅ Be specific ("validate email format" not "validate input")
- ✅ Think in terms of triggers ("when user clicks", "when timeout occurs")
- ✅ State negative requirements ("shall NOT allow unauthenticated access")
- ❌ Don't use vague words ("should", "may", "could")
- ❌ Don't skip patterns (all 6 are important)

### During Analysis
- ✅ Accept discovered requirements (mental lattices catch what you missed)
- ✅ Review inversion failures (they're often security gaps)
- ✅ Consider second-order consequences (deleting users affects many systems)
- ❌ Don't skip analysis (edge cases WILL appear during execution)

### During Contract Generation
- ✅ Validate CUE schemas (`cue vet schema/contract.cue .intent/CONTRACTS.cue`)
- ✅ Review preconditions (are they realistic?)
- ✅ Review postconditions (are they sufficient?)
- ❌ Don't skip validation (invalid contracts → invalid beads)

### During Bead Generation
- ✅ Review dependency graph (is parallelization maximized?)
- ✅ Check wave assignments (are they correct?)
- ✅ Verify estimates (are they realistic?)
- ❌ Don't skip quality check (aim for 90%+ score)

---

## Troubleshooting

**"Interview session interrupted"**
```
/intent:resume-interview
```

**"CUE validation failed"**
```bash
cue vet schema/contract.cue .intent/CONTRACTS.cue
# Fix reported errors, then retry
```

**"Quality score below 90%"**
```
/intent:gaps
# Review recommendations
# Fix issues
/intent:quality  # Re-check
```

**"Circular dependencies detected"**
```
/intent:bead-graph
# Review dependency graph
# Identify cycle
# Refactor contracts to break cycle
```

---

## Next Steps

**For implementation:**
- Use `bd ready` to see ready beads
- Use `bv --robot-next` for AI agent recommendations
- Use your favorite implementation tool (GSD, Cursor, Copilot, etc.)

**For iteration:**
- `/intent:analyze` again after discovering new requirements
- `/intent:generate-beads` again to refresh bead definitions
- `/intent:quality` to track improvement

**For new features:**
- Add requirements to `.intent/REQUIREMENTS.md`
- Run `/intent:analyze` → `/intent:contract` → `/intent:generate-beads`

---

**You now have a systematic, deterministic planning system.**

No more ambiguity. No more missed edge cases. No more "hope Claude gets it right."

Just crystal-clear beads that AI can execute mechanically.
