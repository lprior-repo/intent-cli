# Intent CLI - Pure Planning System

**Mission**: Transform vague requirements into crystal-clear beads with proper epic hierarchies, dependencies, and structure in the bd database.

**THAT'S IT. NO EXECUTION.**

---

## Core Philosophy

```
Vague Idea → EARS Interview → Mental Lattices → KIRK Contracts → Beads in bd
                                                                    ↓
                                                            (Other tools execute)
```

**Intent CLI's ONLY job:**
1. Extract complete requirements (EARS)
2. Find gaps and edge cases (Mental Lattices)
3. Generate formal contracts (KIRK)
4. Create properly structured beads in bd database

**Intent CLI does NOT:**
- ❌ Execute code
- ❌ Run tests
- ❌ Implement anything
- ❌ Touch your codebase
- ❌ Generate code files

---

## The Planning Pipeline

### Input: Vague Idea
```
"I want a user authentication system"
```

### Output: Beads in bd Database
```
Epic: bd-001 "User Authentication System"
  ├─ Feature: bd-001.1 "Email/Password Registration"
  │   ├─ Task: bd-001.1.1 "Validate email format"
  │   ├─ Task: bd-001.1.2 "Hash password with bcrypt"
  │   └─ Task: bd-001.1.3 "Store user in database"
  ├─ Feature: bd-001.2 "Login Flow"
  │   ├─ Task: bd-001.2.1 "Validate credentials"
  │   ├─ Task: bd-001.2.2 "Generate JWT token"
  │   └─ Task: bd-001.2.3 "Return token in response"
  ├─ Feature: bd-001.3 "Token Validation"
  │   ├─ Task: bd-001.3.1 "Verify JWT signature"
  │   ├─ Task: bd-001.3.2 "Check token expiry"
  │   └─ Task: bd-001.3.3 "Extract user from token"
  └─ Feature: bd-001.4 "Security Edge Cases"
      ├─ Task: bd-001.4.1 "Prevent SQL injection in email"
      ├─ Task: bd-001.4.2 "Rate limit login attempts"
      ├─ Task: bd-001.4.3 "Invalidate tokens on logout"
      └─ Task: bd-001.4.4 "Handle expired tokens gracefully"
```

---

## The 5-Step Workflow

### Step 1: EARS Interview

```
/intent:interview
```

**What happens:**
- Agent asks systematic questions using 6 EARS patterns
- You answer each question
- Agent formats into EARS requirements

**Output:**
```
.intent/REQUIREMENTS.md

THE SYSTEM SHALL validate all inputs (Ubiquitous)
WHEN user submits registration THE SYSTEM SHALL validate email format (Event-Driven)
WHILE user is authenticated THE SYSTEM SHALL allow access to dashboard (State-Driven)
WHERE feature flag enabled THE SYSTEM SHALL show beta features (Optional)
IF authentication fails THE SYSTEM SHALL NOT grant access (Unwanted)
WHILE session active WHEN token expires THE SYSTEM SHALL redirect to login (Complex)
```

---

### Step 2: Mental Lattice Analysis

```
/intent:analyze
```

**What happens:**
- Agent applies 5 thinking models to find gaps

**Lattice 1 - Inversion (what could fail?):**
```
Requirement: "WHEN user submits login THE SYSTEM SHALL authenticate credentials"

Failures discovered:
- SQL injection in email field
- Brute force attack (no rate limiting)
- Expired token still accepted
- Password in plain text in logs

New requirements added:
- "IF input contains SQL patterns THE SYSTEM SHALL NOT execute query"
- "WHEN login attempts exceed 5 in 1 minute THE SYSTEM SHALL NOT accept further attempts"
- "IF token is expired THE SYSTEM SHALL NOT accept token"
- "THE SYSTEM SHALL NOT log passwords or tokens"
```

**Lattice 2 - Second-Order (consequences):**
```
Requirement: "WHEN user is deleted THE SYSTEM SHALL remove user record"

Second-order effects:
- User's sessions must be invalidated
- User's created items become orphaned
- Audit logs reference missing user

New requirements added:
- "WHEN user is deleted THE SYSTEM SHALL invalidate all user sessions"
- "WHEN user is deleted THE SYSTEM SHALL transfer or delete user's items"
```

**Output:**
```
.intent/ANALYSIS.md

Original requirements: 24
Discovered requirements: 41
Total requirements: 65
```

---

### Step 3: KIRK Contract Generation

```
/intent:contract
```

**What happens:**
- Agent transforms EARS requirements into KIRK contracts
- Each contract defines preconditions, postconditions, invariants

**Output:**
```
.intent/CONTRACTS.cue

{
  behavior: "validate-registration-email"
  intent: "Ensure all registered users have valid email addresses"

  kirk: {
    preconditions: {
      fields: {
        required: ["email"]
        email: {
          type: "string"
          constraints: ["non-empty", "valid RFC 5322 format"]
        }
      }
    }

    postconditions: {
      state_changes: [
        "If valid: email accepted, continue to user creation",
        "If invalid: return 400 with clear error message"
      ]
      response: {
        status: 200 | 400
        guarantees: {
          valid_email: "Email format verified"
          unique_email: "Email doesn't exist in database"
        }
      }
    }

    invariants: [
      "Emails are unique (case-insensitive)",
      "All stored emails pass RFC 5322 validation"
    ]
  }

  ears_pattern: "event_driven"
  ears_formatted: "WHEN user submits registration THE SYSTEM SHALL validate email format"
}
```

---

### Step 4: Epic & Bead Structure Planning

```
/intent:plan-structure
```

**What happens:**
- Agent groups contracts into logical features
- Creates epic hierarchy
- Identifies dependencies
- Plans parallel execution waves

**Output:**
```
.intent/STRUCTURE.md

Epic: User Authentication System
├─ Feature: Registration (no dependencies)
│  └─ Contracts: validate-registration-email, hash-password, create-user-record
├─ Feature: Login (depends on: Registration)
│  └─ Contracts: validate-credentials, generate-jwt, return-token
├─ Feature: Token Validation (depends on: Login)
│  └─ Contracts: verify-jwt-signature, check-token-expiry, extract-user
└─ Feature: Security (depends on: all above)
   └─ Contracts: prevent-sql-injection, rate-limit-login, handle-expired-tokens

Execution Waves:
- Wave 1 (parallel): Registration feature
- Wave 2 (sequential): Login feature
- Wave 3 (sequential): Token Validation feature
- Wave 4 (sequential): Security feature
```

---

### Step 5: Generate Beads

```
/intent:generate-beads
```

**What happens:**
- Agent creates beads in bd database
- Epic → Features → Tasks hierarchy
- All dependencies set up
- All metadata populated

**Bead structure:**
```yaml
# Epic
id: bd-001
type: epic
title: "User Authentication System"
description: "Complete authentication system with email/password, JWT tokens, and security"
priority: 1

# Feature (child of epic)
id: bd-001.1
parent: bd-001
type: feature
title: "Email/Password Registration"
description: "Allow users to register with email and password"
priority: 1

# Task (child of feature)
id: bd-001.1.1
parent: bd-001.1
type: task
title: "Validate email format on registration"
description: |
  Implement email validation that checks format against RFC 5322 standard.

  Why: Ensure all registered users have valid email addresses to prevent fake accounts.

  Done when:
  - Valid emails pass validation (e.g., user@example.com)
  - Invalid emails return 400 with clear error message
  - All test cases pass
  - Edge cases handled

  Preconditions:
  - Request includes email field
  - Email is non-empty string

  Postconditions:
  - Valid email accepted (continues to user creation)
  - Invalid email rejected with 400
  - Error message explains format requirement

  Invariants:
  - Emails are unique (case-insensitive)
  - All stored emails are valid format

  Edge Cases to Test:
  - Email with unicode characters
  - Email with multiple @ symbols
  - Email with spaces
  - Very long email (>320 chars)
  - Email with SQL injection payload (admin'--@example.com)
  - Empty string
  - Null value

  Test Cases:
  1. Given valid email "user@example.com", when POST /register, then returns 200
  2. Given invalid email "not-an-email", when POST /register, then returns 400
  3. Given SQL injection "admin'--@example.com", when POST /register, then returns 400
  4. Given unicode email "用户@example.com", when POST /register, then handles correctly

  Context Files to Read:
  - schema/users.sql
  - src/types/user.ts

  Files to Create/Modify:
  - src/validators/email.ts
  - test/validators/email.test.ts

priority: 1
estimate_hours: 0.75
wave: 1
dependencies: []
```

**bd commands executed:**
```bash
# Create epic
bd create "User Authentication System" \
  -t epic -p 1 \
  --json

# Create feature (child of epic)
bd create "Email/Password Registration" \
  --parent bd-001 \
  -t feature -p 1 \
  --json

# Create task (child of feature)
bd create "Validate email format on registration" \
  --parent bd-001.1 \
  -t task -p 1 \
  --json

# Set dependencies (for tasks that need them)
bd dep add bd-001.2.1 bd-001.1.3  # Login depends on registration complete
```

**Output:**
```
✓ Generated beads in bd database

Structure:
- 1 epic (bd-001)
- 4 features (bd-001.1 to bd-001.4)
- 14 tasks (bd-001.1.1 to bd-001.4.4)

Dependency graph saved to: .intent/DEPENDENCIES.mermaid

Execute with:
  bd ready --json              # See ready tasks
  bv --robot-triage            # AI-powered task selection
  bv --robot-plan              # Parallel execution tracks
```

---

## Quality Verification

```
/intent:quality
```

**What happens:**
- Agent scores the PLAN (not execution) on 5 dimensions
- Checks bead structure quality

**Output:**
```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  BEAD STRUCTURE QUALITY: 96/100 ✓
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Completeness: 100/100 ✓
├─ All epics have features
├─ All features have tasks
├─ All tasks have descriptions
├─ All tasks have done_when criteria
├─ All tasks have edge_cases
└─ All tasks have test_cases

Consistency: 100/100 ✓
├─ No circular dependencies
├─ All parent-child relationships valid
├─ Wave assignments consistent with dependencies
└─ Priorities cascade correctly (epic → feature → task)

Testability: 100/100 ✓
├─ Every task has test cases
├─ Every test has given/when/then
└─ Edge cases enumerated

Clarity: 95/100 ⚠
├─ All tasks have 'why' explanations
├─ 95% of descriptions are clear
└─ Recommendation: Clarify bd-001.4.2 description

Security: 90/100 ✓
├─ SQL injection covered
├─ XSS covered
├─ Auth bypass covered
├─ Rate limiting covered
└─ Recommendation: Add CSRF protection to state-changing operations

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Hierarchy Validation:
✓ Epic has 4 features (recommended: 3-7)
✓ Features have avg 3.5 tasks (recommended: 2-5)
✓ Deepest nesting: 3 levels (epic → feature → task)
✓ No orphaned tasks
✓ No missing dependencies

Execution Readiness:
✓ Wave 1 has 5 tasks (can execute in parallel)
✓ Wave 2 has 4 tasks (blocked by Wave 1)
✓ Wave 3 has 3 tasks (blocked by Wave 2)
✓ Wave 4 has 2 tasks (blocked by Wave 3)
✓ Parallelization: 36% (5/14 tasks can run concurrently)

Recommendations:
1. Add CSRF protection to bd-001.2.2 and bd-001.1.3
2. Clarify rate limiting threshold in bd-001.4.2
3. Consider splitting bd-001.4 into two features (auth security vs data security)
```

---

## Bead Metadata Structure

Every bead created by Intent includes:

### Required Fields
```yaml
id: string                    # bd-001, bd-001.1, bd-001.1.1
parent: string | null         # Parent bead ID
type: epic | feature | task
title: string                 # Action-oriented title
description: string           # Full description with context
priority: 0-4                 # 0=critical, 2=medium, 4=backlog
```

### Planning Fields (in description)
```yaml
why: string                   # Business justification
done_when: [string]           # Measurable completion criteria
preconditions: [string]       # KIRK preconditions
postconditions: [string]      # KIRK postconditions
invariants: [string]          # KIRK invariants
edge_cases: [string]          # From inversion analysis
test_cases: [object]          # given/when/then test cases
context_files: [string]       # Files to read before starting
output_files: [string]        # Files to create/modify
```

### Execution Fields
```yaml
estimate_hours: float         # Effort estimate
wave: int                     # Parallel execution wave
dependencies: [string]        # Other bead IDs (set via bd dep add)
```

---

## Dependency Management

### How Dependencies Work

**Method 1: Hierarchical (parent-child)**
```bash
bd create "Epic" -t epic --json
bd create "Feature" --parent bd-001 -t feature --json
bd create "Task" --parent bd-001.1 -t task --json
```

Hierarchy: `bd-001 (epic) → bd-001.1 (feature) → bd-001.1.1 (task)`

**Method 2: Explicit Dependencies**
```bash
bd dep add bd-002.1 bd-001.3  # bd-002.1 depends on bd-001.3
```

This means: "bd-002.1 cannot start until bd-001.3 is complete"

### Intent's Dependency Logic

```
Tasks in same feature:
→ No dependencies (can run in parallel) unless contracts reference each other

Tasks in different features:
→ Feature-level dependencies determine task-level dependencies

Example:
bd-001.1 (Registration feature)
  ├─ bd-001.1.1 (validate email) - Wave 1
  ├─ bd-001.1.2 (hash password) - Wave 1
  └─ bd-001.1.3 (create user) - Wave 2 (depends on 1.1 AND 1.2)

bd-001.2 (Login feature) depends on bd-001.1
  ├─ bd-001.2.1 (validate credentials) - Wave 3 (blocked by bd-001.1.3)
  ├─ bd-001.2.2 (generate JWT) - Wave 3 (can run parallel with 2.1)
  └─ bd-001.2.3 (return token) - Wave 4 (depends on 2.2)
```

---

## Wave Assignment Rules

**Wave 1**: No dependencies (execute immediately, in parallel)
- Tasks that require no prior tasks
- Typically: validation, schema definitions, types

**Wave 2**: Depends on Wave 1 only
- Tasks that need Wave 1 outputs
- Typically: data creation, basic operations

**Wave 3**: Depends on Wave 2 (or Wave 1 + Wave 2)
- Tasks that need data from Wave 2
- Typically: business logic, integrations

**Wave 4+**: Sequential work that builds on everything
- Tasks that need complete system
- Typically: end-to-end flows, security, edge cases

---

## Example: Complete Session

```bash
# Step 1: Interview
/intent:interview user-authentication

# Agent asks questions across 6 EARS patterns
# User answers ~15-20 questions
# Output: .intent/REQUIREMENTS.md (24 requirements)

# Step 2: Analyze
/intent:analyze

# Agent applies mental lattices
# Discovers 41 additional requirements
# Output: .intent/ANALYSIS.md (65 total requirements)

# Step 3: Generate Contracts
/intent:contract

# Agent transforms EARS → KIRK contracts
# Output: .intent/CONTRACTS.cue (65 contracts, CUE validated)

# Step 4: Plan Structure
/intent:plan-structure

# Agent groups into epic/feature hierarchy
# Identifies dependencies
# Calculates waves
# Output: .intent/STRUCTURE.md

# Step 5: Generate Beads
/intent:generate-beads

# Agent creates beads in bd database
# Output: 1 epic, 4 features, 14 tasks
# All dependencies set up
# All metadata populated

# Step 6: Verify Quality
/intent:quality

# Agent scores bead structure
# Output: 96/100 quality score
# Recommendations for improvement

# Done! Beads ready for execution
bd ready --json
bv --robot-triage
```

---

## What Happens After Intent CLI?

Intent CLI outputs beads. You execute them however you want:

### Option 1: Manual Execution
```bash
bd ready --json                # See what's ready
bd update bd-001.1.1 --status in_progress
# ... implement the task ...
bd close bd-001.1.1
```

### Option 2: AI-Powered Selection
```bash
bv --robot-next               # AI picks best task
# Follow AI recommendation
```

### Option 3: Use with GSD
```bash
# Intent plans, GSD executes
/gsd:execute-plan <generated-plan>
```

### Option 4: Use with Other Tools
- Cursor
- GitHub Copilot
- Aider
- Any AI coding assistant

**Intent's job is DONE once beads are in bd database.**

---

## Key Benefits

### 1. Completeness
Every bead has:
- Clear "what" and "why"
- Measurable "done when" criteria
- Preconditions and postconditions (KIRK)
- Edge cases enumerated (mental lattices)
- Test cases defined (given/when/then)

### 2. Structure
- Proper epic → feature → task hierarchy
- Dependencies explicitly set
- Wave assignments for parallelization
- Priority cascading (epic priority → feature priority → task priority)

### 3. Quality
- 5-dimension scoring
- Validation before bead creation
- Recommendations for improvement
- Gap detection

### 4. Determinism
- Same requirements → same beads (100% reproducible)
- No AI variance in bead generation
- CUE schema validation ensures consistency

---

## Anti-Patterns to Avoid

❌ **Skipping EARS interview** → Ambiguous requirements → incomplete beads
❌ **Skipping mental lattice analysis** → Missing edge cases → surprises during execution
❌ **Skipping KIRK contracts** → Unclear acceptance criteria → no clear "done"
❌ **Skipping quality verification** → Low-quality beads slip through
❌ **Manual bead creation** → Inconsistent structure, missing metadata

✅ **Always use the full workflow**: Interview → Analyze → Contract → Plan → Generate → Verify

---

## Success Criteria

Intent CLI succeeds when:

- ✅ Beads are generated in bd database
- ✅ Proper epic → feature → task hierarchy
- ✅ All dependencies set correctly
- ✅ Wave assignments enable parallelization
- ✅ Every bead has complete metadata
- ✅ Quality score ≥ 90%
- ✅ No manual bead editing needed
- ✅ Execution tools can pick up and run immediately

**Intent CLI's ONLY output: Beads in bd database. That's it.**

No execution. No code generation. Just perfect planning.
