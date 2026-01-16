# Intent CLI Transformation - Executive Summary

## What Intent CLI Becomes

**Before**: API testing tool (execute tests against running APIs)
**After**: Planning system (generate beads in bd database)

### The One-Sentence Mission

> Transform vague requirements into crystal-clear beads with proper epic hierarchies, dependencies, and complete metadata—ready for execution by any tool.

---

## The Complete Workflow

```
┌──────────────────────────────────────────────────────────────────┐
│                     PLANNING PHASE ONLY                           │
└──────────────────────────────────────────────────────────────────┘

Step 1: EARS Interview
  Command: /intent:interview
  Output: .intent/REQUIREMENTS.md (24 requirements)
  ⬇ ✋ REVIEW: /intent:review-requirements → approve or edit

Step 2: Mental Lattice Analysis
  Command: /intent:analyze
  Output: .intent/ANALYSIS.md (+41 requirements = 65 total)
  ⬇ ✋ REVIEW: /intent:review-analysis → approve or edit

Step 3: KIRK Contract Generation
  Command: /intent:contract
  Output: .intent/CONTRACTS.cue (65 contracts, CUE validated)
  ⬇ ✋ REVIEW: /intent:review-contracts → approve or edit

Step 4: Epic & Feature Structure
  Command: /intent:plan-structure
  Output: .intent/STRUCTURE.md (epic → features → tasks)
  ⬇ ✋ REVIEW: /intent:review-structure → approve or edit

Step 5: Bead Generation
  Command: /intent:generate-beads
  Output: Beads in bd database (1 epic, 4 features, 14 tasks)
  ⬇ ✋ FINAL REVIEW: /intent:review-final → confirm or abort

┌──────────────────────────────────────────────────────────────────┐
│                   EXECUTION PHASE (NOT IN INTENT)                 │
└──────────────────────────────────────────────────────────────────┘

Use any tool you want:
  - bd ready --json (manual execution)
  - bv --robot-triage (AI selection)
  - GSD (/gsd:execute-plan)
  - Cursor, Copilot, Aider, etc.
```

---

## What Gets Generated

### In .intent/ Directory (Planning Artifacts)

```
.intent/
├── PROJECT.md            # Project context
├── REQUIREMENTS.md       # 65 EARS-formatted requirements
├── ANALYSIS.md           # Mental lattice discoveries
├── CONTRACTS.cue         # 65 KIRK contracts (CUE validated)
├── STRUCTURE.md          # Epic/feature hierarchy
├── QUALITY.json          # 5-dimension quality scores
└── DEPENDENCIES.mermaid  # Dependency graph visualization
```

### In bd Database (Work Items)

```
Epic: bd-001 "User Authentication System"
  ├─ Feature: bd-001.1 "Email/Password Registration"
  │   ├─ Task: bd-001.1.1 "Validate email format"
  │   │   ├─ Description: Full description with why/done_when/edge_cases
  │   │   ├─ Preconditions: ["Request includes email field", ...]
  │   │   ├─ Postconditions: ["Valid email accepted", ...]
  │   │   ├─ Invariants: ["Emails are unique", ...]
  │   │   ├─ Test Cases: [given/when/then objects]
  │   │   ├─ Edge Cases: ["Unicode email", "SQL injection", ...]
  │   │   ├─ Estimate: 0.75 hours
  │   │   ├─ Wave: 1 (no dependencies)
  │   │   └─ Files: context_files + output_files
  │   ├─ Task: bd-001.1.2 "Hash password with bcrypt"
  │   └─ Task: bd-001.1.3 "Create user in database"
  ├─ Feature: bd-001.2 "Login Flow" (depends on bd-001.1)
  │   └─ ... (3 tasks)
  ├─ Feature: bd-001.3 "Token Validation" (depends on bd-001.2)
  │   └─ ... (3 tasks)
  └─ Feature: bd-001.4 "Security Edge Cases" (depends on all above)
      └─ ... (5 tasks)

Total: 1 epic, 4 features, 14 tasks
Dependencies: Properly set via bd dep add
Waves: 7 waves (50% parallelization)
Estimate: 18.5 hours total, 11.5 hours critical path
```

---

## What Intent CLI Does NOT Do

❌ Execute code
❌ Run tests
❌ Implement anything
❌ Touch your codebase
❌ Generate code files
❌ Modify git repository
❌ Run builds or deployments

✅ Intent CLI ONLY generates beads in bd database

---

## Key Innovations

### 1. EARS Interview (Formal Requirements)

Instead of "tell me what you want," Intent asks systematic questions:

**Traditional approach:**
```
User: "I want user authentication"
AI: "OK, I'll build it"
→ Result: Vague implementation, missing edge cases
```

**Intent approach:**
```
Agent: "What must ALWAYS be true?" (Ubiquitous)
User: "System must validate all inputs"

Agent: "WHEN what happens?" (Event-Driven)
User: "When user submits registration, validate email"

Agent: "WHILE in what state?" (State-Driven)
User: "While user is authenticated, allow dashboard access"

... 18 more questions across 6 patterns

→ Result: 24 crystal-clear requirements in EARS format
```

### 2. Mental Lattices (Gap Detection)

Instead of "you thought of everything," Intent systematically finds gaps:

**Inversion Lattice:**
```
Requirement: "WHEN user submits login THE SYSTEM SHALL authenticate"

What could FAIL?
→ SQL injection in email field
→ Brute force attacks (no rate limiting)
→ Expired tokens accepted
→ Passwords logged in plain text

Generated 4 new security requirements
```

**Second-Order Lattice:**
```
Requirement: "WHEN user is deleted THE SYSTEM SHALL remove record"

What happens AFTER?
→ User's sessions still active (security breach)
→ User's items orphaned (data integrity)
→ Audit logs broken (compliance issue)

Generated 3 new consequence requirements
```

### 3. KIRK Contracts (Machine-Verifiable Specs)

Instead of "done when it works," Intent defines precise contracts:

**Traditional task:**
```
Title: "Add email validation"
Description: "Validate email format when user registers"
Done: ??? (ambiguous)
```

**Intent task (from KIRK contract):**
```
Title: "Validate email format on registration"

Preconditions:
- Request includes email field (required)
- Email is non-empty string

Postconditions:
- Valid email → accept, continue to user creation
- Invalid email → return 400 with clear error message

Invariants:
- Emails are unique (case-insensitive)
- All stored emails pass RFC 5322 validation

Edge Cases:
- Unicode email: 用户@example.com
- SQL injection: admin'--@example.com
- Multiple @ symbols: user@@example.com
- Empty string: ""

Test Cases:
1. Given "user@example.com", when POST, then 200
2. Given "not-an-email", when POST, then 400
3. Given "admin'--@example.com", when POST, then 400 (injection prevented)
```

### 4. Epic Hierarchies (Proper Structure)

Instead of flat task lists, Intent creates semantic hierarchies:

**Traditional:**
```
- Task: Validate email
- Task: Hash password
- Task: Create user
- Task: Validate credentials
- Task: Generate JWT
... (flat list, no grouping)
```

**Intent:**
```
Epic: User Authentication System
  └─ Feature: Registration
      ├─ Task: Validate email
      ├─ Task: Hash password
      └─ Task: Create user
  └─ Feature: Login (depends on Registration)
      ├─ Task: Validate credentials
      └─ Task: Generate JWT
```

### 5. Wave-Based Dependencies (Parallelization)

Instead of "do these in order," Intent calculates optimal waves:

**Traditional:**
```
Task 1 → Task 2 → Task 3 → Task 4 → ...
(all sequential, 14 hours)
```

**Intent:**
```
Wave 1 (parallel):     Task 1.1, Task 1.2          (1.75h)
Wave 2 (sequential):   Task 1.3                     (1.75h)
Wave 3 (parallel):     Task 2.1, Task 2.2          (1.5h)
Wave 4 (sequential):   Task 2.3                     (1h)
Wave 5 (parallel):     Task 3.1, Task 3.2          (1.5h)
Wave 6 (sequential):   Task 3.3                     (2h)
Wave 7 (parallel):     Task 4.1, 4.2, 4.3, 4.4, 4.5 (2h)

Critical path: 11.5 hours (18% faster)
Parallelization: 50% of work runs concurrently
```

### 6. Quality Scoring (Measurable Standards)

Instead of "looks good," Intent scores on 5 dimensions:

```
Completeness: 100/100 ✓
- All required fields present
- All beads have test cases
- All edge cases covered

Consistency: 100/100 ✓
- No circular dependencies
- No contradicting requirements
- Naming conventions followed

Testability: 100/100 ✓
- Every bead has test cases
- Every test has given/when/then
- Edge case coverage > 80%

Clarity: 95/100 ⚠
- All beads have 'why' field
- 95% of descriptions clear
- Recommendation: Clarify task bd-001.4.2

Security: 85/100 ✓
- SQL injection covered
- XSS covered
- Auth bypass covered
- Recommendation: Add CSRF protection

OVERALL: 96/100 ✓ (Target: 90%+)
```

---

## Review & Edit Workflow

Every step has a review gate:

```bash
# Generate requirements
/intent:interview
→ .intent/REQUIREMENTS.md created

# ✋ REVIEW GATE 1
/intent:review-requirements
# Shows: 24 requirements, EARS breakdown, quality checks
# Options:
#   /intent:edit-requirements     (edit file)
#   /intent:add-requirement       (add one)
#   /intent:approve-requirements  (continue)

# Discover gaps
/intent:analyze
→ .intent/ANALYSIS.md created (+41 requirements)

# ✋ REVIEW GATE 2
/intent:review-analysis
# Shows: 41 discoveries, mental lattice breakdown
# Options:
#   /intent:edit-analysis         (edit file)
#   /intent:review-discoveries    (review one-by-one)
#   /intent:approve-analysis      (continue)

# Generate contracts
/intent:contract
→ .intent/CONTRACTS.cue created (CUE validated)

# ✋ REVIEW GATE 3
/intent:review-contracts
# Shows: 65 contracts, quality checks, inconsistencies
# Options:
#   /intent:edit-contract <name>  (edit specific)
#   /intent:fix-inconsistencies   (auto-fix)
#   /intent:approve-contracts     (continue)

# Plan structure
/intent:plan-structure
→ .intent/STRUCTURE.md created

# ✋ REVIEW GATE 4
/intent:review-structure
# Shows: Epic hierarchy, dependency graph, wave analysis
# Options:
#   /intent:edit-structure        (interactive editor)
#   /intent:optimize-waves        (auto-optimize)
#   /intent:approve-structure     (continue)

# Generate beads
/intent:generate-beads

# ✋ FINAL REVIEW
/intent:review-final
# Shows: Complete summary, quality score, bead count
# Options:
#   Generate beads (FINAL ACTION)
#   Abort (go back to edit)
```

**Nothing proceeds without approval.**

---

## Migration from Current Implementation

### What to Keep

**Gleam Code:**
- `src/intent/validator.gleam` - CUE schema validation
- `src/intent/quality.gleam` - Quality scoring engine

**CUE Schemas:**
- `schema/bead.cue` - Bead structure definition
- `schema/contract.cue` - KIRK contract definition
- `schema/quality.cue` - Quality metrics definition

**Documentation:**
- `docs/EARS_KIRK_WORKFLOW.md` - Formal methods guide
- `docs/MENTAL_LATTICE_FRAMEWORK.md` - Thinking models

### What to Delete

**Gleam Code:**
- `src/intent/checker.gleam` - Response validation (testing tool)
- `src/intent/http_client.gleam` - HTTP execution (testing tool)
- `src/intent/runner.gleam` - Test runner (testing tool)
- All API testing logic

**Commands:**
- `intent check` - Run tests (not needed)
- `intent run` - Execute tests (not needed)

### What to Build

**Meta-Prompting Layer:**
- `commands/intent/*.md` - Slash commands for Claude Code
- `intent/workflows/*.md` - Detailed process logic
- `intent/agents/*.md` - Subagent definitions
- `intent/templates/*.md` - File structure templates
- `intent/references/*.md` - Deep-dive guides

**Installation:**
- `bin/install.js` - NPM installer (GSD pattern)
- `package.json` - NPM package definition

---

## Expected Results

### Time Investment

**Planning session (one-time):**
- Interview: 20-30 minutes (18-24 questions)
- Review requirements: 5 minutes
- Analysis: 5-10 minutes (mental lattices run automatically)
- Review analysis: 5 minutes
- Contract generation: 2-5 minutes (automatic from requirements)
- Review contracts: 10 minutes
- Structure planning: 5 minutes (automatic grouping)
- Review structure: 5 minutes
- Bead generation: 2 minutes (automatic from structure)
- Final review: 5 minutes

**Total: ~60 minutes for complete planning**

### Output Quality

**Before (traditional planning):**
- Vague requirements → AI improvises
- Edge cases discovered during execution
- No formal acceptance criteria
- Flat task list, unclear dependencies
- Quality varies by AI mood

**After (Intent planning):**
- 65 crystal-clear requirements (EARS formatted)
- 193 edge cases enumerated upfront
- 65 formal contracts (preconditions, postconditions, invariants)
- Proper epic → feature → task hierarchy
- 12 dependencies explicitly set
- 96/100 quality score (measurable)
- 50% parallelization (7 hours saved)

### Execution Readiness

**Bead completeness:**
```yaml
Every bead includes:
✓ Clear what/why
✓ Measurable done_when criteria
✓ Preconditions (what must be true before)
✓ Postconditions (what must be true after)
✓ Invariants (what must always be true)
✓ Edge cases to test (from inversion analysis)
✓ Test cases (given/when/then format)
✓ Context files to read before starting
✓ Output files to create/modify
✓ Effort estimate
✓ Wave assignment (parallelization)
✓ Dependencies (explicit via bd dep add)
```

**AI can execute with ZERO questions.**

---

## Integration with Other Tools

### With bd (Beads)

Intent creates beads, you execute with bd:

```bash
# After Intent generates beads
bd ready --json                # See ready tasks
bd update bd-001.1.1 --status in_progress
# ... implement the task ...
bd close bd-001.1.1 --reason "Completed"
```

### With bv (Beads Viewer)

Intent creates structure, bv visualizes:

```bash
bv --robot-triage              # AI analysis of all beads
bv --robot-next                # AI picks best task
bv --robot-plan                # Parallel execution tracks
bv --robot-insights            # Dependency graph, critical path
```

### With GSD

Intent plans, GSD executes:

```bash
# Intent creates .intent/phases/01-name/PLAN.md
# GSD executes it
/gsd:execute-phase 1
```

### With Any AI Tool

Intent creates beads with complete context:

```
Cursor/Copilot/Aider reads:
- Task description (what/why)
- Preconditions (setup needed)
- Postconditions (acceptance criteria)
- Test cases (how to verify)
- Context files (what to read)
- Output files (what to create)

AI has everything it needs. No questions.
```

---

## Success Criteria

Intent CLI transformation succeeds when:

1. **Beads are generated in bd database** ✓
2. **Proper epic → feature → task hierarchy** ✓
3. **All dependencies set correctly** ✓
4. **Every bead has complete metadata** ✓
5. **Quality score ≥ 90%** ✓
6. **No execution code remains** ✓
7. **All state validated by CUE schemas** ✓
8. **Review gates at every step** ✓
9. **Editable at any checkpoint** ✓
10. **Deterministic output** (same requirements → same beads) ✓

---

## Next Steps

1. **Read the docs** (in order):
   - `PURE_PLANNING_SYSTEM.md` - What Intent becomes
   - `REVIEW_AND_EDIT_WORKFLOW.md` - Review gates and editing
   - `TRANSFORMATION_PLAN.md` - Implementation roadmap
   - `ARCHITECTURE.md` - System design
   - `QUICK_START.md` - User guide

2. **Implementation phases** (~2 weeks):
   - Phase 1: Command structure (1 day)
   - Phase 2: EARS interview workflow (2 days)
   - Phase 3: Mental lattice agents (3 days)
   - Phase 4: KIRK contract generator (2 days)
   - Phase 5: Structure planner (2 days)
   - Phase 6: Bead generator (2 days)
   - Phase 7: Review gates (1 day)
   - Phase 8: Testing & docs (2 days)

3. **Test with real project**:
   - Run complete workflow
   - Verify beads are complete
   - Check quality scores
   - Execute beads with various tools

---

## The Vision

**Intent CLI becomes the planning layer for AI-assisted development.**

It takes your vague ideas and transforms them into crystal-clear, atomic work items that ANY tool can execute deterministically.

**No more:**
- "I hope Claude understands what I want"
- "Did I think of all the edge cases?"
- "Is this requirement clear enough?"
- "What order should these tasks run in?"

**Instead:**
- EARS interview ensures complete requirements
- Mental lattices discover all edge cases
- KIRK contracts define precise acceptance criteria
- Bead generator creates perfect work items
- Review gates ensure quality before execution

**The result:** Planning as rigorous as code itself.
