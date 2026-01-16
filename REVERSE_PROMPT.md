# Intent CLI - Master Reverse Prompt

**Use this prompt to guide AI development, onboard new developers, or ensure consistency across all work on this codebase.**

---

## IDENTITY & MISSION

You are working on **Intent CLI**, a contract-driven API testing framework written in Gleam. The core philosophy is:

> **"Human-writes, AI-verifies, AI-implements"**

The mission: Transform vague requirements into crystal-clear, atomic work items (beads) that AI can execute **deterministically**. By the time a bead reaches the AI, every question is answered, every edge case enumerated, and implementation becomes mechanical translation.

**CUE is the center of the universe.** All state flows through typed, validated CUE schemas:
- Requirements → CUE
- Interview state → CUE
- AI directives → CUE
- Work items (beads) → CUE
- Feedback loops → CUE

---

## CORE ARCHITECTURE PRINCIPLES

### 1. Deterministic Planning
```
Human writes requirements → CLI interviews systematically → CUE schemas control AI → AI executes perfectly
```

No improvisation. No guessing. The CLI tells the AI exactly what to do via structured CUE directives.

### 2. All State in CUE (Option B Architecture)
- Session files: `.intent/sessions-{id}.cue`
- Feedback (append-only): `.intent/feedback-{id}.cue`
- Interview answers: `.intent/answers-{id}.cue`
- All validated with `cue vet`
- **NO JSONL for session state** - pure CUE

### 3. No Backwards Compatibility
**ALL spec fields are mandatory.** No silent defaults. This forces completeness and provides clearer error messages.

### 4. Result Types for Error Handling
Use Gleam's `Result` type everywhere. Pattern match exhaustively. Never panic.

### 5. Beads for Issue Tracking
Use `bd` (beads) CLI for all work tracking. Git-friendly JSONL format. **Never use markdown TODOs.**

---

## THE FIVE MENTAL LATTICES

Every feature must be analyzed through these five thinking models:

### 1. EARS (Requirements Syntax)
Six patterns eliminate ambiguity in requirements:

| Pattern | Template | Use For |
|---------|----------|---------|
| **Ubiquitous** | THE SYSTEM SHALL [behavior] | Always true |
| **Event-Driven** | WHEN [trigger] THE SYSTEM SHALL [behavior] | Cause-effect |
| **State-Driven** | WHILE [state] THE SYSTEM SHALL [behavior] | State-dependent |
| **Optional** | WHERE [condition] THE SYSTEM SHALL [behavior] | Feature flags |
| **Unwanted** | IF [condition] THE SYSTEM SHALL NOT [behavior] | Security |
| **Complex** | WHILE [state] WHEN [trigger] THE SYSTEM SHALL | Combinations |

### 2. KIRK Contracts (Design by Contract)
Every behavior needs:
- **Preconditions**: What must be true before
- **Postconditions**: What must be true after
- **Invariants**: What must always be true

### 3. Inversion Thinking
For every feature, ask: "What could fail?"
- Security inversions (auth bypass, SQL injection, XSS)
- Usability inversions (not found, invalid format, duplicates)
- Integration inversions (idempotency, timeouts, versioning)

### 4. Second-Order Thinking
- What happens after this action?
- What are the cascade effects?
- What dependencies are hidden?

### 5. Pre-Mortem Analysis (Gary Klein)
- Assume the project failed
- Work backwards to identify likely causes
- Assess probability (low/medium/high)
- Define mitigation strategies

---

## SPEC FORMAT REQUIREMENTS

### Required Spec-Level Fields
```cue
#Spec: {
  name: string           // Spec name
  description: string    // Human-readable description
  audience: string       // Target users of the API
  version: string        // Semantic version
  success_criteria: [...string]  // List of acceptance criteria
  config: #Config        // base_url, timeout_ms, headers
  features: [...#Feature] // At least one feature
  rules: [...#Rule]       // Global validation rules (can be empty)
  anti_patterns: [...#AntiPattern]  // (can be empty)
  ai_hints: #AIHints      // Implementation guidance
}
```

### Required Feature Fields
```cue
#Feature: {
  name: string
  description: string
  behaviors: [...#Behavior]  // CANNOT be empty
}
```

### Required Behavior Fields
```cue
#Behavior: {
  name: string           // Unique identifier
  intent: string         // Plain English description
  notes: string          // Can be empty string ""
  requires: [...string]  // Dependencies (can be empty list)
  tags: [...string]      // Classification (can be empty list)
  request: #Request      // method, path, headers, query, body
  response: #Response    // status, example, checks, headers
  captures: {...}        // Output values (can be empty dict)
}
```

### Required Check Fields
```cue
#Check: {
  rule: string   // Validation rule expression
  why: string    // Explanation of why this check matters
}
```

---

## VALIDATION RULE SYNTAX

### Field Validation Rules
```
is uuid              # Valid UUID v4
is email             # RFC 5322 email
is url               # Valid URL
is iso8601           # ISO8601 timestamp
is json              # Valid JSON
matches <regex>      # Regex pattern match
equals <value>       # Exact value comparison
length <n>           # Exact length
min_length <n>       # Minimum length
max_length <n>       # Maximum length
is integer           # Integer value
is number            # Number (int or float)
is string            # String value
is boolean           # Boolean value
is array             # Array value
is object            # Object value
is null              # Null value
absent               # Field must not exist
non-empty string     # Non-empty string
string containing <substring>
string starts with <prefix>
string ends with <suffix>
```

### Global Rule Conditions
```cue
when: {
  status: ">= 200"      // Status conditions: "200", ">= 200", "< 300"
  method: "GET"         // HTTP method
  path: "/users.*"      // Regex path pattern
}
```

---

## QUALITY DIMENSIONS

Every spec is scored 0-100 on five dimensions:

| Dimension | Target | Meaning |
|-----------|--------|---------|
| **Completeness** | 100% | All required fields filled |
| **Consistency** | 100% | Zero conflicts between behaviors |
| **Testability** | 100% | Every behavior has checks |
| **Clarity** | 100% | Every check has 'why' explanation |
| **Security** | 80%+ | OWASP coverage |

**Overall Target: 90%+**

---

## CODE STYLE REQUIREMENTS

### Gleam Conventions
```gleam
// Use Result types for error handling
pub fn parse_something(data: Dynamic) -> Result(Thing, List(DecodeError))

// Pattern match exhaustively
case method {
  Get -> "GET"
  Post -> "POST"
  Put -> "PUT"
  Patch -> "PATCH"
  Delete -> "DELETE"
  Head -> "HEAD"
  Options -> "OPTIONS"
}

// Prefer pipelines for data transformation
data
|> parse_spec
|> result.map(validate)
|> result.flatten
```

### File Organization
```
src/intent/
├── types.gleam          # Core type definitions
├── parser.gleam         # JSON parsing with dynamic_to_json utility
├── loader.gleam         # CUE file loading
├── runner.gleam         # Test execution orchestrator
├── checker.gleam        # Response validation (~900 lines)
├── http_client.gleam    # HTTP execution with interpolation
├── resolver.gleam       # Behavior dependency resolution
├── interpolate.gleam    # Variable substitution
├── rules_engine.gleam   # Global rule evaluation
├── interview.gleam      # Interview engine (722 lines)
├── kirk/
│   ├── ears_parser.gleam      # EARS → behaviors
│   ├── quality_analyzer.gleam # 5-dimension scoring
│   ├── inversion_checker.gleam
│   ├── coverage_analyzer.gleam
│   └── gap_detector.gleam
```

### Keep It Simple
- Only make changes directly requested
- Don't add features beyond what was asked
- Don't refactor code unless asked
- Don't add docstrings to code you didn't change
- Don't add error handling for impossible scenarios
- Delete unused code completely (no `_var` renames)

---

## CLI COMMANDS

### Core Testing
```bash
intent check <spec.cue> --target <url>   # Run spec against API
intent validate <spec.cue>                # Validate CUE syntax only
intent show <spec.cue>                    # Pretty print spec
intent export <spec.cue>                  # Export to JSON
intent lint <spec.cue>                    # Quality checks
intent analyze <spec.cue>                 # Comprehensive analysis
```

### Interview & Automation
```bash
intent interview --profile api --cue              # Start interview
intent interview --session X --answer "response"  # Submit answer
intent interview --answers=file.cue --export=spec.cue  # Non-interactive
intent beads <session>                            # Generate beads
intent bead-status <id> --result success|failed|blocked
intent beads-regenerate <session_id>              # Regenerate from feedback
intent plan <session_id>                          # Generate execution plan
intent plan-approve <session_id> --yes            # Approve for CI
```

### KIRK Analysis
```bash
intent quality <spec.cue>    # 5-dimension scoring
intent invert <spec.cue>     # Failure case analysis
intent coverage <spec.cue>   # HTTP method/status coverage
intent gaps <spec.cue>       # Gap detection
intent ears <requirements.md> --output cue   # Parse EARS
intent effects <spec.cue>    # Second-order effects
intent compact <spec.cue>    # 50% token reduction for AI
```

---

## BEADS WORKFLOW

### Finding Work
```bash
bd ready --json              # Show unblocked issues
bv --robot-triage            # Comprehensive analysis
bv --robot-next              # Single top pick with claim command
bv --robot-plan              # Parallel execution tracks
```

### Working on Issues
```bash
bd update <id> --status in_progress --json   # Claim work
# ... do the work ...
bd close <id> --reason "Done" --json         # Complete
```

### Creating Issues
```bash
bd create "Title" -t bug|feature|task -p 0-4 --json
bd create "Subtask" --parent <epic-id> --json  # Hierarchical
bd create "Found bug" -p 1 --deps discovered-from:<parent-id> --json
```

### Priority Levels
- **P0**: Critical (security, data loss, broken builds)
- **P1**: High (major features, important bugs)
- **P2**: Medium (default, nice-to-have)
- **P3**: Low (polish, optimization)
- **P4**: Backlog (future ideas)

**NEVER use markdown TODOs. Always use bd.**

---

## KEY CONSTRAINTS

1. **All spec fields mandatory** - No optional fields without explicit empty values
2. **No backwards compatibility** - Version changes are breaking
3. **Deterministic execution** - Same input always produces same output
4. **Type safety** - Use Result types, pattern match exhaustively
5. **Token efficiency** - Compact format for AI (<50% of original)
6. **Human-in-loop** - Plan approval before execution
7. **Non-interactive support** - `--answers` flag for CI/CD
8. **CUE validation** - All state validated with `cue vet`

---

## OPEN ISSUES

### P2: DOS Protection (intent-cli-22e)
JSON parser has no size/depth limits. Need:
- 10MB size limit
- 1000 nesting depth limit
- Timeout protection

### P2: Performance (intent-cli-44e)
Rules engine header checking is O(n²). Need lowercase index caching.

---

## DEVELOPMENT COMMANDS

```bash
gleam build     # Compile
gleam test      # Run tests (583 tests)
gleam run -- check examples/user-api.cue --target http://localhost:8080
```

---

## THE GOAL

> By the time a bead reaches the AI, every possible question has been answered, every edge case has been enumerated, and the implementation is purely mechanical translation from specification to code.

**This is deterministic AI-assisted development.**

---

## SUMMARY CHECKLIST

When working on this codebase, always:

- [ ] Use `bd` for all issue tracking
- [ ] Write specs with ALL required fields
- [ ] Include `why` for every check
- [ ] Apply the 5 mental lattices to new features
- [ ] Use Result types for error handling
- [ ] Pattern match exhaustively
- [ ] Keep changes minimal and focused
- [ ] Run `gleam test` before committing
- [ ] Validate CUE files with `cue vet`
- [ ] Target 90%+ quality score
