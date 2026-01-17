# Claude Instructions for Intent CLI

## 🚨 MANDATORY: Moon CI/CD Only

**CRITICAL**: This project uses moon for ALL build, test, and development tasks. You MUST use moon commands exclusively.

- ❌ **NEVER** run `gleam build`, `gleam test`, `gleam format` directly
- ✅ **ALWAYS** use `moon run :build`, `moon run :test`, `moon run :format`
- ✅ **ALWAYS** use `moon run :ci` before commits
- ✅ **ALWAYS** use `moon run :install` to install the binary system-wide

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads) for issue tracking. Use `bd` commands instead of markdown TODOs. See AGENTS.md for workflow details.

## 🤖 AI AGENT INTERVIEW MODE

**FOR AI AGENTS**: Intent has a special CUE mode designed for AI agents to conduct requirements interviews non-interactively.

### Quick Start for AI Agents

**1. Start Interview (outputs JSON with session ID and first question):**
```bash
intent interview --cue --profile api
```

**2. Submit Answer (outputs JSON with next question or completion):**
```bash
intent interview --cue --session "interview-abc123" --answer "THE SYSTEM SHALL authenticate users via JWT tokens"
```

**3. Repeat step 2 until you receive `"action": "interview_complete"`**

### Key Points
- **Always use `--cue` flag** for AI-friendly JSON output
- **Session ID** is provided in the first response - use it for all subsequent answers
- **EARS format** - answers should follow patterns like "THE SYSTEM SHALL [behavior]"
- **5 rounds** - interview progresses through discovery, refinement, and validation automatically
- **Progress tracking** - each response includes `progress.current_step` and `progress.total_steps`

### Example Response Format
```json
{
  "action": "ask_question",
  "question": {
    "text": "In one sentence, what should this API do?",
    "pattern": "ubiquitous",
    "examples": ["THE SYSTEM SHALL provide user authentication"],
    "hint": "Start with 'THE SYSTEM SHALL' and describe the core behavior"
  },
  "progress": {
    "current_step": 1,
    "total_steps": 25,
    "percent_complete": 0
  },
  "session": {
    "id": "interview-abc123",
    "profile": "api"
  }
}
```

### Available Profiles
- `api` - REST/HTTP APIs
- `cli` - Command-line tools
- `event` - Event-driven systems
- `data` - Data systems
- `workflow` - Business workflows
- `ui` - User interfaces

**Full documentation:** See `docs/AI_PROTOCOL_QUICKSTART.md` for complete details.

## Quick Reference

### Moon CI/CD (MANDATORY)
```bash
moon run :ci              # Run full pipeline before commits
moon run :install         # Build and install binary system-wide
moon run :format          # Auto-format code
moon run :test            # Run test suite
```

### Beads Workflow
```bash
bd ready --json           # Find ready work
bd update <id> --status in_progress --json  # Claim work
bd close <id> --reason "Done" --json        # Complete work
```

## Beads Viewer (bv) - Robot Commands Only

**CRITICAL**: Use ONLY `--robot-*` flags. Bare `bv` launches an interactive TUI that blocks your session.

```bash
bv --robot-triage          # Comprehensive analysis with recommendations
bv --robot-next            # Single top pick with claim command
bv --robot-plan            # Parallel execution tracks
bv --robot-insights        # Full metrics (PageRank, critical path)
bv --robot-graph --graph-format=json  # Dependency graph export
```

## Project Context

**Intent** is a requirements engineering and API testing CLI that transforms vague specifications into deterministic work items. Written in Gleam (BEAM/Erlang VM).

### What Intent Does

1. **Requirements Engineering**
   - AI-driven interviews to gather complete requirements
   - EARS (Easy Approach to Requirements Syntax) pattern parsing
   - Mental lattice frameworks for completeness checking
   - Gap detection via inversion/second-order thinking

2. **API Contract Testing**
   - CUE specification parsing and validation
   - HTTP request execution against target APIs
   - Response validation with custom rules engine
   - Behavior dependency resolution

3. **KIRK Analysis** (Design by Contract)
   - Quality scoring (5 dimensions: completeness, clarity, testability, coverage, correctness)
   - Inversion checking (what could fail?)
   - Coverage analysis (HTTP methods/status codes)
   - Effects analysis (side effects and state changes)

4. **Bead Generation**
   - Atomic work item synthesis from requirements
   - Dependency tracking between beads
   - Feedback loop for iterative refinement
   - CUE-formatted output for AI consumption

5. **Plan Mode**
   - Collaborative specification refinement
   - Iterative feedback cycles
   - Human approval gates

### The Vision

```
Human writes requirements → CLI interviews systematically →
CUE schemas control AI → Atomic beads generated → AI executes deterministically
```

CUE is the center: requirements, interview state, AI directives, beads, and feedback all flow through validated CUE schemas.

## Key Files

### Core Entry Point
- `src/intent.gleam` - CLI with glint commands (check, validate, interview, beads, quality, etc.)

### Interview System
- `src/intent/interview.gleam` - Interview orchestration engine
- `src/intent/interview_questions.gleam` - Question database
- `src/intent/interview_storage.gleam` - Session persistence
- `src/intent/question_loader.gleam` - CUE question loading
- `src/intent/answer_loader.gleam` - Answer processing

### KIRK Analysis
- `src/intent/kirk/quality_analyzer.gleam` - 5-dimension quality scoring
- `src/intent/kirk/inversion_checker.gleam` - Failure mode analysis
- `src/intent/kirk/coverage_analyzer.gleam` - Test coverage metrics
- `src/intent/kirk/gap_detector.gleam` - Mental lattice gap detection
- `src/intent/kirk/ears_parser.gleam` - EARS requirements parsing
- `src/intent/kirk/effects_analyzer.gleam` - Side effect analysis

### API Testing
- `src/intent/checker.gleam` - Response validation (largest module)
- `src/intent/runner.gleam` - Test execution orchestrator
- `src/intent/http_client.gleam` - HTTP request execution
- `src/intent/rules_engine.gleam` - Global rule evaluation
- `src/intent/anti_patterns.gleam` - Anti-pattern detection

### Core Infrastructure
- `src/intent/types.gleam` - Custom type definitions (Spec, Behavior, Request, Response, Check)
- `src/intent/parser.gleam` - JSON to Gleam type parsing
- `src/intent/loader.gleam` - CUE file loading via system calls
- `src/intent/resolver.gleam` - Behavior dependency resolution
- `src/intent/validator.gleam` - Spec validation
- `src/intent/interpolate.gleam` - Variable interpolation
- `src/intent/output.gleam` - Result formatting

### Bead System
- `src/intent/bead_templates.gleam` - Atomic work item generation
- `src/intent/bead_feedback.gleam` - Iterative refinement
- `src/intent/spec_builder.gleam` - Spec construction from beads

### Utilities
- `src/intent/rule.gleam` - Rule expression parser
- `src/intent/formats.gleam` - Output formatting
- `src/intent/errors.gleam` - Error type definitions
- `src/intent/ai_errors.gleam` - **AI-friendly error messages with structured output**
- `src/intent_ffi.erl` - Erlang FFI for system operations
- `src/intent/stdin.gleam` - Standard input handling
- `src/intent/cli_ui.gleam` - Terminal UI components
- `src/intent/security.gleam` - Security validation

## Development Commands

**CRITICAL**: Use moon exclusively. Direct gleam commands are prohibited.

```bash
# Moon CI/CD commands (REQUIRED)
moon run :ci             # Full pipeline: format → build → test
moon run :format         # Auto-format code
moon run :build          # Compile project
moon run :test           # Run test suite (1201+ tests)
moon run :install        # Build and install binary to ~/.local/bin
moon run :dev            # Run local development server

# Run CLI commands (after install)
intent check examples/user-api.cue --target http://localhost:8080
intent interview --profile api
intent quality examples/user-api.cue
intent invert examples/user-api.cue
intent beads <session-id>
```

## Spec Format Requirements

All fields in Intent specifications are **required**. No backwards compatibility defaults are provided:

### Required Spec Fields
- `name` - Spec name
- `description` - Human-readable description
- `audience` - Target users of the API
- `version` - Semantic version
- `success_criteria` - List of acceptance criteria
- `config` - Configuration with `base_url`, `timeout_ms`, and `headers`
- `features` - List of feature specifications
- `rules` - Global validation rules
- `anti_patterns` - Anti-patterns to avoid
- `ai_hints` - Implementation guidance

### Required Feature Fields
- `name` - Feature name
- `description` - Feature description
- `behaviors` - List of behavior specifications (cannot be empty)

### Required Behavior Fields
- `name` - Behavior identifier
- `intent` - What this behavior demonstrates
- `request` - HTTP request with `method`, `path`, `headers`, `query`, `body`
- `response` - Expected response with `status`, `example`, `checks`, `headers`
- `notes` - Implementation notes (can be empty string)
- `requires` - Behavior dependencies (can be empty list)
- `tags` - Classification tags (can be empty list)
- `captures` - Output values for later use (can be empty dict)

### Required Check Fields
- `rule` - Validation rule expression
- `why` - Explanation of why this check matters

All optional-looking fields (like empty strings, empty lists) must be explicitly provided in CUE specs.

## AI Agent Workflows

This section provides complete documentation for AI agents using Intent CLI in non-interactive, deterministic workflows.

### Overview

Intent CLI provides multiple AI-optimized workflows:
1. **Interview Mode** - Gather requirements via structured Q&A (`--cue` flag)
2. **KIRK Analysis** - Analyze specs for quality, gaps, coverage, effects
3. **API Testing** - Execute contract tests against live APIs
4. **Bead Generation** - Convert requirements into atomic work items
5. **Error Handling** - Parse and recover from structured errors

All AI workflows use **CUE/JSON output** for machine parsing and **session-based state** for resumability.

---

### 1. Interview Mode (`--cue` flag)

**Purpose**: Conduct systematic requirements gathering without human interaction.

#### Starting an Interview

```bash
# Start new interview for API profile
intent interview --cue --profile api
```

**Output** (CUE format):
```cue
{
    action: "ask_question"

    question: {
        text: "In one sentence, what should this API do?"
        pattern: "ubiquitous"
        examples: ["THE SYSTEM SHALL provide user authentication"]
        hint: "Start with 'THE SYSTEM SHALL' and describe the core behavior"
    }

    progress: {
        current_step: 1
        total_steps: 25
        percent_complete: 0
    }

    session: {
        id: "interview-abc123"
        profile: "api"
        started_at: "2026-01-17T10:30:00Z"
    }
}
```

#### Submitting Answers

```bash
# Submit answer to current question
intent interview --cue --session "interview-abc123" --answer "THE SYSTEM SHALL authenticate users via JWT tokens and return access/refresh token pairs"
```

**Output** (next question or completion):
```cue
{
    action: "ask_question"

    question: {
        text: "What are the main features or capabilities?"
        pattern: "event"
        examples: [
            "WHEN user submits valid credentials, THE SYSTEM SHALL return JWT tokens",
            "WHEN token expires, THE SYSTEM SHALL return 401 Unauthorized"
        ]
        hint: "Use WHEN/WHERE for event-driven behaviors"
    }

    progress: {
        current_step: 2
        total_steps: 25
        percent_complete: 8
    }

    session: {
        id: "interview-abc123"
        profile: "api"
        started_at: "2026-01-17T10:30:00Z"
    }
}
```

#### Interview Completion

After all questions answered:

```cue
{
    action: "interview_complete"

    output: {
        spec_path: ".interview/spec-interview-abc123.cue"
        behaviors_count: 12
        anti_patterns_count: 3
        summary: "Interview complete. Generated spec with 12 behaviors."
    }

    session: {
        id: "interview-abc123"
        profile: "api"
        started_at: "2026-01-17T10:30:00Z"
        completed_at: "2026-01-17T10:45:00Z"
    }

    next_steps: [
        "Review generated spec: cat .interview/spec-interview-abc123.cue",
        "Run quality analysis: intent quality .interview/spec-interview-abc123.cue",
        "Generate beads: intent beads interview-abc123"
    ]
}
```

#### Interview Profiles

- **`api`** - REST/HTTP APIs (default)
- **`cli`** - Command-line tools
- **`event`** - Event-driven systems
- **`data`** - Data processing systems
- **`workflow`** - Business workflows
- **`ui`** - User interfaces

#### EARS Patterns

Answers should follow EARS (Easy Approach to Requirements Syntax):

- **Ubiquitous**: `THE SYSTEM SHALL [behavior]`
- **Event**: `WHEN [trigger], THE SYSTEM SHALL [response]`
- **State**: `WHILE [condition], THE SYSTEM SHALL [behavior]`
- **Unwanted**: `IF [condition], THEN THE SYSTEM SHALL [behavior]`
- **Optional**: `WHERE [feature enabled], THE SYSTEM SHALL [behavior]`

#### Session Management

```bash
# Resume existing session
intent interview --cue --session "interview-abc123"

# List all sessions
intent sessions

# View session history
intent history interview-abc123
```

#### Error Handling in CUE Mode

If validation fails:

```cue
{
    action: "validation_error"
    error: {
        message: "Answer too short"
        suggestion: "Please provide a more detailed response"
        retry_allowed: true
    }
}
```

Exit code: `1` (retry the question)

---

### 2. KIRK Analysis Commands

**KIRK** = **K**nowledge **I**nversion **R**easoning **K**it

Design by Contract analysis for specifications.

#### Quality Analysis

**Purpose**: Score spec across 5 dimensions (0-100 scale)

```bash
intent quality examples/user-api.cue
```

**Output**:
```
Quality Analysis Report
=======================

Overall Score: 82/100

Dimensions:
  Completeness:  85/100  - Good coverage of CRUD operations
  Clarity:       90/100  - Clear behavior descriptions
  Testability:   80/100  - Most behaviors have test criteria
  Coverage:      75/100  - Missing DELETE operations
  Correctness:   78/100  - Some validation rules incomplete

Strengths:
  ✓ All behaviors have clear intent statements
  ✓ Request/response examples provided
  ✓ Security headers specified

Weaknesses:
  ✗ Missing error response examples
  ✗ No rate limiting behaviors
  ✗ Incomplete edge case coverage

Recommendations:
  1. Add error response examples for all 4xx/5xx status codes
  2. Define rate limiting behaviors (429 responses)
  3. Add edge cases for empty/null inputs
```

#### Inversion Checking

**Purpose**: Find what could break (second-order thinking)

```bash
intent invert examples/user-api.cue
```

**Output**:
```
Inversion Analysis
==================

What Could Go Wrong?

Feature: User Registration
  Behavior: successful-registration
    ✗ Email uniqueness not validated
    ✗ Password strength requirements unclear
    ✗ No rate limiting on registration endpoint
    ✗ Missing username validation (SQL injection risk)

  Behavior: registration-duplicate-email
    ✓ Duplicate email handling defined
    ✗ Case sensitivity not specified (user@X.com vs USER@x.com)

Feature: Authentication
  Behavior: successful-login
    ✗ No failed login attempt tracking
    ✗ Account lockout policy undefined
    ✗ Token expiration not specified
    ✗ Missing refresh token rotation

Critical Risks:
  🔴 No SQL injection protection
  🔴 Missing CSRF token validation
  🟡 Password reset flow undefined
  🟡 Email verification not required

Recommendations:
  1. Add input sanitization rules
  2. Define security headers (CSP, HSTS, etc.)
  3. Specify rate limiting per endpoint
  4. Add account recovery workflows
```

#### Coverage Analysis

**Purpose**: Check HTTP method/status code coverage

```bash
intent coverage examples/user-api.cue
```

**Output**:
```
Coverage Analysis
=================

HTTP Methods:
  ✓ GET     (5 behaviors)
  ✓ POST    (3 behaviors)
  ✓ PUT     (2 behaviors)
  ✓ PATCH   (1 behavior)
  ✗ DELETE  (0 behaviors)  ← Missing!

Status Codes:
  ✓ 200 OK
  ✓ 201 Created
  ✓ 400 Bad Request
  ✓ 401 Unauthorized
  ✓ 404 Not Found
  ✗ 403 Forbidden      ← Missing!
  ✗ 409 Conflict       ← Missing!
  ✗ 429 Too Many Requests  ← Missing!
  ✗ 500 Internal Server Error  ← Missing!

Endpoints:
  /users         GET, POST  (✓ Complete)
  /users/:id     GET, PUT   (✗ Missing DELETE)
  /auth/login    POST       (✓ Complete)
  /auth/refresh  POST       (✓ Complete)

Recommendations:
  1. Add DELETE /users/:id behavior
  2. Define 403 for authorization failures
  3. Add 409 for conflict scenarios
  4. Define rate limiting (429) responses
  5. Add error handling (500) behaviors
```

#### Gap Detection

**Purpose**: Find missing requirements using mental lattice framework

```bash
intent gaps examples/user-api.cue
```

**Output**:
```
Gap Analysis
============

Missing Critical Requirements:

Authentication & Authorization:
  ✗ Password reset workflow
  ✗ Email verification flow
  ✗ Two-factor authentication
  ✗ Session management (logout)
  ✗ API key authentication option

Data Validation:
  ✗ Email format validation rules
  ✗ Password complexity requirements
  ✗ Input length limits
  ✗ Character encoding handling

Error Handling:
  ✗ Rate limiting responses
  ✗ Maintenance mode (503)
  ✗ Request timeout handling
  ✗ Malformed JSON responses

Security:
  ✗ CORS policy
  ✗ CSRF protection
  ✗ SQL injection prevention
  ✗ XSS protection headers

Performance:
  ✗ Pagination for list endpoints
  ✗ Response caching strategy
  ✗ Concurrent request handling

Audit & Monitoring:
  ✗ Audit logging requirements
  ✗ Performance metrics
  ✗ Error tracking

Blocking Gaps (Must Fix):
  🔴 Password reset (users will get locked out)
  🔴 Rate limiting (DoS vulnerability)
  🔴 Input validation (injection attacks)

Nice-to-Have Gaps:
  🟡 Email verification
  🟡 Two-factor auth
  🟡 API key option
```

#### Effects Analysis

**Purpose**: Identify side effects and state changes

```bash
intent effects examples/user-api.cue
```

**Output**:
```
Effects Analysis
================

Side Effects by Behavior:

POST /users (successful-registration)
  Write Effects:
    - Creates new user record in database
    - Hashes and stores password
    - Generates unique user ID
  State Changes:
    - User count increments
    - Email becomes unavailable for registration
  External Effects:
    - May send welcome email (not specified)
  Idempotent: No
  Reversible: Yes (via DELETE /users/:id)

POST /auth/login (successful-login)
  Write Effects:
    - Creates session token
    - Updates last_login timestamp
  State Changes:
    - User session becomes active
    - Token pool increases
  External Effects:
    - None
  Idempotent: No (generates new token each time)
  Reversible: Partial (session expires, but timestamp persists)

PUT /users/:id (update-user-profile)
  Write Effects:
    - Updates user record fields
  State Changes:
    - User data changes
  External Effects:
    - None specified
  Idempotent: Yes
  Reversible: No (old values not preserved)

Recommendations:
  1. Document email sending behavior
  2. Add audit trail for user updates
  3. Consider soft deletes instead of hard deletes
  4. Specify session expiration behavior
  5. Add version tracking for updates
```

---

### 3. API Testing Workflow

**Purpose**: Execute contract tests against live APIs

#### Basic Check

```bash
# Run all tests against target API
intent check examples/user-api.cue --target http://localhost:8080
```

**Output** (human-readable by default):
```
Running Intent Specification Tests
===================================

Spec: User Management API
Target: http://localhost:8080

Feature: User Registration
  ✓ successful-registration (201 Created, 245ms)
  ✓ registration-duplicate-email (400 Bad Request, 89ms)
  ✗ registration-invalid-email (Expected 400, got 200)
    - Rule failed: status == 400
    - Response: {"id": 123, "email": "invalid"}

Feature: Authentication
  ✓ successful-login (200 OK, 156ms)
  ✓ login-invalid-credentials (401 Unauthorized, 78ms)

Results:
  Passed: 4/5 behaviors (80%)
  Failed: 1/5 behaviors
  Blocked: 0/5 behaviors
  Duration: 657ms

Exit code: 1
```

#### JSON Output for AI Parsing

```bash
# Machine-readable JSON output
intent check examples/user-api.cue --target http://localhost:8080 --json
```

**Output**:
```json
{
  "spec_name": "User Management API",
  "target_url": "http://localhost:8080",
  "started_at": "2026-01-17T10:30:00Z",
  "completed_at": "2026-01-17T10:30:01Z",
  "duration_ms": 657,
  "features": [
    {
      "name": "User Registration",
      "behaviors": [
        {
          "name": "successful-registration",
          "status": "passed",
          "duration_ms": 245,
          "checks_passed": 8,
          "checks_failed": 0
        },
        {
          "name": "registration-invalid-email",
          "status": "failed",
          "duration_ms": 123,
          "checks_passed": 7,
          "checks_failed": 1,
          "failures": [
            {
              "rule": "status == 400",
              "expected": "400",
              "actual": "200",
              "why": "Invalid email should be rejected"
            }
          ]
        }
      ]
    }
  ],
  "summary": {
    "total_behaviors": 5,
    "passed": 4,
    "failed": 1,
    "blocked": 0,
    "pass_rate": 0.8
  }
}
```

#### Filtering Tests

```bash
# Run specific feature only
intent check spec.cue --target URL --feature "User Registration"

# Run specific behavior only
intent check spec.cue --target URL --behavior "successful-registration"

# Verbose output (includes request/response details)
intent check spec.cue --target URL --verbose

# Quiet output (errors only)
intent check spec.cue --target URL --quiet
```

#### Handling Blocked Behaviors

If a behavior depends on another that failed:

```
Feature: User Management
  ✓ create-user (201 Created)
  ✗ login-user (401 Unauthorized - expected 200)
  ⊘ update-user (BLOCKED: depends on login-user)
  ⊘ delete-user (BLOCKED: depends on login-user)

Exit code: 2 (blocked behaviors present)
```

---

### 4. Bead Generation Workflow

**Purpose**: Convert interview sessions into atomic work items

#### Generate Beads from Interview

```bash
# Generate beads for completed interview
intent beads interview-abc123
```

**Output**:
```cue
{
    action: "beads_generated"

    beads: [
        {
            id: "bead-1"
            title: "Implement user registration endpoint"
            type: "feature"
            priority: "high"

            description: """
                Implement POST /users endpoint for user registration.

                Acceptance Criteria:
                - Accept email, password, name in request body
                - Validate email format (RFC 5322)
                - Validate password strength (min 8 chars, 1 upper, 1 lower, 1 digit)
                - Return 201 Created with user object (exclude password)
                - Return 400 Bad Request for invalid input
                - Return 409 Conflict for duplicate email
                """

            depends_on: []
            blocks: ["bead-2", "bead-3"]

            estimated_effort: "4 hours"
            test_criteria: [
                "POST /users with valid data returns 201",
                "POST /users with duplicate email returns 409",
                "POST /users with weak password returns 400",
                "Response excludes password field"
            ]
        },
        {
            id: "bead-2"
            title: "Implement user login endpoint"
            type: "feature"
            priority: "high"

            description: """
                Implement POST /auth/login for user authentication.

                Acceptance Criteria:
                - Accept email and password
                - Return JWT access token and refresh token
                - Return 401 for invalid credentials
                - Token expires in 15 minutes
                - Refresh token expires in 7 days
                """

            depends_on: ["bead-1"]
            blocks: ["bead-4", "bead-5"]

            estimated_effort: "6 hours"
            test_criteria: [
                "POST /auth/login with valid credentials returns 200 + tokens",
                "POST /auth/login with invalid credentials returns 401",
                "Access token expires after 15 minutes",
                "Tokens are valid JWT format"
            ]
        }
    ]

    summary: {
        total_beads: 12
        high_priority: 5
        medium_priority: 6
        low_priority: 1
        dependency_chains: 3
    }

    next_steps: [
        "Review generated beads",
        "Approve execution plan: intent plan-approve interview-abc123",
        "Mark beads as completed: intent bead-status <bead-id> --status completed"
    ]
}
```

#### Mark Bead Status

```bash
# Mark bead as started
intent bead-status bead-1 --status in_progress --session interview-abc123

# Mark bead as completed
intent bead-status bead-1 --status completed --session interview-abc123

# Mark bead as failed (triggers regeneration)
intent bead-status bead-1 --status failed --session interview-abc123 \
    --reason "Validation logic too complex, needs refactoring"
```

#### Regenerate Failed Beads

```bash
# Regenerate beads that failed
intent beads-regenerate interview-abc123
```

**Output**: Refined beads with adjusted scope based on failure feedback.

---

### 5. Error Handling Patterns

Intent CLI uses **AI-friendly error messages** with structured output for programmatic recovery.

#### File Not Found Error

**Command**:
```bash
intent check examples/missing-api.cue --target http://localhost:8080
```

**Output** (CUE format):
```cue
{
    action: "file_error"
    error: {
        type: "file_not_found"
        message: "File not found: examples/missing-api.cue"
        context: {
            path: "examples/missing-api.cue"
            expected_location: "CUE specification file"
        }
    }
    suggestion: "Create the missing file or directory"
    recovery: [
        "Check if the parent directory exists",
        "Create directory: mkdir -p examples",
        "Create the file with appropriate content",
        "Verify file permissions allow read/write access"
    ]
}
```

**AI Recovery Strategy**:
1. Parse the `recovery` array
2. Execute commands in order: `mkdir -p examples`
3. Create file with template content
4. Retry original command

#### CUE Validation Error

**Command**:
```bash
intent check examples/invalid-spec.cue --target http://localhost:8080
```

**Output**:
```cue
{
    action: "validation_error"
    error: {
        type: "cue_validation_error"
        message: "field 'timeout_ms' not allowed in struct"
        context: {
            file_path: "examples/invalid-spec.cue"
            line: 24
            column: 5
        }
    }
    suggestion: "Fix CUE validation errors in the specification"
    recovery: [
        "Review CUE schema: cue def github.com/intent-cli/intent/schema:intent",
        "Check field names against schema",
        "Remove invalid field or rename to match schema",
        "Run: cue vet examples/invalid-spec.cue"
    ]
}
```

#### Session Not Found Error

**Command**:
```bash
intent interview --cue --session "interview-unknown"
```

**Output**:
```cue
{
    action: "session_error"
    error: {
        type: "session_not_found"
        message: "Session not found: interview-unknown"
        context: {
            session_id: "interview-unknown"
            sessions_path: ".interview/sessions.jsonl"
        }
    }
    suggestion: "Start a new interview session"
    recovery: [
        "List available sessions: intent sessions",
        "Start new session: intent interview --cue --profile api",
        "Check session ID spelling"
    ]
}
```

#### HTTP Connection Error

**Command**:
```bash
intent check examples/user-api.cue --target http://localhost:9999
```

**Output**:
```cue
{
    action: "http_error"
    error: {
        type: "connection_refused"
        message: "Failed to connect to http://localhost:9999"
        context: {
            target_url: "http://localhost:9999"
            behavior: "successful-registration"
            endpoint: "/users"
        }
    }
    suggestion: "Verify the target API is running and accessible"
    recovery: [
        "Check if service is running: curl http://localhost:9999/health",
        "Start the API server",
        "Verify port number is correct",
        "Check firewall rules"
    ]
}
```

#### Structured Error Format

All errors follow this CUE schema:

```cue
{
    action: "error_category"  // file_error | validation_error | session_error | http_error
    error: {
        type: "specific_error_type"
        message: "human-readable description"
        context: {
            // Error-specific fields
        }
    }
    suggestion: "what to do next"
    recovery: [
        "step 1",
        "step 2",
        "step 3"
    ]
}
```

**Exit Codes**:
- `0` - Success
- `1` - Test failures
- `2` - Blocked behaviors
- `3` - Invalid specification
- `4` - General error (file not found, network error, etc.)

---

### 6. Session Management

#### Viewing Sessions

```bash
# List all sessions
intent sessions

# Filter by profile
intent sessions --profile api

# View session details
intent history interview-abc123

# View diff between sessions
intent diff interview-abc123 interview-xyz789
```

#### Session Files

- **Location**: `.interview/sessions.jsonl`
- **Format**: JSONL (one JSON object per line)
- **Persistence**: Auto-saved after each answer
- **Resumability**: Can resume from any point

**Example `.interview/sessions.jsonl`**:
```jsonl
{"id":"interview-abc123","profile":"api","stage":"discovery","rounds_completed":1,"answers":[...],"created_at":"2026-01-17T10:30:00Z"}
{"id":"interview-abc123","profile":"api","stage":"refinement","rounds_completed":2,"answers":[...],"updated_at":"2026-01-17T10:35:00Z"}
{"id":"interview-abc123","profile":"api","stage":"complete","rounds_completed":5,"answers":[...],"completed_at":"2026-01-17T10:45:00Z"}
```

---

### 7. Plan Mode (Collaborative Refinement)

**Purpose**: Iterative specification refinement with human approval gates

```bash
# Display execution plan for interview
intent plan interview-abc123
```

**Output**:
```
Execution Plan
==============

Interview: interview-abc123 (API Profile)
Status: Awaiting Approval

Beads to Implement (12 total):

Phase 1: Foundation (No dependencies)
  [bead-1] Implement user registration endpoint (4h)
  [bead-7] Set up database schema (2h)
  [bead-8] Configure JWT middleware (3h)

Phase 2: Authentication (Depends on Phase 1)
  [bead-2] Implement user login endpoint (6h)
  [bead-3] Implement token refresh endpoint (4h)

Phase 3: Profile Management (Depends on Phase 2)
  [bead-4] Implement profile update endpoint (5h)
  [bead-5] Implement profile retrieval endpoint (3h)

Phase 4: Advanced Features (Depends on Phase 3)
  [bead-6] Implement password reset flow (8h)
  [bead-9] Add rate limiting middleware (4h)

Phase 5: Testing & Documentation (Depends on all phases)
  [bead-10] Write integration tests (6h)
  [bead-11] Generate API documentation (3h)
  [bead-12] Create deployment guide (2h)

Estimated Total Effort: 50 hours
Critical Path: bead-1 → bead-2 → bead-4 → bead-6 → bead-10

Next Steps:
  1. Review plan carefully
  2. Approve: intent plan-approve interview-abc123
  3. Execute beads in dependency order
```

#### Approve Plan

```bash
# Approve execution plan
intent plan-approve interview-abc123
```

**Output**:
```cue
{
    action: "plan_approved"
    session_id: "interview-abc123"
    approved_at: "2026-01-17T11:00:00Z"
    next_steps: [
        "Start with Phase 1 beads (no dependencies)",
        "Execute: intent bead-status bead-1 --status in_progress",
        "Implement and test each bead",
        "Mark complete: intent bead-status bead-1 --status completed",
        "Proceed to next phase when all dependencies satisfied"
    ]
}
```

---

### 8. Complete AI Agent Workflow Example

**End-to-end requirements → implementation flow**:

```bash
# Step 1: Start interview
intent interview --cue --profile api
# Output: { action: "ask_question", session: { id: "interview-abc123" }, ... }

# Step 2: Answer all questions (repeat until complete)
intent interview --cue --session "interview-abc123" --answer "THE SYSTEM SHALL..."
# Output: { action: "ask_question", progress: { current_step: 2 }, ... }
# ... continue until action: "interview_complete"

# Step 3: Analyze generated spec quality
intent quality .interview/spec-interview-abc123.cue
# Output: Quality score, recommendations

# Step 4: Check for gaps and risks
intent gaps .interview/spec-interview-abc123.cue
intent invert .interview/spec-interview-abc123.cue

# Step 5: Generate beads
intent beads interview-abc123
# Output: { action: "beads_generated", beads: [...], ... }

# Step 6: Review execution plan
intent plan interview-abc123
# Output: Phased execution plan with dependencies

# Step 7: Approve plan
intent plan-approve interview-abc123
# Output: { action: "plan_approved", ... }

# Step 8: Execute beads (AI implements code)
for each bead in dependency order:
    intent bead-status <bead-id> --status in_progress --session interview-abc123
    # AI implements the bead
    intent bead-status <bead-id> --status completed --session interview-abc123

# Step 9: Test implementation against spec
intent check .interview/spec-interview-abc123.cue --target http://localhost:8080 --json
# Output: { summary: { passed: X, failed: Y }, ... }

# Step 10: If failures, regenerate beads
if failures:
    intent beads-regenerate interview-abc123
    # Output: Refined beads based on test failures
```

---

### 9. AI Agent Best Practices

1. **Always use `--cue` or `--json` flags** for machine-readable output
2. **Parse exit codes** to determine success/failure/blocked states
3. **Save session IDs** for resumability across context resets
4. **Parse `recovery` arrays** from errors for automatic fixes
5. **Check `action` field** first in CUE responses to route logic
6. **Use `progress` fields** to show user progress indicators
7. **Store `.interview/` directory** in version control for audit trail
8. **Run KIRK analysis** before generating beads to catch gaps
9. **Execute beads in dependency order** (use `blocks` and `depends_on`)
10. **Mark beads as failed with reasons** to improve regeneration

---

### 10. Common AI Agent Pitfalls

❌ **Don't**: Run interactive commands (blocks AI session)
✅ **Do**: Always use `--cue` or `--json` flags

❌ **Don't**: Ignore exit codes
✅ **Do**: Check exit code and parse error output

❌ **Don't**: Skip KIRK analysis
✅ **Do**: Run `quality`, `gaps`, `invert` before generating beads

❌ **Don't**: Execute beads in random order
✅ **Do**: Respect `depends_on` dependencies

❌ **Don't**: Lose session IDs
✅ **Do**: Save session ID from first response and reuse

❌ **Don't**: Assume all fields are optional
✅ **Do**: Provide all required CUE spec fields (see Spec Format Requirements)

❌ **Don't**: Use relative paths
✅ **Do**: Use absolute paths for all file operations

❌ **Don't**: Commit without running `moon run :ci`
✅ **Do**: Always run full pipeline before commits

---

### 11. Resources

- **AI Protocol Documentation**: `docs/AI_PROTOCOL_QUICKSTART.md`
- **EARS Syntax Guide**: `docs/EARS_KIRK_WORKFLOW.md`
- **Mental Lattice Framework**: `docs/MENTAL_LATTICE_FRAMEWORK.md`
- **API Reference**: `docs/API_REFERENCE.md`
- **Spec Format**: `docs/SPEC_FORMAT.md`
- **AI-Friendly Errors**: `docs/ai-friendly-errors.md`

---

## Moon CI/CD Pipeline

This project uses [moon](https://moonrepo.dev/) for local CI/CD pipelines and task orchestration. Moon ensures code quality through automated checks before committing.

### Installation

Moon is already installed in `~/.moon/bin`. Add to your PATH:

```bash
export PATH="$HOME/.moon/bin:$PATH"
```

### Core Commands

```bash
# Run complete CI pipeline (format → build → test)
moon run :ci

# Run individual tasks
moon run :format-check    # Check formatting
moon run :build           # Compile project
moon run :test            # Run test suite
moon run :escript         # Build single binary

# Development
moon run :format          # Auto-format code
moon run :dev             # Run local development server

# Quality checks
moon check                # Validate moon configuration
```

### Pipeline Tasks

The pipeline automatically runs tasks in dependency order:

1. **format-check**: Ensures code is formatted with `gleam format --check`
2. **build**: Compiles the project (`gleam build --target erlang`)
3. **test**: Runs the full test suite (1201+ tests)
4. **escript**: Builds single binary executable at `dist/intent/intent`

### Smart Caching

Moon uses hash-based caching to skip unchanged tasks:

```bash
# First run: builds everything
moon run :ci
▪▪▪▪ intent-cli:format-check (726ms)
▪▪▪▪ intent-cli:build (2s 747ms)
▪▪▪▪ intent-cli:test (6s 836ms)

# Second run: uses cache
moon run :ci
▪▪▪▪ intent-cli:format-check (cached)
▪▪▪▪ intent-cli:build (cached)
▪▪▪▪ intent-cli:test (cached)
```

### Building Single Binary

Compile to standalone executable:

```bash
moon run :escript
# Creates: dist/intent/intent (2.1MB escript)
```

The binary requires Erlang/OTP installed on the target system.

### Installing Binary to PATH

To build and install the `intent` binary system-wide:

```bash
# Single command to build and install
moon run :install

# Add ~/.local/bin to PATH (if not already present)
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc

# Reload shell or source config
source ~/.bashrc  # or source ~/.zshrc
```

The `:install` task automatically:
1. Runs the full CI pipeline (format-check → build → test)
2. Builds the escript binary at `dist/intent/intent`
3. Creates `~/.local/bin` directory
4. Copies binary to `~/.local/bin/intent`
5. Sets executable permissions

Verify installation:

```bash
intent --help
```

### Configuration Files

- **`.moon/workspace.yml`**: Workspace-level configuration
- **`moon.yml`**: Project-level tasks and dependencies
- **`dist/`**: Output directory for compiled binaries (gitignored)

### Pre-Commit Quality Gates

Before committing, moon ensures:
- ✅ No formatting violations
- ✅ No compilation errors
- ✅ No warnings
- ✅ All tests passing (1201+ tests)
- ✅ No type errors

**MANDATORY**: Run the full pipeline before commits:

```bash
moon run :ci && git commit -m "Your message"
```

Never commit without running `moon run :ci` first.

### Task Dependencies

Tasks run in topological order based on dependencies:

```
format-check → build → test → escript → install
                ↓
              check (type checking)
```

The `:install` task depends on `:escript`, which depends on `:test`, ensuring the binary is always built from passing code.

Moon automatically parallelizes independent tasks for optimal performance.

## The 7 Commandments of Gleam

When writing or modifying Gleam code, strictly follow these principles:

### 1. Explicitness Over Implicitness
- **No implicit type conversions**: `int.to_string(42)` not `42` as string
- **No operator overloading**: `+` for Int, `+.` for Float, `<>` for String concatenation
- **No exceptions for control flow**: Use `Result(a, e)` types
- **Explicit transformations**: Every conversion is a function call

```gleam
// Bad (would not compile)
let x = 42
let y = x + 3.14  // Type error!

// Good
let x = 42
let y = int.to_float(x) +. 3.14
```

### 2. Immutability by Default
- Variables are **labels for values**, not buckets that change
- "Mutation" is creating a new binding with the same name (shadowing)
- Lists, records, all data structures are immutable

```gleam
// Idiomatic shadowing pattern
let user = "  alice  "
let user = string.trim(user)
let user = string.lowercase(user)
io.println(user)  // "alice"
```

### 3. Type-First Design
- Define custom types before writing logic
- **No null/nil/undefined** - use `Option(T)` for optional values
- **Union types** for states (not boolean flags or integer codes)
- **Opaque types** for domain entities requiring validation

```gleam
// Model states as unions, not flags
pub type ConnectionState {
  Disconnected
  Connecting(attempt: Int)
  Connected(ip: String, port: Int)
}

// Opaque wrapper for validation
pub opaque type Email {
  Email(String)
}

pub fn new(value: String) -> Result(Email, String) {
  case string.contains(value, "@") {
    True -> Ok(Email(value))
    False -> Error("Invalid email format")
  }
}
```

### 4. Exhaustive Pattern Matching with case
- Prefer `case` over `if` for almost all logic
- Compiler enforces exhaustiveness
- Use tuple matching for complex conditions
- Guards limited to comparisons (no function calls)

```gleam
// Idiomatic: tuple matching
case user.role, is_authenticated {
  Admin, True -> admin_dashboard()
  User, True -> user_home()
  _, False -> login_page()
}

// Guards with safe operations only
case list {
  [x, ..] if x % 2 == 0 -> "Even"
  [x, ..] -> "Odd"
  [] -> "Empty"
}
```

### 5. Pipeline Flow
- Transform data with `|>` operator
- Subject-first parameter order in all functions
- Prefer pipelines over nested function calls
- Use `_` capture syntax for non-first argument position

```gleam
// Idiomatic pipeline
raw_input
|> string.trim
|> string.lowercase
|> string.split(on: ",")
|> list.map(string.trim)
|> list.filter(fn(s) { s != "" })

// Capture syntax for non-first argument
10
|> int.add(5, _)  // Passes 10 as second argument
```

### 6. Railway-Oriented Error Handling
- **Never use exceptions** for control flow
- Return `Result(value, error)` for all fallible operations
- Chain with `result.try` or `use` expression
- Map errors to domain types with `result.map_error`

```gleam
// The 'use' expression flattens Result chains
pub fn load_config() -> Result(Config, AppError) {
  use content <- result.try(
    simplifile.read("config.json")
    |> result.map_error(FileError)
  )

  use data <- result.try(
    json.decode(content, config_decoder)
    |> result.map_error(fn(_) { ParseError("Invalid JSON") })
  )

  Ok(data)
}
```

### 7. Strict Naming Conventions
- **Variables/functions**: `snake_case` (e.g., `user_id`, `parse_request`)
- **Types/Constructors**: `PascalCase` (e.g., `User`, `HttpRequest`, `Ok`, `Error`)
- **Modules**: `snake_case` matching filename (e.g., `intent/http_client`)
- **Constants**: `SCREAMING_SNAKE_CASE` (e.g., `MAX_RETRIES`)
- Casing is **semantic** and enforced by the compiler

```gleam
// Custom type definition
pub type User {
  User(id: Int, name: String, email: Option(String))
}

// Function using the type
pub fn create_user(id: Int, name: String) -> User {
  User(id: id, name: name, email: option.None)
}
```

## Anti-Patterns to Avoid

1. **Bool Blindness**: Don't use `Bool` for complex states
   - Bad: `fn check_login() -> Bool`
   - Good: `fn check_login() -> Result(User, LoginError)`

2. **Stringly Typed**: Don't use strings for enums
   - Bad: `status = "connected"`
   - Good: `status = Connected`

3. **Index Iteration**: Lists are linked lists, not arrays
   - Bad: Loop with `list.length` and indexing (O(n²))
   - Good: Use `list.map`, `list.fold`, pattern matching

4. **Primitive Obsession**: Don't pass raw `Int`/`String` everywhere
   - Bad: `fn get_user(id: Int)`
   - Good: `pub opaque type UserId { UserId(Int) }`

5. **Manual Recursion**: Prefer standard library over hand-written loops
   - Bad: Write recursive functions for everything
   - Good: Use `list.map`, `list.fold`, `list.filter`

6. **Panic in Libraries**: Never use `panic` for validation in library code
   - Bad: `panic as "Invalid input"`
   - Good: Return `Result(T, Error)`

## Testing Conventions

- Test files in `test/` directory mirror `src/` structure
- Test modules end in `_test.gleam`
- Test functions are `pub fn` and end in `_test`
- Use `gleeunit/should` for assertions: `should.equal`, `should.be_ok`
- Keep tests fast (EUnit default timeout: 5 seconds)
- Use higher-order functions for mocking (no traditional mocking framework)

```gleam
import gleeunit/should

pub fn parse_valid_email_test() {
  email.new("user@example.com")
  |> should.be_ok
}

pub fn parse_invalid_email_test() {
  email.new("not-an-email")
  |> should.be_error
}
```

**Run tests via moon**:
```bash
moon run :test           # Run all tests (1201+ tests)
moon run :ci             # Format + build + test pipeline
```

## Documentation Standards

- Module docs: `////` at top of file
- Function docs: `///` immediately before `pub fn`
- Regular comments: `//` for implementation details
- Always document public APIs
- Explain the "why", not just the "what"

```gleam
//// This module provides HTTP request execution with retry logic
//// and automatic timeout handling for API testing.

/// Execute an HTTP request with the given configuration.
///
/// Returns the response body and status code on success.
/// Returns an error if the request fails or times out.
pub fn execute(request: Request) -> Result(Response, HttpError) {
  // Implementation
}
```

## Module Organization

- **File = Module**: One-to-one mapping
- **No circular dependencies**: Extract shared types to separate module
- **Private by default**: Use `pub` explicitly for public APIs
- **Opaque types**: Use `pub opaque type` for encapsulation

```
src/intent/
  types.gleam          # Shared types (imported by many modules)
  parser.gleam         # Imports types
  checker.gleam        # Imports types and parser
  http_client.gleam    # Imports types only
```

## Git Workflow with bd

1. **Find work**: `bd ready` or `bv --robot-next`
2. **Claim task**: `bd update <id> --status in_progress`
3. **Implement with commits**: Stage code changes
4. **Beads auto-sync**: Daemon handles `.beads/issues.jsonl` automatically
5. **Complete work**: `bd close <id> --reason "Done"`
6. **Push code**: `git push` (beads already synced by daemon)

**IMPORTANT**: Daemon auto-syncs beads. You only commit/push your code changes.

## When Adding New Features

1. **Check for beads**: `bd ready` for planned work
2. **Define types first**: Create custom types before logic
3. **Write tests**: Test file in `test/` directory
4. **Implement using pipelines**: Transform data with `|>`
5. **Handle all errors**: Return `Result`, never panic
6. **Run CI pipeline**: `moon run :ci` (includes formatting, building, testing)
7. **Update docs**: Add `///` comments for public functions
8. **Commit changes**: `git add . && git commit -m "message"`
9. **Close bead**: `bd close <id>` when complete
10. **Install binary**: `moon run :install` to update system-wide binary

## AI-Friendly Error Messages

Intent CLI now provides **structured, actionable error messages** designed for AI agents to understand and recover from errors automatically.

### Error Format

All errors follow a consistent CUE structure:

```cue
{
    action: "error_category"
    error: {
        type: "specific_error_type"
        message: "what went wrong"
        context: { field: "value" }
    }
    suggestion: "what to do next"
    recovery: ["step 1", "step 2", "step 3"]
}
```

### Using AI-Friendly Errors

Each error module provides multiple formatters:

```gleam
import intent/loader
import intent/ai_errors

case loader.load_spec("missing.cue") {
  Ok(spec) -> // use spec
  Error(e) -> {
    // For AI agents (CUE format)
    let cue_error = loader.format_error_ai(e)

    // For humans (readable text with recovery steps)
    let text_error = loader.format_error_text(e)

    // Legacy simple message
    let simple = loader.format_error(e)
  }
}
```

### Modules with AI-Friendly Errors

- **`intent/ai_errors`** - Core error builders and formatters
- **`intent/loader`** - CUE loading and validation errors
- **`intent/bead_feedback`** - Bead execution feedback errors
- **`intent/http_client`** - HTTP request execution errors
- **`intent/interview_storage`** - Session storage errors

### Common Error Builders

The `ai_errors` module provides pre-built error constructors:

- `file_not_found(path, expected_location)` - File/directory not found
- `directory_not_found(path)` - Missing directory with mkdir command
- `cue_validation_error(message, file_path)` - CUE syntax errors
- `cue_export_error(message, file_path)` - CUE export failures
- `session_not_found(session_id, sessions_path)` - Invalid session ID
- `bead_not_found(bead_id, session_id)` - Missing bead
- `write_permission_error(path)` - Permission denied
- `http_connection_error(error_message, target_url)` - Network failures
- `interpolation_error(variable, available_vars)` - Variable not captured

### Example: File Not Found

**Before:**
```
Error: File not found: examples/missing-api.cue
```

**After (AI-friendly CUE):**
```cue
{
    action: "file_error"
    error: {
        type: "file_not_found"
        message: "File not found: examples/missing-api.cue"
        context: {
            path: "examples/missing-api.cue"
            expected_location: "CUE specification file"
        }
    }
    suggestion: "Create the missing file or directory"
    recovery: [
        "Check if the parent directory exists",
        "Create directory: mkdir -p examples",
        "Create the file with appropriate content",
        "Verify file permissions allow read/write access"
    ]
}
```

**After (Human-readable):**
```
Error: File not found: examples/missing-api.cue

Context:
  path: examples/missing-api.cue
  expected_location: CUE specification file

Suggestion: Create the missing file or directory

Recovery Steps:
  1. Check if the parent directory exists
  2. Create directory: mkdir -p examples
  3. Create the file with appropriate content
  4. Verify file permissions allow read/write access
```

### Benefits for AI Agents

1. **Self-Recovery** - AI agents can parse recovery steps and attempt automatic fixes
2. **Context Awareness** - Structured context helps understand root causes
3. **Deterministic** - Same error always produces same structured output
4. **Actionable** - Every error includes concrete next steps
5. **Machine-Readable** - CUE format is parseable for automated workflows

**See `docs/ai-friendly-errors.md` for complete examples and usage patterns.**

## Resources

- [Gleam Language Tour](https://tour.gleam.run/)
- [Gleam Standard Library](https://hexdocs.pm/gleam_stdlib/)
- [bd (beads) Documentation](https://github.com/steveyegge/beads)
- [CUE Language](https://cuelang.org/)
- [AI-Friendly Errors Guide](docs/ai-friendly-errors.md)
