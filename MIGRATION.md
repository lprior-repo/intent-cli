# Migration Guide: v2.0 to v3.0

This guide helps you migrate from Intent v2.0 (HTTP testing tool) to v3.0 (Planning and Bead Generation tool).

## Overview of Breaking Changes

Intent v3.0 represents a fundamental shift from **contract-driven HTTP API testing** to **planning and work item generation**. The tool no longer executes HTTP requests or validates API responses. Instead, it focuses on:

- **Interactive requirement capture** through interviews
- **Semantic validation** of specifications
- **Work item generation** for the `br` issue tracker
- **Planning assistance** with next-task suggestions

### What's Removed

- All HTTP execution infrastructure (client, runner, request/response handling)
- Response validation and checking system
- API quality analysis (KIRK modules)
- CUE spec file parsing and validation
- Real-time dependency resolution between behaviors
- Variable interpolation and capture system

### What's Added

- Interactive interview system for requirement gathering
- Bead (work item) template generation
- Integration with `br` issue tracker
- Plan mode with next-task suggestions
- Effects analysis for second-order impacts
- Interview session management and storage

## Conceptual Model Changes

### v2.0 Model: Contract Testing

```
Spec → Parse → Validate → Execute HTTP → Check Response → Report
```

**Focus**: Verify that APIs match their specifications through actual HTTP calls.

### v3.0 Model: Planning & Generation

```
Interview → Capture Requirements → Validate Semantics → Generate Beads → Plan Work
```

**Focus**: Help teams think through requirements systematically and generate actionable work items.

## Type System Changes

### Removed Types

The following types have been completely removed from v3.0:

```gleam
// HTTP Infrastructure
pub type Method { Get | Post | Put | Patch | Delete | Head | Options }
pub type Request { Request(method: Method, path: String, headers: Dict(...), query: Json, body: Json) }
pub type Response { Response(status: Int, example: Json, checks: Dict(...), headers: Dict(...)) }
pub type Config { Config(base_url: String, timeout_ms: Int, headers: Dict(...)) }

// Behavior Testing
pub type Behavior { Behavior(
  name: String,
  intent: String,
  notes: String,
  requires: List(String),
  tags: List(String),
  request: Request,        // REMOVED
  response: Response,      // REMOVED
  captures: Dict(String, String),  // REMOVED
)}

// Validation Rules
pub type Rule { Rule(
  name: String,
  description: String,
  when: When,              // REMOVED
  check: RuleCheck,        // REMOVED
  example: Json,
)}
pub type When { When(status: String, method: Method, path: String) }
pub type RuleCheck { RuleCheck(
  body_must_not_contain: List(String),
  body_must_contain: List(String),
  fields_must_exist: List(String),
  fields_must_not_exist: List(String),
  header_must_exist: String,
  header_must_not_exist: String,
)}
```

### New Concepts

v3.0 introduces new conceptual types (not necessarily Gleam types, but new ideas):

```gleam
// Planning & Work Items
pub type Bead {
  Bead(
    id: String,
    title: String,
    description: String,
    status: String,
    priority: Int,
    blocks: List(String),
    blocked_by: List(String),
  )
}

// Interview Artifacts
pub type InterviewSession {
  InterviewSession(
    id: String,
    profile: String,
    questions_asked: Int,
    rounds_completed: Int,
    responses: Dict(String, String),
  )
}

// Verification (replaces response checking)
pub type Verification {
  Verification(
    criterion: String,
    how_to_check: String,
    automated_tests: List(String),
    manual_checks: List(String),
  )
}

// Invariant (replaces global rules)
pub type Invariant {
  Invariant(
    name: String,
    description: String,
    scope: String,  // "all_behaviors", "specific_features", etc.
    verification: Verification,
  )
}
```

## Spec Structure Changes

### v2.0 Spec Format (HTTP Testing)

```cue
spec: intent.#Spec & {
  name: "User Management API"
  description: "API for managing user accounts"
  audience: "Web and mobile clients"
  version: "1.0.0"
  success_criteria: [
    "Users can register and login",
    "Passwords never exposed in responses",
  ]

  // REMOVED: HTTP execution config
  config: {
    base_url:   "http://localhost:8080"
    timeout_ms: 5000
    headers: {
      "Content-Type": "application/json"
    }
  }

  features: [
    {
      name: "User Registration"
      description: "New user registration flow"
      behaviors: [
        {
          name:   "successful-registration"
          intent: "A new user can create an account"

          // REMOVED: HTTP request definition
          request: {
            method: "POST"
            path:   "/users"
            body: {
              email:    "newuser@example.com"
              password: "SecurePass123!"
            }
          }

          // REMOVED: HTTP response definition
          response: {
            status: 201
            example: {
              id:         "usr_abc123"
              email:      "newuser@example.com"
              created_at: "2024-01-15T10:30:00Z"
            }
            checks: {
              "id": { rule: "string matching usr_[a-z0-9]+", why: "..." }
              "password": { rule: "absent", why: "Security" }
            }
          }

          // REMOVED: Variable capture
          captures: {
            new_user_id: "response.body.id"
          }
        },
      ]
    },
  ]

  // CHANGED: rules → invariants
  rules: [
    {
      name:        "no-passwords-in-responses"
      description: "Never expose passwords"
      when:        { status: ">= 200", method: "*", path: "*" }  // REMOVED
      check: {
        body_must_not_contain: ["password", "pwd", "secret"]  // REMOVED
      }
      example: null
    },
  ]

  anti_patterns: [...]  // KEPT but with different semantics
  ai_hints: {...}       // KEPT
}
```

### v3.0 Spec Format (Planning)

**Note**: v3.0 doesn't use CUE spec files in the same way. Instead, it uses:

1. **Interview sessions** (stored in `.interview/sessions.jsonl`)
2. **Bead templates** (generated work items)
3. **Plan documents** (markdown or JSON)

However, if you're documenting requirements for v3.0, the conceptual format would be:

```yaml
# Conceptual v3.0 Specification (no formal CUE schema)

name: "User Management API"
description: "API for managing user accounts"
audience: "Web and mobile clients"
version: "1.0.0"
success_criteria:
  - "Users can register and login"
  - "Passwords never exposed in responses"

# NO CONFIG: No HTTP execution configuration

features:
  - name: "User Registration"
    description: "New user registration flow"

    # REPLACES: behaviors with request/response
    scenarios:
      - name: "successful-registration"
        intent: "A new user can create an account"

        # NEW: Preconditions (was in request)
        preconditions:
          - "User has valid email"
          - "User chooses strong password"
          - "Email not already registered"

        # NEW: Postconditions (was in response.checks)
        postconditions:
          - criterion: "User account created"
            verification: "Query database for user ID"
          - criterion: "Password stored securely"
            verification: "Check password hash in database"
          - criterion: "Password not in response"
            verification: "Review API response schema"

        # NEW: Acceptance criteria
        acceptance_criteria:
          - "Returns 201 status"
          - "User ID follows format usr_[a-z0-9]+"
          - "Response contains email and created_at"
          - "Response does NOT contain password"

        # REPLACES: captures
        outputs:
          user_id: "For use in subsequent tests"

# REPLACES: rules
invariants:
  - name: "no-passwords-in-responses"
    scope: "all_api_endpoints"
    verification:
      automated_tests:
        - "Schema validation excludes password fields"
      manual_checks:
        - "Code review response builders"
      how_to_check: "Audit all response DTOs"

anti_patterns:
  - name: "plaintext-passwords"
    bad_example: { password: "plaintext123" }
    good_example: { password_hash: "$2a$12$..." }
    why: "Security requirement"

ai_hints:
  implementation:
    suggested_stack: ["Go", "PostgreSQL", "bcrypt"]
  security:
    password_hashing: "bcrypt with cost factor 12"
  pitfalls:
    - "Don't log passwords"
    - "Don't include password in error messages"
```

## Migration Checklist

### Step 1: Identify Affected Specs

- [ ] List all CUE spec files in your project
- [ ] Identify which specs are actively used for testing
- [ ] Document current test coverage from v2.0

### Step 2: Extract Business Logic

For each spec file:

- [ ] Extract `name`, `description`, `audience`, `version` (no changes)
- [ ] Extract `success_criteria` (no changes)
- [ ] Remove `config` section entirely
- [ ] Transform `features` → keep name/description, transform behaviors
- [ ] Transform `behaviors`:
  - [ ] Convert `request` → `preconditions`
  - [ ] Convert `response.checks` → `postconditions` and `acceptance_criteria`
  - [ ] Convert `captures` → `outputs`
  - [ ] Extract intent from `intent` field (no change)
- [ ] Transform `rules` → `invariants`:
  - [ ] Remove `when` conditions
  - [ ] Convert `check` → `verification` with automated/manual checks
- [ ] Keep `anti_patterns` and `ai_hints` (mostly unchanged)

### Step 3: Adopt Interview Workflow

- [ ] Run `intent interview` to capture requirements interactively
- [ ] Choose appropriate profile: `api`, `cli`, `event`, `data`, `workflow`, or `ui`
- [ ] Answer questions through 5-round interview process
- [ ] Review generated bead templates
- [ ] Export beads to `br` issue tracker

### Step 4: Generate Work Items

- [ ] Run `intent plan-emit-beads <session-id> --dry-run` to preview
- [ ] Review generated beads
- [ ] Run `intent plan-emit-beads <session-id> --execute` to create issues
- [ ] Use `br ready --json` to find work
- [ ] Use `br update <id> --status in_progress` to claim work

### Step 5: Replace Test Automation

If you were using v2.0 for automated testing:

- [ ] Consider adopting a dedicated API testing tool (e.g., Postman, REST Assured, Karate)
- [ ] Migrate `request` definitions to your new tool's format
- [ ] Migrate `response.checks` to assertions in your new tool
- [ ] Set up CI/CD integration for the new tool
- [ ] Archive v2.0 test files

### Step 6: Update Documentation

- [ ] Update API documentation to reference new testing approach
- [ ] Document interview workflow for team members
- [ ] Create training materials for `br` integration
- [ ] Update onboarding docs

## Before/After Examples

### Example 1: Simple POST Endpoint

#### v2.0 (HTTP Testing)

```gleam
// src/intent/types.gleam
let behavior = Behavior(
  name: "create-user",
  intent: "Create a new user account",
  notes: "",
  requires: [],
  tags: ["happy-path"],
  request: Request(
    method: Post,
    path: "/users",
    headers: dict.from_list([#("Content-Type", "application/json")]),
    query: dict.new(),
    body: json.object([
      #("email", json.string("user@example.com")),
      #("password", json.string("SecurePass123!")),
    ])
  ),
  response: Response(
    status: 201,
    example: json.object([
      #("id", json.string("usr_abc123")),
      #("email", json.string("user@example.com")),
    ]),
    checks: dict.from_list([
      #("id", Check(rule: "string matching usr_[a-z0-9]+", why: "ID format")),
      #("email", Check(rule: "equals request.body.email", why: "Echo back")),
      #("password", Check(rule: "absent", why: "Security")),
    ]),
    headers: dict.new()
  ),
  captures: dict.from_list([#("user_id", "response.body.id")])
)
```

#### v3.0 (Planning)

```yaml
# Interview session output or bead template

name: "create-user"
intent: "Create a new user account"
tags: ["happy-path", "authentication"]

preconditions:
  - Email address is valid and unique
  - Password meets strength requirements
  - User has accepted terms of service

postconditions:
  - criterion: "User account exists in database"
    verification:
      automated_tests:
        - "SELECT * FROM users WHERE email = ?"
      manual_checks: []
  - criterion: "User ID follows naming convention"
    verification:
      automated_tests:
        - "Assert user_id matches regex usr_[a-z0-9]+"
      manual_checks: []
  - criterion: "Password not exposed in response"
    verification:
      automated_tests:
        - "Schema validation: response.password must not exist"
      manual_checks:
        - "Code review of response builder"

acceptance_criteria:
  - "Returns 201 Created status"
  - "Response includes user ID, email, created_at"
  - "Response does NOT include password"
  - "Password is hashed using bcrypt before storage"

outputs:
  user_id: "Use in subsequent test scenarios"

invariants:
  - scope: "user_management"
    name: "no-password-exposure"
    verification:
      how_to_check: "Audit all response DTOs for password fields"
```

### Example 2: Global Rule Transformation

#### v2.0 (Global Rule)

```gleam
// src/intent/types.gleam
let rule = Rule(
  name: "structured-errors",
  description: "All errors must have code and message",
  when: When(
    status: ">= 400",
    method: Get,  // Applied to all methods in actual implementation
    path: "*"     // Applied to all paths
  ),
  check: RuleCheck(
    body_must_not_contain: [],
    body_must_contain: [],
    fields_must_exist: ["error.code", "error.message"],
    fields_must_not_exist: [],
    header_must_exist: "",
    header_must_not_exist: ""
  ),
  example: json.object([
    #("error", json.object([
      #("code", json.string("VALIDATION_ERROR")),
      #("message", json.string("Invalid input")),
    ]))
  ])
)
```

#### v3.0 (Invariant)

```yaml
# Bead template or documentation

invariants:
  - name: "structured-errors"
    scope: "all_endpoints"
    description: "All errors must have code and message"

    verification:
      automated_tests:
        - "Schema test: All error responses have error.code field"
        - "Schema test: All error responses have error.message field"
        - "Contract test: Error codes from approved enum"
      manual_checks:
        - "Code review: Error response builders"
        - "Documentation: Error code catalog"

      how_to_check: |
        1. Review OpenAPI/Swagger schema for error responses
        2. Run schema validation tests in CI/CD
        3. Audit code for manual error responses

    good_example:
      status: 400
      body:
        error:
          code: "VALIDATION_ERROR"
          message: "Email is required"

    bad_example:
      status: 400
      body:
        error: "Something went wrong"  # Lacks structure
```

### Example 3: Dependency Chain

#### v2.0 (HTTP Testing with Dependencies)

```gleam
let login = Behavior(
  name: "successful-login",
  intent: "User can login with valid credentials",
  requires: ["successful-registration"],  // Must run after registration
  request: Request(
    method: Post,
    path: "/auth/login",
    headers: dict.new(),
    query: dict.new(),
    body: json.object([
      #("email", json.string("user@example.com")),
      #("password", json.string("SecurePass123!")),
    ])
  ),
  response: Response(
    status: 200,
    example: json.object([
      #("token", json.string("eyJhbGciOiJIUzI1NiIs...")),
    ]),
    checks: dict.from_list([
      #("token", Check(rule: "non-empty string", why: "JWT required")),
    ]),
    headers: dict.new()
  ),
  captures: dict.from_list([
    #("auth_token", "response.body.token")  // Used in subsequent requests
  ])
)

let get_profile = Behavior(
  name: "get-user-profile",
  intent: "Get current user profile",
  requires: ["successful-login"],  // Depends on login
  request: Request(
    method: Get,
    path: "/users/me",
    headers: dict.from_list([
      #("Authorization", json.string("Bearer ${auth_token}"))  // Interpolated
    ]),
    query: dict.new(),
    body: json.null
  ),
  // ... response definition
)
```

#### v3.0 (Planning with Bead Dependencies)

```yaml
# Bead 1: User Registration
bead_id: "bd-user-registration"
title: "Implement user registration endpoint"
description: |-
  Create POST /users endpoint for new user registration.

  Preconditions:
  - Database schema includes users table
  - Password hashing library integrated

  Postconditions:
  - User account created in database
  - Password stored as bcrypt hash
  - User ID returned in response

  Acceptance Criteria:
  - 201 status on success
  - 409 status for duplicate email
  - 400 status for validation errors
  - Password never in response

blocks: ["bd-user-login", "bd-user-profile"]
blocked_by: ["bd-database-schema", "bd-password-hashing"]

---

# Bead 2: User Login
bead_id: "bd-user-login"
title: "Implement user authentication endpoint"
description: |-
  Create POST /auth/login endpoint for user authentication.

  Preconditions:
  - User registration working (bd-user-registration)
  - JWT library integrated

  Postconditions:
  - Valid credentials return JWT token
  - Invalid credentials return 401
  - Token expires after 1 hour

  Acceptance Criteria:
  - 200 status with token on success
  - 401 status on invalid credentials
  - Token includes user ID and expiration

blocks: ["bd-user-profile"]
blocked_by: ["bd-user-registration", "bd-jwt-integration"]

---

# Bead 3: User Profile
bead_id: "bd-user-profile"
title: "Implement authenticated profile endpoint"
description: |-
  Create GET /users/me endpoint for current user profile.

  Preconditions:
  - Authentication working (bd-user-login)
  - JWT middleware implemented

  Postconditions:
  - Valid token returns user profile
  - Invalid/expired token returns 401
  - Response includes email, name, created_at

  Acceptance Criteria:
  - 200 status with user profile on valid token
  - 401 status on missing/invalid token
  - 403 status on insufficient permissions

blocks: []
blocked_by: ["bd-user-login", "bd-jwt-middleware"]
```

## CLI Command Changes

### v2.0 Commands (Removed)

```bash
# Run tests against a spec
intent check examples/user-api.cue --target http://localhost:8080

# Watch mode for continuous testing
intent check examples/user-api.cue --watch

# Generate coverage report
intent coverage examples/user-api.cue

# Analyze API quality
intent analyze examples/user-api.cue

# List missing tests
intent gap-analyze examples/user-api.cue
```

### v3.0 Commands (New)

```bash
# Interactive interview to capture requirements
intent interview --profile api

# Generate work items from interview session
intent beads --session abc123 --format json

# Suggest next task to work on
intent plan-next --strategy page_rank

# Emit beads to br issue tracker
intent plan-emit-beads abc123 --dry-run
intent plan-emit-beads abc123 --execute

# Analyze second-order effects
intent effects spec.cue --behavior create-order

# Generate vision document
intent vision --out ./docs

# Generate ready document
intent ready --out ./docs
```

## Migration Strategies

### Strategy A: Parallel Run (Recommended)

1. **Keep v2.0 running** for existing test suites
2. **Adopt v3.0** for new projects and features
3. **Gradually migrate** documentation and workflows
4. **Decommission v2.0** when team is comfortable with v3.0

**Pros**: Minimal disruption, learning time, safety net
**Cons**: Maintaining two tools temporarily

### Strategy B: Big Bang

1. **Stop all v2.0 usage** immediately
2. **Migrate all specs** to v3.0 format
3. **Train team** on interview workflow
4. **Replace test automation** with dedicated testing tool

**Pros**: Clean break, forced adoption
**Cons**: High disruption, steep learning curve

### Strategy C: Hybrid

1. **Use v3.0** for planning and requirements
2. **Keep v2.0** for API testing
3. **Integrate** both into CI/CD pipeline
4. **Evaluate** v2.0 replacement over time

**Pros**: Best of both worlds, gradual transition
**Cons**: More complex toolchain

## Common Migration Patterns

### Pattern 1: From Response Checks to Acceptance Criteria

**Before (v2.0)**:
```gleam
checks: dict.from_list([
  #("status", Check(rule: "equals 200", why: "Success")),
  #("data.id", Check(rule: "non-empty string", why: "Required")),
  #("error", Check(rule: "absent", why: "No errors on success")),
])
```

**After (v3.0)**:
```yaml
acceptance_criteria:
  - "HTTP 200 status code returned"
  - "Response includes non-empty data.id field"
  - "Response does not include error object"
  - "Response time under 200ms"
```

### Pattern 2: From Variable Capture to Bead Outputs

**Before (v2.0)**:
```gleam
captures: dict.from_list([
  #("user_id", "response.body.id"),
  #("auth_token", "response.body.token"),
])
```

**After (v3.0)**:
```yaml
outputs:
  user_id: "Available to dependent beads via database lookup"
  auth_token: "Stored in test context for authentication"

# Or in bead dependencies:
blocks: ["bd-user-profile"]  # Profile endpoint needs user_id
```

### Pattern 3: From Global Rules to Invariants

**Before (v2.0)**:
```gleam
rules: [
  Rule(
    name: "no-internal-ids",
    description: "Don't expose database IDs",
    when: When(status: "*", method: "*", path: "*"),
    check: RuleCheck(
      fields_must_not_exist: ["id", "internal_id", "_id"]
    ),
    example: json.object([#("userId", json.string("usr_123"))])
  )
]
```

**After (v3.0)**:
```yaml
invariants:
  - name: "opaque-ids-only"
    scope: "all_public_apis"
    description: "Never expose internal database IDs"

    verification:
      automated_tests:
        - "Schema validation: No 'id', 'internal_id', '_id' fields in responses"
        - "Regex test: IDs match entity prefix (usr_, ord_, etc.)"
      manual_checks:
        - "Code review: Response DTOs"
        - "API documentation review"

      good_example:
        userId: "usr_abc123"
        orderId: "ord_20240115_001"

      bad_example:
        id: 123
        internal_id: "db_row_456"
```

## Testing Strategy Changes

### v2.0 Testing Approach

- **Automated**: Execute HTTP requests, assert on responses
- **Continuous**: Watch mode for TDD workflow
- **Coverage**: Measure endpoint and scenario coverage
- **Validation**: Real-time feedback on API conformance

### v3.0 Planning Approach

- **Interactive**: Interview to explore requirements
- **Deliberative**: Think through scenarios before coding
- **Traceable**: Beads track work from idea to completion
- **Collaborative**: Team discussions around interview questions

### Recommended Test Automation for v3.0 Users

Since v3.0 doesn't execute HTTP tests, consider these alternatives:

1. **Postman/Newman**: API testing with collections
2. **Karate**: BDD-style API testing
3. **REST Assured**: Java-based API testing
4. **pytest + requests**: Python API testing
5. **tapir + sttp**: Scala API testing

## FAQ

**Q: Why was HTTP testing removed?**

A: Intent's focus shifted from "verify API matches spec" to "help teams think through requirements." The interview-based approach catches issues earlier (during requirements gathering) rather than later (during testing).

**Q: Can I still use my existing CUE spec files?**

A: Not directly. v3.0 doesn't parse CUE spec files. However, you can use your existing specs as reference during interviews, or extract the business logic (name, description, scenarios) into interview answers.

**Q: How do I test my APIs now?**

A: Adopt a dedicated API testing tool (see "Recommended Test Automation" above). Intent v3.0 focuses on planning; actual testing should use specialized tools.

**Q: What happens to my test coverage data?**

A: Coverage metrics are no longer tracked. Instead, v3.0 focuses on requirement completeness through interview questions and bead tracking.

**Q: Can I convert my old specs to beads?**

A: Yes, but it's a manual process. Extract scenarios as beads, convert response checks to acceptance criteria, and use dependencies to define execution order.

**Q: Is there a migration script?**

A: No. The conceptual changes are too significant for automated migration. Manual conversion ensures you rethink requirements rather than mechanically translate.

**Q: What if I need both planning and testing?**

A: Use both tools in parallel: v3.0 for requirements gathering and planning, plus a dedicated API testing tool for validation.

**Q: How does the interview workflow replace spec writing?**

A: Instead of writing a spec, you answer questions in 5 rounds (ubiquitous, event-driven, state-driven, optional, unwanted, complex). The system captures your answers and generates bead templates.

**Q: Can I customize interview questions?**

A: Yes. The interview system uses profile-specific questions. You can extend `src/intent/interview_questions.gleam` to add domain-specific questions.

**Q: How do I track progress without test execution?**

A: Use `br` commands: `br ready` to see available work, `br update` to claim work, `br close` to complete work. Beads track dependencies and blockers.

**Q: What's the learning curve for v3.0?**

A: If you're familiar with v2.0 specs, the concepts (features, scenarios, acceptance criteria) are similar. The main shift is from "define and test" to "interview and plan."

## Getting Help

- **Documentation**: See `CLAUDE.md` and `AGENTS.md` for workflows
- **Examples**: Check `examples/` directory for interview patterns
- **Issues**: Use GitHub issues for bug reports
- **Community**: Check for discussions or Discord (if available)

## Summary

| Aspect | v2.0 | v3.0 |
|--------|------|------|
| **Purpose** | Contract-driven API testing | Planning and work generation |
| **Input** | CUE spec files | Interview sessions |
| **Output** | Test results, coverage | Beads, plans, documents |
| **Execution** | HTTP requests | None (planning only) |
| **Validation** | Response checks | Semantic validation |
| **Dependencies** | Behavior requires list | Bead blocks/blocked_by |
| **Variable Capture** | Response interpolation | Bead outputs |
| **Global Rules** | When conditions + checks | Invariants with verification |
| **Integration** | Direct API calls | br issue tracker |
| **Workflow** | Write spec → Run tests | Interview → Generate beads → Plan work |

---

**Last Updated**: 2025-02-09
**Intent Version**: 0.1.0 (conceptually v3.0 after refactor)
