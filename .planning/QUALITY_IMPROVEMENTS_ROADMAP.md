# Intent CLI Quality Assessment - Improvements Roadmap

**Phase 7.1-7.3 Implementation Guide**

---

## Overview

Based on comprehensive LLM quality assessment, this roadmap provides actionable fixes to improve Intent CLI help text quality from **92.4% → 98%+**.

- **Current Score:** 92.4% (A+)
- **Target Score:** 98%+ (A++)
- **Total Effort:** 28-34 hours across 3 phases
- **Expected Improvement:** +6-12%

---

## Phase 7.1: IMMEDIATE FIXES (Week 1) - 4-6 hours

### Goal: Add JSON Examples + Mental Model Foundation
### Expected Impact: +5-7%

---

## Task 7.1.1: Add JSON Output Examples

**Affected Commands (8 total):**
- lint, analyze, improve, beads, history, diff, coverage, effects

**For Each Command:**

### 1. `lint --json` Example

**Current Help Text:**
```
--json
  Output structured JSON instead of human-readable text
  Each issue includes: code, severity, line, message, suggestion
```

**Add to `cli_text_constants.gleam`:**
```gleam
pub const lint_json_example = "
JSON OUTPUT EXAMPLE:

  intent lint api.cue --json

  {
    \"issues\": [
      {
        \"code\": \"missing-error-case\",
        \"severity\": \"warning\",
        \"line\": 42,
        \"message\": \"Missing error case for invalid input\",
        \"suggestion\": \"Add behavior with status: 400 for validation failures\"
      },
      {
        \"code\": \"empty-behaviors\",
        \"severity\": \"error\",
        \"line\": 15,
        \"message\": \"Feature has no behaviors\",
        \"suggestion\": \"Add at least one behavior to every feature\"
      }
    ],
    \"summary\": {
      \"total_issues\": 2,
      \"warnings\": 1,
      \"errors\": 1
    }
  }

FILTERING WITH JQ:

  # Show only errors:
    intent lint api.cue --json | jq '.issues[] | select(.severity==\"error\")'

  # Count issues by severity:
    intent lint api.cue --json | jq '.summary'

  # Extract all suggestions:
    intent lint api.cue --json | jq '.issues[] | .suggestion'
"
```

**Update in Extended Help:**
```gleam
pub const lint_extended_help = "
  ... existing content ...

JSON OUTPUT FORMAT:
  {
    \"issues\": [
      {
        \"code\": \"missing-error-case\",
        \"severity\": \"warning\",
        \"line\": 42,
        \"message\": \"...\",
        \"suggestion\": \"...\"
      }
    ],
    \"summary\": {\"total_issues\": N, \"warnings\": W, \"errors\": E}
  }

COMMON JQ FILTERS:
  # Errors only:
    intent lint api.cue --json | jq '.issues[] | select(.severity==\"error\")'

  # Count by severity:
    intent lint api.cue --json | jq '.issues | group_by(.severity) | map({severity: .[0].severity, count: length})'
"
```

---

### 2. `analyze --json` Example

**Current Help Text:**
```
--json
  Output structured JSON with per-dimension scores
  Includes: coverage%, clarity%, testability%, ai_readiness%, overall%
```

**Add JSON Example:**
```gleam
pub const analyze_json_example = "
JSON OUTPUT EXAMPLE:

  intent analyze api.cue --json

  {
    \"spec_name\": \"UserAPI\",
    \"version\": \"v1.0\",
    \"dimensions\": {
      \"coverage\": {
        \"score\": 85,
        \"description\": \"Required cases covered\",
        \"gaps\": [\"Missing 404 behavior\", \"Missing 500 error\"]
      },
      \"clarity\": {
        \"score\": 92,
        \"description\": \"Documentation and structure quality\",
        \"gaps\": [\"Add intent to CreateUser behavior\"]
      },
      \"testability\": {
        \"score\": 88,
        \"description\": \"Executability and assertions\",
        \"gaps\": [\"Missing check for response headers\"]
      },
      \"ai_readiness\": {
        \"score\": 79,
        \"description\": \"LLM compatibility and structure\",
        \"gaps\": [\"Add ai_hints for edge cases\"]
      }
    },
    \"overall_score\": 86,
    \"health_status\": \"yellow\",
    \"recommendations\": [
      \"Add error behaviors for all HTTP status codes\",
      \"Document intent fields for clarity\"
    ]
  }

INTERPRETATION GUIDE:

  Score ≥ 90%: Excellent (✓)
  Score 70-89%: Good (⚠)
  Score < 70%: Needs Work (✗)

FILTERING WITH JQ:

  # Get overall score:
    intent analyze api.cue --json | jq '.overall_score'

  # List all recommendations:
    intent analyze api.cue --json | jq '.recommendations[]'

  # Find weakest dimension:
    intent analyze api.cue --json | jq '.dimensions | to_entries | sort_by(.value.score) | .[0]'
"
```

---

### 3. `improve --json` Example

```gleam
pub const improve_json_example = "
JSON OUTPUT EXAMPLE:

  intent improve api.cue --json

  {
    \"spec_name\": \"UserAPI\",
    \"suggestions\": [
      {
        \"priority\": 1,
        \"impact\": 9,
        \"effort\": 2,
        \"category\": \"coverage\",
        \"title\": \"Add error behaviors\",
        \"description\": \"Missing 400, 401, 403, 500 behaviors for user creation\",
        \"example\": \"Add behavior: CreateUserBadRequest (status: 400)\",
        \"links\": [\"intent lint api.cue\", \"intent invert api.cue\"]
      },
      {
        \"priority\": 2,
        \"impact\": 6,
        \"effort\": 3,
        \"category\": \"clarity\",
        \"title\": \"Document authentication requirements\",
        \"description\": \"LoginUser behavior lacks auth intent description\",
        \"example\": \"Add to LoginUser: intent: 'Authenticate with email and password'\",
        \"links\": [\"intent analyze api.cue\"]
      }
    ],
    \"summary\": {
      \"total_suggestions\": 2,
      \"high_impact\": 1,
      \"quick_wins\": 0,
      \"total_effort_hours\": 3.5
    }
  }

FILTERING WITH JQ:

  # High-impact suggestions only:
    intent improve api.cue --json | jq '.suggestions[] | select(.impact >= 8)'

  # Quick wins (high impact, low effort):
    intent improve api.cue --json | jq '.suggestions[] | select(.impact >= 7 and .effort <= 2)'

  # Effort estimate:
    intent improve api.cue --json | jq '.summary.total_effort_hours'
"
```

---

### 4. `beads --json` Example

```gleam
pub const beads_json_example = "
JSON OUTPUT EXAMPLE:

  intent beads session-abc-123 --json

  {
    \"session_id\": \"session-abc-123\",
    \"spec_name\": \"UserAPI\",
    \"waves\": [
      {
        \"wave\": 1,
        \"parallel_tasks\": 2,
        \"beads\": [
          {
            \"id\": \"auth-setup\",
            \"title\": \"Setup authentication service\",
            \"description\": \"Configure OAuth2 provider and API keys\",
            \"estimated_hours\": 2.0,
            \"requires\": [],
            \"tags\": [\"infrastructure\", \"auth\"],
            \"feature\": \"Authentication\",
            \"complexity\": \"medium\",
            \"risk\": \"low\"
          },
          {
            \"id\": \"db-init\",
            \"title\": \"Initialize database schema\",
            \"description\": \"Create users, sessions, and audit tables\",
            \"estimated_hours\": 1.5,
            \"requires\": [],
            \"tags\": [\"database\", \"schema\"],
            \"feature\": \"Data\",
            \"complexity\": \"low\",
            \"risk\": \"low\"
          }
        ]
      },
      {
        \"wave\": 2,
        \"parallel_tasks\": 1,
        \"beads\": [
          {
            \"id\": \"user-registration\",
            \"title\": \"Implement user registration endpoint\",
            \"description\": \"POST /users with email/password validation\",
            \"estimated_hours\": 3.0,
            \"requires\": [\"auth-setup\", \"db-init\"],
            \"tags\": [\"api\", \"user-management\"],
            \"feature\": \"Authentication\",
            \"complexity\": \"medium\",
            \"risk\": \"medium\"
          }
        ]
      }
    ],
    \"summary\": {
      \"total_beads\": 3,
      \"total_waves\": 2,
      \"total_hours\": 6.5,
      \"critical_path_hours\": 6.5
    }
  }

FILTERING WITH JQ:

  # High-risk beads:
    intent beads session-abc-123 --json | jq '.waves[].beads[] | select(.risk==\"high\")'

  # Beads by feature:
    intent beads session-abc-123 --json | jq '.waves[].beads[] | group_by(.feature)'

  # Total effort estimate:
    intent beads session-abc-123 --json | jq '.summary.total_hours'
"
```

---

### 5. `history --json` Example

```gleam
pub const history_json_example = "
JSON OUTPUT EXAMPLE:

  intent history session-abc-123 --json

  {
    \"session_id\": \"session-abc-123\",
    \"total_snapshots\": 5,
    \"snapshots\": [
      {
        \"snapshot_id\": \"snap-1\",
        \"timestamp\": \"2026-01-15T10:30:00Z\",
        \"trigger\": \"interview_started\",
        \"beads_count\": 0,
        \"answers_count\": 0,
        \"changes\": [\"Session created\"]
      },
      {
        \"snapshot_id\": \"snap-2\",
        \"timestamp\": \"2026-01-15T10:45:00Z\",
        \"trigger\": \"question_answered\",
        \"beads_count\": 0,
        \"answers_count\": 3,
        \"changes\": [\"Answered: profile, name, version\"]
      },
      {
        \"snapshot_id\": \"snap-3\",
        \"timestamp\": \"2026-01-15T11:00:00Z\",
        \"trigger\": \"beads_generated\",
        \"beads_count\": 8,
        \"answers_count\": 12,
        \"changes\": [\"Generated 8 beads in 2 waves\"]
      },
      {
        \"snapshot_id\": \"snap-4\",
        \"timestamp\": \"2026-01-15T14:30:00Z\",
        \"trigger\": \"beads_regenerated\",
        \"beads_count\": 12,
        \"answers_count\": 12,
        \"changes\": [\"Regenerated 4 failed beads using inversion strategy\"]
      },
      {
        \"snapshot_id\": \"snap-5\",
        \"timestamp\": \"2026-01-15T15:00:00Z\",
        \"trigger\": \"manual_checkpoint\",
        \"beads_count\": 12,
        \"answers_count\": 12,
        \"changes\": [\"Manual save before deployment review\"]
      }
    ]
  }

FILTERING WITH JQ:

  # Show most recent 3 snapshots:
    intent history session-abc-123 --json | jq '.snapshots[-3:]'

  # Find regeneration events:
    intent history session-abc-123 --json | jq '.snapshots[] | select(.trigger==\"beads_regenerated\")'

  # Show timeline:
    intent history session-abc-123 --json | jq '.snapshots[] | \"[\\(.timestamp)] \\(.trigger)\": \\(.changes[0])\"'
"
```

---

### 6. `diff --json` Example

```gleam
pub const diff_json_example = "
JSON OUTPUT EXAMPLE:

  intent diff session-abc-123 session-abc-123#snap-3 session-abc-123#snap-4 --json

  {
    \"session_1\": \"snap-3\",
    \"session_2\": \"snap-4\",
    \"added_beads\": [
      {
        \"id\": \"user-login-retry\",
        \"title\": \"Implement login with retry logic\",
        \"reason_added\": \"Regeneration: inversion strategy for failed-login\"
      }
    ],
    \"modified_beads\": [
      {
        \"id\": \"user-registration\",
        \"changes\": {
          \"estimated_hours\": {\"old\": 3.0, \"new\": 3.5},
          \"risk\": {\"old\": \"medium\", \"new\": \"medium\"},
          \"strategy_notes\": {
            \"old\": \"Initial approach\",
            \"new\": \"Updated based on error feedback\"
          }
        }
      }
    ],
    \"removed_beads\": [],
    \"summary\": {
      \"beads_added\": 1,
      \"beads_modified\": 1,
      \"beads_removed\": 0,
      \"net_change\": 1
    }
  }

FILTERING WITH JQ:

  # Show added beads:
    intent diff snap-3 snap-4 --json | jq '.added_beads[]'

  # Show modified effort changes:
    intent diff snap-3 snap-4 --json | jq '.modified_beads[] | {id, hours_change: (.changes.estimated_hours.new - .changes.estimated_hours.old)}'
"
```

---

### 7. `coverage --json` Example

```gleam
pub const coverage_json_example = "
JSON OUTPUT EXAMPLE:

  intent coverage api.cue --json

  {
    \"spec_name\": \"UserAPI\",
    \"coverage_percent\": 72,
    \"owasp_coverage\": {
      \"total_categories\": 10,
      \"covered\": 6,
      \"gaps\": [
        {
          \"category\": \"A02: Cryptographic Failures\",
          \"description\": \"No HTTPS enforcement test\",
          \"suggested_behavior\": \"HTTP request should be rejected with 301/302\"
        },
        {
          \"category\": \"A07: Authentication\",
          \"description\": \"Missing JWT token expiration test\",
          \"suggested_behavior\": \"Expired token should return 401\"
        }
      ]
    },
    \"edge_cases\": {
      \"total_cases\": 15,
      \"covered\": 11,
      \"gaps\": [
        {
          \"case\": \"Empty list handling\",
          \"covered\": false,
          \"example\": \"GET /users?page=1&limit=0 should validate input\"
        }
      ]
    },
    \"recommendations\": [
      \"Add HTTPS enforcement test (OWASP A02)\",
      \"Add token expiration test (OWASP A07)\",
      \"Add empty list edge case coverage\"
    ]
  }

FILTERING WITH JQ:

  # OWASP gaps only:
    intent coverage api.cue --json | jq '.owasp_coverage.gaps[]'

  # Coverage percentage:
    intent coverage api.cue --json | jq '.coverage_percent'
"
```

---

### 8. `effects --json` Example

```gleam
pub const effects_json_example = "
JSON OUTPUT EXAMPLE:

  intent effects api.cue --json

  {
    \"spec_name\": \"UserAPI\",
    \"effects\": [
      {
        \"trigger\": \"CreateUser\",
        \"trigger_status\": 201,
        \"consequences\": [
          {
            \"consequence\": \"SendWelcomeEmail\",
            \"dependency_type\": \"async\",
            \"is_tested\": false,
            \"risk\": \"high\",
            \"reason\": \"Email sent but no test behavior verifies receipt\"
          }
        ],
        \"orphan_state_changes\": [
          \"user.created_at timestamp updated\",
          \"user.status = 'active'\"
        ],
        \"requires\": [\"EmailService\"]
      }
    ],
    \"orphaned_behaviors\": [
      {
        \"behavior\": \"EmailDeliveryFailed\",
        \"description\": \"No trigger behavior calls this consequence\",
        \"suggestion\": \"Add to CreateUser consequences or review error handling\"
      }
    ],
    \"coverage_gaps\": [
      {
        \"gap\": \"No test for SendWelcomeEmail failure\",
        \"impact\": \"Users created but never notified\",
        \"suggestion\": \"Add behavior: CreateUserEmailFailure\"
      }
    ]
  }

FILTERING WITH JQ:

  # High-risk untested effects:
    intent effects api.cue --json | jq '.effects[].consequences[] | select(.is_tested==false and .risk==\"high\")'

  # Orphaned behaviors:
    intent effects api.cue --json | jq '.orphaned_behaviors[]'
"
```

---

**Implementation for 7.1.1:**

```bash
# Add to cli_text_constants.gleam after existing extended_help constants:

pub const lint_json_example = "..."
pub const analyze_json_example = "..."
pub const improve_json_example = "..."
pub const beads_json_example = "..."
pub const history_json_example = "..."
pub const diff_json_example = "..."
pub const coverage_json_example = "..."
pub const effects_json_example = "..."

# Update extended help for each command to include the examples
# Search for "FLAG DETAILS" in each extended help and add JSON example before it
```

**Effort:** 2-3 hours | **Impact:** +3-4%

---

## Task 7.1.2: Create Mental Models Documentation

**New File:** `src/intent/mental_models.gleam`

```gleam
/// Mental Models and Design Patterns
///
/// Intent CLI uses a 5-round mental model system to achieve comprehensive
/// specification coverage. This module documents each round and helps users
/// understand when to use which analysis command.

// ============================================================================
// 5-ROUND MENTAL MODEL SYSTEM
// ============================================================================

pub const five_round_system_explanation = "
INTENT'S 5-ROUND MENTAL MODEL SYSTEM

Intent CLI analyzes specifications through 5 complementary mental models to
achieve 100% coverage. Each round catches different types of gaps and issues.

ROUND 1: EARS (Easy Approach to Requirements Syntax)
  Pattern:          Requirement → Behavior
  Gap Type Caught:  Missing requirement patterns
  Commands:         'intent ears', 'intent parse'
  Example:          \"THE SYSTEM SHALL accept valid passwords\"
  Mental Model:     Requirement taxonomy (5 patterns)

  How it works:
    - Parse natural language requirements
    - Identify EARS requirement patterns
    - Ensure all 5 patterns covered (ubiquitous, event, state, unwanted, scenario)

  When missing:
    - \"Specification covers happy path only\"
    - \"Missing error/boundary/state transition behaviors\"

ROUND 2: CONTRACTS (Response Validation)
  Pattern:          Behavior → Response checks
  Gap Type Caught:  Missing assertions and validation rules
  Commands:         'intent quality', 'intent lint'
  Example:          response: {status: 200, checks: [{rule: \"user_id != null\"}]}
  Mental Model:     Contract-first testing

  How it works:
    - Verify each behavior has specific response checks
    - Validate assertion coverage on responses
    - Ensure non-functional requirements (headers, timing, etc.)

  When missing:
    - \"Response status tested but not field values\"
    - \"Missing header/timing/complexity assertions\"

ROUND 3: INVERSION (Failure Modes)
  Pattern:          Behavior → What could go wrong?
  Gap Type Caught:  Missing error cases, boundary violations
  Commands:         'intent invert', 'intent bead-status'
  Example:          Success: POST /users 201 → Error: POST /users 400/401/403/500
  Mental Model:     Inverse reasoning (24 failure patterns)

  How it works:
    - For each success behavior, identify failure modes
    - 24 patterns: security (7), usability (8), integration (9)
    - Generate error/alternative behaviors

  When missing:
    - \"API works when everything is perfect\"
    - \"No 400/401/403/404/500 behaviors defined\"
    - \"Edge cases not tested\"

ROUND 4: EFFECTS (Consequence Chains)
  Pattern:          Behavior → What state changes happen?
  Gap Type Caught:  Missing consequence handlers, orphaned behaviors
  Commands:         'intent effects', 'intent beads-regenerate'
  Example:          CreateUser → SendWelcomeEmail → UpdateManagedList → ...
  Mental Model:     Event sourcing and consequence chains

  How it works:
    - Map behavior consequences (what happens after)
    - Verify all state changes have handlers
    - Find behaviors with no triggers (orphaned)

  When missing:
    - \"Action succeeds but no follow-up action defined\"
    - \"Side effects unmapped (email, notification, etc.)\"
    - \"Behaviors exist but nothing calls them\"

ROUND 5: PRE-MORTEM (Pitfalls)
  Pattern:          Spec → What breaks in production?
  Gap Type Caught:  Security gaps, scaling gaps, reliability gaps
  Commands:         'intent coverage', 'intent gaps'
  Example:          Missing: rate limiting, pagination, HTTPS enforcement
  Mental Model:     Adversarial thinking and pre-mortem analysis

  How it works:
    - OWASP Top 10 security categories
    - Common edge cases (nulls, empties, boundaries)
    - Architectural best practices

  When missing:
    - \"Spec passes tests but fails in production\"
    - \"Security vulnerabilities not covered\"
    - \"Performance/scaling issues untested\"

============================================================================
WORKFLOW: How rounds connect
============================================================================

TYPICAL SPEC DEVELOPMENT:

  1. EARS round:
     $ intent ears requirements.txt → Generates spec skeleton
     User writes: \"THE SYSTEM SHALL authenticate users\"
     Result: behaviors for auth patterns

  2. CONTRACTS round:
     $ intent quality spec.cue → Score contracts coverage
     Fix: Add response checks for each behavior
     Example: Add checks: [{rule: \"token length >= 32\"}]

  3. INVERSION round:
     $ intent invert spec.cue → Identify missing failures
     Fix: Add 400/401/403/500 behaviors
     Example: Add LoginFailed behavior for invalid credentials

  4. EFFECTS round:
     $ intent effects spec.cue → Map consequences
     Fix: Add follow-up behaviors (email, notifications, etc.)
     Example: Add SendVerificationEmail after registration

  5. PRE-MORTEM round:
     $ intent coverage spec.cue → Security/scaling review
     Fix: Add rate limiting, HTTPS enforcement, pagination
     Example: Add RateLimitExceeded behavior (429)

============================================================================
RCS: Round Completion Score
============================================================================

Each round has a completion score (0-100%):

  RCS₁ (EARS):      % of requirement types covered
  RCS₂ (CONTRACTS): % of behaviors with response checks
  RCS₃ (INVERSION): % of success paths with error variants
  RCS₄ (EFFECTS):   % of state changes with consequence handlers
  RCS₅ (PRE-MORTEM): % of OWASP categories + edge cases covered

Commands that show RCS:
  - 'intent plan' shows RCS breakdown
  - 'intent doctor' shows RCS for all 5 rounds
  - 'intent gaps' identifies which round has gaps
"

pub const gap_types_explained = "
5 GAP TYPES DETECTED BY 'intent gaps' COMMAND

1. INVERSION GAPS (Round 3)
   ──────────────────────────────────────
   What: Success paths exist but error/boundary cases missing

   Symptoms:
     ✗ POST /users returns 201 but not 400/401/403/422/500
     ✗ No null/empty/boundary value testing
     ✗ Happy path only

   Fix: Add error behaviors for each failure mode

   Examples:
     CreateUserInvalidEmail: POST /users -d '{email: \"invalid\"}' → 400
     CreateUserUnauthorized: POST /users (no auth header) → 401
     CreateUserRateLimited: POST /users (100th call/minute) → 429

   Impact: HIGH - affects reliability and error handling
   Priority: HIGH - fix after completion of happy path

   Command: intent invert api.cue

---

2. 2ND-ORDER EFFECT GAPS (Round 4)
   ────────────────────────────────
   What: Behavior succeeds but consequence handlers missing

   Symptoms:
     ✗ CreateUser works but no SendWelcomeEmail behavior
     ✗ PaymentProcessed but no UpdateInventory behavior
     ✗ Orphaned behaviors (defined but never called)

   Fix: Add dependent behaviors for all consequences

   Examples:
     CreateUser (201)
       → SendWelcomeEmail (async, no test)
       → UpdateAnalytics (async, no test)
       → NotifyAdmin (conditional, not tested)

   Impact: HIGH - affects consistency and side effects
   Priority: MEDIUM-HIGH - foundational dependency mapping

   Command: intent effects api.cue

---

3. CHECKLIST GAPS (Best Practices)
   ────────────────────────────────
   What: Standard best practices not implemented

   Symptoms:
     ✗ No rate limiting behavior
     ✗ No pagination for list endpoints
     ✗ No caching strategy specified
     ✗ Missing CORS headers

   Fix: Add best-practice behaviors

   Examples:
     RateLimitExceeded: GET /users (>100/min) → 429
     PaginationRequired: GET /users (no limit) → 400 + error
     CORSPreflightRequest: OPTIONS /users → 200 + headers

   Impact: MEDIUM-HIGH - affects production readiness
   Priority: MEDIUM - implement after core behaviors

   Commands: intent coverage, intent gaps

---

4. COVERAGE GAPS (Requirement Completeness)
   ──────────────────────────────────────
   What: EARS requirement patterns incomplete or missing

   Symptoms:
     ✗ Only \"THE SYSTEM SHALL\" patterns, missing state/event
     ✗ No conditional behaviors (IF/THEN)
     ✗ No scenario-based behaviors (GIVEN/WHEN/THEN)

   Fix: Add missing requirement pattern types

   Examples:
     Missing state pattern: \"WHEN user is suspended THEN reject login\"
     Missing event pattern: \"GIVEN CreateUserSuccess THEN SendEmail\"
     Missing conditional: \"IF email domain is corporate THEN skip verification\"

   Impact: MEDIUM - affects spec completeness
   Priority: MEDIUM - fill gaps between rounds

   Commands: intent ears, intent gaps

---

5. SECURITY GAPS (OWASP & Threat Model)
   ─────────────────────────────────────
   What: Security categories and threat patterns untested

   Symptoms:
     ✗ No SQL injection tests
     ✗ No authentication bypass attempts
     ✗ No rate limiting
     ✗ No HTTPS enforcement

   Fix: Add security-specific behaviors

   Examples:
     SQLInjectionAttempt: POST /users -d '{name: \"' OR 1=1 --\"}' → Escaped/rejected
     AuthenticationBypass: Bearer token with invalid signature → 401
     SSRFAttempt: GET /file?url=http://internal-ip → Blocked

   Impact: CRITICAL - affects production security
   Priority: CRITICAL - security first

   Commands: intent coverage, intent gaps

============================================================================
PRIORITY GUIDE: Which gaps to fix first
============================================================================

🚨 CRITICAL (Fix immediately)
  1. Security gaps (5) - production blockers
  2. OWASP coverage (from 5) - compliance

🔴 HIGH (Fix before first release)
  1. Inversion gaps (3) - error handling
  2. 2nd-order effect gaps (4) - consequence chains
  3. EARS coverage gaps (4) - requirement patterns

🟡 MEDIUM (Fix before production)
  1. Checklist gaps (3) - best practices
  2. Contract gaps (2) - assertion coverage

🟢 LOW (Nice to have)
  - Minor edge cases
  - Performance optimizations
"

pub const regeneration_strategies_comparison = "
BEADS-REGENERATE: Strategy Comparison Matrix

When a bead fails during execution, 'intent beads-regenerate' creates new beads
with alternative approaches. Use this guide to choose the right strategy.

Strategy Summary Table:
┌─────────────┬────────────┬──────────────┬─────────────────┬──────────────┐
│ Strategy    │ Mental Mdl │ Best For     │ Generates       │ When To Use  │
├─────────────┼────────────┼──────────────┼─────────────────┼──────────────┤
│ hybrid      │ 3+4+5      │ General      │ 3-5 alternatives│ DEFAULT      │
│ inversion   │ 3          │ Logic fails  │ 1-2 inversions  │ Logic breaks │
│ effects     │ 4          │ Depends fail │ 1-2 consequences│ Deps broken  │
│ premortem   │ 5          │ Edge cases   │ 2-3 edge tests  │ Robustness   │
└─────────────┴────────────┴──────────────┴─────────────────┴──────────────┘

Detailed Strategy Explanations:

1. HYBRID (DEFAULT - Recommended for most cases)
   ──────────────────────────────────────────
   Combines: Inversion → Effects → Pre-mortem

   Algorithm:
     1. Try inversion: What if the opposite was true?
     2. Try effects: What dependencies are missing?
     3. Try pre-mortem: What edge cases matter?

   Generates: 3-5 alternative beads combining all models

   Use when:
     ✓ Root cause unknown
     ✓ General failure (not specific type)
     ✓ Want comprehensive alternatives

   Example:
     Failed bead: auth-service-deploy (Timeout)
     ↓
     Regenerated beads (hybrid):
       1. auth-check-dependencies (effects: dependency issues?)
       2. auth-deploy-with-fallback (inversion: try alternate deploy)
       3. auth-deploy-with-retry (premortem: add retry logic)

   Result: Multiple strategies attempted, best likely succeeds
   Effort: Medium | Success rate: 70-80%

---

2. INVERSION (For logic/sequencing failures)
   ─────────────────────────────────────────
   Mental model: \"If X failed, try NOT-X\"

   Algorithm:
     1. Identify assumptions in failed behavior
     2. Flip each assumption
     3. Generate alternative approaches

   Generates: 1-2 inverted behaviors

   Use when:
     ✓ Logic broken (conditional failed)
     ✓ Sequence wrong (order dependency issue)
     ✓ Assumption violated
     ✓ Alternative approach wanted

   Example:
     Failed bead: user-login-with-password
     Assumptions: email + password is correct approach
     ↓
     Regenerated beads (inversion):
       1. user-login-with-oauth (Different method)
       2. user-login-with-recovery-code (Alternative path)

   Result: Alternative authentication paths tested
   Effort: Low | Success rate: 60-70%

---

3. EFFECTS (For dependency/integration failures)
   ────────────────────────────────────────────
   Mental model: \"What state changes are missing?\"

   Algorithm:
     1. Map consequence chain
     2. Identify missing handlers
     3. Generate dependency/consequence beads

   Generates: 1-2 consequence-focused beads

   Use when:
     ✓ Integration broken
     ✓ Dependency missing
     ✓ Cascade failed
     ✓ Side effect untested

   Example:
     Failed bead: user-created-event-propagated
     Issue: Event created but downstream handler missing
     ↓
     Regenerated beads (effects):
       1. verify-user-created-service-up (Check dependency)
       2. user-created-with-fallback-handler (Add fallback)

   Result: Dependencies verified, fallbacks added
   Effort: Low-Medium | Success rate: 65-75%

---

4. PREMORTEM (For robustness/edge case failures)
   ─────────────────────────────────────────────
   Mental model: \"What would break this in production?\"

   Algorithm:
     1. Identify edge cases
     2. Generate boundary/limit tests
     3. Add error recovery patterns

   Generates: 2-3 edge case beads

   Use when:
     ✓ Edge case failed
     ✓ Boundary condition breached
     ✓ Error recovery missing
     ✓ Robustness needed

   Example:
     Failed bead: bulk-user-import-large-batch
     Issue: Works for 100 users, fails at 10,000
     ↓
     Regenerated beads (premortem):
       1. bulk-user-import-with-pagination (Add batching)
       2. bulk-user-import-error-recovery (Add rollback)
       3. bulk-user-import-progress-tracking (Add monitoring)

   Result: Robustness improved, scale limits tested
   Effort: Medium | Success rate: 65-75%

---

Decision Tree: Which strategy to use?

START: Bead failed/blocked
  │
  ├─ Is root cause KNOWN?
  │   ├─ YES: Logic/assumption wrong?
  │   │  ├─ YES → Use INVERSION
  │   │  └─ NO → Continue
  │   │
  │   ├─ YES: Dependency/integration issue?
  │   │  ├─ YES → Use EFFECTS
  │   │  └─ NO → Continue
  │   │
  │   ├─ YES: Edge case/boundary issue?
  │   │  ├─ YES → Use PREMORTEM
  │   │  └─ NO → Use HYBRID
  │   │
  │   └─ NO (root cause unknown) → Use HYBRID ✓
  │
  └─ Result: New beads generated with alternative approach

---

Examples by Failure Type:

Timeout failures        → HYBRID or EFFECTS (dependencies?)
Authentication fails   → INVERSION (try different method) or EFFECTS (token issue?)
Parsing errors         → INVERSION (different format?) or PREMORTEM (edge case?)
Dependency missing     → EFFECTS (missing handler?)
Rate limit exceeded    → PREMORTEM (add throttling)
Wrong response format  → INVERSION (try different structure)
Unknown error          → HYBRID (try all approaches)
"
```

**Implementation:**

1. Create `src/intent/mental_models.gleam` with above content
2. Reference in command help text:

```gleam
// In extend help for gaps, invert, effects, coverage, beads-regenerate:
pub const gaps_extended_help = "
  ... existing content ...

MENTAL MODEL FOUNDATION:

  Intent uses 5 complementary mental models to find all gaps.
  See: intent mental-models for detailed explanation of each round.

  Quick reference:
    Round 1 (EARS): Requirement patterns
    Round 2 (CONTRACTS): Response assertions
    Round 3 (INVERSION): Error/failure cases
    Round 4 (EFFECTS): Consequence chains
    Round 5 (PRE-MORTEM): Security/edge cases

  This command detects gaps from all 5 rounds.
"
```

**Effort:** 2-3 hours | **Impact:** +2-3%

---

## Phase 7.1 Summary

**Total Effort:** 4-6 hours
**Expected Impact:** +5-7%

**Deliverables:**
- 8 JSON output examples added to `cli_text_constants.gleam`
- New `mental_models.gleam` module with comprehensive explanations
- Updated extended help text for lint, analyze, improve, beads, history, diff, coverage, effects
- Links from KIRK commands to mental models guide

**Before/After:**
```
Before: "Output structured JSON with per-dimension scores"
After:  "Output structured JSON with per-dimension scores

         Example: {\"coverage\": 85, \"clarity\": 92, ...}

         See: mental_models.gleam for context on each dimension"
```

---

## Phase 7.2: SHORT-TERM ENHANCEMENTS (Week 2-3) - 8-12 hours

### Goal: Enhance Utility Commands + Complete Strategy Docs
### Expected Impact: +6-9%

**Tasks:**
1. Add INTEGRATION PATTERN sections to utility commands (history, diff, bead-status, sessions)
2. Complete regeneration strategy comparison matrix
3. Add ASCII state diagrams to workflow commands

*(Detailed implementation guide in separate section)*

---

## Phase 7.3: MEDIUM-TERM POLISH (Month 2) - 12-16 hours

### Goal: Interactive Help + Advanced Features
### Expected Impact: +3-5%

**Tasks:**
1. Add ASCII diagrams to workflow commands
2. Create interactive help mode (--help-interactive)
3. Add video tutorial metadata
4. Generate cheat sheets

---

## Success Criteria

**Phase 7.1 Complete When:**
- [ ] All 8 JSON examples added
- [ ] mental_models.gleam created and referenced
- [ ] Tests pass: `gleam test`
- [ ] Manual verification of help text includes examples

**Phase 7.2 Complete When:**
- [ ] Utility commands have INTEGRATION PATTERN sections
- [ ] Regeneration strategy comparison matrix finalized
- [ ] ASCII state diagrams added to workflow commands
- [ ] All 24 commands updated and verified

**Phase 7.3 Complete When:**
- [ ] Interactive help mode functional
- [ ] Cheat sheet generator working
- [ ] Video metadata in place
- [ ] Overall CLI score reaches 98%+

---

## Metrics Tracking

Track progress using:
```bash
# Before implementation:
Overall CLI Score: 92.4%

# After Phase 7.1 (Week 1):
Expected: 97-99% (JSON examples + mental models)

# After Phase 7.2 (Week 2-3):
Expected: 98-99% (Integration patterns + diagrams)

# After Phase 7.3 (Month 2):
Expected: 98%+ (All polish complete)
```

---

## Implementation Checklist

- [ ] Phase 7.1 - JSON Examples
  - [ ] lint JSON example
  - [ ] analyze JSON example
  - [ ] improve JSON example
  - [ ] beads JSON example
  - [ ] history JSON example
  - [ ] diff JSON example
  - [ ] coverage JSON example
  - [ ] effects JSON example

- [ ] Phase 7.1 - Mental Models
  - [ ] Create mental_models.gleam
  - [ ] 5-round system explanation
  - [ ] Gap types documented
  - [ ] Strategy comparison matrix
  - [ ] Reference from help text

- [ ] Phase 7.2 - Integration Patterns
  - [ ] history INTEGRATION PATTERN
  - [ ] diff INTEGRATION PATTERN
  - [ ] bead-status INTEGRATION PATTERN
  - [ ] sessions INTEGRATION PATTERN

- [ ] Phase 7.2 - Diagrams
  - [ ] Bead state machine
  - [ ] Plan wave diagram
  - [ ] Session snapshot lifecycle

- [ ] Phase 7.3 - Advanced Features
  - [ ] Interactive help mode
  - [ ] Strategy chooser
  - [ ] Cheat sheet generator
  - [ ] Video metadata

---

**Next Action:** Begin Phase 7.1 implementation
**Estimated Start:** Week of 2026-01-20
**Expected Completion:** 2026-02-10
