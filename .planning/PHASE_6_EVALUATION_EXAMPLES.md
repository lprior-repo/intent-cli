# PHASE 6: LLM Evaluation Examples & Reference

Complete evaluation examples demonstrating the framework from `PHASE_6_LLM_EVALUATION_FRAMEWORK.md`.

---

## Example 1: Excellent Command (90+) - `check`

### Current Help Text

```
WHAT IT DOES
  Execute all behaviors defined in a spec against a target HTTP API, verifying
  that responses match expected status codes, headers, and validation rules.

WHY YOU'D USE IT
  During development and testing to confirm your API implementation matches the
  contract-driven specification. Catch behavioral regressions early and ensure
  consistency across features.

WHEN TO USE IT
  After implementing features, before committing to version control, or as part
  of CI/CD pipeline validation. Use with --target to point to dev/staging/prod
  APIs for environment-specific testing.

PREREQUISITES
  - A valid Intent CUE spec file (validate with: intent validate spec.cue)
  - Target API running and accessible at the specified --target URL
  - Network access from the current machine to the target

USAGE EXAMPLES

  Simple execution against localhost:
    intent check api.cue --target http://localhost:8080 --allow-localhost

  Test production API with JSON output:
    intent check api.cue --target https://api.example.com --json

  Run specific feature:
    intent check api.cue --target http://localhost:8080 --feature Authentication

  Run specific behavior:
    intent check api.cue --target http://localhost:8080 --only "Create User"

  Verbose output for debugging:
    intent check api.cue --target http://localhost:8080 --verbose

COMMON ISSUES

  Target URL required:
    Error: "Target URL required"
    Fix: Use --target or set INTENT_TARGET environment variable

  Localhost blocked by SSRF protection:
    Error: "Connection to localhost denied"
    Fix: Use --allow-localhost flag to bypass SSRF checks in dev

  All checks fail:
    Error: Multiple behavior failures reported
    Fix: Run 'intent doctor api.cue' for diagnostic analysis

  Connection timeout:
    Error: "Request timeout after 30000ms"
    Fix: Check API is running, increase INTENT_TIMEOUT_MS, or check firewall

FLAGS
  --target URL         Target base URL to test against (required) [env: INTENT_TARGET]
  --json               Output results as JSON (optional)
  --feature NAME       Filter to specific feature (optional)
  --only BEHAVIOR      Run only specific behavior (optional)
  --verbose            Enable verbose diagnostic output (optional)
  --quiet              Suppress non-error output (optional)
  --allow-localhost    Allow localhost URLs, bypassing SSRF protection (optional)

SEE ALSO
  intent validate      Validate CUE spec syntax before running tests
  intent doctor        Analyze failures and get health report
  intent lint          Find anti-patterns in spec
  intent improve       Get improvement suggestions
```

### AI-Friendliness Evaluation (88/100)

**Structural Clarity (36/40):**
- ✓ Sections clearly delimited: WHAT/WHY/WHEN/EXAMPLES all present (10/10)
- ✓ Consistent header format: All uppercase with colon pattern (8/8)
- ✓ Structured lists: Code blocks with ` ``` `, bullet lists (8/8)
- ✓ Code blocks properly marked: All examples in markdown (8/8)
- ⚠ Technical terms defined: "behavior", "check" mostly implied (2/6) - *Improvement: Define in WHAT IT DOES*

**Actionability (29/30):**
- ✓ Examples copy-paste ready: All show exact command format (10/10)
- ✓ Flags explained with values: --target URL, --feature NAME shown (8/8)
- ✓ Error scenarios documented: 4 error cases with recovery (7/7)
- ⚠ Expected output described: Result interpretation not explicit (4/5) - *Improvement: Add "Output Format" section*

**Training Suitability (23/30):**
- ⚠ Mental model: Not explicit - implicit from WHAT/WHY (6/10) - *Improvement: Add "MENTAL MODEL" section*
  - Extracted: "Execute each behavior in sequence, validate responses against checks"
- ✓ Workflow integration: Shown in "WHEN TO USE IT" (validate → check → doctor) (8/8)
- ✓ Failure modes: 4 documented scenarios (7/7)
- ✓ Related commands: Listed in SEE ALSO (2/5) - *Note: Could add more context (lint, improve)*

**Overall AI-Friendliness: 88/100** ✓ Excellent for LLM understanding

---

### Usability Evaluation (92/100)

**Clarity (28/30):**
- Plain English, no jargon (except "spec", "behavior", "checks" which are defined)
- Grade level: ~7th grade (appropriate for technical audience)
- ⚠ "SSRF protection" unexplained (1pt deduction)

**Completeness (29/30):**
- All required information present: what/why/when/examples/errors
- Flags comprehensively documented
- ⚠ Output format section missing (1pt deduction)

**Actionability (19/20):**
- Quick-start very clear: "intent check api.cue --target URL"
- 5 examples covering common scenarios
- ⚠ Edge case missing: "quiet mode for CI" implicit but not shown (1pt deduction)

**Examples (18/20):**
- 5 examples provided covering:
  - ✓ Localhost (dev)
  - ✓ Production with JSON (tooling)
  - ✓ Feature filtering
  - ✓ Single behavior
  - ✓ Verbose debugging
- ⚠ Missing: Batch/multiple features example

**Overall Usability: 92/100** ✓ Excellent for human users

---

### Consistency Evaluation (85/100)

**Terminology Consistency:**
- Uses "spec" consistently (not "specification", "config")
- Uses "feature" consistently (not "endpoint", "operation")
- Uses "behavior" consistently (not "scenario", "test")
- Uses "check" consistently (not "validation rule", "assertion")
- ✓ All aligned with CLAUDE.md definitions

**Formatting Consistency:**
- Section headers: Uppercase with colon ✓
- Code blocks: ` ``` ` delimiters ✓
- Bullet lists: `-` prefix ✓
- Emphasis: None used (but could use `code` for literals) - *Improvement*

**Tone Consistency:**
- Imperative throughout: "Execute", "Run", "Use" ✓
- Explanatory in WHY/WHEN sections ✓
- No preachy language ("should", "must") ✓

**Structure Consistency:**
- Follows WHAT/WHY/WHEN/EXAMPLES/FLAGS/SEE ALSO pattern ✓
- Matches other leading commands (lint, analyze) ✓

**Consistency Score: 85/100** ⚠ Good but minor formatting improvements possible

---

### Coverage Evaluation (82/100)

**Flag Coverage (7/7 = 10 points):**
- ✓ --target documented with requirement marker
- ✓ --json documented
- ✓ --feature documented
- ✓ --only documented
- ✓ --verbose documented
- ✓ --quiet documented
- ✓ --allow-localhost documented with security context

**Scenario Coverage (28 points available, score 23):**
- ✓ Happy path: basic execution with target
- ✓ CI/tooling path: --json output
- ✓ Feature-level filtering: --feature
- ✓ Behavior-level filtering: --only
- ✓ Debugging path: --verbose
- ✓ Quiet mode path: --quiet (listed but no example)
- ⚠ Environment variable path documented in FLAGS but not with example (2pt deduction)
- ⚠ Performance/timeout scenario documented in errors but not in examples (2pt deduction)

**Integration Coverage (15 points available, score 12):**
- ✓ Dependency shown: "After implementing features" (timing)
- ✓ Sequence shown: validate → check → doctor (SEE ALSO)
- ✓ Parallel tools mentioned: lint, improve
- ⚠ Config/environment setup not shown in workflow (1pt deduction)
- ⚠ Interview tool not mentioned as pre-check alternative (2pt deduction)

**Edge Case Coverage (20 points available, score 14):**
- ✓ Empty results path (in doctor reference)
- ✓ Timeout handling (COMMON ISSUES)
- ✓ SSRF/localhost (COMMON ISSUES)
- ✓ Feature/behavior not found (implicit in "all checks fail")
- ✓ Network issues (firewall mentioned)
- ⚠ Rate limiting not mentioned (2pt deduction)
- ⚠ Large payload handling not mentioned (2pt deduction)
- ⚠ Certificate validation/TLS errors not mentioned (2pt deduction)

**Coverage Score: 82/100** ✓ Good but some edge cases could be explicit

---

### Completeness Evaluation (100%)

**Required Categories (All Present):**
- ✓ Purpose statement: WHAT IT DOES (1 sentence)
- ✓ Prerequisites: 3 items listed
- ✓ Usage examples: 5 provided
- ✓ Flag descriptions: 7 flags documented
- ✓ Output format: Implicit ("responses match"), could be explicit
- ✓ Error handling: 4 common errors with recovery
- ✓ Related commands: 4 commands mentioned
- ✓ Environment variables: INTENT_TARGET shown

**Optional but Recommended (Partially Present):**
- ⚠ Mental model: Implicit but not explicit
- ✓ Workflow integration: Shown in WHEN and SEE ALSO
- ⚠ Performance notes: Timeout mentioned but not characterization
- ✓ Security notes: SSRF protection explained

**Completeness Score: 100%** ✓ All required, most optional

---

### Overall Score Calculation

```
Overall = (AI-Friendliness × 0.35) + (Usability × 0.25) +
          (Consistency × 0.20) + (Coverage × 0.10) + (Completeness × 0.10)

Overall = (88 × 0.35) + (92 × 0.25) + (85 × 0.20) + (82 × 0.10) + (100 × 0.10)
Overall = 30.8 + 23.0 + 17.0 + 8.2 + 10.0
Overall = 89.0/100
```

**Tier: ★★★★☆ Good (89/100)**

---

### Recommendations for 90+

1. **Add Explicit Mental Model Section** (+ 2 AI-Friendliness points)
   ```
   MENTAL MODEL
     Intent's execution model: for each feature in spec, execute each behavior
     in sequence. Each behavior performs the HTTP request and validates the
     response against all defined checks. A single failed check fails the behavior.
   ```

2. **Add Output Format Section** (+ 2 Usability points)
   ```
   OUTPUT FORMAT (text mode)
     ✓ Behavior name: reason if passed
     ✗ Behavior name: check failure details if failed

   OUTPUT FORMAT (--json mode)
     {
       "results": [
         {"name": "...", "status": "pass|fail", "checks": [...], "duration_ms": N}
       ],
       "summary": {"passed": N, "failed": N}
     }
   ```

3. **Add Edge Cases Subsection** (+ 3 Coverage points)
   ```
   EDGE CASES
     - Large request bodies: Checked against HTTP body size limits (100KB default)
     - Streaming responses: Not supported; response must be complete within timeout
     - Redirects (3xx): Followed automatically up to 5 hops
   ```

4. **Explicit Environment Variable Example** (+ 2 Coverage points)
   ```
   Using environment variables (alternative to --target):
     export INTENT_TARGET=https://api.example.com
     intent check api.cue
   ```

**Projected Score After Improvements: 94-95/100** → ★★★★★ Excellent

---

## Example 2: Poor Command (< 50) - `interview` (Hypothetical)

### Current Help Text (Bad Example)

```
Start guided specification discovery interview
```

### Evaluation Results

**AI-Friendliness: 15/100** ✗ CRITICAL

Breakdown:
- Structural Clarity (1/40): Single sentence, no sections, no code blocks
- Actionability (2/30): No examples, flags undocumented
- Training Suitability (12/30): Mentions interview but not workflow

**Usability: 20/100** ✗ CRITICAL

- Clarity (5/30): No explanation of what "interview" means
- Completeness (5/30): Missing all essential information
- Actionability (5/20): No examples, no flags documented
- Examples (5/20): No examples provided

**Consistency: 40/100** ⚠ POOR

- No header structure → inconsistent with other commands
- No terminology alignment

**Coverage: 15/100** ✗ CRITICAL

- Flag coverage: 0/7+ flags documented
- Scenario coverage: No scenarios documented
- Integration: No workflow shown
- Edge cases: None documented

**Completeness: 10%** ✗ CRITICAL

- Missing: Prerequisites, examples, error handling, flags, etc.

**Overall Score: 22/100** → ★☆☆☆☆ CRITICAL

---

### Required Fixes

```
MINIMUM FIX (to reach 50/100):

WHAT IT DOES
  Run an interactive interview to discover your system's specification. Build
  a complete CUE spec through guided questions covering features, behaviors,
  rules, and security considerations.

WHY YOU'D USE IT
  Faster than writing specs manually. Interview guides you through all necessary
  components and validates completeness as you go.

PREREQUISITES
  - System already deployed or designed
  - 15-30 minutes for typical project

USAGE EXAMPLES
  Start new interview for REST API:
    intent interview --profile api

  Resume previously started interview:
    intent interview --resume SESSION_ID

FLAGS
  --profile       System profile: api, cli, event, data, workflow, ui (required)
  --resume        Resume existing session by ID (optional)

OUTPUT
  Exported CUE spec file suitable for: intent check, intent lint, intent analyze
```

This minimal version would score ~45-55/100.

---

### Full Fix (to reach 85+/100)

```
WHAT IT DOES
  Run a guided interview to discover your system's specification. Interview walks
  through 5 rounds of mental model development (EARS patterns, contracts,
  inversion analysis, effects, pre-mortem) to build a comprehensive CUE spec.

WHY YOU'D USE IT
  Specification writing is error-prone and often incomplete. Interview ensures
  systematic coverage of features, edge cases, security, and integration points.
  Typically faster than manual spec writing.

WHEN TO USE IT
  Early in design phase (before implementation) or after MVP (to document existing
  behavior). Part of contract-driven development workflow: interview → plan →
  implement → check.

PREREQUISITES
  - System architecture defined (or existing system to document)
  - Access to domain experts who understand the system
  - 20-40 minutes (varies by system complexity)

USAGE EXAMPLES

  Start new API specification interview:
    intent interview --profile api

  Use pre-filled answers for automation:
    intent interview --profile api --answers answers.cue --export api-spec.cue

  Resume interrupted session:
    intent interview --resume 7ea9c4b2

  Extract answers without completing interview:
    intent interview --profile api --dry-run

MENTAL MODEL
  Interview implements a 5-round mental model system:

  Round 1 (EARS): Capture ubiquitous patterns, events, state, unwanted behaviors
  Round 2 (Contracts): Define request/response contracts with validation rules
  Round 3 (Inversion): Identify failure scenarios and error paths
  Round 4 (Effects): Trace second-order effects and dependencies
  Round 5 (Pre-mortem): Document security, performance, and operational pitfalls

  After each round, system builds progressively richer spec and validates
  completeness.

WORKFLOW INTEGRATION
  1. Create spec: intent interview --profile api --export spec.cue
  2. Plan work: intent plan spec.cue
  3. Build beads from interview: intent beads --session <id>
  4. Generate suggestions: intent doctor spec.cue
  5. Execute: intent check spec.cue --target http://localhost:8080

COMMON ISSUES

  Session expired:
    Error: "Session not found"
    Fix: Run 'intent sessions' to list available sessions, or start fresh

  Missing required answers:
    Error: "Name field required in round 1"
    Fix: Use --answers answers.cue with all fields populated, or fill interactively

  Unsupported profile:
    Error: "Unknown profile: messaging"
    Fix: Use --profile with one of: api, cli, event, data, workflow, ui

FLAGS
  --profile        System profile type (required):
                   - api: REST/GraphQL services
                   - cli: Command-line tools
                   - event: Event-driven systems
                   - data: Data pipelines, ETL
                   - workflow: State machines, orchestration
                   - ui: User interfaces

  --resume         Resume existing interview by session ID (optional)
  --answers        Pre-fill answers from CUE file for automation (optional)
  --export         Export completed spec to file path (optional)
  --strict         Fail if answers file missing required fields (optional)
  --dry-run        Show CUE directives without executing (optional)

OUTPUT FORMAT
  Interactive: Series of prompts with validation feedback per round
  Non-interactive (--answers + --export): Generates spec.cue with no output
  Dry-run (--dry-run): Prints CUE directives that would be executed

SEE ALSO
  intent plan           Generate execution plan from completed spec
  intent beads          Create work items from interview session
  intent doctor         Health analysis and improvement suggestions
  intent validate       Validate generated spec syntax
  intent lint           Check for anti-patterns in generated spec
```

This would score 85-90/100.

---

## Example 3: Good Command (75-89) - `lint`

### Help Text

```
WHAT IT DOES
  Analyze spec for anti-patterns and quality issues. Detects 6 warning categories:
  missing behaviors, incomplete response checks, untested error paths, unused rules,
  documentation gaps, and security concerns.

WHY YOU'D USE IT
  Catch specification issues before testing. Anti-pattern detection prevents common
  mistakes that lead to escaped defects, incomplete feature coverage, and poor
  maintainability.

WHEN TO USE IT
  After creating initial spec, before running tests. Part of quality gate in
  development workflow. Run lint → fix issues → validate → check.

PREREQUISITES
  - Valid CUE spec file (syntax-valid, parseable)

USAGE EXAMPLES

  Analyze spec for issues:
    intent lint api.cue

  Output as JSON for tool integration:
    intent lint api.cue --json

  Analyze specific feature:
    intent lint api.cue --feature Authentication

FLAGS
  --json           Output results as JSON (optional)
  --feature        Filter to specific feature (optional)

SEE ALSO
  intent validate  Check spec syntax first
  intent analyze   Get numeric quality scores
  intent improve   Get suggestions for fixing issues
```

### Evaluation Results

**AI-Friendliness: 72/100** ⚠ Fair

- Structural Clarity (28/40): Has sections but examples are brief, 6 warning categories mentioned but not detailed
- Actionability (22/30): Examples provided but edge cases not shown
- Training Suitability (22/30): Anti-pattern concept mentioned but not explained, workflow shown but terse

**Usability: 75/100** ✓ Fair-to-Good

- Clarity (22/30): "6 warning categories" listed but not explained
- Completeness (22/30): Missing detailed issue descriptions
- Actionability (18/20): Clear enough to run but not to understand results
- Examples (13/20): Basic examples but not enough variety

**Overall: 77/100** → ★★★☆☆ Fair

### Recommendations (to reach 85+)

1. **Expand Anti-Pattern List** (+ 5 AI-Friendliness)
   ```
   ANTI-PATTERNS DETECTED
     1. Missing behaviors: Feature with no behavior definitions
     2. Incomplete checks: Response with no validation rules
     3. Untested errors: No negative test scenarios defined
     4. Unused rules: Spec rule that never affects output
     5. Documentation gaps: Behavior without descriptive text
     6. Security concerns: Missing auth, CORS, rate limit tests
   ```

2. **Add Example Issue Output** (+ 5 Usability)
   ```
   EXAMPLE OUTPUT
     ⚠ [Feature: User Mgmt] No error behaviors for password validation
     ⚠ [Behavior: Create User] Missing timeout check in response
     ⚠ [Rule: email_format] Defined but not used in any behavior
   ```

3. **Add Edge Cases** (+ 3 Coverage)
   ```
   LIMITATIONS
     - Only checks spec structure, not semantic correctness
     - Rule syntax validated but logic not analyzed
     - Does not test against actual API (use 'intent check' for that)
   ```

---

## Template: Adding Evaluations

Use this template for evaluating each command:

```markdown
## Command Name

### Help Text
[Include full help text in code block]

### Evaluation Scores

| Dimension | Score | Details |
|-----------|-------|---------|
| AI-Friendliness | X/100 | [breakdown] |
| Usability | X/100 | [breakdown] |
| Consistency | X/100 | [breakdown] |
| Coverage | X/100 | [breakdown] |
| Completeness | X%/100% | [missing categories] |
| **Overall** | **X/100** | **Tier** |

### Detailed Breakdown

**AI-Friendliness: X/100**
- Structural Clarity (X/40): [analysis]
- Actionability (X/30): [analysis]
- Training Suitability (X/30): [analysis]

[... continue for other dimensions ...]

### Recommendations

1. [Priority 1 improvement]
2. [Priority 2 improvement]
3. [Priority 3 improvement]

**Projected Score After Fixes: X-Y/100**
```

---

## Scoring Quick Reference

| Score | Tier | Status |
|-------|------|--------|
| 90-100 | ★★★★★ | Excellent - Production ready |
| 75-89 | ★★★★☆ | Good - Acceptable |
| 60-74 | ★★★☆☆ | Fair - Needs work |
| 45-59 | ★★☆☆☆ | Poor - High priority |
| <45 | ★☆☆☆☆ | Critical - Urgent |

---

**Document Status**: Reference for Phase 6 Implementation
**Next**: Evaluate all 24 commands using this framework
