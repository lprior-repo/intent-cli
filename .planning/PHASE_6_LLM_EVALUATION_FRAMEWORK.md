# PHASE 6: LLM-Based Help Text Quality Assessment Framework

**Objective**: Design and implement a comprehensive framework for evaluating help text quality across 24 Intent CLI commands using LLM-based assessment criteria.

**Status**: Framework Design Document (Pre-Implementation)

---

## 1. Overview

### Purpose
Establish measurable, LLM-assessable criteria for help text quality that ensures:
- AI agents can confidently understand and invoke commands
- Human users can quickly grasp command purpose and usage
- Help text serves as a training corpus for downstream LLM operations
- Consistency and pattern adherence across all commands

### Scope
- All 24 Intent CLI commands (Core Testing, Quality Analysis, Interview/Workflow, KIRK Analysis, Planning)
- 5 scoring dimensions: AI-Friendliness, Usability, Consistency, Coverage, and Completeness
- Automated evaluation via custom scoring script
- Both qualitative rubrics and quantitative metrics

---

## 2. Evaluation Dimensions

### 2.1 AI-Friendliness Score (0-100)

**Purpose**: Measure how well help text supports LLM understanding and agent decision-making.

#### Submetrics (weighted)

##### A. Structural Clarity for LLM Parsing (40 points)
Evaluates whether help text is parsed by LLMs into clear semantic units.

**Scoring Rubric:**

| Criteria | Points | Quality Threshold |
|----------|--------|-------------------|
| Sections clearly delimited (WHAT/WHY/WHEN/EXAMPLES) | 10 | Must have ≥3 sections |
| Consistent header format across commands | 8 | Same regex pattern for headers |
| Structured lists (not prose) | 8 | Bullet/numbered format, not paragraphs |
| Code blocks properly marked with backticks | 8 | All examples in markdown code blocks |
| Technical terms defined in context | 6 | Jargon explained on first use |

**Evaluation Prompts:**
```
1. "Can you extract the command's core function, prerequisites, and usage examples
   from this help text without ambiguity?"

2. "If this help text were tokenized by an LLM, what key semantic units would be identified?"

3. "Are there any sections that mix multiple concerns (e.g., prerequisites + examples
   in same paragraph)? How many?"
```

**Example (Good - 35/40 points):**
```
WHAT IT DOES
  Execute all behaviors defined in a spec against a target HTTP API, verifying
  that responses match expected status codes, headers, and validation rules.

WHY YOU'D USE IT
  During development and testing to confirm your API implementation matches the
  contract-driven specification.

WHEN TO USE IT
  After implementing features, before committing to version control, or as part
  of CI/CD pipeline validation.

PREREQUISITES
  - A valid Intent CUE spec file
  - Target API running and accessible

USAGE EXAMPLES
  intent check api.cue --target http://localhost:8080 --allow-localhost
  intent check api.cue --target https://api.example.com --json
```

**Example (Poor - 15/40 points):**
```
This command runs your API tests. You need a spec file and a target URL.
Just use --target to specify where your API is. For example, you might do
"intent check api.cue --target http://localhost:8080". You can also use
--json for machine-readable output. Some people use --allow-localhost for
local testing, and you can filter by feature with --feature or run just one
behavior with --only.
```

---

##### B. Actionability for AI Agents (30 points)
Evaluates whether examples are specific enough for agents to replicate behavior.

**Scoring Rubric:**

| Criteria | Points | Quality Threshold |
|----------|--------|-------------------|
| Examples are copy-paste ready (not "like", "something", "etc") | 10 | ≥2 complete examples |
| All flags explained with values/patterns | 8 | Shows `--flag VALUE` or `--flag` with meaning |
| Error scenarios + recovery documented | 7 | "If X, then Y recovery step" pattern |
| Expected output/side effects described | 5 | "Returns JSON object with fields X,Y,Z" |

**Evaluation Prompts:**
```
1. "Could an AI agent successfully run this command by following only this help text?"

2. "What flag combinations are demonstrated? What combinations are NOT shown
   that might cause confusion?"

3. "If this command fails, what troubleshooting steps does the help text provide?"

4. "Are parameter types and ranges specified? (e.g., --rounds is int 1-5)"
```

**Example (Good - 28/30 points):**
```
USAGE EXAMPLES
  Check full spec with JSON output:
    intent check api.cue --target https://api.example.com --json

  Test just authentication feature:
    intent check api.cue --target http://localhost:8080 --allow-localhost --feature Authentication

  Run single behavior with verbose output:
    intent check api.cue --target https://staging.api.com --only "Create User" --verbose

COMMON ISSUES
  If you get "Target URL required", use: export INTENT_TARGET=https://api.example.com
  If you get "Connection refused", check that the target service is running
  If you get "Localhost blocked", use: intent check spec.cue --target http://localhost:8080 --allow-localhost
```

**Example (Poor - 12/30 points):**
```
Use --target to set the target URL. You can also use --json.
There's a --feature flag for filtering. Some flags like --only
can be used together. If it doesn't work, check your spec file.
```

---

##### C. Suitability for Agent Training (30 points)
Evaluates whether help text teaches LLMs mental models and workflow integration.

**Scoring Rubric:**

| Criteria | Points | Quality Threshold |
|----------|--------|-------------------|
| Mental model documented (what command does conceptually) | 10 | Explains algorithm/approach, not just I/O |
| Workflow integration shown (related commands, sequencing) | 8 | Shows "After X command, run Y" patterns |
| Failure modes documented (what can go wrong) | 7 | Lists 2+ failure scenarios |
| Related commands mentioned | 5 | "See also: intent validate, intent lint" |

**Evaluation Prompts:**
```
1. "What is the underlying mental model for this command? How does the help text explain it?"

2. "How does this command fit into a larger workflow? Can you identify the sequence of commands
   an agent should follow?"

3. "What are all the ways this command can fail? How many are documented?"

4. "What commands would logically precede or follow this one? Are those relationships mentioned?"
```

**Example (Good - 28/30 points):**
```
MENTAL MODEL
  Intent traverses each behavior in your spec and executes its request against
  the target API. For each response, it validates status code, headers, and runs
  all checks defined in the behavior's response.checks array. A single failed
  check fails the entire behavior.

WORKFLOW INTEGRATION
  1. Create spec: intent interview --profile api --export spec.cue
  2. Validate syntax: intent validate spec.cue
  3. Execute tests: intent check spec.cue --target http://localhost:8080
  4. If failures, analyze with: intent doctor spec.cue
  5. Fix and iterate

FAILURE MODES
  - Behavior timeout: API not responding, increase with INTENT_TIMEOUT_MS env var
  - All checks fail: Response structure doesn't match expected shape
  - Feature not found: Feature name in spec doesn't exist
```

**Example (Poor - 10/30 points):**
```
Run the check command to test your spec. It sends requests and checks responses.
```

---

### 2.2 Usability Score (0-100)

**Purpose**: Measure effectiveness for human end-users.

#### Submetrics (weighted)

| Criteria | Weight | Description |
|----------|--------|-------------|
| Clarity for human users | 30% | Simple language, avoids jargon |
| Completeness of information | 30% | All necessary info present (not scattered) |
| Actionability (ease of use) | 20% | Quick-start path clear, minimal cognitive load |
| Example quality & variety | 20% | Covers common use cases, edge cases |

**Evaluation Prompts:**
```
1. "If a user has never seen this command before, how many steps would it take
   them to successfully run it?"

2. "Are there any undefined terms or concepts that would send a user to documentation?"

3. "Do the examples cover: happy path, error case, advanced usage? What's missing?"

4. "Could a user understand this help text without reading CLAUDE.md?"
```

**Scoring Thresholds:**

| Score | Rating | Assessment |
|-------|--------|------------|
| 90-100 | Excellent | Users can self-serve; no external docs needed |
| 75-89 | Good | Most users succeed; minor gaps acceptable |
| 60-74 | Fair | Some users need external help; gaps present |
| <60 | Poor | Requires external docs; significant gaps |

---

### 2.3 Consistency Score (0-100)

**Purpose**: Measure adherence to patterns and standards across all commands.

#### Submetrics

| Criteria | Weight | Pattern |
|----------|--------|---------|
| Terminology consistency | 30% | Same term means same thing across 24 commands |
| Formatting consistency | 25% | Headers, sections, code blocks follow same regex |
| Tone consistency | 25% | Voice (imperative, explanatory, etc.) is uniform |
| Structure consistency | 20% | All commands follow WHAT/WHY/WHEN/EXAMPLES pattern |

**Evaluation Checklist:**

```
Terminology Matrix:
  - "spec" vs "specification" vs "config": pick one
  - "feature" vs "endpoint" vs "operation": pick one
  - "behavior" vs "scenario" vs "test case": pick one
  - "beads" vs "tasks" vs "work items": pick one
  - "round" vs "pass" vs "iteration": pick one

Formatting Matrix:
  Header format:     WHAT IT DOES / WHY / WHEN / EXAMPLES
  Examples format:   ```bash\n  intent cmd args\n```
  Flag format:       --flag VALUE or --flag
  Emphasis:          **bold** for concepts, `code` for literals

Tone:
  Imperative:  "Run X to achieve Y"
  Explanatory: "This command X so that Y"
  Avoid:       "It does X" (too vague), "You should X" (preachy)

Structure:
  All 24 commands should follow:
  1. WHAT IT DOES (1-2 sentences)
  2. WHY YOU'D USE IT (1-2 sentences)
  3. WHEN TO USE IT (context + timing)
  4. PREREQUISITES (if any)
  5. USAGE EXAMPLES (2+ examples)
  6. COMMON ISSUES / FAILURE MODES (if applicable)
  7. SEE ALSO (related commands)
```

**Consistency Scoring Example:**

Command `check`: Uses "spec" → all other commands must use "spec"
Command `interview`: Uses "feature" → all other commands must use "feature"
Command `beads`: Uses "bead" → all other commands must use "bead"

Score: Count violations / (24 commands × 5 dimensions) × 100

---

### 2.4 Coverage Score (0-100)

**Purpose**: Measure completeness of documented functionality and edge cases.

#### Submetrics

| Criteria | Weight | Description |
|----------|--------|-------------|
| Flag coverage | 30% | All flags documented with examples |
| Scenario coverage | 30% | Happy path + error paths covered |
| Integration coverage | 20% | Shows interaction with other commands |
| Edge case coverage | 20% | Documents boundary conditions, gotchas |

**Evaluation Prompts:**
```
1. "How many flags does this command support? How many are documented in help text?"

2. "What scenarios can this command handle? Which are documented?"

3. "If this command is part of a workflow, are those dependencies shown?"

4. "What are the boundary conditions? Empty inputs? Maximum sizes? Timeouts?"
```

---

### 2.5 Completeness Score (0-100)

**Purpose**: Measure presence of all required information categories.

#### Required Categories

```
For every command, help text MUST include:

✓ Purpose statement (1-2 sentences)
✓ Prerequisites (what must exist first)
✓ ≥2 usage examples (copy-paste ready)
✓ Flag descriptions (all flags)
✓ Output format (what user will see)
✓ Error handling (2+ common errors)
✓ Related commands (cross-references)
✓ Environment variables (if supported)

Optional but recommended:
○ Mental model (conceptual algorithm)
○ Workflow integration (before/after commands)
○ Performance notes (when slow, why)
○ Security notes (auth, SSRF, etc.)
```

**Scoring**: (Present Categories / Total Categories) × 100

---

## 3. Scoring Rules & Thresholds

### 3.1 Overall Quality Score Calculation

```
Overall Score = (
  (AI-Friendliness × 0.35) +
  (Usability × 0.25) +
  (Consistency × 0.20) +
  (Coverage × 0.10) +
  (Completeness × 0.10)
) / 100
```

### 3.2 Quality Tiers

| Score Range | Tier | Assessment | Action |
|-------------|------|-----------|--------|
| 90-100 | ★★★★★ | Excellent | Production-ready; use as template |
| 75-89 | ★★★★☆ | Good | Minor improvements; acceptable |
| 60-74 | ★★★☆☆ | Fair | Needs work; assign to backlog |
| 45-59 | ★★☆☆☆ | Poor | Significant gaps; high priority |
| <45 | ★☆☆☆☆ | Critical | Broken/incomplete; urgent fix |

### 3.3 Per-Command Exit Criteria

For a command to pass Phase 6 review:

```
✓ AI-Friendliness ≥ 85
✓ Usability ≥ 80
✓ Consistency ≥ 90
✓ Overall Score ≥ 85
✓ Completeness = 100% (all required categories present)
```

---

## 4. LLM Evaluation Prompts (Production Use)

### 4.1 Structural Assessment Prompt

```
Evaluate this help text for AI parsing clarity. Score 0-40 based on:
- Sections are clearly delimited (WHAT/WHY/WHEN/EXAMPLES): 10 points
- Header format is consistent and parseable: 8 points
- Information is in structured lists, not prose: 8 points
- All code examples are in markdown code blocks: 8 points
- Technical terms are defined in context: 6 points

Help text:
---
[INSERT HELP TEXT]
---

Provide:
1. Individual scores for each criterion
2. Total /40 score
3. 2-3 specific improvements

Format as JSON:
{
  "sections_clarity": N,
  "header_consistency": N,
  "list_structure": N,
  "code_blocks": N,
  "term_definitions": N,
  "total": N,
  "improvements": ["...", "...", "..."]
}
```

### 4.2 Actionability Assessment Prompt

```
Evaluate this help text for AI agent actionability. Score 0-30 based on:
- Examples are copy-paste ready (not vague): 10 points
- All flags explained with values/patterns: 8 points
- Error scenarios + recovery documented: 7 points
- Expected output/side effects described: 5 points

Help text:
---
[INSERT HELP TEXT]
---

Provide:
1. Individual scores for each criterion
2. Total /30 score
3. Can an AI successfully run this command? (yes/no/maybe)
4. Missing examples or documentation

Format as JSON:
{
  "examples_ready": N,
  "flags_explained": N,
  "errors_documented": N,
  "output_described": N,
  "total": N,
  "ai_actionable": "yes|no|maybe",
  "gaps": ["...", "...", "..."]
}
```

### 4.3 Training Suitability Prompt

```
Evaluate this help text for training an AI agent. Score 0-30 based on:
- Mental model documented: 10 points
- Workflow integration shown: 8 points
- Failure modes documented: 7 points
- Related commands mentioned: 5 points

Help text:
---
[INSERT HELP TEXT]
---

Provide:
1. Individual scores for each criterion
2. Total /30 score
3. Extracted mental model (1-2 sentences)
4. Identified workflow sequence (if any)
5. Documented failure modes (list)

Format as JSON:
{
  "mental_model_score": N,
  "workflow_score": N,
  "failure_modes_score": N,
  "related_commands_score": N,
  "total": N,
  "mental_model": "...",
  "workflow_sequence": ["cmd1", "cmd2", ...],
  "failure_modes": ["...", "...", "..."]
}
```

---

## 5. Example Evaluations

### 5.1 Example Command: `check`

#### Baseline Help Text (Current)

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

  Test production API with JSON output for tooling:
    intent check api.cue --target https://api.example.com --json

  Run specific feature:
    intent check api.cue --target http://localhost:8080 --feature Authentication

  Run specific behavior with verbose output:
    intent check api.cue --target http://localhost:8080 --only "Create User" --verbose

COMMON ISSUES
  - If target URL is blocked, check SSRF protection:
    Use --allow-localhost for development, or contact ops for prod access
  - If all checks fail, validate spec structure: intent validate api.cue
  - Connection timeout: API slow or unavailable. Check with: curl -I <target>

FLAGS
  --target URL          Target base URL (required) [env: INTENT_TARGET]
  --feature NAME        Filter to specific feature (optional)
  --only BEHAVIOR       Run only specific behavior (optional)
  --json                Output results as JSON (optional)
  --verbose             Enable verbose output (optional)
  --quiet               Suppress non-error output (optional)
  --allow-localhost     Bypass SSRF protection (optional)

SEE ALSO
  intent validate       Check spec syntax before running tests
  intent doctor         Get health report if tests fail
  intent lint           Find anti-patterns in spec
```

#### Evaluation Results

**AI-Friendliness: 88/100**
- Structural Clarity (40): 36/40 ✓
  - Sections clearly delimited ✓
  - Some prose mixing in PREREQUISITES ✗ (minor)
  - Submetric: "Connection timeout" is prose, should be list
- Actionability (30): 29/30 ✓
  - All examples copy-paste ready ✓
  - Flag patterns shown with values ✓
  - Error recovery documented ✓
  - Expected behavior implicit (could be explicit)
- Training Suitability (30): 23/30 ⚠️
  - Mental model present but brief (lacks "execution flow")
  - Workflow shown implicitly (validate → check → doctor) but not explicit
  - Failure modes documented well (3 scenarios)
  - Related commands listed

**Usability: 92/100**
- Clarity (30): 28/30 - Plain language, minimal jargon
- Completeness (30): 29/30 - All essential info present
- Actionability (20): 19/20 - Quick-start very clear
- Examples (20): 16/20 - Good variety but missing edge case example

**Consistency: 85/100**
- Terminology: "spec", "feature", "behavior" consistent with CLAUDE.md ✓
- Formatting: Headers follow pattern, code blocks present ✓
- Tone: Imperative and clear
- Structure: Follows WHAT/WHY/WHEN/EXAMPLES pattern ✓
- Minor: FLAGS section not in other commands yet

**Coverage: 82/100**
- Flag coverage: 7/7 flags documented ✓
- Scenario coverage: Happy path, error paths, filtering shown ✓
- Integration: validate→check→doctor shown ✓
- Edge cases: Timeout, SSRF, spec issues covered ✓
- Missing: Performance characteristics, batch behavior

**Completeness: 100%**
- All required categories present

**Overall Score: 89.5/100** → ★★★★☆ Good

**Recommendations:**
1. Add explicit "Execution Flow" subsection under WHAT IT DOES
2. Move FLAGS section into structured subsection (currently looks like command reference)
3. Document timeout behavior more explicitly
4. Add edge case example: "Run with quiet mode in CI pipeline"

---

### 5.2 Example Command: `interview` (Hypothetical Evaluation)

#### Baseline Help Text

```
Start a guided specification discovery interview. Walks through 5 rounds of
mental model development: EARS patterns, contracts, inversion, effects, pre-mortem.
```

**Evaluation Results**

**AI-Friendliness: 42/100** ⚠️ CRITICAL
- Structural Clarity (40): 8/40 ✗ - No sections, single sentence
- Actionability (30): 5/30 ✗ - No examples, flags not documented
- Training Suitability (30): 29/30 ✓ - Actually mentions mental model rounds!

**Usability: 35/100** ✗ CRITICAL
- Clarity: 10/30 - What are "EARS patterns"? Undefined
- Completeness: 5/30 - Missing prerequisites, examples, flags
- Actionability: 5/20 - No clear path forward
- Examples: 0/20 - No examples given

**Overall Score: 48/100** → ★☆☆☆☆ Critical

**Required Fixes:**
- Add WHAT/WHY/WHEN/EXAMPLES structure
- Document all flags (--profile, --resume, --answers, --export, etc.)
- Add 3+ copy-paste-ready examples
- Explain EARS, rounds, and workflow explicitly
- Add prerequisites section

---

## 6. Automated Evaluation Script

### 6.1 Proposed Implementation: `evaluate-help-text.gleam`

```gleam
import gleam/json
import gleam/list
import gleam/string

pub type HelpTextMetrics {
  HelpTextMetrics(
    command_name: String,
    ai_friendliness: Float,
    usability: Float,
    consistency: Float,
    coverage: Float,
    completeness: Float,
    overall_score: Float,
    tier: String,
    issues: List(String),
    recommendations: List(String),
  )
}

pub fn evaluate_command(name: String, help_text: String) -> HelpTextMetrics {
  // Automated checks:
  // 1. Has WHAT/WHY/WHEN/EXAMPLES sections? (regex)
  // 2. How many code blocks? (```count)
  // 3. How many examples? (count "intent" commands)
  // 4. How many flags documented? (-- count)
  // 5. Terminology consistency with known set
  // 6. Reads at Flesch-Kincaid grade level 8 or below

  // Return computed metrics
  HelpTextMetrics(
    command_name: name,
    ai_friendliness: 0.0,
    usability: 0.0,
    consistency: 0.0,
    coverage: 0.0,
    completeness: 0.0,
    overall_score: 0.0,
    tier: "",
    issues: [],
    recommendations: [],
  )
}

pub fn evaluate_all_commands(commands: List(#(String, String))) -> List(HelpTextMetrics) {
  list.map(commands, fn(cmd) {
    let #(name, help_text) = cmd
    evaluate_command(name, help_text)
  })
}

pub fn generate_report(metrics: List(HelpTextMetrics)) -> String {
  // Generate markdown report with:
  // - Summary statistics (avg scores)
  // - Per-command breakdown
  // - Tier distribution
  // - Highest/lowest performers
  // - Batch recommendations
  ""
}
```

### 6.2 Automated Checks

```
Pattern Matching:
  ✓ Has section headers: "^WHAT IT DOES|^WHY YOU'D USE IT|^WHEN TO USE IT|^EXAMPLES"
  ✓ Code block count: count(```gleam|```bash|```shell)
  ✓ "intent" command count: grep -o "intent [a-z-]*" | wc -l
  ✓ Flag count: grep -o "\-\-[a-z-]*" | wc -l
  ✓ Average sentence length: split, measure
  ✓ Flesch-Kincaid grade: compute from word/syllable counts

Terminology Checks:
  ✓ spec vs specification vs config: count each
  ✓ feature vs endpoint: count each
  ✓ behavior vs scenario: count each
  ✓ bead vs task: count each

Cross-Command Consistency:
  ✓ Compare WHAT IT DOES format across all 24 commands
  ✓ Detect terminology drift
  ✓ Flag similar commands with different patterns
```

---

## 7. Implementation Roadmap

### Phase 6a: Evaluation Framework (Current)
- [ ] Design scoring rubrics (DONE)
- [ ] Define LLM prompts (DONE)
- [ ] Create example evaluations
- [ ] Document automation approach

### Phase 6b: Baseline Assessment
- [ ] Evaluate all 24 current commands
- [ ] Categorize by tier
- [ ] Identify highest-priority improvements

### Phase 6c: Remediation
- [ ] Fix critical tier commands (score < 45)
- [ ] Improve poor tier commands (45-59)
- [ ] Standardize consistency across fair tier

### Phase 6d: Automation
- [ ] Implement `evaluate-help-text.gleam` script
- [ ] Integrate with CI/CD (pre-commit hook)
- [ ] Create regression testing

### Phase 6e: Validation
- [ ] Run LLM evaluations on improved text
- [ ] Measure improvement delta
- [ ] Document best practices

---

## 8. Integration Points

### 8.1 With Existing Modules

```
error_handler.gleam
  → Use for error documentation examples
  → Show how commands fail + recovery

formatter_utils.gleam
  → Demonstrate box_header, progress_bar patterns
  → Show output formatting expectations

cli_text_constants.gleam
  → Pull command descriptions from here
  → Use flag description helpers as examples

config.gleam
  → Document environment variable usage
  → Show config precedence (env → flags)
```

### 8.2 With Glint Commands

```gleam
// In intent.gleam, use long_help() for detailed help text

fn check_command() -> glint.Command(Nil) {
  glint.command(fn(input) { ... })
  |> glint.description(cli_text_constants.cmd_check_desc)
  |> glint.long_help("""
    // Full help text following evaluation framework
    WHAT IT DOES
      ...
    WHY YOU'D USE IT
      ...
    // ... etc
  """)
}
```

---

## 9. Acceptance Criteria

### For Phase 6 Completion

```
✓ Framework document complete (this file)
✓ All 24 commands evaluated with baseline scores
✓ Tier distribution identified (X excellent, Y good, Z fair, etc.)
✓ LLM evaluation prompts tested (manual review with Claude)
✓ Automated script produces JSON metrics
✓ Best practices documented
✓ Remediation backlog created (prioritized by tier)

Threshold for "Done":
  - 90%+ of commands in Good tier (75+) or better
  - Zero commands in Critical tier (< 45)
  - Consistency score ≥ 90 across all commands
  - Framework validated via LLM re-evaluation
```

---

## 10. Appendix: Quick Reference

### 10.1 Terminology Dictionary

| Term | Definition | Used In |
|------|-----------|---------|
| Spec | CUE specification file with features/behaviors | All commands |
| Feature | Named collection of behaviors (e.g., "Authentication") | check, lint, analyze |
| Behavior | Single request-response test case with checks | check, lint, analyze |
| Bead | Atomic 5-30min work unit | beads, bead-status, plan |
| Round | Mental model pass (EARS, Contracts, etc.) | interview, plan, doctor |
| Check | Validation rule in behavior.response.checks | check, lint, improve |
| KIRK | Analysis framework (Quality, Inversion, Coverage, Gaps, Effects) | 5 KIRK commands |
| EARS | Structured requirement patterns (Ubiquitous, Event, State, Unwanted) | ears, parse |

### 10.2 Standard Section Headers

```
WHAT IT DOES
  [1-2 sentences describing core function]

WHY YOU'D USE IT
  [1-2 sentences describing motivation/benefit]

WHEN TO USE IT
  [Context and timing: after X command, before Y, in CI pipeline, etc.]

PREREQUISITES
  [Bulleted list of what must exist first]
  - A valid Intent CUE spec file
  - Network access to target
  - (if applicable)

USAGE EXAMPLES
  [2+ copy-paste-ready examples]

COMMON ISSUES
  [2+ failure scenarios with recovery steps]

FLAGS
  [All flags documented with --flag VALUE or --flag]

SEE ALSO
  [Related commands: intent cmd1, intent cmd2]
```

### 10.3 Help Text Template

```
pub fn cmd_name_help() -> String {
  """WHAT IT DOES
  [1-2 sentences]

WHY YOU'D USE IT
  [1-2 sentences]

WHEN TO USE IT
  [Context]

PREREQUISITES
  - [Item 1]
  - [Item 2]

USAGE EXAMPLES

  Example 1 (title):
    intent cmd arg --flag value

  Example 2 (title):
    intent cmd arg --flag value

COMMON ISSUES

  Issue 1:
    Error: ...
    Fix: ...

  Issue 2:
    Error: ...
    Fix: ...

FLAGS
  --flag         Description (value or type)

SEE ALSO
  intent cmd1    Related command
  intent cmd2    Related command"""
}
```

---

## 11. Success Metrics

### Quantitative
- Average AI-Friendliness: from X to ≥85
- Average Usability: from X to ≥80
- Consistency: achieve ≥90
- Tier distribution: 60%+ at ★★★★☆ or better
- Zero commands below ★★☆☆☆

### Qualitative
- LLMs can successfully parse and understand all commands
- New users report faster onboarding
- Help text serves as effective training corpus
- Reduced GitHub issues about "how do I use X?"
- Commands feel cohesive and intentional

---

**Document Status**: Ready for Review
**Next Phase**: Baseline Assessment of all 24 commands
