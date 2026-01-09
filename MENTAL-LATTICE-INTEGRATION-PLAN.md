# Mental Lattice Integration Plan: Comprehensive Framework

## Executive Summary

This plan systematically integrates the five Mental Lattices (EARS, KIRK, Inversion, Second-Order Thinking, Pre-Mortem) into the Intent CLI bead system, transforming it from a task-tracking tool into a formally-verified, AI-guided planning system.

**Strategic Goals**:
1. Validate every bead through mental models **upfront** (during planning phase)
2. Generate beads **systematically** from specs using EARS + inversions
3. **Iteratively refine** beads through KIRK quality checks
4. **Regenerate failed beads** smarter by understanding root causes (second-order + pre-mortem)
5. Add KIRK metadata to schema while keeping beads clean and readable

**Expected Outcomes**:
- 90%+ of beads will have explicit mental model validation
- Hidden complexity revealed before execution
- Reduced bead failures through inversion checking
- Automated gap detection across all specifications
- Integration with existing KIRK modules (ears_parser, inversion_checker, quality_analyzer, gap_detector)

---

## SECTION 1: EXTENDED SCHEMA DESIGN

### 1.1 Extended #Bead Type

The current `#Bead` schema is excellent but lacks mental model metadata. Extend it minimally to capture KIRK analysis without cluttering the core definition:

```cue
// In schema/beads.cue

#Bead: {
    // ===================== EXISTING FIELDS =====================
    id:    #BeadID
    title: string
    what:  string        // Imperative, actionable requirement
    why:   string        // Business value
    test:  #TestSpec
    done_when: [...string] & list.MinItems(1)
    file:  string
    edge_cases: [...string]
    requires: [...#BeadID]
    effort: #Effort
    status: #BeadStatus

    // ===================== KIRK EXTENSIONS =====================
    kirk?: #BeadKirkMetadata
}

#BeadKirkMetadata: {
    // EARS: Requirement clarity and pattern
    ears?: #BeadEARS

    // KIRK: Contract specification
    contract?: #BeadContract

    // Inversion: Failure modes and edge cases
    inversion?: #BeadInversion

    // Quality: Multi-dimensional scoring
    quality?: #BeadQuality

    // Flags: What analyses have been run?
    analysis_performed?: {
        ears_checked?:      bool
        inversion_checked?: bool
        quality_scored?:    bool
        gaps_detected?:     bool
    }

    // Audit: When was analysis done?
    analysis_metadata?: {
        analyzed_at?: string  // ISO8601
        analyzer?:    string  // e.g., "intent analyze-beads"
        version?:     string  // Schema version
    }
}

// ===== EARS ANALYSIS =====
#BeadEARS: {
    // Which EARS pattern does this bead's "what" field follow?
    pattern: "ubiquitous" | "event_driven" | "state_driven" | "optional" | "unwanted" | "complex"

    // Extracted components from EARS pattern
    trigger?:   string    // When [trigger]
    condition?: string    // Where [condition]
    state?:     string    // While [state]

    // Clarity score: 0-100
    clarity_score?: int

    // Is this EARS pattern correctly matched to the bead's test?
    pattern_matches_test?: bool
}

// ===== CONTRACT SPECIFICATION =====
#BeadContract: {
    // Design by Contract: What must be true before?
    preconditions?: [...string] & list.UniqueItems()

    // Design by Contract: What must be true after?
    postconditions?: [...string] & list.UniqueItems()

    // Invariants: What must ALWAYS be true?
    invariants?: [...string] & list.UniqueItems()
}

// ===== INVERSION ANALYSIS =====
#BeadInversion: {
    // Failure modes identified by inversion
    security_risks?: [...#IdentifiedRisk]
    usability_risks?: [...#IdentifiedRisk]
    integration_risks?: [...#IdentifiedRisk]

    // Is this bead covered by adequate edge_cases?
    edge_case_coverage?: int  // 0-100 percentage

    // Missing edge cases that should be tested
    suggested_edge_cases?: [...string]
}

#IdentifiedRisk: {
    risk:       string
    severity:   "low" | "medium" | "high" | "critical"
    mitigation: string
}

// ===== QUALITY ANALYSIS =====
#BeadQuality: {
    // Multi-dimensional quality scores (0-100)
    completeness: int & >=0 & <=100
    testability:  int & >=0 & <=100
    clarity:      int & >=0 & <=100

    // Overall quality
    overall: int & >=0 & <=100

    // Issues found
    issues?: [...#QualityIssue]
}

#QualityIssue: {
    field:      string  // e.g., "done_when"
    issue:      string
    severity:   "info" | "warning" | "error"
    suggestion: string
}

// ===== BEAD ID FORMAT =====
#BeadID: =~"^[A-Z0-9]+-[0-9]{3}$"
```

### 1.2 Schema Organization

The extended schema maintains clean separation:
- **Core fields** (id, title, what, why, test, done_when, etc.) remain unchanged
- **KIRK metadata** is **optional** via `kirk?: #BeadKirkMetadata`
- Beads without KIRK analysis remain valid and usable
- KIRK fields are purely **additive** - no breaking changes

**Benefits**:
1. Humans write simple beads; KIRK analysis is optional
2. Tool output can progressively enrich beads with metadata
3. Backwards compatibility maintained
4. Schema remains readable and maintainable

---

## SECTION 2: BEAD GENERATION PIPELINE

### 2.1 Three-Phase Generation Flow

Instead of generating beads once, implement an **iterative refinement loop**:

```
Phase 1: GENERATE
    Spec (CUE) → Parse behaviors → Create initial beads
    Output: beads with basic what/why/test

Phase 2: VALIDATE
    Beads → EARS check → Inversion check → Quality check
    Output: beads enriched with kirk metadata

Phase 3: REFINE
    Issues found → Regenerate problem beads → Re-validate
    Output: high-quality beads ready for execution
```

### 2.2 Generation Process Detailed

#### **Step 1: Parse Specification → Behaviors**

Input: A CUE spec (example from examples/user-api.cue)

```cue
features: [{
    name: "user-management"
    behaviors: [{
        name: "create-user"
        intent: "Create a new user account"
        request: { method: "POST", path: "/users", body: {...} }
        response: { status: 201, checks: {...} }
    }]
}]
```

#### **Step 2: Map Each Behavior → Initial Bead**

```gleam
pub fn behavior_to_bead(
    feature_name: String,
    behavior: Behavior,
    sequence: Int
) -> Result(Bead, String) {
    let bead_id = string.concat([
        feature_name,
        "-",
        behavior.name,
        "-",
        int.to_string(sequence)
    ]) |> string.uppercase

    // Extract from behavior
    let test = behavior_to_test_spec(behavior)
    let edge_cases = extract_edge_cases_from_behavior(behavior)
    let requirements_from_behavior = extract_requires(behavior)

    Ok(Bead(
        id: bead_id,
        title: behavior.intent,
        what: format_EARS_from_behavior(behavior),  // <- Imperative!
        why: explain_why_test_matters(behavior),
        test: test,
        done_when: generate_observable_criteria(behavior),
        file: infer_implementation_file(feature_name, behavior),
        edge_cases: edge_cases,
        requires: requirements_from_behavior,
        effort: estimate_effort(behavior),
        status: "pending",
        kirk: None,  // Will be filled in Phase 2
    ))
}
```

**Key Insight**: Map `behavior.intent` to imperative "what" using EARS format:
- API behavior → "POST /endpoint with payload X"
- Database behavior → "Insert record with constraints Y"
- Validation → "Reject invalid Z with error code W"

#### **Step 3: Validate Against EARS Patterns**

```gleam
pub fn validate_bead_ears(bead: Bead) -> Result(BeadEARS, List(String)) {
    // Parse the bead's "what" field as EARS requirement
    let ears_parse_result = ears_parser.parse(bead.what)

    case ears_parse_result {
        Ok(parsed_requirement) -> {
            // Check 1: Does the pattern match the test?
            let pattern_valid = case parsed_requirement.pattern {
                "event_driven" ->
                    // Pattern expects trigger - should be in test.command
                    string.contains(bead.test.command, "WHEN")
                "ubiquitous" ->
                    // No trigger expected
                    True
                _ -> True
            }

            // Check 2: Is clarity score acceptable?
            let clarity = calculate_clarity_score(parsed_requirement)
            let clarity_ok = clarity >= 75

            case pattern_valid && clarity_ok {
                True -> Ok(BeadEARS(
                    pattern: parsed_requirement.pattern,
                    trigger: parsed_requirement.trigger,
                    condition: parsed_requirement.condition,
                    clarity_score: clarity,
                    pattern_matches_test: True,
                ))
                False -> Error([
                    "EARS pattern doesn't match test specification",
                    "Clarity score too low: " <> int.to_string(clarity)
                ])
            }
        }
        Error(parse_error) -> Error(["Invalid EARS pattern: " <> parse_error])
    }
}
```

#### **Step 4: Run Inversion Analysis**

```gleam
pub fn analyze_bead_inversions(
    bead: Bead,
    category: String  // "security" | "usability" | "integration"
) -> BeadInversion {
    // Use existing inversion_checker module
    let risks = inversion_checker.find_risks_for_behavior(
        bead.what,
        bead.test,
        category
    )

    // Check: Does bead.edge_cases cover these risks?
    let missing_inversions = list.filter(risks, fn(risk) {
        !list.any(bead.edge_cases, fn(edge_case) {
            string.contains(edge_case, risk.risk)
        })
    })

    BeadInversion(
        security_risks: filter_by_category(risks, "security"),
        usability_risks: filter_by_category(risks, "usability"),
        integration_risks: filter_by_category(risks, "integration"),
        edge_case_coverage: calculate_coverage(
            list.length(bead.edge_cases),
            list.length(risks)
        ),
        suggested_edge_cases: list.map(missing_inversions, fn(r) { r.risk }),
    )
}
```

#### **Step 5: Extract KIRK Contracts**

```gleam
pub fn extract_kirk_contract(bead: Bead) -> BeadContract {
    // From bead.what, extract preconditions
    let preconditions = case bead.what {
        "WHEN " <> trigger <> " THEN " <> _ ->
            // Event-driven: what must be true for trigger to apply?
            extract_preconditions_from_trigger(trigger)
        "WHILE " <> state <> " THEN " <> _ ->
            // State-driven: what state is required?
            [state]
        "WHERE " <> condition <> " THEN " <> _ ->
            // Optional: what condition must hold?
            [condition]
        _ -> []
    }

    // From bead.done_when, extract postconditions
    let postconditions = bead.done_when

    BeadContract(
        preconditions: preconditions,
        postconditions: postconditions,
        invariants: extract_invariants(bead.test),
    )
}
```

#### **Step 6: Calculate Quality Score**

```gleam
pub fn score_bead_quality(bead: Bead) -> BeadQuality {
    // Use existing quality_analyzer module
    let completeness = case (bead.why, bead.file) {
        (s, f) if string.length(s) > 50 && string.length(f) > 0 -> 100
        (s, f) if string.length(s) > 20 && string.length(f) > 0 -> 75
        (_, f) if string.length(f) > 0 -> 50
        _ -> 0
    }

    let testability = case bead.test {
        Bead(test: Test(command: cmd, ..)) if string.length(cmd) > 0 ->
            case bead.done_when {
                [] -> 30  // No verification
                [_] -> 60  // One criterion
                _ -> 100  // Multiple criteria
            }
        _ -> 0
    }

    let clarity = case bead.kirk {
        Some(BeadKirkMetadata(ears: Some(ears))) -> ears.clarity_score
        _ -> 50
    }

    let issues = identify_quality_issues(bead, completeness, testability, clarity)

    BeadQuality(
        completeness: completeness,
        testability: testability,
        clarity: clarity,
        overall: (completeness + testability + clarity) / 3,
        issues: issues,
    )
}
```

### 2.3 Refinement Loop: Regenerate Problem Beads

When validation identifies issues, regenerate that bead with improvements:

```gleam
pub fn regenerate_bead_from_issues(
    original_bead: Bead,
    issues: List(QualityIssue)
) -> Result(Bead, String) {
    let improved_bead = original_bead

    // Fix 1: Poor clarity? Rewrite using EARS patterns
    let improved_bead = case list.find(issues, fn(i) { i.field == "what" }) {
        Ok(clarity_issue) -> {
            let new_what = reformat_with_ears_template(original_bead.what)
            Bead(..improved_bead, what: new_what)
        }
        Error(Nil) -> improved_bead
    }

    // Fix 2: Missing done_when criteria? Generate from test
    let improved_bead = case list.find(issues, fn(i) { i.field == "done_when" }) {
        Ok(_) -> {
            let new_done_when = generate_done_when_from_test(improved_bead.test)
            Bead(..improved_bead, done_when: new_done_when)
        }
        Error(Nil) -> improved_bead
    }

    // Fix 3: Missing edge cases? Add from inversion analysis
    let improved_bead = case list.find(issues, fn(i) { i.field == "edge_cases" }) {
        Ok(_) -> {
            let new_edge_cases = list.concat([
                improved_bead.edge_cases,
                generate_edge_cases_from_inversions(improved_bead)
            ]) |> list.unique_by(fn(x) { x })
            Bead(..improved_bead, edge_cases: new_edge_cases)
        }
        Error(Nil) -> improved_bead
    }

    // Recursively re-validate
    case validate_bead_kirk(&improved_bead) {
        Ok(Nil) -> Ok(improved_bead)
        Error(new_issues) ->
            // Still has issues? Try more aggressive regeneration
            regenerate_bead_more_aggressively(improved_bead, new_issues)
    }
}
```

---

## SECTION 3: PHASE 1 AUTOMATION BEADS - MENTAL LATTICE ENHANCEMENTS

### 3.1 P1-T1.1: answer_loader.gleam

**Current Bead**:
- **What**: "Load pre-filled answers from CUE file for non-interactive interview"
- **Why**: "Enable automation: CI/scripts can provide answers upfront without human prompt"

**EARS Enhancement**:
```
THE SYSTEM SHALL load answers from CUE file
WHERE the file exists and contains valid CUE syntax
WHEN loaded answers are requested
```

**KIRK Preconditions**:
- ✅ CUE file exists at provided path (absolute or resolvable relative)
- ✅ Process has read permissions on file
- ✅ File contains valid CUE syntax (passes `cue vet`)
- ✅ File structure matches expected #AnswerMap schema
- ✅ Answer map keys are question IDs in interview schema

**KIRK Postconditions**:
- ✅ Returns Result(Dict(QuestionID, Answer), String)
- ✅ Dict contains only valid question IDs
- ✅ All values are non-empty, trimmed strings
- ✅ Dict is immutable after construction
- ✅ Errors are descriptive and actionable (file not found vs parse error vs schema mismatch)

**Inversion Analysis**:
- 🔴 **Critical**: File not found → must return clear error
  - Mitigation: Check file existence before parse
- 🔴 **Critical**: Invalid CUE syntax → parse error handling
  - Mitigation: Use CUE parser's error messages
- 🟡 **High**: Schema mismatch → answer key not in interview questions
  - Mitigation: Validate keys against schema before returning
- 🟡 **High**: Empty answer string → should it be accepted?
  - Mitigation: Define empty string handling policy (reject vs accept)
- 🟢 **Medium**: Large file (>1MB) → performance
  - Mitigation: Document expected file size bounds
- 🟢 **Medium**: Unicode in answers → encoding issues
  - Mitigation: UTF-8 validation

**Edge Cases to Add**:
```cue
edge_cases: [
    ..., // existing
    "File not found → clear error message with path shown",
    "Invalid CUE syntax → parse error with line number",
    "Answer key doesn't match any question → skip unknown keys, warn user",
    "Empty string answer → reject with suggestion to omit key",
    "Answer too long (>10KB) → warn about length",
    "Symlinked file → resolve and load from actual path",
    "File with BOM marker → handle UTF-8 BOM correctly",
]
```

**Testing**:
```gleam
// Test: File not found
assert_error!(
    answer_loader.load_answers_from_cue("/nonexistent/file.cue"),
    contains("not found")
)

// Test: Invalid CUE
assert_error!(
    answer_loader.load_answers_from_cue("/path/invalid.cue"),
    contains("syntax error")
)

// Test: Valid answers
let answers = answer_loader.load_answers_from_cue("/path/valid.cue")
assert!(dict.has_key(answers, "question_1"))
```

### 3.2 P1-T1.2: ask_single_question modification

**Current Bead**:
- **What**: "Update ask_single_question() to accept optional dict of pre-filled answers"
- **Why**: "Merge manual and automated answer modes seamlessly"

**EARS Enhancement**:
```
WHEN pre-filled answers dict is provided
THE SYSTEM SHALL check dict first for answer
WHERE answer exists and is valid
THE SYSTEM SHALL use dict answer
WHERE answer missing or invalid
IF strict_mode is true THEN THE SYSTEM SHALL fail with error
IF strict_mode is false THEN THE SYSTEM SHALL prompt user
```

**KIRK Preconditions**:
- ✅ ask_single_question() function exists and is callable
- ✅ Optional dict parameter is addable (Option type)
- ✅ Strict mode flag parameter is addable (bool)
- ✅ All existing callers must still compile (backwards compatibility)
- ✅ Question validation logic is available

**KIRK Postconditions**:
- ✅ Returns same Answer type (no signature breaking)
- ✅ If dict provided and contains answer → use dict answer
- ✅ If dict provided but answer missing and not strict → prompt user
- ✅ If dict provided but answer missing and strict → error with message
- ✅ If dict not provided → original behavior (prompt always)
- ✅ Answer is validated regardless of source (dict or prompt)
- ✅ All existing tests pass unchanged

**Inversion Analysis**:
- 🔴 **Critical**: Dict has wrong answer type → validation should reject
  - Mitigation: Validate answer against question type before use
- 🔴 **Critical**: Strict mode without dict → ambiguous behavior
  - Mitigation: Define clear semantics (what does strict mean with no dict?)
- 🟡 **High**: Dict lookup case-sensitivity → key matching
  - Mitigation: Document key naming (e.g., match schema exactly)
- 🟡 **High**: Concurrent dict access → thread safety
  - Mitigation: Dict is immutable (no thread safety issue)
- 🟢 **Medium**: Dict has extra answers (not in interview) → waste
  - Mitigation: Ignore extra keys, no error

**Edge Cases to Add**:
```cue
edge_cases: [
    ..., // existing
    "Dict provided but answer missing, strict=false → prompt user",
    "Dict provided but answer missing, strict=true → error with key",
    "Dict answer is wrong type (e.g. expected int got string) → error",
    "Dict contains extra answers (not in interview) → ignore gracefully",
    "Dict answer is empty string → error (no valid answer)",
    "Backward compatibility: no dict parameter → original behavior",
]
```

**Testing**:
```gleam
// Test: Dict lookup success
let dict = dict.from_list([#("q1", "answer1")])
let answer = ask_single_question(question, Some(dict), False)
assert_eq!(answer, "answer1")

// Test: Strict mode missing answer
let empty_dict = dict.new()
assert_error!(
    ask_single_question(question, Some(empty_dict), True),
    contains("missing")
)

// Test: Non-strict mode missing answer (should prompt)
// This requires mocking stdin - handle appropriately
```

### 3.3 P1-T1.3: intent.gleam CLI integration

**Enhancement**: Add `--strict` flag, document interaction with `--answers`

**EARS Pattern**:
```
WHEN --answers flag provided with file path
THE SYSTEM SHALL load answers from file
WHEN --strict flag provided
THE SYSTEM SHALL fail if any question missing answer
WHEN --answers omitted
THE SYSTEM SHALL prompt all questions (ignore --strict)
```

**KIRK Preconditions**:
- ✅ run_interview() function exists
- ✅ Flag parsing infrastructure exists (glint/flag)
- ✅ answer_loader module exists and is callable
- ✅ interview loop accepts answers dict and strict flag

**KIRK Postconditions**:
- ✅ Help text documents both `--answers` and `--strict`
- ✅ Answers loaded from file when `--answers` provided
- ✅ Strict mode enforced when `--strict` provided
- ✅ Clear error if `--answers` path invalid
- ✅ Integration tests cover all flag combinations

**Inversion Analysis**:
- 🔴 **Critical**: `--strict` without `--answers` → confusing
  - Mitigation: Warn or error if `--strict` used without `--answers`
- 🔴 **Critical**: File path resolution issues (relative vs absolute)
  - Mitigation: Normalize path early, show full resolved path in errors
- 🟡 **High**: Wrong file provided accidentally → load wrong answers
  - Mitigation: Show file being loaded with full path, ask confirmation
- 🟢 **Medium**: Help text clarity → UX
  - Mitigation: Include example in help: `--answers answers.cue --strict`

**Edge Cases to Add**:
```cue
edge_cases: [
    ..., // existing
    "--strict without --answers → error with helpful message",
    "File path is relative → resolve relative to cwd",
    "File path contains ~/ → expand to home directory",
    "--answers file.cue (file not in cwd) → try to resolve, show full path",
    "Empty answers file → load empty dict, not error",
]
```

### 3.4 P1-T1.4: bead_feedback.gleam

**EARS Pattern**:
```
WHEN bead execution completes
THE SYSTEM SHALL append feedback to CUE file
WHERE feedback is valid #BeadFeedback
THE SYSTEM SHALL ensure file remains valid CUE after append
WHILE multiple beads may finish concurrently
THE SYSTEM SHALL prevent race conditions
```

**KIRK Preconditions**:
- ✅ .intent/ directory exists (created by session)
- ✅ Session file exists and is readable
- ✅ Write permissions on .intent/ directory
- ✅ #BeadFeedback schema is defined
- ✅ Bead ID exists in session
- ✅ File locking mechanism available

**KIRK Postconditions**:
- ✅ #BeadFeedback appended to feedback file
- ✅ Append is atomic (all or nothing)
- ✅ File validates with `cue vet` after append
- ✅ Timestamp is ISO8601 UTC
- ✅ Duration_ms is accurate
- ✅ Error/blocked_by fields populated when relevant
- ✅ load_feedback() can parse via cue export

**Inversion Analysis**:
- 🔴 **Critical**: Concurrent writes to same file → corruption
  - Mitigation: Implement file locking (flock on Unix, LockFile on Windows)
- 🔴 **Critical**: Append fails mid-write → invalid file
  - Mitigation: Atomic append: write to temp file, sync, rename
- 🔴 **Critical**: Disk full during append → partial write
  - Mitigation: Check disk space before write, handle ENOSPC
- 🟡 **High**: Session doesn't exist → where to write feedback?
  - Mitigation: Error if session missing, suggest session creation
- 🟡 **High**: Bead ID doesn't exist → orphaned feedback
  - Mitigation: Validate bead ID before appending
- 🟢 **Medium**: Very large feedback file (1000+ entries) → performance
  - Mitigation: Consider archiving old feedback

**Edge Cases to Add**:
```cue
edge_cases: [
    ..., // existing
    "Concurrent appends (two beads finish simultaneously) → atomic writes prevent corruption",
    "Disk full during append → graceful error, preserve file integrity",
    "Session doesn't exist → clear error message",
    "Bead ID doesn't match any in session → warn but still append",
    "Timestamp precision → use nanoseconds, format ISO8601",
    "Error object present → validate error.type and error.message",
    "Blocked reason missing → error if result='blocked'",
]
```

**Testing**:
```gleam
// Test: Atomic append
let feedback = BeadFeedback(...)
assert_ok!(bead_feedback.mark_bead_executed(session_id, feedback))
let loaded = bead_feedback.load_feedback(session_id)
assert_contains!(loaded, feedback.bead_id)

// Test: CUE validation
let file_content = simplifile.read(".intent/feedback-abc123.cue")
assert_ok!(run_cue_vet(file_content))
```

### 3.5 P1-T1.5: bead-status command

**EARS Pattern**:
```
WHEN user runs: intent bead-status <id> --status <status> --reason <text>
THE SYSTEM SHALL validate bead ID exists
WHERE status is blocked THEN reason is required
WHERE status is not blocked THEN reason is optional
THE SYSTEM SHALL append feedback and show success
```

**KIRK Preconditions**:
- ✅ bead_feedback module exists
- ✅ Session ID can be inferred (from env or flag)
- ✅ Bead ID is valid format (WORD-NNN)
- ✅ Status enum is valid
- ✅ If status=blocked, reason is provided

**KIRK Postconditions**:
- ✅ Feedback appended to .intent/feedback-{session}.cue
- ✅ Success message shows: "Bead {id} marked as {status}"
- ✅ Exit code 0 on success, non-zero on error
- ✅ JSON output available for parsing

**Inversion Analysis**:
- 🔴 **Critical**: Invalid bead ID → silent failure
  - Mitigation: Validate format early, show error
- 🔴 **Critical**: Session doesn't exist → orphaned feedback
  - Mitigation: Require session ID or infer from env, validate it exists
- 🟡 **High**: Duplicate status updates (same bead marked twice) → confusing
  - Mitigation: Warn if bead already has status, show previous value
- 🟡 **High**: No reason for blocked bead → incomplete feedback
  - Mitigation: Enforce `--reason` for `--status blocked`
- 🟢 **Medium**: Output format → needs both human and JSON
  - Mitigation: Add `--json` flag for CI parsing

**Edge Cases to Add**:
```cue
edge_cases: [
    ..., // existing
    "Bead already marked as completed, try to mark failed → warn, update",
    "Status value invalid (typo) → error with valid options",
    "Session ID missing → infer from .intent/ or require --session",
    "--status blocked without --reason → error, show required",
    "Multiple --reason arguments → use last one (or error on ambiguity)",
]
```

### 3.6 P1-T1.6: beads-regenerate command

**EARS Pattern** (Complex):
```
WHEN beads have failed or are blocked
THE SYSTEM SHALL load execution feedback from .intent/
WHERE feedback explains the failure reason
THE SYSTEM SHALL analyze root cause (inversion, second-order)
THE SYSTEM SHALL generate new beads with adjusted approach
WHILE preserving successful beads unchanged
WHILE preventing circular dependencies in new beads
THE SYSTEM SHALL update session with new beads
```

**KIRK Preconditions**:
- ✅ Session file exists with beads
- ✅ Feedback file exists with execution results
- ✅ At least one failed or blocked bead exists
- ✅ Original successful beads are backed up
- ✅ AI/analysis logic available for root cause analysis

**KIRK Postconditions**:
- ✅ New beads generated with different IDs
- ✅ Session file updated with new beads
- ✅ Original successful beads preserved unchanged
- ✅ Dependency graph remains acyclic
- ✅ Regeneration reason logged in bead metadata
- ✅ Old beads marked as "superseded" or archived

**Inversion Analysis**:
- 🔴 **Critical**: Regeneration quality worse than original → regression
  - Mitigation: Compare new beads to old, show diff, require approval
- 🔴 **Critical**: New beads create circular dependencies → impossible to execute
  - Mitigation: Validate DAG after generation, reject if cyclic
- 🟡 **High**: No failed/blocked beads → should command error or no-op?
  - Mitigation: Either error clearly or silently no-op (document)
- 🟡 **High**: Root cause analysis missing → generate same failed beads again
  - Mitigation: Use inversion + pre-mortem to understand failure
- 🟢 **Medium**: History lost (can't see what was regenerated) → debugging
  - Mitigation: Keep original beads, mark as superseded

**Edge Cases to Add**:
```cue
edge_cases: [
    ..., // existing
    "No failed beads → error or no-op (document clearly)",
    "Regeneration creates same beads → detect, offer rollback",
    "New beads have circular dependencies → reject with error",
    "Backup of original beads → preserve for rollback",
    "Root cause: 'external service timeout' → regenerate with retry logic",
    "Root cause: 'missing file' → regenerate with file creation step",
]
```

**What is "Adjusted Approach"?**

This is intentionally vague, which is the problem. Define strategies:

```gleam
pub type RegenerationStrategy {
    // Failure analysis: use inversion to identify root cause
    InversionBasedAnalysis
    // Consequence tracing: use second-order to understand impacts
    SecondOrderAnalysis
    // Retrospective: use pre-mortem (imagine it failed differently, why?)
    PreMortemAnalysis
    // Empirical: human feedback (user explains what went wrong)
    HumanGuidedAnalysis
}

pub fn regenerate_bead(
    failed_bead: Bead,
    feedback: BeadFeedback,
    strategy: RegenerationStrategy
) -> Result(List(Bead), String) {
    case strategy {
        InversionBasedAnalysis -> {
            // What inversions were NOT covered?
            // Generate new beads to cover them
            let missing_inversions = find_uncovered_inversions(failed_bead)
            let new_beads = create_beads_for_inversions(missing_inversions)
            Ok(new_beads)
        }
        SecondOrderAnalysis -> {
            // What are second-order effects of this bead's failure?
            // Generate beads to test those effects
            let second_order = analyze_second_order_effects(failed_bead)
            let new_beads = create_beads_for_effects(second_order)
            Ok(new_beads)
        }
        PreMortemAnalysis -> {
            // Imagine this bead's failure cascades - what happens next?
            // Generate preventive beads
            let cascade_risks = pre_mortem_analysis(failed_bead)
            let new_beads = create_preventive_beads(cascade_risks)
            Ok(new_beads)
        }
        HumanGuidedAnalysis -> {
            // Ask human: why did it fail?
            // Regenerate based on feedback
            let explanation = prompt_user_for_failure_explanation(failed_bead)
            let new_beads = regenerate_from_explanation(failed_bead, explanation)
            Ok(new_beads)
        }
    }
}
```

### 3.7 P1-T1.7: plan_mode.gleam

**EARS Pattern**:
```
WHEN user requests execution plan
THE SYSTEM SHALL compute topological sort of beads by dependencies
WHERE beads can run in parallel without dependencies
THE SYSTEM SHALL group into phases
THE SYSTEM SHALL calculate effort and risk
THE SYSTEM SHALL output in human or JSON format
```

**KIRK Preconditions**:
- ✅ Session file loaded with beads array
- ✅ Each bead has valid requires array (bead IDs only)
- ✅ Dependency graph is acyclic (no circular dependencies)
- ✅ #ExecutionPlan schema is defined

**KIRK Postconditions**:
- ✅ ExecutionPlan computed from session.beads
- ✅ Phases are topologically ordered (dependencies satisfied)
- ✅ Parallel-safe beads identified (no mutual dependencies)
- ✅ Effort aggregated correctly
- ✅ Risk calculated (low/medium/high/critical)
- ✅ Human format readable, JSON matches schema

**Inversion Analysis**:
- 🔴 **Critical**: Circular dependencies in beads → infinite loop
  - Mitigation: Detect cycles upfront, error with cycle path
- 🔴 **Critical**: Very large dependency graph (100+ beads) → performance
  - Mitigation: Use efficient topological sort (Kahn's)
- 🟡 **High**: Bead requires non-existent bead → missing dependency
  - Mitigation: Validate all bead IDs before processing
- 🟡 **High**: No beads in session → empty plan
  - Mitigation: Generate empty plan or error (document choice)
- 🟢 **Medium**: Human plan rendering, large graphs are hard to read
  - Mitigation: Tree format, truncate if >100 beads

**Edge Cases to Add**:
```cue
edge_cases: [
    ..., // existing
    "Circular dependencies (A → B → C → A) → detect and error with cycle",
    "100+ beads → efficient sort, consider pagination",
    "Bead requires non-existent ID → validation error",
    "Empty session (no beads) → empty plan vs error",
    "Single bead (no dependencies) → phase with 1 bead",
    "All beads parallel → single phase with all",
]
```

### 3.8 P1-T1.8: plan command

**EARS Pattern**:
```
WHEN user runs: intent plan <session_id> --format human|json
THE SYSTEM SHALL load session file
THE SYSTEM SHALL compute ExecutionPlan
THE SYSTEM SHALL display in requested format
```

**KIRK Preconditions**:
- ✅ plan_mode module exists
- ✅ Session ID is valid
- ✅ Format flag is provided (default: human)
- ✅ Session file exists and is readable

**KIRK Postconditions**:
- ✅ Plan loaded from .intent/session-{id}.cue
- ✅ Human format uses ASCII tree, shows effort/risk
- ✅ JSON format validates against #ExecutionPlan schema
- ✅ Exit code 0 on success, non-zero on error

**Edge Cases to Add**:
```cue
edge_cases: [
    ..., // existing
    "Session doesn't exist → clear error with path",
    "Format invalid (typo) → show valid options",
    "Plan is very large (200+ beads) → ask about pagination or summary",
    "--json on large plan → stream output or chunk",
]
```

---

## SECTION 4: IMPLEMENTATION ROADMAP

### 4.1 Epic Structure

```
MENTAL-LATTICE-INTEGRATION (Epic)
├── Phase 0: SCHEMA EXTENSIONS (Foundation)
│   ├── [ML-SCHEMA-1] Add #BeadKirkMetadata to schema/beads.cue
│   ├── [ML-SCHEMA-2] Add #BeadEARS, #BeadContract, #BeadInversion types
│   ├── [ML-SCHEMA-3] Add #AnswerMap schema for answer loader
│   └── [ML-SCHEMA-4] Validate schema compiles and matches spec
│
├── Phase 1A: P1-T1 ENHANCEMENTS (Enrich Existing Beads)
│   ├── [ML-P1A-1] Analyze qnf (answer_loader) through mental models
│   ├── [ML-P1A-2] Add edge cases and KIRK metadata to qnf
│   ├── [ML-P1A-3] Analyze f5y through mental models
│   ├── [ML-P1A-4] Add edge cases and KIRK metadata to f5y
│   ├── [ML-P1A-5] Analyze xk8, 18a, bto, ozf, 8sg, woq (same pattern)
│   └── [ML-P1A-12] Create test beads for all P1-T1 beads
│
├── Phase 1B: GENERATION PIPELINE (Tool Support)
│   ├── [ML-GEN-1] Create src/intent/bead_generator.gleam
│   ├── [ML-GEN-2] Implement behavior_to_bead() function
│   ├── [ML-GEN-3] Integrate EARS validation (ears_parser)
│   ├── [ML-GEN-4] Integrate inversion analysis (inversion_checker)
│   ├── [ML-GEN-5] Integrate KIRK contract extraction
│   ├── [ML-GEN-6] Implement quality scoring
│   ├── [ML-GEN-7] Implement regenerate_bead() for problem beads
│   └── [ML-GEN-8] Add 'beads-generate' command to intent.gleam
│
├── Phase 2: VALIDATION GATES (Quality Assurance)
│   ├── [ML-VAL-1] Create src/intent/bead_validator.gleam
│   ├── [ML-VAL-2] Implement validate_bead_kirk() function
│   ├── [ML-VAL-3] Implement EARS pattern validation
│   ├── [ML-VAL-4] Implement inversion coverage check
│   ├── [ML-VAL-5] Implement quality threshold enforcement
│   ├── [ML-VAL-6] Add 'analyze-beads' command to intent.gleam
│   └── [ML-VAL-7] CLI shows validation results with suggestions
│
├── Phase 3: SECOND-ORDER & PRE-MORTEM (Risk Management)
│   ├── [ML-2ND-1] Create src/intent/second_order_analyzer.gleam
│   ├── [ML-2ND-2] Create src/intent/premortem_analyzer.gleam
│   ├── [ML-2ND-3] Integrate into bead_generator for regeneration strategy
│   ├── [ML-2ND-4] Add 'effects' command to show second-order consequences
│   └── [ML-2ND-5] Add 'premortem' command to show risk analysis
│
├── Phase 4: FEEDBACK-DRIVEN REGENERATION (Smart Improvement)
│   ├── [ML-REGEN-1] Enhance beads-regenerate to use mental models
│   ├── [ML-REGEN-2] Implement root cause analysis from feedback
│   ├── [ML-REGEN-3] Use inversion to identify uncovered failure modes
│   ├── [ML-REGEN-4] Use second-order to find systemic impacts
│   ├── [ML-REGEN-5] Use pre-mortem to prevent cascading failures
│   └── [ML-REGEN-6] Test regeneration quality on real failed beads
│
└── Phase 5: DOCUMENTATION & INTEGRATION (Completion)
    ├── [ML-DOC-1] Document Mental Lattice workflow in README
    ├── [ML-DOC-2] Create examples/mental-lattice/ with worked examples
    ├── [ML-DOC-3] Add Mental Lattice tutorial (interactive)
    ├── [ML-DOC-4] Document best practices for bead quality
    └── [ML-DOC-5] Create mental-lattice.md API reference
```

### 4.2 Dependency Graph

```
Schema Foundation
    ↓
ML-SCHEMA-1,2,3,4
    ↓
P1 Enhancement & Generation Pipeline (can proceed in parallel)
    ↙              ↘
ML-P1A-1..12    ML-GEN-1..8
    ↓              ↓
ML-VAL-1..7 ←────┘

Validation Gates ready
    ↓
ML-2ND-1,2,3
ML-REGEN-1..6 (depends on validation working)
    ↓
ML-DOC-1..5

Legend:
→ : Depends on
↓ : Sequential
↙ ↘ : Can run in parallel
```

---

## SECTION 5: CRITICAL QUESTIONS FOR USER

Before proceeding to implementation, I need your guidance on these:

### 5.1 Regeneration Strategy Definition

**Question**: When a bead fails, how should regeneration decide on the "adjusted approach"?

**Options**:
1. **Inversion-driven**: Use inversion_checker to find uncovered failure modes, generate new beads
2. **Second-order-driven**: Analyze consequences of failure, generate defensive beads
3. **Pre-mortem-driven**: Imagine worst-case failure cascade, generate preventive beads
4. **Hybrid**: Try all three, merge results
5. **Human-guided**: Ask user to explain failure, regenerate based on explanation

**Impact**: This determines how smart beads-regenerate is and how much it relies on AI vs human input.

### 5.2 Quality Threshold

**Question**: What quality score should beads achieve before being executable?

**Options**:
1. **Strict (80+)**: Only high-quality beads allowed, enforced validation gate
2. **Moderate (70+)**: Good quality, some issues permitted
3. **Lenient (60+)**: Beads are executable even with some gaps
4. **No gate**: Generate beads, show quality scores, let user decide

**Impact**: This affects how many beads fail validation and need regeneration.

### 5.3 Backwards Compatibility

**Question**: Should Phase 1 beads (qnf, f5y, xk8, etc.) be updated retroactively or kept as-is?

**Options**:
1. **Full enhancement**: Update all Phase 1 beads with KIRK metadata and edge cases now
2. **Gradual**: Mark Phase 1 beads as "legacy", generate new versions with mental models
3. **Optional**: Phase 1 beads remain simple, Mental Lattice is opt-in via `--with-analysis`

**Impact**: Determines scope of immediate work vs technical debt.

### 5.4 Schema Versioning

**Question**: How should we handle schema evolution (e.g., if #BeadKirkMetadata fields change)?

**Options**:
1. **Version in bead**: Add `schema_version: "1.0"` to every bead
2. **Global version**: Single schema version in session file
3. **No versioning**: Assume schema never changes (immutability)
4. **Lazy evolution**: Update beads on load if old schema detected

**Impact**: Affects long-term maintainability and migration strategy.

### 5.5 Output Formats

**Question**: When displaying mental model analysis (inversion, pre-mortem, quality), what formats?

**Options**:
1. **CLI tables**: Human-readable, great for terminal
2. **JSON**: Machine-parseable, better for tools/CI
3. **Markdown**: Documentation-ready, good for reports
4. **All three**: Support all formats via `--format` flag

**Impact**: Determines UX and integrations.

---

## SECTION 6: SUCCESS CRITERIA

### 6.1 Functional Success

- ✅ All 10 Phase 1 beads have complete KIRK metadata
- ✅ EARS validation works: beads parse as valid EARS patterns
- ✅ Inversion analysis works: finds 80%+ of security/usability risks
- ✅ Quality scoring works: identifies weak beads (clarity, testability gaps)
- ✅ Gap detection works: suggests missing beads for untested scenarios
- ✅ bead-generator works: creates valid beads from CUE specs
- ✅ validate_bead_kirk works: enforces quality threshold
- ✅ beads-regenerate works: improves failed beads using mental models

### 6.2 Quality Success

- ✅ Phase 1 beads overall quality score: 85+
- ✅ Zero circular dependencies in any bead set
- ✅ All done_when criteria are observable and testable
- ✅ All edge_cases are covered by test assertions
- ✅ KIRK preconditions and postconditions are explicit

### 6.3 Integration Success

- ✅ EARS parser used in bead generation ✓
- ✅ Inversion checker used in bead validation ✓
- ✅ Quality analyzer used in quality scoring ✓
- ✅ Gap detector used in gap analysis ✓
- ✅ Compact format used in AI prompts ✓

### 6.4 User Experience Success

- ✅ CLI commands are intuitive: `intent analyze-beads`, `intent beads-generate`
- ✅ Error messages are actionable and helpful
- ✅ Performance is acceptable (even for 100+ beads)
- ✅ Documentation is clear and includes examples

---

## SECTION 7: RISKS & MITIGATIONS

### Risk 1: Schema Extensions Break Existing Code
**Mitigation**: Make kirk field optional; all existing code works without it

### Risk 2: Regeneration Creates Worse Beads
**Mitigation**: Implement quality comparison; show diff to user before applying

### Risk 3: Mental Models Add Too Much Complexity
**Mitigation**: Progressive rollout; users can ignore mental model output initially

### Risk 4: Performance Issues with Large Bead Sets
**Mitigation**: Benchmark; use efficient algorithms (O(V+E) topological sort)

### Risk 5: False Positives in Inversion Checking
**Mitigation**: Document which inversions are auto-detected vs manual; allow override

---

## CONCLUSION

This plan provides a **systematic pathway** to integrate Mental Lattices into every bead, transforming Intent from a task tracker into a formally-verified planning system.

**Key Achievement**: Every bead will have:
1. **EARS clarity**: Requirement is unambiguous (THE SYSTEM SHALL...)
2. **KIRK contracts**: Pre/post/invariants are explicit
3. **Inversion coverage**: Failure modes are identified and tested
4. **Quality score**: Multi-dimensional quality is measurable
5. **Gap awareness**: Missing tests and edge cases are flagged
6. **Regeneration strategy**: Failed beads improve using mental models

**Timeline**: The roadmap is designed to be executed incrementally:
- **Phase 0** (Schema): 1-2 days
- **Phase 1A** (P1 Enhancement): 3-4 days
- **Phase 1B** (Generation): 4-5 days
- **Phase 2** (Validation): 2-3 days
- **Phase 3-5** (Advanced): 5-7 days

**Total estimated effort**: 15-22 days of focused development.

Would you like me to proceed with implementation, starting with any specific phase?
