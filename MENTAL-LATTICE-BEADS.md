# Mental Lattice Framework: Bead Breakdown

Strategic Decisions Encoded in CUE:
- **Regeneration**: Hybrid (inversion + second-order + pre-mortem)
- **Quality Threshold**: 80+ required for executable
- **P1 Approach**: Full enhancement now (no legacy)
- **Schema Versioning**: No versioning (immutable)
- **Output Formats**: All via --format flag (human, json, markdown)

---

## PHASE 0: SCHEMA EXTENSIONS (Foundation)

These beads extend the CUE schema to support Mental Lattice metadata on beads.

### [ML-SCHEMA-1] Add #BeadKirkMetadata to schema/beads.cue

**WHAT**: Add optional `kirk?: #BeadKirkMetadata` field to #Bead type in schema/beads.cue

**WHY**: Enable beads to carry mental model validation metadata without breaking existing beads

**EFFORT**: 5min

**DONE WHEN**:
- `kirk` field added as optional to #Bead
- CUE schema compiles without errors
- Backwards compatibility preserved (existing beads without kirk field still valid)

**TEST**:
```gleam
command: "cue vet schema/beads.cue"
expect: {
  exit_code: 0
  stderr_empty: true
}
```

**EDGE CASES**:
- Beads without kirk field must still validate
- kirk field is completely optional
- No default values for kirk subfields

**FILES**: schema/beads.cue

**DEPENDS ON**: none

---

### [ML-SCHEMA-2] Add EARS, Contract, Inversion, Quality types

**WHAT**: Add #BeadEARS, #BeadContract, #BeadInversion, #BeadQuality types to schema/beads.cue

**WHY**: Provide structured types for mental model metadata

**EFFORT**: 10min

**DONE WHEN**:
- All 4 types defined with proper constraints
- Type relationships are clear and documented
- Examples show how to use each type

**TEST**:
```gleam
command: "cue vet schema/beads.cue"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Clarity_score must be 0-100 (int)
- Risk severity must be one of: low, medium, high, critical
- Scores must not exceed 100 or be negative

**FILES**: schema/beads.cue

**DEPENDS ON**: [ML-SCHEMA-1]

---

### [ML-SCHEMA-3] Add RegenerationStrategy and BeadKirkAudit types

**WHAT**: Add #RegenerationStrategy and #BeadKirkAudit types for regeneration tracking and audit

**WHY**: Track how/when beads were improved when they fail

**EFFORT**: 5min

**DONE WHEN**:
- Both types defined with clear enum values
- Audit trail captures analyzer, timestamp, schema version
- Strategy indicates how regeneration was done

**TEST**:
```gleam
command: "cue vet schema/beads.cue"
expect: {
  exit_code: 0
}
```

**FILES**: schema/beads.cue

**DEPENDS ON**: [ML-SCHEMA-2]

---

### [ML-SCHEMA-4] Add MentalLatticeConfig type

**WHAT**: Add #MentalLatticeConfig type that encodes the 5 strategic decisions

**WHY**: Provide a single source of truth for how mental lattice tools should behave

**EFFORT**: 5min

**DONE WHEN**:
- Config type has all 5 strategic decision fields
- Each field has correct enum values
- Comments explain what each decision means

**TEST**:
```gleam
command: "cue vet schema/beads.cue && cue eval -c schema/beads.cue"
expect: {
  exit_code: 0
}
```

**FILES**: schema/beads.cue

**DEPENDS ON**: [ML-SCHEMA-3]

---

### [ML-SCHEMA-5] Create schema/mental-lattice-config.cue with Phase 1 enhancements

**WHAT**: Create new file with concrete KIRK metadata for all 8 Phase 1 beads

**WHY**: Document the mental lattice analysis of Phase 1 and make it machine-readable

**EFFORT**: 15min

**DONE WHEN**:
- All 8 Phase 1 beads have complete KIRK metadata
- Each has EARS pattern, contract, inversion risks, quality scores
- File is valid CUE and validates against schema

**TEST**:
```gleam
command: "cue vet schema/mental-lattice-config.cue schema/beads.cue"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Each bead must have audit field with analyzed_at timestamp
- Quality scores must add up correctly
- Edge cases must be specific and actionable

**FILES**: schema/mental-lattice-config.cue (new)

**DEPENDS ON**: [ML-SCHEMA-4]

---

## PHASE 1A: PHASE 1 BEAD ENHANCEMENT (Mental Lattice Analysis)

These beads update the actual Phase 1 automation beads (qnf, f5y, xk8, etc.) based on mental lattice analysis.

### [ML-P1A-1] Enhance [P1-T1.1] answer_loader.gleam with KIRK metadata

**WHAT**: Update the original qnf bead with:
- EARS pattern validation
- Preconditions (file exists, readable, valid CUE, schema match)
- Postconditions (returns Dict, values trimmed, errors descriptive)
- Inversion analysis (70% edge case coverage)
- Suggested edge cases (file not found, invalid CUE, schema mismatch, unicode, symlinks)
- Quality scores (overall: 93)

**WHY**: Ensure bead meets 80+ quality threshold and has explicit contracts

**EFFORT**: 10min

**DONE WHEN**:
- Original qnf bead updated with complete KIRK metadata
- Edge cases list expanded from ? to 10+ specific cases
- Quality score calculated and documented (93)
- File compiles and validates

**TEST**:
```bash
command: "cue vet .intent/*.cue schema/*.cue && bd show intent-cli-qnf | grep kirk"
expect: {
  exit_code: 0
  stdout_contains: "ears"
}
```

**EDGE CASES**:
- File not found error message must be clear
- Invalid CUE parse errors must include line number
- Unknown answer keys should warn but not fail
- Empty strings should be rejected

**FILES**: Updated bead in .beads/ (via bd update command)

**DEPENDS ON**: [ML-SCHEMA-5]

---

### [ML-P1A-2] Enhance [P1-T1.2] ask_single_question modification

**WHAT**: Update f5y bead with:
- EARS pattern (optional: check dict first, fallback to prompt)
- Preconditions (function exists, dict parameter addable, backwards compatible)
- Postconditions (returns same Answer type, validates regardless of source)
- Inversion risks (75% edge case coverage)
- Suggested edge cases (dict answer missing, wrong type, strict mode edge cases)
- Quality scores (overall: 87)

**WHY**: Ensure bead handles backwards compatibility and edge cases correctly

**EFFORT**: 10min

**DONE WHEN**:
- Original f5y bead updated with KIRK metadata
- Edge cases expanded to cover dict/prompt interaction
- Quality issues documented (strict mode semantics unclear)
- File compiles

**TEST**:
```bash
command: "cue vet .intent/*.cue schema/*.cue"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Dict provided, answer missing, strict=false → must prompt
- Dict provided, answer missing, strict=true → must error
- Dict answer wrong type → validation must fail
- Dict with extra answers → must ignore gracefully
- Backwards compatibility: no dict → original behavior

**FILES**: Updated bead in .beads/

**DEPENDS ON**: [ML-SCHEMA-5]

---

### [ML-P1A-3] Enhance [P1-T1.3] intent.gleam CLI integration

**WHAT**: Update xk8 bead with:
- EARS pattern (event-driven)
- Preconditions/postconditions
- Inversion risks (70% edge case coverage)
- Suggested edge cases (--strict without --answers is error, path resolution)
- Quality scores (overall: 83)
- Note: Flag interaction needs clear documentation

**WHY**: Ensure CLI flags work correctly and interactions are documented

**EFFORT**: 10min

**DONE WHEN**:
- Original xk8 bead updated with KIRK metadata
- Edge cases include flag interactions
- Quality issues note missing documentation
- Compiles

**TEST**:
```bash
command: "cue vet .intent/*.cue schema/*.cue"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- --strict without --answers → error (not silent fail)
- Relative file paths → resolve from cwd
- ~ expansion → handle home directory
- Empty answers file → load empty dict (not error)

**FILES**: Updated bead in .beads/

**DEPENDS ON**: [ML-SCHEMA-5]

---

### [ML-P1A-4] Enhance [P1-T1.4] bead_feedback.gleam

**WHAT**: Update 18a bead with:
- EARS pattern (ubiquitous)
- Preconditions (directory exists, writable, session valid)
- Postconditions (atomic append, valid CUE, ISO8601 timestamp)
- Inversion risks (65% edge case coverage - CRITICAL: concurrency)
- Suggested edge cases (concurrent writes, disk full, session missing)
- Quality scores (overall: 87)
- CRITICAL ISSUE FLAGGED: File locking strategy not specified

**WHY**: Ensure bead handles concurrent append safely - this is CRITICAL

**EFFORT**: 15min

**DONE WHEN**:
- Original 18a bead updated with KIRK metadata
- Edge cases emphasize concurrent write protection
- Quality issue flags missing file locking specification
- Edge case for atomic append (no partial writes)
- Compiles

**TEST**:
```bash
command: "cue vet .intent/*.cue schema/*.cue"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Concurrent appends → atomic writes prevent corruption
- Disk full → graceful error, preserve file
- Session missing → clear error message
- Bead ID doesn't exist in session → warn but append anyway
- Very large feedback file → performance test with 1000+ entries

**FILES**: Updated bead in .beads/

**DEPENDS ON**: [ML-SCHEMA-5]

---

### [ML-P1A-5] Enhance [P1-T1.5] bead-status command

**WHAT**: Update bto bead with:
- EARS pattern (event-driven)
- Preconditions/postconditions
- Inversion risks (75% edge case coverage)
- Suggested edge cases (invalid ID, missing session, duplicate updates)
- Quality scores (overall: 87)

**WHY**: Ensure CLI command validates input and shows clear errors

**EFFORT**: 10min

**DONE WHEN**:
- Original bto bead updated with KIRK metadata
- Edge cases include all validation scenarios
- Quality score documented
- Compiles

**TEST**:
```bash
command: "cue vet .intent/*.cue schema/*.cue"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Bead already marked completed → warn, update anyway
- Invalid status value → error with valid options
- --status blocked without --reason → error
- Session missing → clear error

**FILES**: Updated bead in .beads/

**DEPENDS ON**: [ML-SCHEMA-5]

---

### [ML-P1A-6] Enhance [P1-T1.6] beads-regenerate command

**WHAT**: Update ozf bead with:
- EARS pattern (complex - multiple conditions)
- Preconditions/postconditions
- Inversion risks (60% edge case coverage - HIGH RISK)
- Suggested edge cases (no failed beads, circular deps, quality regression)
- Quality scores (overall: 67 - LOW, below 80 threshold)
- CRITICAL ISSUES: Vague "adjusted approach", no rollback, testing strategy missing

**WHY**: This is the MOST COMPLEX bead - mental lattice reveals it needs more work

**EFFORT**: 20min

**DONE WHEN**:
- Original ozf bead updated with KIRK metadata
- Quality issues clearly documented (only 67, below 80 threshold)
- Suggested strategies for regeneration spelled out
- Flag: This bead FAILS quality gate (needs Phase 1B work before executable)
- Compiles

**TEST**:
```bash
command: "cue vet .intent/*.cue schema/*.cue"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- No failed beads → error (document choice)
- Regeneration worse than original → compare, require approval
- New beads have circular deps → reject
- Backup of original beads → preserve for rollback
- Root cause: external timeout → regenerate with retry logic

**FILES**: Updated bead in .beads/

**DEPENDS ON**: [ML-SCHEMA-5]

---

### [ML-P1A-7] Enhance [P1-T1.7] plan_mode.gleam

**WHAT**: Update 8sg bead with:
- EARS pattern (ubiquitous)
- Preconditions/postconditions
- Inversion risks (85% edge case coverage)
- Suggested edge cases (circular deps, large graphs, missing IDs)
- Quality scores (overall: 93 - HIGH)

**WHY**: Well-defined algorithmic task - should pass quality gate easily

**EFFORT**: 10min

**DONE WHEN**:
- Original 8sg bead updated with KIRK metadata
- Quality score 93 documented
- Edge cases include all algorithmic edge cases
- Compiles

**TEST**:
```bash
command: "cue vet .intent/*.cue schema/*.cue"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Circular dependencies (A → B → C → A) → error with cycle path
- 100+ beads → efficient topological sort
- Missing bead ID in requires → validation error
- Empty session → empty plan (not error)

**FILES**: Updated bead in .beads/

**DEPENDS ON**: [ML-SCHEMA-5]

---

### [ML-P1A-8] Enhance [P1-T1.8] plan command

**WHAT**: Update woq bead with:
- EARS pattern (event-driven)
- Preconditions/postconditions
- Inversion risks (85% edge case coverage)
- Suggested edge cases (missing session, invalid format)
- Quality scores (overall: 93 - HIGH)

**WHY**: Simple wrapper command - should pass quality gate

**EFFORT**: 10min

**DONE WHEN**:
- Original woq bead updated with KIRK metadata
- Quality score 93 documented
- Edge cases documented
- Compiles

**TEST**:
```bash
command: "cue vet .intent/*.cue schema/*.cue"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Session doesn't exist → clear error with path
- Invalid format → show valid options
- Large plan (200+ beads) → pagination or summary
- Terminal colors detection for human format

**FILES**: Updated bead in .beads/

**DEPENDS ON**: [ML-SCHEMA-5]

---

## PHASE 1B: CRITICAL GAP FIXES

These beads address the gaps revealed by mental lattice analysis. They should be done BEFORE Phase 1 can be executed safely.

### [ML-GAP-1] CRITICAL: Specify file locking strategy for bead_feedback

**WHAT**: Design and document the file locking approach for concurrent append in 18a (bead_feedback.gleam)

**WHY**: Concurrent appends cause file corruption - this is critical

**EFFORT**: 15min

**DONE WHEN**:
- Design document created explaining locking strategy
- Chosen: flock (Unix), LockFile (Windows), or OS-agnostic library?
- Implementation guidance provided
- Edge case: what if lock acquisition times out?

**TEST**:
```bash
command: "grep -i 'lock\\|atomic' design-doc.md | wc -l"
expect: {
  stdout_contains: "5"  // At least 5 mentions
}
```

**EDGE CASES**:
- Lock acquisition timeout
- Stale locks (process crashed while holding)
- Lock file permissions issues
- Cross-process lock coordination

**FILES**: design/file-locking.md (new)

**DEPENDS ON**: [ML-P1A-4]

---

### [ML-GAP-2] CRITICAL: Define regeneration strategies for beads-regenerate

**WHAT**: Design the 4 regeneration strategies (inversion, second-order, pre-mortem, hybrid)

**WHY**: ozf bead says "adjusted approach" but doesn't define what that means

**EFFORT**: 20min

**DONE WHEN**:
- Strategy guide created with clear definitions
- For each strategy: inputs, algorithm, outputs explained
- Hybrid strategy: how are 3 approaches combined?
- Approval workflow documented

**TEST**:
```bash
command: "grep -i 'strategy\\|algorithm' design-doc.md | wc -l"
expect: {
  stdout_contains: "10"
}
```

**EDGE CASES**:
- Multiple strategies generate conflicting beads
- No failed beads (what does regenerate do?)
- Regeneration produces worse beads (quality regression)
- Circular dependencies in regenerated beads

**FILES**: design/regeneration-strategies.md (new)

**DEPENDS ON**: [ML-P1A-6]

---

### [ML-GAP-3] Create quality validation gates

**WHAT**: Implement validate_bead_kirk() function in Gleam

**WHY**: Enforce 80+ quality threshold before bead execution

**EFFORT**: 20min

**DONE WHEN**:
- Function exists in src/intent/bead_validator.gleam
- Checks: EARS pattern valid, preconditions present, quality score >=80
- Returns Result(Nil, List(ValidationIssue))
- Tested on all 8 Phase 1 beads

**TEST**:
```bash
command: "gleam build && gleam test"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Bead without KIRK metadata → validation passes (optional)
- Bead with quality 79 → fails (below 80)
- Quality issue with severity="error" → fails validation

**FILES**: src/intent/bead_validator.gleam (new)

**DEPENDS ON**: [ML-SCHEMA-5]

---

### [ML-GAP-4] Create EARS parser integration for beads

**WHAT**: Integrate existing ears_parser module to validate bead "what" field

**WHY**: Ensure requirement clarity is enforced

**EFFORT**: 15min

**DONE WHEN**:
- Function validate_bead_ears() uses ears_parser.parse()
- Checks pattern matches test specification
- Extracts trigger/condition/state components
- Calculates clarity score

**TEST**:
```bash
command: "gleam test -- test_validate_bead_ears"
expect: {
  exit_code: 0
  stdout_contains: "passed"
}
```

**EDGE CASES**:
- Bead "what" doesn't match any EARS pattern
- Pattern matches test but clarity score too low
- Trigger/condition/state extraction fails

**FILES**: src/intent/bead_validator.gleam

**DEPENDS ON**: [ML-SCHEMA-5], [ML-GAP-3]

---

### [ML-GAP-5] Create inversion coverage checker for beads

**WHAT**: Use inversion_checker to validate bead edge cases

**WHY**: Ensure edge cases cover identified risks

**EFFORT**: 15min

**DONE WHEN**:
- Function analyze_bead_inversions() uses inversion_checker
- Identifies security/usability/integration risks
- Calculates edge case coverage %
- Returns suggested edge cases

**TEST**:
```bash
command: "gleam test -- test_analyze_bead_inversions"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Bead with no identified risks (coverage = 100%)
- Inversion checker finds risks but no edge cases exist
- Multiple risks, only some are covered

**FILES**: src/intent/bead_validator.gleam

**DEPENDS ON**: [ML-SCHEMA-5], [ML-GAP-3]

---

## PHASE 2: BEAD GENERATION PIPELINE

These beads implement the iterative generation loop: generate → validate → refine.

### [ML-GEN-1] Create bead_generator.gleam module

**WHAT**: Implement behavior_to_bead() function to convert CUE behaviors to beads

**WHY**: Automate bead creation from specifications

**EFFORT**: 20min

**DONE WHEN**:
- Module created with behavior_to_bead() function
- Maps behavior.intent to EARS-formatted "what"
- Generates done_when criteria from behavior checks
- Infers implementation file from feature name
- Tests pass on example specs

**TEST**:
```bash
command: "gleam test -- test_behavior_to_bead"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Behavior with no checks → minimal done_when
- API behavior vs database behavior vs validation
- Feature name to file inference ambiguity

**FILES**: src/intent/bead_generator.gleam (new)

**DEPENDS ON**: [ML-GAP-3]

---

### [ML-GEN-2] Integrate EARS validation into generation

**WHAT**: Use EARS parser in bead_generator to validate generated "what" field

**WHY**: Ensure generated beads have clear EARS patterns

**EFFORT**: 10min

**DONE WHEN**:
- Generation validates EARS pattern
- Clarity score calculated
- Low clarity triggers regeneration
- Tests pass

**TEST**:
```bash
command: "gleam test -- test_ears_validation_in_generation"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Generated "what" fails EARS check → needs rewrite
- Pattern identified but clarity < 75 → rewrite
- Multiple patterns possible → choose best

**FILES**: src/intent/bead_generator.gleam

**DEPENDS ON**: [ML-GEN-1], [ML-GAP-4]

---

### [ML-GEN-3] Integrate inversion analysis into generation

**WHAT**: Run inversion_checker on each generated bead

**WHY**: Identify missing edge cases automatically

**EFFORT**: 10min

**DONE WHEN**:
- Generation runs inversion analysis
- Suggests additional edge cases
- Adds to bead.edge_cases automatically
- Tests pass

**TEST**:
```bash
command: "gleam test -- test_inversion_in_generation"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- No identified inversions → edge_case_coverage 100%
- Many inversions but few edge cases → coverage low
- Suggested edge case already exists → dedup

**FILES**: src/intent/bead_generator.gleam

**DEPENDS ON**: [ML-GEN-1], [ML-GAP-5]

---

### [ML-GEN-4] Quality scoring in generation

**WHAT**: Calculate quality scores for generated beads

**WHY**: Identify weak beads early

**EFFORT**: 10min

**DONE WHEN**:
- Quality module calculates completeness/testability/clarity
- Overall score is weighted average
- Issues identified and logged
- Tests pass

**TEST**:
```bash
command: "gleam test -- test_quality_scoring"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Perfect bead → quality 100
- Minimal bead → quality < 80 (fails threshold)
- Missing preconditions → quality penalty

**FILES**: src/intent/bead_generator.gleam

**DEPENDS ON**: [ML-GEN-3]

---

### [ML-GEN-5] Regenerate problem beads in generation loop

**WHAT**: For beads with quality < 80, automatically regenerate with improvements

**WHY**: Iteratively improve beads until they meet quality threshold

**EFFORT**: 20min

**DONE WHEN**:
- Function regenerate_bead_from_issues() exists
- Rewrites "what" with EARS patterns
- Expands done_when from test
- Adds missing edge cases from inversion
- Re-validates and recalculates quality
- Tests show iteration improves score

**TEST**:
```bash
command: "gleam test -- test_regenerate_problem_beads"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- Regeneration still doesn't reach 80 → needs human intervention
- Circular improvement (same issue regenerated) → detect and stop
- Max iterations reached → fail with message

**FILES**: src/intent/bead_generator.gleam

**DEPENDS ON**: [ML-GEN-4]

---

### [ML-GEN-6] Create 'beads-generate' command

**WHAT**: Add CLI command: intent beads-generate <spec.cue> → generates beads

**WHY**: Enable automation of bead creation from specs

**EFFORT**: 15min

**DONE WHEN**:
- Command exists in intent.gleam
- Reads spec file
- Generates beads using full pipeline (generate → validate → refine)
- Outputs session with beads
- Tests cover error cases

**TEST**:
```bash
command: "intent beads-generate examples/user-api.cue | jq .beads | length"
expect: {
  stdout_contains: "[0-9]"  // At least one bead
}
```

**EDGE CASES**:
- Spec file not found → clear error
- Spec invalid CUE → parse error
- Spec has no behaviors → empty beads list
- Very large spec → performance acceptable

**FILES**: src/intent.gleam

**DEPENDS ON**: [ML-GEN-5]

---

## PHASE 3: VALIDATION & ANALYSIS COMMANDS

These beads add new CLI commands for analyzing and validating beads.

### [ML-VAL-1] Create 'analyze-beads' command

**WHAT**: Add CLI: intent analyze-beads <session_id> → shows mental lattice analysis

**WHY**: Display KIRK metadata, quality scores, and gaps to user

**EFFORT**: 15min

**DONE WHEN**:
- Command loads session
- Runs all validators (EARS, inversion, quality)
- Displays results in human/json/markdown format (--format flag)
- Shows gaps and improvement suggestions

**TEST**:
```bash
command: "intent analyze-beads <session_id> --format human | grep -i quality"
expect: {
  stdout_contains: "quality"
}
```

**EDGE CASES**:
- Session doesn't exist → clear error
- No beads in session → empty output
- Very large output → pagination
- --format invalid → error

**FILES**: src/intent.gleam

**DEPENDS ON**: [ML-GAP-3]

---

### [ML-VAL-2] Add --with-analysis flag to beads command

**WHAT**: Update existing `intent beads` command to show analysis by default

**WHY**: Make mental lattice output available when displaying beads

**EFFORT**: 10min

**DONE WHEN**:
- Default output shows basic KIRK metadata
- --detailed shows full analysis
- Quality score displayed for each bead
- Tests pass

**TEST**:
```bash
command: "intent beads <id> | grep -i quality"
expect: {
  stdout_contains: "quality"
}
```

**EDGE CASES**:
- Beads without KIRK metadata → gracefully omitted
- Quality threshold not met → warning
- Very large analysis → truncate intelligently

**FILES**: src/intent.gleam

**DEPENDS ON**: [ML-VAL-1]

---

## PHASE 4: ADVANCED MENTAL MODELS (Second-Order & Pre-Mortem)

### [ML-2ND-1] Create second_order_analyzer.gleam

**WHAT**: Implement second-order consequence analysis for beads

**WHY**: Identify cascading impacts and hidden dependencies

**EFFORT**: 20min

**DONE WHEN**:
- Module analyzes bead consequences
- Identifies affected systems
- Suggests defensive beads
- Tests pass

**TEST**:
```bash
command: "gleam test -- test_second_order_analysis"
expect: {
  exit_code: 0
}
```

**FILES**: src/intent/second_order_analyzer.gleam (new)

**DEPENDS ON**: [ML-VAL-2]

---

### [ML-2ND-2] Create premortem_analyzer.gleam

**WHAT**: Implement pre-mortem (imagine failure, work backwards) analysis

**WHY**: Prevent failures by identifying likely causes proactively

**EFFORT**: 20min

**DONE WHEN**:
- Module performs pre-mortem analysis
- Identifies probable failure scenarios
- Suggests preventive beads
- Tests pass

**TEST**:
```bash
command: "gleam test -- test_premortem_analysis"
expect: {
  exit_code: 0
}
```

**FILES**: src/intent/premortem_analyzer.gleam (new)

**DEPENDS ON**: [ML-VAL-2]

---

## PHASE 5: SMART REGENERATION

### [ML-REGEN-1] Enhance beads-regenerate with mental models

**WHAT**: Update beads-regenerate command to use inversion + second-order + pre-mortem

**WHY**: Failed beads improve using multiple mental models

**EFFORT**: 25min

**DONE WHEN**:
- Command uses hybrid regeneration strategy
- Runs all three analyses on failed bead
- Generates replacement beads
- Quality comparison with original
- Requires approval before applying
- Tests pass

**TEST**:
```bash
command: "gleam test -- test_hybrid_regeneration"
expect: {
  exit_code: 0
}
```

**EDGE CASES**:
- No failed beads → no-op
- Regeneration worse than original → show diff, reject
- Circular dependencies generated → detect and error
- User rejects regeneration → rollback

**FILES**: src/intent.gleam

**DEPENDS ON**: [ML-2ND-1], [ML-2ND-2]

---

## SUMMARY OF BEADS

**Phase 0 (Schema)**: 5 beads
- ML-SCHEMA-1 through ML-SCHEMA-5
- Total effort: 40min

**Phase 1A (P1 Enhancement)**: 8 beads
- ML-P1A-1 through ML-P1A-8
- Total effort: 75min

**Phase 1B (Critical Gaps)**: 5 beads
- ML-GAP-1 through ML-GAP-5
- Total effort: 85min

**Phase 2 (Generation)**: 6 beads
- ML-GEN-1 through ML-GEN-6
- Total effort: 90min

**Phase 3 (Validation)**: 2 beads
- ML-VAL-1 through ML-VAL-2
- Total effort: 25min

**Phase 4 (Advanced Models)**: 2 beads
- ML-2ND-1 through ML-2ND-2
- Total effort: 40min

**Phase 5 (Smart Regeneration)**: 1 bead
- ML-REGEN-1
- Total effort: 25min

**TOTAL**: 29 beads, ~380 minutes (~6.3 hours) of focused work

---

## BEAD DEPENDENCIES

```
Schema Foundation
  ML-SCHEMA-1 → ML-SCHEMA-2 → ML-SCHEMA-3 → ML-SCHEMA-4 → ML-SCHEMA-5

P1 Enhancement (depends on schema, can run in parallel)
  ML-P1A-1, ML-P1A-2, ..., ML-P1A-8  [all depend on ML-SCHEMA-5]

Critical Gaps (depends on P1 enhancement)
  ML-GAP-1, ML-GAP-2  [depend on P1A results]
  ML-GAP-3 → ML-GAP-4 → ML-GAP-5  [sequential]

Generation Pipeline
  ML-GEN-1 → ML-GEN-2 → ML-GEN-3 → ML-GEN-4 → ML-GEN-5 → ML-GEN-6
  [all depend on ML-GAP-3 validation gates]

Validation Commands
  ML-VAL-1 → ML-VAL-2
  [depends on ML-GAP-3]

Advanced Models
  ML-2ND-1, ML-2ND-2
  [depend on ML-VAL-2]

Smart Regeneration
  ML-REGEN-1
  [depends on ML-2ND-1 and ML-2ND-2]

Critical Path: Schema → P1 Enhancement → Gaps → Generation → Validation → Advanced → Regen
```

---

## NEXT STEPS

1. Use this breakdown to create beads in the .beads system: `bd create <bead-definition>`
2. Each bead has acceptance criteria in "DONE WHEN" section
3. Each bead has test specification
4. Beads can be parallelized where dependencies allow
5. Quality threshold (80+) is enforced for all beads
6. Strategic decisions (hybrid regeneration, full P1 enhancement, etc.) are encoded in CUE

All beads are designed to be atomic, testable, and independently verifiable.
