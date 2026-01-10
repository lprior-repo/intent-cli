# CUE Schema to Beads Conversion Guide

This guide explains how to convert the Mental Lattice CUE schemas into executable beads using the `bd` system.

---

## Overview

We have created:
1. **CUE Schema** (source of truth): `schema/beads.cue`, `schema/mental-lattice-config.cue`
2. **Bead Specifications** (implementation guide): `MENTAL-LATTICE-BEADS.md`

This guide shows how to go from specification to beads.

---

## Quick Start

### 1. Verify CUE Schema
```bash
# The schema should be in .intent/ during sessions
# For reference, check extended definitions:
grep -A 5 "BeadKirkMetadata" schema/beads.cue

# Validate Phase 1 analysis
grep "quality:" schema/mental-lattice-config.cue | head
```

### 2. Create First Bead (ML-SCHEMA-1)
```bash
bd create \
  --title "[ML-SCHEMA-1] Add #BeadKirkMetadata to schema/beads.cue" \
  --description "Add optional kirk?: #BeadKirkMetadata field to #Bead type" \
  --acceptance-criteria "kirk field added as optional to #Bead | CUE schema compiles | Backwards compatibility preserved" \
  --effort "5min" \
  --file "schema/beads.cue"
```

### 3. Execute and Verify
```bash
# Mark bead in_progress
bd update <bead-id> --status in_progress

# Work on it (add KIRK field to beads.cue)
# Then verify:
cue vet schema/beads.cue

# Mark complete when done
bd update <bead-id> --status completed
```

---

## Detailed Conversion Process

### Phase 0: Schema (5 beads)

Each of these beads modifies `schema/beads.cue`:

#### [ML-SCHEMA-1] Add #BeadKirkMetadata Container
```
WHAT:   Add optional kirk?: #BeadKirkMetadata field to #Bead
WHY:    Enable beads to carry mental model validation metadata
EFFORT: 5min
FILE:   schema/beads.cue
TEST:   cue vet schema/beads.cue && grep "kirk?" schema/beads.cue
DONE:   Field added, schema compiles, existing beads still valid
```

**Bead creation command:**
```bash
bd create \
  --id ML-SCHEMA-1 \
  --title "[ML-SCHEMA-1] Add #BeadKirkMetadata to schema/beads.cue" \
  --description "Add optional kirk?: #BeadKirkMetadata field to #Bead" \
  --what "Add optional kirk?: #BeadKirkMetadata field to #Bead type" \
  --why "Enable beads to carry mental model validation metadata without breaking existing beads" \
  --acceptance-criteria "kirk field added as optional to #Bead | CUE schema compiles without errors | Backwards compatibility preserved (existing beads without kirk field still valid)" \
  --effort "5min" \
  --file "schema/beads.cue" \
  --test "cue vet schema/beads.cue" \
  --expect-exit-code 0
```

#### [ML-SCHEMA-2] Add EARS, Contract, Inversion, Quality Types
```
WHAT:   Add #BeadEARS, #BeadContract, #BeadInversion, #BeadQuality types
WHY:    Provide structured types for mental model metadata
EFFORT: 10min
FILE:   schema/beads.cue
TEST:   cue eval -l '#BeadEARS | #BeadContract | #BeadInversion | #BeadQuality' schema/beads.cue
DONE:   All 4 types defined, schema compiles, examples show usage
```

#### [ML-SCHEMA-3] Add Regeneration and Audit Types
```
WHAT:   Add #RegenerationStrategy and #BeadKirkAudit types
WHY:    Track how/when beads were improved when they fail
EFFORT: 5min
FILE:   schema/beads.cue
TEST:   cue eval -l '#RegenerationStrategy | #BeadKirkAudit' schema/beads.cue
DONE:   Both types defined, audit trail captures metadata
```

#### [ML-SCHEMA-4] Add MentalLatticeConfig Type
```
WHAT:   Add #MentalLatticeConfig type with 5 strategic decisions
WHY:    Provide single source of truth for mental lattice behavior
EFFORT: 5min
FILE:   schema/beads.cue
TEST:   cue eval schema/beads.cue | grep "regeneration_strategy"
DONE:   Config type defined with all 5 fields, well documented
```

#### [ML-SCHEMA-5] Create mental-lattice-config.cue
```
WHAT:   Create new file with concrete KIRK metadata for all 8 Phase 1 beads
WHY:    Document the mental lattice analysis in machine-readable form
EFFORT: 15min
FILE:   schema/mental-lattice-config.cue (new)
TEST:   cue vet schema/mental-lattice-config.cue schema/beads.cue
DONE:   File created, all 8 P1 beads have complete KIRK metadata, validates
```

**Reference**: Use `schema/mental-lattice-config.cue` as-is (already created).

---

### Phase 1A: P1 Bead Enhancement (8 beads)

Each of these beads **updates an existing bead** with KIRK metadata.

#### [ML-P1A-1] Enhance answer_loader (intent-cli-qnf)
```
WHAT:   Update qnf bead with KIRK metadata:
        - EARS pattern (ubiquitous, clarity: 95)
        - Preconditions (file exists, readable, valid CUE, schema match)
        - Postconditions (returns Dict, values trimmed, errors descriptive)
        - Inversion risks (70% edge case coverage)
        - Quality score (93/100)

WHY:    Ensure bead meets 80+ quality threshold and has explicit contracts

EFFORT: 10min

HOW:    1. Find existing qnf bead: bd show intent-cli-qnf
        2. Reference schema/mental-lattice-config.cue::p1_t1_1_kirk
        3. Update qnf bead with KIRK metadata and expanded edge cases:
           - "File not found → clear error with path shown"
           - "Invalid CUE syntax → parse error with line number"
           - "Answer key doesn't match question → skip unknown, warn"
           - "Empty string answer → reject with suggestion"
           - "Answer too long (>10KB) → warn about length"
           - "Symlinked file → resolve correctly"
           - "UTF-8 BOM marker → handle correctly"
           ... (see MENTAL-LATTICE-CONFIG.cue for complete list)
        4. Update quality score to 93
        5. Verify: cue vet .intent/*.cue schema/*.cue
```

**Command pattern:**
```bash
bd update intent-cli-qnf \
  --edge-cases "[...expanded list...]" \
  --quality-score 93 \
  --kirk-metadata-reference "schema/mental-lattice-config.cue:p1_t1_1_kirk"
```

#### [ML-P1A-2] through [ML-P1A-8]: Similar Pattern
Each follows the same pattern:
1. Reference the existing bead (intent-cli-f5y, intent-cli-xk8, etc.)
2. Look up corresponding KIRK metadata in mental-lattice-config.cue
3. Update edge cases (expand from 5-10 to 10-15+)
4. Update quality score
5. Verify with `cue vet`

See MENTAL-LATTICE-BEADS.md for details on each.

---

### Phase 1B: Critical Gap Fixes (5 beads)

These beads create design documents and implementation modules to fill gaps.

#### [ML-GAP-1] File Locking Strategy Design
```
WHAT:   Design and document file locking approach for concurrent append
WHY:    Concurrent appends cause file corruption - CRITICAL
EFFORT: 15min
FILE:   design/file-locking.md (new)
OUTPUT: Design document explaining:
        - Chosen locking mechanism (flock/LockFile/library)
        - Lock acquisition timeout
        - Stale lock handling
        - Cross-process coordination
TEST:   grep -i "lock\|atomic" design/file-locking.md | wc -l >= 5
DONE:   Design doc created, strategy clear
```

**Process:**
1. Create `design/file-locking.md`
2. Write section on Unix (flock) and Windows (LockFile) approaches
3. Describe OS-agnostic library option (if using one)
4. Document edge cases
5. Reference in P1-T1.4 bead (18a)

#### [ML-GAP-2] Regeneration Strategies Design
```
WHAT:   Design the 4 regeneration strategies (inversion, second-order, pre-mortem, hybrid)
WHY:    ozf bead says "adjusted approach" but doesn't define it
EFFORT: 20min
FILE:   design/regeneration-strategies.md (new)
OUTPUT: Design document with:
        - Inversion-driven strategy (algorithm, inputs, outputs)
        - Second-order-driven strategy
        - Pre-mortem-driven strategy
        - Hybrid strategy (how to combine all three)
        - Approval workflow
TEST:   grep -i "strategy\|algorithm" design/regeneration-strategies.md | wc -l >= 10
DONE:   Strategies clearly defined, implementable
```

#### [ML-GAP-3] Quality Validation Gates Implementation
```
WHAT:   Implement validate_bead_kirk() function in Gleam
WHY:    Enforce 80+ quality threshold before bead execution
EFFORT: 20min
FILE:   src/intent/bead_validator.gleam (new)
OUTPUT: Function with:
        - EARS pattern validation
        - Quality score >= 80 check
        - Preconditions present check
        - Returns Result(Nil, List(ValidationIssue))
TEST:   gleam test -- test_validate_bead_kirk
DONE:   Tests pass, validation gates ready
```

#### [ML-GAP-4] EARS Parser Integration
```
WHAT:   Integrate ears_parser module to validate bead "what" field
WHY:    Ensure requirement clarity is enforced
EFFORT: 15min
FILE:   src/intent/bead_validator.gleam (extend)
OUTPUT: Function validate_bead_ears() that:
        - Uses ears_parser.parse() on bead.what
        - Checks pattern matches test specification
        - Extracts trigger/condition/state
        - Calculates clarity score
TEST:   gleam test -- test_validate_bead_ears
DONE:   EARS validation integrated
```

#### [ML-GAP-5] Inversion Coverage Checker
```
WHAT:   Use inversion_checker to validate bead edge cases
WHY:    Ensure edge cases cover identified risks
EFFORT: 15min
FILE:   src/intent/bead_validator.gleam (extend)
OUTPUT: Function analyze_bead_inversions() that:
        - Uses inversion_checker.find_risks_for_behavior()
        - Identifies security/usability/integration risks
        - Calculates edge_case_coverage %
        - Returns suggested_edge_cases
TEST:   gleam test -- test_analyze_bead_inversions
DONE:   Inversion analysis integrated
```

---

## Bead Status Tracking

### As You Create Beads

```bash
# Create bead
bd create [options]

# Transition states
bd update <id> --status in_progress
# ... do the work ...
bd update <id> --status completed

# Verify quality
bd show <id> | jq '.quality'
```

### Expected Quality Progression

```
Phase 0 (Schema):      All beads should validate (quality: not measured yet)
Phase 1A (P1 Enhance): 6/8 beads at 80+, 2/8 flagged for gaps
Phase 1B (Gaps):       All gap beads complete, enables Phase 2
Phase 2 (Generation):  New tools built, tested, integrated
Phase 3+ (Advanced):   Enhancements to generation pipeline
```

---

## Tips and Gotchas

### ✅ DO
- Reference `schema/mental-lattice-config.cue` directly (already has all Phase 1 analysis)
- Verify `cue vet` passes after schema changes
- Keep edge cases specific and actionable
- Test each bead before marking complete
- Use the strategic decisions from #MentalLatticeConfig

### ❌ DON'T
- Skip Phase 1B gap fixes (will cause failures in Phase 2)
- Create beads with quality < 80 (enforced from Phase 1B)
- Ignore the dependency graph (try to run Phase 2 before Phase 1B)
- Lose track of the 5 strategic decisions

### 🔍 Verification Steps

After each Phase:
```bash
# Phase 0 complete?
cue vet schema/beads.cue schema/mental-lattice-config.cue

# Phase 1A complete?
bd ready --json | jq '.[] | select(.id | startswith("intent-cli-")) | {id, quality}'

# Phase 1B complete?
ls design/file-locking.md design/regeneration-strategies.md
ls src/intent/bead_validator.gleam
gleam test

# Phase 2 progress?
ls src/intent/bead_generator.gleam
```

---

## Example: Creating ML-SCHEMA-1 Bead

### Step 1: Understand the Requirement
```
Read MENTAL-LATTICE-BEADS.md section "Phase 0 - Schema Extensions"
Read ML-SCHEMA-1 entry:
  - WHAT: Add optional kirk?: #BeadKirkMetadata field to #Bead
  - WHY: Enable beads to carry mental model metadata
  - EFFORT: 5min
  - DONE WHEN: field added, compiles, backwards compatible
```

### Step 2: Create the Bead
```bash
bd create \
  --title "[ML-SCHEMA-1] Add #BeadKirkMetadata to schema/beads.cue" \
  --description "Extend #Bead schema with optional mental model metadata" \
  --what "Add optional kirk?: #BeadKirkMetadata field to #Bead type" \
  --why "Enable beads to carry KIRK validation metadata without breaking existing beads" \
  --acceptance-criteria "kirk field added as optional to #Bead | CUE schema compiles | Backwards compatibility preserved" \
  --effort "5min" \
  --file "schema/beads.cue" \
  --edge-cases "Beads without kirk field must validate | kirk field is completely optional"
```

### Step 3: Do the Work
```bash
# Edit schema/beads.cue
# Add: kirk?: #BeadKirkMetadata
# (already done in this project)
```

### Step 4: Verify and Complete
```bash
# Test
cue vet schema/beads.cue

# Update bead status
bd update <returned-id> --status completed
```

---

## Conclusion

The CUE schema → Beads conversion is straightforward:

1. **Understand** the specification in MENTAL-LATTICE-BEADS.md
2. **Create** bead with `bd create` using spec as input
3. **Implement** the work specified in WHAT/WHY/DONE WHEN
4. **Verify** with the test command
5. **Complete** the bead with `bd update --status completed`

The Mental Lattice analysis (in `schema/mental-lattice-config.cue`) provides all the context needed to understand **why** each bead exists and **what** it should accomplish.

Ready to create beads? Start with Phase 0 schema beads - they're simple and enable everything else!
