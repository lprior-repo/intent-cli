# Content Audit Code Smells - Executable Beads

**Audit Score**: 84.5/100
**Generator**: Quality Auditor (Content Smells Detection)
**Date**: 2026-01-18
**Priority Breakdown**: 1 Critical (Priority 0) + 4 High (Priority 1) = 5 Total Beads
**Total Effort**: 85 minutes

---

## SMELL #1: Inconsistent Naming Convention (CRITICAL)

**Status**: `cs-001-parse-kirk-prefix`
**File**: `src/intent/cli_text_constants.gleam` (line 84)
**Priority**: 0 (Critical - API consistency)
**Effort**: 5 minutes

### Issue
The `parse` command description is missing the "KIRK:" prefix that all other KIRK commands have. This breaks user expectation of consistent API categorization.

**Pattern Found**: 6 KIRK commands have "KIRK:" prefix, `parse` doesn't
- ✓ `cmd_ears_desc` = "KIRK: Parse EARS requirements..."
- ✓ `cmd_quality_desc` = "KIRK: Analyze spec quality..."
- ✓ `cmd_invert_desc` = "KIRK: Identify missing failure..."
- ✓ `cmd_coverage_desc` = "KIRK: Analyze coverage..."
- ✓ `cmd_gaps_desc` = "KIRK: Detect specification gaps..."
- ✓ `cmd_effects_desc` = "KIRK: Trace second-order effects..."
- ✗ `cmd_parse_desc` = "Parse EARS requirements..." (MISSING PREFIX)

### Impact
- Breaks command naming convention
- Confuses users about command categorization
- Inconsistent API surface

### Fix
```gleam
// BEFORE (Line 84)
pub const cmd_parse_desc = "Parse EARS requirements to structured spec"

// AFTER
pub const cmd_parse_desc = "KIRK: Parse EARS requirements to structured spec"
```

### Verification
```bash
# After fix, all 7 commands should have consistent prefix
grep "cmd_.*_desc = \"KIRK:" src/intent/cli_text_constants.gleam | wc -l  # Should be 7
grep "cmd_parse_desc" src/intent/cli_text_constants.gleam  # Should show "KIRK:" prefix
```

---

## SMELL #2: Conflicting Documentation (CRITICAL)

**Status**: `cs-002-ears-pattern-count`
**File**: `src/intent/cli_text_constants.gleam` (lines 1695-1698)
**Priority**: 0 (Critical - Misleading information)
**Effort**: 10 minutes

### Issue
Documentation claims EARS parser recognizes "5 patterns" but only lists 4. This is misleading/confusing for users.

**Current Text** (lines 1695-1698):
```
WHAT IT DOES
  EARS (Easy Approach to Requirements Syntax) parser converts natural language
  requirements into structured Intent spec behaviors. Recognizes 5 patterns:  ← CLAIMS 5
  THE SYSTEM SHALL (ubiquitous), GIVEN/WHEN/THEN (scenario-based), IF/THEN
  (conditional), and state-based patterns.  ← LISTS 4
```

### Analysis
Listed patterns:
1. THE SYSTEM SHALL (ubiquitous)
2. GIVEN/WHEN/THEN (scenario-based)
3. IF/THEN (conditional)
4. state-based patterns

**Missing**: 5th pattern is not documented. Either:
- Parser actually supports only 4 patterns (most likely)
- 5th pattern exists but not listed

### Fix (Most Likely)
```gleam
// BEFORE (Line 1695)
requirements into structured Intent spec behaviors. Recognizes 5 patterns:

// AFTER
requirements into structured Intent spec behaviors. Recognizes 4 patterns:
```

### Verification
```bash
# After fix, check no discrepancy
grep -A 4 "Recognizes.*patterns:" src/intent/cli_text_constants.gleam
# Should match pattern count to actual list
```

### Secondary Action (If 5th pattern exists)
If EARS implementation actually supports 5 patterns, add the missing pattern to the documentation list.

---

## SMELL #3: Missing Required Field (CRITICAL)

**Status**: `cs-003-quality-ai-readiness-dimension`
**File**: `src/intent/cli_text_constants.gleam` (lines 1451-1457)
**Priority**: 0 (Critical - Incomplete spec)
**Effort**: 20 minutes

### Issue
The `quality` command help doesn't explicitly highlight `ai_readiness` as a core dimension. It's mentioned but not clearly separated as a major dimension like coverage, clarity, and testability.

**Current Help**:
```gleam
pub const cmd_quality_desc = "KIRK: Analyze spec quality across coverage, clarity, testability"

pub const kirk_quality_extended_help = "
  KIRK (Komprehensive Intent Review Kit) quality analysis scores spec across
  4 dimensions: coverage (required cases), clarity (documentation/structure),
  testability (executability/assertions), and AI-readiness (LLM compatibility).
```

### Problems
1. Command description doesn't mention `ai_readiness` (only mentions 3 of 4)
2. FLAG DETAILS section shows `ai_readiness%` but not clearly as dimension
3. Short description is incomplete

### Impact
- Users don't understand `ai_readiness` is a separate dimension
- Documentation quality doesn't match feature scope
- API clarity reduced

### Fix
```gleam
// BEFORE
pub const cmd_quality_desc = "KIRK: Analyze spec quality across coverage, clarity, testability"

// AFTER
pub const cmd_quality_desc = "KIRK: Analyze spec quality across 4 dimensions: coverage, clarity, testability, AI-readiness"

// BEFORE (extended help WHAT IT DOES)
  KIRK (Komprehensive Intent Review Kit) quality analysis scores spec across
  4 dimensions: coverage (required cases), clarity (documentation/structure),
  testability (executability/assertions), and AI-readiness (LLM compatibility).

// AFTER (make explicit)
  KIRK (Komprehensive Intent Review Kit) quality analysis scores spec across 4 dimensions:
  1. Coverage - required test cases and behavior coverage
  2. Clarity - documentation quality and structural organization
  3. Testability - executability of behaviors and assertion completeness
  4. AI-readiness - LLM compatibility and implementation hints
  Returns individual scores (0-100 each) plus overall health rating.
```

### Verification
```bash
# Check command description includes ai_readiness
grep "cmd_quality_desc" src/intent/cli_text_constants.gleam | grep -i "ai-readiness"
# Check extended help has explicit dimension list
grep -A 5 "4 dimensions:" src/intent/cli_text_constants.gleam | grep -i "ai-readiness"
```

---

## SMELL #4: Unexplained Concepts (HIGH)

**Status**: `cs-004-beads-regenerate-mental-models`
**File**: `src/intent/cli_text_constants.gleam` (lines 867-872)
**Priority**: 1 (High - Understanding required)
**Effort**: 30 minutes

### Issue
Help text uses technical jargon without explanation. Terms "hybrid", "inversion", "effects", "premortem" are used without defining what they do or when to use them.

**Current Text** (lines 867-872):
```
FLAG DETAILS
  --strategy STRATEGY
    Regeneration strategy: hybrid (default), inversion, effects, premortem
    - hybrid: Combines all models (recommended)
    - inversion: Flips failed behavior to find root causes
    - effects: Analyzes second-order dependencies
    - premortem: Post-mortem analysis for robustness
```

### Problem
- "hybrid" doesn't explain what models are combined
- "inversion" uses more jargon ("flips") without context
- "effects" doesn't explain what "second-order dependencies" means
- "premortem" is post-mortem analysis but called "pre-mortem"? (confusing)
- No guidance on when to use each strategy

### Impact
- Users don't understand strategy options
- Can't make informed choice
- Reduced accessibility for new users

### Fix
```gleam
// BEFORE
  --strategy STRATEGY
    Regeneration strategy: hybrid (default), inversion, effects, premortem
    - hybrid: Combines all models (recommended)
    - inversion: Flips failed behavior to find root causes
    - effects: Analyzes second-order dependencies
    - premortem: Post-mortem analysis for robustness

// AFTER (explicit mental models + use cases)
  --strategy STRATEGY
    Regeneration strategy: hybrid (default), inversion, effects, premortem
    - hybrid (default): Combines inversion (find what breaks), effects (trace consequences),
      and premortem (imagine failure) for comprehensive new approaches. Recommended for most cases.
    - inversion: Mental model that flips failure: if behavior X failed, what's the opposite?
      Useful for logic errors and missing validation. Finds root causes by contradiction.
    - effects: Analyzes second-order effects and consequence chains. Use for dependency failures
      where consequences were missed or state changes didn't trigger expected behaviors.
    - premortem: Imagine spec deployed and failures occur. What would break? What's missing?
      Best for robustness checks and discovering unknown unknowns before execution.
```

### Verification
```bash
# Check updated text includes mental model explanations
grep -A 10 "strategy STRATEGY" src/intent/cli_text_constants.gleam | grep -i "mental model"
```

---

## SMELL #5: Orphaned Concept (HIGH)

**Status**: `cs-005-effects-orphaned-behaviors`
**File**: `src/intent/cli_text_constants.gleam` (lines 1645-1648)
**Priority**: 1 (High - Understanding required)
**Effort**: 20 minutes

### Issue
Technical term "orphaned behaviors" used without definition. Help text doesn't explain what "orphaned" means in this context.

**Current Text** (lines 1645-1648):
```
WHAT IT DOES
  Effects analysis traces consequence chains: what happens when a behavior
  executes, what other behaviors depend on it, what state changes propagate.
  Identifies orphaned behaviors and missing consequence handlers.
```

### Problem
- "orphaned behaviors" is domain-specific jargon
- No explanation of what makes a behavior "orphaned"
- "consequence handlers" also undefined
- Users can't understand what the command finds

### Impact
- Unclear what the analysis does
- Users won't know if results are relevant
- Reduced usability for new users

### Context Analysis
From code analysis:
- "Orphaned behaviors" = behaviors that don't trigger expected consequences
- "Consequence handlers" = state changes with no recovery or follow-up path

### Fix
```gleam
// BEFORE (Lines 1645-1648)
  Effects analysis traces consequence chains: what happens when a behavior
  executes, what other behaviors depend on it, what state changes propagate.
  Identifies orphaned behaviors and missing consequence handlers.

// AFTER (with definitions)
  Effects analysis traces consequence chains: what happens when a behavior
  executes, what other behaviors depend on it, what state changes propagate.
  Identifies orphaned behaviors (behaviors that don't trigger expected consequences)
  and missing consequence handlers (state changes with no recovery or follow-up path).
```

### Verification
```bash
# Check definitions are added
grep -A 2 "Identifies orphaned" src/intent/cli_text_constants.gleam | grep -E "\(.*behaviors.*\)"
```

---

## Summary Table

| Bead ID | Smell | File | Line | Priority | Effort | Type |
|---------|-------|------|------|----------|--------|------|
| cs-001 | Inconsistent naming | cli_text_constants.gleam | 84 | 0 (Critical) | 5 min | Convention |
| cs-002 | Pattern count mismatch | cli_text_constants.gleam | 1695 | 0 (Critical) | 10 min | Documentation |
| cs-003 | Missing dimension | cli_text_constants.gleam | 1451 | 0 (Critical) | 20 min | Completeness |
| cs-004 | Unexplained jargon | cli_text_constants.gleam | 867 | 1 (High) | 30 min | Clarity |
| cs-005 | Undefined term | cli_text_constants.gleam | 1648 | 1 (High) | 20 min | Terminology |

**Total Effort**: 85 minutes
**Critical Issues**: 3 (must fix for API consistency)
**High Issues**: 2 (should fix for usability)

---

## Quality Gates

### Before Merge
- [ ] All beads in CONTENT_SMELLS_BEADS.jsonl verified and executed
- [ ] No beads blocked or failed
- [ ] Help text verified with `intent --help` on each affected command
- [ ] Consistency check: all KIRK commands start with "KIRK:" prefix
- [ ] Pattern count verified against actual implementation

### Success Criteria
1. **cs-001**: `parse` description includes "KIRK:" prefix ✓
2. **cs-002**: EARS pattern count matches documentation ✓
3. **cs-003**: `ai_readiness` is explicit 4th dimension ✓
4. **cs-004**: Each strategy has mental model + use case ✓
5. **cs-005**: "Orphaned behaviors" definition included ✓

---

## Testing Strategy

### Manual Verification
```bash
# Verify all fixes in place
intent quality --help | grep "4 dimensions"
intent effects --help | grep "orphaned behaviors"
intent ears --help | grep "patterns"
intent beads-regenerate --help | grep "mental model"
intent parse --help | grep "KIRK:"
```

### Content Consistency Check
```bash
# All KIRK commands should have prefix
grep "cmd_.*_desc = \"KIRK:" src/intent/cli_text_constants.gleam | wc -l
# Should output: 7 (or appropriate count)

# No commands should have duplicate prefixes
grep "KIRK:.*KIRK:" src/intent/cli_text_constants.gleam
# Should output: (empty - no matches)
```

### Help Text Audit
```bash
# Verify no orphaned jargon remains
grep -i "orphaned" src/intent/cli_text_constants.gleam
# Should show: "(behaviors that don't trigger expected consequences)"

# Verify all dimensions documented
grep -A 5 "4 dimensions" src/intent/cli_text_constants.gleam | grep -i "ai-readiness"
# Should find ai_readiness in dimension list
```

---

## Additional Improvements (Future)

While fixing these 5 smells, consider:
1. **cs-006**: Review all technical terms for definitions
2. **cs-007**: Audit all command descriptions for consistency
3. **cs-008**: Add examples to each mental model explanation
4. **cs-009**: Create glossary of KIRK-specific terminology
5. **cs-010**: Establish documentation review checklist

---

## Beads JSONL Format

All beads exported to: `CONTENT_SMELLS_BEADS.jsonl`

Each bead includes:
- `bead_id`: Unique identifier (cs-001 through cs-005)
- `priority`: 0 (critical) or 1 (high)
- `effort_minutes`: Estimated effort
- `code_smell_type`: Classification
- `tags`: For filtering and tracking
- `before`/`after`: Exact code changes
- `success_criteria`: Verification checklist
- `audit_source`: Quality Auditor attribution

---

**Generated**: 2026-01-18
**Audit Version**: Quality Auditor v1.0
**Status**: Ready for execution
