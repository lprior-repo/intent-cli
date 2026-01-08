# Integration: Improvement Plan + KIRK + EARS

## Overview

The improvement planning work (Phases 1-3) aligns perfectly with KIRK (Knowledge-Informed Requirements & Kontract) and EARS (Easy Approach to Requirements Syntax) systems that have been added to intent-cli.

This document shows how these systems work together to create a **world-class planning CLI tool**.

---

## What We Have Now

### 1. EARS Parser (`src/intent/kirk/ears_parser.gleam`)
- Converts structured English requirements to Intent behaviors
- 5 EARS patterns:
  - **Ubiquitous**: "THE SYSTEM SHALL [behavior]"
  - **EventDriven**: "WHEN [trigger] THE SYSTEM SHALL [behavior]"
  - **StateDriven**: "WHILE [state] THE SYSTEM SHALL [behavior]"
  - **Optional**: "WHERE [condition] THE SYSTEM SHALL [behavior]"
  - **Unwanted**: "IF [condition] THEN THE SYSTEM SHALL NOT [behavior]"

### 2. KIRK System (`docs/KIRK_SPEC_DESIGN.md`)
- Applies Munger's latticework of mental models
- Pre-mortem analysis for failure prediction
- Second-order thinking for dependencies
- Integration with requirements engineering

### 3. AI Planning Research (`docs/AI_PLANNING_DETERMINISM_RESEARCH.md`)
- Research on deterministic planning for AI agents
- Token efficiency optimization
- Specification clarity principles

### 4. Our Improvement Plan (3 Phases, 34 hours)
- Phase 1: Critical automation fixes
- Phase 2: Quality improvements
- Phase 3: Robustness & polish

---

## How They Integrate

### Level 1: Requirements → EARS → Behaviors

Our improvement plan can now be expressed in EARS format for automatic conversion to Intent behaviors:

```ears
THE SYSTEM SHALL support non-interactive interview mode
WHEN interview flag includes --answers=file.cue THE SYSTEM SHALL load CUE answers
WHILE user is not providing interactive input THE SYSTEM SHALL use pre-filled answers
WHERE strict mode is enabled THE SYSTEM SHALL fail on missing answers
WHERE strict mode is disabled THE SYSTEM SHALL prompt for missing answers
```

This gets parsed by EARS parser and converted to Intent Behaviors with proper metadata.

### Level 2: KIRK Mental Models → Beads Design

Our "atomic beads" approach aligns with KIRK's inversion principle:

**Inversion Applied to Beads:**
```
What would make a bead execution fail?
- Missing prerequisites not validated
- Unclear success criteria
- No rollback plan on failure
- Unknown tool requirements
```

**KIRK Pre-Mortem for Phase 1:**
```
Assumed failure: "Interview automation breaks, Claude Code can't run plans"
Likely causes:
1. Interactive mode still blocks on unexpected prompts
   Mitigation: Validation of all questions have answers before proceeding
2. Feedback loop doesn't regenerate correct beads
   Mitigation: Comprehensive tests for bead regeneration
3. Plan approval required but user can't edit plan
   Mitigation: Future Phase with edit capability
```

### Level 3: Beads → Execution Plan → KIRK Contracts

The execution plan created in Phase 1 becomes a KIRK contract:

**Phase 1 Execution Plan (in KIRK Contract Format):**
```cue
contract: {
  name: "Intent CLI Phase 1 Automation"

  pre_conditions: {
    // Must have before execution
    requirements: [
      "All 7 interview modules analyzed",
      "All friction points identified",
      "All decisions locked in",
      "All beads created",
    ]
  }

  behaviors: {
    // What the system SHALL do
    task_1_1: {
      name: "Create answer_loader.gleam"
      trigger: "Phase 1 started"
      shall: "Load CUE answers file and extract to Dict"
      shall_not: "Block on EOF for missing answers"
      success_criteria: ["Loads valid CUE", "Handles errors", "Tests pass"]
    }

    task_1_2: {
      name: "Modify interview.gleam"
      trigger: "answer_loader.gleam complete"
      shall: "Check answers dict before prompting"
      shall_not: "Lose functionality in strict mode"
    }

    // ... more behaviors for tasks 1.3-1.12
  }

  post_conditions: {
    // Verify success
    acceptance: [
      "Can run: intent interview --answers=file.cue --export=spec.cue",
      "Can run: intent bead-status <id>",
      "Can run: intent beads-regenerate <session_id>",
      "Can run: intent plan <session_id>",
      "Can run: intent plan-approve <session_id>",
    ]
  }
}
```

### Level 4: Atomic Beads + KIRK = Deterministic Planning

The combination creates **deterministic planning**:

1. **Structured Input**: EARS requirements
2. **Structured Processing**: KIRK contracts define what must happen
3. **Structured Output**: Atomic beads that Claude Code can execute
4. **Structured Verification**: Each bead has clear success criteria

---

## The Improvement Plan Through KIRK/EARS Lens

### Phase 1: Critical Automation (EARS Requirements)

```ears
// Core Problem: Interview blocks on interactive mode
THE SYSTEM SHALL support non-interactive operation
WHEN flag --answers=file.cue is provided THEN THE SYSTEM SHALL load CUE answers
WHILE questions are asked THEN THE SYSTEM SHALL check answers dict first
IF answer exists in dict THEN THE SYSTEM SHALL use it without prompting
WHERE strict mode is enabled AND answer missing THEN THE SYSTEM SHALL fail with clear error
WHERE strict mode disabled AND answer missing THEN THE SYSTEM SHALL prompt user

// Secondary Problem: No feedback loop
WHEN bead execution completes THEN THE SYSTEM SHALL record outcome
WHEN user calls bead-status THEN THE SYSTEM SHALL mark bead result
WHILE feedback exists FOR session THEN THE SYSTEM SHALL regenerate beads
IF beads are regenerated THEN THE SYSTEM SHALL preserve dependencies

// Tertiary Problem: No plan approval
WHEN plan is generated THEN THE SYSTEM SHALL display human-readable format
WHEN user calls plan-approve THEN THE SYSTEM SHALL show summary and ask confirmation
WHILE user hasn't approved THEN THE SYSTEM SHALL not execute plan
IF user says 'yes' THEN THE SYSTEM SHALL save approval record
```

### Phase 2: Quality Improvements (KIRK Second-Order Thinking)

```
Second-order consequences of Phase 1:
- Better answer extraction → better specs generated
- Better specs → more complete beads
- Beads with metadata → intelligent execution
- Feedback loop → iterative improvement

Inversion principle applied:
- What would make beads bad? No metadata
- What would make specs bad? Comments instead of structure
- What would make extraction bad? Brittle patterns
```

### Phase 3: Robustness (KIRK Pre-Mortem)

```
Assumed failure: "Beads fail during execution"
Likely causes:
1. Missing validation of session state
   → Task 3.1: Enforce state transitions
2. Contradictory feedback not handled
   → Task 3.2: Edge case handling
3. System slow with many beads
   → Task 3.3: Performance optimization
4. No recovery from failures
   → Task 3.4: Error recovery mechanisms
```

---

## Execution Path: From Requirements to Beads

```
User Requirements (English)
         ↓
    EARS Parser
         ↓
    EARS Requirements (structured)
         ↓
    KIRK Contracts (formal specification)
         ↓
    Atomic Beads (executable work items)
         ↓
   Claude Code (execution)
         ↓
    Beads Results (success/fail/blocked)
         ↓
    Feedback Loop (update understanding)
         ↓
    Regenerate Beads (EARS → Beads again)
```

---

## New Files & Modules

### Added by Recent Commits (KIRK Integration)

- `src/intent/kirk/ears_parser.gleam` - EARS parser
- `schema/kirk.cue` - KIRK CUE schema
- `schema/kirk.proto` - KIRK protobuf schema
- `docs/KIRK_SPEC_DESIGN.md` - KIRK detailed design
- `docs/KIRK_IMPLEMENTATION_PLAN.md` - KIRK implementation
- `docs/AI_PLANNING_DETERMINISM_RESEARCH.md` - Research foundation
- `examples/requirements.ears.md` - EARS example
- `intent-kirk.cue` - KIRK self-spec

### We Will Create (Improvement Plan)

**Phase 1:**
- `src/intent/answer_loader.gleam` - Load CUE answers
- `src/intent/bead_feedback.gleam` - Track bead feedback
- `src/intent/plan_mode.gleam` - Display execution plans

**Phase 2:**
- `src/intent/smart_extraction.gleam` - Better answer parsing
- `src/intent/feedback_loop.gleam` - Feedback processing

**Existing to Enhance:**
- `src/intent.gleam` - Add new CLI commands
- `src/intent/interview.gleam` - Non-interactive mode
- `src/intent/bead_templates.gleam` - Add metadata
- `src/intent/spec_builder.gleam` - Structured output

---

## The System is Now World-Class Because

### 1. Requirements Clarity (EARS + KIRK)
- Natural language → Structured → Formal contracts
- No ambiguity in what "done" means

### 2. Deterministic Execution (Atomic Beads + Metadata)
- Each bead is independent
- Clear prerequisites and success criteria
- Tools and effort known upfront

### 3. Feedback Loop (Beads → Results → Regenerate)
- Can learn from failures
- Can iterate on plans
- Improves over time

### 4. Human + AI Partnership (KIRK Mental Models)
- Inversion principle prevents obvious failures
- Pre-mortem catches hidden risks
- Second-order thinking traces consequences

### 5. Full Automation (Non-Interactive + Plan Approval)
- Claude Code can work without human prompting
- Humans maintain approval control
- Feedback tells system what went wrong

---

## Success Criteria: After All Three Phases

Users can:

1. **Write requirements in EARS**: Natural English, structured patterns
2. **Parse to Intent behaviors**: EARS parser converts automatically
3. **Wrap in KIRK contracts**: Define success, failure, dependencies
4. **Generate execution plan**: Atomic beads with full metadata
5. **Review and approve**: Human sees plan before execution
6. **Execute autonomously**: Claude Code runs beads intelligently
7. **Provide feedback**: Mark results as success/failed/blocked
8. **Regenerate from feedback**: System learns and improves
9. **Iterate**: Repeat steps 4-8 until plan succeeds

This is a **complete, world-class planning tool** for AI-assisted software engineering.

---

## Timeline

- **Week 1**: Phase 1 (automation fixes) - 9 hours
- **Week 2-3**: Phase 2 (quality) - 18 hours
- **Week 4**: Phase 3 (robustness) - 7 hours
- **Total**: 34 hours / 4-5 weeks

---

## Integration Checklist

- [x] EARS parser implemented
- [x] KIRK system designed
- [x] AI planning research complete
- [x] Improvement plan created (3 phases)
- [x] Beads structure defined
- [ ] Phase 1 implementation start
- [ ] EARS requirements written for Phase 1
- [ ] KIRK contracts generated
- [ ] Phase 1 completion
- [ ] Phase 2 implementation start
- [ ] Phase 2 completion
- [ ] Phase 3 implementation start
- [ ] Phase 3 completion
- [ ] Documentation updated
- [ ] Launch world-class planning CLI

---

## Next: Phase 1 Implementation

See `PHASE_1_QUICKSTART.md` for detailed implementation guide.

The beads are defined. The EARS patterns are ready. The KIRK contracts are framed.

Time to build the world-class planning CLI.
