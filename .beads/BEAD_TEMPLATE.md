# World-Class Beads Ticket Template

## The Philosophy

> **If GPT-4 or a competent high school senior cannot implement this ticket perfectly on their first attempt, the ticket is incomplete.**

Every bead must be so rigorously specified that implementation becomes mechanical. The AI has no choice but to succeed because every edge case, failure mode, and test is explicitly enumerated.

---

## Template Structure

```yaml
# ============================================================================
# BEAD: [ID] - [Title]
# ============================================================================

id: "intent-cli-XXXX"
title: "[Component]: [Action verb] [specific thing]"
type: feature | bug | task | epic | chore
priority: 0 (critical) | 1 (high) | 2 (medium) | 3 (low) | 4 (backlog)
effort_estimate: "15min | 30min | 1hr | 2hr | 4hr"  # Max 4hr per bead
labels: [component, category, methodology]

# ============================================================================
# SECTION 1: EARS REQUIREMENTS (What must happen)
# ============================================================================

# EARS = Easy Approach to Requirements Syntax
# Every requirement MUST use one of these 6 patterns:

ears_requirements:

  # Pattern 1: UBIQUITOUS - Always true, no conditions
  ubiquitous:
    - "THE SYSTEM SHALL [behavior that is always true]"
    - "THE SYSTEM SHALL [another universal behavior]"

  # Pattern 2: EVENT-DRIVEN - Trigger-response pairs
  event_driven:
    - trigger: "WHEN [specific user action or system event]"
      shall: "THE SYSTEM SHALL [specific response]"
    - trigger: "WHEN [another trigger]"
      shall: "THE SYSTEM SHALL [another response]"

  # Pattern 3: STATE-DRIVEN - Behavior during specific states
  state_driven:
    - state: "WHILE [system/user/resource is in state X]"
      shall: "THE SYSTEM SHALL [behavior during that state]"

  # Pattern 4: OPTIONAL - Conditional on configuration/roles
  optional:
    - condition: "WHERE [feature flag / user role / config option]"
      shall: "THE SYSTEM SHALL [conditional behavior]"

  # Pattern 5: UNWANTED - Things that must NEVER happen (Inversion!)
  unwanted:
    - condition: "IF [bad state or input]"
      shall_not: "THE SYSTEM SHALL NOT [forbidden behavior]"
      because: "[Why this would be catastrophic]"

  # Pattern 6: COMPLEX - State + Event combinations
  complex:
    - state: "WHILE [in state X]"
      trigger: "WHEN [event Y occurs]"
      shall: "THE SYSTEM SHALL [combined behavior]"

# ============================================================================
# SECTION 2: KIRK CONTRACTS (Design by Contract)
# ============================================================================

# KIRK = Knowledge-Informed Requirements & Kontract
# Every behavior has preconditions, postconditions, and invariants

contracts:

  preconditions:
    # What MUST be true before this code runs
    auth_required: true | false
    required_inputs:
      - field: "field_name"
        type: "String | Int | Bool | List | Object"
        constraints: "[validation rules]"
        example_valid: "[concrete valid value]"
        example_invalid: "[concrete invalid value]"
    system_state:
      - "[State that must exist before execution]"

  postconditions:
    # What MUST be true after this code runs
    state_changes:
      - "[Specific change to system state]"
      - "[Another state change]"
    return_guarantees:
      - field: "response.field"
        guarantee: "[What is guaranteed about this field]"
      - field: "exit_code"
        guarantee: "0 on success, 3 on invalid input, 4 on missing resource"
    side_effects:
      - "[Any IO, network, file, or external effects]"

  invariants:
    # What must ALWAYS be true, before and after
    - "[Invariant 1 - e.g., 'Passwords never appear in logs']"
    - "[Invariant 2 - e.g., 'All timestamps are ISO8601']"
    - "[Invariant 3 - e.g., 'Exit codes match AGENTS.md spec']"

# ============================================================================
# SECTION 3: INVERSION ANALYSIS (What could go wrong)
# ============================================================================

# Charlie Munger: "Invert, always invert"
# Define failure modes BEFORE implementation

inversions:

  security_failures:
    - failure: "[Security vulnerability that could occur]"
      prevention: "[How the code MUST prevent this]"
      test_for_it: "[Specific test case]"

  usability_failures:
    - failure: "[UX problem that could occur]"
      prevention: "[How to prevent it]"
      test_for_it: "[Specific test case]"

  data_integrity_failures:
    - failure: "[Data corruption/loss scenario]"
      prevention: "[How to prevent it]"
      test_for_it: "[Specific test case]"

  integration_failures:
    - failure: "[What could break downstream systems]"
      prevention: "[How to prevent it]"
      test_for_it: "[Specific test case]"

# ============================================================================
# SECTION 4: ATDD ACCEPTANCE TESTS (Tests FIRST, code second)
# ============================================================================

# ATDD = Acceptance Test-Driven Development
# Define the EXACT tests that prove the feature works
# NO MOCKS. NO FAKE DATA. REAL END-TO-END TESTS.

acceptance_tests:

  # Happy path tests (must all pass for bead to close)
  happy_paths:
    - name: "test_[descriptive_name]"
      given: "[Exact precondition state - real data]"
      when: "[Exact action taken - real command/call]"
      then:
        - "[Exact assertion 1 - real output]"
        - "[Exact assertion 2]"
      real_input: |
        [Actual input data - not placeholder]
      expected_output: |
        [Actual expected output - not placeholder]

  # Error path tests (every failure mode must be tested)
  error_paths:
    - name: "test_[error_scenario]"
      given: "[Precondition that leads to error]"
      when: "[Action that triggers error]"
      then:
        - "Exit code is [specific code]"
        - "Error message contains '[specific text]'"
        - "No side effects occurred"
      real_input: |
        [Actual invalid input]
      expected_error: |
        [Actual error response]

  # Edge case tests (boundary conditions)
  edge_cases:
    - name: "test_[edge_case]"
      scenario: "[Description of boundary condition]"
      input: "[Exact edge case input]"
      expected: "[Exact expected behavior]"

  # Contract verification tests (prove contracts hold)
  contract_tests:
    - name: "test_precondition_[name]"
      verifies: "[Which precondition]"
      test: "[How to verify it]"
    - name: "test_postcondition_[name]"
      verifies: "[Which postcondition]"
      test: "[How to verify it]"
    - name: "test_invariant_[name]"
      verifies: "[Which invariant]"
      test: "[How to verify it]"

# ============================================================================
# SECTION 5: END-TO-END TEST SPECIFICATION
# ============================================================================

# SOUP TO NUTS: Full pipeline testing with REAL data
# This is the Martin Fowler "walking skeleton" test

e2e_tests:

  pipeline_test:
    name: "test_full_pipeline_[feature]"
    description: "Complete end-to-end test from raw input to final output"

    # Step 1: Setup (real state, real data)
    setup:
      files_to_create:
        - path: "[exact file path]"
          content: |
            [exact file content - real data]
      environment:
        - "[Environment variable]=[value]"
      precondition_commands:
        - "[Command to run before test]"

    # Step 2: Execute (real command)
    execute:
      command: "[Exact command to run]"
      stdin: |
        [Exact stdin if any]
      timeout_ms: 5000

    # Step 3: Verify (real assertions)
    verify:
      exit_code: 0
      stdout_contains:
        - "[Exact string that must appear]"
      stdout_matches_json:
        field: "expected.value"
        type: "string"
        pattern: "[regex pattern]"
      files_created:
        - path: "[file that should exist]"
          contains: "[content verification]"
      files_not_modified:
        - "[file that should not change]"
      side_effects:
        - "[Verifiable side effect]"

    # Step 4: Cleanup
    cleanup:
      commands:
        - "[Cleanup command]"
      files_to_delete:
        - "[temp file path]"

  # Additional E2E scenarios
  e2e_scenarios:
    - name: "e2e_[scenario_name]"
      description: "[What this proves]"
      steps:
        - action: "[Step 1]"
          verify: "[Verification]"
        - action: "[Step 2]"
          verify: "[Verification]"

# ============================================================================
# SECTION 6: IMPLEMENTATION TASK LIST
# ============================================================================

# Explicit, ordered, atomic tasks
# Each task should be completable in <30 minutes

implementation_tasks:

  phase_1_tests_first:
    - task: "Write test: test_[name]"
      file: "[exact file path]"
      what: "[Exact test to write]"
      done_when: "Test exists and FAILS (red phase)"

    - task: "Write test: test_[name2]"
      file: "[exact file path]"
      what: "[Exact test to write]"
      done_when: "Test exists and FAILS (red phase)"

  phase_2_implementation:
    - task: "Implement [function/module]"
      file: "[exact file path]"
      what: "[Exact implementation]"
      patterns_to_use:
        - "Result<T, Error> for all fallible operations"
        - "? operator for error propagation"
        - "[Other required patterns]"
      done_when: "All phase_1 tests PASS (green phase)"

  phase_3_integration:
    - task: "Wire up [component] to [system]"
      file: "[exact file path]"
      what: "[Exact integration work]"
      done_when: "E2E test passes"

  phase_4_verification:
    - task: "Run moon run :ci"
      done_when: "All tests pass, no clippy warnings"

    - task: "Manual verification"
      commands:
        - "[Command to run]"
      expected: "[Expected output]"

# ============================================================================
# SECTION 7: FAILURE MODES & DEBUGGING GUIDE
# ============================================================================

# Where to look when things go wrong

failure_modes:

  - symptom: "[Observable problem]"
    likely_cause: "[What probably went wrong]"
    where_to_look:
      - file: "[file path]"
        line_range: "[start-end]"
        what_to_check: "[Specific thing to verify]"
    fix_pattern: "[How to fix it]"

  - symptom: "[Another problem]"
    likely_cause: "[Cause]"
    where_to_look:
      - file: "[file path]"
        function: "[function name]"
        what_to_check: "[What to verify]"
    fix_pattern: "[Fix approach]"

debugging_commands:

  - scenario: "[When X happens]"
    run: "[Command to debug]"
    look_for: "[What to look for in output]"

# ============================================================================
# SECTION 8: COMPLETION CRITERIA
# ============================================================================

# Bead is ONLY complete when ALL of these are true

completion_checklist:

  tests:
    - "[ ] All acceptance tests written and passing"
    - "[ ] All error path tests written and passing"
    - "[ ] All edge case tests written and passing"
    - "[ ] E2E pipeline test passing with real data"
    - "[ ] No mocks or fake data in any test"

  code:
    - "[ ] Implementation uses Result<T, Error> throughout"
    - "[ ] Zero unwrap() or expect() calls"
    - "[ ] All preconditions validated"
    - "[ ] All postconditions guaranteed"
    - "[ ] All invariants maintained"

  ci:
    - "[ ] moon run :ci passes"
    - "[ ] No clippy warnings"
    - "[ ] No compiler warnings"

  documentation:
    - "[ ] Close reason documents what was done"
    - "[ ] Any new CLI flags documented in --help"

# ============================================================================
# SECTION 9: CONTEXT & REFERENCES
# ============================================================================

# Everything the implementer needs to know

context:

  related_files:
    - path: "[file path]"
      relevance: "[Why this file matters]"

  similar_implementations:
    - "[Reference to similar code in codebase]"

  external_references:
    - "[Link to relevant documentation]"

  codebase_patterns:
    - pattern: "[Pattern name]"
      example_location: "[Where to see it used]"
      how_to_apply: "[How to apply here]"

# ============================================================================
# SECTION 10: AI IMPLEMENTATION HINTS
# ============================================================================

# Explicit guidance for AI implementers

ai_hints:

  do:
    - "Use functional patterns: map, and_then, ?"
    - "Return Result<T, Error> from all fallible functions"
    - "Use exhaustive pattern matching"
    - "Follow existing code conventions in [file]"

  do_not:
    - "Do NOT use unwrap() or expect()"
    - "Do NOT use panic!, todo!, or unimplemented!"
    - "Do NOT modify clippy configuration"
    - "Do NOT use raw cargo commands (use moon)"
    - "Do NOT use raw git commands (use jj)"

  code_patterns:
    - name: "[Pattern]"
      use_when: "[When to use]"
      example: |
        [Code example]
```

---

## The Litmus Tests

Before submitting a bead, verify:

1. **GPT-4 Test**: Could GPT-4 implement this without asking clarifying questions?
2. **High School Senior Test**: Could a competent CS student implement this?
3. **90% Coverage Test**: Are there tests for 90%+ of code paths?
4. **No Mocks Test**: Are ALL tests using real data and real commands?
5. **Soup-to-Nuts Test**: Is there an E2E test proving the full pipeline works?
6. **Inversion Test**: Have you defined what must NOT happen?
7. **30-Minute Task Test**: Is every task completable in under 30 minutes?

If any answer is "no", the bead is incomplete.
