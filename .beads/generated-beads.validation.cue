package schema

bead_1: #EnhancedBead & {
  id: "intent-cli-api1"
  title: "API: Implement API endpoint"
  type: "task"
  priority: 3
  effort_estimate: "1hr"
  labels: ["api", "endpoint", "implementation"]

  clarifications: {
    clarification_status: "RESOLVED"
  }

  ears_requirements: {
    ubiquitous: ["THE SYSTEM SHALL implement api behavior"]
    event_driven: [{trigger: "WHEN api work is executed", shall: "THE SYSTEM SHALL complete the requested outcome"}]
    unwanted: [{condition: "IF required inputs are missing", shall_not: "THE SYSTEM SHALL NOT continue with invalid state", because: "Invalid state causes unreliable execution"}]
  }

  contracts: {
    preconditions: {
      auth_required: false
      required_inputs: []
      system_state: ["Target codebase is available"]
    }
    postconditions: {
      state_changes: ["Implementation state updated"]
      return_guarantees: []
    }
    invariants: ["No silent failures are accepted"]
  }

  research_requirements: {
    files_to_read: [{
      path: "src/intent.gleam"
      what_to_extract: "Existing CLI flow and command behavior"
      document_in: "research_notes.md"
    }]
    research_questions: [{
      question: "What existing pattern should this bead follow?"
      answered: false
    }]
    research_complete_when: ["Key patterns are documented before changes"]
  }

  inversions: {
    usability_failures: [{
      failure: "User receives unclear output"
      prevention: "Return actionable error and usage guidance"
      test_for_it: "test_error_output_is_actionable"
    }]
  }

  acceptance_tests: {
    happy_paths: [{
      name: "test_happy_path"
      given: "Valid inputs"
      when: "User runs the command"
      then: ["Exit code is 0", "Output matches expected behavior"]
      real_input: "(input error - please try again)"
      expected_output: "Expected successful execution"
    }]
    error_paths: [{
      name: "test_error_path"
      given: "Invalid input"
      when: "User runs the command"
      then: ["Exit code is non-zero", "Error message is clear"]
      real_input: "invalid input"
      expected_output: null
      expected_error: "Actionable validation error"
    }]
  }

  e2e_tests: {
    pipeline_test: {
      name: "test_full_pipeline"
      description: "Validate full CLI workflow"
      setup: {}
      execute: { command: "intent check examples/user-api.cue --target http://localhost:8080" }
      verify: { exit_code: 0 }
    }
  }

  verification_checkpoints: {
    gate_0_research: {
      name: "Research Gate"
      must_pass_before: "Writing code"
      checks: ["Relevant files reviewed"]
      evidence_required: ["Research notes recorded"]
    }
    gate_1_tests: {
      name: "Test Gate"
      must_pass_before: "Implementation"
      checks: ["Failing tests exist"]
      evidence_required: ["Test file added"]
    }
    gate_2_implementation: {
      name: "Implementation Gate"
      must_pass_before: "Completion"
      checks: ["Tests pass"]
      evidence_required: ["Test output captured"]
    }
    gate_3_integration: {
      name: "Integration Gate"
      must_pass_before: "Closing bead"
      checks: ["Integration flow verified"]
      evidence_required: ["Manual verification complete"]
    }
  }

  implementation_tasks: {
    phase_0_research: {
      parallelizable: true
      tasks: [{ task: "Review existing behavior", done_when: "Research complete", parallel_group: "research" }]
    }
    phase_1_tests_first: {
      parallelizable: true
      gate_required: "gate_0_research"
      tasks: [{ task: "Write failing tests", done_when: "Tests fail for expected reason", parallel_group: "tests" }]
    }
    phase_2_implementation: {
      parallelizable: false
      gate_required: "gate_1_tests"
      tasks: [{ task: "Implement required behavior", done_when: "Tests pass" }]
    }
    phase_4_verification: {
      parallelizable: true
      gate_required: "gate_2_implementation"
      tasks: [{ task: "Run CI verification", done_when: "CI passes", parallel_group: "verification" }]
    }
  }

  failure_modes: {
    failure_modes: [{
      symptom: "Feature does not behave as expected"
      likely_cause: "Implementation diverged from contract"
      where_to_look: [{
        file: "src/intent.gleam"
        what_to_check: "Command execution logic"
      }]
      fix_pattern: "Align implementation with tests and contracts"
    }]
  }

  anti_hallucination: {
    read_before_write: [{
      file: "src/intent.gleam"
      must_read_first: true
      key_sections_to_understand: ["Command registration and handlers"]
    }]
    apis_that_exist: []
    no_placeholder_values: ["All values must be derived from real code context"]
    git_verification: {
      before_claiming_done: "git status && git diff && gleam test"
    }
  }

  context_survival: {
    progress_file: {
      path: ".bead-progress/intent-cli-api1/progress.txt"
      format: "Markdown checklist"
    }
    recovery_instructions: "Read progress file and continue incomplete tasks"
  }

  completion_checklist: {
    tests: [
      "[ ] All acceptance tests written and passing",
      "[ ] All error path tests written and passing",
      "[ ] E2E pipeline test passing with real data",
      "[ ] No mocks or fake data in any test"
    ]
    code: [
      "[ ] Implementation uses Result<T, Error> throughout",
      "[ ] Zero unwrap() or expect() calls"
    ]
    ci: [
      "[ ] moon run :ci passes"
    ]
  }

  context: {
    related_files: [{
      path: "src/intent.gleam"
      relevance: "Primary CLI flow"
    }]
  }

  ai_hints: {
    do: ["Follow existing command and output patterns"]
    do_not: ["Do not skip validation gates"]
    constitution: ["Coder liability is absolute: verify every bead before persistence"]
  }
}
