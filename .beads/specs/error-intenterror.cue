package beads

// Import the schema
// Note: In production, use: import "intent-cli.com/schema"

// ============================================================================
// BEAD: error-intenterror - IntentError enum implementation
// ============================================================================

bead: #ValidBead & {
    id:              "intent-cli-error01"
    title:           "error: Implement IntentError enum with Railway-Oriented error handling"
    type:            "feature"
    priority:        1
    effort_estimate: "2hr"
    labels:          ["error-handling", "foundation", "m1", "rust-port", "functional"]

    // ========================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ========================================================================
    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use Result<T, IntentError> for all fallible operations",
            "THE SYSTEM SHALL preserve source error chains via #[source] attribute",
            "THE SYSTEM SHALL provide human-readable error messages via Display trait",
            "THE SYSTEM SHALL map errors to POSIX exit codes (0-6)",
        ]

        event_driven: [
            {
                trigger: "WHEN a file read operation fails"
                shall:   "THE SYSTEM SHALL return IntentError::NotFound with path context"
            },
            {
                trigger: "WHEN a CUE spec has syntax errors"
                shall:   "THE SYSTEM SHALL return IntentError::Parse with line, column, and suggestion"
            },
            {
                trigger: "WHEN an HTTP request fails"
                shall:   "THE SYSTEM SHALL return IntentError::Http with method, URL, and status"
            },
            {
                trigger: "WHEN domain validation fails"
                shall:   "THE SYSTEM SHALL return IntentError::Validation with field and message"
            },
        ]

        state_driven: [
            {
                state: "WHILE in JSON output mode"
                shall: "THE SYSTEM SHALL serialize errors to JSON with error code and message"
            },
        ]

        unwanted: [
            {
                condition: "IF any code path uses .unwrap()"
                shall_not: "THE SYSTEM SHALL NOT compile (clippy::unwrap_used = forbid)"
                because:   "unwrap() causes panics which crash the CLI unexpectedly"
            },
            {
                condition: "IF any code path uses .expect()"
                shall_not: "THE SYSTEM SHALL NOT compile (clippy::expect_used = deny)"
                because:   "expect() causes panics with messages that leak internal details"
            },
            {
                condition: "IF an error message contains stack traces"
                shall_not: "THE SYSTEM SHALL NOT expose internal implementation details in user-facing errors"
                because:   "Stack traces confuse users and leak code structure"
            },
        ]
    }

    // ========================================================================
    // SECTION 2: KIRK CONTRACTS
    // ========================================================================
    contracts: {
        preconditions: {
            auth_required: false
            required_inputs: [
                {
                    field:           "error_context"
                    type:            "String"
                    constraints:     "Non-empty, describes what operation failed"
                    example_valid:   "reading spec file"
                    example_invalid: ""
                },
                {
                    field:           "source_error"
                    type:            "Object"
                    constraints:     "Original error if available (Option<std::io::Error>)"
                    example_valid:   "Some(io::Error::NotFound)"
                    example_invalid: "N/A"
                },
            ]
            system_state: [
                "Rust project compiles with clippy::unwrap_used = forbid",
            ]
        }

        postconditions: {
            state_changes: []
            return_guarantees: [
                {
                    field:     "IntentError::exit_code()"
                    guarantee: "Returns i32 in range 1-6 based on error category"
                },
                {
                    field:     "IntentError::to_string()"
                    guarantee: "Returns human-readable message without internal details"
                },
                {
                    field:     "IntentError::source()"
                    guarantee: "Returns underlying error if available"
                },
            ]
            side_effects: []
        }

        invariants: [
            "All IntentError variants are constructible without panic",
            "All error messages are actionable (tell user what to do)",
            "Exit codes are consistent: 1=general, 2=config, 3=validation, 4=notfound, 5=timeout, 6=network",
            "Source error chain is always preserved when available",
        ]
    }

    // ========================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ========================================================================
    inversions: {
        security_failures: [
            {
                failure:     "Error messages leak file paths outside project directory"
                prevention:  "Use relative paths or project-relative paths in error messages"
                test_for_it: "test_error_paths_are_relative"
            },
            {
                failure:     "Error messages contain secret values (API keys, tokens)"
                prevention:  "Never include raw request bodies or headers in error context"
                test_for_it: "test_http_error_no_secrets"
            },
        ]

        usability_failures: [
            {
                failure:     "Error message is too technical for users"
                prevention:  "Include 'suggestion' field with actionable fix"
                test_for_it: "test_parse_error_has_suggestion"
            },
            {
                failure:     "User cannot identify which file caused the error"
                prevention:  "Always include file path in Parse and NotFound errors"
                test_for_it: "test_file_errors_include_path"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Error chain is lost, root cause unknown"
                prevention:  "Use #[source] attribute on all variants with underlying errors"
                test_for_it: "test_error_chain_preserved"
            },
        ]

        integration_failures: [
            {
                failure:     "Exit codes don't match documented behavior"
                prevention:  "Hard-code exit codes in exit_code() method with comments"
                test_for_it: "test_exit_codes_match_spec"
            },
            {
                failure:     "JSON error format breaks AI agent parsing"
                prevention:  "Always include {\"error\": \"message\", \"code\": N} structure"
                test_for_it: "test_json_error_format"
            },
        ]
    }

    // ========================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ========================================================================
    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_not_found_error_display"
                given: "An IntentError::NotFound with path 'specs/missing.cue'"
                when:  "Formatted with Display trait"
                then: [
                    "Output contains 'spec file'",
                    "Output contains 'specs/missing.cue'",
                    "Output is human-readable (no Debug format)",
                ]
                real_input: """
                    IntentError::spec_not_found("specs/missing.cue")
                    """
                expected_output: """
                    "Not found: spec file at 'specs/missing.cue'"
                    """
            },
            {
                name:  "test_parse_error_with_location"
                given: "An IntentError::Parse at line 42, column 10"
                when:  "Formatted with Display trait"
                then: [
                    "Output contains 'line 42'",
                    "Output contains 'column 10'",
                    "Output contains the error message",
                ]
                real_input: """
                    IntentError::parse("spec.cue", 42, 10, "unexpected token")
                    """
                expected_output: """
                    "Parse error in 'spec.cue' at line 42, column 10: unexpected token"
                    """
            },
            {
                name:  "test_http_error_display"
                given: "An IntentError::Http with POST to /users returning 404"
                when:  "Formatted with Display trait"
                then: [
                    "Output contains 'POST'",
                    "Output contains URL",
                    "Output contains '404'",
                ]
                real_input: """
                    IntentError::http("POST", "https://api.example.com/users", 404)
                    """
                expected_output: """
                    "HTTP error: POST https://api.example.com/users returned status 404"
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_from_io_error_conversion"
                given: "A std::io::Error with NotFound kind"
                when:  "Converted to IntentError via From trait"
                then: [
                    "Result is IntentError::Io variant",
                    "Source error is preserved",
                ]
                real_input: """
                    let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
                    let intent_err: IntentError = io_err.into();
                    """
                expected_output: null
                expected_error:  "matches!(intent_err, IntentError::Io { .. })"
            },
        ]

        edge_cases: [
            {
                name:     "test_validation_batch_with_empty_list"
                scenario: "ValidationBatch created with empty error list"
                input:    "IntentError::validation_batch(vec![])"
                expected: "count field is 0, errors field is empty vec"
            },
            {
                name:     "test_very_long_error_message"
                scenario: "Error message exceeds 1000 characters"
                input:    "IntentError::validation('field', 'a'.repeat(2000))"
                expected: "Message is stored without truncation"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_exit_codes_in_range"
                verifies: "All exit codes are in range 1-6"
                test:     "For each variant, assert exit_code() >= 1 && exit_code() <= 6"
            },
            {
                name:     "test_postcondition_display_no_debug"
                verifies: "Display output doesn't contain Debug artifacts"
                test:     "Assert !to_string().contains('IntentError::')"
            },
        ]
    }

    // ========================================================================
    // SECTION 5: E2E TESTS
    // ========================================================================
    e2e_tests: {
        pipeline_test: {
            name:        "test_full_error_pipeline"
            description: "Test error creation, propagation, and output formatting"

            setup: {
                files_to_create: [
                    {
                        path: "/tmp/test-intent/specs/invalid.cue"
                        content: """
                            // Invalid CUE - missing closing brace
                            spec: {
                              name: "Test"
                            """
                    },
                ]
                precondition_commands: [
                    "mkdir -p /tmp/test-intent/specs",
                ]
            }

            execute: {
                command:    "cd /home/lewis/src/intent-cli && cargo test error:: --no-fail-fast"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test_not_found_error_display",
                    "test_parse_error_with_location",
                    "test_exit_codes",
                    "passed",
                ]
            }

            cleanup: {
                files_to_delete: ["/tmp/test-intent"]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_error_json_output"
                description: "Verify errors serialize to JSON correctly"
                steps: [
                    {
                        action: "Create IntentError::validation('email', 'invalid format')"
                        verify: "serde_json::to_string produces valid JSON"
                    },
                ]
            },
        ]
    }

    // ========================================================================
    // SECTION 6: IMPLEMENTATION TASKS
    // ========================================================================
    implementation_tasks: {
        phase_1_tests_first: [
            {
                task:      "Write test: test_not_found_error_display"
                file:      "crates/intent-core/src/error.rs"
                what:      "#[test] fn test_not_found_error_display() { ... }"
                done_when: "Test exists and FAILS (no IntentError yet)"
            },
            {
                task:      "Write test: test_exit_codes"
                file:      "crates/intent-core/src/error.rs"
                what:      "#[test] fn test_exit_codes() { ... }"
                done_when: "Test exists and FAILS"
            },
        ]

        phase_2_implementation: [
            {
                task:      "Implement IntentError enum with thiserror"
                file:      "crates/intent-core/src/error.rs"
                what:      "Define all variants with context fields"
                done_when: "All phase_1 tests PASS"
                patterns_to_use: [
                    "#[derive(Debug, Error)] on enum",
                    "#[error(\"message {field}\")] for Display",
                    "#[source] for error chaining",
                ]
            },
            {
                task:      "Implement constructor methods"
                file:      "crates/intent-core/src/error.rs"
                what:      "Add ::not_found(), ::parse(), ::http() constructors"
                done_when: "Constructor tests pass"
                patterns_to_use: [
                    "impl Into<T> for flexible input types",
                    "#[must_use] on all constructors",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export from lib.rs and prelude"
                file:      "crates/intent-core/src/lib.rs"
                what:      "Add pub mod error; and re-export"
                done_when: "use intent_core::error::IntentError works"
            },
        ]

        phase_4_verification: [
            {
                task:      "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
            },
            {
                task:      "Verify no unwrap in error.rs"
                commands:  ["grep -n 'unwrap()' crates/intent-core/src/error.rs"]
                expected:  "No matches found"
                done_when: "grep returns no matches"
            },
        ]
    }

    // ========================================================================
    // SECTION 7: FAILURE MODES
    // ========================================================================
    failure_modes: {
        failure_modes: [
            {
                symptom:      "Clippy error: unwrap_used"
                likely_cause: "Used .unwrap() somewhere in error handling"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/error.rs"
                        what_to_check: "Search for .unwrap() or .expect()"
                    },
                ]
                fix_pattern: "Replace with .map_err()?, .ok_or()?, or match"
            },
            {
                symptom:      "Test fails with 'assertion failed: display.contains'"
                likely_cause: "#[error] attribute format string doesn't match expected"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/error.rs"
                        function:      "IntentError variants"
                        what_to_check: "Verify #[error] string matches test expectations"
                    },
                ]
                fix_pattern: "Update #[error] attribute or test assertion"
            },
        ]

        debugging_commands: [
            {
                scenario: "When error display format is wrong"
                run:      "cargo test error::tests::test_not_found -- --nocapture"
                look_for: "Actual vs expected string comparison"
            },
        ]
    }

    // ========================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ========================================================================
    completion_checklist: {
        tests: [
            "[ ] All acceptance tests written and passing",
            "[ ] All error path tests written and passing",
            "[ ] E2E pipeline test passing with real data",
            "[ ] No mocks or fake data in any test",
            "[ ] Property test: all errors never panic on construction",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] All preconditions validated at construction",
            "[ ] All postconditions (exit_code range) guaranteed",
            "[ ] #[must_use] on all constructors",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] cargo doc builds without warnings",
        ]

        documentation: [
            "[ ] Module-level doc comment with examples",
            "[ ] Each variant documented with use case",
        ]
    }

    // ========================================================================
    // SECTION 9: CONTEXT
    // ========================================================================
    context: {
        related_files: [
            {
                path:      "crates/intent-core/src/lib.rs"
                relevance: "Exports error module"
            },
            {
                path:      "crates/intent-core/src/prelude.rs"
                relevance: "Re-exports IntentError and IntentResult"
            },
            {
                path:      "Cargo.toml"
                relevance: "thiserror dependency declaration"
            },
        ]

        similar_implementations: [
            "zjj/crates/zjj-core/src/error.rs - Similar Railway-oriented error handling",
        ]

        external_references: [
            "https://docs.rs/thiserror - thiserror documentation",
            "https://fsharpforfunandprofit.com/rop/ - Railway Oriented Programming",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-oriented error propagation"
                example_location: "crates/intent-core/src/error.rs"
                how_to_apply:     "Use .map_err()? and .and_then() chains"
            },
        ]
    }

    // ========================================================================
    // SECTION 10: AI HINTS
    // ========================================================================
    ai_hints: {
        do: [
            "Use thiserror for all error derivation",
            "Include context fields in every variant",
            "Use impl Into<T> for flexible constructors",
            "Add #[must_use] to all constructor methods",
            "Use match statements for exit_code() mapping",
        ]

        do_not: [
            "Do NOT use unwrap() or expect() anywhere",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT use String as error type",
            "Do NOT expose internal paths in error messages",
            "Do NOT modify clippy configuration",
        ]

        code_patterns: [
            {
                name:     "Error constructor with context"
                use_when: "Creating error with additional context"
                example: """
                    pub fn not_found(path: impl Into<PathBuf>, source: io::Error) -> Self {
                        Self::NotFound {
                            resource_type: "file".to_string(),
                            path: path.into(),
                            source: Some(source),
                        }
                    }
                    """
            },
        ]
    }
}

// ============================================================================
// Schema Definitions (inline for self-contained validation)
// ============================================================================

#ValidBead: #EnhancedBead

#EnhancedBead: {
    id:              string & =~"^intent-cli-[a-z0-9]+$"
    title:           string & =~"^[A-Za-z0-9_-]+: .+"
    type:            "feature" | "bug" | "task" | "epic" | "chore"
    priority:        0 | 1 | 2 | 3 | 4
    effort_estimate: "15min" | "30min" | "1hr" | "2hr" | "4hr"
    labels:          [...string]

    ears_requirements:    #EarsRequirements
    contracts:            #KirkContracts
    inversions:           #InversionAnalysis
    acceptance_tests:     #AcceptanceTests
    e2e_tests:            #E2ETests
    implementation_tasks: #ImplementationTasks
    failure_modes:        #FailureModes
    completion_checklist: #CompletionChecklist
    context:              #Context
    ai_hints:             #AIHints
}

#EarsRequirements: {
    ubiquitous:    [...string] & [_, ...]  // At least one required
    event_driven:  [...{trigger: string, shall: string}] & [_, ...]  // At least one required
    state_driven?: [...{state: string, shall: string}]
    optional?:     [...{condition: string, shall: string}]
    unwanted:      [...{condition: string, shall_not: string, because: string}] & [_, ...]  // At least one required
    complex?:      [...{state: string, trigger: string, shall: string}]
}

#KirkContracts: {
    preconditions: {
        auth_required:   bool
        required_inputs: [...{field: string, type: string, constraints: string, example_valid: _, example_invalid: _}]
        system_state?:   [...string]
    }
    postconditions: {
        state_changes:     [...string]
        return_guarantees: [...{field: string, guarantee: string}]
        side_effects?:     [...string]
    }
    invariants: [...string] & [_, ...]
}

#InversionAnalysis: {
    // At least one category must have entries (enforced by usage, not computed)
    security_failures?:       [...{failure: string, prevention: string, test_for_it: string}]
    usability_failures?:      [...{failure: string, prevention: string, test_for_it: string}]
    data_integrity_failures?: [...{failure: string, prevention: string, test_for_it: string}]
    integration_failures?:    [...{failure: string, prevention: string, test_for_it: string}]
}

#AcceptanceTests: {
    happy_paths:     [...{name: string, given: string, when: string, then: [...string], real_input: string, expected_output: string | null, expected_error?: string}] & [_, ...]
    error_paths:     [...{name: string, given: string, when: string, then: [...string], real_input: string, expected_output: string | null, expected_error?: string}] & [_, ...]
    edge_cases?:     [...{name: string, scenario: string, input: string, expected: string}]
    contract_tests?: [...{name: string, verifies: string, test: string}]
}

#E2ETests: {
    pipeline_test: {
        name:        string & =~"^test_full_.+"
        description: string
        setup: {
            files_to_create?:       [...{path: string, content: string}]
            environment?:           [...string]
            precondition_commands?: [...string]
        }
        execute: {
            command:     string
            stdin?:      string
            timeout_ms?: number | *10000
        }
        verify: {
            exit_code:            number
            stdout_contains?:     [...string]
            stdout_matches_json?: [...{path: string, value?: _, type?: string, pattern?: string, min_length?: number}]
            files_created?:       [...{path: string, contains?: string}]
            files_not_modified?:  [...string]
            side_effects?:        [...string]
        }
        cleanup?: {
            commands?:        [...string]
            files_to_delete?: [...string]
        }
    }
    e2e_scenarios?: [...{name: string, description: string, steps: [...{action: string, verify: string}]}]
}

#ImplementationTasks: {
    phase_1_tests_first:    [...{task: string, file?: string, what?: string, done_when: string, patterns_to_use?: [...string], commands?: [...string], expected?: string}] & [_, ...]
    phase_2_implementation: [...{task: string, file?: string, what?: string, done_when: string, patterns_to_use?: [...string], commands?: [...string], expected?: string}] & [_, ...]
    phase_3_integration?:   [...{task: string, file?: string, what?: string, done_when: string, patterns_to_use?: [...string], commands?: [...string], expected?: string}]
    phase_4_verification:   [...{task: string, file?: string, what?: string, done_when: string, patterns_to_use?: [...string], commands?: [...string], expected?: string}] & [_, ...]
}

#FailureModes: {
    failure_modes: [...{symptom: string, likely_cause: string, where_to_look: [...{file: string, line_range?: string, function?: string, what_to_check: string}], fix_pattern: string}]
    debugging_commands?: [...{scenario: string, run: string, look_for: string}]
}

#CompletionChecklist: {
    tests:          [...string] & [_, _, _, _, ...]
    code:           [...string] & [_, _, ...]
    ci:             [...string] & [_, ...]
    documentation?: [...string]
}

#Context: {
    related_files:            [...{path: string, relevance: string}]
    similar_implementations?: [...string]
    external_references?:     [...string]
    codebase_patterns?:       [...{pattern: string, example_location: string, how_to_apply: string}]
}

#AIHints: {
    do:             [...string] & [_, ...]
    do_not:         [...string] & [_, ...]
    code_patterns?: [...{name: string, use_when: string, example: string}]
}
