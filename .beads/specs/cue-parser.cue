package beads

// Import the schema
// Note: In production, use: import "intent-cli.com/schema"

// ============================================================================
// BEAD: cue-parser - CUE spec parser implementation
// ============================================================================

bead: #ValidBead & {
    id:              "intent-cli-cue01"
    title:           "cue: Implement CUE spec parser with validation"
    type:            "feature"
    priority:        1
    effort_estimate: "4hr"
    labels:          ["cue", "parser", "m2", "rust-port"]

    // ========================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ========================================================================
    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL parse CUE files into structured IntentSpec objects",
            "THE SYSTEM SHALL validate parsed CUE against the IntentSpec schema",
            "THE SYSTEM SHALL return Result<IntentSpec, IntentError> for all parse operations",
            "THE SYSTEM SHALL preserve source locations for all parse errors",
        ]

        event_driven: [
            {
                trigger: "WHEN a valid CUE file is provided"
                shall:   "THE SYSTEM SHALL return a fully populated IntentSpec struct"
            },
            {
                trigger: "WHEN a CUE file contains syntax errors"
                shall:   "THE SYSTEM SHALL return IntentError::Parse with line, column, and error message"
            },
            {
                trigger: "WHEN a CUE file violates the IntentSpec schema"
                shall:   "THE SYSTEM SHALL return IntentError::Validation listing all schema violations"
            },
            {
                trigger: "WHEN a CUE file does not exist"
                shall:   "THE SYSTEM SHALL return IntentError::NotFound with the file path"
            },
        ]

        state_driven: [
            {
                state: "WHILE the cue CLI is not installed"
                shall: "THE SYSTEM SHALL return IntentError::Config with installation instructions"
            },
        ]

        unwanted: [
            {
                condition: "IF a CUE file has malformed syntax"
                shall_not: "THE SYSTEM SHALL NOT crash or panic"
                because:   "Malformed input is expected; graceful error handling is required"
            },
            {
                condition: "IF a CUE file has missing required fields"
                shall_not: "THE SYSTEM SHALL NOT silently ignore the missing fields"
                because:   "Missing fields cause downstream failures; early detection is critical"
            },
            {
                condition: "IF a CUE parse error occurs"
                shall_not: "THE SYSTEM SHALL NOT return generic error messages without location info"
                because:   "Users need precise locations to fix errors in their spec files"
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
                    field:           "file_path"
                    type:            "PathBuf"
                    constraints:     "Must be a valid filesystem path to a .cue file"
                    example_valid:   "specs/api-users.cue"
                    example_invalid: ""
                },
            ]
            system_state: [
                "cue CLI must be installed and available in PATH",
                "File system must be readable",
            ]
        }

        postconditions: {
            state_changes: []
            return_guarantees: [
                {
                    field:     "IntentSpec"
                    guarantee: "All required fields are populated with validated data"
                },
                {
                    field:     "IntentSpec.name"
                    guarantee: "Non-empty string identifying the spec"
                },
                {
                    field:     "IntentSpec.requests"
                    guarantee: "At least one HTTP request definition"
                },
                {
                    field:     "IntentError::Parse"
                    guarantee: "Contains file path, line number, column number, and message"
                },
            ]
            side_effects: [
                "Spawns cue CLI subprocess",
                "Reads from filesystem",
            ]
        }

        invariants: [
            "Valid CUE input always produces IntentSpec",
            "Invalid CUE input always produces IntentError with location",
            "Parse errors include actionable suggestions when possible",
            "Schema violations list all failing fields, not just the first",
        ]
    }

    // ========================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ========================================================================
    inversions: {
        security_failures: [
            {
                failure:     "CUE CLI command injection via malicious file paths"
                prevention:  "Use Command::arg() not shell interpolation; validate path characters"
                test_for_it: "test_path_injection_prevention"
            },
            {
                failure:     "Information leakage through error messages"
                prevention:  "Sanitize absolute paths to project-relative paths in errors"
                test_for_it: "test_error_paths_are_relative"
            },
        ]

        usability_failures: [
            {
                failure:     "Error messages don't help users fix the problem"
                prevention:  "Include suggestions field with common fixes for each error type"
                test_for_it: "test_parse_error_has_suggestion"
            },
            {
                failure:     "Users can't tell which field failed schema validation"
                prevention:  "Include field path (e.g., 'request.headers[0].name') in validation errors"
                test_for_it: "test_validation_error_includes_field_path"
            },
            {
                failure:     "Cryptic cue CLI error messages passed through verbatim"
                prevention:  "Parse cue CLI stderr and translate to user-friendly messages"
                test_for_it: "test_cue_error_translation"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Partial IntentSpec returned when some fields fail validation"
                prevention:  "Use all-or-nothing parsing: either full IntentSpec or error"
                test_for_it: "test_no_partial_spec_on_error"
            },
            {
                failure:     "JSON output from cue CLI parsed incorrectly"
                prevention:  "Use serde with strict mode; fail on unknown fields"
                test_for_it: "test_strict_json_parsing"
            },
        ]

        integration_failures: [
            {
                failure:     "cue CLI version incompatibility"
                prevention:  "Check cue version on first use; warn if outside supported range"
                test_for_it: "test_cue_version_check"
            },
            {
                failure:     "cue CLI timeout on large/complex specs"
                prevention:  "Set reasonable timeout (30s); return IntentError::Timeout"
                test_for_it: "test_cue_timeout_handling"
            },
        ]
    }

    // ========================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ========================================================================
    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_parse_valid_spec"
                given: "A valid CUE spec file with all required fields"
                when:  "parse_spec(path) is called"
                then: [
                    "Returns Ok(IntentSpec)",
                    "IntentSpec.name matches CUE spec.name",
                    "IntentSpec.requests contains all defined requests",
                    "IntentSpec.assertions contains all defined assertions",
                ]
                real_input: """
                    // test-fixtures/valid-spec.cue
                    spec: {
                        name: "User API"
                        requests: [{
                            name: "get_user"
                            method: "GET"
                            url: "https://api.example.com/users/1"
                        }]
                    }
                    """
                expected_output: """
                    IntentSpec {
                        name: "User API",
                        requests: [Request { name: "get_user", method: GET, url: "..." }]
                    }
                    """
            },
            {
                name:  "test_parse_spec_with_assertions"
                given: "A CUE spec with response assertions"
                when:  "parse_spec(path) is called"
                then: [
                    "Returns Ok(IntentSpec)",
                    "IntentSpec.assertions contains status code checks",
                    "IntentSpec.assertions contains body field checks",
                ]
                real_input: """
                    spec: {
                        name: "User API"
                        requests: [{
                            name: "get_user"
                            method: "GET"
                            url: "https://api.example.com/users/1"
                            assertions: [{
                                type: "status"
                                expected: 200
                            }, {
                                type: "json_path"
                                path: "$.id"
                                expected: 1
                            }]
                        }]
                    }
                    """
                expected_output: """
                    IntentSpec with 2 assertions on get_user request
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_parse_syntax_error"
                given: "A CUE file with syntax error (missing closing brace)"
                when:  "parse_spec(path) is called"
                then: [
                    "Returns Err(IntentError::Parse)",
                    "Error contains file path",
                    "Error contains line number",
                    "Error contains column number",
                    "Error message describes the syntax issue",
                ]
                real_input: """
                    spec: {
                        name: "Broken"
                        requests: [{
                            name: "oops"
                    // missing closing braces
                    """
                expected_output: null
                expected_error:  "IntentError::Parse { file: \"...\", line: 5, column: 1, message: \"...\" }"
            },
            {
                name:  "test_parse_schema_violation"
                given: "A CUE file missing required 'requests' field"
                when:  "parse_spec(path) is called"
                then: [
                    "Returns Err(IntentError::Validation)",
                    "Error lists 'requests' as missing required field",
                    "Error suggests adding the field",
                ]
                real_input: """
                    spec: {
                        name: "Incomplete"
                        // missing 'requests' field
                    }
                    """
                expected_output: null
                expected_error:  "IntentError::Validation { field: \"spec.requests\", message: \"required field missing\" }"
            },
            {
                name:  "test_parse_file_not_found"
                given: "A path to a non-existent CUE file"
                when:  "parse_spec('does-not-exist.cue') is called"
                then: [
                    "Returns Err(IntentError::NotFound)",
                    "Error contains the attempted path",
                ]
                real_input: """
                    parse_spec("specs/nonexistent.cue")
                    """
                expected_output: null
                expected_error:  "IntentError::NotFound { path: \"specs/nonexistent.cue\" }"
            },
        ]

        edge_cases: [
            {
                name:     "test_parse_empty_cue_file"
                scenario: "CUE file exists but is empty"
                input:    "parse_spec('empty.cue') where empty.cue has 0 bytes"
                expected: "IntentError::Validation with 'spec field required' message"
            },
            {
                name:     "test_parse_cue_with_comments_only"
                scenario: "CUE file contains only comments"
                input:    "parse_spec('comments.cue') containing only // comments"
                expected: "IntentError::Validation with 'spec field required' message"
            },
            {
                name:     "test_parse_unicode_in_spec"
                scenario: "CUE file contains unicode in names and values"
                input:    "spec.name contains emoji and non-ASCII characters"
                expected: "IntentSpec with unicode preserved correctly"
            },
            {
                name:     "test_parse_very_large_spec"
                scenario: "CUE file with 100+ requests"
                input:    "spec with requests array containing 100 items"
                expected: "IntentSpec returned within reasonable time (<5s)"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_valid_cue_produces_spec"
                verifies: "Valid CUE input always produces IntentSpec"
                test:     "Property test: for all valid CUE inputs, parse returns Ok"
            },
            {
                name:     "test_invariant_invalid_cue_produces_error_with_location"
                verifies: "Invalid CUE input always produces IntentError with location"
                test:     "Property test: for all invalid inputs, error contains line >= 1"
            },
            {
                name:     "test_postcondition_spec_has_required_fields"
                verifies: "All returned IntentSpec have required fields populated"
                test:     "Assert spec.name.is_empty() == false && spec.requests.len() >= 1"
            },
        ]
    }

    // ========================================================================
    // SECTION 5: E2E TESTS
    // ========================================================================
    e2e_tests: {
        pipeline_test: {
            name:        "test_full_cue_pipeline"
            description: "Test parsing real CUE files through the full pipeline"

            setup: {
                files_to_create: [
                    {
                        path: "/tmp/test-intent/specs/valid-api.cue"
                        content: """
                            // Valid API specification
                            spec: {
                                name: "User Management API"
                                base_url: "https://api.example.com"
                                requests: [{
                                    name: "list_users"
                                    method: "GET"
                                    path: "/users"
                                    assertions: [{
                                        type: "status"
                                        expected: 200
                                    }]
                                }, {
                                    name: "create_user"
                                    method: "POST"
                                    path: "/users"
                                    body: {
                                        name: "Test User"
                                        email: "test@example.com"
                                    }
                                    assertions: [{
                                        type: "status"
                                        expected: 201
                                    }]
                                }]
                            }
                            """
                    },
                    {
                        path: "/tmp/test-intent/specs/invalid-syntax.cue"
                        content: """
                            // Invalid CUE - missing closing brace
                            spec: {
                                name: "Broken"
                                requests: [{
                                    name: "oops"
                            """
                    },
                    {
                        path: "/tmp/test-intent/specs/invalid-schema.cue"
                        content: """
                            // Valid CUE syntax, invalid schema
                            spec: {
                                name: "Missing Requests"
                                // missing required 'requests' field
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "mkdir -p /tmp/test-intent/specs",
                    "which cue || echo 'WARNING: cue CLI not installed'",
                ]
            }

            execute: {
                command:    "cd /home/lewis/src/intent-cli && cargo test cue::parser --no-fail-fast"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test_parse_valid_spec",
                    "test_parse_syntax_error",
                    "test_parse_schema_violation",
                    "passed",
                ]
            }

            cleanup: {
                files_to_delete: ["/tmp/test-intent"]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_cue_to_intent_spec"
                description: "Parse CUE file and verify IntentSpec structure"
                steps: [
                    {
                        action: "Parse valid-api.cue"
                        verify: "IntentSpec has name 'User Management API'"
                    },
                    {
                        action: "Access IntentSpec.requests"
                        verify: "Contains 2 requests: list_users and create_user"
                    },
                    {
                        action: "Access list_users.assertions"
                        verify: "Contains status assertion expecting 200"
                    },
                ]
            },
            {
                name:        "e2e_cue_error_reporting"
                description: "Verify error messages are actionable"
                steps: [
                    {
                        action: "Parse invalid-syntax.cue"
                        verify: "Error message includes line number and suggestion"
                    },
                    {
                        action: "Parse invalid-schema.cue"
                        verify: "Error lists 'requests' as missing required field"
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
                task:      "Write test: test_parse_valid_spec"
                file:      "crates/intent-core/src/cue/parser.rs"
                what:      "#[test] fn test_parse_valid_spec() with fixture file"
                done_when: "Test exists and FAILS (no parser implementation yet)"
            },
            {
                task:      "Write test: test_parse_syntax_error"
                file:      "crates/intent-core/src/cue/parser.rs"
                what:      "#[test] fn test_parse_syntax_error() with malformed CUE"
                done_when: "Test exists and FAILS"
            },
            {
                task:      "Write test: test_parse_schema_violation"
                file:      "crates/intent-core/src/cue/parser.rs"
                what:      "#[test] fn test_parse_schema_violation() with invalid schema"
                done_when: "Test exists and FAILS"
            },
            {
                task:      "Create test fixture files"
                file:      "crates/intent-core/tests/fixtures/"
                what:      "valid-spec.cue, invalid-syntax.cue, invalid-schema.cue"
                done_when: "Fixture files exist in tests/fixtures/"
            },
        ]

        phase_2_implementation: [
            {
                task:      "Implement CueParser struct"
                file:      "crates/intent-core/src/cue/parser.rs"
                what:      "CueParser with parse_spec(path: &Path) -> Result<IntentSpec, IntentError>"
                done_when: "test_parse_valid_spec PASSES"
                patterns_to_use: [
                    "std::process::Command to shell out to cue CLI",
                    "cue export --out json for JSON output",
                    "serde_json::from_str for JSON parsing",
                ]
            },
            {
                task:      "Implement error parsing from cue CLI"
                file:      "crates/intent-core/src/cue/parser.rs"
                what:      "Parse cue CLI stderr to extract line, column, message"
                done_when: "test_parse_syntax_error PASSES"
                patterns_to_use: [
                    "Regex to extract location from cue error format",
                    "IntentError::parse() constructor with location info",
                ]
            },
            {
                task:      "Implement schema validation"
                file:      "crates/intent-core/src/cue/parser.rs"
                what:      "Validate parsed JSON against IntentSpec schema"
                done_when: "test_parse_schema_violation PASSES"
                patterns_to_use: [
                    "serde #[serde(deny_unknown_fields)]",
                    "Custom validation after deserialization",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Create cue module"
                file:      "crates/intent-core/src/cue/mod.rs"
                what:      "pub mod parser; with re-exports"
                done_when: "use intent_core::cue::CueParser works"
            },
            {
                task:      "Export from lib.rs"
                file:      "crates/intent-core/src/lib.rs"
                what:      "pub mod cue; and prelude re-export"
                done_when: "use intent_core::prelude::CueParser works"
            },
            {
                task:      "Integrate with test runner"
                file:      "crates/intent-cli/src/commands/test.rs"
                what:      "Use CueParser to load specs before test execution"
                done_when: "intent test specs/*.cue works"
            },
        ]

        phase_4_verification: [
            {
                task:      "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
            },
            {
                task:      "Verify no unwrap in parser"
                commands:  ["grep -n 'unwrap()' crates/intent-core/src/cue/parser.rs"]
                expected:  "No matches found"
                done_when: "grep returns no matches"
            },
            {
                task:      "Test with real CUE specs"
                commands:  ["cargo run -- test specs/example.cue"]
                done_when: "Real spec files parse without errors"
            },
        ]
    }

    // ========================================================================
    // SECTION 7: FAILURE MODES
    // ========================================================================
    failure_modes: {
        failure_modes: [
            {
                symptom:      "CUE CLI not found: 'cue: command not found'"
                likely_cause: "cue CLI not installed or not in PATH"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/cue/parser.rs"
                        function:      "parse_spec"
                        what_to_check: "Command::new('cue') error handling"
                    },
                ]
                fix_pattern: "Return IntentError::Config with install instructions: 'Install CUE: https://cuelang.org/docs/install/'"
            },
            {
                symptom:      "JSON parse error from cue export"
                likely_cause: "cue export output format changed or unexpected"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/cue/parser.rs"
                        function:      "parse_json_output"
                        what_to_check: "serde_json::from_str error handling"
                    },
                ]
                fix_pattern: "Log raw cue output; check cue version; use strict serde parsing"
            },
            {
                symptom:      "File not found but path looks correct"
                likely_cause: "Relative path resolution from wrong directory"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/cue/parser.rs"
                        function:      "parse_spec"
                        what_to_check: "Path canonicalization before Command::arg()"
                    },
                ]
                fix_pattern: "Use std::fs::canonicalize() or resolve relative to project root"
            },
            {
                symptom:      "cue export hangs indefinitely"
                likely_cause: "CUE file has infinite recursion or very complex constraints"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/cue/parser.rs"
                        function:      "execute_cue_command"
                        what_to_check: "Timeout configuration on Command"
                    },
                ]
                fix_pattern: "Set timeout using wait_timeout crate; return IntentError::Timeout"
            },
        ]

        debugging_commands: [
            {
                scenario: "When cue CLI output is unexpected"
                run:      "cue export --out json specs/problem.cue 2>&1"
                look_for: "Raw JSON output or error message format"
            },
            {
                scenario: "When schema validation fails unexpectedly"
                run:      "cue export --out json specs/problem.cue | jq ."
                look_for: "Field names and structure in JSON output"
            },
            {
                scenario: "When error location is wrong"
                run:      "cue vet specs/problem.cue 2>&1 | head -5"
                look_for: "Line:column format in cue error output"
            },
        ]
    }

    // ========================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ========================================================================
    completion_checklist: {
        tests: [
            "[ ] test_parse_valid_spec passing with real CUE file",
            "[ ] test_parse_syntax_error returns correct line/column",
            "[ ] test_parse_schema_violation lists all missing fields",
            "[ ] test_parse_file_not_found returns IntentError::NotFound",
            "[ ] E2E test_full_cue_pipeline passing with multiple files",
        ]

        code: [
            "[ ] CueParser::parse_spec returns Result<IntentSpec, IntentError>",
            "[ ] All CUE constructs handled (objects, arrays, strings, numbers)",
            "[ ] Error locations are accurate (line, column from cue CLI)",
            "[ ] No unwrap() or expect() in parser code",
            "[ ] Timeout handling for cue CLI execution",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] cargo doc builds without warnings",
        ]

        documentation: [
            "[ ] Module-level doc comment with usage example",
            "[ ] CueParser struct documented with supported CUE features",
        ]
    }

    // ========================================================================
    // SECTION 9: CONTEXT
    // ========================================================================
    context: {
        related_files: [
            {
                path:      "crates/intent-core/src/error.rs"
                relevance: "IntentError types used for parse failures"
            },
            {
                path:      "crates/intent-core/src/types.rs"
                relevance: "IntentSpec struct that parser produces"
            },
            {
                path:      "crates/intent-cli/src/commands/test.rs"
                relevance: "Test runner that consumes parsed specs"
            },
            {
                path:      "specs/"
                relevance: "Example CUE spec files for testing"
            },
        ]

        similar_implementations: [
            "cue-rs crate - Rust bindings for CUE (if available)",
            "serde_json parsing patterns in existing codebase",
        ]

        external_references: [
            "https://cuelang.org/docs/reference/spec/ - CUE language specification",
            "https://cuelang.org/docs/reference/cli/ - cue CLI documentation",
            "https://docs.rs/serde_json - serde_json for JSON parsing",
        ]

        codebase_patterns: [
            {
                pattern:          "Shell out to external CLI"
                example_location: "Similar pattern in other CLI tools"
                how_to_apply:     "Use std::process::Command with timeout and error capture"
            },
            {
                pattern:          "Result<T, IntentError> return type"
                example_location: "crates/intent-core/src/error.rs"
                how_to_apply:     "All fallible functions return this type"
            },
        ]
    }

    // ========================================================================
    // SECTION 10: AI HINTS
    // ========================================================================
    ai_hints: {
        do: [
            "Shell out to cue CLI using std::process::Command",
            "Use 'cue export --out json' to get JSON output",
            "Parse JSON output with serde_json into IntentSpec",
            "Extract error locations from cue CLI stderr using regex",
            "Map cue errors to IntentError::Parse with line, column, message",
            "Use wait_timeout crate for timeout handling",
            "Canonicalize paths before passing to cue CLI",
        ]

        do_not: [
            "Do NOT implement a CUE parser from scratch",
            "Do NOT use unwrap() or expect() - use ? operator",
            "Do NOT ignore cue CLI stderr - it contains error details",
            "Do NOT assume cue CLI is always installed",
            "Do NOT block indefinitely on cue CLI execution",
            "Do NOT expose absolute paths in error messages",
        ]

        code_patterns: [
            {
                name:     "Shell out to cue CLI"
                use_when: "Parsing a CUE file"
                example: """
                    use std::process::Command;

                    fn execute_cue(path: &Path) -> Result<String, IntentError> {
                        let output = Command::new("cue")
                            .args(["export", "--out", "json"])
                            .arg(path)
                            .output()
                            .map_err(|e| IntentError::config("cue CLI not found", e))?;

                        if output.status.success() {
                            String::from_utf8(output.stdout)
                                .map_err(|e| IntentError::parse(path, 0, 0, "invalid UTF-8"))
                        } else {
                            let stderr = String::from_utf8_lossy(&output.stderr);
                            Err(parse_cue_error(path, &stderr))
                        }
                    }
                    """
            },
            {
                name:     "Parse cue CLI error output"
                use_when: "Extracting location from cue error message"
                example: """
                    use regex::Regex;

                    fn parse_cue_error(path: &Path, stderr: &str) -> IntentError {
                        // cue error format: "path/file.cue:42:10: error message"
                        let re = Regex::new(r":(\\d+):(\\d+):\\s*(.+)$").unwrap();
                        if let Some(caps) = re.captures(stderr) {
                            let line = caps[1].parse().unwrap_or(0);
                            let col = caps[2].parse().unwrap_or(0);
                            let msg = caps[3].to_string();
                            IntentError::parse(path, line, col, msg)
                        } else {
                            IntentError::parse(path, 0, 0, stderr.to_string())
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
