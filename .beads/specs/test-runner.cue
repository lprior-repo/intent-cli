package beads

// Import the schema
// Note: In production, use: import "intent-cli.com/schema"

// ============================================================================
// BEAD: test-runner - Test runner with assertion engine
// ============================================================================

bead: #ValidBead & {
    id:              "intent-cli-runner01"
    title:           "runner: Implement test runner with assertion engine"
    type:            "feature"
    priority:        2
    effort_estimate: "4hr"
    labels:          ["runner", "testing", "m4", "rust-port"]

    // ========================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ========================================================================
    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL execute test specs by making HTTP calls and running assertions",
            "THE SYSTEM SHALL report test results as pass, fail, or skip",
            "THE SYSTEM SHALL aggregate results across multiple tests in a suite",
            "THE SYSTEM SHALL support both sequential and parallel test execution",
        ]

        event_driven: [
            {
                trigger: "WHEN a test spec is loaded and validated"
                shall:   "THE SYSTEM SHALL execute the HTTP call defined in the spec"
            },
            {
                trigger: "WHEN an HTTP response is received"
                shall:   "THE SYSTEM SHALL run all assertions defined in the spec against the response"
            },
            {
                trigger: "WHEN all assertions pass"
                shall:   "THE SYSTEM SHALL mark the test as passed and emit a success result"
            },
            {
                trigger: "WHEN any assertion fails"
                shall:   "THE SYSTEM SHALL mark the test as failed with a detailed diff"
            },
            {
                trigger: "WHEN a test is marked as skip in the spec"
                shall:   "THE SYSTEM SHALL skip execution and report skip reason"
            },
        ]

        state_driven: [
            {
                state: "WHILE running in parallel mode"
                shall: "THE SYSTEM SHALL isolate test state to prevent race conditions"
            },
            {
                state: "WHILE in verbose mode"
                shall: "THE SYSTEM SHALL emit progress for each assertion check"
            },
        ]

        unwanted: [
            {
                condition: "IF an assertion failure occurs"
                shall_not: "THE SYSTEM SHALL NOT crash or panic"
                because:   "Assertion failures are expected outcomes, not program errors"
            },
            {
                condition: "IF diff output is generated"
                shall_not: "THE SYSTEM SHALL NOT produce uncolored or unclear diff output"
                because:   "Unclear diffs make debugging test failures difficult"
            },
            {
                condition: "IF tests are run in parallel"
                shall_not: "THE SYSTEM SHALL NOT allow shared mutable state between tests"
                because:   "Race conditions cause flaky tests and non-deterministic results"
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
                    field:           "test_spec"
                    type:            "TestSpec"
                    constraints:     "Valid CUE spec with request and assertions defined"
                    example_valid:   "TestSpec { request: { method: GET, url: /health }, assertions: [...] }"
                    example_invalid: "TestSpec { request: null }"
                },
                {
                    field:           "http_client"
                    type:            "HttpClient"
                    constraints:     "Configured HTTP client with timeout settings"
                    example_valid:   "HttpClient::new(Duration::from_secs(30))"
                    example_invalid: "None"
                },
            ]
            system_state: [
                "HTTP client is configured and ready",
                "CUE spec has been parsed and validated",
                "Target endpoint is reachable (or test will timeout)",
            ]
        }

        postconditions: {
            state_changes: [
                "Test result is recorded in the result collector",
            ]
            return_guarantees: [
                {
                    field:     "TestResult::status"
                    guarantee: "Returns Pass, Fail, or Skip - never undefined"
                },
                {
                    field:     "TestResult::duration"
                    guarantee: "Contains actual HTTP request duration in milliseconds"
                },
                {
                    field:     "TestResult::assertions"
                    guarantee: "Contains result for each assertion in the spec"
                },
            ]
            side_effects: [
                "HTTP request is made to the target endpoint",
            ]
        }

        invariants: [
            "A test that passes all assertions returns Pass status",
            "A test that fails any assertion returns Fail status with all failures",
            "A skipped test never makes HTTP requests",
            "Parallel execution produces identical results to sequential execution",
            "Assertion order in results matches order in spec",
        ]
    }

    // ========================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ========================================================================
    inversions: {
        usability_failures: [
            {
                failure:     "Assertion failure produces crash instead of failure result"
                prevention:  "Wrap assertion checks in Result, never panic on mismatch"
                test_for_it: "test_assertion_failure_no_crash"
            },
            {
                failure:     "Diff output is unclear or unreadable"
                prevention:  "Use colored diff library with context lines around changes"
                test_for_it: "test_diff_output_readability"
            },
            {
                failure:     "User cannot tell which assertion failed"
                prevention:  "Include assertion name, expected, actual, and JSON path in output"
                test_for_it: "test_failure_identifies_assertion"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Parallel execution produces race conditions"
                prevention:  "Use Arc<Mutex> or channels for shared state, prefer immutable data"
                test_for_it: "test_parallel_execution_deterministic"
            },
            {
                failure:     "Test results are lost or not collected"
                prevention:  "Use thread-safe result collector, verify count matches input"
                test_for_it: "test_all_results_collected"
            },
        ]

        integration_failures: [
            {
                failure:     "HTTP client errors are confused with test failures"
                prevention:  "Distinguish Error status from Fail status in TestResult"
                test_for_it: "test_http_error_vs_assertion_fail"
            },
            {
                failure:     "Timeout is not respected, tests hang forever"
                prevention:  "Set per-request timeout in HTTP client, propagate timeout errors"
                test_for_it: "test_timeout_respected"
            },
            {
                failure:     "Spec validation errors prevent execution"
                prevention:  "Validate specs before test run, report validation errors separately"
                test_for_it: "test_spec_validation_before_run"
            },
        ]

        security_failures: [
            {
                failure:     "Sensitive response data leaked in error messages"
                prevention:  "Truncate response bodies in error output, redact auth headers"
                test_for_it: "test_no_sensitive_data_in_errors"
            },
        ]
    }

    // ========================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ========================================================================
    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_run_single_test"
                given: "A valid test spec with GET /health endpoint"
                when:  "Runner executes the spec against a mock server"
                then: [
                    "HTTP GET request is made to /health",
                    "Response is captured with status and body",
                    "TestResult::status is Pass",
                    "TestResult::duration is recorded",
                ]
                real_input: """
                    let spec = TestSpec::from_cue(r#"
                        request: { method: "GET", url: "/health" }
                        response: { status: 200 }
                    "#)?;
                    runner.run_test(&spec).await
                    """
                expected_output: """
                    TestResult { status: Pass, duration_ms: 42, assertions: [AssertionResult { name: "status", passed: true }] }
                    """
            },
            {
                name:  "test_assert_status_code"
                given: "A test spec expecting status 201"
                when:  "Server responds with status 201"
                then: [
                    "Status assertion passes",
                    "TestResult::status is Pass",
                ]
                real_input: """
                    let spec = TestSpec::from_cue(r#"
                        request: { method: "POST", url: "/users" }
                        response: { status: 201 }
                    "#)?;
                    """
                expected_output: """
                    TestResult { status: Pass, assertions: [AssertionResult { name: "status", passed: true, expected: "201", actual: "201" }] }
                    """
            },
            {
                name:  "test_assert_json_path"
                given: "A test spec asserting $.user.name equals 'Alice'"
                when:  "Response contains {\"user\": {\"name\": \"Alice\"}}"
                then: [
                    "JSON path assertion passes",
                    "Path $.user.name is correctly evaluated",
                    "TestResult::status is Pass",
                ]
                real_input: """
                    let spec = TestSpec::from_cue(r#"
                        request: { method: "GET", url: "/user/1" }
                        response: {
                            assertions: [
                                { jsonpath: "$.user.name", equals: "Alice" }
                            ]
                        }
                    "#)?;
                    """
                expected_output: """
                    TestResult { status: Pass, assertions: [AssertionResult { name: "$.user.name", passed: true, expected: "Alice", actual: "Alice" }] }
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_assertion_failure_returns_fail"
                given: "A test spec expecting status 200"
                when:  "Server responds with status 404"
                then: [
                    "Status assertion fails",
                    "TestResult::status is Fail",
                    "Failure includes expected=200, actual=404",
                ]
                real_input: """
                    let spec = TestSpec::from_cue(r#"
                        request: { method: "GET", url: "/missing" }
                        response: { status: 200 }
                    "#)?;
                    """
                expected_output: null
                expected_error:  "TestResult { status: Fail, assertions: [AssertionResult { passed: false, expected: \"200\", actual: \"404\" }] }"
            },
            {
                name:  "test_timeout_returns_error"
                given: "A test spec with 100ms timeout"
                when:  "Server takes 5 seconds to respond"
                then: [
                    "TestResult::status is Error",
                    "Error contains timeout message",
                    "Duration shows actual wait time",
                ]
                real_input: """
                    let spec = TestSpec::from_cue(r#"
                        request: { method: "GET", url: "/slow", timeout_ms: 100 }
                    "#)?;
                    """
                expected_output: null
                expected_error:  "TestResult { status: Error, error: \"timeout after 100ms\" }"
            },
        ]

        edge_cases: [
            {
                name:     "test_empty_assertions_auto_pass"
                scenario: "Test spec with no assertions defined"
                input:    "TestSpec { request: { method: GET, url: /health }, assertions: [] }"
                expected: "TestResult::status is Pass (successful response = pass)"
            },
            {
                name:     "test_skip_test_no_request"
                scenario: "Test spec with skip: true"
                input:    "TestSpec { skip: true, skip_reason: \"pending implementation\" }"
                expected: "TestResult::status is Skip, no HTTP request made"
            },
            {
                name:     "test_multiple_assertions_first_fail"
                scenario: "Multiple assertions where first fails"
                input:    "TestSpec with assertions [status: 200, json: $.id exists]"
                expected: "All assertions are run, all failures reported"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_parallel_equals_sequential"
                verifies: "Parallel execution produces identical results to sequential"
                test:     "Run same suite sequentially and in parallel, compare results (ignoring duration)"
            },
            {
                name:     "test_postcondition_all_assertions_in_result"
                verifies: "Every assertion in spec appears in result"
                test:     "Assert result.assertions.len() == spec.assertions.len()"
            },
        ]
    }

    // ========================================================================
    // SECTION 5: E2E TESTS
    // ========================================================================
    e2e_tests: {
        pipeline_test: {
            name:        "test_full_runner_pipeline"
            description: "Test complete runner pipeline from spec loading to result output"

            setup: {
                files_to_create: [
                    {
                        path: "/tmp/test-intent-runner/specs/health.cue"
                        content: """
                            package tests

                            test: {
                                name: "health_check"
                                request: {
                                    method: "GET"
                                    url:    "http://localhost:9999/health"
                                }
                                response: {
                                    status: 200
                                    assertions: [
                                        { jsonpath: "$.status", equals: "ok" }
                                    ]
                                }
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "mkdir -p /tmp/test-intent-runner/specs",
                ]
            }

            execute: {
                command:    "cd /home/lewis/src/intent-cli && cargo test runner:: --no-fail-fast"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test_run_single_test",
                    "test_assert_status_code",
                    "test_assert_json_path",
                    "passed",
                ]
            }

            cleanup: {
                files_to_delete: ["/tmp/test-intent-runner"]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_parallel_suite_execution"
                description: "Run multiple tests in parallel and verify all complete"
                steps: [
                    {
                        action: "Load 10 test specs from specs/ directory"
                        verify: "All specs parsed successfully"
                    },
                    {
                        action: "Execute all tests with parallelism=4"
                        verify: "All 10 results collected, no data races"
                    },
                    {
                        action: "Compare results to sequential run"
                        verify: "Same pass/fail status for each test"
                    },
                ]
            },
            {
                name:        "e2e_assertion_types_comprehensive"
                description: "Verify all assertion types work correctly"
                steps: [
                    {
                        action: "Run test with status assertion"
                        verify: "Status code compared correctly"
                    },
                    {
                        action: "Run test with JSON path assertion"
                        verify: "JSONPath evaluated and compared"
                    },
                    {
                        action: "Run test with header assertion"
                        verify: "Response header matched"
                    },
                    {
                        action: "Run test with body contains assertion"
                        verify: "Substring search works"
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
                task:      "Write test: test_assert_status_code"
                file:      "crates/intent-runner/src/lib.rs"
                what:      "#[test] fn test_assert_status_code() { ... }"
                done_when: "Test exists and FAILS (no assertion engine yet)"
            },
            {
                task:      "Write test: test_assert_json_path"
                file:      "crates/intent-runner/src/assertions.rs"
                what:      "#[test] fn test_assert_json_path() { ... }"
                done_when: "Test exists and FAILS"
            },
            {
                task:      "Write test: test_run_single_test"
                file:      "crates/intent-runner/src/runner.rs"
                what:      "#[tokio::test] async fn test_run_single_test() { ... }"
                done_when: "Test exists and FAILS"
            },
        ]

        phase_2_implementation: [
            {
                task:      "Implement AssertionEngine with status assertion"
                file:      "crates/intent-runner/src/assertions.rs"
                what:      "AssertionEngine::assert_status() method"
                done_when: "test_assert_status_code PASSES"
                patterns_to_use: [
                    "Return AssertionResult with pass/fail and diff",
                    "Use colored crate for diff output",
                ]
            },
            {
                task:      "Implement JSONPath assertion"
                file:      "crates/intent-runner/src/assertions.rs"
                what:      "AssertionEngine::assert_json_path() method"
                done_when: "test_assert_json_path PASSES"
                patterns_to_use: [
                    "Use jsonpath-rust or serde_json_path crate",
                    "Handle missing paths as assertion failure, not panic",
                ]
            },
            {
                task:      "Implement TestRunner with HTTP execution"
                file:      "crates/intent-runner/src/runner.rs"
                what:      "TestRunner::run_test() method"
                done_when: "test_run_single_test PASSES"
                patterns_to_use: [
                    "Use reqwest for HTTP client",
                    "Measure duration with std::time::Instant",
                    "Collect all assertion results before returning",
                ]
            },
            {
                task:      "Implement parallel execution with rayon"
                file:      "crates/intent-runner/src/runner.rs"
                what:      "TestRunner::run_suite_parallel() method"
                done_when: "test_parallel_execution_deterministic PASSES"
                patterns_to_use: [
                    "Use rayon::par_iter() for parallelism",
                    "Collect results into Arc<Mutex<Vec<TestResult>>>",
                    "Avoid shared mutable state in test execution",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export runner and assertions from lib.rs"
                file:      "crates/intent-runner/src/lib.rs"
                what:      "pub mod runner; pub mod assertions;"
                done_when: "use intent_runner::TestRunner works"
            },
            {
                task:      "Add http-client and cue-parser dependencies"
                file:      "crates/intent-runner/Cargo.toml"
                what:      "Add intent-http, intent-cue to dependencies"
                done_when: "Cargo build succeeds with all deps"
            },
        ]

        phase_4_verification: [
            {
                task:      "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
            },
            {
                task:      "Verify no panics in assertion code"
                commands:  ["grep -n 'panic!' crates/intent-runner/src/assertions.rs"]
                expected:  "No matches found"
                done_when: "grep returns no matches"
            },
            {
                task:      "Verify colored diff output"
                commands:  ["cargo test runner::tests::test_diff_output -- --nocapture"]
                expected:  "Colored output visible in terminal"
                done_when: "Manual verification of colored output"
            },
        ]
    }

    // ========================================================================
    // SECTION 7: FAILURE MODES
    // ========================================================================
    failure_modes: {
        failure_modes: [
            {
                symptom:      "Test crashes on assertion failure instead of returning Fail"
                likely_cause: "Used assert! macro instead of returning AssertionResult"
                where_to_look: [
                    {
                        file:          "crates/intent-runner/src/assertions.rs"
                        what_to_check: "Search for assert!, assert_eq!, panic!"
                    },
                ]
                fix_pattern: "Replace assert! with comparison that returns AssertionResult"
            },
            {
                symptom:      "Diff output is plain text without colors"
                likely_cause: "Colored crate not detecting terminal or colors disabled"
                where_to_look: [
                    {
                        file:          "crates/intent-runner/src/assertions.rs"
                        function:      "format_diff"
                        what_to_check: "Verify ColoredString is used, check TERM env"
                    },
                ]
                fix_pattern: "Use colored::control::set_override(true) in tests or check terminal detection"
            },
            {
                symptom:      "Parallel tests produce different results than sequential"
                likely_cause: "Shared mutable state between tests (global variables, static mut)"
                where_to_look: [
                    {
                        file:          "crates/intent-runner/src/runner.rs"
                        what_to_check: "Look for static, lazy_static, or shared references"
                    },
                ]
                fix_pattern: "Use thread-local storage or pass owned data to each test"
            },
            {
                symptom:      "Timeout test hangs forever"
                likely_cause: "HTTP client timeout not configured or not respected"
                where_to_look: [
                    {
                        file:          "crates/intent-runner/src/runner.rs"
                        function:      "run_test"
                        what_to_check: "Verify timeout is set on reqwest::Client"
                    },
                ]
                fix_pattern: "Set .timeout(Duration::from_millis(spec.timeout_ms)) on request"
            },
            {
                symptom:      "JSONPath assertion fails on valid path"
                likely_cause: "JSONPath library uses different syntax ($ vs root)"
                where_to_look: [
                    {
                        file:          "crates/intent-runner/src/assertions.rs"
                        function:      "assert_json_path"
                        what_to_check: "Verify path syntax matches library documentation"
                    },
                ]
                fix_pattern: "Check library docs, may need to adjust path format"
            },
        ]

        debugging_commands: [
            {
                scenario: "When assertion comparison is wrong"
                run:      "cargo test assertions::tests::test_assert -- --nocapture"
                look_for: "Printed expected vs actual values"
            },
            {
                scenario: "When parallel execution has race conditions"
                run:      "RUST_TEST_THREADS=1 cargo test runner::tests::test_parallel"
                look_for: "Test behavior with single thread vs multi-thread"
            },
            {
                scenario: "When HTTP request is not made"
                run:      "RUST_LOG=debug cargo test runner::tests::test_run -- --nocapture"
                look_for: "HTTP request/response logs"
            },
        ]
    }

    // ========================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ========================================================================
    completion_checklist: {
        tests: [
            "[ ] test_assert_status_code passing",
            "[ ] test_assert_json_path passing",
            "[ ] test_run_single_test passing",
            "[ ] test_assertion_failure_no_crash passing",
            "[ ] test_parallel_execution_deterministic passing",
            "[ ] E2E test_full_runner_pipeline passing with real HTTP",
        ]

        code: [
            "[ ] All assertion types tested (status, json_path, header, body)",
            "[ ] Parallel execution is safe (no data races)",
            "[ ] Colored diff output for assertion failures",
            "[ ] No assert! or panic! in assertion code",
            "[ ] Timeout respected for all HTTP requests",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings in intent-runner crate",
            "[ ] Integration tests pass with mock server",
        ]

        documentation: [
            "[ ] Module-level doc comment with usage example",
            "[ ] AssertionEngine methods documented",
            "[ ] TestRunner usage documented with examples",
        ]
    }

    // ========================================================================
    // SECTION 9: CONTEXT
    // ========================================================================
    context: {
        related_files: [
            {
                path:      "crates/intent-http/src/client.rs"
                relevance: "HTTP client used for making requests"
            },
            {
                path:      "crates/intent-cue/src/parser.rs"
                relevance: "CUE parser for loading test specs"
            },
            {
                path:      "crates/intent-core/src/types.rs"
                relevance: "TestSpec, TestResult, AssertionResult types"
            },
        ]

        similar_implementations: [
            "hurl - Similar CLI test runner for HTTP (https://hurl.dev)",
            "pytest-httpx - Python HTTP testing patterns",
        ]

        external_references: [
            "https://docs.rs/rayon - Rayon parallel iterators documentation",
            "https://docs.rs/jsonpath-rust - JSONPath library for Rust",
            "https://docs.rs/similar - Text diffing library",
            "https://docs.rs/colored - Terminal colors library",
        ]

        codebase_patterns: [
            {
                pattern:          "Result-based error handling"
                example_location: "crates/intent-core/src/error.rs"
                how_to_apply:     "Use IntentError for all fallible operations in runner"
            },
            {
                pattern:          "Async HTTP with reqwest"
                example_location: "crates/intent-http/src/client.rs"
                how_to_apply:     "Use existing HttpClient wrapper for requests"
            },
        ]
    }

    // ========================================================================
    // SECTION 10: AI HINTS
    // ========================================================================
    ai_hints: {
        do: [
            "Use rayon for parallel test execution (par_iter on test specs)",
            "Use jq-like JSON path syntax (jsonpath-rust crate)",
            "Use colored crate for diff output with red/green highlighting",
            "Use similar crate for generating text diffs",
            "Return AssertionResult with expected/actual for every check",
            "Measure duration using std::time::Instant",
            "Wrap HTTP calls in timeout with tokio::time::timeout",
        ]

        do_not: [
            "Do NOT use unwrap() or expect() - use proper error handling",
            "Do NOT use assert! macros in assertion engine (return Result instead)",
            "Do NOT panic on assertion failures",
            "Do NOT use global mutable state for test results",
            "Do NOT block the async runtime with synchronous calls",
            "Do NOT expose full response bodies in error messages (truncate)",
        ]

        code_patterns: [
            {
                name:     "Assertion result construction"
                use_when: "Returning assertion comparison result"
                example: """
                    pub fn assert_status(expected: u16, actual: u16) -> AssertionResult {
                        AssertionResult {
                            name: "status".to_string(),
                            passed: expected == actual,
                            expected: expected.to_string(),
                            actual: actual.to_string(),
                            diff: if expected != actual {
                                Some(format!("expected {} but got {}", expected.green(), actual.red()))
                            } else {
                                None
                            },
                        }
                    }
                    """
            },
            {
                name:     "Parallel test execution with rayon"
                use_when: "Running multiple tests concurrently"
                example: """
                    use rayon::prelude::*;

                    pub fn run_suite_parallel(specs: Vec<TestSpec>) -> Vec<TestResult> {
                        specs.par_iter()
                            .map(|spec| self.run_test(spec))
                            .collect()
                    }
                    """
            },
            {
                name:     "JSON path extraction"
                use_when: "Evaluating JSONPath assertions"
                example: """
                    use jsonpath_rust::JsonPath;

                    pub fn assert_json_path(json: &Value, path: &str, expected: &Value) -> AssertionResult {
                        let jp = JsonPath::try_from(path).map_err(|e| format!("invalid path: {}", e))?;
                        let actual = jp.find(json);
                        AssertionResult {
                            name: path.to_string(),
                            passed: actual == Some(expected.clone()),
                            expected: serde_json::to_string_pretty(expected).unwrap_or_default(),
                            actual: actual.map(|v| serde_json::to_string_pretty(&v).unwrap_or_default()).unwrap_or("(not found)".to_string()),
                            diff: None,
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
