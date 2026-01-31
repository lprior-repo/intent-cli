package beads

// Import the schema
// Note: In production, use: import "intent-cli.com/schema"

// ============================================================================
// BEAD: types-intendomain - Core domain types foundation
// ============================================================================

bead: #ValidBead & {
    id:              "intent-cli-types01"
    title:           "types: Implement core domain types with Newtype pattern"
    type:            "feature"
    priority:        1
    effort_estimate: "4hr"
    labels:          ["types", "foundation", "m1", "rust-port", "functional"]

    // ========================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ========================================================================
    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use Newtype pattern for all domain types to prevent primitive obsession",
            "THE SYSTEM SHALL implement Clone, Debug, PartialEq on all domain types",
            "THE SYSTEM SHALL validate inputs at construction time, making invalid states unrepresentable",
            "THE SYSTEM SHALL use strongly-typed enums for HttpMethod (GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS)",
        ]

        event_driven: [
            {
                trigger: "WHEN constructing an IntentSpec with empty name"
                shall:   "THE SYSTEM SHALL return Err with validation error describing the constraint violation"
            },
            {
                trigger: "WHEN parsing an HTTP method string"
                shall:   "THE SYSTEM SHALL return the corresponding HttpMethod enum variant or parsing error"
            },
            {
                trigger: "WHEN constructing an IntentTest with invalid URL"
                shall:   "THE SYSTEM SHALL return Err with URL validation error"
            },
            {
                trigger: "WHEN converting between domain types and primitives"
                shall:   "THE SYSTEM SHALL use From/TryFrom traits for type-safe conversions"
            },
        ]

        state_driven: [
            {
                state: "WHILE an IntentSpec is constructed"
                shall: "THE SYSTEM SHALL guarantee all fields satisfy their invariants"
            },
        ]

        unwanted: [
            {
                condition: "IF a domain type uses raw String where a Newtype should exist"
                shall_not: "THE SYSTEM SHALL NOT compile (enforce via type system)"
                because:   "Raw strings enable primitive obsession and lose domain semantics"
            },
            {
                condition: "IF a constructor uses .unwrap() or .expect()"
                shall_not: "THE SYSTEM SHALL NOT use panic-prone methods in constructors"
                because:   "Type construction must be fallible and return Result"
            },
            {
                condition: "IF invalid data can be stored in a domain type"
                shall_not: "THE SYSTEM SHALL NOT allow invalid states to be representable"
                because:   "Invalid states lead to runtime errors instead of compile-time guarantees"
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
                    field:           "spec_name"
                    type:            "String"
                    constraints:     "Non-empty, alphanumeric with underscores/hyphens, max 128 chars"
                    example_valid:   "user_api_spec"
                    example_invalid: ""
                },
                {
                    field:           "http_method"
                    type:            "String"
                    constraints:     "One of: GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS (case-insensitive)"
                    example_valid:   "POST"
                    example_invalid: "FETCH"
                },
                {
                    field:           "url"
                    type:            "String"
                    constraints:     "Valid URL with scheme (http/https), host, optional path/query"
                    example_valid:   "https://api.example.com/users"
                    example_invalid: "not-a-url"
                },
            ]
            system_state: [
                "Rust project compiles with clippy::unwrap_used = forbid",
                "All derive macros available (Clone, Debug, PartialEq, Eq, Hash)",
            ]
        }

        postconditions: {
            state_changes: []
            return_guarantees: [
                {
                    field:     "IntentSpec::name()"
                    guarantee: "Returns non-empty SpecName with valid characters"
                },
                {
                    field:     "HttpMethod::as_str()"
                    guarantee: "Returns uppercase HTTP method string"
                },
                {
                    field:     "IntentTest::url()"
                    guarantee: "Returns validated Url type"
                },
            ]
            side_effects: []
        }

        invariants: [
            "All Newtype wrappers have private inner fields with accessor methods",
            "All constructors return Result<T, IntentError> for fallible creation",
            "All types are Send + Sync for thread safety",
            "All enums are exhaustive with no #[non_exhaustive] unless intentional",
            "Equality is structural for all domain types",
        ]
    }

    // ========================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ========================================================================
    inversions: {
        security_failures: [
            {
                failure:     "URL contains credentials in userinfo component"
                prevention:  "Validate and reject URLs with embedded credentials"
                test_for_it: "test_url_rejects_embedded_credentials"
            },
        ]

        usability_failures: [
            {
                failure:     "Error message doesn't explain what valid input looks like"
                prevention:  "Include constraint description and example in validation errors"
                test_for_it: "test_validation_error_includes_constraint"
            },
            {
                failure:     "HttpMethod parsing is case-sensitive (user types 'get' and fails)"
                prevention:  "Normalize case before parsing HTTP methods"
                test_for_it: "test_http_method_case_insensitive"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Newtype inner value is accessible and can be mutated"
                prevention:  "Keep inner fields private, expose only immutable accessors"
                test_for_it: "test_newtype_encapsulation"
            },
            {
                failure:     "Clone produces shallow copy with shared mutable state"
                prevention:  "Derive Clone, ensure all inner types are Clone without Rc/RefCell"
                test_for_it: "test_clone_produces_independent_copy"
            },
        ]

        integration_failures: [
            {
                failure:     "Type cannot be serialized to JSON for API communication"
                prevention:  "Derive Serialize/Deserialize with serde"
                test_for_it: "test_types_serialize_to_json"
            },
            {
                failure:     "HttpMethod doesn't match reqwest/hyper method enum"
                prevention:  "Implement From<HttpMethod> for http::Method"
                test_for_it: "test_http_method_to_http_crate_method"
            },
        ]
    }

    // ========================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ========================================================================
    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_intent_spec_construction"
                given: "A valid spec name 'user_api' and version '1.0.0'"
                when:  "IntentSpec::new is called with these values"
                then: [
                    "Result is Ok with IntentSpec",
                    "spec.name() returns SpecName wrapping 'user_api'",
                    "spec.version() returns Version wrapping '1.0.0'",
                ]
                real_input: """
                    IntentSpec::new("user_api", "1.0.0")
                    """
                expected_output: """
                    Ok(IntentSpec { name: SpecName("user_api"), version: Version("1.0.0") })
                    """
            },
            {
                name:  "test_http_method_parsing"
                given: "A string 'post' (lowercase)"
                when:  "HttpMethod::try_from is called"
                then: [
                    "Result is Ok with HttpMethod::Post",
                    "method.as_str() returns 'POST'",
                ]
                real_input: """
                    HttpMethod::try_from("post")
                    """
                expected_output: """
                    Ok(HttpMethod::Post)
                    """
            },
            {
                name:  "test_newtype_validation"
                given: "A valid URL string 'https://api.example.com/v1/users'"
                when:  "Url::parse is called"
                then: [
                    "Result is Ok with Url Newtype",
                    "url.as_str() returns the original URL",
                    "url.host() returns 'api.example.com'",
                ]
                real_input: """
                    Url::parse("https://api.example.com/v1/users")
                    """
                expected_output: """
                    Ok(Url("https://api.example.com/v1/users"))
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_spec_name_empty_rejected"
                given: "An empty string ''"
                when:  "SpecName::new is called"
                then: [
                    "Result is Err with validation error",
                    "Error message mentions 'spec name' and 'empty'",
                ]
                real_input: """
                    SpecName::new("")
                    """
                expected_output: null
                expected_error:  "matches!(result, Err(IntentError::Validation { .. }))"
            },
            {
                name:  "test_http_method_invalid_rejected"
                given: "An invalid method string 'FETCH'"
                when:  "HttpMethod::try_from is called"
                then: [
                    "Result is Err with parse error",
                    "Error message lists valid methods",
                ]
                real_input: """
                    HttpMethod::try_from("FETCH")
                    """
                expected_output: null
                expected_error:  "matches!(result, Err(IntentError::Parse { .. }))"
            },
            {
                name:  "test_url_invalid_rejected"
                given: "An invalid URL 'not-a-valid-url'"
                when:  "Url::parse is called"
                then: [
                    "Result is Err with validation error",
                    "Error describes URL format requirements",
                ]
                real_input: """
                    Url::parse("not-a-valid-url")
                    """
                expected_output: null
                expected_error:  "matches!(result, Err(IntentError::Validation { .. }))"
            },
        ]

        edge_cases: [
            {
                name:     "test_spec_name_max_length"
                scenario: "Spec name at exactly 128 characters"
                input:    "SpecName::new(\"a\".repeat(128))"
                expected: "Ok - exactly at limit"
            },
            {
                name:     "test_spec_name_over_max_length"
                scenario: "Spec name at 129 characters"
                input:    "SpecName::new(\"a\".repeat(129))"
                expected: "Err - exceeds 128 character limit"
            },
            {
                name:     "test_http_method_all_variants"
                scenario: "All seven HTTP methods parse correctly"
                input:    "[\"GET\", \"POST\", \"PUT\", \"DELETE\", \"PATCH\", \"HEAD\", \"OPTIONS\"]"
                expected: "All parse to corresponding enum variants"
            },
            {
                name:     "test_url_with_query_params"
                scenario: "URL with complex query parameters"
                input:    "Url::parse(\"https://api.example.com?foo=bar&baz=qux\")"
                expected: "Ok - query params preserved"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_newtypes_are_send_sync"
                verifies: "All domain types implement Send + Sync"
                test:     "fn assert_send_sync<T: Send + Sync>() {} assert_send_sync::<IntentSpec>()"
            },
            {
                name:     "test_postcondition_spec_name_never_empty"
                verifies: "SpecName always contains non-empty string"
                test:     "For all valid SpecName instances, assert !name.as_str().is_empty()"
            },
            {
                name:     "test_invariant_clone_eq"
                verifies: "Clone produces equal value"
                test:     "For any T: Clone + Eq, assert t.clone() == t"
            },
        ]
    }

    // ========================================================================
    // SECTION 5: E2E TESTS
    // ========================================================================
    e2e_tests: {
        pipeline_test: {
            name:        "test_full_types_pipeline"
            description: "Test type creation, validation, and conversion through complete spec lifecycle"

            setup: {
                files_to_create: [
                    {
                        path: "/tmp/test-intent-types/spec.cue"
                        content: """
                            spec: {
                                name: "user_api"
                                version: "1.0.0"
                                tests: [{
                                    method: "POST"
                                    url: "https://api.example.com/users"
                                }]
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "mkdir -p /tmp/test-intent-types",
                ]
            }

            execute: {
                command:    "cd /home/lewis/src/intent-cli && cargo test types:: --no-fail-fast"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test_intent_spec_construction",
                    "test_http_method_parsing",
                    "test_newtype_validation",
                    "passed",
                ]
            }

            cleanup: {
                files_to_delete: ["/tmp/test-intent-types"]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_spec_from_cue_to_domain"
                description: "Parse CUE spec and convert to domain types"
                steps: [
                    {
                        action: "Parse CUE file into raw JSON"
                        verify: "JSON contains name, version, tests fields"
                    },
                    {
                        action: "Convert JSON to IntentSpec domain type"
                        verify: "IntentSpec::try_from(json) succeeds with valid data"
                    },
                    {
                        action: "Access nested IntentTest from spec"
                        verify: "spec.tests()[0].method() returns HttpMethod::Post"
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
                task:      "Write test: test_intent_spec_construction"
                file:      "crates/intent-core/src/types.rs"
                what:      "#[test] fn test_intent_spec_construction() { ... }"
                done_when: "Test exists and FAILS (no IntentSpec yet)"
            },
            {
                task:      "Write test: test_http_method_parsing"
                file:      "crates/intent-core/src/types.rs"
                what:      "#[test] fn test_http_method_parsing() { ... }"
                done_when: "Test exists and FAILS (no HttpMethod yet)"
            },
            {
                task:      "Write test: test_newtype_validation"
                file:      "crates/intent-core/src/types.rs"
                what:      "#[test] fn test_newtype_validation() { ... }"
                done_when: "Test exists and FAILS (no SpecName yet)"
            },
            {
                task:      "Write test: test_spec_name_empty_rejected"
                file:      "crates/intent-core/src/types.rs"
                what:      "#[test] fn test_spec_name_empty_rejected() { ... }"
                done_when: "Test exists and FAILS"
            },
        ]

        phase_2_implementation: [
            {
                task:      "Implement HttpMethod enum"
                file:      "crates/intent-core/src/types.rs"
                what:      "Define enum with all seven HTTP method variants"
                done_when: "test_http_method_parsing PASSES"
                patterns_to_use: [
                    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]",
                    "impl TryFrom<&str> for HttpMethod",
                    "impl Display for HttpMethod",
                ]
            },
            {
                task:      "Implement SpecName Newtype"
                file:      "crates/intent-core/src/types.rs"
                what:      "Newtype wrapper for validated spec names"
                done_when: "test_newtype_validation and test_spec_name_empty_rejected PASS"
                patterns_to_use: [
                    "pub struct SpecName(String); // private inner",
                    "impl SpecName { pub fn new(s: impl Into<String>) -> Result<Self, IntentError> }",
                    "pub fn as_str(&self) -> &str { &self.0 }",
                ]
            },
            {
                task:      "Implement IntentSpec domain type"
                file:      "crates/intent-core/src/types.rs"
                what:      "Main spec type with validated construction"
                done_when: "test_intent_spec_construction PASSES"
                patterns_to_use: [
                    "#[derive(Debug, Clone, PartialEq)]",
                    "Builder pattern with validation",
                    "impl IntentSpec { pub fn new(...) -> Result<Self, IntentError> }",
                ]
            },
            {
                task:      "Implement IntentTest domain type"
                file:      "crates/intent-core/src/types.rs"
                what:      "Test case type with HttpMethod and Url"
                done_when: "All phase_1 tests PASS"
                patterns_to_use: [
                    "Compose from HttpMethod and url::Url",
                    "Validate URL at construction",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export from lib.rs and prelude"
                file:      "crates/intent-core/src/lib.rs"
                what:      "Add pub mod types; and re-export domain types"
                done_when: "use intent_core::types::{IntentSpec, HttpMethod} works"
            },
            {
                task:      "Add serde derives for JSON serialization"
                file:      "crates/intent-core/src/types.rs"
                what:      "Add #[derive(Serialize, Deserialize)] with custom impls where needed"
                done_when: "serde_json::to_string(&spec) compiles"
            },
        ]

        phase_4_verification: [
            {
                task:      "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
            },
            {
                task:      "Verify no unwrap in types.rs"
                commands:  ["grep -n 'unwrap()' crates/intent-core/src/types.rs"]
                expected:  "No matches found"
                done_when: "grep returns no matches"
            },
            {
                task:      "Verify no primitive obsession"
                commands:  ["grep -n 'pub.*String' crates/intent-core/src/types.rs"]
                expected:  "Only Newtype inner fields, no public String fields"
                done_when: "All String fields are wrapped in Newtypes"
            },
        ]
    }

    // ========================================================================
    // SECTION 7: FAILURE MODES
    // ========================================================================
    failure_modes: {
        failure_modes: [
            {
                symptom:      "Type construction panics instead of returning Result"
                likely_cause: "Used unwrap() or expect() in constructor"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/types.rs"
                        what_to_check: "Search for .unwrap() or .expect() in new() methods"
                    },
                ]
                fix_pattern: "Replace with .map_err()? and return Result<Self, IntentError>"
            },
            {
                symptom:      "Test fails: type does not implement trait Clone"
                likely_cause: "Missing #[derive(Clone)] on type or inner field not Clone"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/types.rs"
                        function:      "struct definitions"
                        what_to_check: "Verify all structs have #[derive(Clone)]"
                    },
                ]
                fix_pattern: "Add Clone to derive list, ensure all fields implement Clone"
            },
            {
                symptom:      "HttpMethod parsing fails for lowercase input"
                likely_cause: "Case-sensitive comparison in TryFrom implementation"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/types.rs"
                        function:      "impl TryFrom<&str> for HttpMethod"
                        what_to_check: "Check if to_uppercase() is called before matching"
                    },
                ]
                fix_pattern: "Call s.to_uppercase() before matching against variants"
            },
            {
                symptom:      "SpecName validation too strict (valid names rejected)"
                likely_cause: "Regex/validation too restrictive"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/types.rs"
                        function:      "SpecName::new"
                        what_to_check: "Verify allowed character set includes underscores, hyphens"
                    },
                ]
                fix_pattern: "Use regex ^[a-zA-Z][a-zA-Z0-9_-]{0,127}$"
            },
        ]

        debugging_commands: [
            {
                scenario: "When type construction fails unexpectedly"
                run:      "cargo test types::tests::test_intent_spec -- --nocapture"
                look_for: "Actual error message vs expected"
            },
            {
                scenario: "When derive macro fails"
                run:      "cargo expand --lib --package intent-core types"
                look_for: "Expanded macro code for type definitions"
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
            "[ ] E2E pipeline test passing with real spec data",
            "[ ] No mocks or fake data in any test",
            "[ ] Property test: all types roundtrip through serde",
        ]

        code: [
            "[ ] All types use Newtype pattern (no raw String/i32 for domain concepts)",
            "[ ] Zero unwrap() or expect() calls in any constructor",
            "[ ] All preconditions validated at construction time",
            "[ ] All types implement Clone, Debug, PartialEq",
            "[ ] HttpMethod has From/TryFrom implementations",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] cargo doc builds without warnings",
        ]

        documentation: [
            "[ ] Module-level doc comment explaining Newtype pattern usage",
            "[ ] Each type documented with construction examples",
        ]
    }

    // ========================================================================
    // SECTION 9: CONTEXT
    // ========================================================================
    context: {
        related_files: [
            {
                path:      "crates/intent-core/src/error.rs"
                relevance: "IntentError used for validation failures in type construction"
            },
            {
                path:      "crates/intent-core/src/lib.rs"
                relevance: "Exports types module"
            },
            {
                path:      "crates/cue-parser/src/lib.rs"
                relevance: "Parser that creates domain types from CUE specs"
            },
        ]

        similar_implementations: [
            "Newtype pattern in Rust: https://doc.rust-lang.org/rust-by-example/generics/new_types.html",
            "Domain-driven design value objects: typed wrappers for primitives",
        ]

        external_references: [
            "https://doc.rust-lang.org/std/convert/trait.TryFrom.html - TryFrom trait",
            "https://serde.rs/derive.html - serde derive macros",
            "https://docs.rs/url/latest/url/ - url crate for URL parsing",
        ]

        codebase_patterns: [
            {
                pattern:          "Newtype with validation"
                example_location: "crates/intent-core/src/types.rs"
                how_to_apply:     "Private inner field, new() returns Result, as_str() accessor"
            },
            {
                pattern:          "TryFrom for string parsing"
                example_location: "crates/intent-core/src/types.rs"
                how_to_apply:     "impl TryFrom<&str> with IntentError::Parse on failure"
            },
        ]
    }

    // ========================================================================
    // SECTION 10: AI HINTS
    // ========================================================================
    ai_hints: {
        do: [
            "Use #[derive(Debug, Clone, PartialEq, Eq, Hash)] on all types where applicable",
            "Implement From<T> for lossless conversions, TryFrom<T> for fallible ones",
            "Keep Newtype inner fields private with pub fn as_str(&self) accessors",
            "Use url::Url crate for URL validation, don't roll your own",
            "Add #[must_use] on all constructor methods",
            "Implement Display for user-facing output, Debug for developer output",
        ]

        do_not: [
            "Do NOT use unwrap() or expect() in constructors - return Result",
            "Do NOT use panic!, todo!, or unimplemented! in type implementations",
            "Do NOT expose inner Newtype field as pub - use accessor methods",
            "Do NOT use raw String where a domain type (SpecName, etc.) should exist",
            "Do NOT implement Copy on types with String fields",
            "Do NOT use regex for simple validations - prefer str methods",
        ]

        code_patterns: [
            {
                name:     "Newtype with validation"
                use_when: "Wrapping a primitive with domain constraints"
                example: """
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct SpecName(String);

                    impl SpecName {
                        #[must_use]
                        pub fn new(s: impl Into<String>) -> Result<Self, IntentError> {
                            let s = s.into();
                            if s.is_empty() {
                                return Err(IntentError::validation("spec_name", "cannot be empty"));
                            }
                            if s.len() > 128 {
                                return Err(IntentError::validation("spec_name", "exceeds 128 characters"));
                            }
                            Ok(Self(s))
                        }

                        pub fn as_str(&self) -> &str {
                            &self.0
                        }
                    }
                    """
            },
            {
                name:     "Enum with TryFrom"
                use_when: "Parsing string into enum variant"
                example: """
                    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
                    pub enum HttpMethod {
                        Get, Post, Put, Delete, Patch, Head, Options,
                    }

                    impl TryFrom<&str> for HttpMethod {
                        type Error = IntentError;

                        fn try_from(s: &str) -> Result<Self, Self::Error> {
                            match s.to_uppercase().as_str() {
                                "GET" => Ok(Self::Get),
                                "POST" => Ok(Self::Post),
                                // ... other variants
                                other => Err(IntentError::parse("http_method", 0, 0,
                                    format!("unknown method '{}', expected GET|POST|PUT|DELETE|PATCH|HEAD|OPTIONS", other)))
                            }
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
