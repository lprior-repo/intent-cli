package beads

// Import the schema
// Note: In production, use: import "intent-cli.com/schema"

// ============================================================================
// BEAD: http-client - HTTP client with request/response handling
// ============================================================================

bead: #ValidBead & {
    id:              "intent-cli-http01"
    title:           "http: Implement HTTP client with request/response handling"
    type:            "feature"
    priority:        2
    effort_estimate: "4hr"
    labels:          ["http", "client", "m3", "rust-port"]

    // ========================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ========================================================================
    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL build HTTP requests from CUE spec definitions",
            "THE SYSTEM SHALL execute HTTP requests and capture complete responses",
            "THE SYSTEM SHALL record request/response timing with millisecond precision",
            "THE SYSTEM SHALL support all standard HTTP methods (GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS)",
            "THE SYSTEM SHALL handle request bodies with proper Content-Type encoding",
        ]

        event_driven: [
            {
                trigger: "WHEN a valid HTTP spec is provided"
                shall:   "THE SYSTEM SHALL build and execute the corresponding HTTP request"
            },
            {
                trigger: "WHEN an HTTP response is received"
                shall:   "THE SYSTEM SHALL capture status code, headers, body, and elapsed time"
            },
            {
                trigger: "WHEN the request includes a JSON body"
                shall:   "THE SYSTEM SHALL serialize the body and set Content-Type: application/json"
            },
            {
                trigger: "WHEN the request includes form data"
                shall:   "THE SYSTEM SHALL encode as application/x-www-form-urlencoded"
            },
            {
                trigger: "WHEN custom headers are specified"
                shall:   "THE SYSTEM SHALL include all headers in the outgoing request"
            },
        ]

        state_driven: [
            {
                state: "WHILE executing an HTTP request"
                shall: "THE SYSTEM SHALL track elapsed time from send to first byte received"
            },
            {
                state: "WHILE following redirects"
                shall: "THE SYSTEM SHALL limit redirect chain to 10 hops maximum"
            },
        ]

        unwanted: [
            {
                condition: "IF a request timeout is not configured"
                shall_not: "THE SYSTEM SHALL NOT wait indefinitely for a response"
                because:   "Hanging requests block test execution and waste resources"
            },
            {
                condition: "IF a redirect loop is detected"
                shall_not: "THE SYSTEM SHALL NOT follow infinite redirect chains"
                because:   "Redirect loops cause the client to hang indefinitely"
            },
            {
                condition: "IF TLS certificate validation fails"
                shall_not: "THE SYSTEM SHALL NOT silently accept invalid certificates"
                because:   "Silent certificate bypass compromises security testing accuracy"
            },
            {
                condition: "IF request body encoding fails"
                shall_not: "THE SYSTEM SHALL NOT send malformed request bodies"
                because:   "Malformed bodies produce misleading test failures"
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
                    field:           "method"
                    type:            "HttpMethod"
                    constraints:     "Valid HTTP method (GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS)"
                    example_valid:   "POST"
                    example_invalid: "FETCH"
                },
                {
                    field:           "url"
                    type:            "String"
                    constraints:     "Valid URL with scheme (http:// or https://)"
                    example_valid:   "https://api.example.com/users"
                    example_invalid: "not-a-url"
                },
                {
                    field:           "headers"
                    type:            "HashMap<String, String>"
                    constraints:     "Valid header names and values (no control characters)"
                    example_valid:   "{\"Authorization\": \"Bearer token123\"}"
                    example_invalid: "{\"Invalid\\nHeader\": \"value\"}"
                },
                {
                    field:           "body"
                    type:            "Option<RequestBody>"
                    constraints:     "JSON, form-encoded, or raw bytes"
                    example_valid:   "{\"name\": \"test\"}"
                    example_invalid: "N/A"
                },
                {
                    field:           "timeout_ms"
                    type:            "u64"
                    constraints:     "Positive integer, typically 1000-30000"
                    example_valid:   5000
                    example_invalid: 0
                },
            ]
            system_state: [
                "Network connectivity available (or mock server running for tests)",
                "DNS resolution functional",
            ]
        }

        postconditions: {
            state_changes: []
            return_guarantees: [
                {
                    field:     "HttpResponse::status"
                    guarantee: "Returns HTTP status code (100-599)"
                },
                {
                    field:     "HttpResponse::headers"
                    guarantee: "Returns all response headers as key-value pairs"
                },
                {
                    field:     "HttpResponse::body"
                    guarantee: "Returns response body as bytes (may be empty)"
                },
                {
                    field:     "HttpResponse::elapsed_ms"
                    guarantee: "Returns request duration in milliseconds (>= 0)"
                },
                {
                    field:     "HttpResponse::url"
                    guarantee: "Returns final URL after any redirects"
                },
            ]
            side_effects: [
                "HTTP request sent to specified URL",
                "Network bandwidth consumed",
            ]
        }

        invariants: [
            "All requests have a configured timeout (never infinite)",
            "Response timing is always captured, even on error",
            "Original request details are preserved for debugging",
            "Headers are case-insensitive for lookup but preserved for output",
            "Body encoding matches Content-Type header",
        ]
    }

    // ========================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ========================================================================
    inversions: {
        security_failures: [
            {
                failure:     "Sensitive headers (Authorization, Cookie) logged in debug output"
                prevention:  "Redact sensitive headers in Debug impl and error messages"
                test_for_it: "test_sensitive_headers_redacted"
            },
            {
                failure:     "TLS errors silently ignored, allowing MITM attacks"
                prevention:  "Return IntentError::Tls on certificate validation failure"
                test_for_it: "test_tls_error_not_ignored"
            },
            {
                failure:     "Request body contains secrets that appear in logs"
                prevention:  "Never log full request bodies; use truncated previews"
                test_for_it: "test_body_not_fully_logged"
            },
        ]

        usability_failures: [
            {
                failure:     "Timeout error doesn't indicate configured timeout value"
                prevention:  "Include timeout_ms in IntentError::Timeout context"
                test_for_it: "test_timeout_error_includes_duration"
            },
            {
                failure:     "Connection refused error doesn't show target URL"
                prevention:  "Include URL in all network error messages"
                test_for_it: "test_connection_error_includes_url"
            },
            {
                failure:     "User cannot determine if redirect occurred"
                prevention:  "Include original and final URL in response"
                test_for_it: "test_redirect_urls_captured"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Response body truncated without indication"
                prevention:  "Always capture full body; warn if unusually large"
                test_for_it: "test_large_body_not_truncated"
            },
            {
                failure:     "Response headers lost or corrupted"
                prevention:  "Store headers immediately after response received"
                test_for_it: "test_all_headers_captured"
            },
            {
                failure:     "Timing measurement includes processing overhead"
                prevention:  "Capture timing at request/response boundaries only"
                test_for_it: "test_timing_accuracy"
            },
        ]

        integration_failures: [
            {
                failure:     "HTTP client config conflicts with test requirements"
                prevention:  "Allow per-request configuration override"
                test_for_it: "test_per_request_config"
            },
            {
                failure:     "Connection pooling causes state bleed between tests"
                prevention:  "Use fresh client per test suite or disable pooling"
                test_for_it: "test_no_connection_reuse_bleed"
            },
        ]
    }

    // ========================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ========================================================================
    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_build_get_request"
                given: "A spec with method GET and URL https://httpbin.org/get"
                when:  "HttpRequest::from_spec() is called"
                then: [
                    "Request method is GET",
                    "Request URL is https://httpbin.org/get",
                    "No request body is set",
                ]
                real_input: """
                    HttpSpec {
                        method: "GET",
                        url: "https://httpbin.org/get",
                        headers: {},
                        body: None,
                    }
                    """
                expected_output: """
                    HttpRequest {
                        method: Method::GET,
                        url: Url::parse("https://httpbin.org/get"),
                        headers: HeaderMap::new(),
                        body: None,
                    }
                    """
            },
            {
                name:  "test_build_post_with_body"
                given: "A spec with method POST, URL, and JSON body"
                when:  "HttpRequest::from_spec() is called"
                then: [
                    "Request method is POST",
                    "Request body contains JSON payload",
                    "Content-Type header is application/json",
                ]
                real_input: """
                    HttpSpec {
                        method: "POST",
                        url: "https://httpbin.org/post",
                        headers: {},
                        body: Some(json!({"name": "test", "value": 42})),
                    }
                    """
                expected_output: """
                    HttpRequest {
                        method: Method::POST,
                        url: Url::parse("https://httpbin.org/post"),
                        headers: {"Content-Type": "application/json"},
                        body: Some(b"{\"name\":\"test\",\"value\":42}"),
                    }
                    """
            },
            {
                name:  "test_response_capture"
                given: "An executed HTTP request to a valid endpoint"
                when:  "Response is received"
                then: [
                    "Status code is captured (e.g., 200)",
                    "Response headers are captured",
                    "Response body is captured",
                    "Elapsed time is recorded in milliseconds",
                ]
                real_input: """
                    client.execute(HttpRequest::get("https://httpbin.org/get")).await
                    """
                expected_output: """
                    HttpResponse {
                        status: 200,
                        headers: {"content-type": "application/json", ...},
                        body: b"{\"args\": {}, ...}",
                        elapsed_ms: 150, // approximate
                        url: "https://httpbin.org/get",
                    }
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_connection_refused"
                given: "A request to a non-existent server"
                when:  "Client attempts to connect"
                then: [
                    "IntentError::Network is returned",
                    "Error message contains 'connection refused'",
                    "Error includes the target URL",
                ]
                real_input: """
                    client.execute(HttpRequest::get("http://localhost:59999/nonexistent")).await
                    """
                expected_output: null
                expected_error:  "matches!(err, IntentError::Network { .. })"
            },
            {
                name:  "test_timeout_exceeded"
                given: "A request to a slow endpoint with 100ms timeout"
                when:  "Endpoint takes longer than timeout"
                then: [
                    "IntentError::Timeout is returned",
                    "Error includes configured timeout value",
                ]
                real_input: """
                    let client = HttpClient::new(HttpClientConfig { timeout_ms: 100 });
                    client.execute(HttpRequest::get("https://httpbin.org/delay/5")).await
                    """
                expected_output: null
                expected_error:  "matches!(err, IntentError::Timeout { .. })"
            },
            {
                name:  "test_invalid_url"
                given: "A spec with an invalid URL"
                when:  "HttpRequest::from_spec() is called"
                then: [
                    "IntentError::Validation is returned",
                    "Error message indicates invalid URL format",
                ]
                real_input: """
                    HttpSpec {
                        method: "GET",
                        url: "not-a-valid-url",
                        headers: {},
                        body: None,
                    }
                    """
                expected_output: null
                expected_error:  "matches!(err, IntentError::Validation { field: \"url\", .. })"
            },
        ]

        edge_cases: [
            {
                name:     "test_empty_response_body"
                scenario: "Server returns 204 No Content"
                input:    "HttpRequest::delete(\"https://httpbin.org/status/204\")"
                expected: "Response body is empty, status is 204"
            },
            {
                name:     "test_binary_response_body"
                scenario: "Server returns binary data (image)"
                input:    "HttpRequest::get(\"https://httpbin.org/bytes/1024\")"
                expected: "Response body contains exactly 1024 bytes"
            },
            {
                name:     "test_unicode_headers"
                scenario: "Response contains UTF-8 in header values"
                input:    "Request to endpoint with UTF-8 headers"
                expected: "Headers preserved without corruption"
            },
            {
                name:     "test_redirect_chain"
                scenario: "Server returns 302 redirect"
                input:    "HttpRequest::get(\"https://httpbin.org/redirect/3\")"
                expected: "Final URL differs from original, redirect followed"
            },
        ]

        contract_tests: [
            {
                name:     "test_precondition_valid_method"
                verifies: "Only valid HTTP methods accepted"
                test:     "HttpRequest::from_spec with invalid method returns Validation error"
            },
            {
                name:     "test_precondition_valid_url"
                verifies: "URL must have valid scheme"
                test:     "HttpRequest::from_spec with ftp:// URL returns Validation error"
            },
            {
                name:     "test_postcondition_timing_nonnegative"
                verifies: "elapsed_ms is always >= 0"
                test:     "For any response, assert response.elapsed_ms >= 0"
            },
            {
                name:     "test_invariant_timeout_configured"
                verifies: "All requests have timeout"
                test:     "HttpClient::new() without config uses default timeout"
            },
        ]
    }

    // ========================================================================
    // SECTION 5: E2E TESTS
    // ========================================================================
    e2e_tests: {
        pipeline_test: {
            name:        "test_full_http_pipeline"
            description: "Test complete HTTP flow: spec parsing, request building, execution, response capture"

            setup: {
                files_to_create: [
                    {
                        path: "/tmp/test-intent-http/specs/api-test.cue"
                        content: """
                            package specs

                            test_get_users: {
                                request: {
                                    method: "GET"
                                    url: "http://localhost:8080/users"
                                    headers: {
                                        "Accept": "application/json"
                                    }
                                }
                                expect: {
                                    status: 200
                                    body: {
                                        users: [...]
                                    }
                                }
                            }
                            """
                    },
                ]
                environment: [
                    "INTENT_TEST_MOCK_SERVER=true",
                ]
                precondition_commands: [
                    "mkdir -p /tmp/test-intent-http/specs",
                ]
            }

            execute: {
                command:    "cd /home/lewis/src/intent-cli && cargo test http:: --no-fail-fast"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test_build_get_request",
                    "test_build_post_with_body",
                    "test_response_capture",
                    "passed",
                ]
            }

            cleanup: {
                files_to_delete: ["/tmp/test-intent-http"]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_mock_server_integration"
                description: "Verify HTTP client works with mock server for isolated testing"
                steps: [
                    {
                        action: "Start mock server on localhost:8080"
                        verify: "Server responds to health check"
                    },
                    {
                        action: "Execute GET request to mock endpoint"
                        verify: "Response matches mock configuration"
                    },
                    {
                        action: "Execute POST with JSON body"
                        verify: "Mock server receives correct body"
                    },
                    {
                        action: "Verify timing capture"
                        verify: "elapsed_ms is reasonable (< 100ms for localhost)"
                    },
                ]
            },
            {
                name:        "e2e_error_handling_pipeline"
                description: "Verify error cases produce correct IntentError types"
                steps: [
                    {
                        action: "Request to non-existent server"
                        verify: "IntentError::Network returned with URL context"
                    },
                    {
                        action: "Request with very short timeout"
                        verify: "IntentError::Timeout returned with timeout value"
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
                task:      "Write test: test_build_get_request"
                file:      "crates/intent-http/src/client.rs"
                what:      "#[test] fn test_build_get_request() { ... }"
                done_when: "Test exists and FAILS (no HttpRequest yet)"
            },
            {
                task:      "Write test: test_build_post_with_body"
                file:      "crates/intent-http/src/client.rs"
                what:      "#[test] fn test_build_post_with_body() { ... }"
                done_when: "Test exists and FAILS"
            },
            {
                task:      "Write test: test_response_capture"
                file:      "crates/intent-http/src/client.rs"
                what:      "#[tokio::test] async fn test_response_capture() { ... }"
                done_when: "Test exists and FAILS"
            },
            {
                task:      "Write test: test_timeout_error"
                file:      "crates/intent-http/src/client.rs"
                what:      "#[tokio::test] async fn test_timeout_error() { ... }"
                done_when: "Test exists and FAILS"
            },
        ]

        phase_2_implementation: [
            {
                task:      "Implement HttpRequest struct and builder"
                file:      "crates/intent-http/src/request.rs"
                what:      "Define HttpRequest with method, url, headers, body fields"
                done_when: "test_build_get_request and test_build_post_with_body PASS"
                patterns_to_use: [
                    "Builder pattern for request construction",
                    "From<HttpSpec> for HttpRequest conversion",
                    "Validate URL format in constructor",
                ]
            },
            {
                task:      "Implement HttpResponse struct"
                file:      "crates/intent-http/src/response.rs"
                what:      "Define HttpResponse with status, headers, body, elapsed_ms, url"
                done_when: "Response struct compiles with all fields"
                patterns_to_use: [
                    "Capture timing with std::time::Instant",
                    "Store body as Vec<u8> for binary safety",
                ]
            },
            {
                task:      "Implement HttpClient with reqwest"
                file:      "crates/intent-http/src/client.rs"
                what:      "HttpClient::execute() method using reqwest::Client"
                done_when: "test_response_capture PASSES"
                patterns_to_use: [
                    "reqwest::Client with configured timeout",
                    "Capture elapsed time with Instant::elapsed()",
                    "Map reqwest errors to IntentError variants",
                ]
            },
            {
                task:      "Implement error handling for network failures"
                file:      "crates/intent-http/src/client.rs"
                what:      "Map reqwest::Error to IntentError::Network, Timeout, Tls"
                done_when: "test_timeout_error PASSES"
                patterns_to_use: [
                    "Pattern match on reqwest::Error::is_timeout()",
                    "Pattern match on reqwest::Error::is_connect()",
                    "Include URL context in all error variants",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export from lib.rs"
                file:      "crates/intent-http/src/lib.rs"
                what:      "pub mod client; pub mod request; pub mod response;"
                done_when: "use intent_http::{HttpClient, HttpRequest, HttpResponse} works"
            },
            {
                task:      "Add to workspace Cargo.toml"
                file:      "Cargo.toml"
                what:      "Add intent-http to workspace members"
                done_when: "cargo build -p intent-http succeeds"
            },
        ]

        phase_4_verification: [
            {
                task:      "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
            },
            {
                task:      "Verify all HTTP methods tested"
                commands:  ["grep -c 'Method::' crates/intent-http/src/client.rs"]
                expected:  "At least 7 (GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS)"
                done_when: "All methods have test coverage"
            },
            {
                task:      "Verify timeout is always configured"
                commands:  ["grep -n 'timeout' crates/intent-http/src/client.rs"]
                expected:  "Default timeout set in HttpClient::new()"
                done_when: "No way to create client without timeout"
            },
        ]
    }

    // ========================================================================
    // SECTION 7: FAILURE MODES
    // ========================================================================
    failure_modes: {
        failure_modes: [
            {
                symptom:      "Connection refused error"
                likely_cause: "Target server not running or wrong port"
                where_to_look: [
                    {
                        file:          "crates/intent-http/src/client.rs"
                        function:      "HttpClient::execute"
                        what_to_check: "Verify URL and port in request"
                    },
                ]
                fix_pattern: "Check server is running; verify URL scheme and port"
            },
            {
                symptom:      "Timeout error on fast endpoints"
                likely_cause: "Timeout configured too low"
                where_to_look: [
                    {
                        file:          "crates/intent-http/src/client.rs"
                        function:      "HttpClient::new"
                        what_to_check: "Default timeout value (should be >= 5000ms)"
                    },
                ]
                fix_pattern: "Increase timeout_ms in HttpClientConfig"
            },
            {
                symptom:      "TLS handshake error"
                likely_cause: "Server certificate invalid or self-signed"
                where_to_look: [
                    {
                        file:          "crates/intent-http/src/client.rs"
                        function:      "HttpClient::new"
                        what_to_check: "TLS configuration in reqwest::ClientBuilder"
                    },
                ]
                fix_pattern: "For testing, allow insecure; for production, fix certificate"
            },
            {
                symptom:      "Body encoding error with non-ASCII"
                likely_cause: "JSON serialization failing on invalid UTF-8"
                where_to_look: [
                    {
                        file:          "crates/intent-http/src/request.rs"
                        function:      "HttpRequest::from_spec"
                        what_to_check: "Body serialization logic"
                    },
                ]
                fix_pattern: "Use serde_json for JSON; handle binary separately"
            },
            {
                symptom:      "Redirect not followed"
                likely_cause: "reqwest redirect policy disabled"
                where_to_look: [
                    {
                        file:          "crates/intent-http/src/client.rs"
                        function:      "HttpClient::new"
                        what_to_check: "redirect(Policy::limited(10)) in builder"
                    },
                ]
                fix_pattern: "Enable redirect following with reasonable limit"
            },
        ]

        debugging_commands: [
            {
                scenario: "When request building fails"
                run:      "cargo test http::tests::test_build -- --nocapture"
                look_for: "Validation error message with field name"
            },
            {
                scenario: "When network error occurs"
                run:      "RUST_LOG=reqwest=debug cargo test http::tests::test_connection -- --nocapture"
                look_for: "Connection attempt details in debug output"
            },
            {
                scenario: "When timing seems incorrect"
                run:      "cargo test http::tests::test_timing -- --nocapture"
                look_for: "Printed elapsed_ms values"
            },
        ]
    }

    // ========================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ========================================================================
    completion_checklist: {
        tests: [
            "[ ] test_build_get_request passes",
            "[ ] test_build_post_with_body passes",
            "[ ] test_response_capture passes with real HTTP",
            "[ ] test_connection_refused returns correct error",
            "[ ] test_timeout_exceeded returns correct error",
            "[ ] All HTTP methods (GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS) tested",
            "[ ] Mock server tests pass in isolation",
        ]

        code: [
            "[ ] HttpRequest validates URL format",
            "[ ] HttpResponse captures all fields (status, headers, body, timing)",
            "[ ] HttpClient uses reqwest with configured timeout",
            "[ ] All reqwest errors mapped to IntentError variants",
            "[ ] Timeouts are always configured (never infinite)",
            "[ ] Redirects followed up to 10 hops",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] cargo doc builds without warnings",
        ]

        documentation: [
            "[ ] Module-level doc comment with usage example",
            "[ ] HttpClient documented with configuration options",
            "[ ] Error handling documented with all failure cases",
        ]
    }

    // ========================================================================
    // SECTION 9: CONTEXT
    // ========================================================================
    context: {
        related_files: [
            {
                path:      "crates/intent-core/src/types.rs"
                relevance: "Defines HttpSpec that HttpRequest converts from"
            },
            {
                path:      "crates/intent-core/src/error.rs"
                relevance: "IntentError variants for HTTP failures"
            },
            {
                path:      "crates/intent-runner/src/runner.rs"
                relevance: "Test runner that uses HttpClient to execute specs"
            },
            {
                path:      "Cargo.toml"
                relevance: "Workspace configuration and reqwest dependency"
            },
        ]

        similar_implementations: [
            "reqwest crate - Underlying HTTP client library",
            "ureq crate - Simpler blocking HTTP client (not used but similar API)",
        ]

        external_references: [
            "https://docs.rs/reqwest - reqwest documentation",
            "https://httpbin.org - HTTP testing service for integration tests",
            "https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods - HTTP methods reference",
        ]

        codebase_patterns: [
            {
                pattern:          "Type conversion from spec to runtime"
                example_location: "crates/intent-core/src/types.rs"
                how_to_apply:     "Implement From<HttpSpec> for HttpRequest"
            },
            {
                pattern:          "Error mapping with context"
                example_location: "crates/intent-core/src/error.rs"
                how_to_apply:     "Map reqwest::Error to IntentError with URL context"
            },
        ]
    }

    // ========================================================================
    // SECTION 10: AI HINTS
    // ========================================================================
    ai_hints: {
        do: [
            "Use reqwest crate for HTTP client implementation",
            "Capture timing with std::time::Instant before and after request",
            "Map all reqwest::Error cases to appropriate IntentError variants",
            "Include URL context in all error messages",
            "Use tokio runtime for async HTTP operations",
            "Configure default timeout (5-10 seconds) that can be overridden",
            "Store response body as Vec<u8> for binary safety",
            "Validate URL has http:// or https:// scheme",
        ]

        do_not: [
            "Do NOT use blocking HTTP calls in async context",
            "Do NOT ignore timeout configuration (always set a default)",
            "Do NOT silently swallow TLS errors",
            "Do NOT log full request/response bodies (may contain secrets)",
            "Do NOT use .unwrap() on network operations",
            "Do NOT hardcode URLs or ports in implementation",
        ]

        code_patterns: [
            {
                name:     "HTTP client with timeout"
                use_when: "Creating the reqwest client"
                example: """
                    let client = reqwest::Client::builder()
                        .timeout(Duration::from_millis(config.timeout_ms))
                        .redirect(reqwest::redirect::Policy::limited(10))
                        .build()
                        .map_err(|e| IntentError::internal(e.to_string()))?;
                    """
            },
            {
                name:     "Response capture with timing"
                use_when: "Executing a request and building response"
                example: """
                    let start = Instant::now();
                    let resp = client.execute(request).await
                        .map_err(|e| map_reqwest_error(e, &url))?;
                    let elapsed_ms = start.elapsed().as_millis() as u64;

                    Ok(HttpResponse {
                        status: resp.status().as_u16(),
                        headers: convert_headers(resp.headers()),
                        body: resp.bytes().await?.to_vec(),
                        elapsed_ms,
                        url: resp.url().to_string(),
                    })
                    """
            },
            {
                name:     "Error mapping from reqwest"
                use_when: "Converting reqwest errors to IntentError"
                example: """
                    fn map_reqwest_error(err: reqwest::Error, url: &str) -> IntentError {
                        if err.is_timeout() {
                            IntentError::timeout(url, "request timed out")
                        } else if err.is_connect() {
                            IntentError::network(url, "connection failed")
                        } else if err.is_request() {
                            IntentError::http(url, err.to_string())
                        } else {
                            IntentError::internal(err.to_string())
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
