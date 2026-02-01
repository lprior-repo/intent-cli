package beads

// Import the schema
// Note: In production, use: import "intent-cli.com/schema"

// ============================================================================
// BEAD: config-loader - Configuration system with environment fallbacks
// ============================================================================

bead: #ValidBead & {
    id:              "intent-cli-config01"
    title:           "config: Implement configuration loader with environment fallbacks"
    type:            "feature"
    priority:        1
    effort_estimate: "2hr"
    labels:          ["config", "foundation", "m1", "rust-port"]

    // ========================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ========================================================================
    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL load configuration from file, environment variables, or defaults",
            "THE SYSTEM SHALL use Railway-oriented loading chain (file -> env -> default)",
            "THE SYSTEM SHALL validate all configuration values before returning",
            "THE SYSTEM SHALL return Result<Config, IntentError> from all loading operations",
        ]

        event_driven: [
            {
                trigger: "WHEN loading configuration"
                shall:   "THE SYSTEM SHALL check CLI args first, then env vars, then config file, then defaults"
            },
            {
                trigger: "WHEN a config file is found"
                shall:   "THE SYSTEM SHALL parse TOML and merge values with appropriate precedence"
            },
            {
                trigger: "WHEN an environment variable is set"
                shall:   "THE SYSTEM SHALL override file-based and default values for that key"
            },
            {
                trigger: "WHEN no config source provides a required value"
                shall:   "THE SYSTEM SHALL use the hard-coded default value"
            },
        ]

        state_driven: [
            {
                state: "WHILE INTENT_LOG_LEVEL env var is set"
                shall: "THE SYSTEM SHALL use that log level regardless of config file setting"
            },
            {
                state: "WHILE in CI environment (CI=true)"
                shall: "THE SYSTEM SHALL disable interactive prompts and color output"
            },
        ]

        unwanted: [
            {
                condition: "IF config file is missing"
                shall_not: "THE SYSTEM SHALL NOT fail or error"
                because:   "Missing config file is a valid state; defaults should be used silently"
            },
            {
                condition: "IF env var unexpectedly overrides a file setting"
                shall_not: "THE SYSTEM SHALL NOT silently change behavior without logging at debug level"
                because:   "Users need to understand why config differs from their file"
            },
            {
                condition: "IF config file contains unknown keys"
                shall_not: "THE SYSTEM SHALL NOT silently ignore unknown configuration keys"
                because:   "Unknown keys often indicate typos; warn the user"
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
                    field:           "config_path"
                    type:            "Option<PathBuf>"
                    constraints:     "Optional path to config file; if None, use XDG config dir"
                    example_valid:   "Some(PathBuf::from('~/.config/intent/config.toml'))"
                    example_invalid: "Some(PathBuf::from('/root/secret.toml'))"
                },
                {
                    field:           "env_prefix"
                    type:            "String"
                    constraints:     "Prefix for environment variables (default: 'INTENT_')"
                    example_valid:   "INTENT_"
                    example_invalid: ""
                },
            ]
            system_state: [
                "File system is accessible for reading config file",
                "Environment variables are readable",
            ]
        }

        postconditions: {
            state_changes: []
            return_guarantees: [
                {
                    field:     "Config.log_level"
                    guarantee: "Returns valid LogLevel enum value (trace|debug|info|warn|error)"
                },
                {
                    field:     "Config.output_format"
                    guarantee: "Returns valid OutputFormat enum value (text|json|table)"
                },
                {
                    field:     "Config.color_enabled"
                    guarantee: "Returns bool, false if NO_COLOR env is set or stdout is not a tty"
                },
                {
                    field:     "Config.config_path"
                    guarantee: "Returns Option<PathBuf> of the config file actually loaded"
                },
            ]
            side_effects: [
                "Reads config file from disk if present",
                "Reads environment variables",
            ]
        }

        invariants: [
            "Config loading never panics, always returns Result",
            "Precedence order is always: CLI > env > file > default",
            "All config values are validated before Config struct is returned",
            "Unknown config keys trigger a warning log but don't fail",
            "Config is immutable after construction",
        ]
    }

    // ========================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ========================================================================
    inversions: {
        security_failures: [
            {
                failure:     "Config file path traversal allows reading arbitrary files"
                prevention:  "Canonicalize path and verify it's within allowed directories"
                test_for_it: "test_config_path_traversal_blocked"
            },
            {
                failure:     "Sensitive config values logged at info level"
                prevention:  "Never log config values containing 'key', 'token', 'secret', 'password'"
                test_for_it: "test_sensitive_values_not_logged"
            },
        ]

        usability_failures: [
            {
                failure:     "User doesn't know which config source provided a value"
                prevention:  "Log at debug level: 'Config key X loaded from ENV/FILE/DEFAULT'"
                test_for_it: "test_config_source_logged"
            },
            {
                failure:     "Config file syntax error gives unhelpful message"
                prevention:  "Use toml crate's error messages with line/column info"
                test_for_it: "test_toml_parse_error_helpful"
            },
            {
                failure:     "User typos config key and wonders why it's ignored"
                prevention:  "Warn on unknown keys with suggestion of similar valid keys"
                test_for_it: "test_unknown_key_warning_with_suggestion"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Partial config file overwrites all defaults"
                prevention:  "Merge file config onto defaults, not replace"
                test_for_it: "test_partial_config_merges_with_defaults"
            },
            {
                failure:     "Empty string env var treated as missing"
                prevention:  "Distinguish between unset and set-to-empty env vars"
                test_for_it: "test_empty_env_var_vs_unset"
            },
        ]

        integration_failures: [
            {
                failure:     "XDG_CONFIG_HOME not respected on Linux"
                prevention:  "Use directories crate which handles XDG properly"
                test_for_it: "test_xdg_config_home_respected"
            },
            {
                failure:     "Config changes require restart, no reload capability"
                prevention:  "Document that config is loaded once at startup (acceptable for v1)"
                test_for_it: "test_config_immutable_after_load"
            },
        ]
    }

    // ========================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ========================================================================
    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_config_from_file"
                given: "A config file at ~/.config/intent/config.toml with log_level = 'debug'"
                when:  "Config::load() is called"
                then: [
                    "Config.log_level is LogLevel::Debug",
                    "Config.config_path is Some(path to loaded file)",
                    "No errors are returned",
                ]
                real_input: """
                    // config.toml contents:
                    log_level = "debug"
                    output_format = "json"
                    """
                expected_output: """
                    Config { log_level: Debug, output_format: Json, ... }
                    """
            },
            {
                name:  "test_env_override"
                given: "A config file with log_level = 'info' and INTENT_LOG_LEVEL=debug in environment"
                when:  "Config::load() is called"
                then: [
                    "Config.log_level is LogLevel::Debug (from env)",
                    "Debug log shows 'log_level loaded from ENV'",
                ]
                real_input: """
                    std::env::set_var("INTENT_LOG_LEVEL", "debug");
                    // config.toml has log_level = "info"
                    let config = Config::load()?;
                    """
                expected_output: """
                    config.log_level == LogLevel::Debug
                    """
            },
            {
                name:  "test_default_fallback"
                given: "No config file exists and no environment variables are set"
                when:  "Config::load() is called"
                then: [
                    "Config loads successfully with all default values",
                    "Config.log_level is LogLevel::Info (default)",
                    "Config.output_format is OutputFormat::Text (default)",
                    "Config.color_enabled depends on tty detection",
                ]
                real_input: """
                    // No config file, no env vars
                    let config = Config::load()?;
                    """
                expected_output: """
                    Config { log_level: Info, output_format: Text, color_enabled: true }
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_invalid_toml_syntax"
                given: "A config file with invalid TOML syntax"
                when:  "Config::load() is called"
                then: [
                    "Returns IntentError::Config with parse error details",
                    "Error message contains line and column of syntax error",
                ]
                real_input: """
                    // config.toml contents (invalid):
                    log_level = debug  // missing quotes
                    """
                expected_output: null
                expected_error:  "IntentError::Config with message containing 'line' and 'column'"
            },
            {
                name:  "test_invalid_enum_value"
                given: "A config file with log_level = 'verbose' (not a valid level)"
                when:  "Config::load() is called"
                then: [
                    "Returns IntentError::Config with validation error",
                    "Error message lists valid values",
                ]
                real_input: """
                    // config.toml contents:
                    log_level = "verbose"
                    """
                expected_output: null
                expected_error:  "IntentError::Config containing 'valid values: trace, debug, info, warn, error'"
            },
        ]

        edge_cases: [
            {
                name:     "test_empty_config_file"
                scenario: "Config file exists but is empty"
                input:    "touch ~/.config/intent/config.toml (0 bytes)"
                expected: "All defaults are used, no error"
            },
            {
                name:     "test_config_file_is_directory"
                scenario: "Config path points to a directory instead of file"
                input:    "Config::load_from(Path::new('/tmp'))"
                expected: "IntentError::Config with 'expected file, found directory'"
            },
            {
                name:     "test_unicode_in_config_values"
                scenario: "Config file contains Unicode characters"
                input:    "project_name = 'Projet Tete'"
                expected: "Unicode preserved correctly in Config struct"
            },
        ]

        contract_tests: [
            {
                name:     "test_precedence_cli_over_env"
                verifies: "CLI args override environment variables"
                test:     "Set INTENT_LOG_LEVEL=info, call Config::load_with_overrides(log_level: Some(Debug)), verify Debug is used"
            },
            {
                name:     "test_precedence_env_over_file"
                verifies: "Environment variables override file values"
                test:     "Create config file with log_level=info, set INTENT_LOG_LEVEL=debug, verify debug is used"
            },
            {
                name:     "test_invariant_validated_config"
                verifies: "All config values are validated before return"
                test:     "Assert Config struct only contains valid enum variants, never raw strings"
            },
        ]
    }

    // ========================================================================
    // SECTION 5: E2E TESTS
    // ========================================================================
    e2e_tests: {
        pipeline_test: {
            name:        "test_full_config_pipeline"
            description: "Test config loading from file with env overrides and validation"

            setup: {
                files_to_create: [
                    {
                        path: "/tmp/test-intent-config/config.toml"
                        content: """
                            # Test configuration file
                            log_level = "info"
                            output_format = "text"
                            color = true

                            [http]
                            timeout_secs = 30
                            retry_count = 3
                            """
                    },
                ]
                precondition_commands: [
                    "mkdir -p /tmp/test-intent-config",
                ]
                environment: [
                    "INTENT_LOG_LEVEL=debug",
                    "INTENT_CONFIG_PATH=/tmp/test-intent-config/config.toml",
                ]
            }

            execute: {
                command:    "cd /home/lewis/src/intent-cli && cargo test config:: --no-fail-fast"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test_config_from_file",
                    "test_env_override",
                    "test_default_fallback",
                    "passed",
                ]
            }

            cleanup: {
                files_to_delete: ["/tmp/test-intent-config"]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_config_xdg_paths"
                description: "Verify XDG Base Directory Specification compliance"
                steps: [
                    {
                        action: "Set XDG_CONFIG_HOME=/tmp/xdg-test and create config there"
                        verify: "Config::load() finds and uses that config file"
                    },
                    {
                        action: "Unset XDG_CONFIG_HOME, create ~/.config/intent/config.toml"
                        verify: "Config::load() uses ~/.config/intent/ by default"
                    },
                ]
            },
            {
                name:        "e2e_config_ci_detection"
                description: "Verify CI environment is detected correctly"
                steps: [
                    {
                        action: "Set CI=true environment variable"
                        verify: "Config.color_enabled is false, Config.interactive is false"
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
                task:      "Write test: test_config_from_file"
                file:      "crates/intent-core/src/config.rs"
                what:      "#[test] fn test_config_from_file() { ... }"
                done_when: "Test exists and FAILS (no Config struct yet)"
            },
            {
                task:      "Write test: test_env_override"
                file:      "crates/intent-core/src/config.rs"
                what:      "#[test] fn test_env_override() { ... }"
                done_when: "Test exists and FAILS"
            },
            {
                task:      "Write test: test_default_fallback"
                file:      "crates/intent-core/src/config.rs"
                what:      "#[test] fn test_default_fallback() { ... }"
                done_when: "Test exists and FAILS"
            },
        ]

        phase_2_implementation: [
            {
                task:      "Implement Config struct with default values"
                file:      "crates/intent-core/src/config.rs"
                what:      "Define Config struct with all fields and Default impl"
                done_when: "test_default_fallback PASSES"
                patterns_to_use: [
                    "#[derive(Debug, Clone, Default)] on Config",
                    "Use serde for deserialization",
                    "Implement Default trait with sensible defaults",
                ]
            },
            {
                task:      "Implement TOML file loading"
                file:      "crates/intent-core/src/config.rs"
                what:      "Add Config::load_from_file() method"
                done_when: "test_config_from_file PASSES"
                patterns_to_use: [
                    "Use toml crate for parsing",
                    "Map toml::de::Error to IntentError::Config",
                    "Use directories crate for XDG paths",
                ]
            },
            {
                task:      "Implement environment variable loading"
                file:      "crates/intent-core/src/config.rs"
                what:      "Add env_override() method that merges env vars"
                done_when: "test_env_override PASSES"
                patterns_to_use: [
                    "Use std::env::var() with proper error handling",
                    "Parse enum values with FromStr trait",
                    "Log source of each config value at debug level",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export Config from lib.rs"
                file:      "crates/intent-core/src/lib.rs"
                what:      "Add pub mod config; and re-export Config"
                done_when: "use intent_core::config::Config works"
            },
            {
                task:      "Add IntentError::Config variant"
                file:      "crates/intent-core/src/error.rs"
                what:      "Add Config variant with appropriate fields"
                done_when: "Config errors map to IntentError::Config"
            },
        ]

        phase_4_verification: [
            {
                task:      "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
            },
            {
                task:      "Verify precedence order"
                commands:  ["cargo test config::tests::test_precedence"]
                expected:  "All precedence tests pass"
                done_when: "CLI > env > file > default is verified"
            },
            {
                task:      "Verify no unwrap in config.rs"
                commands:  ["grep -n 'unwrap()' crates/intent-core/src/config.rs"]
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
                symptom:      "Config file not found error when file exists"
                likely_cause: "Path expansion (~/) not happening or wrong XDG path"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/config.rs"
                        function:      "config_file_path()"
                        what_to_check: "Verify directories crate is used, path is expanded"
                    },
                ]
                fix_pattern: "Use directories::ProjectDirs::from() and handle None case"
            },
            {
                symptom:      "TOML parse error: invalid type"
                likely_cause: "Serde type mismatch between TOML value and struct field"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/config.rs"
                        function:      "Config struct definition"
                        what_to_check: "Verify field types match expected TOML types"
                    },
                ]
                fix_pattern: "Use #[serde(default)] for optional fields, or use Option<T>"
            },
            {
                symptom:      "Environment variable not being read"
                likely_cause: "Wrong env var name or missing INTENT_ prefix"
                where_to_look: [
                    {
                        file:          "crates/intent-core/src/config.rs"
                        function:      "env_override()"
                        what_to_check: "Verify env var name matches exactly (case-sensitive)"
                    },
                ]
                fix_pattern: "Use const for env var names, log attempted reads at trace level"
            },
        ]

        debugging_commands: [
            {
                scenario: "When config file is not being found"
                run:      "RUST_LOG=trace cargo run -- --help 2>&1 | grep -i config"
                look_for: "Log messages showing config path resolution"
            },
            {
                scenario: "When env var override is not working"
                run:      "INTENT_LOG_LEVEL=trace RUST_LOG=debug cargo run -- --help"
                look_for: "Log showing 'log_level loaded from ENV'"
            },
        ]
    }

    // ========================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ========================================================================
    completion_checklist: {
        tests: [
            "[ ] test_config_from_file written and passing",
            "[ ] test_env_override written and passing",
            "[ ] test_default_fallback written and passing",
            "[ ] test_invalid_toml_syntax written and passing",
            "[ ] test_precedence_* tests written and passing",
            "[ ] E2E test with real config file passing",
        ]

        code: [
            "[ ] Config struct with all fields defined",
            "[ ] Default trait implemented with sensible defaults",
            "[ ] TOML loading with proper error mapping",
            "[ ] Environment variable override with INTENT_ prefix",
            "[ ] Precedence order enforced (CLI > env > file > default)",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] cargo doc builds without warnings",
        ]

        documentation: [
            "[ ] Module-level doc comment with example config.toml",
            "[ ] Each config field documented with env var name",
            "[ ] Precedence order documented",
        ]
    }

    // ========================================================================
    // SECTION 9: CONTEXT
    // ========================================================================
    context: {
        related_files: [
            {
                path:      "crates/intent-core/src/error.rs"
                relevance: "Config errors map to IntentError::Config variant"
            },
            {
                path:      "crates/intent-core/src/lib.rs"
                relevance: "Exports config module"
            },
            {
                path:      "crates/intent-cli/src/main.rs"
                relevance: "CLI commands use Config for settings"
            },
            {
                path:      "Cargo.toml"
                relevance: "toml and directories crate dependencies"
            },
        ]

        similar_implementations: [
            "ripgrep/crates/core/config.rs - Similar config loading pattern",
            "bat/src/config.rs - XDG config with env overrides",
        ]

        external_references: [
            "https://docs.rs/directories - XDG Base Directory Specification",
            "https://docs.rs/toml - TOML parsing",
            "https://no-color.org/ - NO_COLOR environment variable standard",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-oriented configuration loading"
                example_location: "crates/intent-core/src/config.rs"
                how_to_apply:     "Chain Config::default().merge_file()?.merge_env()?.validate()"
            },
            {
                pattern:          "Environment variable naming"
                example_location: "crates/intent-core/src/config.rs"
                how_to_apply:     "Use INTENT_ prefix, SCREAMING_SNAKE_CASE, e.g., INTENT_LOG_LEVEL"
            },
        ]
    }

    // ========================================================================
    // SECTION 10: AI HINTS
    // ========================================================================
    ai_hints: {
        do: [
            "Use directories crate for XDG path resolution",
            "Use toml crate for TOML parsing",
            "Use serde for Config struct deserialization",
            "Log config source at debug level for each key",
            "Implement Default trait for sensible defaults",
            "Use FromStr for parsing enum values from env",
            "Check NO_COLOR env var for color_enabled default",
        ]

        do_not: [
            "Do NOT hardcode paths like ~/.config - use directories crate",
            "Do NOT use unwrap() or expect() for config loading",
            "Do NOT silently ignore unknown config keys",
            "Do NOT log sensitive values (keys, tokens, passwords)",
            "Do NOT fail on missing config file - use defaults",
            "Do NOT use panic!, todo!, or unimplemented!",
        ]

        code_patterns: [
            {
                name:     "Railway-oriented config chain"
                use_when: "Loading configuration from multiple sources"
                example: """
                    pub fn load() -> Result<Config, IntentError> {
                        let default = Config::default();
                        let from_file = default.merge_file()?;
                        let from_env = from_file.merge_env()?;
                        from_env.validate()
                    }
                    """
            },
            {
                name:     "XDG config path resolution"
                use_when: "Finding the config file location"
                example: """
                    fn config_file_path() -> Option<PathBuf> {
                        directories::ProjectDirs::from("com", "intent", "intent-cli")
                            .map(|dirs| dirs.config_dir().join("config.toml"))
                    }
                    """
            },
            {
                name:     "Environment variable override"
                use_when: "Reading a config value from environment"
                example: """
                    fn env_log_level(&self) -> Result<Option<LogLevel>, IntentError> {
                        match std::env::var("INTENT_LOG_LEVEL") {
                            Ok(val) => val.parse().map(Some).map_err(|_| {
                                IntentError::config(format!(
                                    "Invalid INTENT_LOG_LEVEL '{}'. Valid: trace, debug, info, warn, error",
                                    val
                                ))
                            }),
                            Err(std::env::VarError::NotPresent) => Ok(None),
                            Err(e) => Err(IntentError::config(format!("Error reading INTENT_LOG_LEVEL: {}", e))),
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
