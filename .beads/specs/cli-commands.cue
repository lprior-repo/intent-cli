package beads

// Import the schema
// Note: In production, use: import "intent-cli.com/schema"

// ============================================================================
// BEAD: cli-commands - CLI command structure with clap
// ============================================================================

bead: #ValidBead & {
    id:              "intent-cli-cli01"
    title:           "cli: Implement CLI command structure with clap"
    type:            "feature"
    priority:        2
    effort_estimate: "4hr"
    labels:          ["cli", "commands", "m5", "rust-port"]

    // ========================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ========================================================================
    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL parse command-line arguments using clap derive macros",
            "THE SYSTEM SHALL support subcommands: run, validate, init",
            "THE SYSTEM SHALL provide --help for the main command and all subcommands",
            "THE SYSTEM SHALL provide --version showing semantic version",
            "THE SYSTEM SHALL use structured exit codes (0=success, 1-6=error categories)",
        ]

        event_driven: [
            {
                trigger: "WHEN user invokes 'intent run <spec>'"
                shall:   "THE SYSTEM SHALL parse the spec path and dispatch to runner module"
            },
            {
                trigger: "WHEN user invokes 'intent validate <spec>'"
                shall:   "THE SYSTEM SHALL parse the spec and validate CUE syntax without executing"
            },
            {
                trigger: "WHEN user invokes 'intent init'"
                shall:   "THE SYSTEM SHALL create a new intent project with scaffold files"
            },
            {
                trigger: "WHEN user provides --help flag"
                shall:   "THE SYSTEM SHALL display help text and exit with code 0"
            },
            {
                trigger: "WHEN user provides --version flag"
                shall:   "THE SYSTEM SHALL display version string and exit with code 0"
            },
            {
                trigger: "WHEN user provides invalid arguments"
                shall:   "THE SYSTEM SHALL display helpful error message with suggestion and exit with code 1"
            },
        ]

        state_driven: [
            {
                state: "WHILE in --json output mode"
                shall: "THE SYSTEM SHALL format all output (including errors) as JSON"
            },
            {
                state: "WHILE in --quiet mode"
                shall: "THE SYSTEM SHALL suppress informational output, showing only errors"
            },
        ]

        unwanted: [
            {
                condition: "IF user provides no subcommand"
                shall_not: "THE SYSTEM SHALL NOT crash or show stack trace"
                because:   "Users expect helpful guidance, not cryptic errors"
            },
            {
                condition: "IF error messages are vague or technical"
                shall_not: "THE SYSTEM SHALL NOT show raw clap errors without context"
                because:   "Users need actionable guidance to fix their command"
            },
            {
                condition: "IF flag names are inconsistent"
                shall_not: "THE SYSTEM SHALL NOT use different flag styles (e.g., mixing --out-dir and --output-directory)"
                because:   "Inconsistent flags confuse users and break muscle memory"
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
                    field:           "args"
                    type:            "Vec<String>"
                    constraints:     "Command-line arguments from std::env::args()"
                    example_valid:   "[\"intent\", \"run\", \"specs/api.cue\"]"
                    example_invalid: "[]"
                },
            ]
            system_state: [
                "clap and clap_derive dependencies available",
                "anyhow for main error handling",
            ]
        }

        postconditions: {
            state_changes: []
            return_guarantees: [
                {
                    field:     "Cli::parse()"
                    guarantee: "Returns parsed Cli struct or exits with helpful error"
                },
                {
                    field:     "Command variant"
                    guarantee: "Matches exactly one of: Run, Validate, Init"
                },
                {
                    field:     "Exit code"
                    guarantee: "0 for success, 1-6 for categorized errors"
                },
            ]
            side_effects: [
                "May print to stdout (help, version, normal output)",
                "May print to stderr (errors)",
            ]
        }

        invariants: [
            "All subcommands have --help available",
            "Flag names use kebab-case consistently (--output-format, not --outputFormat)",
            "Short flags are single letters and don't conflict (-o, -v, -h, -q)",
            "Required arguments are validated before dispatch",
            "Exit codes are consistent with IntentError mapping",
        ]
    }

    // ========================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ========================================================================
    inversions: {
        usability_failures: [
            {
                failure:     "Missing subcommand shows cryptic error instead of help"
                prevention:  "Configure clap to show help when no subcommand provided"
                test_for_it: "test_no_subcommand_shows_help"
            },
            {
                failure:     "Error message doesn't suggest correct command"
                prevention:  "Enable clap's suggestion feature for typos"
                test_for_it: "test_typo_suggests_correction"
            },
            {
                failure:     "Help text is auto-generated and unhelpful"
                prevention:  "Write custom help strings with examples for each subcommand"
                test_for_it: "test_help_contains_examples"
            },
            {
                failure:     "Required argument missing shows generic error"
                prevention:  "Use clap's value_name and help attributes for clear messages"
                test_for_it: "test_missing_spec_path_error"
            },
        ]

        integration_failures: [
            {
                failure:     "Exit codes don't match documented behavior"
                prevention:  "Use IntentError::exit_code() for all error exits"
                test_for_it: "test_exit_codes_match_error_types"
            },
            {
                failure:     "JSON output mode not respected by subcommands"
                prevention:  "Pass OutputFormat down to all handlers"
                test_for_it: "test_json_flag_propagates"
            },
            {
                failure:     "Version string is hardcoded instead of from Cargo.toml"
                prevention:  "Use clap's crate_version! macro"
                test_for_it: "test_version_matches_cargo"
            },
        ]

        security_failures: [
            {
                failure:     "Path traversal via spec argument"
                prevention:  "Validate spec path is within allowed directories"
                test_for_it: "test_spec_path_traversal_blocked"
            },
        ]
    }

    // ========================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ========================================================================
    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_run_command_parsing"
                given: "Arguments: ['intent', 'run', 'specs/api.cue']"
                when:  "Parsed with Cli::parse_from()"
                then: [
                    "Command is Command::Run",
                    "spec_path is PathBuf('specs/api.cue')",
                    "No errors thrown",
                ]
                real_input: """
                    Cli::parse_from(["intent", "run", "specs/api.cue"])
                    """
                expected_output: """
                    Command::Run { spec_path: PathBuf::from("specs/api.cue"), .. }
                    """
            },
            {
                name:  "test_validate_command"
                given: "Arguments: ['intent', 'validate', 'specs/user.cue', '--strict']"
                when:  "Parsed with Cli::parse_from()"
                then: [
                    "Command is Command::Validate",
                    "spec_path is PathBuf('specs/user.cue')",
                    "strict flag is true",
                ]
                real_input: """
                    Cli::parse_from(["intent", "validate", "specs/user.cue", "--strict"])
                    """
                expected_output: """
                    Command::Validate { spec_path: PathBuf::from("specs/user.cue"), strict: true }
                    """
            },
            {
                name:  "test_help_output"
                given: "Arguments: ['intent', '--help']"
                when:  "Executed"
                then: [
                    "Output contains 'intent'",
                    "Output contains 'run'",
                    "Output contains 'validate'",
                    "Output contains 'init'",
                    "Exit code is 0",
                ]
                real_input: """
                    intent --help
                    """
                expected_output: """
                    Usage: intent <COMMAND>

                    Commands:
                      run       Run API tests from a CUE spec
                      validate  Validate a CUE spec without running
                      init      Initialize a new intent project
                    """
            },
            {
                name:  "test_init_command"
                given: "Arguments: ['intent', 'init', '--name', 'my-api-tests']"
                when:  "Parsed with Cli::parse_from()"
                then: [
                    "Command is Command::Init",
                    "name is Some('my-api-tests')",
                ]
                real_input: """
                    Cli::parse_from(["intent", "init", "--name", "my-api-tests"])
                    """
                expected_output: """
                    Command::Init { name: Some("my-api-tests".to_string()) }
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_invalid_subcommand"
                given: "Arguments: ['intent', 'foobar']"
                when:  "Parsed with Cli::try_parse_from()"
                then: [
                    "Returns Err",
                    "Error message mentions 'foobar'",
                    "Error suggests similar commands",
                ]
                real_input: """
                    Cli::try_parse_from(["intent", "foobar"])
                    """
                expected_output: null
                expected_error:  "error: unrecognized subcommand 'foobar'"
            },
            {
                name:  "test_missing_required_arg"
                given: "Arguments: ['intent', 'run'] (missing spec path)"
                when:  "Parsed with Cli::try_parse_from()"
                then: [
                    "Returns Err",
                    "Error message mentions required argument",
                ]
                real_input: """
                    Cli::try_parse_from(["intent", "run"])
                    """
                expected_output: null
                expected_error:  "error: required arguments were not provided"
            },
            {
                name:  "test_conflicting_flags"
                given: "Arguments: ['intent', 'run', 'spec.cue', '--json', '--quiet']"
                when:  "Parsed with Cli::try_parse_from()"
                then: [
                    "Returns Err",
                    "Error indicates conflicting options",
                ]
                real_input: """
                    Cli::try_parse_from(["intent", "run", "spec.cue", "--json", "--quiet"])
                    """
                expected_output: null
                expected_error:  "error: argument '--json' cannot be used with '--quiet'"
            },
        ]

        edge_cases: [
            {
                name:     "test_empty_args"
                scenario: "No arguments provided (just binary name)"
                input:    "Cli::try_parse_from([\"intent\"])"
                expected: "Shows help text, exits cleanly"
            },
            {
                name:     "test_spec_path_with_spaces"
                scenario: "Spec path contains spaces"
                input:    "Cli::parse_from([\"intent\", \"run\", \"specs/my spec.cue\"])"
                expected: "Path is correctly parsed including spaces"
            },
            {
                name:     "test_multiple_verbose_flags"
                scenario: "User provides -vvv for extra verbosity"
                input:    "Cli::parse_from([\"intent\", \"run\", \"spec.cue\", \"-vvv\"])"
                expected: "verbose count is 3"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_all_commands_have_help"
                verifies: "All subcommands support --help"
                test:     "For each Command variant, assert try_parse_from([cmd, '--help']) exits cleanly"
            },
            {
                name:     "test_postcondition_parse_returns_valid_enum"
                verifies: "Cli::parse() always returns valid Command variant"
                test:     "After successful parse, matches!(cli.command, Command::Run | Command::Validate | Command::Init)"
            },
        ]
    }

    // ========================================================================
    // SECTION 5: E2E TESTS
    // ========================================================================
    e2e_tests: {
        pipeline_test: {
            name:        "test_full_cli_pipeline"
            description: "Test complete CLI from args to command dispatch"

            setup: {
                files_to_create: [
                    {
                        path: "/tmp/test-intent-cli/specs/simple.cue"
                        content: """
                            package specs

                            spec: {
                                name: "Simple Test"
                                base_url: "https://httpbin.org"
                                tests: [{
                                    name: "get_request"
                                    request: {
                                        method: "GET"
                                        path: "/get"
                                    }
                                    expect: {
                                        status: 200
                                    }
                                }]
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "mkdir -p /tmp/test-intent-cli/specs",
                ]
            }

            execute: {
                command:    "cd /home/lewis/src/intent-cli && cargo test cli:: --no-fail-fast"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test_run_command_parsing",
                    "test_validate_command",
                    "test_help_output",
                    "passed",
                ]
            }

            cleanup: {
                files_to_delete: ["/tmp/test-intent-cli"]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_cli_to_runner_integration"
                description: "Verify CLI correctly dispatches to runner with parsed args"
                steps: [
                    {
                        action: "Run 'intent run specs/simple.cue --json'"
                        verify: "Runner receives spec_path and json output format"
                    },
                    {
                        action: "Check exit code"
                        verify: "Exit code matches runner result (0 for success)"
                    },
                ]
            },
            {
                name:        "e2e_validate_without_execution"
                description: "Verify validate command checks syntax without HTTP calls"
                steps: [
                    {
                        action: "Run 'intent validate specs/simple.cue'"
                        verify: "No HTTP requests made"
                    },
                    {
                        action: "Check output"
                        verify: "Output indicates 'valid' or shows parse errors"
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
                task:      "Write test: test_run_command_parsing"
                file:      "crates/intent-cli/src/cli.rs"
                what:      "#[test] fn test_run_command_parsing() { ... }"
                done_when: "Test exists and FAILS (no Cli struct yet)"
            },
            {
                task:      "Write test: test_validate_command"
                file:      "crates/intent-cli/src/cli.rs"
                what:      "#[test] fn test_validate_command() { ... }"
                done_when: "Test exists and FAILS"
            },
            {
                task:      "Write test: test_help_output"
                file:      "crates/intent-cli/src/cli.rs"
                what:      "#[test] fn test_help_output() { ... }"
                done_when: "Test exists and FAILS"
            },
            {
                task:      "Write test: test_init_command"
                file:      "crates/intent-cli/src/cli.rs"
                what:      "#[test] fn test_init_command() { ... }"
                done_when: "Test exists and FAILS"
            },
        ]

        phase_2_implementation: [
            {
                task:      "Implement Cli struct with clap derive"
                file:      "crates/intent-cli/src/cli.rs"
                what:      "Define Cli struct with global flags (--json, --quiet, -v)"
                done_when: "Cli struct compiles with clap derive"
                patterns_to_use: [
                    "#[derive(Parser)] on Cli struct",
                    "#[command(author, version, about)] for metadata",
                    "#[arg(short, long)] for flags",
                ]
            },
            {
                task:      "Implement Command enum with subcommands"
                file:      "crates/intent-cli/src/cli.rs"
                what:      "Define Command::Run, Command::Validate, Command::Init"
                done_when: "All phase_1 tests PASS"
                patterns_to_use: [
                    "#[derive(Subcommand)] on Command enum",
                    "#[command(about = \"...\")] for help text",
                    "#[arg(value_name = \"SPEC\")] for positional args",
                ]
            },
            {
                task:      "Implement dispatch function"
                file:      "crates/intent-cli/src/cli.rs"
                what:      "fn dispatch(cli: Cli) -> Result<(), IntentError>"
                done_when: "Commands are routed to correct handlers"
                patterns_to_use: [
                    "match cli.command { ... }",
                    "Use anyhow::Context for error context",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Wire up main.rs with Cli"
                file:      "crates/intent-cli/src/main.rs"
                what:      "Parse args and call dispatch"
                done_when: "cargo run -- --help works"
            },
            {
                task:      "Export from lib.rs"
                file:      "crates/intent-cli/src/lib.rs"
                what:      "pub mod cli; with re-exports"
                done_when: "use intent_cli::Cli works"
            },
        ]

        phase_4_verification: [
            {
                task:      "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
            },
            {
                task:      "Verify help text quality"
                commands:  ["cargo run -- --help", "cargo run -- run --help"]
                expected:  "Help contains examples and clear descriptions"
                done_when: "Help text is user-friendly"
            },
            {
                task:      "Test all exit codes"
                commands:  ["cargo run -- run missing.cue; echo $?"]
                expected:  "Exit codes match IntentError mapping"
                done_when: "Exit codes are consistent"
            },
        ]
    }

    // ========================================================================
    // SECTION 7: FAILURE MODES
    // ========================================================================
    failure_modes: {
        failure_modes: [
            {
                symptom:      "clap error: 'required arguments were not provided'"
                likely_cause: "Missing #[arg] attribute or wrong positional order"
                where_to_look: [
                    {
                        file:          "crates/intent-cli/src/cli.rs"
                        what_to_check: "Verify #[arg] attributes on Command variants"
                    },
                ]
                fix_pattern: "Add #[arg(required = true)] or check positional arg order"
            },
            {
                symptom:      "Help text shows 'USAGE: ...' without examples"
                likely_cause: "Missing #[command(about = \"...\")] or custom help"
                where_to_look: [
                    {
                        file:          "crates/intent-cli/src/cli.rs"
                        function:      "Cli and Command definitions"
                        what_to_check: "Verify #[command(about, long_about)] attributes"
                    },
                ]
                fix_pattern: "Add descriptive about strings with examples"
            },
            {
                symptom:      "Short flag conflict: '-v' already used"
                likely_cause: "Multiple args using same short flag"
                where_to_look: [
                    {
                        file:          "crates/intent-cli/src/cli.rs"
                        what_to_check: "Search for #[arg(short = 'v')] duplicates"
                    },
                ]
                fix_pattern: "Use unique short flags or omit short for less common args"
            },
            {
                symptom:      "Exit code is always 1 regardless of error type"
                likely_cause: "Not using IntentError::exit_code() in main"
                where_to_look: [
                    {
                        file:          "crates/intent-cli/src/main.rs"
                        function:      "main"
                        what_to_check: "Verify std::process::exit(err.exit_code())"
                    },
                ]
                fix_pattern: "Use IntentError::exit_code() for process exit"
            },
        ]

        debugging_commands: [
            {
                scenario: "When arg parsing fails unexpectedly"
                run:      "RUST_BACKTRACE=1 cargo run -- run spec.cue 2>&1"
                look_for: "Clap error details and arg ordering"
            },
            {
                scenario: "When help text is wrong"
                run:      "cargo run -- --help 2>&1 | head -30"
                look_for: "Generated help text structure"
            },
        ]
    }

    // ========================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ========================================================================
    completion_checklist: {
        tests: [
            "[ ] test_run_command_parsing passes",
            "[ ] test_validate_command passes",
            "[ ] test_help_output passes",
            "[ ] test_init_command passes",
            "[ ] test_invalid_subcommand shows helpful error",
            "[ ] test_missing_required_arg shows clear message",
            "[ ] E2E test with real CLI invocation passes",
        ]

        code: [
            "[ ] Cli struct derives Parser with all global flags",
            "[ ] Command enum has Run, Validate, Init variants",
            "[ ] All subcommands have custom help text",
            "[ ] Exit codes use IntentError::exit_code()",
            "[ ] No unwrap() or expect() in CLI code",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] cargo clippy --all-targets passes",
            "[ ] cargo doc builds without warnings",
        ]

        documentation: [
            "[ ] Module-level doc comment with CLI usage examples",
            "[ ] Each Command variant has doc comment",
        ]
    }

    // ========================================================================
    // SECTION 9: CONTEXT
    // ========================================================================
    context: {
        related_files: [
            {
                path:      "crates/intent-cli/src/main.rs"
                relevance: "Entry point that calls Cli::parse() and dispatch()"
            },
            {
                path:      "crates/intent-cli/src/lib.rs"
                relevance: "Exports cli module"
            },
            {
                path:      "crates/intent-core/src/runner.rs"
                relevance: "Handler for Command::Run"
            },
            {
                path:      "crates/intent-core/src/config.rs"
                relevance: "Configuration loaded by CLI"
            },
            {
                path:      "crates/intent-core/src/error.rs"
                relevance: "IntentError for exit codes"
            },
        ]

        similar_implementations: [
            "zjj/crates/zjj-cli/src/cli.rs - Similar clap derive pattern",
            "ripgrep/crates/core/app.rs - Production CLI with clap",
        ]

        external_references: [
            "https://docs.rs/clap/latest/clap/ - clap documentation",
            "https://docs.rs/clap/latest/clap/_derive/ - clap derive tutorial",
            "https://clig.dev/ - Command Line Interface Guidelines",
        ]

        codebase_patterns: [
            {
                pattern:          "clap derive for arg parsing"
                example_location: "crates/intent-cli/src/cli.rs"
                how_to_apply:     "Use #[derive(Parser, Subcommand)] with #[arg] attributes"
            },
            {
                pattern:          "Dispatch with match on Command"
                example_location: "crates/intent-cli/src/cli.rs"
                how_to_apply:     "match cli.command { Command::Run { .. } => run_handler(), ... }"
            },
        ]
    }

    // ========================================================================
    // SECTION 10: AI HINTS
    // ========================================================================
    ai_hints: {
        do: [
            "Use clap derive macros (#[derive(Parser, Subcommand)])",
            "Use anyhow for main() error handling",
            "Use structured exit codes via IntentError::exit_code()",
            "Write custom help text with examples for each subcommand",
            "Use crate_version! and crate_authors! macros",
            "Make --json and --quiet mutually exclusive with clap groups",
            "Use PathBuf for file arguments, not String",
        ]

        do_not: [
            "Do NOT use unwrap() or expect() anywhere in CLI code",
            "Do NOT use manual arg parsing (no hand-rolled matches on args)",
            "Do NOT hardcode version strings",
            "Do NOT panic on invalid args (let clap handle errors)",
            "Do NOT use println! for errors (use eprintln! or structured output)",
            "Do NOT ignore the global flags when dispatching",
        ]

        code_patterns: [
            {
                name:     "Cli struct with global flags"
                use_when: "Defining the main CLI entry point"
                example: """
                    #[derive(Parser)]
                    #[command(author, version, about, long_about = None)]
                    pub struct Cli {
                        #[command(subcommand)]
                        pub command: Command,

                        /// Output format (json for machine parsing)
                        #[arg(long, global = true)]
                        pub json: bool,

                        /// Suppress non-error output
                        #[arg(short, long, global = true)]
                        pub quiet: bool,

                        /// Increase verbosity (-v, -vv, -vvv)
                        #[arg(short, long, action = clap::ArgAction::Count, global = true)]
                        pub verbose: u8,
                    }
                    """
            },
            {
                name:     "Command enum with subcommands"
                use_when: "Defining subcommands (run, validate, init)"
                example: """
                    #[derive(Subcommand)]
                    pub enum Command {
                        /// Run API tests from a CUE spec
                        #[command(about = "Run API tests from a CUE spec")]
                        Run {
                            /// Path to the CUE spec file
                            #[arg(value_name = "SPEC")]
                            spec_path: PathBuf,

                            /// Base URL override
                            #[arg(long)]
                            base_url: Option<String>,
                        },

                        /// Validate a CUE spec without running tests
                        #[command(about = "Validate a CUE spec without running")]
                        Validate {
                            /// Path to the CUE spec file
                            #[arg(value_name = "SPEC")]
                            spec_path: PathBuf,

                            /// Enable strict validation mode
                            #[arg(long)]
                            strict: bool,
                        },

                        /// Initialize a new intent project
                        #[command(about = "Initialize a new intent project")]
                        Init {
                            /// Project name
                            #[arg(long)]
                            name: Option<String>,
                        },
                    }
                    """
            },
            {
                name:     "Main with structured exit codes"
                use_when: "Implementing main() entry point"
                example: """
                    fn main() -> ExitCode {
                        let cli = Cli::parse();

                        match dispatch(cli) {
                            Ok(()) => ExitCode::SUCCESS,
                            Err(err) => {
                                eprintln!("Error: {err}");
                                ExitCode::from(err.exit_code() as u8)
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
