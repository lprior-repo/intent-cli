// Intent CLI command metadata - source of truth for all commands
//
// This file defines the complete structure of the Intent CLI, including
// all commands, flags, arguments, output formats, and AI protocols.
//
// AI agents should use `intent manifest` to access this data programmatically.
package command

cli: #CLI & {
	name:        "intent"
	version:     "0.1.0"
	description: "Contract-driven requirements engineering and API testing"

	commands: {
		// =====================================================================
		// CORE COMMANDS
		// =====================================================================

		about: {
			name:        "about"
			description: "Comprehensive explanation of Intent's purpose and AI-first design"
			category:    "core"
			usage:       "intent about"
			examples: [
				"intent about",
			]
			arguments: []
			flags: {}
			outputs: {
				formats: ["text"]
			}
			exit_codes: [
				{code: 0, meaning: "success", when: "always"},
			]
			related: ["interview", "manifest", "protocol"]
		}

		check: {
			name:        "check"
			description: "Run spec against a target URL and verify behaviors"
			category:    "core"
			usage:       "intent check <spec.cue> [flags]"
			examples: [
				"intent check spec.cue",
				"intent check spec.cue --target http://localhost:8080",
				"intent check spec.cue --feature auth --json",
				"intent check spec.cue --only login --verbose",
				"intent check spec.cue --dry-run",
			]
			arguments: [
				{
					name:        "spec"
					description: "Path to CUE specification file"
					required:    true
					type:        "path"
					examples: ["spec.cue", "examples/user-api.cue"]
				},
			]
			flags: {
				target: {
					name:        "target"
					description: "Target base URL to test against"
					type:        "string"
					default:     ""
					env_var:     "INTENT_TARGET"
				}
				json: {
					name:        "json"
					description: "Output results as JSON"
					type:        "bool"
					default:     false
					env_var:     "INTENT_JSON"
				}
				junit: {
					name:        "junit"
					description: "Output results as JUnit XML for CI integration"
					type:        "bool"
					default:     false
				}
				tap: {
					name:        "tap"
					description: "Output results as TAP (Test Anything Protocol)"
					type:        "bool"
					default:     false
				}
				sarif: {
					name:        "sarif"
					description: "Output results as SARIF (Static Analysis Results Interchange Format)"
					type:        "bool"
					default:     false
				}
				feature: {
					name:        "feature"
					description: "Filter to a specific feature"
					type:        "string"
					default:     ""
				}
				only: {
					name:        "only"
					description: "Run only a specific behavior"
					type:        "string"
					default:     ""
				}
				verbose: {
					name:        "verbose"
					short:       "v"
					description: "Verbose output"
					type:        "bool"
					default:     false
					env_var:     "INTENT_VERBOSE"
				}
				quiet: {
					name:        "quiet"
					short:       "q"
					description: "Quiet output (errors only)"
					type:        "bool"
					default:     false
				}
				"dry-run": {
					name:        "dry-run"
					description: "Show what would be tested without making requests"
					type:        "bool"
					default:     false
				}
			}
			outputs: {
				formats: ["json", "junit", "tap", "sarif", "text"]
				schema:  "intent.cue#TestResult"
			}
			exit_codes: [
				{code: 0, meaning: "success", when: "all tests passed"},
				{code: 1, meaning: "failure", when: "one or more tests failed"},
				{code: 4, meaning: "error", when: "spec loading failed or invalid configuration"},
			]
			related: ["validate", "show", "quality"]
		}

		// =====================================================================
		// INTERVIEW COMMANDS
		// =====================================================================

		interview: {
			name:        "interview"
			description: "Guided specification discovery through structured interview"
			category:    "interview"
			usage:       "intent interview [flags]"
			examples: [
				"intent interview --cue --profile api",
				"intent interview --cue --session interview-abc123 --answer \"THE SYSTEM SHALL...\"",
				"intent interview --profile cli --export spec.cue",
				"intent interview --resume session-123",
			]
			arguments: []
			flags: {
				profile: {
					name:        "profile"
					description: "System profile: api, cli, event, data, workflow, or ui"
					type:        "string"
					default:     "api"
					values: ["api", "cli", "event", "data", "workflow", "ui"]
				}
				cue: {
					name:        "cue"
					description: "AI AGENT MODE - Output CUE (no interactive prompts)"
					type:        "bool"
					default:     false
				}
				session: {
					name:        "session"
					description: "Session ID for resuming Q&A loop (requires --cue)"
					type:        "string"
					default:     ""
				}
				answer: {
					name:        "answer"
					description: "Answer text in EARS format (requires --cue --session)"
					type:        "string"
					default:     ""
				}
				resume: {
					name:        "resume"
					description: "Resume existing interview session by ID"
					type:        "string"
					default:     ""
				}
				export: {
					name:        "export"
					description: "Export completed interview to spec file"
					type:        "string"
					default:     ""
				}
				answers: {
					name:        "answers"
					description: "Path to CUE file with pre-filled answers for non-interactive mode"
					type:        "string"
					default:     ""
				}
				strict: {
					name:        "strict"
					description: "Strict mode - reject incomplete answers"
					type:        "bool"
					default:     false
				}
			}
			outputs: {
				formats: ["cue", "text"]
				schema:  "ai_interview.cue#AIDirective"
			}
			ai_protocol: {
				non_interactive: true
				deterministic:   true
				input_format:    "ears_pattern"
				output_format:   "cue_directive"
				validation:      "ears_conformance"
			}
			exit_codes: [
				{code: 0, meaning: "success", when: "interview completed or question asked"},
				{code: 4, meaning: "error", when: "validation failed or invalid session"},
			]
			related: ["beads", "quality", "gaps", "invert"]
		}

		// =====================================================================
		// BEADS COMMANDS
		// =====================================================================

		beads: {
			name:        "beads"
			description: "Generate atomic, executable work items (beads) from validated interview requirements"
			category:    "beads"
			usage:       "intent beads <session-id>"
			examples: [
				"intent beads interview-abc123def456",
			]
			arguments: [
				{
					name:        "session_id"
					description: "Interview session ID"
					required:    true
					type:        "id"
					examples: ["interview-abc123def456"]
				},
			]
			flags: {}
			outputs: {
				formats: ["json"]
				schema:  "bead.cue#Bead"
			}
			ai_protocol: {
				non_interactive: true
				deterministic:   true
				input_format:    "session_id"
				output_format:   "json_beads"
				validation:      "session_exists"
			}
			exit_codes: [
				{code: 0, meaning: "success", when: "beads generated and exported"},
				{code: 4, meaning: "error", when: "session not found or invalid"},
			]
			related: ["interview", "execute-beads", "bead-status"]
		}

		// =====================================================================
		// KIRK COMMANDS
		// =====================================================================

		quality: {
			name:        "quality"
			description: "KIRK: Multi-dimensional quality scoring with contract-driven rigor"
			category:    "kirk"
			usage:       "intent quality <spec.cue> [flags]"
			examples: [
				"intent quality spec.cue",
				"intent quality spec.cue --json",
			]
			arguments: [
				{
					name:        "spec"
					description: "Path to CUE specification file"
					required:    true
					type:        "path"
					examples: ["spec.cue"]
				},
			]
			flags: {
				json: {
					name:        "json"
					description: "Output as JSON"
					type:        "bool"
					default:     false
				}
			}
			outputs: {
				formats: ["json", "text"]
				schema:  "kirk.cue#QualityScore"
			}
			exit_codes: [
				{code: 0, meaning: "success", when: "analysis completed"},
				{code: 4, meaning: "error", when: "spec loading failed"},
			]
			related: ["invert", "gaps", "coverage", "check"]
		}
	}
}
