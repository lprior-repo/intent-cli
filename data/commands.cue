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

		check: {
			name:        "check"
			description: "Run spec against a target URL and verify behaviors"
			category:    "core"
			usage:       "intent check <spec.cue> [flags]"
			examples: [
				"intent check spec.cue",
				"intent check spec.cue --target=http://localhost:8080",
				"intent check spec.cue --feature=auth --json",
			]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: {
				target:  {name: "target", description: "Target base URL to test against", type: "string", required: false, default: "", env_var: "INTENT_TARGET"}
				json:    {name: "json", description: "Output results as JSON", type: "bool", required: false, default: false}
				feature: {name: "feature", description: "Filter to a specific feature", type: "string", required: false, default: ""}
				only:    {name: "only", description: "Run only a specific behavior", type: "string", required: false, default: ""}
				verbose: {name: "verbose", short: "v", description: "Verbose output", type: "bool", required: false, default: false}
				quiet:   {name: "quiet", short: "q", description: "Quiet output (errors only)", type: "bool", required: false, default: false}
			}
			outputs: {formats: ["json", "text"], schema: "intent.cue#TestResult"}
			exit_codes: [
				{code: 0, meaning: "success", when: "all tests passed"},
				{code: 1, meaning: "failure", when: "one or more tests failed"},
				{code: 4, meaning: "error", when: "spec loading failed"},
			]
			related: ["validate", "quality"]
		}

		validate: {
			name:        "validate"
			description: "Validate a CUE spec file (syntax and structure)"
			category:    "core"
			usage:       "intent validate <spec.cue>"
			examples: ["intent validate spec.cue"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: {}
			outputs: {formats: ["text"]}
			exit_codes: [
				{code: 0, meaning: "success", when: "spec is valid"},
				{code: 4, meaning: "error", when: "spec is invalid"},
			]
			related: ["check", "lint"]
		}

		show: {
			name:        "show"
			description: "Pretty print a parsed spec"
			category:    "core"
			usage:       "intent show <spec.cue> [flags]"
			examples: ["intent show spec.cue", "intent show spec.cue --json"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: json: {name: "json", description: "Output as JSON", type: "bool", required: false, default: false}
			outputs: {formats: ["json", "text"]}
			exit_codes: [{code: 0, meaning: "success", when: "spec displayed"}, {code: 4, meaning: "error", when: "spec loading failed"}]
			related: ["validate", "export"]
		}

		export: {
			name:        "export"
			description: "Export spec to JSON format"
			category:    "core"
			usage:       "intent export <spec.cue>"
			examples: ["intent export spec.cue"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: {}
			outputs: {formats: ["json"]}
			exit_codes: [{code: 0, meaning: "success", when: "spec exported"}, {code: 4, meaning: "error", when: "spec loading failed"}]
			related: ["show"]
		}

		lint: {
			name:        "lint"
			description: "Check spec for anti-patterns and quality issues"
			category:    "core"
			usage:       "intent lint <spec.cue>"
			examples: ["intent lint spec.cue"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: {}
			outputs: {formats: ["text"]}
			exit_codes: [{code: 0, meaning: "success", when: "no issues found"}, {code: 1, meaning: "failure", when: "issues found"}]
			related: ["analyze", "quality"]
		}

		analyze: {
			name:        "analyze"
			description: "Analyze spec quality and provide improvement suggestions"
			category:    "core"
			usage:       "intent analyze <spec.cue>"
			examples: ["intent analyze spec.cue"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: {}
			outputs: {formats: ["text"]}
			exit_codes: [{code: 0, meaning: "success", when: "analysis completed"}]
			related: ["quality", "improve"]
		}

		improve: {
			name:        "improve"
			description: "Suggest improvements based on quality analysis and linting"
			category:    "core"
			usage:       "intent improve <spec.cue>"
			examples: ["intent improve spec.cue"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: {}
			outputs: {formats: ["text"]}
			exit_codes: [{code: 0, meaning: "success", when: "suggestions provided"}]
			related: ["analyze", "lint"]
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
				"intent interview --cue --profile=api",
				"intent interview --cue --session=interview-abc123 --answer=\"THE SYSTEM SHALL...\"",
				"intent interview --profile=cli --export=spec.cue",
			]
			arguments: []
			flags: {
				profile: {name: "profile", description: "System profile: api, cli, event, data, workflow, or ui", type: "string", required: false, default: "api", values: ["api", "cli", "event", "data", "workflow", "ui"]}
				cue:     {name: "cue", description: "AI AGENT MODE - Output CUE (no interactive prompts)", type: "bool", required: false, default: false}
				session: {name: "session", description: "Session ID for resuming Q&A loop (requires --cue)", type: "string", required: false, default: ""}
				answer:  {name: "answer", description: "Answer text in EARS format (requires --cue --session)", type: "string", required: false, default: ""}
				resume:  {name: "resume", description: "Resume existing interview session by ID", type: "string", required: false, default: ""}
				export:  {name: "export", description: "Export completed interview to spec file", type: "string", required: false, default: ""}
				answers: {name: "answers", description: "Path to CUE file with pre-filled answers", type: "string", required: false, default: ""}
				strict:  {name: "strict", description: "Strict mode - reject incomplete answers", type: "bool", required: false, default: false}
			}
			outputs: {formats: ["cue", "text"], schema: "ai_interview.cue#AIDirective"}
			ai_protocol: {non_interactive: true, deterministic: true, input_format: "ears_pattern", output_format: "cue_directive", validation: "ears_conformance"}
			exit_codes: [
				{code: 0, meaning: "success", when: "interview completed or question asked"},
				{code: 4, meaning: "error", when: "validation failed or invalid session"},
			]
			related: ["beads", "quality", "gaps"]
		}

		// =====================================================================
		// BEADS COMMANDS
		// =====================================================================

		beads: {
			name:        "beads"
			description: "Generate atomic, executable work items (beads) from validated interview requirements"
			category:    "beads"
			usage:       "intent beads <session-id>"
			examples: ["intent beads interview-abc123def456"]
			arguments: [{name: "session_id", description: "Interview session ID", required: true, type: "id", examples: ["interview-abc123"]}]
			flags: {}
			outputs: {formats: ["json"], schema: "bead.cue#Bead"}
			ai_protocol: {non_interactive: true, deterministic: true, input_format: "session_id", output_format: "json_beads", validation: "session_exists"}
			exit_codes: [
				{code: 0, meaning: "success", when: "beads generated and exported"},
				{code: 4, meaning: "error", when: "session not found"},
			]
			related: ["interview", "bead-status"]
		}

		"bead-status": {
			name:        "bead-status"
			description: "Mark bead execution status (success/failed/blocked)"
			category:    "beads"
			usage:       "intent bead-status --bead-id=<id> --status=<success|failed|blocked>"
			examples: [
				"intent bead-status --bead-id=bead-123 --status=success",
				"intent bead-status --bead-id=bead-456 --status=blocked --reason=\"waiting on API\"",
			]
			arguments: []
			flags: {
				"bead-id": {name: "bead-id", description: "Bead ID (required)", type: "string", required: true}
				status:    {name: "status", description: "Status: success, failed, or blocked", type: "string", required: true, values: ["success", "failed", "blocked"]}
				reason:    {name: "reason", description: "Reason for status (required for blocked)", type: "string", required: false, default: ""}
				session:   {name: "session", description: "Session ID", type: "string", required: false, default: ""}
			}
			outputs: {formats: ["json"]}
			exit_codes: [{code: 0, meaning: "success", when: "status updated"}]
			related: ["beads"]
		}

		"beads-regenerate": {
			name:        "beads-regenerate"
			description: "Regenerate failed/blocked beads with adjusted approach"
			category:    "beads"
			usage:       "intent beads-regenerate <session-id> [--strategy=<hybrid|inversion|premortem>]"
			examples: ["intent beads-regenerate abc123", "intent beads-regenerate abc123 --strategy=inversion"]
			arguments: [{name: "session_id", description: "Interview session ID", required: true, type: "id", examples: ["abc123"]}]
			flags: strategy: {name: "strategy", description: "Regeneration strategy", type: "string", required: false, default: "hybrid", values: ["hybrid", "inversion", "premortem"]}
			outputs: {formats: ["json"]}
			exit_codes: [{code: 0, meaning: "success", when: "beads regenerated"}]
			related: ["beads", "bead-status"]
		}

		// =====================================================================
		// PLANNING COMMANDS
		// =====================================================================

		plan: {
			name:        "plan"
			description: "Display execution plan from session beads"
			category:    "planning"
			usage:       "intent plan <session-id> [--format=<human|json>]"
			examples: ["intent plan abc123", "intent plan abc123 --format=json"]
			arguments: [{name: "session_id", description: "Interview session ID", required: true, type: "id", examples: ["abc123"]}]
			flags: format: {name: "format", description: "Output format", type: "string", required: false, default: "human", values: ["human", "json"]}
			outputs: {formats: ["json", "text"]}
			exit_codes: [{code: 0, meaning: "success", when: "plan displayed"}]
			related: ["plan-approve", "beads"]
		}

		"plan-approve": {
			name:        "plan-approve"
			description: "Approve execution plan for session"
			category:    "planning"
			usage:       "intent plan-approve <session-id> [--yes] [--notes=<text>]"
			examples: ["intent plan-approve abc123", "intent plan-approve abc123 --yes"]
			arguments: [{name: "session_id", description: "Interview session ID", required: true, type: "id", examples: ["abc123"]}]
			flags: {
				yes:   {name: "yes", description: "Auto-approve for CI", type: "bool", required: false, default: false}
				notes: {name: "notes", description: "Approval notes", type: "string", required: false, default: ""}
			}
			outputs: {formats: ["json"]}
			exit_codes: [{code: 0, meaning: "success", when: "plan approved"}]
			related: ["plan"]
		}

		// =====================================================================
		// SESSION MANAGEMENT
		// =====================================================================

		history: {
			name:        "history"
			description: "View snapshot history for an interview session"
			category:    "utility"
			usage:       "intent history <session-id>"
			examples: ["intent history interview-abc123"]
			arguments: [{name: "session_id", description: "Interview session ID", required: true, type: "id", examples: ["interview-abc123"]}]
			flags: {}
			outputs: {formats: ["text"]}
			exit_codes: [{code: 0, meaning: "success", when: "history displayed"}]
			related: ["diff", "sessions"]
		}

		diff: {
			name:        "diff"
			description: "Compare two interview sessions and show differences"
			category:    "utility"
			usage:       "intent diff <from-session> <to-session>"
			examples: ["intent diff interview-abc123 interview-def456"]
			arguments: [
				{name: "from_id", description: "First session ID", required: true, type: "id", examples: ["interview-abc123"]},
				{name: "to_id", description: "Second session ID", required: true, type: "id", examples: ["interview-def456"]},
			]
			flags: {}
			outputs: {formats: ["text"]}
			exit_codes: [{code: 0, meaning: "success", when: "differences displayed"}]
			related: ["history", "sessions"]
		}

		sessions: {
			name:        "sessions"
			description: "List all interview sessions"
			category:    "utility"
			usage:       "intent sessions [--json] [--profile=<profile>]"
			examples: ["intent sessions", "intent sessions --json", "intent sessions --profile=api"]
			arguments: []
			flags: {
				json:    {name: "json", description: "Output as JSON", type: "bool", required: false, default: false}
				profile: {name: "profile", description: "Filter by profile", type: "string", required: false, default: "", values: ["api", "cli", "event", "data", "workflow", "ui"]}
			}
			outputs: {formats: ["json", "text"]}
			exit_codes: [{code: 0, meaning: "success", when: "sessions listed"}]
			related: ["interview", "history"]
		}

		// =====================================================================
		// KIRK ANALYSIS COMMANDS
		// =====================================================================

		quality: {
			name:        "quality"
			description: "KIRK: Multi-dimensional quality scoring with contract-driven rigor"
			category:    "kirk"
			usage:       "intent quality <spec.cue> [--json]"
			examples: ["intent quality spec.cue", "intent quality spec.cue --json"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: json: {name: "json", description: "Output as JSON", type: "bool", required: false, default: false}
			outputs: {formats: ["json", "text"], schema: "kirk.cue#QualityScore"}
			exit_codes: [{code: 0, meaning: "success", when: "analysis completed"}, {code: 4, meaning: "error", when: "spec loading failed"}]
			related: ["invert", "gaps", "coverage"]
		}

		invert: {
			name:        "invert"
			description: "KIRK: Inversion thinking - systematically discover what could fail"
			category:    "kirk"
			usage:       "intent invert <spec.cue> [--json]"
			examples: ["intent invert spec.cue", "intent invert spec.cue --json"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: json: {name: "json", description: "Output as JSON", type: "bool", required: false, default: false}
			outputs: {formats: ["json", "text"]}
			exit_codes: [{code: 0, meaning: "success", when: "analysis completed"}]
			related: ["quality", "gaps"]
		}

		coverage: {
			name:        "coverage"
			description: "KIRK: Coverage analysis including OWASP Top 10"
			category:    "kirk"
			usage:       "intent coverage <spec.cue> [--json]"
			examples: ["intent coverage spec.cue", "intent coverage spec.cue --json"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: json: {name: "json", description: "Output as JSON", type: "bool", required: false, default: false}
			outputs: {formats: ["json", "text"]}
			exit_codes: [{code: 0, meaning: "success", when: "analysis completed"}]
			related: ["quality", "gaps"]
		}

		gaps: {
			name:        "gaps"
			description: "KIRK: Mental lattice gap detection using systematic thinking frameworks"
			category:    "kirk"
			usage:       "intent gaps <spec.cue> [--json]"
			examples: ["intent gaps spec.cue", "intent gaps spec.cue --json"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: json: {name: "json", description: "Output as JSON", type: "bool", required: false, default: false}
			outputs: {formats: ["json", "text"]}
			exit_codes: [{code: 0, meaning: "success", when: "analysis completed"}]
			related: ["quality", "invert", "coverage"]
		}

		effects: {
			name:        "effects"
			description: "KIRK: Analyze second-order effects (consequence tracing)"
			category:    "kirk"
			usage:       "intent effects <spec.cue>"
			examples: ["intent effects spec.cue"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: {}
			outputs: {formats: ["text"]}
			exit_codes: [{code: 0, meaning: "success", when: "analysis completed"}]
			related: ["quality", "gaps"]
		}

		compact: {
			name:        "compact"
			description: "KIRK: Convert to Compact Intent Notation (token-efficient)"
			category:    "kirk"
			usage:       "intent compact <spec.cue> [--tokens]"
			examples: ["intent compact spec.cue", "intent compact spec.cue --tokens"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: tokens: {name: "tokens", description: "Show token comparison", type: "bool", required: false, default: false}
			outputs: {formats: ["text"]}
			exit_codes: [{code: 0, meaning: "success", when: "conversion completed"}]
			related: ["prototext"]
		}

		prototext: {
			name:        "prototext"
			description: "KIRK: Export to Protobuf text format"
			category:    "kirk"
			usage:       "intent prototext <spec.cue>"
			examples: ["intent prototext spec.cue"]
			arguments: [{name: "spec", description: "Path to CUE specification file", required: true, type: "path", examples: ["spec.cue"]}]
			flags: {}
			outputs: {formats: ["text"]}
			exit_codes: [{code: 0, meaning: "success", when: "export completed"}]
			related: ["compact"]
		}

		// =====================================================================
		// REQUIREMENTS PARSING
		// =====================================================================

		ears: {
			name:        "ears"
			description: "KIRK: Parse EARS requirements to Intent behaviors"
			category:    "kirk"
			usage:       "intent ears <requirements.md> [--output=<format>]"
			examples: ["intent ears requirements.md", "intent ears requirements.md --output=cue"]
			arguments: [{name: "requirements_path", description: "Path to EARS requirements file", required: true, type: "path", examples: ["requirements.md"]}]
			flags: {
				output: {name: "output", description: "Output format", type: "string", required: false, default: "text", values: ["text", "cue", "json"]}
				out:    {name: "out", description: "Output file path", type: "string", required: false, default: ""}
				name:   {name: "name", description: "Spec name for CUE output", type: "string", required: false, default: "GeneratedSpec"}
			}
			outputs: {formats: ["text", "cue", "json"]}
			exit_codes: [{code: 0, meaning: "success", when: "parsing completed"}]
			related: ["parse"]
		}

		parse: {
			name:        "parse"
			description: "Parse EARS requirements to Intent behaviors"
			category:    "kirk"
			usage:       "intent parse <requirements.ears.md> [--o=<spec.cue>]"
			examples: ["intent parse requirements.md", "intent parse requirements.md --o=spec.cue"]
			arguments: [{name: "requirements_path", description: "Path to EARS requirements file", required: true, type: "path", examples: ["requirements.ears.md"]}]
			flags: {
				o:    {name: "o", description: "Output spec file path", type: "string", required: false, default: ""}
				json: {name: "json", description: "Output as JSON", type: "bool", required: false, default: false}
			}
			outputs: {formats: ["json", "text", "cue"]}
			exit_codes: [{code: 0, meaning: "success", when: "parsing completed"}]
			related: ["ears"]
		}
	}
}
