package protocol

// The Universal Input Envelope
// AI Agents send this JSON structure to STDIN
#Request: {
	// The command to execute (e.g., "check", "quality", "sys.schema")
	command: string

	// Command-specific parameters
	params: {...}

	// Session context for stateful operations (optional)
	// If provided, the CLI resumes this session state.
	session_id?: string
	
	// If true, calculates the effect but writes nothing to disk
	simulate?: bool | *false

	// Select specific output fields to save tokens (GraphQL-style)
	// e.g., ["status", "data.score"]
	select?: [...string]
}

// The Universal Output Envelope
// The CLI writes this JSON structure to STDOUT
#Response: {
	// "ok" | "error" | "requires_input"
	status: "ok" | "error" | "requires_input"

	// The primary payload. Structure depends on `command`.
	data: {...}

	// AI-specific metadata
	metadata: {
		timestamp: string
		duration_ms: int
		version: string
	}

	// Context for the next turn
	session_id?: string

	// Exact JSON payloads the AI should consider sending next.
	// This eliminates "guessing" valid next steps.
	next_actions: [...#Request]

	// If an error occurred, this field is populated
	error?: #Error
}

// Structured Error for AI Self-Repair
#Error: {
	// Machine-readable error code (e.g., "SPEC_NOT_FOUND")
	code: string

	// Human-readable message (fallback)
	message: string

	// Path to the file/field causing the error
	path?: string

	// Zero-shot repair suggestion.
	// The AI can verify this and then apply it.
	fix?: {
		type: "replace_file" | "replace_string" | "run_command"
		file?: string
		content?: string
		old?: string
		new?: string
		command?: string
	}
}

// --- Command Definitions ---

// command: "check"
#CheckParams: {
	spec: string
	target?: string
}

// command: "validate"
#ValidateParams: {
	spec: string
}

// command: "quality"
#QualityParams: {
	spec: string
}

// command: "sys.schema"
#SchemaParams: {}

