package common

// The Universal Input Envelope
// AI Agents send this JSON structure to STDIN
#Request: {
	// Correlation ID for request/response matching (AI feature)
	id?: string

	// The command to execute (e.g., "check", "quality", "sys.schema")
	command: string

	// Command-specific parameters (also available as 'args' for AI compatibility)
	params: {...}

	// Alias for params (AI compatibility)
	args?: params

	// Session context for stateful operations (optional)
	// If provided, the CLI resumes this session state.
	session_id?: string

	// If true, calculates the effect but writes nothing to disk
	simulate?: bool | *false

	// Select specific output fields to save tokens (GraphQL-style)
	// e.g., ["status", "data.score"]
	select?: [...string]

	// Optional execution options (AI feature)
	options?: {
		// Timeout in milliseconds (must be positive)
		timeout_ms?: int & >0

		// If true, validates but doesn't execute
		dry_run?: bool
	}
}

// The Universal Output Envelope
// The CLI writes this JSON structure to STDOUT
#Response: {
	// Echo correlation ID from request (AI feature)
	id?: string

	// "ok" | "error" | "requires_input"
	status: "ok" | "error" | "requires_input"

	// Boolean success indicator (AI feature - complements status)
	success?: bool

	// Echo command from request (AI feature)
	command?: string

	// The primary payload. Structure depends on `command`.
	data: {...}

	// AI-specific metadata
	metadata: {
		timestamp:   string
		duration_ms: int
		version:     string

		// Process exit code (AI feature)
		exit_code?: int
	}

	// Context for the next turn
	session_id?: string

	// Exact JSON payloads the AI should consider sending next.
	// This eliminates "guessing" valid next steps.
	next_actions: [...#Request]

	// Rich next actions with priority/reason (AI feature)
	next_actions_rich?: [...#NextAction]

	// Errors array for AI self-repair (AI feature)
	errors?: [...#Error]

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

	// Alias for path (AI compatibility) - can be provided instead of path
	location?: string

	// Human-readable fix suggestion (AI feature)
	fix_hint?: string

	// Zero-shot repair suggestion.
	// The AI can verify this and then apply it.
	fix?: {
		type:     "replace_file" | "replace_string" | "run_command"
		file?:    string
		content?: string
		old?:     string
		new?:     string
		command?: string

		// Human-readable suggestion
		suggestion?: string
	}
}

// Next action suggestion for AI agents (AI feature)
#NextAction: {
	// Full JSONL request as a string or structured object
	command: string

	// Explanation of why this action makes sense
	reason: string

	// Priority level (1=highest, 5=lowest)
	priority: int & >=1 & <=5

	// Request IDs that must complete before this action
	blocks?: [...string]
}
