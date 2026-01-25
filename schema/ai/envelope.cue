// AI-Only Request/Response Envelope
// For JSONL-based command protocol with Claude Code
package ai

// Request envelope for AI agents
// Sent as JSONL to stdin
#Request: {
	// Correlation ID for request/response matching
	id: string

	// Command in "domain.action" format (e.g., "vision.start", "spec.quality")
	command: string

	// Command-specific arguments
	args: {...}

	// Optional execution options
	options?: {
		// Timeout in milliseconds (must be positive)
		timeout_ms?: int & >0

		// If true, validates but doesn't execute
		dry_run?: bool
	}
}

// Response envelope from CLI
// Written as JSONL to stdout
#Response: {
	// Echo correlation ID from request
	id: string

	// Whether the command succeeded
	success: bool

	// Echo command from request
	command: string

	// Command-specific payload
	data: {...}

	// Errors if any occurred
	errors: [...#Error]

	// Suggested next actions for AI workflow
	next_actions: [...#NextAction]

	// Execution metadata
	metadata: #Metadata
}

// Structured error for AI self-repair
#Error: {
	// Machine-readable error code (e.g., "SESSION_NOT_FOUND")
	code: string

	// Human-readable message
	message: string

	// Path to the file/field causing the error
	location?: string

	// Suggestion for fixing the error
	fix_hint?: string

	// JSONL command that can fix this error
	fix_command?: string
}

// Next action suggestion for AI agents
#NextAction: {
	// Full JSONL request as a string
	command: string

	// Explanation of why this action makes sense
	reason: string

	// Priority level (1=highest, 5=lowest)
	priority: int & >=1 & <=5

	// Request IDs that must complete before this action
	blocks?: [...string]
}

// Metadata about command execution
#Metadata: {
	// ISO 8601 timestamp
	timestamp: string

	// CLI version
	version: string

	// Execution duration in milliseconds
	duration_ms: int

	// Exit code (0=success)
	exit_code: int
}
