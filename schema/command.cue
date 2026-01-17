// Command metadata schema - source of truth for CLI structure
//
// This schema defines the contract for all Intent CLI commands, enabling
// AI agents to discover and understand command capabilities programmatically.
//
// Philosophy:
// - CUE as source of truth (all command metadata defined in data/commands.cue)
// - AI-first design (machine-readable contracts, not just human help text)
// - Type safety (Gleam types mirror these CUE schemas)
// - Required fields (no defaults - explicit is better than implicit)
// - Multiple output formats (JSON, CUE, text)
package command

// CLI represents the complete Intent command-line interface
#CLI: {
	name:        string
	version:     string
	description: string
	commands: [string]: #Command
}

// Command represents a single CLI command with all metadata
#Command: {
	name:        string
	description: string
	category:    #Category

	// Usage patterns
	usage:    string
	examples: [...string]

	// Arguments and flags
	arguments: [...#Argument]
	flags: [string]: #Flag

	// Output capabilities
	outputs: {
		formats: [...#OutputFormat]
		schema?: string // Reference to CUE schema (e.g., "intent.cue#Spec")
	}

	// AI protocol information (for non-interactive AI agents)
	ai_protocol?: #AIProtocol

	// Exit codes and error handling
	exit_codes: [...#ExitCode]

	// Related commands for workflow discovery
	related?: [...string]
}

// Category groups commands by functional area
#Category: "core" | "interview" | "beads" | "kirk" | "planning" | "review" | "utility"

// Argument represents a positional argument
#Argument: {
	name:        string
	description: string
	required:    bool | *true
	type:        #ArgumentType
	examples:    [...string]
}

#ArgumentType: "path" | "string" | "id" | "number"

// Flag represents a named option
#Flag: {
	name:        string
	short?:      string           // Single letter short form (e.g., "-v" for verbose)
	description: string
	type:        #FlagType
	required:    bool | *false
	default?:    string | bool | int
	values?:     [...string]      // For enum flags (valid values)
	env_var?:    string           // Environment variable fallback
}

#FlagType: "bool" | "string" | "int"

// OutputFormat specifies supported output formats
#OutputFormat: "json" | "cue" | "text" | "xml" | "tap" | "sarif" | "junit"

// AIProtocol defines how AI agents interact with this command
#AIProtocol: {
	non_interactive: bool   // Can run without human interaction
	deterministic:   bool   // Same input produces same output
	input_format:    string // "ears_pattern", "spec_path", etc.
	output_format:   string // "cue_directive", "json_result", etc.
	validation:      string // "reject_if_vague", "ears_conformance", etc.
}

// ExitCode documents command exit codes
#ExitCode: {
	code:    int
	meaning: string
	when:    string // When this exit code occurs
}

// HelpRequest represents a request for help information
#HelpRequest: {
	command?: string        // Specific command, or empty for all
	format:   #HelpFormat   // Output format
	verbose:  bool | *false // Include detailed examples
}

#HelpFormat: "text" | "json" | "cue" | "markdown"

// HelpResponse is the structured help output
#HelpResponse: {
	action:  "help" | "error"
	command: string

	// Command metadata (if found)
	metadata?: #Command

	// Formatted output (depending on requested format)
	output: string

	// Error information (if action == "error")
	error?: {
		message: string
		suggestion?: string
	}
}
