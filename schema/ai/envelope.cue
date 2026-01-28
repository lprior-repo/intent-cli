// AI Envelope (Compatibility Layer)
// This file re-exports from common/envelope.cue for backward compatibility
// All envelope definitions are now consolidated in schema/common/envelope.cue
package ai

import "github.com/intent-cli/intent/schema/common"

// Request envelope for AI agents (re-exported from common)
// Sent as JSONL to stdin
#Request: common.#Request

// Response envelope from CLI (re-exported from common)
// Written as JSONL to stdout
#Response: common.#Response

// Structured error for AI self-repair (re-exported from common)
#Error: common.#Error

// Next action suggestion for AI agents (re-exported from common)
#NextAction: common.#NextAction

// Metadata about command execution (re-exported from common)
// Note: Use common.#Response.metadata which now includes exit_code
#Metadata: {
	timestamp:   string
	duration_ms: int
	version:     string
	exit_code?:  int
}
