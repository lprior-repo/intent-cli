package ai

import ( "github.com/intent-cli/intent/schema/common"

	// AI Error Taxonomy for Intent CLI
	// Provides structured error classification for AI operations
	//
	// Design Principles:
	// 1. All AI error codes follow AI_CATEGORY_SPECIFIC pattern
	// 2. Extends common error taxonomy with AI-specific categories
	// 3. Each error includes actionable recovery steps
	// 4. Aligns with Gleam ai_errors and prompt_errors modules
	// 5. Supports both human-readable and JSON error output
)

// AI Error Category - AI-specific error classification
#AIErrorCategory: "prompt" | "template" | "model" | "context" | "parsing"

// AI Error Code - Machine-readable identifier (AI_CATEGORY_SPECIFIC)
#AIErrorCode: string & =~"^AI_[A-Z_]+$"

// AI Error Taxonomy Structure - Extends common error taxonomy
#AIErrorTaxonomy: {
	// Machine-readable error code
	code: #AIErrorCode

	// AI-specific error category
	category: #AIErrorCategory

	// Severity level (from common)
	severity: common.#ErrorSeverity

	// Human-readable message
	message: string

	// Optional: Detailed explanation
	explanation?: string

	// Optional: File/field path causing error
	path?: string

	// Optional: Session context
	session_id?: string

	// Optional: Bead context
	bead_id?: string

	// Optional: Fix suggestion
	fix?: common.#ErrorFix

	// Optional: Additional context
	context?: {...}

	// Optional: Recovery steps
	recovery?: [...string]

	// Whether retry might succeed
	retry_allowed?: bool | *false

	// Suggested exit code
	exit_code?: int | *1
}

// ============================================================================
// Prompt Error Codes
// ============================================================================

#PromptErrors: {
	BEAD_LOAD_ERROR: #AIErrorTaxonomy & {
		code:        "AI_BEAD_LOAD_ERROR"
		category:    "prompt"
		severity:    "error"
		message:     "Failed to load bead data from session"
		explanation: "Could not read or parse bead data from the session file"
		recovery: [
			"Verify session exists: intent sessions",
			"Check session file integrity",
			"Review CUE export output",
			"Ensure session file is not corrupted",
		]
		retry_allowed: true
		exit_code:     3
	}

	BEAD_PARSE_ERROR: #AIErrorTaxonomy & {
		code:        "AI_BEAD_PARSE_ERROR"
		category:    "prompt"
		severity:    "error"
		message:     "Failed to parse bead data structure"
		explanation: "Bead JSON/CUE structure does not match expected schema"
		recovery: [
			"Check bead structure matches schema",
			"Verify all required fields are present",
			"Review decode errors for specific fields",
			"Validate CUE export: cue export <session>.cue",
		]
		retry_allowed: true
		exit_code:     3
	}

	BEAD_NOT_FOUND: #AIErrorTaxonomy & {
		code:        "AI_BEAD_NOT_FOUND"
		category:    "prompt"
		severity:    "error"
		message:     "Bead not found in session"
		explanation: "The requested bead ID does not exist in the session"
		recovery: [
			"List available beads: intent beads <session>",
			"Check bead ID spelling",
			"Verify session contains beads",
			"Regenerate beads if needed",
		]
		retry_allowed: false
		exit_code:     4
	}

	SESSION_NOT_FOUND: #AIErrorTaxonomy & {
		code:        "AI_SESSION_NOT_FOUND"
		category:    "prompt"
		severity:    "error"
		message:     "Interview session not found"
		explanation: "The specified session file does not exist"
		recovery: [
			"List sessions: intent sessions",
			"Start new session: intent interview",
			"Check session ID spelling",
			"Verify .interview/ directory exists",
		]
		retry_allowed: false
		exit_code:     4
	}

	CUE_VALIDATION_ERROR: #AIErrorTaxonomy & {
		code:        "AI_CUE_VALIDATION_ERROR"
		category:    "prompt"
		severity:    "error"
		message:     "CUE validation failed for session data"
		explanation: "Session CUE file has validation errors"
		recovery: [
			"Run: cue vet <session>.cue",
			"Check for syntax errors",
			"Verify schema constraints",
			"Review line number in error message",
		]
		retry_allowed: true
		exit_code:     3
	}

	PROMPT_BUILD_FAILED: #AIErrorTaxonomy & {
		code:        "AI_PROMPT_BUILD_FAILED"
		category:    "prompt"
		severity:    "error"
		message:     "Failed to build AI prompt from beads"
		explanation: "Error occurred while assembling prompt from bead data"
		recovery: [
			"Check bead data completeness",
			"Verify template compatibility",
			"Review bead structure",
			"Check for missing required fields",
		]
		retry_allowed: true
		exit_code:     1
	}
}

// ============================================================================
// Template Error Codes
// ============================================================================

#TemplateErrors: {
	RENDER_FAILED: #AIErrorTaxonomy & {
		code:        "AI_TEMPLATE_RENDER_ERROR"
		category:    "template"
		severity:    "error"
		message:     "Template rendering failed"
		explanation: "Error occurred while rendering template with bead data"
		recovery: [
			"Check template syntax",
			"Verify all template variables exist in data",
			"Review template error message",
			"Validate bead data structure",
		]
		retry_allowed: false
		exit_code:     1
	}

	TEMPLATE_NOT_FOUND: #AIErrorTaxonomy & {
		code:        "AI_TEMPLATE_NOT_FOUND"
		category:    "template"
		severity:    "error"
		message:     "Prompt template not found"
		explanation: "The specified template file does not exist"
		recovery: [
			"Check template path",
			"Verify template file exists",
			"Use default template if custom template unavailable",
		]
		retry_allowed: false
		exit_code:     4
	}

	TEMPLATE_SYNTAX_ERROR: #AIErrorTaxonomy & {
		code:        "AI_TEMPLATE_SYNTAX_ERROR"
		category:    "template"
		severity:    "error"
		message:     "Template syntax error"
		explanation: "Template contains invalid syntax"
		recovery: [
			"Review template syntax",
			"Check for unclosed tags",
			"Verify template language compatibility",
			"Validate template against schema",
		]
		retry_allowed: true
		exit_code:     3
	}
}

// ============================================================================
// Model Error Codes
// ============================================================================

#ModelErrors: {
	INFERENCE_FAILED: #AIErrorTaxonomy & {
		code:        "AI_INFERENCE_FAILED"
		category:    "model"
		severity:    "error"
		message:     "AI model inference failed"
		explanation: "Model failed to generate a response"
		recovery: [
			"Check model availability",
			"Verify API credentials",
			"Review prompt size and complexity",
			"Try again with simpler prompt",
		]
		retry_allowed: true
		exit_code:     1
	}

	MODEL_NOT_AVAILABLE: #AIErrorTaxonomy & {
		code:        "AI_MODEL_NOT_AVAILABLE"
		category:    "model"
		severity:    "error"
		message:     "AI model not available"
		explanation: "The specified model is not accessible"
		recovery: [
			"Check model name spelling",
			"Verify model exists in provider",
			"Check network connectivity",
			"Verify API credentials",
		]
		retry_allowed: true
		exit_code:     4
	}

	RATE_LIMIT_EXCEEDED: #AIErrorTaxonomy & {
		code:        "AI_RATE_LIMIT_EXCEEDED"
		category:    "model"
		severity:    "warning"
		message:     "API rate limit exceeded"
		explanation: "Too many requests to the AI model API"
		recovery: [
			"Wait before retrying",
			"Check rate limit settings",
			"Consider upgrading API plan",
			"Implement exponential backoff",
		]
		retry_allowed: true
		exit_code:     1
	}

	TOKEN_LIMIT_EXCEEDED: #AIErrorTaxonomy & {
		code:        "AI_TOKEN_LIMIT_EXCEEDED"
		category:    "model"
		severity:    "error"
		message:     "Token limit exceeded"
		explanation: "Prompt exceeds model's maximum token limit"
		recovery: [
			"Reduce prompt size",
			"Split into multiple prompts",
			"Summarize input content",
			"Use a model with larger context window",
		]
		retry_allowed: true
		exit_code:     1
	}
}

// ============================================================================
// Context Error Codes
// ============================================================================

#ContextErrors: {
	CONTEXT_OVERFLOW: #AIErrorTaxonomy & {
		code:        "AI_CONTEXT_OVERFLOW"
		category:    "context"
		severity:    "error"
		message:     "Context window overflow"
		explanation: "Combined prompt and context exceeds available space"
		recovery: [
			"Reduce context size",
			"Summarize previous context",
			"Use selective context retrieval",
			"Split conversation into sessions",
		]
		retry_allowed: true
		exit_code:     1
	}

	CONTEXT_INVALID: #AIErrorTaxonomy & {
		code:        "AI_CONTEXT_INVALID"
		category:    "context"
		severity:    "error"
		message:     "Invalid context structure"
		explanation: "Context data does not match expected format"
		recovery: [
			"Validate context schema",
			"Check for malformed JSON",
			"Verify all required context fields",
			"Review context generation logic",
		]
		retry_allowed: true
		exit_code:     3
	}

	CONTEXT_MISSING: #AIErrorTaxonomy & {
		code:        "AI_CONTEXT_MISSING"
		category:    "context"
		severity:    "warning"
		message:     "Required context missing"
		explanation: "Expected context data is not available"
		recovery: [
			"Provide required context",
			"Check context source",
			"Verify context retrieval logic",
			"Use default context if available",
		]
		retry_allowed: true
		exit_code:     1
	}
}

// ============================================================================
// Parsing Error Codes
// ============================================================================

#ParsingErrors: {
	DECODE_FAILED: #AIErrorTaxonomy & {
		code:        "AI_DECODE_FAILED"
		category:    "parsing"
		severity:    "error"
		message:     "JSON/CUE decode failed"
		explanation: "Could not decode data structure from JSON or CUE"
		recovery: [
			"Check JSON syntax",
			"Verify CUE structure",
			"Review decode error details",
			"Validate against schema",
		]
		retry_allowed: true
		exit_code:     3
	}

	RESPONSE_PARSE_ERROR: #AIErrorTaxonomy & {
		code:        "AI_RESPONSE_PARSE_ERROR"
		category:    "parsing"
		severity:    "error"
		message:     "Failed to parse model response"
		explanation: "AI model response does not match expected format"
		recovery: [
			"Check expected response schema",
			"Review model output format",
			"Adjust prompt for structured output",
			"Implement fallback parsing",
		]
		retry_allowed: true
		exit_code:     1
	}

	FIELD_EXTRACTION_FAILED: #AIErrorTaxonomy & {
		code:        "AI_FIELD_EXTRACTION_FAILED"
		category:    "parsing"
		severity:    "error"
		message:     "Failed to extract field from response"
		explanation: "Expected field not found in parsed response"
		recovery: [
			"Check field path",
			"Verify field exists in response",
			"Review parsing logic",
			"Check for schema changes",
		]
		retry_allowed: false
		exit_code:     1
	}

	SECURITY_ERROR: #AIErrorTaxonomy & {
		code:        "AI_SECURITY_ERROR"
		category:    "parsing"
		severity:    "critical"
		message:     "Security violation in AI operation"
		explanation: "Potential security issue detected in input or output"
		recovery: [
			"Review input for malicious content",
			"Validate against security policy",
			"Sanitize user input",
			"Check for injection attempts",
		]
		retry_allowed: false
		exit_code:     4
	}
}

// ============================================================================
// Unified AI Error Registry
// ============================================================================

// All AI error codes for easy lookup
#AllAIErrors: {
	// Prompt errors
	AI_BEAD_LOAD_ERROR:      string
	AI_BEAD_PARSE_ERROR:     string
	AI_BEAD_NOT_FOUND:       string
	AI_SESSION_NOT_FOUND:    string
	AI_CUE_VALIDATION_ERROR: string
	AI_PROMPT_BUILD_FAILED:  string

	// Template errors
	AI_TEMPLATE_RENDER_ERROR: string
	AI_TEMPLATE_NOT_FOUND:    string
	AI_TEMPLATE_SYNTAX_ERROR: string

	// Model errors
	AI_INFERENCE_FAILED:     string
	AI_MODEL_NOT_AVAILABLE:  string
	AI_RATE_LIMIT_EXCEEDED:  string
	AI_TOKEN_LIMIT_EXCEEDED: string

	// Context errors
	AI_CONTEXT_OVERFLOW: string
	AI_CONTEXT_INVALID:  string
	AI_CONTEXT_MISSING:  string

	// Parsing errors
	AI_DECODE_FAILED:           string
	AI_RESPONSE_PARSE_ERROR:    string
	AI_FIELD_EXTRACTION_FAILED: string
	AI_SECURITY_ERROR:          string
}
