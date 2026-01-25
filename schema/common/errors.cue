package common

// Error Taxonomy for Intent CLI
// Provides structured error classification, codes, and recovery guidance
//
// Design Principles:
// 1. All error codes follow UPPERCASE_SNAKE_CASE naming
// 2. Errors grouped by category for routing (validation, runtime, config, network, auth, system)
// 3. Each error includes actionable recovery steps
// 4. Exit codes follow Unix conventions (0=success, 1=general, 3=parse, 4=input)
// 5. retry_allowed indicates if the operation is idempotent

// Error Category - Top-level classification
#ErrorCategory: "validation" | "runtime" | "config" | "network" | "auth" | "system"

// Error Severity - Impact level
#ErrorSeverity: "critical" | "error" | "warning" | "info"

// Error Code - Machine-readable identifier (CATEGORY_SPECIFIC_ERROR)
#ErrorCode: string & =~"^[A-Z_]+$"

// Fix Action Type - How to remediate
#FixType: "replace_file" | "replace_string" | "run_command" | "edit_config" | "manual"

// Error Fix Suggestion - Actionable repair guidance
#ErrorFix: {
	// Type of fix action
	type: #FixType

	// Human-readable suggestion
	suggestion: string

	// File path if applicable
	file?: string

	// Content for file operations
	content?: string

	// Old/new strings for replacements
	old?: string
	new?: string

	// Command to run
	command?: string

	// Additional context
	context?: {...}
}

// Complete Error Taxonomy Structure
#ErrorTaxonomy: {
	// Machine-readable error code
	code: #ErrorCode

	// Error category for routing
	category: #ErrorCategory

	// Severity level
	severity: #ErrorSeverity

	// Human-readable message
	message: string

	// Optional: Detailed explanation
	explanation?: string

	// Optional: File/field path causing error
	path?: string

	// Optional: Fix suggestion
	fix?: #ErrorFix

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
// Validation Error Codes
// ============================================================================

#ValidationErrors: {
	SPEC_NOT_FOUND: #ErrorTaxonomy & {
		code: "SPEC_NOT_FOUND"
		category: "validation"
		severity: "error"
		message: "Specification file not found"
		exit_code: 4
		retry_allowed: true
	}

	SPEC_INVALID: #ErrorTaxonomy & {
		code: "SPEC_INVALID"
		category: "validation"
		severity: "error"
		message: "Specification structure is invalid"
		exit_code: 3
		retry_allowed: true
	}

	CUE_PARSE_ERROR: #ErrorTaxonomy & {
		code: "CUE_PARSE_ERROR"
		category: "validation"
		severity: "error"
		message: "CUE syntax error"
		exit_code: 3
		retry_allowed: true
	}

	CUE_VALIDATION_FAILED: #ErrorTaxonomy & {
		code: "CUE_VALIDATION_FAILED"
		category: "validation"
		severity: "error"
		message: "CUE validation constraints not met"
		exit_code: 3
		retry_allowed: true
	}

	CUE_EXPORT_FAILED: #ErrorTaxonomy & {
		code: "CUE_EXPORT_FAILED"
		category: "validation"
		severity: "error"
		message: "CUE export to JSON failed"
		exit_code: 3
		retry_allowed: true
	}

	JSON_PARSE_ERROR: #ErrorTaxonomy & {
		code: "JSON_PARSE_ERROR"
		category: "validation"
		severity: "error"
		message: "JSON parsing failed"
		exit_code: 3
		retry_allowed: false
	}

	JSON_DECODE_FAILED: #ErrorTaxonomy & {
		code: "JSON_DECODE_FAILED"
		category: "validation"
		severity: "error"
		message: "JSON structure decoding failed"
		exit_code: 3
		retry_allowed: false
	}

	FIELD_NOT_FOUND: #ErrorTaxonomy & {
		code: "FIELD_NOT_FOUND"
		category: "validation"
		severity: "error"
		message: "Required field not found in response"
		exit_code: 1
		retry_allowed: false
	}

	VALIDATION_FAILED: #ErrorTaxonomy & {
		code: "VALIDATION_FAILED"
		category: "validation"
		severity: "error"
		message: "Response validation check failed"
		exit_code: 1
		retry_allowed: false
	}

	FORMAT_INVALID: #ErrorTaxonomy & {
		code: "FORMAT_INVALID"
		category: "validation"
		severity: "error"
		message: "Field format validation failed"
		exit_code: 1
		retry_allowed: false
	}

	INVALID_INPUT: #ErrorTaxonomy & {
		code: "INVALID_INPUT"
		category: "validation"
		severity: "error"
		message: "Invalid input provided"
		exit_code: 4
		retry_allowed: true
	}
}

// ============================================================================
// Runtime Error Codes
// ============================================================================

#RuntimeErrors: {
	EXECUTION_FAILED: #ErrorTaxonomy & {
		code: "EXECUTION_FAILED"
		category: "runtime"
		severity: "error"
		message: "Command execution failed"
		exit_code: 1
		retry_allowed: false
	}

	ASSERTION_FAILED: #ErrorTaxonomy & {
		code: "ASSERTION_FAILED"
		category: "runtime"
		severity: "error"
		message: "Assertion check failed"
		exit_code: 1
		retry_allowed: false
	}

	TIMEOUT: #ErrorTaxonomy & {
		code: "TIMEOUT"
		category: "runtime"
		severity: "error"
		message: "Operation timed out"
		exit_code: 1
		retry_allowed: true
	}

	CIRCULAR_DEPENDENCY: #ErrorTaxonomy & {
		code: "CIRCULAR_DEPENDENCY"
		category: "runtime"
		severity: "error"
		message: "Circular dependency detected in behavior chain"
		exit_code: 3
		retry_allowed: true
	}

	INTERPOLATION_FAILED: #ErrorTaxonomy & {
		code: "INTERPOLATION_FAILED"
		category: "runtime"
		severity: "error"
		message: "Variable interpolation failed"
		exit_code: 3
		retry_allowed: true
	}

	CAPTURE_FAILED: #ErrorTaxonomy & {
		code: "CAPTURE_FAILED"
		category: "runtime"
		severity: "error"
		message: "Failed to capture variable from response"
		exit_code: 1
		retry_allowed: false
	}
}

// ============================================================================
// Configuration Error Codes
// ============================================================================

#ConfigErrors: {
	FILE_NOT_FOUND: #ErrorTaxonomy & {
		code: "FILE_NOT_FOUND"
		category: "config"
		severity: "error"
		message: "Configuration file not found"
		exit_code: 4
		retry_allowed: true
	}

	FILE_PERMISSION_DENIED: #ErrorTaxonomy & {
		code: "FILE_PERMISSION_DENIED"
		category: "config"
		severity: "error"
		message: "Permission denied accessing file"
		exit_code: 4
		retry_allowed: false
	}

	INVALID_PATH: #ErrorTaxonomy & {
		code: "INVALID_PATH"
		category: "config"
		severity: "error"
		message: "Invalid file path provided"
		exit_code: 4
		retry_allowed: true
	}

	SESSION_NOT_FOUND: #ErrorTaxonomy & {
		code: "SESSION_NOT_FOUND"
		category: "config"
		severity: "error"
		message: "Interview session not found"
		exit_code: 4
		retry_allowed: true
	}

	MISSING_CONFIG: #ErrorTaxonomy & {
		code: "MISSING_CONFIG"
		category: "config"
		severity: "error"
		message: "Required configuration missing"
		exit_code: 4
		retry_allowed: true
	}
}

// ============================================================================
// Network Error Codes
// ============================================================================

#NetworkErrors: {
	CONNECTION_REFUSED: #ErrorTaxonomy & {
		code: "CONNECTION_REFUSED"
		category: "network"
		severity: "error"
		message: "Connection refused by target"
		exit_code: 4
		retry_allowed: true
	}

	NETWORK_TIMEOUT: #ErrorTaxonomy & {
		code: "NETWORK_TIMEOUT"
		category: "network"
		severity: "error"
		message: "Network request timed out"
		exit_code: 4
		retry_allowed: true
	}

	DNS_FAILED: #ErrorTaxonomy & {
		code: "DNS_FAILED"
		category: "network"
		severity: "error"
		message: "DNS resolution failed"
		exit_code: 4
		retry_allowed: true
	}

	HTTP_CONNECTION_ERROR: #ErrorTaxonomy & {
		code: "HTTP_CONNECTION_ERROR"
		category: "network"
		severity: "error"
		message: "HTTP connection error"
		exit_code: 4
		retry_allowed: true
	}

	HTTP_SERVER_ERROR: #ErrorTaxonomy & {
		code: "HTTP_SERVER_ERROR"
		category: "network"
		severity: "error"
		message: "HTTP 5xx server error"
		exit_code: 4
		retry_allowed: true
	}
}

// ============================================================================
// Authentication Error Codes
// ============================================================================

#AuthErrors: {
	AUTH_REQUIRED: #ErrorTaxonomy & {
		code: "AUTH_REQUIRED"
		category: "auth"
		severity: "error"
		message: "Authentication required"
		exit_code: 4
		retry_allowed: true
	}

	HTTP_AUTH_ERROR: #ErrorTaxonomy & {
		code: "HTTP_AUTH_ERROR"
		category: "auth"
		severity: "error"
		message: "HTTP authentication failed"
		exit_code: 4
		retry_allowed: true
	}

	TOKEN_EXPIRED: #ErrorTaxonomy & {
		code: "TOKEN_EXPIRED"
		category: "auth"
		severity: "error"
		message: "Authentication token expired"
		exit_code: 4
		retry_allowed: true
	}

	FORBIDDEN: #ErrorTaxonomy & {
		code: "FORBIDDEN"
		category: "auth"
		severity: "error"
		message: "Access forbidden"
		exit_code: 4
		retry_allowed: false
	}
}

// ============================================================================
// System Error Codes
// ============================================================================

#SystemErrors: {
	SECURITY_VIOLATION: #ErrorTaxonomy & {
		code: "SECURITY_VIOLATION"
		category: "system"
		severity: "critical"
		message: "Security policy violation"
		exit_code: 4
		retry_allowed: true
	}

	PATH_TRAVERSAL: #ErrorTaxonomy & {
		code: "PATH_TRAVERSAL"
		category: "system"
		severity: "critical"
		message: "Path traversal attempt detected"
		exit_code: 4
		retry_allowed: true
	}

	SSRF_ATTEMPT: #ErrorTaxonomy & {
		code: "SSRF_ATTEMPT"
		category: "system"
		severity: "critical"
		message: "SSRF attempt detected"
		exit_code: 4
		retry_allowed: true
	}

	UNSAFE_REGEX: #ErrorTaxonomy & {
		code: "UNSAFE_REGEX"
		category: "system"
		severity: "error"
		message: "Unsafe regex pattern detected"
		exit_code: 4
		retry_allowed: true
	}

	SHELL_INJECTION: #ErrorTaxonomy & {
		code: "SHELL_INJECTION"
		category: "system"
		severity: "critical"
		message: "Shell injection attempt detected"
		exit_code: 4
		retry_allowed: true
	}

	SYMLINK_NOT_ALLOWED: #ErrorTaxonomy & {
		code: "SYMLINK_NOT_ALLOWED"
		category: "system"
		severity: "error"
		message: "Symlinks not allowed for security"
		exit_code: 4
		retry_allowed: true
	}

	UNKNOWN_ERROR: #ErrorTaxonomy & {
		code: "UNKNOWN_ERROR"
		category: "system"
		severity: "error"
		message: "An unknown error occurred"
		exit_code: 1
		retry_allowed: false
	}
}

// ============================================================================
// Unified Error Registry
// ============================================================================

// All error codes in one place for easy lookup
// This is a documentation structure showing available error codes
#AllErrors: {
	// Validation errors
	SPEC_NOT_FOUND: string
	SPEC_INVALID: string
	CUE_PARSE_ERROR: string
	CUE_VALIDATION_FAILED: string
	CUE_EXPORT_FAILED: string
	JSON_PARSE_ERROR: string
	JSON_DECODE_FAILED: string
	FIELD_NOT_FOUND: string
	VALIDATION_FAILED: string
	FORMAT_INVALID: string
	INVALID_INPUT: string

	// Runtime errors
	EXECUTION_FAILED: string
	ASSERTION_FAILED: string
	TIMEOUT: string
	CIRCULAR_DEPENDENCY: string
	INTERPOLATION_FAILED: string
	CAPTURE_FAILED: string

	// Config errors
	FILE_NOT_FOUND: string
	FILE_PERMISSION_DENIED: string
	INVALID_PATH: string
	SESSION_NOT_FOUND: string
	MISSING_CONFIG: string

	// Network errors
	CONNECTION_REFUSED: string
	NETWORK_TIMEOUT: string
	DNS_FAILED: string
	HTTP_CONNECTION_ERROR: string
	HTTP_SERVER_ERROR: string

	// Auth errors
	AUTH_REQUIRED: string
	HTTP_AUTH_ERROR: string
	TOKEN_EXPIRED: string
	FORBIDDEN: string

	// System errors
	SECURITY_VIOLATION: string
	PATH_TRAVERSAL: string
	SSRF_ATTEMPT: string
	UNSAFE_REGEX: string
	SHELL_INJECTION: string
	SYMLINK_NOT_ALLOWED: string
	UNKNOWN_ERROR: string
}
