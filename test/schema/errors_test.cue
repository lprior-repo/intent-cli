package schema_test

import "github.com/priorax/intent-cli/schema/common"

// Test that error categories are defined
test_error_categories: {
	categories: common.#ErrorCategory

	// Validate required categories exist
	_validation: categories == "validation" ||
	             categories == "runtime" ||
	             categories == "config" ||
	             categories == "network" ||
	             categories == "auth" ||
	             categories == "system"
}

// Test that error codes are properly structured
test_error_code: {
	code: common.#ErrorCode

	// Error code should follow pattern: CATEGORY_SPECIFIC_ERROR
	// e.g., "SPEC_NOT_FOUND", "NETWORK_TIMEOUT"
	_validation: code =~ "^[A-Z_]+$"
}

// Test that error severity levels exist
test_error_severity: {
	severity: common.#ErrorSeverity

	_validation: severity == "critical" ||
	             severity == "error" ||
	             severity == "warning" ||
	             severity == "info"
}

// Test that complete error structure can be instantiated
test_error_structure: {
	error: common.#ErrorTaxonomy

	// Must have required fields
	error: {
		code: "SPEC_NOT_FOUND"
		category: "validation"
		severity: "error"
		message: "Specification file not found"
	}
}

// Test that fix suggestions are properly typed
test_error_fix: {
	fix: common.#ErrorFix

	fix: {
		type: "replace_file"
		suggestion: "Check the file path"
	}
}

// Test validation errors category
test_validation_errors: {
	error: common.#ErrorTaxonomy & {
		category: "validation"
		code: "SPEC_NOT_FOUND" | "SPEC_INVALID" | "CUE_PARSE_ERROR"
	}
}

// Test runtime errors category
test_runtime_errors: {
	error: common.#ErrorTaxonomy & {
		category: "runtime"
		code: "EXECUTION_FAILED" | "ASSERTION_FAILED" | "TIMEOUT"
	}
}

// Test network errors category
test_network_errors: {
	error: common.#ErrorTaxonomy & {
		category: "network"
		code: "CONNECTION_REFUSED" | "NETWORK_TIMEOUT" | "DNS_FAILED"
	}
}
