package schema

import "github.com/intent-cli/intent/schema/common"

// Test that complete error structure can be instantiated
testError: common.#ErrorTaxonomy & {
	code: "SPEC_NOT_FOUND"
	category: "validation"
	severity: "error"
	message: "Specification file not found"
}

// Test that fix suggestions are properly typed
testFix: common.#ErrorFix & {
	type: "replace_file"
	suggestion: "Check the file path"
}

// Test validation errors
testValidationError: common.#ValidationErrors.SPEC_NOT_FOUND

// Test runtime errors
testRuntimeError: common.#RuntimeErrors.EXECUTION_FAILED

// Test network errors
testNetworkError: common.#NetworkErrors.CONNECTION_REFUSED

// Test auth errors
testAuthError: common.#AuthErrors.HTTP_AUTH_ERROR

// Test system errors
testSystemError: common.#SystemErrors.SECURITY_VIOLATION

// Test custom validation error
customValidationError: common.#ErrorTaxonomy & {
	category: "validation"
	code: "CUSTOM_ERROR"
	severity: "error"
	message: "Custom validation error"
}

// Test error with fix suggestion
errorWithFix: common.#ErrorTaxonomy & {
	code: "SPEC_INVALID"
	category: "validation"
	severity: "error"
	message: "Spec is invalid"
	fix: {
		type: "replace_string"
		suggestion: "Fix the spec syntax"
		old: "wrong"
		new: "correct"
	}
}

// Test error with recovery steps
errorWithRecovery: common.#ErrorTaxonomy & {
	code: "FILE_NOT_FOUND"
	category: "config"
	severity: "error"
	message: "File not found"
	recovery: [
		"Check file path",
		"Verify file exists",
		"Check permissions"
	]
	retry_allowed: true
}
