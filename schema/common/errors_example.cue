package common

// Example Usage of Error Taxonomy
//
// This file demonstrates how to use the error taxonomy in specs and code generation

// Example 1: Basic validation error
exampleValidationError: #ErrorTaxonomy & {
	code: "SPEC_NOT_FOUND"
	category: "validation"
	severity: "error"
	message: "Cannot find specification file"
	path: "/path/to/spec.cue"
	retry_allowed: true
	exit_code: 4
}

// Example 2: Error with fix suggestion
exampleWithFix: #ErrorTaxonomy & {
	code: "CUE_PARSE_ERROR"
	category: "validation"
	severity: "error"
	message: "CUE syntax error on line 42"
	path: "spec.cue"
	fix: {
		type: "replace_string"
		suggestion: "Add missing comma after field"
		file: "spec.cue"
		old: "field1: \"value\"\n  field2:"
		new: "field1: \"value\",\n  field2:"
	}
	retry_allowed: true
	exit_code: 3
}

// Example 3: Error with recovery steps
exampleWithRecovery: #ErrorTaxonomy & {
	code: "HTTP_CONNECTION_ERROR"
	category: "network"
	severity: "error"
	message: "Failed to connect to http://localhost:8080"
	path: "http://localhost:8080"
	recovery: [
		"Verify the service is running: curl http://localhost:8080",
		"Check the port number is correct",
		"Try with --allow-localhost flag for development",
	]
	retry_allowed: true
	exit_code: 4
}

// Example 4: Critical security error
exampleSecurityError: #ErrorTaxonomy & {
	code: "PATH_TRAVERSAL"
	category: "system"
	severity: "critical"
	message: "Path traversal attempt detected"
	path: "../../../etc/passwd"
	explanation: "The provided path contains parent directory references that could access files outside the allowed directory"
	recovery: [
		"Remove .. from the path",
		"Use absolute paths instead",
		"Ensure the path is within the project directory",
	]
	retry_allowed: true
	exit_code: 4
}

// Example 5: Using predefined error templates
examplePredefinedError: #ValidationErrors.SPEC_NOT_FOUND & {
	path: "missing-spec.cue"
}

// Example 6: Custom error extending taxonomy
customBusinessError: #ErrorTaxonomy & {
	code: "BUSINESS_RULE_VIOLATED"
	category: "validation"
	severity: "error"
	message: "Order total exceeds customer credit limit"
	context: {
		order_total: "5000.00"
		credit_limit: "3000.00"
		customer_id: "CUST-123"
	}
	explanation: "The order cannot be processed because the total amount exceeds the customer's approved credit limit"
	recovery: [
		"Reduce order quantity",
		"Request credit limit increase",
		"Use alternative payment method",
	]
	retry_allowed: false
	exit_code: 1
}
