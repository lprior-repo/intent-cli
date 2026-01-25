// Common output schema definitions for Intent CLI commands
// All command outputs follow the action-based JSON schema

package output

// Base response structure for all commands
#BaseResponse: {
	success:      bool
	action:       string
	command:      string
	data:         _
	errors:       [...#JsonError]
	next_actions: [...#NextAction]
	metadata:     #JsonMetadata
	spec_path:    string | null
}

// Structured error for AI consumption
#JsonError: {
	code:        string
	message:     string
	location?:   string | null
	fix_hint?:   string | null
	fix_command?: string | null
}

// Suggested follow-up command
#NextAction: {
	command: string
	reason:  string
}

// Metadata included in all JSON responses
#JsonMetadata: {
	timestamp:      string
	version:        string
	exit_code:      int
	correlation_id: string
	duration_ms:    int
}

// Common severity levels
#Severity: "low" | "medium" | "high" | "critical"

// Common lint severity levels
#LintSeverity: "error" | "warning" | "info"

// Common status types
#HealthStatus: "ok" | "warning" | "error"

// Common gap types
#GapType: "inversion" | "second_order" | "checklist" | "coverage" | "security"

// Effect severity levels
#EffectSeverity: "info" | "warning" | "danger" | "critical"

// Effect categories
#EffectCategory: "resource_lifecycle" | "data_integrity" | "system_state" | "security_implication" | "performance_impact" | "external_dependency"
