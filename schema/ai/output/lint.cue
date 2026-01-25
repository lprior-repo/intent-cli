// Lint command output schema
package output

#LintOutput: #BaseResponse & {
	action:  "lint_result"
	command: "lint"
	data: {
		status:   "valid" | "warnings"
		warnings: [...#LintWarning]
	}
}

#LintWarning: {
	severity: #LintSeverity
	category: "anti_pattern" | "vague_rule" | "missing_example" | "unused_anti_pattern" | "naming_convention" | "duplicate_behavior"
	message:  string
	location: {
		behavior?:     string
		field?:        string
		anti_pattern?: string
		behavior1?:    string
		behavior2?:    string
	}
}
