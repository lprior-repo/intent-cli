// Show command output schema
package output

#ShowOutput: #BaseResponse & {
	action:  "show_result"
	command: "show"
	data: {
		spec: {
			name:             string
			description:      string
			version:          string
			audience:         string
			success_criteria: [...string]
			config: {
				base_url:        string
				timeout_ms:      int
				allow_localhost: bool
				headers: {
					[string]: string
				}
			}
			features:      [...#FeatureSummary]
			rules:         [...#RuleSummary]
			anti_patterns: [...#AntiPatternSummary]
			ai_hints: {
				implementation: {
					suggested_stack:    [...string]
					architecture_notes: string
				}
				entities:  [...string]
				security:  [...string]
				pitfalls:  [...string]
			}
		}
		summary: {
			total_features:      int
			total_behaviors:     int
			total_rules:         int
			total_anti_patterns: int
		}
	}
}

#FeatureSummary: {
	name:           string
	description:    string
	behavior_count: int
	behaviors:      [...string]
}

#RuleSummary: {
	name:        string
	description: string
}

#AntiPatternSummary: {
	name:        string
	description: string
	why:         string
}
