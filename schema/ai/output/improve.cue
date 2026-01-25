// Improve command output schema
package output

#ImproveOutput: #BaseResponse & {
	action:  "improve_result"
	command: "improve"
	data: {
		suggestions: [...#ImprovementSuggestion]
		total_count: int
	}
}

#ImprovementSuggestion: {
	title:        string
	description:  string
	reasoning:    string
	impact_score: int
	proposed_change: {
		type:             "add_missing_test" | "refine_vague_rule" | "add_response_example" | "rename_for_clarity" | "simplify_rule" | "add_explanation"
		behavior_name?:   string
		field?:           string
		old_name?:        string
		new_name?:        string
		test_description?: string
		better_rule?:     string
		simpler_rule?:    string
		explanation?:     string
	}
}
