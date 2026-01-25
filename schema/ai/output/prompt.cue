// Prompt command output schema
package output

#PromptOutput: #BaseResponse & {
	action:  "prompt_result"
	command: "prompt"
	data: {
		session_id: string
		prompts:    [...#ImplementationPrompt]
		total:      int
	}
}

#ImplementationPrompt: {
	bead_id:             string
	task_summary:        string
	context_section:     string
	requirements:        [...string]
	acceptance_criteria: [...string]
	relevant_code:       [...#FileContext]
	suggested_approach:  string
	pitfalls_to_avoid:   [...string]
	guardrail_block:     string
	verification_steps:  [...string]
}

#FileContext: {
	path:            string
	language:        string
	purpose:         string
	content_snippet?: string
	relevant_lines?: [...#LineReference]
}

#LineReference: {
	line_number: int
	content:     string
	reason:      string
}
