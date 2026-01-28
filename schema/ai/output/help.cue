// Help command output schema
package output

#HelpOutput: #BaseResponse & {
	action:  "help_result" | "help_command" | "help_error"
	command: "help"
	data: #HelpData | #HelpCommandData | {}
}

#HelpData: {
	tool:           "intent"
	purpose:        string
	usage:          string
	output_format:  string
	exit_codes: {
		"0": string
		"1": string
		"3": string
		"4": string
	}
	workflow:       [...#WorkflowStep]
	commands:       [...#CommandEntry]
	total_commands: int & >=0
}

#HelpCommandData: {
	hint: string
}

#WorkflowStep: {
	step:        int & >=1
	description: string
	command:     string
}

#CommandEntry: {
	command:       string
	group:         "spec" | "kirk" | "interview" | "planning" | "phase" | "utility"
	args:          string
	flags:         string
	description:   string
	output_action: string
}
