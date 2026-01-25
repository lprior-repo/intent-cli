// Interview command output schema
package output

#InterviewOutput: #BaseResponse & {
	action:  "interview_result"
	command: "interview"
	data: {
		session_id: string
		profile:    "api" | "cli" | "event" | "data" | "workflow" | "ui"
		status:     "active" | "completed" | "paused"
		round:      int
		total_rounds: int
		questions_answered: int
		current_question?: #InterviewQuestion
		progress_percentage: int
	}
}

#InterviewQuestion: {
	id:         string
	text:       string
	round:      int
	category:   string
	required:   bool
	hint?:      string
	examples?:  [...string]
}
