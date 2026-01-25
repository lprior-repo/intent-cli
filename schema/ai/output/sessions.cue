// Sessions command output schema
package output

#SessionsOutput: #BaseResponse & {
	action:  "sessions_list"
	command: "sessions"
	data: {
		sessions: [...#SessionSummary]
		total:    int
	}
}

#SessionSummary: {
	id:          string
	profile:     "api" | "cli" | "event" | "data" | "workflow" | "ui"
	status:      "active" | "completed" | "paused"
	created_at:  string
	updated_at:  string
	round:       int
	answers:     int
	description: string
}
