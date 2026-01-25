// Feedback command output schema
package output

#FeedbackOutput: #BaseResponse & {
	action:  "feedback_result"
	command: "feedback"
	data: {
		source_file:  string
		fix_beads:    [...#FixBead]
		total_fixes:  int
		behaviors_analyzed: int
	}
}

#FixBead: {
	behavior_name: string
	feature:       string
	failure_type:  "check_failed" | "status_mismatch" | "connection_error" | "timeout"
	description:   string
	priority:      int
	fix_suggestion: string
	related_checks: [...string]
}
