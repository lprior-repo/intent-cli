// Bead-status command output schema
package output

#BeadStatusOutput: #BaseResponse & {
	action:  "bead_status_result"
	command: "bead-status"
	data: {
		bead_id:        string
		previous_status: string
		new_status:     "success" | "failed" | "blocked"
		reason?:        string
		updated_at:     string
	}
}
