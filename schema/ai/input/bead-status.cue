// Bead-status command input schema
#BeadStatusInput: {
	bead_id: string // required
	status:  string // "success", "failed", or "blocked"
	reason?: string // required for "blocked" status
	session?: string // session ID
}
