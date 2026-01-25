// Plan-approve command input schema
#PlanApproveInput: {
	session_id: string
	yes?:       bool   // auto-approve for CI
	notes?:     string // approval notes
}
