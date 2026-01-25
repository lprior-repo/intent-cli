// Plan-approve command output schema
package output

#PlanApproveOutput: #BaseResponse & {
	action:  "plan_approve_result"
	command: "plan-approve"
	data: {
		session_id:  string
		approved:    bool
		approved_at: string
		notes?:      string
		waves:       int
		total_beads: int
	}
}
