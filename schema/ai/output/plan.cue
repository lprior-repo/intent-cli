// Plan command output schema
package output

#PlanOutput: #BaseResponse & {
	action:  "plan_result"
	command: "plan"
	data: {
		session_id:   string
		health:       #KIRKHealth
		waves:        [...#Wave]
		beads:        [...#BeadRecord]
		total_beads:  int
		estimated_time_minutes: int
	}
}

#KIRKHealth: {
	overall_score:     int
	coverage_score:    int
	clarity_score:     int
	testability_score: int
	ai_readiness_score: int
	gaps:              [...string]
	inversion_gaps:    [...string]
}

#Wave: {
	number:     int
	beads:      [...string]
	parallelizable: bool
	risk_level: "low" | "medium" | "high"
	estimated_minutes: int
}
