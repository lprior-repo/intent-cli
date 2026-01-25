// Quality command output schema
package output

#QualityOutput: #BaseResponse & {
	action:  "quality_report"
	command: "quality"
	data: {
		overall_score:      int
		coverage_score:     int
		clarity_score:      int
		testability_score:  int
		ai_readiness_score: int
		issues:             [...string]
		suggestions:        [...string]
	}
}
