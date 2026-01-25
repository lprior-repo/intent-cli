// Gaps command output schema
package output

#GapsOutput: #BaseResponse & {
	action:  "gaps_report"
	command: "gaps"
	data: {
		total_gaps:         int
		inversion_gaps:     [...#Gap]
		second_order_gaps:  [...#Gap]
		checklist_gaps:     [...#Gap]
		coverage_gaps:      [...#Gap]
		security_gaps:      [...#Gap]
		severity_breakdown: #SeverityBreakdown
	}
}

#Gap: {
	gap_type:    #GapType
	description: string
	severity:    #Severity
	suggestion:  string
	mental_model: string
}

#SeverityBreakdown: {
	critical: int
	high:     int
	medium:   int
	low:      int
}
