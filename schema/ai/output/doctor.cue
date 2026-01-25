// Doctor command output schema
package output

#DoctorOutput: #BaseResponse & {
	action:  "doctor_report"
	command: "doctor"
	data: {
		quality: {
			overall_score:      int
			coverage_score:     int
			clarity_score:      int
			testability_score:  int
			ai_readiness_score: int
			issues:             [...string]
		}
		lint: {
			status:   "valid" | "warnings"
			warnings: [...#LintWarning]
		}
		suggestions: [...#DoctorSuggestion]
	}
}

#DoctorSuggestion: {
	title:        string
	description:  string
	reasoning:    string
	impact_score: int
}

// Health report variant of doctor output
#HealthReportOutput: #BaseResponse & {
	action:  "health_report"
	command: "doctor"
	data: {
		checks: [...#HealthCheck]
	}
}

#HealthCheck: {
	name:    string
	status:  #HealthStatus
	message: string
	details: string
}
