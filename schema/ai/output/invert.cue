// Invert command output schema
package output

#InvertOutput: #BaseResponse & {
	action:  "inversion_report"
	command: "invert"
	data: {
		score:               number
		security_gaps:       [...#InversionGap]
		usability_gaps:      [...#InversionGap]
		integration_gaps:    [...#InversionGap]
		suggested_behaviors: [...#SuggestedBehavior]
	}
}

#InversionGap: {
	category:        string
	description:     string
	severity:        #Severity
	what_could_fail: string
}

#SuggestedBehavior: {
	name:            string
	intent:          string
	method:          string
	path:            string
	expected_status: int
	category:        string
}
