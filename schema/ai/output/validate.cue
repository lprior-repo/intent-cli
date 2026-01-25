// Validate command output schema
package output

#ValidateOutput: #BaseResponse & {
	action:  "validate_result"
	command: "validate"
	data: {
		valid:   bool
		message: string
		spec?: {
			name:        string
			description: string
			version:     string
		}
	}
}
