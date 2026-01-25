// Parse command output schema
package output

#ParseOutput: #BaseResponse & {
	action:  "parse_result"
	command: "parse"
	data: {
		valid:        bool
		requirements: [...#ParsedRequirement]
		summary: {
			total:  int
			valid:  int
			errors: int
		}
		errors?: [...string]
	}
}

#ParsedRequirement: {
	line:    int
	text:    string
	pattern: string
	valid:   bool
	error?:  string
}
