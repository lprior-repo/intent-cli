// EARS command output schema
package output

#EarsOutput: #BaseResponse & {
	action:  "ears_result"
	command: "ears"
	data: {
		requirements: [...#EarsRequirement]
		summary: {
			total:      int
			ubiquitous: int
			event:      int
			state:      int
			unwanted:   int
			optional:   int
			complex:    int
		}
	}
}

#EarsRequirement: {
	id:          string
	text:        string
	pattern:     "ubiquitous" | "event" | "state" | "unwanted" | "optional" | "complex"
	trigger?:    string
	condition?:  string
	action:      string
	behavior?:   string
	source_line: int
}
