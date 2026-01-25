// Beads command output schema
package output

#BeadsOutput: #BaseResponse & {
	action:  "beads_result"
	command: "beads"
	data: {
		session_id: string
		beads:      [...#BeadRecord]
		total:      int
	}
}

#BeadRecord: {
	title:               string
	description:         string
	profile_type:        string
	priority:            int
	issue_type:          string
	labels:              [...string]
	ai_hints:            string
	acceptance_criteria: [...string]
	dependencies:        [...string]
}
