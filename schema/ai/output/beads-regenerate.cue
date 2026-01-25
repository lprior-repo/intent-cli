// Beads-regenerate command output schema
package output

#BeadsRegenerateOutput: #BaseResponse & {
	action:  "beads_regenerate_result"
	command: "beads-regenerate"
	data: {
		spec_path:     string
		beads:         [...#BeadRecord]
		total:         int
		regenerated:   int
		source:        "spec" | "quality" | "gaps"
	}
}
