// Export command output schema
package output

#ExportOutput: #BaseResponse & {
	action:  "export_result"
	command: "export"
	data: {
		session_id:  string
		output_path: string
		format:      "cue" | "json"
		success:     bool
		spec_preview?: string
	}
}
