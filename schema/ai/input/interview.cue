// Interview command input schema
#InterviewInput: {
	profile?:  string // "api", "cli", "event", "data", "workflow", "ui"
	resume?:   string // resume session ID
	session?:  string // session ID for answering
	answer?:   string // answer text
	dry_run?:  bool   // dry run mode
	batch?:    bool   // batch mode
	input?:    string // input file path for batch mode
	export?:   string // export path
}
