// History command output schema
package output

#HistoryOutput: #BaseResponse & {
	action:  "history_result"
	command: "history"
	data: {
		snapshots: [...#SessionSnapshot]
		total:     int
	}
}

#SessionSnapshot: {
	id:          string
	session_id:  string
	timestamp:   string
	round:       int
	question:    string
	answer:      string
	category:    string
}
