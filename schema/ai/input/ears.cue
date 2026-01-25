// Ears command input schema
#EarsInput: {
	file_path: string
	output?:   string // "cue" or "json"
	out?:      string // output file path
	lang?:     string // language (default: "gleam")
}
