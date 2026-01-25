// Compact command input schema
// Converts CUE specs to Compact Intent Notation (CIN) for token efficiency
#CompactInput: {
	spec_path: string
	output?:   string // output file path (default: stdout)
}
