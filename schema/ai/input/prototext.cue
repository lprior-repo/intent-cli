// Prototext command input schema
// Converts CUE specs to Protocol Buffer text format
#PrototextInput: {
	spec_path: string
	output?:   string // output file path (default: stdout)
}
