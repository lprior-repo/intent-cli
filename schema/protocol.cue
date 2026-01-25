package protocol

import "github.com/intent-cli/intent/schema/common"

// Re-export common envelope types for backward compatibility
#Request: common.#Request
#Response: common.#Response
#Error: common.#Error

// --- Command Definitions ---

// command: "check"
#CheckParams: {
	spec: string
	target?: string
}

// command: "validate"
#ValidateParams: {
	spec: string
}

// command: "quality"
#QualityParams: {
	spec: string
}

// command: "sys.schema"
#SchemaParams: {}

