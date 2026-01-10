// Minimal Intent Schema
// This is the DIRT PATH - the absolute minimum to start
// We'll pave more structure as we see where users walk

package intent

// A conversation that becomes a spec
#Session: {
	// What did the user say they want?
	prompt: string

	// The back-and-forth
	conversation: [...#Exchange]

	// What emerged from the conversation
	spec?: #Spec
}

#Exchange: {
	persona:  "architect" | "adversary"
	question: string
	answer:   string

	// What we learned from this exchange
	extracted?: _
}

// The spec that emerges - shape is FLEXIBLE
// Don't over-specify until we see what people need
#Spec: {
	name:   string
	intent: string

	// Everything else is optional and will take shape
	// based on what the conversation produces
	...
}
