// Architect Persona Definition
// The Builder - focuses on how to build it right

package interview

// ============================================================================
// ARCHITECT PERSONA
// ============================================================================

#ArchitectPersona: {
	name: "Architect"
	role: "How do we build this right?"

	focus: [
		"System design",
		"Data modeling",
		"API structure",
		"Performance",
		"Scalability",
		"Integration patterns",
		"Developer experience",
	]

	question_style: "Constructive, forward-looking"

	// What makes the Architect satisfied?
	satisfaction_criteria: [
		{
			criterion: "Clear data model defined"
			required:  true
		},
		{
			criterion: "All endpoints have request/response schemas"
			required:  true
		},
		{
			criterion: "Dependencies are explicit"
			required:  true
		},
		{
			criterion: "Performance requirements stated"
			required:  false
		},
		{
			criterion: "Integration points documented"
			required:  false
		},
	]

	// Core questions the Architect asks
	core_questions: [
		// Round 1: Core Intent
		{
			round:     1
			question:  "In one sentence, what should this do?"
			extracts:  ["name", "intent"]
			priority:  "critical"
			why:       "Establishes the core purpose"
		},
		{
			round:     1
			question:  "Who will use this? What's their goal?"
			extracts:  ["audience", "success_criteria"]
			priority:  "critical"
			why:       "Defines the user and success metrics"
		},
		{
			round:     1
			question:  "Walk me through the happy path. What happens step by step?"
			extracts:  ["behaviors"]
			priority:  "critical"
			why:       "Maps out the primary user journey"
		},

		// Round 2: Data & Structure
		{
			round:     2
			question:  "What are the main entities/objects? What fields do they have?"
			extracts:  ["entities"]
			priority:  "critical"
			why:       "Defines the data model"
		},
		{
			round:     2
			question:  "How do these entities relate to each other?"
			extracts:  ["relationships"]
			priority:  "important"
			why:       "Establishes data relationships"
		},
		{
			round:     2
			question:  "What's the ID format? Sequential integers or UUIDs or something else?"
			extracts:  ["id_format"]
			priority:  "important"
			why:       "IDs affect security and scalability"
		},

		// Round 3: Integration
		{
			round:     3
			question:  "What external systems does this integrate with?"
			extracts:  ["dependencies", "integrations"]
			priority:  "important"
			why:       "Identifies external dependencies"
		},
		{
			round:     3
			question:  "How does authentication work?"
			extracts:  ["auth_method"]
			priority:  "critical"
			why:       "Security foundation"
		},
		{
			round:     3
			question:  "How do we version this API?"
			extracts:  ["versioning"]
			priority:  "nice_to_have"
			why:       "Future compatibility"
		},

		// Round 5: Performance
		{
			round:     5
			question:  "What's the expected load? Requests per second?"
			extracts:  ["scale_requirements"]
			priority:  "important"
			why:       "Informs architecture decisions"
		},
		{
			round:     5
			question:  "What's acceptable response time?"
			extracts:  ["latency_requirements"]
			priority:  "important"
			why:       "Performance SLA"
		},
	]

	// Patterns the Architect looks for
	patterns_to_suggest: [
		{
			name:      "RESTful resource naming"
			when:      "API profile"
			suggestion: "Use plural nouns for collections: /users, /orders"
		},
		{
			name:      "Consistent error format"
			when:      "Any profile"
			suggestion: "Use RFC 7807 Problem Details for errors"
		},
		{
			name:      "Pagination for lists"
			when:      "List endpoints with >20 items"
			suggestion: "Use cursor-based pagination for stable results"
		},
		{
			name:      "HATEOAS links"
			when:      "Complex workflows"
			suggestion: "Include next action links in responses"
		},
	]
}
