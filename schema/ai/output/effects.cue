// Effects command output schema
package output

#EffectsOutput: #BaseResponse & {
	action:  "effects_report"
	command: "effects"
	data: {
		total_second_order_effects: int
		coverage_score:             number
		behavior_effects:           [...#BehaviorEffects]
		orphaned_resources:         [...#OrphanedResource]
		cascade_warnings:           [...#CascadeWarning]
		state_dependencies:         [...#StateDependency]
	}
}

#BehaviorEffects: {
	behavior_name:         string
	first_order:           string
	second_order:          [...#SecondOrderEffect]
	missing_verifications: [...string]
}

#SecondOrderEffect: {
	description:      string
	severity:         #EffectSeverity
	category:         #EffectCategory
	has_verification: bool
}

#OrphanedResource: {
	resource_type: string
	caused_by:     string
	description:   string
	mitigation:    string
}

#CascadeWarning: {
	operation:            string
	cascades_to:          [...string]
	requires_transaction: bool
	description:          string
}

#StateDependency: {
	behavior:        string
	depends_on:      [...string]
	state_mutations: [...string]
	isolation_level: string
}
