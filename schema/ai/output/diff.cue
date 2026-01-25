// Diff command output schema
package output

#DiffOutput: #BaseResponse & {
	action:  "diff_result"
	command: "diff"
	data: {
		has_changes:              bool
		name_changed?:            #StringChange
		description_changed?:     #StringChange
		version_changed?:         #StringChange
		config_changes:           [...#ConfigChange]
		feature_changes:          [...#FeatureChange]
		behavior_changes:         [...#BehaviorChange]
		rule_changes:             [...#RuleChange]
		anti_pattern_changes:     [...#AntiPatternChange]
		success_criteria_changes: #ListChange
	}
}

#StringChange: {
	old: string
	new: string
}

#ListChange: {
	added:   [...string]
	removed: [...string]
}

#ConfigChange: {
	type: "base_url_changed" | "timeout_changed" | "allow_localhost_changed" | "headers_changed"
	old?:      _
	new?:      _
	added?:    [...string]
	removed?:  [...string]
	modified?: [...string]
}

#FeatureChange: {
	type:               "added" | "removed" | "modified"
	name:               string
	behavior_count?:    int
	description_changed?: #StringChange
	behavior_changes?:  [...#BehaviorChange]
}

#BehaviorChange: {
	type:          "added" | "removed" | "modified"
	feature:       string
	name:          string
	intent?:       string
	modifications?: [...#BehaviorModification]
}

#BehaviorModification: {
	field:    string
	old?:     _
	new?:     _
	changes?: [...]
	added?:   [...string]
	removed?: [...string]
}

#RuleChange: {
	type:          "added" | "removed" | "modified"
	name:          string
	description?:  string
	modifications?: [...#RuleModification]
}

#RuleModification: {
	field:    string
	old?:     string
	new?:     string
	subfield?: string
	changed?: bool
}

#AntiPatternChange: {
	type:           "added" | "removed" | "modified"
	name:           string
	description?:   string
	fields_changed?: [...string]
}
