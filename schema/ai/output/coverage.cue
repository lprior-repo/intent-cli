// Coverage command output schema
package output

#CoverageOutput: #BaseResponse & {
	action:  "coverage_report"
	command: "coverage"
	data: {
		overall_score: number
		methods: {
			[string]: int
		}
		status_codes: {
			[string]: int
		}
		paths: {
			[string]: [...string]
		}
		edge_cases: {
			tested:    [...string]
			suggested: [...string]
		}
		owasp: {
			score: number
			categories: {
				[string]: bool
			}
			missing: [...string]
		}
	}
}
