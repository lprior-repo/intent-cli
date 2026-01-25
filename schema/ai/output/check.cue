// Check command output schema
package output

#CheckOutput: #BaseResponse & {
	action:  "check_result"
	command: "check"
	data: {
		total:       int
		passed:      int
		failed:      int
		skipped:     int
		success:     bool
		duration_ms: int
		behaviors: [...#BehaviorResult]
	}
}

#BehaviorResult: {
	name:     string
	feature:  string
	status:   "passed" | "failed" | "skipped"
	duration_ms: int
	request: {
		method:  string
		path:    string
		url:     string
	}
	response: {
		status: int
		body?:  _
	}
	checks: [...#CheckResult]
	error?: string
}

#CheckResult: {
	field:    string
	rule:     string
	expected: _
	actual:   _
	passed:   bool
	why:      string
}
