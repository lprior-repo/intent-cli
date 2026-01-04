-record(spec_result, {
    pass :: boolean(),
    passed :: integer(),
    failed :: integer(),
    blocked :: integer(),
    total :: integer(),
    summary :: binary(),
    failures :: list(intent@output:behavior_failure()),
    blocked_behaviors :: list(intent@output:blocked_behavior()),
    rule_violations :: list(intent@output:rule_violation_group()),
    anti_patterns_detected :: list(intent@anti_patterns:anti_pattern_result())
}).
