-record(rule_violation_group, {
    rule :: binary(),
    description :: binary(),
    violations :: list(intent@output:behavior_violation())
}).
