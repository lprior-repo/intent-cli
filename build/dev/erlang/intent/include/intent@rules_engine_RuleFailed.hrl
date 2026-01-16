-record(rule_failed, {
    rule_name :: binary(),
    description :: binary(),
    violations :: list(intent@rules_engine:rule_violation())
}).
