-record(rule, {
    name :: binary(),
    description :: binary(),
    'when' :: intent@types:'when'(),
    check :: intent@types:rule_check(),
    example :: gleam@json:json()
}).
