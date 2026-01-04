-record(rule, {
    name :: binary(),
    description :: binary(),
    'when' :: gleam@option:option(intent@types:'when'()),
    check :: intent@types:rule_check(),
    example :: gleam@option:option(gleam@json:json())
}).
