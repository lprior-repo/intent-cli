-record(behavior_violation, {
    behavior :: binary(),
    violations :: list(binary()),
    response :: gleam@option:option(gleam@json:json())
}).
