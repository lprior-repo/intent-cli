-record(spec, {
    name :: binary(),
    description :: binary(),
    audience :: binary(),
    version :: binary(),
    success_criteria :: list(binary()),
    config :: intent@types:config(),
    features :: list(intent@types:feature()),
    rules :: list(intent@types:rule()),
    anti_patterns :: list(intent@types:anti_pattern()),
    ai_hints :: intent@types:a_i_hints()
}).
