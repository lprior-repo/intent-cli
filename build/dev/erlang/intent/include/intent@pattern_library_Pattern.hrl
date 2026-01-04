-record(pattern, {
    name :: binary(),
    description :: binary(),
    category :: binary(),
    frequency :: integer(),
    behaviors :: list(intent@types:behavior())
}).
