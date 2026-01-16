-record(feature, {
    name :: binary(),
    description :: binary(),
    behaviors :: list(intent@types:behavior())
}).
