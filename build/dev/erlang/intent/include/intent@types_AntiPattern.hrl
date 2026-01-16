-record(anti_pattern, {
    name :: binary(),
    description :: binary(),
    bad_example :: gleam@json:json(),
    good_example :: gleam@json:json(),
    why :: binary()
}).
