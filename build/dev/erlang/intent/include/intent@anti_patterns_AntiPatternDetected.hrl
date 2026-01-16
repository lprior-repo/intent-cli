-record(anti_pattern_detected, {
    pattern_name :: binary(),
    description :: binary(),
    found :: binary(),
    bad_example :: gleam@json:json(),
    good_example :: gleam@json:json()
}).
