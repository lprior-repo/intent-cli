-record(gap, {
    id :: binary(),
    field :: binary(),
    description :: binary(),
    blocking :: boolean(),
    suggested_default :: binary(),
    why_needed :: binary(),
    round :: integer(),
    resolved :: boolean(),
    resolution :: binary()
}).
