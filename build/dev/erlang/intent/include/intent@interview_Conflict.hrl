-record(conflict, {
    id :: binary(),
    between :: {binary(), binary()},
    description :: binary(),
    impact :: binary(),
    options :: list(intent@interview:conflict_resolution()),
    chosen :: integer()
}).
