-record(bead_stats, {
    total :: integer(),
    by_type :: gleam@dict:dict(binary(), integer()),
    by_priority :: gleam@dict:dict(integer(), integer())
}).
