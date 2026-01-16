-record(store, {
    pokemon :: gleam@dict:dict(binary(), pokemon_api:pokemon()),
    trainers :: gleam@dict:dict(binary(), pokemon_api:trainer()),
    next_id :: integer()
}).
