-record(behavior, {
    name :: binary(),
    intent :: binary(),
    notes :: binary(),
    requires :: list(binary()),
    tags :: list(binary()),
    request :: intent@types:request(),
    response :: intent@types:response(),
    captures :: gleam@dict:dict(binary(), binary())
}).
