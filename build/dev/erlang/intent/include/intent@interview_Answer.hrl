-record(answer, {
    question_id :: binary(),
    question_text :: binary(),
    perspective :: intent@question_types:perspective(),
    round :: integer(),
    response :: binary(),
    extracted :: gleam@dict:dict(binary(), binary()),
    confidence :: float(),
    notes :: binary(),
    timestamp :: binary()
}).
