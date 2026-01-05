-record(answer, {
    question_id :: binary(),
    question_text :: binary(),
    perspective :: intent@interview_questions:perspective(),
    round :: integer(),
    response :: binary(),
    extracted :: gleam@dict:dict(binary(), binary()),
    confidence :: float(),
    notes :: binary(),
    timestamp :: binary()
}).
