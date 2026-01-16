-record(interview_session, {
    id :: binary(),
    profile :: intent@interview:profile(),
    created_at :: binary(),
    updated_at :: binary(),
    completed_at :: binary(),
    stage :: intent@interview:interview_stage(),
    rounds_completed :: integer(),
    answers :: list(intent@interview:answer()),
    gaps :: list(intent@interview:gap()),
    conflicts :: list(intent@interview:conflict()),
    raw_notes :: binary()
}).
