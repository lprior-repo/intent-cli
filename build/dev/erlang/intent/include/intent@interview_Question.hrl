-record(question, {
    id :: binary(),
    round :: integer(),
    perspective :: intent@interview:perspective(),
    category :: intent@interview:question_category(),
    priority :: intent@interview:question_priority(),
    question :: binary(),
    context :: binary(),
    example :: binary(),
    expected_type :: binary(),
    extract_into :: list(binary()),
    depends_on :: list(binary()),
    blocks :: list(binary())
}).
