-record(question, {
    id :: binary(),
    round :: integer(),
    perspective :: intent@interview_questions:perspective(),
    category :: intent@interview_questions:question_category(),
    priority :: intent@interview_questions:question_priority(),
    question :: binary(),
    context :: binary(),
    example :: binary(),
    expected_type :: binary(),
    extract_into :: list(binary()),
    depends_on :: list(binary()),
    blocks :: list(binary())
}).
