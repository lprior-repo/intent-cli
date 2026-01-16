-record(contextual_error, {
    behavior :: binary(),
    field_path :: binary(),
    rule :: binary(),
    expected :: binary(),
    actual :: binary(),
    available_fields :: list(binary()),
    suggestions :: list(binary()),
    explanation :: binary()
}).
