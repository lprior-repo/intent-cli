-record(validation_error, {
    behavior :: binary(),
    failures :: list(intent@errors:field_failure())
}).
