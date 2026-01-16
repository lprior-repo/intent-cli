-record(rule_check, {
    body_must_not_contain :: list(binary()),
    body_must_contain :: list(binary()),
    fields_must_exist :: list(binary()),
    fields_must_not_exist :: list(binary()),
    header_must_exist :: binary(),
    header_must_not_exist :: binary()
}).
