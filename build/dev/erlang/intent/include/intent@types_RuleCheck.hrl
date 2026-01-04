-record(rule_check, {
    body_must_not_contain :: gleam@option:option(list(binary())),
    body_must_contain :: gleam@option:option(list(binary())),
    fields_must_exist :: gleam@option:option(list(binary())),
    fields_must_not_exist :: gleam@option:option(list(binary())),
    header_must_exist :: gleam@option:option(binary()),
    header_must_not_exist :: gleam@option:option(binary())
}).
