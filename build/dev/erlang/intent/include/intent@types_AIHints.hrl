-record(a_i_hints, {
    implementation :: gleam@option:option(intent@types:implementation_hints()),
    entities :: gleam@dict:dict(binary(), intent@types:entity_hint()),
    security :: gleam@option:option(intent@types:security_hints()),
    pitfalls :: list(binary())
}).
