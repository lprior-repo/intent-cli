-record(a_i_hints, {
    implementation :: intent@types:implementation_hints(),
    entities :: gleam@dict:dict(binary(), intent@types:entity_hint()),
    security :: intent@types:security_hints(),
    pitfalls :: list(binary())
}).
