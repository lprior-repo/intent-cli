-record(response, {
    status :: integer(),
    example :: gleam@json:json(),
    checks :: gleam@dict:dict(binary(), intent@types:check()),
    headers :: gleam@dict:dict(binary(), binary())
}).
