-record(response, {
    status :: integer(),
    example :: gleam@option:option(gleam@json:json()),
    checks :: gleam@dict:dict(binary(), intent@types:check()),
    headers :: gleam@option:option(gleam@dict:dict(binary(), binary()))
}).
