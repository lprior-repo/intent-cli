-record(request, {
    method :: intent@types:method(),
    path :: binary(),
    headers :: gleam@dict:dict(binary(), binary()),
    'query' :: gleam@dict:dict(binary(), gleam@json:json()),
    body :: gleam@option:option(gleam@json:json())
}).
