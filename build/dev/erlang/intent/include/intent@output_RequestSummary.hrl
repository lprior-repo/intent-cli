-record(request_summary, {
    method :: binary(),
    url :: binary(),
    headers :: gleam@dict:dict(binary(), binary())
}).
