-record(execution_result, {
    status :: integer(),
    headers :: gleam@dict:dict(binary(), binary()),
    body :: gleam@json:json(),
    raw_body :: binary(),
    elapsed_ms :: integer(),
    request_method :: intent@types:method(),
    request_path :: binary()
}).
