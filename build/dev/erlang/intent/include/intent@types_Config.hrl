-record(config, {
    base_url :: binary(),
    timeout_ms :: integer(),
    headers :: gleam@dict:dict(binary(), binary())
}).
