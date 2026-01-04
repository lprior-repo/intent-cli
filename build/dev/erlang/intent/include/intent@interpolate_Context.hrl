-record(context, {
    variables :: gleam@dict:dict(binary(), gleam@json:json()),
    request_body :: gleam@option:option(gleam@json:json()),
    response_body :: gleam@option:option(gleam@json:json())
}).
