-record(security_hints, {
    password_hashing :: gleam@option:option(binary()),
    jwt_algorithm :: gleam@option:option(binary()),
    jwt_expiry :: gleam@option:option(binary()),
    rate_limiting :: gleam@option:option(binary())
}).
