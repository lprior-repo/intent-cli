-record(security_hints, {
    password_hashing :: binary(),
    jwt_algorithm :: binary(),
    jwt_expiry :: binary(),
    rate_limiting :: binary()
}).
