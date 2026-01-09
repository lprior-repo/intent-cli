-record(response_check_result, {
    passed :: list(intent@checker:check_result()),
    failed :: list(intent@checker:check_result()),
    status_ok :: boolean(),
    status_expected :: integer(),
    status_actual :: integer()
}).
