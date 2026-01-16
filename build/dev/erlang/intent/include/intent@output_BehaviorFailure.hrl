-record(behavior_failure, {
    feature :: binary(),
    behavior :: binary(),
    intent :: binary(),
    problems :: list(intent@output:problem()),
    request_sent :: intent@output:request_summary(),
    response_received :: intent@output:response_summary(),
    hint :: binary(),
    see_also :: list(binary())
}).
