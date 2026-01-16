-record(quality_report, {
    coverage_score :: integer(),
    clarity_score :: integer(),
    testability_score :: integer(),
    ai_readiness_score :: integer(),
    overall_score :: integer(),
    issues :: list(intent@quality_analyzer:quality_issue()),
    suggestions :: list(binary())
}).
