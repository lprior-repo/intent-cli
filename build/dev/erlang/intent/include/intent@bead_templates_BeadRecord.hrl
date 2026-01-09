-record(bead_record, {
    title :: binary(),
    description :: binary(),
    profile_type :: binary(),
    priority :: integer(),
    issue_type :: binary(),
    labels :: list(binary()),
    ai_hints :: binary(),
    acceptance_criteria :: list(binary()),
    dependencies :: list(binary()),
    input_example :: binary(),
    output_example :: binary(),
    must_return :: list(binary()),
    must_not :: list(binary()),
    edge_cases :: list(binary())
}).
