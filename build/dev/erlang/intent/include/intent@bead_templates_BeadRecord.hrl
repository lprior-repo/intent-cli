-record(bead_record, {
    title :: binary(),
    description :: binary(),
    profile_type :: binary(),
    priority :: integer(),
    issue_type :: binary(),
    labels :: list(binary()),
    ai_hints :: binary(),
    acceptance_criteria :: list(binary()),
    dependencies :: list(binary())
}).
