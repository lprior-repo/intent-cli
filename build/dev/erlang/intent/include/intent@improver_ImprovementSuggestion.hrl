-record(improvement_suggestion, {
    title :: binary(),
    description :: binary(),
    reasoning :: binary(),
    impact_score :: integer(),
    proposed_change :: intent@improver:proposed_change()
}).
