-module(intent@interview).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([extract_from_answer/3, detect_gaps/2, detect_conflicts/1, calculate_confidence/3, format_question/1, create_session/3, add_answer/2, check_for_gaps/3, check_for_conflicts/2, complete_round/1, resolve_conflict/3, resolve_gap/3, get_blocking_gaps/1, get_unresolved_conflicts/1, can_proceed/1, profile_to_string/1, get_first_question_for_round/2, get_next_question_in_round/2, get_current_round/1, format_progress/1, string_to_profile/1]).
-export_type([profile/0, interview_stage/0, answer/0, gap/0, conflict/0, conflict_resolution/0, interview_session/0]).

-type profile() :: api | cli | event | data | workflow | ui.

-type interview_stage() :: discovery |
    refinement |
    validation |
    complete |
    paused.

-type answer() :: {answer,
        binary(),
        binary(),
        intent@question_types:perspective(),
        integer(),
        binary(),
        gleam@dict:dict(binary(), binary()),
        float(),
        binary(),
        binary()}.

-type gap() :: {gap,
        binary(),
        binary(),
        binary(),
        boolean(),
        binary(),
        binary(),
        integer(),
        boolean(),
        binary()}.

-type conflict() :: {conflict,
        binary(),
        {binary(), binary()},
        binary(),
        binary(),
        list(conflict_resolution()),
        integer()}.

-type conflict_resolution() :: {conflict_resolution,
        binary(),
        binary(),
        binary(),
        binary()}.

-type interview_session() :: {interview_session,
        binary(),
        profile(),
        binary(),
        binary(),
        binary(),
        interview_stage(),
        integer(),
        list(answer()),
        list(gap()),
        list(conflict()),
        binary()}.

-spec extract_auth_method(binary()) -> {ok, binary()} | {error, binary()}.
extract_auth_method(Text) ->
    Lower = gleam@string:lowercase(Text),
    case gleam_stdlib:contains_string(Lower, <<"jwt"/utf8>>) of
        true ->
            {ok, <<"jwt"/utf8>>};

        false ->
            case gleam_stdlib:contains_string(Lower, <<"oauth"/utf8>>) of
                true ->
                    {ok, <<"oauth"/utf8>>};

                false ->
                    case gleam_stdlib:contains_string(Lower, <<"session"/utf8>>) of
                        true ->
                            {ok, <<"session"/utf8>>};

                        false ->
                            case gleam_stdlib:contains_string(
                                Lower,
                                <<"api key"/utf8>>
                            ) of
                                true ->
                                    {ok, <<"api_key"/utf8>>};

                                false ->
                                    case gleam_stdlib:contains_string(
                                        Lower,
                                        <<"none"/utf8>>
                                    ) of
                                        true ->
                                            {ok, <<"none"/utf8>>};

                                        false ->
                                            {error,
                                                <<"Could not identify auth method"/utf8>>}
                                    end
                            end
                    end
            end
    end.

-spec extract_entities(binary()) -> {ok, binary()} | {error, binary()}.
extract_entities(Text) ->
    Words = gleam@string:split(Text, <<" "/utf8>>),
    Entities = gleam@list:filter_map(
        Words,
        fun(Word) ->
            Clean_word = case gleam@string:ends_with(Word, <<","/utf8>>) of
                true ->
                    gleam@string:slice(Word, 0, gleam@string:length(Word) - 1);

                false ->
                    Word
            end,
            First_char = gleam@string:slice(Clean_word, 0, 1),
            case (gleam@string:uppercase(First_char) =:= First_char) andalso (gleam@string:length(
                Clean_word
            )
            > 2) of
                true ->
                    {ok, Clean_word};

                false ->
                    {error, nil}
            end
        end
    ),
    case Entities of
        [] ->
            {error, <<"No entities found"/utf8>>};

        _ ->
            {ok, gleam@string:join(Entities, <<", "/utf8>>)}
    end.

-spec extract_audience(binary()) -> {ok, binary()} | {error, binary()}.
extract_audience(Text) ->
    Lower = gleam@string:lowercase(Text),
    case gleam_stdlib:contains_string(Lower, <<"mobile"/utf8>>) of
        true ->
            {ok, <<"mobile"/utf8>>};

        false ->
            case gleam_stdlib:contains_string(Lower, <<"web"/utf8>>) of
                true ->
                    {ok, <<"web"/utf8>>};

                false ->
                    case gleam_stdlib:contains_string(Lower, <<"api"/utf8>>) of
                        true ->
                            {ok, <<"api"/utf8>>};

                        false ->
                            case gleam_stdlib:contains_string(
                                Lower,
                                <<"cli"/utf8>>
                            ) of
                                true ->
                                    {ok, <<"cli"/utf8>>};

                                false ->
                                    case gleam_stdlib:contains_string(
                                        Lower,
                                        <<"internal"/utf8>>
                                    ) of
                                        true ->
                                            {ok, <<"internal"/utf8>>};

                                        false ->
                                            {error,
                                                <<"Could not identify audience"/utf8>>}
                                    end
                            end
                    end
            end
    end.

-spec simple_extract(binary(), binary()) -> {ok, binary()} | {error, binary()}.
simple_extract(Field, Text) ->
    case Field of
        <<"auth_method"/utf8>> ->
            extract_auth_method(Text);

        <<"entities"/utf8>> ->
            extract_entities(Text);

        <<"audience"/utf8>> ->
            extract_audience(Text);

        _ ->
            Trimmed = gleam@string:trim(Text),
            case gleam@string:length(Trimmed) > 0 of
                true ->
                    {ok, Trimmed};

                false ->
                    {error, <<"Empty response"/utf8>>}
            end
    end.

-spec extract_from_answer(binary(), binary(), list(binary())) -> gleam@dict:dict(binary(), binary()).
extract_from_answer(_, Response, Extract_fields) ->
    gleam@list:fold(
        Extract_fields,
        gleam@dict:new(),
        fun(Acc, Field) ->
            Extracted_value = simple_extract(Field, Response),
            case Extracted_value of
                {ok, Value} ->
                    gleam@dict:insert(Acc, Field, Value);

                {error, _} ->
                    Acc
            end
        end
    ).

-spec detect_gaps(profile(), list(answer())) -> list(gap()).
detect_gaps(Profile, Answers) ->
    Required_fields = case Profile of
        api ->
            [<<"base_url"/utf8>>,
                <<"auth_method"/utf8>>,
                <<"happy_path"/utf8>>,
                <<"error_cases"/utf8>>,
                <<"response_format"/utf8>>];

        cli ->
            [<<"command_name"/utf8>>,
                <<"happy_path"/utf8>>,
                <<"help_text"/utf8>>,
                <<"exit_codes"/utf8>>];

        event ->
            [<<"event_type"/utf8>>,
                <<"payload_schema"/utf8>>,
                <<"trigger"/utf8>>];

        data ->
            [<<"data_model"/utf8>>,
                <<"access_patterns"/utf8>>,
                <<"retention"/utf8>>];

        workflow ->
            [<<"steps"/utf8>>, <<"happy_path"/utf8>>, <<"error_recovery"/utf8>>];

        ui ->
            [<<"user_flows"/utf8>>, <<"happy_path"/utf8>>, <<"states"/utf8>>]
    end,
    Answered_fields = gleam@list:fold(
        Answers,
        gleam@dict:new(),
        fun(Acc, Answer) ->
            gleam@list:fold(
                maps:to_list(erlang:element(7, Answer)),
                Acc,
                fun(Inner_acc, Pair) ->
                    gleam@dict:insert(
                        Inner_acc,
                        erlang:element(1, Pair),
                        erlang:element(2, Pair)
                    )
                end
            )
        end
    ),
    gleam@list:filter_map(
        Required_fields,
        fun(Field) -> case gleam@dict:get(Answered_fields, Field) of
                {ok, _} ->
                    {error, nil};

                {error, _} ->
                    {ok,
                        {gap,
                            <<"gap-"/utf8, Field/binary>>,
                            Field,
                            <<"Missing: "/utf8, Field/binary>>,
                            true,
                            <<""/utf8>>,
                            <<"Required for "/utf8, Field/binary>>,
                            0,
                            false,
                            <<""/utf8>>}}
            end end
    ).

-spec detect_conflicts(list(answer())) -> list(conflict()).
detect_conflicts(Answers) ->
    Lower_responses = gleam@list:map(
        Answers,
        fun(Ans) ->
            {erlang:element(2, Ans),
                gleam@string:lowercase(erlang:element(6, Ans))}
        end
    ),
    Conflicts = [],
    Has_fast = gleam@list:any(
        Lower_responses,
        fun(Pair) ->
            gleam_stdlib:contains_string(
                erlang:element(2, Pair),
                <<"fast"/utf8>>
            )
            orelse gleam_stdlib:contains_string(
                erlang:element(2, Pair),
                <<"latency"/utf8>>
            )
        end
    ),
    Has_consistent = gleam@list:any(
        Lower_responses,
        fun(Pair@1) ->
            gleam_stdlib:contains_string(
                erlang:element(2, Pair@1),
                <<"consistent"/utf8>>
            )
            orelse gleam_stdlib:contains_string(
                erlang:element(2, Pair@1),
                <<"accurate"/utf8>>
            )
        end
    ),
    Conflicts@1 = case Has_fast andalso Has_consistent of
        true ->
            lists:append(
                Conflicts,
                [{conflict,
                        <<"conflict-cap"/utf8>>,
                        {<<"latency"/utf8>>, <<"consistency"/utf8>>},
                        <<"You want both speed AND strong consistency"/utf8>>,
                        <<"CAP theorem: impossible to have both at scale"/utf8>>,
                        [{conflict_resolution,
                                <<"Prioritize latency"/utf8>>,
                                <<"Accept eventual consistency, cache aggressively"/utf8>>,
                                <<"Data may be stale for a few seconds"/utf8>>,
                                <<"Use when user experience matters more than instant accuracy"/utf8>>},
                            {conflict_resolution,
                                <<"Prioritize consistency"/utf8>>,
                                <<"Use strong consistency, accept higher latency"/utf8>>,
                                <<"P99 latencies may be 100-500ms, not 10ms"/utf8>>,
                                <<"Use for financial/banking systems"/utf8>>}],
                        -1}]
            );

        false ->
            Conflicts
    end,
    Has_anonymous = gleam@list:any(
        Lower_responses,
        fun(Pair@2) ->
            gleam_stdlib:contains_string(
                erlang:element(2, Pair@2),
                <<"anonymous"/utf8>>
            )
        end
    ),
    Has_audit = gleam@list:any(
        Lower_responses,
        fun(Pair@3) ->
            gleam_stdlib:contains_string(
                erlang:element(2, Pair@3),
                <<"audit"/utf8>>
            )
            orelse gleam_stdlib:contains_string(
                erlang:element(2, Pair@3),
                <<"log"/utf8>>
            )
        end
    ),
    case Has_anonymous andalso Has_audit of
        true ->
            lists:append(
                Conflicts@1,
                [{conflict,
                        <<"conflict-anon-audit"/utf8>>,
                        {<<"anonymous"/utf8>>, <<"audit"/utf8>>},
                        <<"You want anonymous users AND an audit trail"/utf8>>,
                        <<"Can't audit completely anonymous users without some tracking"/utf8>>,
                        [{conflict_resolution,
                                <<"Use pseudonymous IDs"/utf8>>,
                                <<"Generate per-session IDs, don't link to user identity"/utf8>>,
                                <<"Still traceable within a session"/utf8>>,
                                <<"Most balanced approach"/utf8>>},
                            {conflict_resolution,
                                <<"Aggregate logging only"/utf8>>,
                                <<"Log statistics (actions per hour) not individual actions"/utf8>>,
                                <<"Can't trace individual user behavior"/utf8>>,
                                <<"For privacy-critical systems"/utf8>>}],
                        -1}]
            );

        false ->
            Conflicts@1
    end.

-spec calculate_confidence(
    binary(),
    binary(),
    gleam@dict:dict(binary(), binary())
) -> float().
calculate_confidence(_, Response, Extracted) ->
    Response_length = gleam@string:length(gleam@string:trim(Response)),
    Field_count = maps:size(Extracted),
    case (Response_length > 50) andalso (Field_count > 0) of
        true ->
            0.85;

        false ->
            0.6
    end.

-spec format_question(intent@question_types:question()) -> binary().
format_question(Question) ->
    Priority_str = case erlang:element(6, Question) of
        critical ->
            <<"[CRITICAL]"/utf8>>;

        important ->
            <<"[IMPORTANT]"/utf8>>;

        nice_tohave ->
            <<""/utf8>>
    end,
    Context_str = case gleam@string:length(erlang:element(8, Question)) > 0 of
        true ->
            <<"\n"/utf8, (erlang:element(8, Question))/binary>>;

        false ->
            <<""/utf8>>
    end,
    Example_str = case gleam@string:length(erlang:element(9, Question)) > 0 of
        true ->
            <<"\nExample: "/utf8, (erlang:element(9, Question))/binary>>;

        false ->
            <<""/utf8>>
    end,
    <<<<<<<<Priority_str/binary, " "/utf8>>/binary,
                (erlang:element(7, Question))/binary>>/binary,
            Context_str/binary>>/binary,
        Example_str/binary>>.

-spec create_session(binary(), profile(), binary()) -> interview_session().
create_session(Id, Profile, Timestamp) ->
    {interview_session,
        Id,
        Profile,
        Timestamp,
        Timestamp,
        <<""/utf8>>,
        discovery,
        0,
        [],
        [],
        [],
        <<""/utf8>>}.

-spec add_answer(interview_session(), answer()) -> interview_session().
add_answer(Session, Answer) ->
    New_answers = lists:append(erlang:element(9, Session), [Answer]),
    erlang:setelement(
        5,
        erlang:setelement(9, Session, New_answers),
        erlang:element(10, Answer)
    ).

-spec get_answered_question_ids(interview_session()) -> list(binary()).
get_answered_question_ids(Session) ->
    gleam@list:map(
        erlang:element(9, Session),
        fun(Answer) -> erlang:element(2, Answer) end
    ).

-spec detect_blocking_gaps(intent@question_types:question(), answer()) -> list(gap()).
detect_blocking_gaps(Question, Answer) ->
    Response_length = gleam@string:length(
        gleam@string:trim(erlang:element(6, Answer))
    ),
    case erlang:element(6, Question) of
        critical when Response_length < 10 ->
            [{gap,
                    <<"gap_"/utf8, (erlang:element(2, Question))/binary>>,
                    erlang:element(7, Question),
                    <<"Critical question answered too briefly"/utf8>>,
                    true,
                    <<"Please provide a more detailed answer"/utf8>>,
                    <<"This is a critical requirement for spec generation"/utf8>>,
                    erlang:element(5, Answer),
                    false,
                    <<""/utf8>>}];

        _ ->
            []
    end.

-spec check_for_gaps(
    interview_session(),
    intent@question_types:question(),
    answer()
) -> {interview_session(), list(gap())}.
check_for_gaps(Session, Question, Answer) ->
    Blocking_gaps = detect_blocking_gaps(Question, Answer),
    Updated_session = erlang:setelement(
        10,
        Session,
        lists:append(erlang:element(10, Session), Blocking_gaps)
    ),
    {Updated_session, Blocking_gaps}.

-spec detect_answer_pair_conflicts(answer(), answer()) -> list(conflict()).
detect_answer_pair_conflicts(Answer1, Answer2) ->
    case {erlang:element(4, Answer1), erlang:element(4, Answer2)} of
        {developer, ops} ->
            Ans1_lower = gleam@string:lowercase(erlang:element(6, Answer1)),
            Ans2_lower = gleam@string:lowercase(erlang:element(6, Answer2)),
            case gleam_stdlib:contains_string(
                Ans1_lower,
                <<"consistency"/utf8>>
            )
            andalso gleam_stdlib:contains_string(
                Ans2_lower,
                <<"high latency"/utf8>>
            ) of
                true ->
                    [{conflict,
                            <<<<<<"cap_conflict_"/utf8,
                                        (erlang:element(2, Answer1))/binary>>/binary,
                                    "_"/utf8>>/binary,
                                (erlang:element(2, Answer2))/binary>>,
                            {erlang:element(2, Answer1),
                                erlang:element(2, Answer2)},
                            <<"Consistency vs. Availability tension (CAP theorem)"/utf8>>,
                            <<"Requires architectural decision on data replication strategy"/utf8>>,
                            [{conflict_resolution,
                                    <<"Prioritize Consistency"/utf8>>,
                                    <<"Strong consistency with potential latency"/utf8>>,
                                    <<"Higher latency, complex distributed coordination"/utf8>>,
                                    <<"Use when data correctness is critical (financial, medical)"/utf8>>},
                                {conflict_resolution,
                                    <<"Prioritize Availability"/utf8>>,
                                    <<"High availability with eventual consistency"/utf8>>,
                                    <<"Temporary data inconsistency, conflict resolution needed"/utf8>>,
                                    <<"Use for content, social feeds, non-critical data"/utf8>>}],
                            -1}];

                false ->
                    []
            end;

        {_, _} ->
            []
    end.

-spec detect_conflicts_in_session(interview_session(), answer()) -> list(conflict()).
detect_conflicts_in_session(Session, New_answer) ->
    Conflicts_found = gleam@list:fold(
        erlang:element(9, Session),
        [],
        fun(Acc, Existing) ->
            New_conflicts = detect_answer_pair_conflicts(Existing, New_answer),
            lists:append(Acc, New_conflicts)
        end
    ),
    Conflicts_found.

-spec check_for_conflicts(interview_session(), answer()) -> {interview_session(),
    list(conflict())}.
check_for_conflicts(Session, New_answer) ->
    Conflicts = detect_conflicts_in_session(Session, New_answer),
    Updated_session = erlang:setelement(
        11,
        Session,
        lists:append(erlang:element(11, Session), Conflicts)
    ),
    {Updated_session, Conflicts}.

-spec complete_round(interview_session()) -> interview_session().
complete_round(Session) ->
    New_stage = case erlang:element(8, Session) + 1 of
        1 ->
            discovery;

        2 ->
            discovery;

        3 ->
            refinement;

        4 ->
            validation;

        5 ->
            complete;

        _ ->
            complete
    end,
    erlang:setelement(
        7,
        erlang:setelement(8, Session, erlang:element(8, Session) + 1),
        New_stage
    ).

-spec resolve_conflict(interview_session(), binary(), integer()) -> {ok,
        interview_session()} |
    {error, binary()}.
resolve_conflict(Session, Conflict_id, Chosen_option) ->
    Updated_conflicts = gleam@list:map(
        erlang:element(11, Session),
        fun(Conflict) -> case erlang:element(2, Conflict) =:= Conflict_id of
                true ->
                    erlang:setelement(7, Conflict, Chosen_option);

                false ->
                    Conflict
            end end
    ),
    {ok, erlang:setelement(11, Session, Updated_conflicts)}.

-spec resolve_gap(interview_session(), binary(), binary()) -> interview_session().
resolve_gap(Session, Gap_id, Resolution) ->
    Updated_gaps = gleam@list:map(
        erlang:element(10, Session),
        fun(Gap) -> case erlang:element(2, Gap) =:= Gap_id of
                true ->
                    erlang:setelement(
                        10,
                        erlang:setelement(9, Gap, true),
                        Resolution
                    );

                false ->
                    Gap
            end end
    ),
    erlang:setelement(10, Session, Updated_gaps).

-spec get_blocking_gaps(interview_session()) -> list(gap()).
get_blocking_gaps(Session) ->
    gleam@list:filter(
        erlang:element(10, Session),
        fun(Gap) ->
            erlang:element(5, Gap) andalso not erlang:element(9, Gap)
        end
    ).

-spec get_unresolved_conflicts(interview_session()) -> list(conflict()).
get_unresolved_conflicts(Session) ->
    gleam@list:filter(
        erlang:element(11, Session),
        fun(Conflict) -> erlang:element(7, Conflict) =:= -1 end
    ).

-spec can_proceed(interview_session()) -> {ok, nil} | {error, binary()}.
can_proceed(Session) ->
    Blocking = get_blocking_gaps(Session),
    case Blocking of
        [] ->
            {ok, nil};

        Gaps ->
            Gap_descriptions = gleam@list:map(
                Gaps,
                fun(Gap) -> erlang:element(4, Gap) end
            ),
            {error,
                <<"Blocking gaps: "/utf8,
                    (gleam@string:join(Gap_descriptions, <<"; "/utf8>>))/binary>>}
    end.

-spec profile_to_string(profile()) -> binary().
profile_to_string(Profile) ->
    case Profile of
        api ->
            <<"api"/utf8>>;

        cli ->
            <<"cli"/utf8>>;

        event ->
            <<"event"/utf8>>;

        data ->
            <<"data"/utf8>>;

        workflow ->
            <<"workflow"/utf8>>;

        ui ->
            <<"ui"/utf8>>
    end.

-spec get_first_question_for_round(interview_session(), integer()) -> {ok,
        intent@question_types:question()} |
    {error, binary()}.
get_first_question_for_round(Session, Round) ->
    Profile_str = profile_to_string(erlang:element(3, Session)),
    Questions = intent@interview_questions:get_questions_for_round(
        Profile_str,
        Round
    ),
    case Questions of
        [] ->
            {error,
                <<"No questions found for round "/utf8,
                    (gleam@string:inspect(Round))/binary>>};

        [First | _] ->
            {ok, First}
    end.

-spec get_next_question_in_round(interview_session(), integer()) -> {ok,
        intent@question_types:question()} |
    {error, binary()}.
get_next_question_in_round(Session, Round) ->
    Profile_str = profile_to_string(erlang:element(3, Session)),
    Answered_ids = get_answered_question_ids(Session),
    case intent@interview_questions:get_next_question(
        Profile_str,
        Round,
        Answered_ids
    ) of
        {some, Question} ->
            {ok, Question};

        none ->
            {error,
                <<"No more unanswered questions in round "/utf8,
                    (gleam@string:inspect(Round))/binary>>}
    end.

-spec get_current_round(interview_session()) -> integer().
get_current_round(Session) ->
    case erlang:element(9, Session) of
        [] ->
            1;

        Answers ->
            Max_round = gleam@list:fold(
                Answers,
                0,
                fun(Acc, Answer) -> case erlang:element(5, Answer) > Acc of
                        true ->
                            erlang:element(5, Answer);

                        false ->
                            Acc
                    end end
            ),
            Current_round_count = erlang:length(
                gleam@list:filter(
                    Answers,
                    fun(A) -> erlang:element(5, A) =:= Max_round end
                )
            ),
            Questions_in_round = erlang:length(
                intent@interview_questions:get_questions_for_round(
                    profile_to_string(erlang:element(3, Session)),
                    Max_round
                )
            ),
            case Current_round_count >= Questions_in_round of
                true ->
                    Max_round + 1;

                false ->
                    Max_round
            end
    end.

-spec format_progress(interview_session()) -> binary().
format_progress(Session) ->
    Stage_str = case erlang:element(7, Session) of
        discovery ->
            <<"Discovery"/utf8>>;

        refinement ->
            <<"Refinement"/utf8>>;

        validation ->
            <<"Validation"/utf8>>;

        complete ->
            <<"Complete"/utf8>>;

        paused ->
            <<"Paused"/utf8>>
    end,
    Profile_str = profile_to_string(erlang:element(3, Session)),
    Answer_count = erlang:length(erlang:element(9, Session)),
    Gap_count = erlang:length(erlang:element(10, Session)),
    Conflict_count = erlang:length(erlang:element(11, Session)),
    <<<<<<<<<<<<<<<<<<"Profile: "/utf8, Profile_str/binary>>/binary,
                                    " | Stage: "/utf8>>/binary,
                                Stage_str/binary>>/binary,
                            " | Answers: "/utf8>>/binary,
                        (gleam@string:inspect(Answer_count))/binary>>/binary,
                    " | Gaps: "/utf8>>/binary,
                (gleam@string:inspect(Gap_count))/binary>>/binary,
            " | Conflicts: "/utf8>>/binary,
        (gleam@string:inspect(Conflict_count))/binary>>.

-spec string_to_profile(binary()) -> {ok, profile()} | {error, binary()}.
string_to_profile(S) ->
    case gleam@string:lowercase(S) of
        <<"api"/utf8>> ->
            {ok, api};

        <<"cli"/utf8>> ->
            {ok, cli};

        <<"event"/utf8>> ->
            {ok, event};

        <<"data"/utf8>> ->
            {ok, data};

        <<"workflow"/utf8>> ->
            {ok, workflow};

        <<"ui"/utf8>> ->
            {ok, ui};

        _ ->
            {error, <<"Unknown profile: "/utf8, S/binary>>}
    end.
