-module(intent@interview).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/interview.gleam").
-export([extract_from_answer/3, get_questions_for_round/2, detect_gaps/2, detect_conflicts/1, calculate_confidence/3, format_question/1, create_session/3, add_answer/2, complete_round/1]).
-export_type([profile/0, interview_stage/0, answer/0, gap/0, conflict/0, conflict_resolution/0, interview_session/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type profile() :: api | cli | event | data | workflow | u_i.

-type interview_stage() :: discovery |
    refinement |
    validation |
    complete |
    paused.

-type answer() :: {answer,
        binary(),
        binary(),
        intent@interview_questions:perspective(),
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

-file("src/intent/interview.gleam", 139).
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

-file("src/intent/interview.gleam", 163).
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

-file("src/intent/interview.gleam", 184).
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

-file("src/intent/interview.gleam", 117).
?DOC(" Simple extraction patterns - can be extended to use NLP/LLM\n").
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

-file("src/intent/interview.gleam", 102).
?DOC(
    " Extract field from answer text (AI-driven)\n"
    " This is where the \"AI adapts\" - the extraction logic learns from patterns\n"
).
-spec extract_from_answer(binary(), binary(), list(binary())) -> gleam@dict:dict(binary(), binary()).
extract_from_answer(Question_id, Response, Extract_fields) ->
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

-file("src/intent/interview.gleam", 210).
?DOC(
    " Get questions for a specific round and profile\n"
    " Delegate to interview_questions module\n"
).
-spec get_questions_for_round(profile(), integer()) -> list(intent@interview_questions:question()).
get_questions_for_round(Profile, Round) ->
    Profile_str = case Profile of
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

        u_i ->
            <<"ui"/utf8>>
    end,
    intent@interview_questions:get_questions_for_round(Profile_str, Round).

-file("src/intent/interview.gleam", 229).
?DOC(" Detect gaps from collected answers\n").
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

        u_i ->
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

-file("src/intent/interview.gleam", 272).
?DOC(" Detect conflicts between answers\n").
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
    end,
    Conflicts@1.

-file("src/intent/interview.gleam", 360).
?DOC(" Calculate confidence in answer extraction (0-1)\n").
-spec calculate_confidence(
    binary(),
    binary(),
    gleam@dict:dict(binary(), binary())
) -> float().
calculate_confidence(Question_id, Response, Extracted) ->
    Response_length = gleam@string:length(gleam@string:trim(Response)),
    Field_count = maps:size(Extracted),
    case (Response_length > 50) andalso (Field_count > 0) of
        true ->
            0.85;

        false ->
            0.6
    end.

-file("src/intent/interview.gleam", 376).
?DOC(" Format a question for display\n").
-spec format_question(intent@interview_questions:question()) -> binary().
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

-file("src/intent/interview.gleam", 401).
?DOC(" Create a new session\n").
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

-file("src/intent/interview.gleam", 418).
?DOC(" Add answer to session\n").
-spec add_answer(interview_session(), answer()) -> interview_session().
add_answer(Session, Answer) ->
    New_answers = lists:append(erlang:element(9, Session), [Answer]),
    {interview_session,
        erlang:element(2, Session),
        erlang:element(3, Session),
        erlang:element(4, Session),
        erlang:element(10, Answer),
        erlang:element(6, Session),
        erlang:element(7, Session),
        erlang:element(8, Session),
        New_answers,
        erlang:element(10, Session),
        erlang:element(11, Session),
        erlang:element(12, Session)}.

-file("src/intent/interview.gleam", 431).
?DOC(" Mark round as completed\n").
-spec complete_round(interview_session()) -> interview_session().
complete_round(Session) ->
    New_round = erlang:element(8, Session) + 1,
    {interview_session,
        erlang:element(2, Session),
        erlang:element(3, Session),
        erlang:element(4, Session),
        erlang:element(5, Session),
        erlang:element(6, Session),
        case New_round of
            5 ->
                complete;

            _ ->
                refinement
        end,
        New_round,
        erlang:element(9, Session),
        erlang:element(10, Session),
        erlang:element(11, Session),
        erlang:element(12, Session)}.
