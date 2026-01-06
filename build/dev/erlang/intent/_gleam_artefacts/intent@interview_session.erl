-module(intent@interview_session).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/interview_session.gleam").
-export([start_interview/3, add_answer/2, check_for_gaps/3, check_for_conflicts/2, complete_round/1, resolve_conflict/3, resolve_gap/3, get_blocking_gaps/1, get_unresolved_conflicts/1, can_proceed/1, get_first_question_for_round/2, get_next_question_in_round/2, get_current_round/1, format_progress/1, string_to_profile/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/intent/interview_session.gleam", 16).
?DOC(" Initialize a new interview session\n").
-spec start_interview(intent@interview:profile(), binary(), binary()) -> intent@interview:interview_session().
start_interview(Profile, Session_id, Timestamp) ->
    {interview_session,
        Session_id,
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

-file("src/intent/interview_session.gleam", 61).
?DOC(" Add an answer to the session\n").
-spec add_answer(
    intent@interview:interview_session(),
    intent@interview:answer()
) -> intent@interview:interview_session().
add_answer(Session, Answer) ->
    {interview_session,
        erlang:element(2, Session),
        erlang:element(3, Session),
        erlang:element(4, Session),
        erlang:element(10, Answer),
        erlang:element(6, Session),
        erlang:element(7, Session),
        erlang:element(8, Session),
        lists:append(erlang:element(9, Session), [Answer]),
        erlang:element(10, Session),
        erlang:element(11, Session),
        erlang:element(12, Session)}.

-file("src/intent/interview_session.gleam", 70).
?DOC(" Get all answered question IDs\n").
-spec get_answered_question_ids(intent@interview:interview_session()) -> list(binary()).
get_answered_question_ids(Session) ->
    gleam@list:map(
        erlang:element(9, Session),
        fun(Answer) -> erlang:element(2, Answer) end
    ).

-file("src/intent/interview_session.gleam", 89).
?DOC(" Detect blocking gaps in the answer\n").
-spec detect_blocking_gaps(
    intent@interview_questions:question(),
    intent@interview:answer()
) -> list(intent@interview:gap()).
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

-file("src/intent/interview_session.gleam", 75).
?DOC(" Check for gaps after answering a question\n").
-spec check_for_gaps(
    intent@interview:interview_session(),
    intent@interview_questions:question(),
    intent@interview:answer()
) -> {intent@interview:interview_session(), list(intent@interview:gap())}.
check_for_gaps(Session, Question, Answer) ->
    Blocking_gaps = detect_blocking_gaps(Question, Answer),
    Updated_session = {interview_session,
        erlang:element(2, Session),
        erlang:element(3, Session),
        erlang:element(4, Session),
        erlang:element(5, Session),
        erlang:element(6, Session),
        erlang:element(7, Session),
        erlang:element(8, Session),
        erlang:element(9, Session),
        lists:append(erlang:element(10, Session), Blocking_gaps),
        erlang:element(11, Session),
        erlang:element(12, Session)},
    {Updated_session, Blocking_gaps}.

-file("src/intent/interview_session.gleam", 137).
?DOC(" Detect conflicts between two answers\n").
-spec detect_answer_pair_conflicts(
    intent@interview:answer(),
    intent@interview:answer()
) -> list(intent@interview:conflict()).
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

-file("src/intent/interview_session.gleam", 125).
?DOC(" Detect conflicting requirements\n").
-spec detect_conflicts_in_session(
    intent@interview:interview_session(),
    intent@interview:answer()
) -> list(intent@interview:conflict()).
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

-file("src/intent/interview_session.gleam", 112).
?DOC(" Check for conflicts between answers\n").
-spec check_for_conflicts(
    intent@interview:interview_session(),
    intent@interview:answer()
) -> {intent@interview:interview_session(), list(intent@interview:conflict())}.
check_for_conflicts(Session, New_answer) ->
    Conflicts = detect_conflicts_in_session(Session, New_answer),
    Updated_session = {interview_session,
        erlang:element(2, Session),
        erlang:element(3, Session),
        erlang:element(4, Session),
        erlang:element(5, Session),
        erlang:element(6, Session),
        erlang:element(7, Session),
        erlang:element(8, Session),
        erlang:element(9, Session),
        erlang:element(10, Session),
        lists:append(erlang:element(11, Session), Conflicts),
        erlang:element(12, Session)},
    {Updated_session, Conflicts}.

-file("src/intent/interview_session.gleam", 177).
?DOC(" Mark a round as complete\n").
-spec complete_round(intent@interview:interview_session()) -> intent@interview:interview_session().
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
    {interview_session,
        erlang:element(2, Session),
        erlang:element(3, Session),
        erlang:element(4, Session),
        erlang:element(5, Session),
        erlang:element(6, Session),
        New_stage,
        erlang:element(8, Session) + 1,
        erlang:element(9, Session),
        erlang:element(10, Session),
        erlang:element(11, Session),
        erlang:element(12, Session)}.

-file("src/intent/interview_session.gleam", 225).
?DOC(" Resolve a conflict by choosing an option\n").
-spec resolve_conflict(
    intent@interview:interview_session(),
    binary(),
    integer()
) -> {ok, intent@interview:interview_session()} | {error, binary()}.
resolve_conflict(Session, Conflict_id, Chosen_option) ->
    Updated_conflicts = gleam@list:map(
        erlang:element(11, Session),
        fun(Conflict) -> case erlang:element(2, Conflict) =:= Conflict_id of
                true ->
                    {conflict,
                        erlang:element(2, Conflict),
                        erlang:element(3, Conflict),
                        erlang:element(4, Conflict),
                        erlang:element(5, Conflict),
                        erlang:element(6, Conflict),
                        Chosen_option};

                false ->
                    Conflict
            end end
    ),
    {ok,
        {interview_session,
            erlang:element(2, Session),
            erlang:element(3, Session),
            erlang:element(4, Session),
            erlang:element(5, Session),
            erlang:element(6, Session),
            erlang:element(7, Session),
            erlang:element(8, Session),
            erlang:element(9, Session),
            erlang:element(10, Session),
            Updated_conflicts,
            erlang:element(12, Session)}}.

-file("src/intent/interview_session.gleam", 248).
?DOC(" Mark a gap as resolved\n").
-spec resolve_gap(intent@interview:interview_session(), binary(), binary()) -> intent@interview:interview_session().
resolve_gap(Session, Gap_id, Resolution) ->
    Updated_gaps = gleam@list:map(
        erlang:element(10, Session),
        fun(Gap) -> case erlang:element(2, Gap) =:= Gap_id of
                true ->
                    {gap,
                        erlang:element(2, Gap),
                        erlang:element(3, Gap),
                        erlang:element(4, Gap),
                        erlang:element(5, Gap),
                        erlang:element(6, Gap),
                        erlang:element(7, Gap),
                        erlang:element(8, Gap),
                        true,
                        Resolution};

                false ->
                    Gap
            end end
    ),
    {interview_session,
        erlang:element(2, Session),
        erlang:element(3, Session),
        erlang:element(4, Session),
        erlang:element(5, Session),
        erlang:element(6, Session),
        erlang:element(7, Session),
        erlang:element(8, Session),
        erlang:element(9, Session),
        Updated_gaps,
        erlang:element(11, Session),
        erlang:element(12, Session)}.

-file("src/intent/interview_session.gleam", 272).
?DOC(" Get all unresolved blocking gaps\n").
-spec get_blocking_gaps(intent@interview:interview_session()) -> list(intent@interview:gap()).
get_blocking_gaps(Session) ->
    gleam@list:filter(
        erlang:element(10, Session),
        fun(Gap) ->
            erlang:element(5, Gap) andalso not erlang:element(9, Gap)
        end
    ).

-file("src/intent/interview_session.gleam", 279).
?DOC(" Get all unresolved conflicts\n").
-spec get_unresolved_conflicts(intent@interview:interview_session()) -> list(intent@interview:conflict()).
get_unresolved_conflicts(Session) ->
    gleam@list:filter(
        erlang:element(11, Session),
        fun(Conflict) -> erlang:element(7, Conflict) =:= -1 end
    ).

-file("src/intent/interview_session.gleam", 286).
?DOC(" Check if interview can proceed (no blocking gaps)\n").
-spec can_proceed(intent@interview:interview_session()) -> {ok, nil} |
    {error, binary()}.
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

-file("src/intent/interview_session.gleam", 325).
?DOC(" Helper: convert Profile to string\n").
-spec profile_to_string(intent@interview:profile()) -> binary().
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

        u_i ->
            <<"ui"/utf8>>
    end.

-file("src/intent/interview_session.gleam", 33).
?DOC(" Get first question for a given round\n").
-spec get_first_question_for_round(
    intent@interview:interview_session(),
    integer()
) -> {ok, intent@interview_questions:question()} | {error, binary()}.
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

-file("src/intent/interview_session.gleam", 47).
?DOC(" Get next unanswered question in current round\n").
-spec get_next_question_in_round(
    intent@interview:interview_session(),
    integer()
) -> {ok, intent@interview_questions:question()} | {error, binary()}.
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

-file("src/intent/interview_session.gleam", 195).
?DOC(" Get current round number based on answers\n").
-spec get_current_round(intent@interview:interview_session()) -> integer().
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

-file("src/intent/interview_session.gleam", 298).
?DOC(" Format progress summary\n").
-spec format_progress(intent@interview:interview_session()) -> binary().
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

-file("src/intent/interview_session.gleam", 337).
?DOC(" Helper: convert string to Profile\n").
-spec string_to_profile(binary()) -> {ok, intent@interview:profile()} |
    {error, binary()}.
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
            {ok, u_i};

        _ ->
            {error, <<"Unknown profile: "/utf8, S/binary>>}
    end.
