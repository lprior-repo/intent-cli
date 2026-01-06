-module(intent@interview_storage).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/interview_storage.gleam").
-export([answer_to_version/3, format_diff/1, list_session_history/2, create_snapshot/2, diff_sessions/2, append_to_history/3, session_to_json/1, session_to_jsonl_line/1, init_database/1, save_session_to_db/2, query_sessions_by_profile/2, query_ready_sessions/1, append_session_to_jsonl/2, sync_to_jsonl/3, list_sessions_from_jsonl/1, get_session_from_jsonl/2, sync_from_jsonl/2]).
-export_type([session_record/0, answer_version/0, answer_with_history/0, session_snapshot/0, session_diff/0, answer_diff/0, answer_change_type/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type session_record() :: {session_record,
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        integer(),
        binary()}.

-type answer_version() :: {answer_version,
        integer(),
        binary(),
        gleam@dict:dict(binary(), binary()),
        float(),
        binary(),
        binary()}.

-type answer_with_history() :: {answer_with_history,
        binary(),
        binary(),
        intent@question_types:perspective(),
        integer(),
        intent@interview:answer(),
        list(answer_version()),
        binary()}.

-type session_snapshot() :: {session_snapshot,
        binary(),
        binary(),
        binary(),
        binary(),
        gleam@dict:dict(binary(), binary()),
        integer(),
        integer(),
        binary()}.

-type session_diff() :: {session_diff,
        binary(),
        binary(),
        binary(),
        binary(),
        list(answer_diff()),
        list(answer_diff()),
        list(binary()),
        integer(),
        integer(),
        integer(),
        integer(),
        gleam@option:option({binary(), binary()})}.

-type answer_diff() :: {answer_diff,
        binary(),
        binary(),
        gleam@option:option(binary()),
        binary(),
        answer_change_type()}.

-type answer_change_type() :: added | modified | removed.

-file("src/intent/interview_storage.gleam", 120).
?DOC(" Create an AnswerVersion from an Answer\n").
-spec answer_to_version(intent@interview:answer(), integer(), binary()) -> answer_version().
answer_to_version(Answer, Version, Change_reason) ->
    {answer_version,
        Version,
        erlang:element(6, Answer),
        erlang:element(7, Answer),
        erlang:element(8, Answer),
        erlang:element(10, Answer),
        Change_reason}.

-file("src/intent/interview_storage.gleam", 327).
?DOC(" Truncate a string with ellipsis\n").
-spec truncate(binary(), integer()) -> binary().
truncate(S, Max_len) ->
    Trimmed = gleam@string:trim(S),
    case gleam@string:length(Trimmed) > Max_len of
        true ->
            <<(gleam@string:slice(Trimmed, 0, Max_len - 3))/binary, "..."/utf8>>;

        false ->
            Trimmed
    end.

-file("src/intent/interview_storage.gleam", 248).
?DOC(" Format a SessionDiff as a human-readable string\n").
-spec format_diff(session_diff()) -> binary().
format_diff(Diff) ->
    Lines = [],
    Lines@1 = lists:append(
        Lines,
        [<<<<<<"Session Diff: "/utf8, (erlang:element(2, Diff))/binary>>/binary,
                    " → "/utf8>>/binary,
                (erlang:element(3, Diff))/binary>>,
            <<<<<<"Time: "/utf8, (erlang:element(4, Diff))/binary>>/binary,
                    " → "/utf8>>/binary,
                (erlang:element(5, Diff))/binary>>,
            <<""/utf8>>]
    ),
    Lines@2 = case erlang:element(13, Diff) of
        {some, {From, To}} ->
            lists:append(
                Lines@1,
                [<<<<<<"Stage: "/utf8, From/binary>>/binary, " → "/utf8>>/binary,
                        To/binary>>,
                    <<""/utf8>>]
            );

        none ->
            Lines@1
    end,
    Lines@3 = case erlang:length(erlang:element(6, Diff)) of
        0 ->
            Lines@2;

        N ->
            Header = [<<<<"Answers Added ("/utf8,
                        (gleam@string:inspect(N))/binary>>/binary,
                    "):"/utf8>>],
            Answer_lines = gleam@list:map(
                erlang:element(6, Diff),
                fun(A) ->
                    <<<<<<"  + ["/utf8, (erlang:element(2, A))/binary>>/binary,
                            "] "/utf8>>/binary,
                        (truncate(erlang:element(5, A), 50))/binary>>
                end
            ),
            lists:append(
                Lines@2,
                lists:append(Header, lists:append(Answer_lines, [<<""/utf8>>]))
            )
    end,
    Lines@4 = case erlang:length(erlang:element(7, Diff)) of
        0 ->
            Lines@3;

        N@1 ->
            Header@1 = [<<<<"Answers Modified ("/utf8,
                        (gleam@string:inspect(N@1))/binary>>/binary,
                    "):"/utf8>>],
            Answer_lines@1 = gleam@list:flat_map(
                erlang:element(7, Diff),
                fun(A@1) ->
                    Old = case erlang:element(4, A@1) of
                        {some, R} ->
                            truncate(R, 40);

                        none ->
                            <<"(none)"/utf8>>
                    end,
                    [<<<<"  ~ ["/utf8, (erlang:element(2, A@1))/binary>>/binary,
                            "]"/utf8>>,
                        <<"    - "/utf8, Old/binary>>,
                        <<"    + "/utf8,
                            (truncate(erlang:element(5, A@1), 40))/binary>>]
                end
            ),
            lists:append(
                Lines@3,
                lists:append(
                    Header@1,
                    lists:append(Answer_lines@1, [<<""/utf8>>])
                )
            )
    end,
    Lines@5 = case erlang:length(erlang:element(8, Diff)) of
        0 ->
            Lines@4;

        N@2 ->
            Header@2 = [<<<<"Answers Removed ("/utf8,
                        (gleam@string:inspect(N@2))/binary>>/binary,
                    "):"/utf8>>],
            Answer_lines@2 = gleam@list:map(
                erlang:element(8, Diff),
                fun(Id) -> <<<<"  - ["/utf8, Id/binary>>/binary, "]"/utf8>> end
            ),
            lists:append(
                Lines@4,
                lists:append(
                    Header@2,
                    lists:append(Answer_lines@2, [<<""/utf8>>])
                )
            )
    end,
    Lines@6 = case (erlang:element(9, Diff) > 0) orelse (erlang:element(
        10,
        Diff
    )
    > 0) of
        true ->
            lists:append(
                Lines@5,
                [<<<<<<<<"Gaps: +"/utf8,
                                    (gleam@string:inspect(
                                        erlang:element(9, Diff)
                                    ))/binary>>/binary,
                                " added, -"/utf8>>/binary,
                            (gleam@string:inspect(erlang:element(10, Diff)))/binary>>/binary,
                        " resolved"/utf8>>]
            );

        false ->
            Lines@5
    end,
    Lines@7 = case (erlang:element(11, Diff) > 0) orelse (erlang:element(
        12,
        Diff
    )
    > 0) of
        true ->
            lists:append(
                Lines@6,
                [<<<<<<<<"Conflicts: +"/utf8,
                                    (gleam@string:inspect(
                                        erlang:element(11, Diff)
                                    ))/binary>>/binary,
                                " added, -"/utf8>>/binary,
                            (gleam@string:inspect(erlang:element(12, Diff)))/binary>>/binary,
                        " resolved"/utf8>>]
            );

        false ->
            Lines@6
    end,
    gleam@string:join(Lines@7, <<"\n"/utf8>>).

-file("src/intent/interview_storage.gleam", 364).
-spec snapshot_to_jsonl_line(session_snapshot()) -> binary().
snapshot_to_jsonl_line(Snapshot) ->
    _pipe@1 = gleam@json:object(
        [{<<"session_id"/utf8>>, gleam@json:string(erlang:element(2, Snapshot))},
            {<<"snapshot_id"/utf8>>,
                gleam@json:string(erlang:element(3, Snapshot))},
            {<<"timestamp"/utf8>>,
                gleam@json:string(erlang:element(4, Snapshot))},
            {<<"description"/utf8>>,
                gleam@json:string(erlang:element(5, Snapshot))},
            {<<"answers"/utf8>>,
                gleam@json:object(
                    begin
                        _pipe = maps:to_list(erlang:element(6, Snapshot)),
                        gleam@list:map(
                            _pipe,
                            fun(Pair) ->
                                {erlang:element(1, Pair),
                                    gleam@json:string(erlang:element(2, Pair))}
                            end
                        )
                    end
                )},
            {<<"gaps_count"/utf8>>, gleam@json:int(erlang:element(7, Snapshot))},
            {<<"conflicts_count"/utf8>>,
                gleam@json:int(erlang:element(8, Snapshot))},
            {<<"stage"/utf8>>, gleam@json:string(erlang:element(9, Snapshot))}]
    ),
    gleam@json:to_string(_pipe@1).

-file("src/intent/interview_storage.gleam", 409).
-spec snapshot_decoder(gleam@dynamic:dynamic_()) -> {ok, session_snapshot()} |
    {error, list(gleam@dynamic:decode_error())}.
snapshot_decoder(Json_value) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"session_id"/utf8>>, fun gleam@dynamic:string/1))(
            Json_value
        ),
        fun(Session_id) ->
            gleam@result:'try'(
                (gleam@dynamic:field(
                    <<"snapshot_id"/utf8>>,
                    fun gleam@dynamic:string/1
                ))(Json_value),
                fun(Snapshot_id) ->
                    gleam@result:'try'(
                        (gleam@dynamic:field(
                            <<"timestamp"/utf8>>,
                            fun gleam@dynamic:string/1
                        ))(Json_value),
                        fun(Timestamp) ->
                            gleam@result:'try'(
                                (gleam@dynamic:field(
                                    <<"description"/utf8>>,
                                    fun gleam@dynamic:string/1
                                ))(Json_value),
                                fun(Description) ->
                                    gleam@result:'try'(
                                        (gleam@dynamic:field(
                                            <<"answers"/utf8>>,
                                            gleam@dynamic:dict(
                                                fun gleam@dynamic:string/1,
                                                fun gleam@dynamic:string/1
                                            )
                                        ))(Json_value),
                                        fun(Answers_list) ->
                                            gleam@result:'try'(
                                                (gleam@dynamic:field(
                                                    <<"gaps_count"/utf8>>,
                                                    fun gleam@dynamic:int/1
                                                ))(Json_value),
                                                fun(Gaps_count) ->
                                                    gleam@result:'try'(
                                                        (gleam@dynamic:field(
                                                            <<"conflicts_count"/utf8>>,
                                                            fun gleam@dynamic:int/1
                                                        ))(Json_value),
                                                        fun(Conflicts_count) ->
                                                            gleam@result:'try'(
                                                                (gleam@dynamic:field(
                                                                    <<"stage"/utf8>>,
                                                                    fun gleam@dynamic:string/1
                                                                ))(Json_value),
                                                                fun(Stage) ->
                                                                    {ok,
                                                                        {session_snapshot,
                                                                            Session_id,
                                                                            Snapshot_id,
                                                                            Timestamp,
                                                                            Description,
                                                                            Answers_list,
                                                                            Gaps_count,
                                                                            Conflicts_count,
                                                                            Stage}}
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/intent/interview_storage.gleam", 382).
?DOC(" List all snapshots for a session from history\n").
-spec list_session_history(binary(), binary()) -> {ok, list(session_snapshot())} |
    {error, binary()}.
list_session_history(History_path, Session_id) ->
    gleam@result:'try'(
        begin
            _pipe = simplifile:read(History_path),
            gleam@result:map_error(
                _pipe,
                fun(Err) ->
                    <<"Failed to read history: "/utf8,
                        (gleam@string:inspect(Err))/binary>>
                end
            )
        end,
        fun(Content) -> case gleam@string:length(gleam@string:trim(Content)) of
                0 ->
                    {ok, []};

                _ ->
                    Lines = gleam@string:split(Content, <<"\n"/utf8>>),
                    Snapshots = begin
                        _pipe@2 = gleam@list:filter_map(
                            Lines,
                            fun(Line) ->
                                case gleam@string:length(
                                    gleam@string:trim(Line)
                                ) of
                                    0 ->
                                        {error, nil};

                                    _ ->
                                        _pipe@1 = gleam@json:decode(
                                            Line,
                                            fun snapshot_decoder/1
                                        ),
                                        gleam@result:map_error(
                                            _pipe@1,
                                            fun(_) -> nil end
                                        )
                                end
                            end
                        ),
                        gleam@list:filter(
                            _pipe@2,
                            fun(S) -> erlang:element(2, S) =:= Session_id end
                        )
                    end,
                    {ok, Snapshots}
            end end
    ).

-file("src/intent/interview_storage.gleam", 453).
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

-file("src/intent/interview_storage.gleam", 464).
-spec stage_to_string(intent@interview:interview_stage()) -> binary().
stage_to_string(Stage) ->
    case Stage of
        discovery ->
            <<"discovery"/utf8>>;

        refinement ->
            <<"refinement"/utf8>>;

        validation ->
            <<"validation"/utf8>>;

        complete ->
            <<"complete"/utf8>>;

        paused ->
            <<"paused"/utf8>>
    end.

-file("src/intent/interview_storage.gleam", 136).
?DOC(" Create a session snapshot for comparison\n").
-spec create_snapshot(intent@interview:interview_session(), binary()) -> session_snapshot().
create_snapshot(Session, Description) ->
    Answers_dict = gleam@list:fold(
        erlang:element(9, Session),
        gleam@dict:new(),
        fun(Acc, Answer) ->
            gleam@dict:insert(
                Acc,
                erlang:element(2, Answer),
                erlang:element(6, Answer)
            )
        end
    ),
    Unresolved_gaps = gleam@list:filter(
        erlang:element(10, Session),
        fun(G) -> not erlang:element(9, G) end
    ),
    Unresolved_conflicts = gleam@list:filter(
        erlang:element(11, Session),
        fun(C) -> erlang:element(7, C) < 0 end
    ),
    {session_snapshot,
        erlang:element(2, Session),
        <<<<(erlang:element(2, Session))/binary, "-"/utf8>>/binary,
            (erlang:element(5, Session))/binary>>,
        erlang:element(5, Session),
        Description,
        Answers_dict,
        erlang:length(Unresolved_gaps),
        erlang:length(Unresolved_conflicts),
        stage_to_string(erlang:element(7, Session))}.

-file("src/intent/interview_storage.gleam", 160).
?DOC(" Compare two sessions and produce a diff\n").
-spec diff_sessions(
    intent@interview:interview_session(),
    intent@interview:interview_session()
) -> session_diff().
diff_sessions(From_session, To_session) ->
    From_answers = gleam@list:fold(
        erlang:element(9, From_session),
        gleam@dict:new(),
        fun(Acc, A) -> gleam@dict:insert(Acc, erlang:element(2, A), A) end
    ),
    To_answers = gleam@list:fold(
        erlang:element(9, To_session),
        gleam@dict:new(),
        fun(Acc@1, A@1) ->
            gleam@dict:insert(Acc@1, erlang:element(2, A@1), A@1)
        end
    ),
    Added = gleam@list:filter_map(
        erlang:element(9, To_session),
        fun(Answer) ->
            case gleam@dict:get(From_answers, erlang:element(2, Answer)) of
                {ok, _} ->
                    {error, nil};

                {error, _} ->
                    {ok,
                        {answer_diff,
                            erlang:element(2, Answer),
                            erlang:element(3, Answer),
                            none,
                            erlang:element(6, Answer),
                            added}}
            end
        end
    ),
    Modified = gleam@list:filter_map(
        erlang:element(9, To_session),
        fun(Answer@1) ->
            case gleam@dict:get(From_answers, erlang:element(2, Answer@1)) of
                {ok, Old_answer} ->
                    case erlang:element(6, Old_answer) =:= erlang:element(
                        6,
                        Answer@1
                    ) of
                        true ->
                            {error, nil};

                        false ->
                            {ok,
                                {answer_diff,
                                    erlang:element(2, Answer@1),
                                    erlang:element(3, Answer@1),
                                    {some, erlang:element(6, Old_answer)},
                                    erlang:element(6, Answer@1),
                                    modified}}
                    end;

                {error, _} ->
                    {error, nil}
            end
        end
    ),
    Removed = gleam@list:filter_map(
        erlang:element(9, From_session),
        fun(Answer@2) ->
            case gleam@dict:get(To_answers, erlang:element(2, Answer@2)) of
                {ok, _} ->
                    {error, nil};

                {error, _} ->
                    {ok, erlang:element(2, Answer@2)}
            end
        end
    ),
    From_unresolved_gaps = gleam@list:filter(
        erlang:element(10, From_session),
        fun(G) -> not erlang:element(9, G) end
    ),
    To_unresolved_gaps = gleam@list:filter(
        erlang:element(10, To_session),
        fun(G@1) -> not erlang:element(9, G@1) end
    ),
    Gaps_added = erlang:length(To_unresolved_gaps) - erlang:length(
        From_unresolved_gaps
    ),
    Gaps_resolved = case Gaps_added < 0 of
        true ->
            - Gaps_added;

        false ->
            0
    end,
    From_unresolved_conflicts = gleam@list:filter(
        erlang:element(11, From_session),
        fun(C) -> erlang:element(7, C) < 0 end
    ),
    To_unresolved_conflicts = gleam@list:filter(
        erlang:element(11, To_session),
        fun(C@1) -> erlang:element(7, C@1) < 0 end
    ),
    Conflicts_added = erlang:length(To_unresolved_conflicts) - erlang:length(
        From_unresolved_conflicts
    ),
    Conflicts_resolved = case Conflicts_added < 0 of
        true ->
            - Conflicts_added;

        false ->
            0
    end,
    Stage_changed = case erlang:element(7, From_session) =:= erlang:element(
        7,
        To_session
    ) of
        true ->
            none;

        false ->
            {some,
                {stage_to_string(erlang:element(7, From_session)),
                    stage_to_string(erlang:element(7, To_session))}}
    end,
    {session_diff,
        erlang:element(2, From_session),
        erlang:element(2, To_session),
        erlang:element(5, From_session),
        erlang:element(5, To_session),
        Added,
        Modified,
        Removed,
        case Gaps_added > 0 of
            true ->
                Gaps_added;

            false ->
                0
        end,
        Gaps_resolved,
        case Conflicts_added > 0 of
            true ->
                Conflicts_added;

            false ->
                0
        end,
        Conflicts_resolved,
        Stage_changed}.

-file("src/intent/interview_storage.gleam", 341).
?DOC(
    " Append a session snapshot to history JSONL\n"
    " File: .interview/history.jsonl\n"
).
-spec append_to_history(
    intent@interview:interview_session(),
    binary(),
    binary()
) -> {ok, nil} | {error, binary()}.
append_to_history(Session, Description, History_path) ->
    Snapshot = create_snapshot(Session, Description),
    Line = snapshot_to_jsonl_line(Snapshot),
    gleam@result:'try'(
        begin
            _pipe = simplifile:read(History_path),
            _pipe@1 = gleam@result:unwrap(_pipe, <<""/utf8>>),
            {ok, _pipe@1}
        end,
        fun(Existing) ->
            Content = case gleam@string:length(gleam@string:trim(Existing)) of
                0 ->
                    Line;

                _ ->
                    <<<<Existing/binary, "\n"/utf8>>/binary, Line/binary>>
            end,
            _pipe@2 = simplifile:write(History_path, Content),
            gleam@result:map_error(
                _pipe@2,
                fun(Err) ->
                    <<"Failed to write history: "/utf8,
                        (gleam@string:inspect(Err))/binary>>
                end
            )
        end
    ).

-file("src/intent/interview_storage.gleam", 491).
-spec perspective_to_string(intent@question_types:perspective()) -> binary().
perspective_to_string(Perspective) ->
    case Perspective of
        user ->
            <<"user"/utf8>>;

        developer ->
            <<"developer"/utf8>>;

        ops ->
            <<"ops"/utf8>>;

        security ->
            <<"security"/utf8>>;

        business ->
            <<"business"/utf8>>
    end.

-file("src/intent/interview_storage.gleam", 474).
-spec answer_to_json(intent@interview:answer()) -> gleam@json:json().
answer_to_json(Answer) ->
    gleam@json:object(
        [{<<"question_id"/utf8>>, gleam@json:string(erlang:element(2, Answer))},
            {<<"question_text"/utf8>>,
                gleam@json:string(erlang:element(3, Answer))},
            {<<"perspective"/utf8>>,
                gleam@json:string(
                    perspective_to_string(erlang:element(4, Answer))
                )},
            {<<"round"/utf8>>, gleam@json:int(erlang:element(5, Answer))},
            {<<"response"/utf8>>, gleam@json:string(erlang:element(6, Answer))},
            {<<"extracted"/utf8>>,
                gleam@json:object(
                    begin
                        _pipe = maps:to_list(erlang:element(7, Answer)),
                        gleam@list:map(
                            _pipe,
                            fun(Pair) ->
                                {erlang:element(1, Pair),
                                    gleam@json:string(erlang:element(2, Pair))}
                            end
                        )
                    end
                )},
            {<<"confidence"/utf8>>, gleam@json:float(erlang:element(8, Answer))},
            {<<"notes"/utf8>>, gleam@json:string(erlang:element(9, Answer))},
            {<<"timestamp"/utf8>>,
                gleam@json:string(erlang:element(10, Answer))}]
    ).

-file("src/intent/interview_storage.gleam", 501).
-spec gap_to_json(intent@interview:gap()) -> gleam@json:json().
gap_to_json(Gap) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Gap))},
            {<<"field"/utf8>>, gleam@json:string(erlang:element(3, Gap))},
            {<<"description"/utf8>>, gleam@json:string(erlang:element(4, Gap))},
            {<<"blocking"/utf8>>, gleam@json:bool(erlang:element(5, Gap))},
            {<<"suggested_default"/utf8>>,
                gleam@json:string(erlang:element(6, Gap))},
            {<<"why_needed"/utf8>>, gleam@json:string(erlang:element(7, Gap))},
            {<<"round"/utf8>>, gleam@json:int(erlang:element(8, Gap))},
            {<<"resolved"/utf8>>, gleam@json:bool(erlang:element(9, Gap))},
            {<<"resolution"/utf8>>, gleam@json:string(erlang:element(10, Gap))}]
    ).

-file("src/intent/interview_storage.gleam", 527).
-spec conflict_resolution_to_json(intent@interview:conflict_resolution()) -> gleam@json:json().
conflict_resolution_to_json(Res) ->
    gleam@json:object(
        [{<<"option"/utf8>>, gleam@json:string(erlang:element(2, Res))},
            {<<"description"/utf8>>, gleam@json:string(erlang:element(3, Res))},
            {<<"tradeoffs"/utf8>>, gleam@json:string(erlang:element(4, Res))},
            {<<"recommendation"/utf8>>,
                gleam@json:string(erlang:element(5, Res))}]
    ).

-file("src/intent/interview_storage.gleam", 515).
-spec conflict_to_json(intent@interview:conflict()) -> gleam@json:json().
conflict_to_json(Conflict) ->
    {Between_1, Between_2} = erlang:element(3, Conflict),
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Conflict))},
            {<<"between"/utf8>>,
                gleam@json:array(
                    [Between_1, Between_2],
                    fun gleam@json:string/1
                )},
            {<<"description"/utf8>>,
                gleam@json:string(erlang:element(4, Conflict))},
            {<<"impact"/utf8>>, gleam@json:string(erlang:element(5, Conflict))},
            {<<"options"/utf8>>,
                gleam@json:array(
                    erlang:element(6, Conflict),
                    fun conflict_resolution_to_json/1
                )},
            {<<"chosen"/utf8>>, gleam@json:int(erlang:element(7, Conflict))}]
    ).

-file("src/intent/interview_storage.gleam", 437).
?DOC(
    " JSONL operations - git-friendly line-delimited JSON\n"
    " Each line is a complete session snapshot\n"
    " Stored at: .interview/sessions.jsonl\n"
).
-spec session_to_json(intent@interview:interview_session()) -> gleam@json:json().
session_to_json(Session) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Session))},
            {<<"profile"/utf8>>,
                gleam@json:string(profile_to_string(erlang:element(3, Session)))},
            {<<"created_at"/utf8>>,
                gleam@json:string(erlang:element(4, Session))},
            {<<"updated_at"/utf8>>,
                gleam@json:string(erlang:element(5, Session))},
            {<<"completed_at"/utf8>>,
                gleam@json:string(erlang:element(6, Session))},
            {<<"stage"/utf8>>,
                gleam@json:string(stage_to_string(erlang:element(7, Session)))},
            {<<"rounds_completed"/utf8>>,
                gleam@json:int(erlang:element(8, Session))},
            {<<"answers"/utf8>>,
                gleam@json:array(
                    erlang:element(9, Session),
                    fun answer_to_json/1
                )},
            {<<"gaps"/utf8>>,
                gleam@json:array(erlang:element(10, Session), fun gap_to_json/1)},
            {<<"conflicts"/utf8>>,
                gleam@json:array(
                    erlang:element(11, Session),
                    fun conflict_to_json/1
                )},
            {<<"raw_notes"/utf8>>,
                gleam@json:string(erlang:element(12, Session))}]
    ).

-file("src/intent/interview_storage.gleam", 537).
?DOC(" Encode session to JSONL line (for git storage)\n").
-spec session_to_jsonl_line(intent@interview:interview_session()) -> binary().
session_to_jsonl_line(Session) ->
    _pipe = Session,
    _pipe@1 = session_to_json(_pipe),
    gleam@json:to_string(_pipe@1).

-file("src/intent/interview_storage.gleam", 653).
?DOC(
    " SQLite operations - local database for queries and performance\n"
    " Database schema:\n"
    "\n"
    " CREATE TABLE sessions (\n"
    "   id TEXT PRIMARY KEY,\n"
    "   profile TEXT NOT NULL,\n"
    "   created_at TEXT NOT NULL,\n"
    "   updated_at TEXT NOT NULL,\n"
    "   completed_at TEXT,\n"
    "   stage TEXT NOT NULL,\n"
    "   rounds_completed INTEGER NOT NULL,\n"
    "   raw_notes TEXT,\n"
    "   data JSONB  -- Full session data\n"
    " );\n"
    "\n"
    " CREATE TABLE answers (\n"
    "   id TEXT PRIMARY KEY,\n"
    "   session_id TEXT NOT NULL REFERENCES sessions(id),\n"
    "   question_id TEXT NOT NULL,\n"
    "   round INTEGER NOT NULL,\n"
    "   perspective TEXT NOT NULL,\n"
    "   response TEXT NOT NULL,\n"
    "   confidence REAL NOT NULL,\n"
    "   timestamp TEXT NOT NULL\n"
    " );\n"
    "\n"
    " CREATE TABLE gaps (\n"
    "   id TEXT PRIMARY KEY,\n"
    "   session_id TEXT NOT NULL REFERENCES sessions(id),\n"
    "   field TEXT NOT NULL,\n"
    "   blocking BOOLEAN NOT NULL,\n"
    "   resolved BOOLEAN NOT NULL\n"
    " );\n"
    "\n"
    " CREATE TABLE conflicts (\n"
    "   id TEXT PRIMARY KEY,\n"
    "   session_id TEXT NOT NULL REFERENCES sessions(id),\n"
    "   description TEXT NOT NULL,\n"
    "   chosen INTEGER\n"
    " );\n"
    " Initialize SQLite database (create tables if not exist)\n"
).
-spec init_database(binary()) -> {ok, nil} | {error, binary()}.
init_database(_) ->
    {ok, nil}.

-file("src/intent/interview_storage.gleam", 662).
?DOC(" Save session to SQLite\n").
-spec save_session_to_db(binary(), intent@interview:interview_session()) -> {ok,
        nil} |
    {error, binary()}.
save_session_to_db(_, _) ->
    {ok, nil}.

-file("src/intent/interview_storage.gleam", 672).
?DOC(" Query sessions by profile\n").
-spec query_sessions_by_profile(binary(), binary()) -> {ok,
        list(session_record())} |
    {error, binary()}.
query_sessions_by_profile(_, _) ->
    {ok, []}.

-file("src/intent/interview_storage.gleam", 680).
?DOC(" Query ready sessions (active, not complete, has gaps)\n").
-spec query_ready_sessions(binary()) -> {ok, list(session_record())} |
    {error, binary()}.
query_ready_sessions(_) ->
    {ok, []}.

-file("src/intent/interview_storage.gleam", 716).
-spec session_id_decoder(gleam@dynamic:dynamic_()) -> {ok, binary()} |
    {error, list(gleam@dynamic:decode_error())}.
session_id_decoder(Json_value) ->
    (gleam@dynamic:field(<<"id"/utf8>>, fun gleam@dynamic:string/1))(Json_value).

-file("src/intent/interview_storage.gleam", 545).
?DOC(
    " Write session to .interview/sessions.jsonl\n"
    " Each session ID appears once, most recent last (for efficient updates)\n"
).
-spec append_session_to_jsonl(intent@interview:interview_session(), binary()) -> {ok,
        nil} |
    {error, binary()}.
append_session_to_jsonl(Session, Jsonl_path) ->
    gleam@result:'try'(
        begin
            _pipe = simplifile:read(Jsonl_path),
            gleam@result:map_error(_pipe, fun(_) -> <<""/utf8>> end)
        end,
        fun(Existing) ->
            Lines = case Existing of
                <<""/utf8>> ->
                    [];

                Content ->
                    gleam@string:split(Content, <<"\n"/utf8>>)
            end,
            Filtered = gleam@list:filter(
                Lines,
                fun(Line) ->
                    case gleam@json:decode(Line, fun session_id_decoder/1) of
                        {ok, Id} ->
                            Id /= erlang:element(2, Session);

                        {error, _} ->
                            true
                    end
                end
            ),
            New_line = session_to_jsonl_line(Session),
            All_lines = lists:append(Filtered, [New_line]),
            Content@1 = gleam@string:join(All_lines, <<"\n"/utf8>>),
            _pipe@1 = simplifile:write(Jsonl_path, Content@1),
            gleam@result:map_error(
                _pipe@1,
                fun(Err) ->
                    <<"Failed to write JSONL: "/utf8,
                        (gleam@string:inspect(Err))/binary>>
                end
            )
        end
    ).

-file("src/intent/interview_storage.gleam", 690).
?DOC(
    " Sync operations - keep SQLite and JSONL in sync\n"
    " Strategy: JSONL is source of truth for git\n"
    " 1. On read: load from JSONL, check SQLite is consistent\n"
    " 2. On write: write to both\n"
    " 3. Conflict resolution: JSONL wins (it's in git)\n"
).
-spec sync_to_jsonl(intent@interview:interview_session(), binary(), binary()) -> {ok,
        nil} |
    {error, binary()}.
sync_to_jsonl(Session, Db_path, Jsonl_path) ->
    gleam@result:'try'(
        save_session_to_db(Db_path, Session),
        fun(_) ->
            gleam@result:'try'(
                append_session_to_jsonl(Session, Jsonl_path),
                fun(_) -> {ok, nil} end
            )
        end
    ).

-file("src/intent/interview_storage.gleam", 720).
-spec session_decoder(gleam@dynamic:dynamic_()) -> {ok,
        intent@interview:interview_session()} |
    {error, list(gleam@dynamic:decode_error())}.
session_decoder(Json_value) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"id"/utf8>>, fun gleam@dynamic:string/1))(
            Json_value
        ),
        fun(Id) ->
            gleam@result:'try'(
                (gleam@dynamic:field(
                    <<"profile"/utf8>>,
                    fun gleam@dynamic:string/1
                ))(Json_value),
                fun(Profile_str) -> gleam@result:'try'(case Profile_str of
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
                                {error,
                                    [{decode_error,
                                            <<"profile"/utf8>>,
                                            <<"invalid profile"/utf8>>,
                                            []}]}
                        end, fun(Profile) ->
                            gleam@result:'try'(
                                (gleam@dynamic:field(
                                    <<"created_at"/utf8>>,
                                    fun gleam@dynamic:string/1
                                ))(Json_value),
                                fun(Created_at) ->
                                    gleam@result:'try'(
                                        (gleam@dynamic:field(
                                            <<"updated_at"/utf8>>,
                                            fun gleam@dynamic:string/1
                                        ))(Json_value),
                                        fun(Updated_at) ->
                                            gleam@result:'try'(
                                                begin
                                                    _pipe = (gleam@dynamic:field(
                                                        <<"completed_at"/utf8>>,
                                                        fun gleam@dynamic:string/1
                                                    ))(Json_value),
                                                    gleam@result:map_error(
                                                        _pipe,
                                                        fun(_) -> [] end
                                                    )
                                                end,
                                                fun(Completed_at) ->
                                                    gleam@result:'try'(
                                                        (gleam@dynamic:field(
                                                            <<"stage"/utf8>>,
                                                            fun gleam@dynamic:string/1
                                                        ))(Json_value),
                                                        fun(Stage_str) ->
                                                            gleam@result:'try'(
                                                                case Stage_str of
                                                                    <<"Discovery"/utf8>> ->
                                                                        {ok,
                                                                            discovery};

                                                                    <<"Refinement"/utf8>> ->
                                                                        {ok,
                                                                            refinement};

                                                                    <<"Validation"/utf8>> ->
                                                                        {ok,
                                                                            validation};

                                                                    <<"Complete"/utf8>> ->
                                                                        {ok,
                                                                            complete};

                                                                    <<"Paused"/utf8>> ->
                                                                        {ok,
                                                                            paused};

                                                                    _ ->
                                                                        {error,
                                                                            [{decode_error,
                                                                                    <<"stage"/utf8>>,
                                                                                    <<"invalid stage"/utf8>>,
                                                                                    []}]}
                                                                end,
                                                                fun(Stage) ->
                                                                    gleam@result:'try'(
                                                                        (gleam@dynamic:field(
                                                                            <<"rounds_completed"/utf8>>,
                                                                            fun gleam@dynamic:int/1
                                                                        ))(
                                                                            Json_value
                                                                        ),
                                                                        fun(
                                                                            Rounds_completed
                                                                        ) ->
                                                                            gleam@result:'try'(
                                                                                begin
                                                                                    _pipe@1 = (gleam@dynamic:field(
                                                                                        <<"raw_notes"/utf8>>,
                                                                                        fun gleam@dynamic:string/1
                                                                                    ))(
                                                                                        Json_value
                                                                                    ),
                                                                                    gleam@result:map_error(
                                                                                        _pipe@1,
                                                                                        fun(
                                                                                            _
                                                                                        ) ->
                                                                                            []
                                                                                        end
                                                                                    )
                                                                                end,
                                                                                fun(
                                                                                    Raw_notes
                                                                                ) ->
                                                                                    {ok,
                                                                                        {interview_session,
                                                                                            Id,
                                                                                            Profile,
                                                                                            Created_at,
                                                                                            Updated_at,
                                                                                            Completed_at,
                                                                                            Stage,
                                                                                            Rounds_completed,
                                                                                            [],
                                                                                            [],
                                                                                            [],
                                                                                            Raw_notes}}
                                                                                end
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end) end
            )
        end
    ).

-file("src/intent/interview_storage.gleam", 576).
?DOC(" List all sessions from JSONL file\n").
-spec list_sessions_from_jsonl(binary()) -> {ok,
        list(intent@interview:interview_session())} |
    {error, binary()}.
list_sessions_from_jsonl(Jsonl_path) ->
    gleam@result:'try'(
        begin
            _pipe = simplifile:read(Jsonl_path),
            gleam@result:map_error(
                _pipe,
                fun(Err) ->
                    <<"Failed to read JSONL: "/utf8,
                        (gleam@string:inspect(Err))/binary>>
                end
            )
        end,
        fun(Content) -> case gleam@string:length(gleam@string:trim(Content)) of
                0 ->
                    {ok, []};

                _ ->
                    Lines = gleam@string:split(Content, <<"\n"/utf8>>),
                    Sessions = gleam@list:filter_map(
                        Lines,
                        fun(Line) ->
                            case gleam@string:length(gleam@string:trim(Line)) of
                                0 ->
                                    {error, nil};

                                _ ->
                                    _pipe@1 = gleam@json:decode(
                                        Line,
                                        fun session_decoder/1
                                    ),
                                    gleam@result:map_error(
                                        _pipe@1,
                                        fun(_) -> nil end
                                    )
                            end
                        end
                    ),
                    {ok, Sessions}
            end end
    ).

-file("src/intent/interview_storage.gleam", 600).
?DOC(" Get session by ID from JSONL\n").
-spec get_session_from_jsonl(binary(), binary()) -> {ok,
        intent@interview:interview_session()} |
    {error, binary()}.
get_session_from_jsonl(Jsonl_path, Session_id) ->
    _pipe = list_sessions_from_jsonl(Jsonl_path),
    gleam@result:'try'(
        _pipe,
        fun(Sessions) ->
            _pipe@1 = gleam@list:find(
                Sessions,
                fun(S) -> erlang:element(2, S) =:= Session_id end
            ),
            gleam@result:map_error(
                _pipe@1,
                fun(_) -> <<"Session not found: "/utf8, Session_id/binary>> end
            )
        end
    ).

-file("src/intent/interview_storage.gleam", 702).
-spec sync_from_jsonl(binary(), binary()) -> {ok,
        list(intent@interview:interview_session())} |
    {error, binary()}.
sync_from_jsonl(Jsonl_path, Db_path) ->
    gleam@result:'try'(
        list_sessions_from_jsonl(Jsonl_path),
        fun(Sessions) ->
            _pipe = gleam@list:fold(
                Sessions,
                {ok, nil},
                fun(Acc, Session) ->
                    gleam@result:'try'(
                        Acc,
                        fun(_) -> save_session_to_db(Db_path, Session) end
                    )
                end
            ),
            gleam@result:map(_pipe, fun(_) -> Sessions end)
        end
    ).
