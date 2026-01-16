-module(intent).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec print_spec_summary(intent@types:spec()) -> nil.
print_spec_summary(Spec) ->
    gleam@io:println(<<"Spec: "/utf8, (erlang:element(2, Spec))/binary>>),
    gleam@io:println(<<"Version: "/utf8, (erlang:element(5, Spec))/binary>>),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"Description:"/utf8>>),
    gleam@io:println(erlang:element(3, Spec)),
    gleam@io:println(<<""/utf8>>),
    case erlang:element(4, Spec) of
        <<""/utf8>> ->
            nil;

        Audience ->
            gleam@io:println(<<"Audience: "/utf8, Audience/binary>>),
            gleam@io:println(<<""/utf8>>)
    end,
    case erlang:element(6, Spec) of
        [] ->
            nil;

        Criteria ->
            gleam@io:println(<<"Success Criteria:"/utf8>>),
            gleam@list:each(
                Criteria,
                fun(C) -> gleam@io:println(<<"  - "/utf8, C/binary>>) end
            ),
            gleam@io:println(<<""/utf8>>)
    end,
    gleam@io:println(<<"Features:"/utf8>>),
    gleam@list:each(
        erlang:element(8, Spec),
        fun(Feature) ->
            gleam@io:println(<<"  "/utf8, (erlang:element(2, Feature))/binary>>),
            gleam@io:println(
                <<"    "/utf8, (erlang:element(3, Feature))/binary>>
            ),
            gleam@io:println(
                <<"    Behaviors: "/utf8,
                    (gleam@string:inspect(
                        erlang:length(erlang:element(4, Feature))
                    ))/binary>>
            ),
            gleam@list:each(
                erlang:element(4, Feature),
                fun(B) ->
                    gleam@io:println(
                        <<<<<<"      - "/utf8, (erlang:element(2, B))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (erlang:element(3, B))/binary>>
                    )
                end
            )
        end
    ),
    case erlang:element(9, Spec) of
        [] ->
            nil;

        Rules ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"Global Rules:"/utf8>>),
            gleam@list:each(
                Rules,
                fun(Rule) ->
                    gleam@io:println(
                        <<<<<<"  - "/utf8, (erlang:element(2, Rule))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (erlang:element(3, Rule))/binary>>
                    )
                end
            )
    end,
    case erlang:element(10, Spec) of
        [] ->
            nil;

        Patterns ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"Anti-Patterns:"/utf8>>),
            gleam@list:each(
                Patterns,
                fun(P) ->
                    gleam@io:println(
                        <<<<<<"  - "/utf8, (erlang:element(2, P))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (erlang:element(3, P))/binary>>
                    )
                end
            )
    end,
    nil.

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

        ui ->
            <<"ui"/utf8>>
    end.

-spec profile_to_display_string(intent@interview:profile()) -> binary().
profile_to_display_string(Profile) ->
    case Profile of
        api ->
            <<"API"/utf8>>;

        cli ->
            <<"CLI"/utf8>>;

        event ->
            <<"Event System"/utf8>>;

        data ->
            <<"Data System"/utf8>>;

        workflow ->
            <<"Workflow"/utf8>>;

        ui ->
            <<"User Interface"/utf8>>
    end.

-spec risk_level_to_string(intent@plan_mode:risk_level()) -> binary().
risk_level_to_string(Risk) ->
    case Risk of
        low ->
            <<"low"/utf8>>;

        medium ->
            <<"medium"/utf8>>;

        high ->
            <<"high"/utf8>>;

        critical ->
            <<"critical"/utf8>>
    end.

-spec escape_cue_string(binary()) -> binary().
escape_cue_string(S) ->
    _pipe = S,
    _pipe@1 = gleam@string:replace(_pipe, <<"\\"/utf8>>, <<"\\\\"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"\""/utf8>>, <<"\\\""/utf8>>),
    _pipe@3 = gleam@string:replace(_pipe@2, <<"\n"/utf8>>, <<"\\n"/utf8>>),
    gleam@string:replace(_pipe@3, <<"\t"/utf8>>, <<"\\t"/utf8>>).

-spec approve_plan(binary(), binary(), binary()) -> {ok, nil} |
    {error, binary()}.
approve_plan(Session_id, Approved_by, Notes) ->
    Session_path = <<<<".intent/session-"/utf8, Session_id/binary>>/binary,
        ".cue"/utf8>>,
    Timestamp = intent_ffi:current_iso8601_timestamp(),
    Notes_line = case gleam@string:is_empty(Notes) of
        true ->
            <<""/utf8>>;

        false ->
            <<<<"\n\tnotes: \""/utf8, (escape_cue_string(Notes))/binary>>/binary,
                "\""/utf8>>
    end,
    Approval_cue = <<<<<<<<<<<<"\n// Plan Approval\napproval: {\n\tapproved: true\n\tapproved_at: \""/utf8,
                            Timestamp/binary>>/binary,
                        "\"\n\tapproved_by: \""/utf8>>/binary,
                    Approved_by/binary>>/binary,
                "\""/utf8>>/binary,
            Notes_line/binary>>/binary,
        "\n}\n"/utf8>>,
    case simplifile:append(Session_path, Approval_cue) of
        {ok, nil} ->
            {ok, nil};

        {error, Err} ->
            {error,
                <<"Failed to write approval: "/utf8,
                    (gleam@string:inspect(Err))/binary>>}
    end.

-spec generate_regeneration_entries(
    list(intent@bead_feedback:bead_feedback()),
    binary()
) -> binary().
generate_regeneration_entries(Failed_beads, Strategy) ->
    Timestamp = intent_ffi:current_iso8601_timestamp(),
    Entries = begin
        _pipe = Failed_beads,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Fb) ->
                Root_cause = case erlang:element(7, Fb) of
                    {some, Err} ->
                        erlang:element(3, Err);

                    none ->
                        erlang:element(4, Fb)
                end,
                <<<<<<<<<<<<<<<<<<<<<<<<<<"  {\n"/utf8, "    bead_id: \""/utf8>>/binary,
                                                                (erlang:element(
                                                                    2,
                                                                    Fb
                                                                ))/binary>>/binary,
                                                            "\"\n"/utf8>>/binary,
                                                        "    strategy: \""/utf8>>/binary,
                                                    Strategy/binary>>/binary,
                                                "\"\n"/utf8>>/binary,
                                            "    root_cause: \""/utf8>>/binary,
                                        (escape_cue_string(Root_cause))/binary>>/binary,
                                    "\"\n"/utf8>>/binary,
                                "    regenerated_at: \""/utf8>>/binary,
                            Timestamp/binary>>/binary,
                        "\"\n"/utf8>>/binary,
                    "  }"/utf8>>
            end
        ),
        gleam@string:join(_pipe@1, <<",\n"/utf8>>)
    end,
    Entries.

-spec append_regeneration_to_session(binary(), binary()) -> {ok, nil} |
    {error, binary()}.
append_regeneration_to_session(Session_path, Entries) ->
    Regen_cue = <<<<"\n// Regeneration Metadata\nregenerations: [\n"/utf8,
            Entries/binary>>/binary,
        "\n]\n"/utf8>>,
    case simplifile:append(Session_path, Regen_cue) of
        {ok, nil} ->
            {ok, nil};

        {error, Err} ->
            {error,
                <<"Failed to append: "/utf8,
                    (gleam@string:inspect(Err))/binary>>}
    end.

-spec bead_feedback_error_to_string(intent@bead_feedback:feedback_error()) -> binary().
bead_feedback_error_to_string(Err) ->
    case Err of
        {session_not_found, Id} ->
            <<"Session not found: "/utf8, Id/binary>>;

        {write_error, Path, Msg} ->
            <<<<<<"Write error to "/utf8, Path/binary>>/binary, ": "/utf8>>/binary,
                Msg/binary>>;

        {validation_error, Msg@1} ->
            <<"Validation error: "/utf8, Msg@1/binary>>
    end.

-spec stage_to_display_string(intent@interview:interview_stage()) -> binary().
stage_to_display_string(Stage) ->
    case Stage of
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
    end.

-spec gap_to_json(intent@kirk@inversion_checker:inversion_gap()) -> gleam@json:json().
gap_to_json(Gap) ->
    gleam@json:object(
        [{<<"category"/utf8>>, gleam@json:string(erlang:element(2, Gap))},
            {<<"description"/utf8>>, gleam@json:string(erlang:element(3, Gap))},
            {<<"severity"/utf8>>,
                gleam@json:string(
                    intent@kirk@inversion_checker:severity_to_string(
                        erlang:element(4, Gap)
                    )
                )},
            {<<"what_could_fail"/utf8>>,
                gleam@json:string(erlang:element(5, Gap))}]
    ).

-spec detected_gap_to_json(intent@kirk@gap_detector:gap()) -> gleam@json:json().
detected_gap_to_json(Gap) ->
    gleam@json:object(
        [{<<"type"/utf8>>,
                gleam@json:string(
                    intent@kirk@gap_detector:gap_type_to_string(
                        erlang:element(2, Gap)
                    )
                )},
            {<<"description"/utf8>>, gleam@json:string(erlang:element(3, Gap))},
            {<<"severity"/utf8>>,
                gleam@json:string(
                    intent@kirk@gap_detector:severity_to_string(
                        erlang:element(4, Gap)
                    )
                )},
            {<<"suggestion"/utf8>>, gleam@json:string(erlang:element(5, Gap))},
            {<<"mental_model"/utf8>>, gleam@json:string(erlang:element(6, Gap))}]
    ).

-spec ask_single_question(
    intent@interview:interview_session(),
    intent@question_types:question(),
    integer()
) -> intent@interview:interview_session().
ask_single_question(Session, Question, Round) ->
    gleam@io:println(<<""/utf8>>),
    gleam@io:print(
        <<<<"Q"/utf8,
                (gleam@string:inspect(erlang:element(6, Question)))/binary>>/binary,
            ": "/utf8>>
    ),
    gleam@io:println(erlang:element(7, Question)),
    case gleam@string:length(erlang:element(8, Question)) > 0 of
        true ->
            gleam@io:println(
                <<"   Context: "/utf8, (erlang:element(8, Question))/binary>>
            );

        false ->
            nil
    end,
    case gleam@string:length(erlang:element(9, Question)) > 0 of
        true ->
            gleam@io:println(
                <<"   Example: "/utf8, (erlang:element(9, Question))/binary>>
            );

        false ->
            nil
    end,
    gleam@io:print(<<""/utf8>>),
    Answer_text = case intent@stdin:prompt_for_answer(<<"> "/utf8>>) of
        {ok, Text} ->
            Text;

        {error, Err} ->
            gleam@io:println_error(<<"Error reading input: "/utf8, Err/binary>>),
            gleam@io:println(<<""/utf8>>),
            <<"(input error - please try again)"/utf8>>
    end,
    Extracted = intent@interview:extract_from_answer(
        erlang:element(2, Question),
        Answer_text,
        erlang:element(11, Question)
    ),
    Confidence = intent@interview:calculate_confidence(
        erlang:element(2, Question),
        Answer_text,
        Extracted
    ),
    Answer = {answer,
        erlang:element(2, Question),
        erlang:element(7, Question),
        erlang:element(4, Question),
        Round,
        Answer_text,
        Extracted,
        Confidence,
        <<""/utf8>>,
        intent_ffi:current_timestamp()},
    Updated_session = intent@interview:add_answer(Session, Answer),
    {Sess_with_gaps, _} = intent@interview:check_for_gaps(
        Updated_session,
        Question,
        Answer
    ),
    {Sess_final, _} = intent@interview:check_for_conflicts(
        Sess_with_gaps,
        Answer
    ),
    Sess_final.

-spec ask_questions_in_round(
    intent@interview:interview_session(),
    integer(),
    intent@question_types:question()
) -> intent@interview:interview_session().
ask_questions_in_round(Session, Round, _) ->
    Profile_str = profile_to_string(erlang:element(3, Session)),
    Questions = intent@interview_questions:get_questions_for_round(
        Profile_str,
        Round
    ),
    Answered_ids = gleam@list:map(
        erlang:element(9, Session),
        fun(A) -> erlang:element(2, A) end
    ),
    Unanswered = gleam@list:filter(
        Questions,
        fun(Q) ->
            not gleam@list:contains(Answered_ids, erlang:element(2, Q))
        end
    ),
    gleam@list:fold(
        Unanswered,
        Session,
        fun(Sess, Question) -> ask_single_question(Sess, Question, Round) end
    ).

-spec interview_loop(intent@interview:interview_session(), integer()) -> intent@interview:interview_session().
interview_loop(Session, Round) ->
    case Round > 5 of
        true ->
            Session;

        false ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(
                <<"═══════════════════════════════════════════════════════════════════"/utf8>>
            ),
            gleam@io:println(
                <<<<"ROUND "/utf8, (gleam@string:inspect(Round))/binary>>/binary,
                    "/5"/utf8>>
            ),
            gleam@io:println(
                <<"═══════════════════════════════════════════════════════════════════"/utf8>>
            ),
            gleam@io:println(<<""/utf8>>),
            case intent@interview:get_first_question_for_round(Session, Round) of
                {error, _} ->
                    gleam@io:println(<<"(No questions for this round)"/utf8>>),
                    interview_loop(Session, Round + 1);

                {ok, First_question} ->
                    Updated_session = ask_questions_in_round(
                        Session,
                        Round,
                        First_question
                    ),
                    Blocking_gaps = intent@interview:get_blocking_gaps(
                        Updated_session
                    ),
                    case Blocking_gaps of
                        [] ->
                            interview_loop(Updated_session, Round + 1);

                        Gaps ->
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<"⚠️ BLOCKING GAPS DETECTED:"/utf8>>
                            ),
                            gleam@list:each(
                                Gaps,
                                fun(Gap) ->
                                    gleam@io:println(
                                        <<"  • "/utf8,
                                            (erlang:element(4, Gap))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"    "/utf8,
                                            (erlang:element(7, Gap))/binary>>
                                    )
                                end
                            ),
                            gleam@io:println(<<""/utf8>>),
                            interview_loop(Updated_session, Round + 1)
                    end
            end
    end.

-spec run_interview(intent@interview:profile(), binary(), boolean(), binary()) -> nil.
run_interview(Profile, Answers_file, _, Export_to) ->
    Session_id = <<"interview-"/utf8, (intent_ffi:generate_uuid())/binary>>,
    Timestamp = intent_ffi:current_timestamp(),
    Session = intent@interview:create_session(Session_id, Profile, Timestamp),
    Answers_dict = case gleam@string:is_empty(Answers_file) of
        true ->
            none;

        false ->
            gleam@io:println(
                <<"⚠ Answer file loading not yet implemented"/utf8>>
            ),
            gleam@io:println(<<"  Continuing in interactive mode..."/utf8>>),
            none
    end,
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
    ),
    gleam@io:println(<<"                    INTENT INTERVIEW"/utf8>>),
    gleam@io:println(
        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
    ),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"Profile: "/utf8, (profile_to_display_string(Profile))/binary>>
    ),
    gleam@io:println(<<"Session: "/utf8, Session_id/binary>>),
    case Answers_dict of
        none ->
            nil;

        {some, _} ->
            gleam@io:println(
                <<"Mode: Non-interactive (answers from file)"/utf8>>
            )
    end,
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"This guided interview will help us discover and refine your"/utf8>>
    ),
    gleam@io:println(<<"specification through structured questioning."/utf8>>),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"We'll ask questions across 5 rounds × multiple perspectives:"/utf8>>
    ),
    gleam@io:println(
        <<"  • Round 1: Core Intent (what are you building?)"/utf8>>
    ),
    gleam@io:println(
        <<"  • Round 2: Scope & Boundaries (what's in/out?)"/utf8>>
    ),
    gleam@io:println(<<"  • Round 3: Error Cases (what can go wrong?)"/utf8>>),
    gleam@io:println(
        <<"  • Round 4: Security & Compliance (how do we keep it safe?)"/utf8>>
    ),
    gleam@io:println(
        <<"  • Round 5: Operations (how does it run in production?)"/utf8>>
    ),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"Press Ctrl+C to save and exit at any time."/utf8>>),
    gleam@io:println(
        <<"Session will be saved to: .interview/sessions.jsonl"/utf8>>
    ),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"Ready? Let's begin."/utf8>>),
    gleam@io:println(<<""/utf8>>),
    Final_session = interview_loop(Session, 1),
    Save_result = intent@interview_storage:append_session_to_jsonl(
        Final_session,
        <<".interview/sessions.jsonl"/utf8>>
    ),
    case Save_result of
        {ok, nil} ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"✓ Session saved: "/utf8, Session_id/binary>>);

        {error, Err} ->
            gleam@io:println_error(
                <<"✗ Failed to save session: "/utf8, Err/binary>>
            )
    end,
    case Export_to of
        <<""/utf8>> ->
            nil;

        Path ->
            Spec_cue = intent@spec_builder:build_spec_from_session(
                Final_session
            ),
            case simplifile:write(Path, Spec_cue) of
                {ok, nil} ->
                    gleam@io:println(
                        <<"✓ Spec exported to: "/utf8, Path/binary>>
                    );

                {error, Err@1} ->
                    gleam@io:println_error(
                        <<"✗ Failed to export spec: "/utf8,
                            (gleam@string:inspect(Err@1))/binary>>
                    )
            end
    end,
    intent_ffi:halt(0).

-spec sessions_command() -> glint:command(nil).
sessions_command() ->
    _pipe@2 = glint:command(
        fun(Input) ->
            Jsonl_path = <<".interview/sessions.jsonl"/utf8>>,
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            Profile_filter = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"profile"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            case intent@interview_storage:list_sessions_from_jsonl(Jsonl_path) of
                {error, _} ->
                    intent@cli_ui:print_warning(
                        <<"No interview sessions found"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(<<"Start a new interview with:"/utf8>>),
                    gleam@io:println(
                        <<"  intent interview --profile api"/utf8>>
                    ),
                    intent_ffi:halt(0);

                {ok, []} ->
                    intent@cli_ui:print_warning(
                        <<"No interview sessions found"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(<<"Start a new interview with:"/utf8>>),
                    gleam@io:println(
                        <<"  intent interview --profile api"/utf8>>
                    ),
                    intent_ffi:halt(0);

                {ok, Sessions} ->
                    Filtered = case Profile_filter of
                        <<""/utf8>> ->
                            Sessions;

                        P ->
                            gleam@list:filter(
                                Sessions,
                                fun(S) ->
                                    profile_to_string(erlang:element(3, S)) =:= gleam@string:lowercase(
                                        P
                                    )
                                end
                            )
                    end,
                    case Is_json of
                        true ->
                            Json_sessions = gleam@json:array(
                                Filtered,
                                fun intent@interview_storage:session_to_json/1
                            ),
                            gleam@io:println(
                                gleam@json:to_string(Json_sessions)
                            );

                        false ->
                            intent@cli_ui:print_header(
                                <<"Interview Sessions"/utf8>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            gleam@list:each(
                                Filtered,
                                fun(Session) ->
                                    Status_icon = case erlang:element(
                                        7,
                                        Session
                                    ) of
                                        complete ->
                                            <<"✓"/utf8>>;

                                        paused ->
                                            <<"⏸"/utf8>>;

                                        _ ->
                                            <<"●"/utf8>>
                                    end,
                                    gleam@io:println(
                                        <<<<Status_icon/binary, " "/utf8>>/binary,
                                            (erlang:element(2, Session))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"  Profile: "/utf8,
                                            (profile_to_display_string(
                                                erlang:element(3, Session)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"  Stage: "/utf8,
                                            (stage_to_display_string(
                                                erlang:element(7, Session)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<<<"  Rounds: "/utf8,
                                                (gleam@string:inspect(
                                                    erlang:element(8, Session)
                                                ))/binary>>/binary,
                                            "/5"/utf8>>
                                    ),
                                    gleam@io:println(
                                        <<"  Answers: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:length(
                                                    erlang:element(9, Session)
                                                )
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"  Created: "/utf8,
                                            (erlang:element(4, Session))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"  Updated: "/utf8,
                                            (erlang:element(5, Session))/binary>>
                                    ),
                                    gleam@io:println(<<""/utf8>>)
                                end
                            ),
                            gleam@io:println(
                                <<<<"Total: "/utf8,
                                        (gleam@string:inspect(
                                            erlang:length(Filtered)
                                        ))/binary>>/binary,
                                    " session(s)"/utf8>>
                            )
                    end,
                    intent_ffi:halt(0)
            end
        end
    ),
    _pipe@3 = glint:description(_pipe@2, <<"List all interview sessions"/utf8>>),
    _pipe@6 = glint:flag(
        _pipe@3,
        <<"json"/utf8>>,
        begin
            _pipe@4 = glint@flag:bool(),
            _pipe@5 = glint@flag:default(_pipe@4, false),
            glint@flag:description(_pipe@5, <<"Output as JSON"/utf8>>)
        end
    ),
    glint:flag(
        _pipe@6,
        <<"profile"/utf8>>,
        begin
            _pipe@7 = glint@flag:string(),
            _pipe@8 = glint@flag:default(_pipe@7, <<""/utf8>>),
            glint@flag:description(
                _pipe@8,
                <<"Filter by profile (api, cli, event, etc.)"/utf8>>
            )
        end
    ).

-spec run_check(
    binary(),
    binary(),
    boolean(),
    binary(),
    binary(),
    intent@runner:output_level()
) -> nil.
run_check(
    Spec_path,
    Target_url,
    Is_json,
    Feature_filter,
    Only_filter,
    Output_level
) ->
    case intent@loader:load_spec(Spec_path) of
        {error, E} ->
            intent@cli_ui:print_error(intent@loader:format_error(E)),
            intent_ffi:halt(3);

        {ok, Spec} ->
            intent@cli_ui:print_header(
                <<"Checking spec: "/utf8, (erlang:element(2, Spec))/binary>>
            ),
            Options = {run_options, case Feature_filter of
                    <<""/utf8>> ->
                        none;

                    F ->
                        {some, F}
                end, case Only_filter of
                    <<""/utf8>> ->
                        none;

                    B ->
                        {some, B}
                end, Output_level},
            Result = intent@runner:run_spec(Spec, Target_url, Options),
            case Is_json of
                true ->
                    Json_result = intent@output:spec_result_to_json(Result),
                    gleam@io:println(gleam@json:to_string(Json_result));

                false ->
                    gleam@io:println(intent@output:spec_result_to_text(Result))
            end,
            Exit_code = case Result of
                {spec_result, true, _, _, _, _, _, _, _, _, _} ->
                    intent@cli_ui:print_success(<<"All checks passed!"/utf8>>),
                    0;

                {spec_result, _, _, _, Blocked, _, _, _, _, _, _} when Blocked > 0 ->
                    intent@cli_ui:print_warning(
                        <<"Blocked behaviors detected"/utf8>>
                    ),
                    2;

                _ ->
                    intent@cli_ui:print_error(<<"Check failed"/utf8>>),
                    1
            end,
            intent_ffi:halt(Exit_code)
    end.

-spec check_command() -> glint:command(nil).
check_command() ->
    _pipe@4 = glint:command(
        fun(Input) ->
            Target_url = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"target"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<""/utf8>>)
            end,
            Is_json = begin
                _pipe@1 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, false)
            end,
            Feature_filter = begin
                _pipe@2 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"feature"/utf8>>
                ),
                gleam@result:unwrap(_pipe@2, <<""/utf8>>)
            end,
            Only_filter = begin
                _pipe@3 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"only"/utf8>>
                ),
                gleam@result:unwrap(_pipe@3, <<""/utf8>>)
            end,
            Output_level = case glint@flag:get_bool(
                erlang:element(3, Input),
                <<"verbose"/utf8>>
            ) of
                {ok, true} ->
                    verbose;

                _ ->
                    case glint@flag:get_bool(
                        erlang:element(3, Input),
                        <<"quiet"/utf8>>
                    ) of
                        {ok, true} ->
                            quiet;

                        _ ->
                            normal
                    end
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    run_check(
                        Spec_path,
                        Target_url,
                        Is_json,
                        Feature_filter,
                        Only_filter,
                        Output_level
                    );

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent check <spec.cue> --target <url>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@5 = glint:description(
        _pipe@4,
        <<"Run spec against a target URL and verify behaviors"/utf8>>
    ),
    _pipe@8 = glint:flag(
        _pipe@5,
        <<"target"/utf8>>,
        begin
            _pipe@6 = glint@flag:string(),
            _pipe@7 = glint@flag:default(_pipe@6, <<""/utf8>>),
            glint@flag:description(
                _pipe@7,
                <<"Target base URL to test against"/utf8>>
            )
        end
    ),
    _pipe@11 = glint:flag(
        _pipe@8,
        <<"json"/utf8>>,
        begin
            _pipe@9 = glint@flag:bool(),
            _pipe@10 = glint@flag:default(_pipe@9, false),
            glint@flag:description(_pipe@10, <<"Output results as JSON"/utf8>>)
        end
    ),
    _pipe@14 = glint:flag(
        _pipe@11,
        <<"feature"/utf8>>,
        begin
            _pipe@12 = glint@flag:string(),
            _pipe@13 = glint@flag:default(_pipe@12, <<""/utf8>>),
            glint@flag:description(
                _pipe@13,
                <<"Filter to a specific feature"/utf8>>
            )
        end
    ),
    _pipe@17 = glint:flag(
        _pipe@14,
        <<"only"/utf8>>,
        begin
            _pipe@15 = glint@flag:string(),
            _pipe@16 = glint@flag:default(_pipe@15, <<""/utf8>>),
            glint@flag:description(
                _pipe@16,
                <<"Run only a specific behavior"/utf8>>
            )
        end
    ),
    _pipe@20 = glint:flag(
        _pipe@17,
        <<"verbose"/utf8>>,
        begin
            _pipe@18 = glint@flag:bool(),
            _pipe@19 = glint@flag:default(_pipe@18, false),
            glint@flag:description(_pipe@19, <<"Verbose output"/utf8>>)
        end
    ),
    glint:flag(
        _pipe@20,
        <<"quiet"/utf8>>,
        begin
            _pipe@21 = glint@flag:bool(),
            _pipe@22 = glint@flag:default(_pipe@21, false),
            glint@flag:description(
                _pipe@22,
                <<"Quiet output (errors only)"/utf8>>
            )
        end
    ).

-spec validate_command() -> glint:command(nil).
validate_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:validate_cue(Spec_path) of
                        {ok, _} ->
                            intent@cli_ui:print_success(
                                <<"Valid spec: "/utf8, Spec_path/binary>>
                            ),
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                <<"Invalid spec: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent validate <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Validate a CUE spec file without running tests"/utf8>>
    ).

-spec show_command() -> glint:command(nil).
show_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case Is_json of
                        true ->
                            case intent@loader:export_spec_json(Spec_path) of
                                {ok, Json_str} ->
                                    gleam@io:println(Json_str),
                                    intent_ffi:halt(0);

                                {error, E} ->
                                    gleam@io:println_error(
                                        <<"Error: "/utf8,
                                            (intent@loader:format_error(E))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end;

                        false ->
                            case intent@loader:load_spec(Spec_path) of
                                {ok, Spec} ->
                                    print_spec_summary(Spec),
                                    intent_ffi:halt(0);

                                {error, E@1} ->
                                    gleam@io:println_error(
                                        <<"Error: "/utf8,
                                            (intent@loader:format_error(E@1))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent show <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(_pipe@1, <<"Pretty print a parsed spec"/utf8>>),
    glint:flag(
        _pipe@2,
        <<"json"/utf8>>,
        begin
            _pipe@3 = glint@flag:bool(),
            _pipe@4 = glint@flag:default(_pipe@3, false),
            glint@flag:description(_pipe@4, <<"Output as JSON"/utf8>>)
        end
    ).

-spec export_command() -> glint:command(nil).
export_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:export_spec_json(Spec_path) of
                        {ok, Json_str} ->
                            gleam@io:println(Json_str),
                            intent_ffi:halt(0);

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(4)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent export <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(_pipe, <<"Export spec to JSON format"/utf8>>).

-spec lint_command() -> glint:command(nil).
lint_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Lint_result = intent@spec_linter:lint_spec(Spec),
                            case Lint_result of
                                lint_valid ->
                                    gleam@io:println(
                                        <<"✓ Spec is well-formed - no linting issues found"/utf8>>
                                    ),
                                    intent_ffi:halt(0);

                                {lint_warnings, Warnings} ->
                                    gleam@io:println(
                                        intent@spec_linter:format_warnings(
                                            Warnings
                                        )
                                    ),
                                    intent_ffi:halt(1)
                            end;

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent lint <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Check spec for anti-patterns and quality issues"/utf8>>
    ).

-spec analyze_command() -> glint:command(nil).
analyze_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@quality_analyzer:analyze_spec(Spec),
                            gleam@io:println(
                                intent@quality_analyzer:format_report(Report)
                            ),
                            intent_ffi:halt(0);

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent analyze <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Analyze spec quality and provide improvement suggestions"/utf8>>
    ).

-spec improve_command() -> glint:command(nil).
improve_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Quality_report = intent@quality_analyzer:analyze_spec(
                                Spec
                            ),
                            Lint_result = intent@spec_linter:lint_spec(Spec),
                            Context = {improvement_context,
                                Quality_report,
                                Lint_result,
                                Spec},
                            Suggestions = intent@improver:suggest_improvements(
                                Context
                            ),
                            gleam@io:println(
                                intent@improver:format_improvements(Suggestions)
                            ),
                            intent_ffi:halt(0);

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent improve <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Suggest improvements based on quality analysis and linting"/utf8>>
    ).

-spec run_resume_interview(binary(), binary()) -> nil.
run_resume_interview(Session_id, Export_to) ->
    Jsonl_path = <<".interview/sessions.jsonl"/utf8>>,
    case intent@interview_storage:get_session_from_jsonl(Jsonl_path, Session_id) of
        {error, Err} ->
            intent@cli_ui:print_error(Err),
            intent_ffi:halt(4);

        {ok, Session} ->
            intent@cli_ui:print_header(
                <<"Resuming Interview: "/utf8,
                    (erlang:element(2, Session))/binary>>
            ),
            intent@cli_ui:print_info(
                <<"Profile: "/utf8,
                    (profile_to_display_string(erlang:element(3, Session)))/binary>>
            ),
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"Progress:"/utf8>>),
            gleam@io:println(
                <<"  • Answers collected: "/utf8,
                    (gleam@string:inspect(
                        erlang:length(erlang:element(9, Session))
                    ))/binary>>
            ),
            gleam@io:println(
                <<"  • Gaps detected: "/utf8,
                    (gleam@string:inspect(
                        erlang:length(erlang:element(10, Session))
                    ))/binary>>
            ),
            gleam@io:println(
                <<"  • Conflicts detected: "/utf8,
                    (gleam@string:inspect(
                        erlang:length(erlang:element(11, Session))
                    ))/binary>>
            ),
            gleam@io:println(<<""/utf8>>),
            Next_round = case erlang:element(8, Session) of
                0 ->
                    1;

                R when R < 5 ->
                    R + 1;

                _ ->
                    5
            end,
            gleam@io:println(
                <<"Resuming from Round "/utf8,
                    (gleam@string:inspect(Next_round))/binary>>
            ),
            gleam@io:println(<<""/utf8>>),
            Final_session = interview_loop(Session, Next_round),
            Save_result = intent@interview_storage:append_session_to_jsonl(
                Final_session,
                Jsonl_path
            ),
            case Save_result of
                {ok, nil} ->
                    gleam@io:println(<<""/utf8>>),
                    intent@cli_ui:print_success(
                        <<"Session updated: "/utf8,
                            (erlang:element(2, Session))/binary>>
                    );

                {error, Err@1} ->
                    intent@cli_ui:print_error(
                        <<"Failed to save session: "/utf8, Err@1/binary>>
                    )
            end,
            case Export_to of
                <<""/utf8>> ->
                    nil;

                Path ->
                    Spec_cue = intent@spec_builder:build_spec_from_session(
                        Final_session
                    ),
                    case simplifile:write(Path, Spec_cue) of
                        {ok, nil} ->
                            intent@cli_ui:print_success(
                                <<"Spec exported to: "/utf8, Path/binary>>
                            );

                        {error, Err@2} ->
                            intent@cli_ui:print_error(
                                <<"Failed to export spec: "/utf8,
                                    (gleam@string:inspect(Err@2))/binary>>
                            )
                    end
            end,
            intent_ffi:halt(0)
    end.

-spec interview_command() -> glint:command(nil).
interview_command() ->
    _pipe@5 = glint:command(
        fun(Input) ->
            Profile_str = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"profile"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<"api"/utf8>>)
            end,
            Resume_id = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"resume"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            Export_to = begin
                _pipe@2 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"export"/utf8>>
                ),
                gleam@result:unwrap(_pipe@2, <<""/utf8>>)
            end,
            Answers_file = begin
                _pipe@3 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"answers"/utf8>>
                ),
                gleam@result:unwrap(_pipe@3, <<""/utf8>>)
            end,
            Strict_mode = begin
                _pipe@4 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"strict"/utf8>>
                ),
                gleam@result:unwrap(_pipe@4, false)
            end,
            case Resume_id of
                <<""/utf8>> ->
                    case gleam@string:lowercase(Profile_str) of
                        <<"api"/utf8>> ->
                            run_interview(
                                api,
                                Answers_file,
                                Strict_mode,
                                Export_to
                            );

                        <<"cli"/utf8>> ->
                            run_interview(
                                cli,
                                Answers_file,
                                Strict_mode,
                                Export_to
                            );

                        <<"event"/utf8>> ->
                            run_interview(
                                event,
                                Answers_file,
                                Strict_mode,
                                Export_to
                            );

                        <<"data"/utf8>> ->
                            run_interview(
                                data,
                                Answers_file,
                                Strict_mode,
                                Export_to
                            );

                        <<"workflow"/utf8>> ->
                            run_interview(
                                workflow,
                                Answers_file,
                                Strict_mode,
                                Export_to
                            );

                        <<"ui"/utf8>> ->
                            run_interview(
                                ui,
                                Answers_file,
                                Strict_mode,
                                Export_to
                            );

                        _ ->
                            gleam@io:println_error(
                                <<<<"Error: unknown profile '"/utf8,
                                        Profile_str/binary>>/binary,
                                    "'"/utf8>>
                            ),
                            gleam@io:println_error(
                                <<"Valid profiles: api, cli, event, data, workflow, ui"/utf8>>
                            ),
                            intent_ffi:halt(4)
                    end;

                Id ->
                    run_resume_interview(Id, Export_to)
            end
        end
    ),
    _pipe@6 = glint:description(
        _pipe@5,
        <<"Guided specification discovery through structured interview"/utf8>>
    ),
    _pipe@9 = glint:flag(
        _pipe@6,
        <<"profile"/utf8>>,
        begin
            _pipe@7 = glint@flag:string(),
            _pipe@8 = glint@flag:default(_pipe@7, <<"api"/utf8>>),
            glint@flag:description(
                _pipe@8,
                <<"System profile: api, cli, event, data, workflow, or ui"/utf8>>
            )
        end
    ),
    _pipe@12 = glint:flag(
        _pipe@9,
        <<"resume"/utf8>>,
        begin
            _pipe@10 = glint@flag:string(),
            _pipe@11 = glint@flag:default(_pipe@10, <<""/utf8>>),
            glint@flag:description(
                _pipe@11,
                <<"Resume existing interview session by ID"/utf8>>
            )
        end
    ),
    _pipe@15 = glint:flag(
        _pipe@12,
        <<"answers"/utf8>>,
        begin
            _pipe@13 = glint@flag:string(),
            _pipe@14 = glint@flag:default(_pipe@13, <<""/utf8>>),
            glint@flag:description(
                _pipe@14,
                <<"Path to CUE file with pre-filled answers for non-interactive mode"/utf8>>
            )
        end
    ),
    _pipe@18 = glint:flag(
        _pipe@15,
        <<"strict"/utf8>>,
        begin
            _pipe@16 = glint@flag:bool(),
            _pipe@17 = glint@flag:default(_pipe@16, false),
            glint@flag:description(
                _pipe@17,
                <<"Strict mode: fail if answers file is missing required answers (requires --answers)"/utf8>>
            )
        end
    ),
    glint:flag(
        _pipe@18,
        <<"export"/utf8>>,
        begin
            _pipe@19 = glint@flag:string(),
            _pipe@20 = glint@flag:default(_pipe@19, <<""/utf8>>),
            glint@flag:description(
                _pipe@20,
                <<"Export completed interview to spec file"/utf8>>
            )
        end
    ).

-spec beads_command() -> glint:command(nil).
beads_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Session_id | _] ->
                    case intent@interview_storage:get_session_from_jsonl(
                        <<".interview/sessions.jsonl"/utf8>>,
                        Session_id
                    ) of
                        {error, Err} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8, Err/binary>>
                            ),
                            intent_ffi:halt(4);

                        {ok, Session} ->
                            Beads = intent@bead_templates:generate_beads_from_session(
                                Session
                            ),
                            Bead_count = erlang:length(Beads),
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<"═══════════════════════════════════════════════════════════════════"/utf8>>
                            ),
                            gleam@io:println(
                                <<"                    BEAD GENERATION"/utf8>>
                            ),
                            gleam@io:println(
                                <<"═══════════════════════════════════════════════════════════════════"/utf8>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<<<<<"Generated "/utf8,
                                            (gleam@string:inspect(Bead_count))/binary>>/binary,
                                        " work items from session: "/utf8>>/binary,
                                    Session_id/binary>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            Jsonl_output = intent@bead_templates:beads_to_jsonl(
                                Beads
                            ),
                            case simplifile:append(
                                <<".beads/issues.jsonl"/utf8>>,
                                <<Jsonl_output/binary, "\n"/utf8>>
                            ) of
                                {ok, nil} ->
                                    gleam@io:println(
                                        <<"✓ Beads exported to: .beads/issues.jsonl"/utf8>>
                                    ),
                                    gleam@io:println(<<""/utf8>>),
                                    Stats = intent@bead_templates:bead_stats(
                                        Beads
                                    ),
                                    gleam@io:println(<<"Summary:"/utf8>>),
                                    gleam@io:println(
                                        <<"  Total beads: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:element(2, Stats)
                                            ))/binary>>
                                    ),
                                    intent_ffi:halt(0);

                                {error, Err@1} ->
                                    gleam@io:println_error(
                                        <<"✗ Failed to write beads: "/utf8,
                                            (gleam@string:inspect(Err@1))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Usage: intent beads <session_id>"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(
                        <<"Example: intent beads interview-abc123def456"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Generate work items (beads) from an interview session"/utf8>>
    ).

-spec bead_status_command() -> glint:command(nil).
bead_status_command() ->
    _pipe@4 = glint:command(
        fun(Input) ->
            Bead_id = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"bead-id"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<""/utf8>>)
            end,
            Status = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"status"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            Reason = begin
                _pipe@2 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"reason"/utf8>>
                ),
                gleam@result:unwrap(_pipe@2, <<""/utf8>>)
            end,
            Session_id = begin
                _pipe@3 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"session"/utf8>>
                ),
                gleam@result:unwrap(_pipe@3, <<""/utf8>>)
            end,
            case gleam@string:is_empty(Bead_id) of
                true ->
                    gleam@io:println_error(
                        <<"Usage: intent bead-status --bead-id <id> --status success|failed|blocked [--reason 'text'] [--session <id>]"/utf8>>
                    ),
                    intent_ffi:halt(4);

                false ->
                    case Status of
                        <<"success"/utf8>> ->
                            case intent@bead_feedback:mark_bead_executed(
                                Session_id,
                                Bead_id,
                                success,
                                Reason,
                                0
                            ) of
                                {ok, nil} ->
                                    gleam@io:println(
                                        <<<<"✓ Bead "/utf8, Bead_id/binary>>/binary,
                                            " marked as success"/utf8>>
                                    ),
                                    intent_ffi:halt(0);

                                {error, Err} ->
                                    gleam@io:println_error(
                                        <<"✗ Failed to mark bead: "/utf8,
                                            (bead_feedback_error_to_string(Err))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end;

                        <<"failed"/utf8>> ->
                            case intent@bead_feedback:mark_bead_failed(
                                Session_id,
                                Bead_id,
                                Reason,
                                <<"execution_error"/utf8>>,
                                <<"Bead execution failed"/utf8>>,
                                none,
                                0
                            ) of
                                {ok, nil} ->
                                    gleam@io:println(
                                        <<<<"✓ Bead "/utf8, Bead_id/binary>>/binary,
                                            " marked as failed"/utf8>>
                                    ),
                                    intent_ffi:halt(0);

                                {error, Err@1} ->
                                    gleam@io:println_error(
                                        <<"✗ Failed to mark bead: "/utf8,
                                            (bead_feedback_error_to_string(
                                                Err@1
                                            ))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end;

                        <<"blocked"/utf8>> ->
                            case gleam@string:is_empty(Reason) of
                                true ->
                                    gleam@io:println_error(
                                        <<"Error: --status blocked requires --reason"/utf8>>
                                    ),
                                    intent_ffi:halt(4);

                                false ->
                                    case intent@bead_feedback:mark_bead_blocked(
                                        Session_id,
                                        Bead_id,
                                        Reason,
                                        <<"user_action"/utf8>>,
                                        <<"User blocked this bead"/utf8>>,
                                        <<"Manual resume required"/utf8>>,
                                        0
                                    ) of
                                        {ok, nil} ->
                                            gleam@io:println(
                                                <<<<<<"✓ Bead "/utf8,
                                                            Bead_id/binary>>/binary,
                                                        " marked as blocked: "/utf8>>/binary,
                                                    Reason/binary>>
                                            ),
                                            intent_ffi:halt(0);

                                        {error, Err@2} ->
                                            gleam@io:println_error(
                                                <<"✗ Failed to mark bead: "/utf8,
                                                    (bead_feedback_error_to_string(
                                                        Err@2
                                                    ))/binary>>
                                            ),
                                            intent_ffi:halt(4)
                                    end
                            end;

                        _ ->
                            gleam@io:println_error(
                                <<<<"Error: invalid status '"/utf8,
                                        Status/binary>>/binary,
                                    "'"/utf8>>
                            ),
                            gleam@io:println_error(
                                <<"Valid statuses: success, failed, blocked"/utf8>>
                            ),
                            intent_ffi:halt(4)
                    end
            end
        end
    ),
    _pipe@5 = glint:description(
        _pipe@4,
        <<"Mark bead execution status (success/failed/blocked)"/utf8>>
    ),
    _pipe@8 = glint:flag(
        _pipe@5,
        <<"bead-id"/utf8>>,
        begin
            _pipe@6 = glint@flag:string(),
            _pipe@7 = glint@flag:default(_pipe@6, <<""/utf8>>),
            glint@flag:description(_pipe@7, <<"Bead ID (required)"/utf8>>)
        end
    ),
    _pipe@11 = glint:flag(
        _pipe@8,
        <<"status"/utf8>>,
        begin
            _pipe@9 = glint@flag:string(),
            _pipe@10 = glint@flag:default(_pipe@9, <<""/utf8>>),
            glint@flag:description(
                _pipe@10,
                <<"Status: success, failed, or blocked (required)"/utf8>>
            )
        end
    ),
    _pipe@14 = glint:flag(
        _pipe@11,
        <<"reason"/utf8>>,
        begin
            _pipe@12 = glint@flag:string(),
            _pipe@13 = glint@flag:default(_pipe@12, <<""/utf8>>),
            glint@flag:description(
                _pipe@13,
                <<"Reason for status (required for blocked)"/utf8>>
            )
        end
    ),
    glint:flag(
        _pipe@14,
        <<"session"/utf8>>,
        begin
            _pipe@15 = glint@flag:string(),
            _pipe@16 = glint@flag:default(_pipe@15, <<""/utf8>>),
            glint@flag:description(_pipe@16, <<"Session ID"/utf8>>)
        end
    ).

-spec plan_command() -> glint:command(nil).
plan_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Format = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"format"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<"human"/utf8>>)
            end,
            case erlang:element(2, Input) of
                [Session_id | _] ->
                    case intent@plan_mode:compute_plan(Session_id) of
                        {error, Err} ->
                            gleam@io:println_error(
                                intent@plan_mode:format_error(Err)
                            ),
                            intent_ffi:halt(4);

                        {ok, Plan} ->
                            Output = case Format of
                                <<"json"/utf8>> ->
                                    intent@plan_mode:format_plan_json(Plan);

                                _ ->
                                    intent@plan_mode:format_plan_human(Plan)
                            end,
                            gleam@io:println(Output),
                            intent_ffi:halt(0)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Usage: intent plan <session_id> [--format human|json]"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(
                        <<"Display execution plan from session beads."/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(<<"Examples:"/utf8>>),
                    gleam@io:println_error(
                        <<"  intent plan abc123              # Human-readable output"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  intent plan abc123 --format json  # JSON output"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(
        _pipe@1,
        <<"Display execution plan from session beads"/utf8>>
    ),
    glint:flag(
        _pipe@2,
        <<"format"/utf8>>,
        begin
            _pipe@3 = glint@flag:string(),
            _pipe@4 = glint@flag:default(_pipe@3, <<"human"/utf8>>),
            glint@flag:description(
                _pipe@4,
                <<"Output format: human or json"/utf8>>
            )
        end
    ).

-spec plan_approve_command() -> glint:command(nil).
plan_approve_command() ->
    _pipe@2 = glint:command(
        fun(Input) ->
            Auto_approve = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"yes"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            Notes = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"notes"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            case erlang:element(2, Input) of
                [Session_id | _] ->
                    case intent@plan_mode:compute_plan(Session_id) of
                        {error, Err} ->
                            gleam@io:println_error(
                                intent@plan_mode:format_error(Err)
                            ),
                            intent_ffi:halt(4);

                        {ok, Plan} ->
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<"═══════════════════════════════════════════════════════════════════"/utf8>>
                            ),
                            gleam@io:println(
                                <<"                    PLAN APPROVAL"/utf8>>
                            ),
                            gleam@io:println(
                                <<"═══════════════════════════════════════════════════════════════════"/utf8>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<"Session: "/utf8,
                                    (erlang:element(2, Plan))/binary>>
                            ),
                            gleam@io:println(
                                <<"Total Beads: "/utf8,
                                    (gleam@string:inspect(
                                        erlang:element(5, Plan)
                                    ))/binary>>
                            ),
                            gleam@io:println(
                                <<"Total Effort: "/utf8,
                                    (erlang:element(6, Plan))/binary>>
                            ),
                            gleam@io:println(
                                <<"Risk Level: "/utf8,
                                    (risk_level_to_string(
                                        erlang:element(7, Plan)
                                    ))/binary>>
                            ),
                            gleam@io:println(
                                <<"Phases: "/utf8,
                                    (gleam@string:inspect(
                                        erlang:length(erlang:element(4, Plan))
                                    ))/binary>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            case gleam@list:is_empty(erlang:element(8, Plan)) of
                                true ->
                                    nil;

                                false ->
                                    gleam@io:println(<<"⚠ BLOCKERS:"/utf8>>),
                                    gleam@list:each(
                                        erlang:element(8, Plan),
                                        fun(B) ->
                                            gleam@io:println(
                                                <<"  • "/utf8, B/binary>>
                                            )
                                        end
                                    ),
                                    gleam@io:println(<<""/utf8>>)
                            end,
                            case Auto_approve of
                                true ->
                                    case approve_plan(
                                        Session_id,
                                        <<"ci"/utf8>>,
                                        Notes
                                    ) of
                                        {ok, nil} ->
                                            gleam@io:println(
                                                <<"✓ Plan approved automatically (CI mode)"/utf8>>
                                            ),
                                            intent_ffi:halt(0);

                                        {error, Err@1} ->
                                            gleam@io:println_error(
                                                <<"✗ Failed to approve plan: "/utf8,
                                                    Err@1/binary>>
                                            ),
                                            intent_ffi:halt(4)
                                    end;

                                false ->
                                    gleam@io:println(
                                        <<"Approve this plan? (yes/no)"/utf8>>
                                    ),
                                    case intent_ffi_stdin:read_line() of
                                        {ok, Response} ->
                                            Cleaned = gleam@string:trim(
                                                gleam@string:lowercase(Response)
                                            ),
                                            case Cleaned of
                                                <<"yes"/utf8>> ->
                                                    case approve_plan(
                                                        Session_id,
                                                        <<"human"/utf8>>,
                                                        Notes
                                                    ) of
                                                        {ok, nil} ->
                                                            gleam@io:println(
                                                                <<"✓ Plan approved"/utf8>>
                                                            ),
                                                            intent_ffi:halt(0);

                                                        {error, Err@2} ->
                                                            gleam@io:println_error(
                                                                <<"✗ Failed to approve plan: "/utf8,
                                                                    Err@2/binary>>
                                                            ),
                                                            intent_ffi:halt(4)
                                                    end;

                                                <<"y"/utf8>> ->
                                                    case approve_plan(
                                                        Session_id,
                                                        <<"human"/utf8>>,
                                                        Notes
                                                    ) of
                                                        {ok, nil} ->
                                                            gleam@io:println(
                                                                <<"✓ Plan approved"/utf8>>
                                                            ),
                                                            intent_ffi:halt(0);

                                                        {error, Err@2} ->
                                                            gleam@io:println_error(
                                                                <<"✗ Failed to approve plan: "/utf8,
                                                                    Err@2/binary>>
                                                            ),
                                                            intent_ffi:halt(4)
                                                    end;

                                                <<"no"/utf8>> ->
                                                    gleam@io:println(
                                                        <<"Plan not approved"/utf8>>
                                                    ),
                                                    intent_ffi:halt(1);

                                                <<"n"/utf8>> ->
                                                    gleam@io:println(
                                                        <<"Plan not approved"/utf8>>
                                                    ),
                                                    intent_ffi:halt(1);

                                                _ ->
                                                    gleam@io:println_error(
                                                        <<"Invalid response. Please enter 'yes' or 'no'"/utf8>>
                                                    ),
                                                    intent_ffi:halt(4)
                                            end;

                                        {error, _} ->
                                            gleam@io:println_error(
                                                <<"Failed to read input"/utf8>>
                                            ),
                                            intent_ffi:halt(4)
                                    end
                            end
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Usage: intent plan-approve <session_id> [--yes] [--notes 'text']"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(
                        <<"Approve execution plan for a session."/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(<<"Flags:"/utf8>>),
                    gleam@io:println_error(
                        <<"  --yes      Auto-approve for CI pipelines (non-interactive)"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  --notes    Optional approval notes"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(<<"Examples:"/utf8>>),
                    gleam@io:println_error(
                        <<"  intent plan-approve abc123           # Interactive approval"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  intent plan-approve abc123 --yes     # CI auto-approval"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@3 = glint:description(
        _pipe@2,
        <<"Approve execution plan for session"/utf8>>
    ),
    _pipe@6 = glint:flag(
        _pipe@3,
        <<"yes"/utf8>>,
        begin
            _pipe@4 = glint@flag:bool(),
            _pipe@5 = glint@flag:default(_pipe@4, false),
            glint@flag:description(
                _pipe@5,
                <<"Auto-approve for CI (non-interactive)"/utf8>>
            )
        end
    ),
    glint:flag(
        _pipe@6,
        <<"notes"/utf8>>,
        begin
            _pipe@7 = glint@flag:string(),
            _pipe@8 = glint@flag:default(_pipe@7, <<""/utf8>>),
            glint@flag:description(_pipe@8, <<"Approval notes"/utf8>>)
        end
    ).

-spec beads_regenerate_command() -> glint:command(nil).
beads_regenerate_command() ->
    _pipe@2 = glint:command(
        fun(Input) ->
            Strategy = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"strategy"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<"hybrid"/utf8>>)
            end,
            case erlang:element(2, Input) of
                [Session_id | _] ->
                    Session_path = <<<<".intent/session-"/utf8,
                            Session_id/binary>>/binary,
                        ".cue"/utf8>>,
                    case simplifile:verify_is_file(Session_path) of
                        {error, _} ->
                            gleam@io:println_error(
                                <<"Session not found: "/utf8,
                                    Session_id/binary>>
                            ),
                            gleam@io:println_error(
                                <<"Expected file: "/utf8, Session_path/binary>>
                            ),
                            intent_ffi:halt(4);

                        {ok, _} ->
                            case intent@bead_feedback:load_feedback_for_session(
                                Session_id
                            ) of
                                {error, Err} ->
                                    gleam@io:println_error(
                                        <<"Failed to load feedback: "/utf8,
                                            (bead_feedback_error_to_string(Err))/binary>>
                                    ),
                                    intent_ffi:halt(4);

                                {ok, Feedback} ->
                                    Needs_regen = begin
                                        _pipe@1 = Feedback,
                                        gleam@list:filter(
                                            _pipe@1,
                                            fun(Fb) ->
                                                case erlang:element(3, Fb) of
                                                    failed ->
                                                        true;

                                                    blocked ->
                                                        true;

                                                    _ ->
                                                        false
                                                end
                                            end
                                        )
                                    end,
                                    gleam@io:println(<<""/utf8>>),
                                    gleam@io:println(
                                        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
                                    ),
                                    gleam@io:println(
                                        <<"                    BEAD REGENERATION"/utf8>>
                                    ),
                                    gleam@io:println(
                                        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
                                    ),
                                    gleam@io:println(<<""/utf8>>),
                                    gleam@io:println(
                                        <<"Session: "/utf8, Session_id/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"Strategy: "/utf8, Strategy/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"Feedback entries: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:length(Feedback)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"Beads needing regeneration: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:length(Needs_regen)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(<<""/utf8>>),
                                    case gleam@list:is_empty(Needs_regen) of
                                        true ->
                                            gleam@io:println(
                                                <<"✓ No beads need regeneration - all passed or skipped"/utf8>>
                                            ),
                                            intent_ffi:halt(0);

                                        false ->
                                            gleam@io:println(
                                                <<"Beads to regenerate:"/utf8>>
                                            ),
                                            gleam@list:each(
                                                Needs_regen,
                                                fun(Fb@1) ->
                                                    Status_icon = case erlang:element(
                                                        3,
                                                        Fb@1
                                                    ) of
                                                        failed ->
                                                            <<"✗"/utf8>>;

                                                        blocked ->
                                                            <<"⊘"/utf8>>;

                                                        _ ->
                                                            <<"?"/utf8>>
                                                    end,
                                                    gleam@io:println(
                                                        <<<<<<<<<<"  "/utf8,
                                                                            Status_icon/binary>>/binary,
                                                                        " "/utf8>>/binary,
                                                                    (erlang:element(
                                                                        2,
                                                                        Fb@1
                                                                    ))/binary>>/binary,
                                                                ": "/utf8>>/binary,
                                                            (erlang:element(
                                                                4,
                                                                Fb@1
                                                            ))/binary>>
                                                    )
                                                end
                                            ),
                                            gleam@io:println(<<""/utf8>>),
                                            Regen_entries = generate_regeneration_entries(
                                                Needs_regen,
                                                Strategy
                                            ),
                                            case append_regeneration_to_session(
                                                Session_path,
                                                Regen_entries
                                            ) of
                                                {ok, nil} ->
                                                    gleam@io:println(
                                                        <<"✓ Regeneration metadata added to session"/utf8>>
                                                    ),
                                                    gleam@io:println(
                                                        <<"  Strategy: "/utf8,
                                                            Strategy/binary>>
                                                    ),
                                                    gleam@io:println(
                                                        <<"  Beads marked for regeneration: "/utf8,
                                                            (gleam@string:inspect(
                                                                erlang:length(
                                                                    Needs_regen
                                                                )
                                                            ))/binary>>
                                                    ),
                                                    gleam@io:println(
                                                        <<""/utf8>>
                                                    ),
                                                    gleam@io:println(
                                                        <<"Next steps:"/utf8>>
                                                    ),
                                                    gleam@io:println(
                                                        <<"  1. Review regeneration suggestions in "/utf8,
                                                            Session_path/binary>>
                                                    ),
                                                    gleam@io:println(
                                                        <<<<"  2. Run 'intent plan "/utf8,
                                                                Session_id/binary>>/binary,
                                                            "' to see updated plan"/utf8>>
                                                    ),
                                                    gleam@io:println(
                                                        <<"  3. Execute regenerated beads"/utf8>>
                                                    ),
                                                    intent_ffi:halt(0);

                                                {error, Err@1} ->
                                                    gleam@io:println_error(
                                                        <<"✗ Failed to update session: "/utf8,
                                                            Err@1/binary>>
                                                    ),
                                                    intent_ffi:halt(4)
                                            end
                                    end
                            end
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Usage: intent beads-regenerate <session_id> [--strategy hybrid|inversion|premortem]"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(
                        <<"Regenerate failed/blocked beads with adjusted approach."/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(<<"Strategies:"/utf8>>),
                    gleam@io:println_error(
                        <<"  hybrid     - Use all analysis methods (default)"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  inversion  - Focus on failure mode analysis"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  premortem  - Focus on what could go wrong"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(<<"Examples:"/utf8>>),
                    gleam@io:println_error(
                        <<"  intent beads-regenerate abc123"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  intent beads-regenerate abc123 --strategy inversion"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@3 = glint:description(
        _pipe@2,
        <<"Regenerate failed/blocked beads with adjusted approach"/utf8>>
    ),
    glint:flag(
        _pipe@3,
        <<"strategy"/utf8>>,
        begin
            _pipe@4 = glint@flag:string(),
            _pipe@5 = glint@flag:default(_pipe@4, <<"hybrid"/utf8>>),
            glint@flag:description(
                _pipe@5,
                <<"Regeneration strategy: hybrid, inversion, or premortem"/utf8>>
            )
        end
    ).

-spec history_command() -> glint:command(nil).
history_command() ->
    _pipe = glint:command(
        fun(Input) ->
            History_path = <<".interview/history.jsonl"/utf8>>,
            case erlang:element(2, Input) of
                [Session_id | _] ->
                    case intent@interview_storage:list_session_history(
                        History_path,
                        Session_id
                    ) of
                        {error, Err} ->
                            intent@cli_ui:print_error(Err),
                            intent_ffi:halt(4);

                        {ok, []} ->
                            intent@cli_ui:print_warning(
                                <<"No history found for session: "/utf8,
                                    Session_id/binary>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<"Tip: Session history is recorded when you save snapshots"/utf8>>
                            ),
                            gleam@io:println(
                                <<"during an interview with --snapshot flag."/utf8>>
                            ),
                            intent_ffi:halt(0);

                        {ok, Snapshots} ->
                            intent@cli_ui:print_header(
                                <<"Session History: "/utf8, Session_id/binary>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            gleam@list:each(
                                Snapshots,
                                fun(Snapshot) ->
                                    gleam@io:println(
                                        <<"┌─ "/utf8,
                                            (erlang:element(3, Snapshot))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Time: "/utf8,
                                            (erlang:element(4, Snapshot))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Stage: "/utf8,
                                            (erlang:element(9, Snapshot))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Description: "/utf8,
                                            (erlang:element(5, Snapshot))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Answers: "/utf8,
                                            (gleam@string:inspect(
                                                maps:size(
                                                    erlang:element(6, Snapshot)
                                                )
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Gaps: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:element(7, Snapshot)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Conflicts: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:element(8, Snapshot)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(<<"└─"/utf8>>),
                                    gleam@io:println(<<""/utf8>>)
                                end
                            ),
                            intent_ffi:halt(0)
                    end;

                [] ->
                    intent@cli_ui:print_error(<<"Session ID required"/utf8>>),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Usage: intent history <session-id>"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Example: intent history interview-abc123"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    glint:description(
        _pipe,
        <<"View snapshot history for an interview session"/utf8>>
    ).

-spec diff_command() -> glint:command(nil).
diff_command() ->
    _pipe = glint:command(
        fun(Input) ->
            Jsonl_path = <<".interview/sessions.jsonl"/utf8>>,
            case erlang:element(2, Input) of
                [From_id, To_id | _] ->
                    case intent@interview_storage:get_session_from_jsonl(
                        Jsonl_path,
                        From_id
                    ) of
                        {error, Err} ->
                            intent@cli_ui:print_error(
                                <<"Failed to load 'from' session: "/utf8,
                                    Err/binary>>
                            ),
                            intent_ffi:halt(4);

                        {ok, From_session} ->
                            case intent@interview_storage:get_session_from_jsonl(
                                Jsonl_path,
                                To_id
                            ) of
                                {error, Err@1} ->
                                    intent@cli_ui:print_error(
                                        <<"Failed to load 'to' session: "/utf8,
                                            Err@1/binary>>
                                    ),
                                    intent_ffi:halt(4);

                                {ok, To_session} ->
                                    Diff = intent@interview_storage:diff_sessions(
                                        From_session,
                                        To_session
                                    ),
                                    intent@cli_ui:print_header(
                                        <<"Session Comparison"/utf8>>
                                    ),
                                    gleam@io:println(<<""/utf8>>),
                                    gleam@io:println(
                                        intent@interview_storage:format_diff(
                                            Diff
                                        )
                                    ),
                                    gleam@io:println(<<""/utf8>>),
                                    Total_changes = (erlang:length(
                                        erlang:element(6, Diff)
                                    )
                                    + erlang:length(erlang:element(7, Diff)))
                                    + erlang:length(erlang:element(8, Diff)),
                                    case Total_changes of
                                        0 ->
                                            intent@cli_ui:print_info(
                                                <<"No answer changes between sessions"/utf8>>
                                            );

                                        N ->
                                            intent@cli_ui:print_info(
                                                <<(gleam@string:inspect(N))/binary,
                                                    " total answer changes"/utf8>>
                                            )
                                    end,
                                    intent_ffi:halt(0)
                            end
                    end;

                [Single_id] ->
                    intent@cli_ui:print_error(
                        <<"Two session IDs required for comparison"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Usage: intent diff <from-session> <to-session>"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Tip: Use 'intent sessions' to list available sessions"/utf8>>
                    ),
                    gleam@io:println(
                        <<"     Session provided: "/utf8, Single_id/binary>>
                    ),
                    intent_ffi:halt(4);

                [] ->
                    intent@cli_ui:print_error(<<"Session IDs required"/utf8>>),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Usage: intent diff <from-session> <to-session>"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Compare two interview sessions and show differences"/utf8>>
                    ),
                    gleam@io:println(
                        <<"in answers, gaps, conflicts, and stage."/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(<<"Example:"/utf8>>),
                    gleam@io:println(
                        <<"  intent diff interview-abc123 interview-def456"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    glint:description(
        _pipe,
        <<"Compare two interview sessions and show differences"/utf8>>
    ).

-spec kirk_quality_command() -> glint:command(nil).
kirk_quality_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@kirk@quality_analyzer:analyze_quality(
                                Spec
                            ),
                            case Is_json of
                                true ->
                                    Json_obj = gleam@json:object(
                                        [{<<"completeness"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(2, Report)
                                                )},
                                            {<<"consistency"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(3, Report)
                                                )},
                                            {<<"testability"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(4, Report)
                                                )},
                                            {<<"clarity"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(5, Report)
                                                )},
                                            {<<"security"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(6, Report)
                                                )},
                                            {<<"overall"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(7, Report)
                                                )},
                                            {<<"issues"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(8, Report),
                                                    fun(I) ->
                                                        gleam@json:object(
                                                            [{<<"field"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            2,
                                                                            I
                                                                        )
                                                                    )},
                                                                {<<"issue"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            3,
                                                                            I
                                                                        )
                                                                    )},
                                                                {<<"severity"/utf8>>,
                                                                    gleam@json:string(
                                                                        intent@kirk@quality_analyzer:severity_to_string(
                                                                            erlang:element(
                                                                                4,
                                                                                I
                                                                            )
                                                                        )
                                                                    )}]
                                                        )
                                                    end
                                                )}]
                                    ),
                                    gleam@io:println(
                                        gleam@json:to_string(Json_obj)
                                    );

                                false ->
                                    gleam@io:println(
                                        intent@kirk@quality_analyzer:format_report(
                                            Report
                                        )
                                    )
                            end,
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent quality <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(
        _pipe@1,
        <<"KIRK: Analyze spec quality across multiple dimensions"/utf8>>
    ),
    glint:flag(
        _pipe@2,
        <<"json"/utf8>>,
        begin
            _pipe@3 = glint@flag:bool(),
            _pipe@4 = glint@flag:default(_pipe@3, false),
            glint@flag:description(_pipe@4, <<"Output as JSON"/utf8>>)
        end
    ).

-spec kirk_invert_command() -> glint:command(nil).
kirk_invert_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@kirk@inversion_checker:analyze_inversions(
                                Spec
                            ),
                            case Is_json of
                                true ->
                                    Json_obj = gleam@json:object(
                                        [{<<"score"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(6, Report)
                                                )},
                                            {<<"security_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(2, Report),
                                                    fun gap_to_json/1
                                                )},
                                            {<<"usability_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(3, Report),
                                                    fun gap_to_json/1
                                                )},
                                            {<<"integration_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(4, Report),
                                                    fun gap_to_json/1
                                                )},
                                            {<<"suggested_behaviors"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(5, Report),
                                                    fun(S) ->
                                                        gleam@json:object(
                                                            [{<<"name"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            2,
                                                                            S
                                                                        )
                                                                    )},
                                                                {<<"intent"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            3,
                                                                            S
                                                                        )
                                                                    )},
                                                                {<<"expected_status"/utf8>>,
                                                                    gleam@json:int(
                                                                        erlang:element(
                                                                            6,
                                                                            S
                                                                        )
                                                                    )},
                                                                {<<"category"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            7,
                                                                            S
                                                                        )
                                                                    )}]
                                                        )
                                                    end
                                                )}]
                                    ),
                                    gleam@io:println(
                                        gleam@json:to_string(Json_obj)
                                    );

                                false ->
                                    gleam@io:println(
                                        intent@kirk@inversion_checker:format_report(
                                            Report
                                        )
                                    )
                            end,
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent invert <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(
        _pipe@1,
        <<"KIRK: Inversion analysis - what failure cases are missing?"/utf8>>
    ),
    glint:flag(
        _pipe@2,
        <<"json"/utf8>>,
        begin
            _pipe@3 = glint@flag:bool(),
            _pipe@4 = glint@flag:default(_pipe@3, false),
            glint@flag:description(_pipe@4, <<"Output as JSON"/utf8>>)
        end
    ).

-spec kirk_coverage_command() -> glint:command(nil).
kirk_coverage_command() ->
    _pipe@5 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@kirk@coverage_analyzer:analyze_coverage(
                                Spec
                            ),
                            case Is_json of
                                true ->
                                    Json_obj = gleam@json:object(
                                        [{<<"overall_score"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(7, Report)
                                                )},
                                            {<<"methods"/utf8>>,
                                                gleam@json:object(
                                                    begin
                                                        _pipe@1 = erlang:element(
                                                            2,
                                                            Report
                                                        ),
                                                        _pipe@2 = maps:to_list(
                                                            _pipe@1
                                                        ),
                                                        gleam@list:map(
                                                            _pipe@2,
                                                            fun(Pair) ->
                                                                {erlang:element(
                                                                        1,
                                                                        Pair
                                                                    ),
                                                                    gleam@json:int(
                                                                        erlang:element(
                                                                            2,
                                                                            Pair
                                                                        )
                                                                    )}
                                                            end
                                                        )
                                                    end
                                                )},
                                            {<<"status_codes"/utf8>>,
                                                gleam@json:object(
                                                    begin
                                                        _pipe@3 = erlang:element(
                                                            3,
                                                            Report
                                                        ),
                                                        _pipe@4 = maps:to_list(
                                                            _pipe@3
                                                        ),
                                                        gleam@list:map(
                                                            _pipe@4,
                                                            fun(Pair@1) ->
                                                                {erlang:element(
                                                                        1,
                                                                        Pair@1
                                                                    ),
                                                                    gleam@json:int(
                                                                        erlang:element(
                                                                            2,
                                                                            Pair@1
                                                                        )
                                                                    )}
                                                            end
                                                        )
                                                    end
                                                )},
                                            {<<"owasp_score"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(
                                                        3,
                                                        erlang:element(
                                                            6,
                                                            Report
                                                        )
                                                    )
                                                )},
                                            {<<"owasp_missing"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(
                                                        4,
                                                        erlang:element(
                                                            6,
                                                            Report
                                                        )
                                                    ),
                                                    fun gleam@json:string/1
                                                )}]
                                    ),
                                    gleam@io:println(
                                        gleam@json:to_string(Json_obj)
                                    );

                                false ->
                                    gleam@io:println(
                                        intent@kirk@coverage_analyzer:format_report(
                                            Report
                                        )
                                    )
                            end,
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent coverage <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@6 = glint:description(
        _pipe@5,
        <<"KIRK: Coverage analysis including OWASP Top 10"/utf8>>
    ),
    glint:flag(
        _pipe@6,
        <<"json"/utf8>>,
        begin
            _pipe@7 = glint@flag:bool(),
            _pipe@8 = glint@flag:default(_pipe@7, false),
            glint@flag:description(_pipe@8, <<"Output as JSON"/utf8>>)
        end
    ).

-spec kirk_gaps_command() -> glint:command(nil).
kirk_gaps_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@kirk@gap_detector:detect_gaps(Spec),
                            case Is_json of
                                true ->
                                    Json_obj = gleam@json:object(
                                        [{<<"total_gaps"/utf8>>,
                                                gleam@json:int(
                                                    erlang:element(7, Report)
                                                )},
                                            {<<"severity_breakdown"/utf8>>,
                                                gleam@json:object(
                                                    [{<<"critical"/utf8>>,
                                                            gleam@json:int(
                                                                erlang:element(
                                                                    2,
                                                                    erlang:element(
                                                                        8,
                                                                        Report
                                                                    )
                                                                )
                                                            )},
                                                        {<<"high"/utf8>>,
                                                            gleam@json:int(
                                                                erlang:element(
                                                                    3,
                                                                    erlang:element(
                                                                        8,
                                                                        Report
                                                                    )
                                                                )
                                                            )},
                                                        {<<"medium"/utf8>>,
                                                            gleam@json:int(
                                                                erlang:element(
                                                                    4,
                                                                    erlang:element(
                                                                        8,
                                                                        Report
                                                                    )
                                                                )
                                                            )},
                                                        {<<"low"/utf8>>,
                                                            gleam@json:int(
                                                                erlang:element(
                                                                    5,
                                                                    erlang:element(
                                                                        8,
                                                                        Report
                                                                    )
                                                                )
                                                            )}]
                                                )},
                                            {<<"inversion_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(2, Report),
                                                    fun detected_gap_to_json/1
                                                )},
                                            {<<"second_order_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(3, Report),
                                                    fun detected_gap_to_json/1
                                                )},
                                            {<<"checklist_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(4, Report),
                                                    fun detected_gap_to_json/1
                                                )},
                                            {<<"coverage_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(5, Report),
                                                    fun detected_gap_to_json/1
                                                )},
                                            {<<"security_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(6, Report),
                                                    fun detected_gap_to_json/1
                                                )}]
                                    ),
                                    gleam@io:println(
                                        gleam@json:to_string(Json_obj)
                                    );

                                false ->
                                    gleam@io:println(
                                        intent@kirk@gap_detector:format_report(
                                            Report
                                        )
                                    )
                            end,
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent gaps <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(
        _pipe@1,
        <<"KIRK: Detect gaps using mental models"/utf8>>
    ),
    glint:flag(
        _pipe@2,
        <<"json"/utf8>>,
        begin
            _pipe@3 = glint@flag:bool(),
            _pipe@4 = glint@flag:default(_pipe@3, false),
            glint@flag:description(_pipe@4, <<"Output as JSON"/utf8>>)
        end
    ).

-spec kirk_effects_command() -> glint:command(nil).
kirk_effects_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@kirk@effects_analyzer:analyze_effects(
                                Spec
                            ),
                            gleam@io:println(
                                intent@kirk@effects_analyzer:format_report(
                                    Report
                                )
                            ),
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent effects <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"KIRK: Analyze second-order effects (consequence tracing)"/utf8>>
    ).

-spec kirk_ears_command() -> glint:command(nil).
kirk_ears_command() ->
    _pipe@2 = glint:command(
        fun(Input) ->
            Output_format = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"output"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<"text"/utf8>>)
            end,
            Output_file = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"out"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            case erlang:element(2, Input) of
                [Requirements_path | _] ->
                    case simplifile:read(Requirements_path) of
                        {ok, Content} ->
                            Result = intent@kirk@ears_parser:parse(Content),
                            Output = case Output_format of
                                <<"cue"/utf8>> ->
                                    Spec_name = case glint@flag:get_string(
                                        erlang:element(3, Input),
                                        <<"name"/utf8>>
                                    ) of
                                        {ok, N} ->
                                            N;

                                        {error, _} ->
                                            <<"GeneratedSpec"/utf8>>
                                    end,
                                    intent@kirk@ears_parser:to_cue(
                                        Result,
                                        Spec_name
                                    );

                                <<"json"/utf8>> ->
                                    Behaviors = intent@kirk@ears_parser:to_behaviors(
                                        Result
                                    ),
                                    Json_obj = gleam@json:object(
                                        [{<<"requirements"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(2, Result),
                                                    fun(R) ->
                                                        gleam@json:object(
                                                            [{<<"id"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            2,
                                                                            R
                                                                        )
                                                                    )},
                                                                {<<"pattern"/utf8>>,
                                                                    gleam@json:string(
                                                                        intent@kirk@ears_parser:pattern_to_string(
                                                                            erlang:element(
                                                                                3,
                                                                                R
                                                                            )
                                                                        )
                                                                    )},
                                                                {<<"system_shall"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            7,
                                                                            R
                                                                        )
                                                                    )},
                                                                {<<"raw_text"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            9,
                                                                            R
                                                                        )
                                                                    )}]
                                                        )
                                                    end
                                                )},
                                            {<<"behaviors"/utf8>>,
                                                gleam@json:array(
                                                    Behaviors,
                                                    fun(B) ->
                                                        gleam@json:object(
                                                            [{<<"name"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            2,
                                                                            B
                                                                        )
                                                                    )},
                                                                {<<"intent"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            3,
                                                                            B
                                                                        )
                                                                    )},
                                                                {<<"method"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            4,
                                                                            B
                                                                        )
                                                                    )},
                                                                {<<"path"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            5,
                                                                            B
                                                                        )
                                                                    )},
                                                                {<<"status"/utf8>>,
                                                                    gleam@json:int(
                                                                        erlang:element(
                                                                            6,
                                                                            B
                                                                        )
                                                                    )}]
                                                        )
                                                    end
                                                )},
                                            {<<"errors"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(3, Result),
                                                    fun(E) ->
                                                        gleam@json:object(
                                                            [{<<"line"/utf8>>,
                                                                    gleam@json:int(
                                                                        erlang:element(
                                                                            2,
                                                                            E
                                                                        )
                                                                    )},
                                                                {<<"message"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            3,
                                                                            E
                                                                        )
                                                                    )},
                                                                {<<"suggestion"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            4,
                                                                            E
                                                                        )
                                                                    )}]
                                                        )
                                                    end
                                                )},
                                            {<<"warnings"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(4, Result),
                                                    fun gleam@json:string/1
                                                )}]
                                    ),
                                    gleam@json:to_string(Json_obj);

                                _ ->
                                    intent@kirk@ears_parser:format_result(
                                        Result
                                    )
                            end,
                            case Output_file of
                                <<""/utf8>> ->
                                    gleam@io:println(Output);

                                Path ->
                                    case simplifile:write(Path, Output) of
                                        {ok, _} ->
                                            gleam@io:println(
                                                <<"Written to: "/utf8,
                                                    Path/binary>>
                                            );

                                        {error, _} ->
                                            intent@cli_ui:print_error(
                                                <<"Failed to write to: "/utf8,
                                                    Path/binary>>
                                            )
                                    end
                            end,
                            intent_ffi:halt(0);

                        {error, _} ->
                            intent@cli_ui:print_error(
                                <<"Failed to read: "/utf8,
                                    Requirements_path/binary>>
                            ),
                            intent_ffi:halt(4)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"requirements file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent ears <requirements.md> [--output text|cue|json] [--out <file>]"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(<<"EARS Patterns:"/utf8>>),
                    gleam@io:println(
                        <<"  THE SYSTEM SHALL [behavior]                    - Ubiquitous"/utf8>>
                    ),
                    gleam@io:println(
                        <<"  WHEN [trigger] THE SYSTEM SHALL [behavior]     - Event-Driven"/utf8>>
                    ),
                    gleam@io:println(
                        <<"  WHILE [state] THE SYSTEM SHALL [behavior]      - State-Driven"/utf8>>
                    ),
                    gleam@io:println(
                        <<"  WHERE [condition] THE SYSTEM SHALL [behavior]  - Optional"/utf8>>
                    ),
                    gleam@io:println(
                        <<"  IF [condition] THEN THE SYSTEM SHALL NOT       - Unwanted"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@3 = glint:description(
        _pipe@2,
        <<"KIRK: Parse EARS requirements to Intent behaviors"/utf8>>
    ),
    _pipe@6 = glint:flag(
        _pipe@3,
        <<"output"/utf8>>,
        begin
            _pipe@4 = glint@flag:string(),
            _pipe@5 = glint@flag:default(_pipe@4, <<"text"/utf8>>),
            glint@flag:description(
                _pipe@5,
                <<"Output format: text, cue, json"/utf8>>
            )
        end
    ),
    _pipe@9 = glint:flag(
        _pipe@6,
        <<"out"/utf8>>,
        begin
            _pipe@7 = glint@flag:string(),
            _pipe@8 = glint@flag:default(_pipe@7, <<""/utf8>>),
            glint@flag:description(_pipe@8, <<"Output file path"/utf8>>)
        end
    ),
    glint:flag(
        _pipe@9,
        <<"name"/utf8>>,
        begin
            _pipe@10 = glint@flag:string(),
            _pipe@11 = glint@flag:default(_pipe@10, <<"GeneratedSpec"/utf8>>),
            glint@flag:description(
                _pipe@11,
                <<"Spec name for CUE output"/utf8>>
            )
        end
    ).

-spec main() -> nil.
main() ->
    _pipe = glint:new(),
    _pipe@1 = glint:with_name(_pipe, <<"intent"/utf8>>),
    _pipe@2 = glint:with_pretty_help(_pipe@1, glint:default_pretty_help()),
    _pipe@3 = glint:add(_pipe@2, [<<"check"/utf8>>], check_command()),
    _pipe@4 = glint:add(_pipe@3, [<<"validate"/utf8>>], validate_command()),
    _pipe@5 = glint:add(_pipe@4, [<<"show"/utf8>>], show_command()),
    _pipe@6 = glint:add(_pipe@5, [<<"export"/utf8>>], export_command()),
    _pipe@7 = glint:add(_pipe@6, [<<"lint"/utf8>>], lint_command()),
    _pipe@8 = glint:add(_pipe@7, [<<"analyze"/utf8>>], analyze_command()),
    _pipe@9 = glint:add(_pipe@8, [<<"improve"/utf8>>], improve_command()),
    _pipe@10 = glint:add(_pipe@9, [<<"interview"/utf8>>], interview_command()),
    _pipe@11 = glint:add(_pipe@10, [<<"beads"/utf8>>], beads_command()),
    _pipe@12 = glint:add(
        _pipe@11,
        [<<"bead-status"/utf8>>],
        bead_status_command()
    ),
    _pipe@13 = glint:add(_pipe@12, [<<"history"/utf8>>], history_command()),
    _pipe@14 = glint:add(_pipe@13, [<<"diff"/utf8>>], diff_command()),
    _pipe@15 = glint:add(_pipe@14, [<<"sessions"/utf8>>], sessions_command()),
    _pipe@16 = glint:add(_pipe@15, [<<"quality"/utf8>>], kirk_quality_command()),
    _pipe@17 = glint:add(_pipe@16, [<<"invert"/utf8>>], kirk_invert_command()),
    _pipe@18 = glint:add(
        _pipe@17,
        [<<"coverage"/utf8>>],
        kirk_coverage_command()
    ),
    _pipe@19 = glint:add(_pipe@18, [<<"gaps"/utf8>>], kirk_gaps_command()),
    _pipe@20 = glint:add(_pipe@19, [<<"ears"/utf8>>], kirk_ears_command()),
    _pipe@21 = glint:add(_pipe@20, [<<"effects"/utf8>>], kirk_effects_command()),
    _pipe@22 = glint:add(_pipe@21, [<<"plan"/utf8>>], plan_command()),
    _pipe@23 = glint:add(
        _pipe@22,
        [<<"plan-approve"/utf8>>],
        plan_approve_command()
    ),
    _pipe@24 = glint:add(
        _pipe@23,
        [<<"beads-regenerate"/utf8>>],
        beads_regenerate_command()
    ),
    glint:run(_pipe@24, erlang:element(4, argv:load())).
