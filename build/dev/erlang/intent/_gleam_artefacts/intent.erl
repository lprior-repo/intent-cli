-module(intent).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent.gleam").
-export([main/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/intent.gleam", 243).
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

-file("src/intent.gleam", 786).
?DOC(" Helper: convert Profile to string for questions module\n").
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

-file("src/intent.gleam", 797).
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

        u_i ->
            <<"User Interface"/utf8>>
    end.

-file("src/intent.gleam", 717).
?DOC(" Ask a single question and collect answer\n").
-spec ask_single_question(
    intent@interview:interview_session(),
    intent@interview_questions:question(),
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
    Updated_session = intent@interview_session:add_answer(Session, Answer),
    {Sess_with_gaps, _} = intent@interview_session:check_for_gaps(
        Updated_session,
        Question,
        Answer
    ),
    {Sess_final, _} = intent@interview_session:check_for_conflicts(
        Sess_with_gaps,
        Answer
    ),
    Sess_final.

-file("src/intent.gleam", 694).
?DOC(" Ask all unanswered questions in a round\n").
-spec ask_questions_in_round(
    intent@interview:interview_session(),
    integer(),
    intent@interview_questions:question()
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

-file("src/intent.gleam", 652).
?DOC(" Main interview loop - asks questions round by round\n").
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
            case intent@interview_session:get_first_question_for_round(
                Session,
                Round
            ) of
                {error, _} ->
                    gleam@io:println(<<"(No questions for this round)"/utf8>>),
                    interview_loop(Session, Round + 1);

                {ok, First_question} ->
                    Updated_session = ask_questions_in_round(
                        Session,
                        Round,
                        First_question
                    ),
                    Blocking_gaps = intent@interview_session:get_blocking_gaps(
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

-file("src/intent.gleam", 501).
-spec run_interview(intent@interview:profile(), binary(), binary()) -> nil.
run_interview(Profile, _, Export_to) ->
    Session_id = <<"interview-"/utf8, (intent_ffi:generate_uuid())/binary>>,
    Timestamp = intent_ffi:current_timestamp(),
    Session = intent@interview_session:start_interview(
        Profile,
        Session_id,
        Timestamp
    ),
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

-file("src/intent.gleam", 106).
-spec run_check(binary(), binary(), boolean(), binary(), binary(), boolean()) -> nil.
run_check(Spec_path, Target_url, Is_json, Feature_filter, Only_filter, Verbose) ->
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
                end, Verbose},
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

-file("src/intent.gleam", 58).
?DOC(" The `check` command - run spec against a target\n").
-spec check_command() -> glint:command(nil).
check_command() ->
    _pipe@5 = glint:command(
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
            Is_verbose = begin
                _pipe@4 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"verbose"/utf8>>
                ),
                gleam@result:unwrap(_pipe@4, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    run_check(
                        Spec_path,
                        Target_url,
                        Is_json,
                        Feature_filter,
                        Only_filter,
                        Is_verbose
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
    _pipe@6 = glint:description(
        _pipe@5,
        <<"Run spec against a target URL and verify behaviors"/utf8>>
    ),
    _pipe@9 = glint:flag(
        _pipe@6,
        <<"target"/utf8>>,
        begin
            _pipe@7 = glint@flag:string(),
            _pipe@8 = glint@flag:default(_pipe@7, <<""/utf8>>),
            glint@flag:description(
                _pipe@8,
                <<"Target base URL to test against"/utf8>>
            )
        end
    ),
    _pipe@12 = glint:flag(
        _pipe@9,
        <<"json"/utf8>>,
        begin
            _pipe@10 = glint@flag:bool(),
            _pipe@11 = glint@flag:default(_pipe@10, false),
            glint@flag:description(_pipe@11, <<"Output results as JSON"/utf8>>)
        end
    ),
    _pipe@15 = glint:flag(
        _pipe@12,
        <<"feature"/utf8>>,
        begin
            _pipe@13 = glint@flag:string(),
            _pipe@14 = glint@flag:default(_pipe@13, <<""/utf8>>),
            glint@flag:description(
                _pipe@14,
                <<"Filter to a specific feature"/utf8>>
            )
        end
    ),
    _pipe@18 = glint:flag(
        _pipe@15,
        <<"only"/utf8>>,
        begin
            _pipe@16 = glint@flag:string(),
            _pipe@17 = glint@flag:default(_pipe@16, <<""/utf8>>),
            glint@flag:description(
                _pipe@17,
                <<"Run only a specific behavior"/utf8>>
            )
        end
    ),
    glint:flag(
        _pipe@18,
        <<"verbose"/utf8>>,
        begin
            _pipe@19 = glint@flag:bool(),
            _pipe@20 = glint@flag:default(_pipe@19, false),
            glint@flag:description(_pipe@20, <<"Verbose output"/utf8>>)
        end
    ).

-file("src/intent.gleam", 172).
?DOC(" The `validate` command - validate CUE spec without running\n").
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

-file("src/intent.gleam", 198).
?DOC(" The `show` command - pretty print a parsed spec\n").
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

-file("src/intent.gleam", 306).
?DOC(" The `export` command - export spec to JSON\n").
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

-file("src/intent.gleam", 332).
?DOC(" The `lint` command - check for specification anti-patterns\n").
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

-file("src/intent.gleam", 367).
?DOC(" The `analyze` command - analyze spec quality\n").
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

-file("src/intent.gleam", 394).
?DOC(" The `improve` command - suggest improvements\n").
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

-file("src/intent.gleam", 576).
?DOC(" Resume an existing interview session\n").
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

-file("src/intent.gleam", 429).
?DOC(" The `interview` command - guided specification discovery\n").
-spec interview_command() -> glint:command(nil).
interview_command() ->
    _pipe@4 = glint:command(
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
            Json_input = begin
                _pipe@3 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"answers"/utf8>>
                ),
                gleam@result:unwrap(_pipe@3, <<""/utf8>>)
            end,
            case Resume_id of
                <<""/utf8>> ->
                    case gleam@string:lowercase(Profile_str) of
                        <<"api"/utf8>> ->
                            run_interview(api, Json_input, Export_to);

                        <<"cli"/utf8>> ->
                            run_interview(cli, Json_input, Export_to);

                        <<"event"/utf8>> ->
                            run_interview(event, Json_input, Export_to);

                        <<"data"/utf8>> ->
                            run_interview(data, Json_input, Export_to);

                        <<"workflow"/utf8>> ->
                            run_interview(workflow, Json_input, Export_to);

                        <<"ui"/utf8>> ->
                            run_interview(u_i, Json_input, Export_to);

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
    _pipe@5 = glint:description(
        _pipe@4,
        <<"Guided specification discovery through structured interview"/utf8>>
    ),
    _pipe@8 = glint:flag(
        _pipe@5,
        <<"profile"/utf8>>,
        begin
            _pipe@6 = glint@flag:string(),
            _pipe@7 = glint@flag:default(_pipe@6, <<"api"/utf8>>),
            glint@flag:description(
                _pipe@7,
                <<"System profile: api, cli, event, data, workflow, or ui"/utf8>>
            )
        end
    ),
    _pipe@11 = glint:flag(
        _pipe@8,
        <<"resume"/utf8>>,
        begin
            _pipe@9 = glint@flag:string(),
            _pipe@10 = glint@flag:default(_pipe@9, <<""/utf8>>),
            glint@flag:description(
                _pipe@10,
                <<"Resume existing interview session by ID"/utf8>>
            )
        end
    ),
    _pipe@14 = glint:flag(
        _pipe@11,
        <<"answers"/utf8>>,
        begin
            _pipe@12 = glint@flag:string(),
            _pipe@13 = glint@flag:default(_pipe@12, <<""/utf8>>),
            glint@flag:description(
                _pipe@13,
                <<"Path to YAML/JSON file with pre-filled answers"/utf8>>
            )
        end
    ),
    glint:flag(
        _pipe@14,
        <<"export"/utf8>>,
        begin
            _pipe@15 = glint@flag:string(),
            _pipe@16 = glint@flag:default(_pipe@15, <<""/utf8>>),
            glint@flag:description(
                _pipe@16,
                <<"Export completed interview to spec file"/utf8>>
            )
        end
    ).

-file("src/intent.gleam", 809).
?DOC(" The `beads` command - generate work items from interview session\n").
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

-file("src/intent.gleam", 41).
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
    glint:run(_pipe@11, erlang:element(4, argv:load())).
