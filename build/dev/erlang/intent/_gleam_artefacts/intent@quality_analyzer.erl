-module(intent@quality_analyzer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/quality_analyzer.gleam").
-export([analyze_spec/1, format_report/1]).
-export_type([quality_report/0, quality_issue/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type quality_report() :: {quality_report,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        list(quality_issue()),
        list(binary())}.

-type quality_issue() :: missing_error_tests |
    missing_authentication_test |
    missing_edge_cases |
    vague_rules |
    no_examples |
    missing_explanations |
    untested_rules |
    missing_a_i_hints.

-file("src/intent/quality_analyzer.gleam", 68).
?DOC(
    " Calculate coverage score (0-100)\n"
    " Measures how many error cases and edge cases are tested\n"
).
-spec calculate_coverage_score(
    list(intent@types:behavior()),
    list(intent@types:rule())
) -> integer().
calculate_coverage_score(Behaviors, Rules) ->
    Base = 50,
    Error_statuses = begin
        _pipe = Behaviors,
        _pipe@1 = gleam@list:filter(
            _pipe,
            fun(B) ->
                Status = erlang:element(2, erlang:element(8, B)),
                (Status >= 400) andalso (Status < 600)
            end
        ),
        erlang:length(_pipe@1)
    end,
    Error_bonus = gleam@int:min(50, Error_statuses * 10),
    Has_auth_test = begin
        _pipe@2 = Behaviors,
        gleam@list:any(
            _pipe@2,
            fun(B@1) ->
                gleam_stdlib:contains_string(
                    gleam@string:lowercase(erlang:element(2, B@1)),
                    <<"auth"/utf8>>
                )
                orelse gleam_stdlib:contains_string(
                    gleam@string:lowercase(erlang:element(3, B@1)),
                    <<"auth"/utf8>>
                )
            end
        )
    end,
    Auth_bonus = case Has_auth_test of
        true ->
            10;

        false ->
            0
    end,
    Has_edge_cases = begin
        _pipe@3 = Behaviors,
        gleam@list:any(
            _pipe@3,
            fun(B@2) ->
                ((gleam_stdlib:contains_string(
                    gleam@string:lowercase(erlang:element(2, B@2)),
                    <<"empty"/utf8>>
                )
                orelse gleam_stdlib:contains_string(
                    gleam@string:lowercase(erlang:element(2, B@2)),
                    <<"invalid"/utf8>>
                ))
                orelse gleam_stdlib:contains_string(
                    gleam@string:lowercase(erlang:element(2, B@2)),
                    <<"max"/utf8>>
                ))
                orelse gleam_stdlib:contains_string(
                    gleam@string:lowercase(erlang:element(3, B@2)),
                    <<"edge"/utf8>>
                )
            end
        )
    end,
    Edge_bonus = case Has_edge_cases of
        true ->
            10;

        false ->
            0
    end,
    Antipattern_bonus = gleam@int:min(5, erlang:length(Rules) * 2),
    Coverage_total = (((Base + Error_bonus) + Auth_bonus) + Edge_bonus) + Antipattern_bonus,
    gleam@int:min(100, Coverage_total).

-file("src/intent/quality_analyzer.gleam", 123).
?DOC(
    " Calculate clarity score (0-100)\n"
    " Measures how well documented the spec is\n"
).
-spec calculate_clarity_score(list(intent@types:behavior())) -> integer().
calculate_clarity_score(Behaviors) ->
    Base = 60,
    With_intent = begin
        _pipe = Behaviors,
        _pipe@1 = gleam@list:filter(
            _pipe,
            fun(B) -> not gleam@string:is_empty(erlang:element(3, B)) end
        ),
        erlang:length(_pipe@1)
    end,
    Intent_ratio = case erlang:length(Behaviors) of
        0 ->
            0;

        N ->
            Ratio = With_intent * 100,
            case N of
                0 -> 0;
                Gleam@denominator -> Ratio div Gleam@denominator
            end
    end,
    Intent_bonus = gleam@int:min(10, Intent_ratio div 10),
    With_notes = begin
        _pipe@2 = Behaviors,
        _pipe@3 = gleam@list:filter(
            _pipe@2,
            fun(B@1) -> not gleam@string:is_empty(erlang:element(4, B@1)) end
        ),
        erlang:length(_pipe@3)
    end,
    Notes_bonus = case erlang:length(Behaviors) of
        0 ->
            0;

        N@1 ->
            Bonus_calc = With_notes * 10,
            case N@1 of
                0 -> 0;
                Gleam@denominator@1 -> Bonus_calc div Gleam@denominator@1
            end
    end,
    Has_vague_rules = begin
        _pipe@4 = Behaviors,
        gleam@list:any(
            _pipe@4,
            fun(B@2) -> _pipe@5 = erlang:element(4, erlang:element(8, B@2)),
                _pipe@6 = gleam@dict:values(_pipe@5),
                gleam@list:any(
                    _pipe@6,
                    fun(Check) ->
                        Rule_lower = gleam@string:lowercase(
                            erlang:element(2, Check)
                        ),
                        ((gleam_stdlib:contains_string(
                            Rule_lower,
                            <<"valid"/utf8>>
                        )
                        andalso not gleam_stdlib:contains_string(
                            Rule_lower,
                            <<"email"/utf8>>
                        ))
                        andalso not gleam_stdlib:contains_string(
                            Rule_lower,
                            <<"uuid"/utf8>>
                        ))
                        andalso not gleam_stdlib:contains_string(
                            Rule_lower,
                            <<"iso"/utf8>>
                        )
                    end
                ) end
        )
    end,
    Vague_penalty = case Has_vague_rules of
        true ->
            -10;

        false ->
            0
    end,
    Clarity_total = ((Base + Intent_bonus) + Notes_bonus) + Vague_penalty,
    gleam@int:max(0, gleam@int:min(100, Clarity_total)).

-file("src/intent/quality_analyzer.gleam", 182).
?DOC(
    " Calculate testability score (0-100)\n"
    " Measures how well structured for execution\n"
).
-spec calculate_testability_score(list(intent@types:behavior())) -> integer().
calculate_testability_score(Behaviors) ->
    Base = 70,
    With_captures = begin
        _pipe = Behaviors,
        _pipe@1 = gleam@list:filter(
            _pipe,
            fun(B) -> not gleam@dict:is_empty(erlang:element(9, B)) end
        ),
        erlang:length(_pipe@1)
    end,
    Capture_bonus = gleam@int:min(10, With_captures * 5),
    With_dependencies = begin
        _pipe@2 = Behaviors,
        _pipe@3 = gleam@list:filter(
            _pipe@2,
            fun(B@1) -> not gleam@list:is_empty(erlang:element(5, B@1)) end
        ),
        erlang:length(_pipe@3)
    end,
    Deps_bonus = gleam@int:min(10, With_dependencies * 5),
    With_examples = begin
        _pipe@4 = Behaviors,
        _pipe@5 = gleam@list:filter(
            _pipe@4,
            fun(B@2) ->
                erlang:element(3, erlang:element(8, B@2)) /= gleam@json:null()
            end
        ),
        erlang:length(_pipe@5)
    end,
    Example_bonus = gleam@int:min(
        5,
        case gleam@int:max(1, erlang:length(Behaviors) div 2) of
            0 -> 0;
            Gleam@denominator -> With_examples div Gleam@denominator
        end
    ),
    Testability_total = ((Base + Capture_bonus) + Deps_bonus) + Example_bonus,
    gleam@int:min(100, Testability_total).

-file("src/intent/quality_analyzer.gleam", 217).
?DOC(
    " Calculate AI readiness score (0-100)\n"
    " Measures how much guidance is available for AI\n"
).
-spec calculate_ai_readiness_score(
    intent@types:spec(),
    list(intent@types:behavior())
) -> integer().
calculate_ai_readiness_score(Spec, Behaviors) ->
    Base = 50,
    Has_ai_hints = not gleam@list:is_empty(
        erlang:element(2, erlang:element(2, erlang:element(11, Spec)))
    )
    orelse not gleam@list:is_empty(erlang:element(5, erlang:element(11, Spec))),
    Hints_bonus = case Has_ai_hints of
        true ->
            20;

        false ->
            -10
    end,
    With_why = begin
        _pipe = Behaviors,
        _pipe@1 = gleam@list:flat_map(
            _pipe,
            fun(B) ->
                gleam@dict:values(erlang:element(4, erlang:element(8, B)))
            end
        ),
        _pipe@2 = gleam@list:filter(
            _pipe@1,
            fun(C) -> not gleam@string:is_empty(erlang:element(3, C)) end
        ),
        erlang:length(_pipe@2)
    end,
    Total_checks = begin
        _pipe@3 = Behaviors,
        _pipe@4 = gleam@list:flat_map(
            _pipe@3,
            fun(B@1) ->
                gleam@dict:values(erlang:element(4, erlang:element(8, B@1)))
            end
        ),
        erlang:length(_pipe@4)
    end,
    Why_bonus = case Total_checks of
        0 ->
            0;

        _ ->
            Why_calc = With_why * 30,
            case gleam@int:max(1, Total_checks) of
                0 -> 0;
                Gleam@denominator -> Why_calc div Gleam@denominator
            end
    end,
    With_examples = begin
        _pipe@5 = Behaviors,
        _pipe@6 = gleam@list:filter(
            _pipe@5,
            fun(B@2) ->
                erlang:element(3, erlang:element(8, B@2)) /= gleam@json:null()
            end
        ),
        erlang:length(_pipe@6)
    end,
    Example_bonus = gleam@int:min(10, With_examples * 5),
    Ai_readiness_total = ((Base + Hints_bonus) + Why_bonus) + Example_bonus,
    gleam@int:max(0, gleam@int:min(100, Ai_readiness_total)).

-file("src/intent/quality_analyzer.gleam", 265).
?DOC(" Find quality issues in spec\n").
-spec find_quality_issues(
    list(intent@types:behavior()),
    list(intent@types:rule())
) -> list(quality_issue()).
find_quality_issues(Behaviors, Rules) ->
    Mut_issues = [],
    Has_error_tests = gleam@list:any(
        Behaviors,
        fun(B) -> erlang:element(2, erlang:element(8, B)) >= 400 end
    ),
    Mut_issues@1 = case Has_error_tests of
        true ->
            Mut_issues;

        false ->
            [missing_error_tests | Mut_issues]
    end,
    Has_auth_test = gleam@list:any(
        Behaviors,
        fun(B@1) ->
            gleam_stdlib:contains_string(
                gleam@string:lowercase(erlang:element(2, B@1)),
                <<"auth"/utf8>>
            )
        end
    ),
    Mut_issues@2 = case Has_auth_test of
        true ->
            Mut_issues@1;

        false ->
            [missing_authentication_test | Mut_issues@1]
    end,
    Has_edge_cases = gleam@list:any(
        Behaviors,
        fun(B@2) ->
            gleam_stdlib:contains_string(
                gleam@string:lowercase(erlang:element(2, B@2)),
                <<"empty"/utf8>>
            )
            orelse gleam_stdlib:contains_string(
                gleam@string:lowercase(erlang:element(2, B@2)),
                <<"invalid"/utf8>>
            )
        end
    ),
    Mut_issues@3 = case Has_edge_cases of
        true ->
            Mut_issues@2;

        false ->
            [missing_edge_cases | Mut_issues@2]
    end,
    Has_vague = gleam@list:any(
        Behaviors,
        fun(B@3) -> _pipe = erlang:element(4, erlang:element(8, B@3)),
            _pipe@1 = gleam@dict:values(_pipe),
            gleam@list:any(
                _pipe@1,
                fun(Check) ->
                    Rule_lower = gleam@string:lowercase(
                        erlang:element(2, Check)
                    ),
                    gleam_stdlib:contains_string(
                        Rule_lower,
                        <<"valid data"/utf8>>
                    )
                    orelse gleam_stdlib:contains_string(
                        Rule_lower,
                        <<"correct format"/utf8>>
                    )
                end
            ) end
    ),
    Mut_issues@4 = case Has_vague of
        false ->
            Mut_issues@3;

        true ->
            [vague_rules | Mut_issues@3]
    end,
    Has_examples = gleam@list:any(
        Behaviors,
        fun(B@4) ->
            erlang:element(3, erlang:element(8, B@4)) /= gleam@json:null()
        end
    ),
    Mut_issues@5 = case Has_examples of
        true ->
            Mut_issues@4;

        false ->
            [no_examples | Mut_issues@4]
    end,
    Has_explanations = gleam@list:any(
        Behaviors,
        fun(B@5) -> _pipe@2 = erlang:element(4, erlang:element(8, B@5)),
            _pipe@3 = gleam@dict:values(_pipe@2),
            gleam@list:any(
                _pipe@3,
                fun(C) -> not gleam@string:is_empty(erlang:element(3, C)) end
            ) end
    ),
    Mut_issues@6 = case Has_explanations of
        true ->
            Mut_issues@5;

        false ->
            [missing_explanations | Mut_issues@5]
    end,
    Has_untested_rules = not gleam@list:is_empty(Rules),
    Mut_issues@7 = case Has_untested_rules of
        false ->
            Mut_issues@6;

        true ->
            [untested_rules | Mut_issues@6]
    end,
    Mut_issues@7.

-file("src/intent/quality_analyzer.gleam", 386).
?DOC(" Helper to add suggestion conditionally\n").
-spec add_suggestion_if(list(binary()), boolean(), binary()) -> list(binary()).
add_suggestion_if(Suggestions, Condition, Suggestion) ->
    case Condition of
        true ->
            [Suggestion | Suggestions];

        false ->
            Suggestions
    end.

-file("src/intent/quality_analyzer.gleam", 353).
?DOC(" Generate suggestions for improvement\n").
-spec generate_suggestions(
    list(quality_issue()),
    list(intent@types:behavior()),
    list(intent@types:rule())
) -> list(binary()).
generate_suggestions(Issues, _, _) ->
    _pipe = [],
    _pipe@1 = add_suggestion_if(
        _pipe,
        gleam@list:contains(Issues, missing_error_tests),
        <<"Add test cases for error status codes (400, 401, 403, 404, 409, 500)"/utf8>>
    ),
    _pipe@2 = add_suggestion_if(
        _pipe@1,
        gleam@list:contains(Issues, missing_authentication_test),
        <<"Add test cases for authentication (missing auth, invalid token)"/utf8>>
    ),
    _pipe@3 = add_suggestion_if(
        _pipe@2,
        gleam@list:contains(Issues, missing_edge_cases),
        <<"Add edge case tests (empty values, max length, invalid input)"/utf8>>
    ),
    _pipe@4 = add_suggestion_if(
        _pipe@3,
        gleam@list:contains(Issues, vague_rules),
        <<"Replace vague rules like 'valid data' with specific validation rules"/utf8>>
    ),
    _pipe@5 = add_suggestion_if(
        _pipe@4,
        gleam@list:contains(Issues, no_examples),
        <<"Add response examples to each behavior for documentation"/utf8>>
    ),
    add_suggestion_if(
        _pipe@5,
        gleam@list:contains(Issues, missing_explanations),
        <<"Add 'why' explanations to validation rules to clarify intent"/utf8>>
    ).

-file("src/intent/quality_analyzer.gleam", 37).
?DOC(" Analyze spec quality\n").
-spec analyze_spec(intent@types:spec()) -> quality_report().
analyze_spec(Spec) ->
    Behaviors = begin
        _pipe = erlang:element(8, Spec),
        gleam@list:flat_map(_pipe, fun(F) -> erlang:element(4, F) end)
    end,
    Coverage_score = calculate_coverage_score(
        Behaviors,
        erlang:element(9, Spec)
    ),
    Clarity_score = calculate_clarity_score(Behaviors),
    Testability_score = calculate_testability_score(Behaviors),
    Ai_readiness_score = calculate_ai_readiness_score(Spec, Behaviors),
    Overall_score = begin
        Sum = ((Coverage_score + Clarity_score) + Testability_score) + Ai_readiness_score,
        Sum div 4
    end,
    Issues = find_quality_issues(Behaviors, erlang:element(9, Spec)),
    Suggestions = generate_suggestions(
        Issues,
        Behaviors,
        erlang:element(9, Spec)
    ),
    {quality_report,
        Coverage_score,
        Clarity_score,
        Testability_score,
        Ai_readiness_score,
        Overall_score,
        Issues,
        Suggestions}.

-file("src/intent/quality_analyzer.gleam", 433).
?DOC(" Format a quality issue\n").
-spec format_issue(quality_issue()) -> binary().
format_issue(Issue) ->
    case Issue of
        missing_error_tests ->
            <<"  • Missing error status code tests (4xx, 5xx)"/utf8>>;

        missing_authentication_test ->
            <<"  • Missing authentication tests"/utf8>>;

        missing_edge_cases ->
            <<"  • Missing edge case tests (empty, invalid, etc)"/utf8>>;

        vague_rules ->
            <<"  • Vague validation rules ('valid data', 'correct format')"/utf8>>;

        no_examples ->
            <<"  • No response examples provided"/utf8>>;

        missing_explanations ->
            <<"  • Missing 'why' explanations in checks"/utf8>>;

        untested_rules ->
            <<"  • Global rules not tested in behaviors"/utf8>>;

        missing_a_i_hints ->
            <<"  • No AI implementation hints provided"/utf8>>
    end.

-file("src/intent/quality_analyzer.gleam", 398).
?DOC(" Format quality report for display\n").
-spec format_report(quality_report()) -> binary().
format_report(Report) ->
    Score_section = <<<<<<<<<<<<<<<<<<<<<<<<<<<<"Quality Score: "/utf8,
                                                            (gleam@int:to_string(
                                                                erlang:element(
                                                                    6,
                                                                    Report
                                                                )
                                                            ))/binary>>/binary,
                                                        "/100\n"/utf8>>/binary,
                                                    "  Coverage: "/utf8>>/binary,
                                                (gleam@int:to_string(
                                                    erlang:element(2, Report)
                                                ))/binary>>/binary,
                                            "/100\n"/utf8>>/binary,
                                        "  Clarity: "/utf8>>/binary,
                                    (gleam@int:to_string(
                                        erlang:element(3, Report)
                                    ))/binary>>/binary,
                                "/100\n"/utf8>>/binary,
                            "  Testability: "/utf8>>/binary,
                        (gleam@int:to_string(erlang:element(4, Report)))/binary>>/binary,
                    "/100\n"/utf8>>/binary,
                "  AI Readiness: "/utf8>>/binary,
            (gleam@int:to_string(erlang:element(5, Report)))/binary>>/binary,
        "/100"/utf8>>,
    Issues_section = case gleam@list:is_empty(erlang:element(7, Report)) of
        true ->
            <<"No quality issues found!"/utf8>>;

        false ->
            <<"Quality Issues:\n"/utf8,
                (gleam@string:join(
                    begin
                        _pipe = erlang:element(7, Report),
                        gleam@list:map(_pipe, fun format_issue/1)
                    end,
                    <<"\n"/utf8>>
                ))/binary>>
    end,
    Suggestions_section = case gleam@list:is_empty(erlang:element(8, Report)) of
        true ->
            <<""/utf8>>;

        false ->
            <<"\n\nSuggestions for Improvement:\n"/utf8,
                (gleam@string:join(
                    begin
                        _pipe@1 = erlang:element(8, Report),
                        gleam@list:index_map(
                            _pipe@1,
                            fun(S, I) ->
                                <<<<(gleam@int:to_string(I + 1))/binary,
                                        ". "/utf8>>/binary,
                                    S/binary>>
                            end
                        )
                    end,
                    <<"\n"/utf8>>
                ))/binary>>
    end,
    <<<<<<Score_section/binary, "\n\n"/utf8>>/binary, Issues_section/binary>>/binary,
        Suggestions_section/binary>>.
