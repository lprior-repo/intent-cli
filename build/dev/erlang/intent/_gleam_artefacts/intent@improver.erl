-module(intent@improver).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/improver.gleam").
-export([suggest_improvements/1, apply_improvements/2, format_improvements/1]).
-export_type([improvement_suggestion/0, proposed_change/0, improvement_context/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type improvement_suggestion() :: {improvement_suggestion,
        binary(),
        binary(),
        binary(),
        integer(),
        proposed_change()}.

-type proposed_change() :: {add_missing_test, binary(), binary()} |
    {refine_vague_rule, binary(), binary(), binary()} |
    {add_response_example, binary()} |
    {rename_for_clarity, binary(), binary()} |
    {simplify_rule, binary(), binary(), binary()} |
    {add_explanation, binary(), binary(), binary()}.

-type improvement_context() :: {improvement_context,
        intent@quality_analyzer:quality_report(),
        intent@spec_linter:lint_result(),
        intent@types:spec()}.

-file("src/intent/improver.gleam", 82).
?DOC(" Add suggestions for improving coverage\n").
-spec append_coverage_suggestions(
    list(improvement_suggestion()),
    intent@quality_analyzer:quality_report(),
    list(intent@types:behavior())
) -> list(improvement_suggestion()).
append_coverage_suggestions(Suggestions, _, Behaviors) ->
    Has_error_tests = gleam@list:any(
        Behaviors,
        fun(B) -> erlang:element(2, erlang:element(8, B)) >= 400 end
    ),
    case Has_error_tests of
        true ->
            Suggestions;

        false ->
            lists:append(
                Suggestions,
                [{improvement_suggestion,
                        <<"Add error case tests"/utf8>>,
                        <<"No behaviors test error responses (4xx, 5xx status codes)"/utf8>>,
                        <<"Testing error cases ensures the API handles failures gracefully and helps AI understand error conditions"/utf8>>,
                        25,
                        {add_missing_test,
                            <<"test-error-not-found"/utf8>>,
                            <<"Test 404 Not Found response for missing resource"/utf8>>}}]
            )
    end.

-file("src/intent/improver.gleam", 113).
?DOC(" Add suggestions for improving clarity\n").
-spec append_clarity_suggestions(
    list(improvement_suggestion()),
    intent@quality_analyzer:quality_report(),
    list(intent@types:behavior())
) -> list(improvement_suggestion()).
append_clarity_suggestions(Suggestions, _, Behaviors) ->
    Missing_intent = begin
        _pipe = Behaviors,
        _pipe@1 = gleam@list:filter(
            _pipe,
            fun(B) -> gleam@string:is_empty(erlang:element(3, B)) end
        ),
        erlang:length(_pipe@1)
    end,
    case Missing_intent > 0 of
        false ->
            Suggestions;

        true ->
            lists:append(
                Suggestions,
                [{improvement_suggestion,
                        <<"Add intent descriptions"/utf8>>,
                        <<(gleam@int:to_string(Missing_intent))/binary,
                            " behavior(s) lack intent descriptions"/utf8>>,
                        <<"Intent descriptions explain WHY a test exists, helping both humans and AI understand the business logic"/utf8>>,
                        15,
                        {add_explanation,
                            <<"test-success"/utf8>>,
                            <<"intent"/utf8>>,
                            <<"Verify successful operation with valid input"/utf8>>}}]
            )
    end.

-file("src/intent/improver.gleam", 147).
?DOC(" Add suggestions for improving testability\n").
-spec append_testability_suggestions(
    list(improvement_suggestion()),
    intent@quality_analyzer:quality_report(),
    list(intent@types:behavior())
) -> list(improvement_suggestion()).
append_testability_suggestions(Suggestions, _, Behaviors) ->
    Missing_examples = begin
        _pipe = Behaviors,
        _pipe@1 = gleam@list:filter(
            _pipe,
            fun(B) ->
                erlang:element(3, erlang:element(8, B)) =:= gleam@json:null()
            end
        ),
        erlang:length(_pipe@1)
    end,
    case Missing_examples > 0 of
        false ->
            Suggestions;

        true ->
            lists:append(
                Suggestions,
                [{improvement_suggestion,
                        <<"Add response examples"/utf8>>,
                        <<(gleam@int:to_string(Missing_examples))/binary,
                            " behavior(s) lack response examples"/utf8>>,
                        <<"Examples make the spec executable and give AI concrete data structures to work with"/utf8>>,
                        20,
                        {add_response_example, <<"test-success"/utf8>>}}]
            )
    end.

-file("src/intent/improver.gleam", 179).
?DOC(" Add suggestions for improving AI readiness\n").
-spec append_ai_readiness_suggestions(
    list(improvement_suggestion()),
    intent@quality_analyzer:quality_report(),
    list(intent@types:behavior())
) -> list(improvement_suggestion()).
append_ai_readiness_suggestions(Suggestions, _, Behaviors) ->
    Missing_why = begin
        _pipe = Behaviors,
        _pipe@1 = gleam@list:flat_map(
            _pipe,
            fun(B) ->
                gleam@dict:values(erlang:element(4, erlang:element(8, B)))
            end
        ),
        _pipe@2 = gleam@list:filter(
            _pipe@1,
            fun(C) -> gleam@string:is_empty(erlang:element(3, C)) end
        ),
        erlang:length(_pipe@2)
    end,
    case Missing_why > 0 of
        false ->
            Suggestions;

        true ->
            lists:append(
                Suggestions,
                [{improvement_suggestion,
                        <<"Add validation explanations"/utf8>>,
                        <<(gleam@int:to_string(Missing_why))/binary,
                            " validation rule(s) lack 'why' explanations"/utf8>>,
                        <<"Explanations help AI understand the business logic behind each validation check"/utf8>>,
                        18,
                        {add_explanation,
                            <<"test-success"/utf8>>,
                            <<"why"/utf8>>,
                            <<"Ensures email field contains valid RFC 5322 compliant email address"/utf8>>}}]
            )
    end.

-file("src/intent/improver.gleam", 66).
?DOC(" Generate suggestions based on quality analysis\n").
-spec suggest_from_quality_issues(
    intent@quality_analyzer:quality_report(),
    intent@types:spec()
) -> list(improvement_suggestion()).
suggest_from_quality_issues(Report, Spec) ->
    Behaviors = begin
        _pipe = erlang:element(8, Spec),
        gleam@list:flat_map(_pipe, fun(F) -> erlang:element(4, F) end)
    end,
    _pipe@1 = [],
    _pipe@2 = append_coverage_suggestions(_pipe@1, Report, Behaviors),
    _pipe@3 = append_clarity_suggestions(_pipe@2, Report, Behaviors),
    _pipe@4 = append_testability_suggestions(_pipe@3, Report, Behaviors),
    append_ai_readiness_suggestions(_pipe@4, Report, Behaviors).

-file("src/intent/improver.gleam", 214).
?DOC(" Generate suggestions from lint warnings\n").
-spec suggest_from_lint_warnings(intent@spec_linter:lint_result()) -> list(improvement_suggestion()).
suggest_from_lint_warnings(Lint_result) ->
    case Lint_result of
        lint_valid ->
            [];

        {lint_warnings, Warnings} ->
            _pipe = Warnings,
            gleam@list:filter_map(_pipe, fun(Warning) -> case Warning of
                        {vague_rule, Behavior, Field, _} ->
                            {ok,
                                {improvement_suggestion,
                                    <<"Clarify validation rule"/utf8>>,
                                    <<<<<<<<"Behavior '"/utf8, Behavior/binary>>/binary,
                                                "', field '"/utf8>>/binary,
                                            Field/binary>>/binary,
                                        "' uses vague language"/utf8>>,
                                    <<"Specific validation rules are more testable and easier for AI to implement"/utf8>>,
                                    22,
                                    {refine_vague_rule,
                                        Behavior,
                                        Field,
                                        <<"email | format:email or similar concrete format"/utf8>>}}};

                        {missing_example, Behavior@1} ->
                            {ok,
                                {improvement_suggestion,
                                    <<"Add response example"/utf8>>,
                                    <<<<"Behavior '"/utf8, Behavior@1/binary>>/binary,
                                        "' has no response example"/utf8>>,
                                    <<"Examples make specifications executable and concrete"/utf8>>,
                                    20,
                                    {add_response_example, Behavior@1}}};

                        {naming_convention, Behavior@2, Suggestion} ->
                            {ok,
                                {improvement_suggestion,
                                    <<"Improve naming"/utf8>>,
                                    Suggestion,
                                    <<"Consistent naming conventions make specs easier to read and navigate"/utf8>>,
                                    10,
                                    {rename_for_clarity,
                                        Behavior@2,
                                        gleam@string:replace(
                                            Behavior@2,
                                            <<"_"/utf8>>,
                                            <<"-"/utf8>>
                                        )}}};

                        {unused_anti_pattern, Pattern} ->
                            {ok,
                                {improvement_suggestion,
                                    <<"Remove unused anti-pattern"/utf8>>,
                                    <<<<"Anti-pattern '"/utf8, Pattern/binary>>/binary,
                                        "' is not tested by any behavior"/utf8>>,
                                    <<"Unused anti-patterns add clutter without providing test coverage"/utf8>>,
                                    5,
                                    {refine_vague_rule,
                                        Pattern,
                                        <<"pattern"/utf8>>,
                                        <<"remove from spec"/utf8>>}}};

                        {anti_pattern_detected, Behavior@3, Pattern@1, _} ->
                            {ok,
                                {improvement_suggestion,
                                    <<"Fix anti-pattern in response"/utf8>>,
                                    <<<<<<<<"Behavior '"/utf8,
                                                    Behavior@3/binary>>/binary,
                                                "' contains anti-pattern '"/utf8>>/binary,
                                            Pattern@1/binary>>/binary,
                                        "'"/utf8>>,
                                    <<"Anti-patterns represent bad responses that should not be in examples"/utf8>>,
                                    30,
                                    {refine_vague_rule,
                                        Behavior@3,
                                        <<"response.example"/utf8>>,
                                        <<"Remove keys matching anti-pattern bad_example"/utf8>>}}}
                    end end)
    end.

-file("src/intent/improver.gleam", 44).
?DOC(" Generate improvement suggestions from analysis results\n").
-spec suggest_improvements(improvement_context()) -> list(improvement_suggestion()).
suggest_improvements(Context) ->
    Mut_suggestions = [],
    Quality_suggestions = suggest_from_quality_issues(
        erlang:element(2, Context),
        erlang:element(4, Context)
    ),
    Mut_suggestions@1 = lists:append(Mut_suggestions, Quality_suggestions),
    Lint_suggestions = suggest_from_lint_warnings(erlang:element(3, Context)),
    Mut_suggestions@2 = lists:append(Mut_suggestions@1, Lint_suggestions),
    _pipe = Mut_suggestions@2,
    gleam@list:sort(
        _pipe,
        fun(A, B) ->
            gleam@int:compare(erlang:element(5, B), erlang:element(5, A))
        end
    ).

-file("src/intent/improver.gleam", 295).
?DOC(" Generate a refined spec based on accepted suggestions\n").
-spec apply_improvements(intent@types:spec(), list(improvement_suggestion())) -> intent@types:spec().
apply_improvements(Spec, _) ->
    Spec.

-file("src/intent/improver.gleam", 305).
?DOC(" Format improvements for interactive display\n").
-spec format_improvements(list(improvement_suggestion())) -> binary().
format_improvements(Suggestions) ->
    Count = erlang:length(Suggestions),
    case Count of
        0 ->
            <<"No improvements suggested - spec is well-formed!"/utf8>>;

        _ ->
            Header = <<<<"Found "/utf8, (gleam@int:to_string(Count))/binary>>/binary,
                " improvement suggestion(s):\\n\\n"/utf8>>,
            Suggestion_lines = begin
                _pipe = Suggestions,
                _pipe@2 = gleam@list:index_map(
                    _pipe,
                    fun(Suggestion, I) ->
                        Idx = I + 1,
                        Impact_bar = gleam@string:repeat(
                            <<"█"/utf8>>,
                            erlang:element(5, Suggestion) div 10
                        ),
                        Impact_empty = gleam@string:repeat(
                            <<"░"/utf8>>,
                            10 - (erlang:element(5, Suggestion) div 10)
                        ),
                        <<<<<<<<<<<<<<<<<<<<<<<<(begin
                                                                            _pipe@1 = Idx,
                                                                            gleam@int:to_string(
                                                                                _pipe@1
                                                                            )
                                                                        end)/binary,
                                                                        ". "/utf8>>/binary,
                                                                    (erlang:element(
                                                                        2,
                                                                        Suggestion
                                                                    ))/binary>>/binary,
                                                                "\\n   Impact: ["/utf8>>/binary,
                                                            Impact_bar/binary>>/binary,
                                                        Impact_empty/binary>>/binary,
                                                    "] "/utf8>>/binary,
                                                (gleam@int:to_string(
                                                    erlang:element(
                                                        5,
                                                        Suggestion
                                                    )
                                                ))/binary>>/binary,
                                            "/100\\n   "/utf8>>/binary,
                                        (erlang:element(3, Suggestion))/binary>>/binary,
                                    "\\n   Why: "/utf8>>/binary,
                                (erlang:element(4, Suggestion))/binary>>/binary,
                            "\\n"/utf8>>
                    end
                ),
                gleam@string:join(_pipe@2, <<"\\n"/utf8>>)
            end,
            <<Header/binary, Suggestion_lines/binary>>
    end.
