-module(intent@output).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/output.gleam").
-export([spec_result_to_json/1, spec_result_to_text/1, create_failure/5, create_blocked/2]).
-export_type([spec_result/0, behavior_failure/0, problem/0, request_summary/0, response_summary/0, blocked_behavior/0, rule_violation_group/0, behavior_violation/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type spec_result() :: {spec_result,
        boolean(),
        integer(),
        integer(),
        integer(),
        integer(),
        binary(),
        list(behavior_failure()),
        list(blocked_behavior()),
        list(rule_violation_group()),
        list(intent@anti_patterns:anti_pattern_result())}.

-type behavior_failure() :: {behavior_failure,
        binary(),
        binary(),
        binary(),
        list(problem()),
        request_summary(),
        response_summary(),
        binary(),
        list(binary())}.

-type problem() :: {problem, binary(), binary(), binary(), binary(), binary()}.

-type request_summary() :: {request_summary,
        binary(),
        binary(),
        gleam@dict:dict(binary(), binary())}.

-type response_summary() :: {response_summary,
        integer(),
        gleam@option:option(gleam@json:json())}.

-type blocked_behavior() :: {blocked_behavior, binary(), binary(), binary()}.

-type rule_violation_group() :: {rule_violation_group,
        binary(),
        binary(),
        list(behavior_violation())}.

-type behavior_violation() :: {behavior_violation,
        binary(),
        list(binary()),
        gleam@option:option(gleam@json:json())}.

-file("src/intent/output.gleam", 129).
-spec problem_to_json(problem()) -> gleam@json:json().
problem_to_json(Problem) ->
    gleam@json:object(
        [{<<"field"/utf8>>, gleam@json:string(erlang:element(2, Problem))},
            {<<"rule"/utf8>>, gleam@json:string(erlang:element(3, Problem))},
            {<<"expected"/utf8>>, gleam@json:string(erlang:element(4, Problem))},
            {<<"actual"/utf8>>, gleam@json:string(erlang:element(5, Problem))},
            {<<"explanation"/utf8>>,
                gleam@json:string(erlang:element(6, Problem))}]
    ).

-file("src/intent/output.gleam", 139).
-spec request_summary_to_json(request_summary()) -> gleam@json:json().
request_summary_to_json(Req) ->
    gleam@json:object(
        [{<<"method"/utf8>>, gleam@json:string(erlang:element(2, Req))},
            {<<"url"/utf8>>, gleam@json:string(erlang:element(3, Req))},
            {<<"headers"/utf8>>,
                gleam@json:object(
                    begin
                        _pipe = erlang:element(4, Req),
                        _pipe@1 = maps:to_list(_pipe),
                        gleam@list:map(
                            _pipe@1,
                            fun(Pair) ->
                                {erlang:element(1, Pair),
                                    gleam@json:string(erlang:element(2, Pair))}
                            end
                        )
                    end
                )}]
    ).

-file("src/intent/output.gleam", 154).
-spec response_summary_to_json(response_summary()) -> gleam@json:json().
response_summary_to_json(Resp) ->
    gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:int(erlang:element(2, Resp))},
            {<<"body"/utf8>>,
                gleam@option:unwrap(erlang:element(3, Resp), gleam@json:null())}]
    ).

-file("src/intent/output.gleam", 116).
-spec behavior_failure_to_json(behavior_failure()) -> gleam@json:json().
behavior_failure_to_json(Failure) ->
    gleam@json:object(
        [{<<"feature"/utf8>>, gleam@json:string(erlang:element(2, Failure))},
            {<<"behavior"/utf8>>, gleam@json:string(erlang:element(3, Failure))},
            {<<"intent"/utf8>>, gleam@json:string(erlang:element(4, Failure))},
            {<<"problems"/utf8>>,
                gleam@json:array(
                    erlang:element(5, Failure),
                    fun problem_to_json/1
                )},
            {<<"request_sent"/utf8>>,
                request_summary_to_json(erlang:element(6, Failure))},
            {<<"response_received"/utf8>>,
                response_summary_to_json(erlang:element(7, Failure))},
            {<<"hint"/utf8>>, gleam@json:string(erlang:element(8, Failure))},
            {<<"see_also"/utf8>>,
                gleam@json:array(
                    erlang:element(9, Failure),
                    fun gleam@json:string/1
                )}]
    ).

-file("src/intent/output.gleam", 161).
-spec blocked_behavior_to_json(blocked_behavior()) -> gleam@json:json().
blocked_behavior_to_json(Blocked) ->
    gleam@json:object(
        [{<<"behavior"/utf8>>, gleam@json:string(erlang:element(2, Blocked))},
            {<<"reason"/utf8>>, gleam@json:string(erlang:element(3, Blocked))},
            {<<"hint"/utf8>>, gleam@json:string(erlang:element(4, Blocked))}]
    ).

-file("src/intent/output.gleam", 177).
-spec behavior_violation_to_json(behavior_violation()) -> gleam@json:json().
behavior_violation_to_json(Violation) ->
    gleam@json:object(
        [{<<"behavior"/utf8>>, gleam@json:string(erlang:element(2, Violation))},
            {<<"violations"/utf8>>,
                gleam@json:array(
                    erlang:element(3, Violation),
                    fun gleam@json:string/1
                )},
            {<<"response"/utf8>>,
                gleam@option:unwrap(
                    erlang:element(4, Violation),
                    gleam@json:null()
                )}]
    ).

-file("src/intent/output.gleam", 169).
-spec rule_violation_group_to_json(rule_violation_group()) -> gleam@json:json().
rule_violation_group_to_json(Group) ->
    gleam@json:object(
        [{<<"rule"/utf8>>, gleam@json:string(erlang:element(2, Group))},
            {<<"description"/utf8>>,
                gleam@json:string(erlang:element(3, Group))},
            {<<"violations"/utf8>>,
                gleam@json:array(
                    erlang:element(4, Group),
                    fun behavior_violation_to_json/1
                )}]
    ).

-file("src/intent/output.gleam", 185).
-spec anti_pattern_result_to_json(intent@anti_patterns:anti_pattern_result()) -> gleam@json:json().
anti_pattern_result_to_json(Result) ->
    case Result of
        no_anti_patterns ->
            gleam@json:null();

        {anti_pattern_detected, Name, Desc, Found, Bad, Good} ->
            gleam@json:object(
                [{<<"pattern"/utf8>>, gleam@json:string(Name)},
                    {<<"description"/utf8>>, gleam@json:string(Desc)},
                    {<<"found"/utf8>>, gleam@json:string(Found)},
                    {<<"see_bad_example"/utf8>>, Bad},
                    {<<"see_good_example"/utf8>>, Good}]
            )
    end.

-file("src/intent/output.gleam", 90).
?DOC(" Convert a SpecResult to JSON\n").
-spec spec_result_to_json(spec_result()) -> gleam@json:json().
spec_result_to_json(Result) ->
    gleam@json:object(
        [{<<"pass"/utf8>>, gleam@json:bool(erlang:element(2, Result))},
            {<<"score"/utf8>>,
                gleam@json:object(
                    [{<<"passed"/utf8>>,
                            gleam@json:int(erlang:element(3, Result))},
                        {<<"failed"/utf8>>,
                            gleam@json:int(erlang:element(4, Result))},
                        {<<"blocked"/utf8>>,
                            gleam@json:int(erlang:element(5, Result))},
                        {<<"total"/utf8>>,
                            gleam@json:int(erlang:element(6, Result))}]
                )},
            {<<"summary"/utf8>>, gleam@json:string(erlang:element(7, Result))},
            {<<"failures"/utf8>>,
                gleam@json:array(
                    erlang:element(8, Result),
                    fun behavior_failure_to_json/1
                )},
            {<<"blocked"/utf8>>,
                gleam@json:array(
                    erlang:element(9, Result),
                    fun blocked_behavior_to_json/1
                )},
            {<<"rule_violations"/utf8>>,
                gleam@json:array(
                    erlang:element(10, Result),
                    fun rule_violation_group_to_json/1
                )},
            {<<"anti_patterns_detected"/utf8>>,
                gleam@json:array(
                    erlang:element(11, Result),
                    fun anti_pattern_result_to_json/1
                )}]
    ).

-file("src/intent/output.gleam", 256).
-spec format_failure(behavior_failure()) -> binary().
format_failure(Failure) ->
    Problems_text = begin
        _pipe = erlang:element(5, Failure),
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(P) ->
                <<<<<<<<<<<<<<"  - "/utf8, (erlang:element(2, P))/binary>>/binary,
                                        ": "/utf8>>/binary,
                                    (erlang:element(6, P))/binary>>/binary,
                                "\n    Expected: "/utf8>>/binary,
                            (erlang:element(4, P))/binary>>/binary,
                        "\n    Actual: "/utf8>>/binary,
                    (erlang:element(5, P))/binary>>
            end
        ),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"["/utf8,
                                                                            (erlang:element(
                                                                                2,
                                                                                Failure
                                                                            ))/binary>>/binary,
                                                                        "] "/utf8>>/binary,
                                                                    (erlang:element(
                                                                        3,
                                                                        Failure
                                                                    ))/binary>>/binary,
                                                                "\n"/utf8>>/binary,
                                                            "Intent: "/utf8>>/binary,
                                                        (erlang:element(
                                                            4,
                                                            Failure
                                                        ))/binary>>/binary,
                                                    "\n"/utf8>>/binary,
                                                "Problems:\n"/utf8>>/binary,
                                            Problems_text/binary>>/binary,
                                        "\n"/utf8>>/binary,
                                    "Request: "/utf8>>/binary,
                                (erlang:element(2, erlang:element(6, Failure)))/binary>>/binary,
                            " "/utf8>>/binary,
                        (erlang:element(3, erlang:element(6, Failure)))/binary>>/binary,
                    "\n"/utf8>>/binary,
                "Response: "/utf8>>/binary,
            (gleam@int:to_string(erlang:element(2, erlang:element(7, Failure))))/binary>>/binary,
        (case erlang:element(8, Failure) of
            <<""/utf8>> ->
                <<""/utf8>>;

            Hint ->
                <<"\nHint: "/utf8, Hint/binary>>
        end)/binary>>.

-file("src/intent/output.gleam", 295).
-spec format_blocked(blocked_behavior()) -> binary().
format_blocked(Blocked) ->
    <<<<<<<<"- "/utf8, (erlang:element(2, Blocked))/binary>>/binary, ": "/utf8>>/binary,
            (erlang:element(3, Blocked))/binary>>/binary,
        (case erlang:element(4, Blocked) of
            <<""/utf8>> ->
                <<""/utf8>>;

            Hint ->
                <<<<" ("/utf8, Hint/binary>>/binary, ")"/utf8>>
        end)/binary>>.

-file("src/intent/output.gleam", 306).
-spec format_rule_violation_group(rule_violation_group()) -> binary().
format_rule_violation_group(Group) ->
    Violations_text = begin
        _pipe = erlang:element(4, Group),
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(V) ->
                <<<<<<"  - "/utf8, (erlang:element(2, V))/binary>>/binary,
                        ": "/utf8>>/binary,
                    (gleam@string:join(erlang:element(3, V), <<", "/utf8>>))/binary>>
            end
        ),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    <<<<<<<<(erlang:element(2, Group))/binary, " ("/utf8>>/binary,
                (erlang:element(3, Group))/binary>>/binary,
            "):\n"/utf8>>/binary,
        Violations_text/binary>>.

-file("src/intent/output.gleam", 200).
?DOC(" Convert a SpecResult to human-readable text\n").
-spec spec_result_to_text(spec_result()) -> binary().
spec_result_to_text(Result) ->
    Header = case erlang:element(2, Result) of
        true ->
            <<"PASS"/utf8>>;

        false ->
            <<"FAIL"/utf8>>
    end,
    Score = <<<<<<<<<<<<<<"Passed: "/utf8,
                                (gleam@int:to_string(erlang:element(3, Result)))/binary>>/binary,
                            " / Failed: "/utf8>>/binary,
                        (gleam@int:to_string(erlang:element(4, Result)))/binary>>/binary,
                    " / Blocked: "/utf8>>/binary,
                (gleam@int:to_string(erlang:element(5, Result)))/binary>>/binary,
            " / Total: "/utf8>>/binary,
        (gleam@int:to_string(erlang:element(6, Result)))/binary>>,
    Failures_text = case erlang:element(8, Result) of
        [] ->
            <<""/utf8>>;

        Failures ->
            <<"\n\nFAILURES:\n"/utf8,
                (gleam@string:join(
                    gleam@list:map(Failures, fun format_failure/1),
                    <<"\n\n"/utf8>>
                ))/binary>>
    end,
    Blocked_text = case erlang:element(9, Result) of
        [] ->
            <<""/utf8>>;

        Blocked ->
            <<"\n\nBLOCKED:\n"/utf8,
                (gleam@string:join(
                    gleam@list:map(Blocked, fun format_blocked/1),
                    <<"\n"/utf8>>
                ))/binary>>
    end,
    Rules_text = case erlang:element(10, Result) of
        [] ->
            <<""/utf8>>;

        Violations ->
            <<"\n\nRULE VIOLATIONS:\n"/utf8,
                (gleam@string:join(
                    gleam@list:map(
                        Violations,
                        fun format_rule_violation_group/1
                    ),
                    <<"\n"/utf8>>
                ))/binary>>
    end,
    Anti_patterns_text = case erlang:element(11, Result) of
        [] ->
            <<""/utf8>>;

        Patterns ->
            <<"\n\nANTI-PATTERNS DETECTED:\n"/utf8,
                (gleam@string:join(
                    gleam@list:map(
                        Patterns,
                        fun intent@anti_patterns:format_anti_pattern/1
                    ),
                    <<"\n"/utf8>>
                ))/binary>>
    end,
    <<<<<<<<<<<<<<<<Header/binary, "\n"/utf8>>/binary, Score/binary>>/binary,
                            "\n"/utf8>>/binary,
                        (erlang:element(7, Result))/binary>>/binary,
                    Failures_text/binary>>/binary,
                Blocked_text/binary>>/binary,
            Rules_text/binary>>/binary,
        Anti_patterns_text/binary>>.

-file("src/intent/output.gleam", 373).
-spec generate_hint(
    intent@types:behavior(),
    intent@checker:response_check_result()
) -> binary().
generate_hint(_, Check_result) ->
    case erlang:element(4, Check_result) of
        false ->
            case erlang:element(6, Check_result) of
                404 ->
                    <<"The resource might not exist. Check that prerequisite behaviors ran successfully."/utf8>>;

                401 ->
                    <<"Authentication may be required. Check that the auth token is being passed correctly."/utf8>>;

                403 ->
                    <<"Access denied. Check permissions and that the correct user is authenticated."/utf8>>;

                500 ->
                    <<"Server error. Check server logs for details."/utf8>>;

                _ ->
                    <<""/utf8>>
            end;

        true ->
            case erlang:length(erlang:element(3, Check_result)) of
                0 ->
                    <<""/utf8>>;

                _ ->
                    <<"Check the field paths and expected values in the spec."/utf8>>
            end
    end.

-file("src/intent/output.gleam", 318).
?DOC(" Create a BehaviorFailure from check results\n").
-spec create_failure(
    binary(),
    intent@types:behavior(),
    intent@checker:response_check_result(),
    intent@http_client:execution_result(),
    binary()
) -> behavior_failure().
create_failure(Feature_name, Behavior, Check_result, Execution, Base_url) ->
    Problems = begin
        _pipe = erlang:element(3, Check_result),
        gleam@list:map(_pipe, fun(Check) -> case Check of
                    {check_failed, Field, Rule, Expected, Actual, Explanation} ->
                        {problem, Field, Rule, Expected, Actual, Explanation};

                    {check_passed, _, _} ->
                        {problem,
                            <<""/utf8>>,
                            <<""/utf8>>,
                            <<""/utf8>>,
                            <<""/utf8>>,
                            <<""/utf8>>}
                end end)
    end,
    Problems@1 = case erlang:element(4, Check_result) of
        true ->
            Problems;

        false ->
            [{problem,
                    <<"status"/utf8>>,
                    <<"equals "/utf8,
                        (gleam@int:to_string(erlang:element(5, Check_result)))/binary>>,
                    gleam@int:to_string(erlang:element(5, Check_result)),
                    gleam@int:to_string(erlang:element(6, Check_result)),
                    <<"HTTP status code mismatch"/utf8>>} |
                Problems]
    end,
    Url = <<Base_url/binary,
        (erlang:element(3, erlang:element(7, Behavior)))/binary>>,
    {behavior_failure,
        Feature_name,
        erlang:element(2, Behavior),
        erlang:element(3, Behavior),
        Problems@1,
        {request_summary,
            intent@types:method_to_string(
                erlang:element(2, erlang:element(7, Behavior))
            ),
            Url,
            erlang:element(4, erlang:element(7, Behavior))},
        {response_summary,
            erlang:element(2, Execution),
            erlang:element(4, Execution)},
        generate_hint(Behavior, Check_result),
        erlang:element(5, Behavior)}.

-file("src/intent/output.gleam", 395).
?DOC(" Create a BlockedBehavior\n").
-spec create_blocked(binary(), binary()) -> blocked_behavior().
create_blocked(Behavior_name, Failed_dependency) ->
    {blocked_behavior,
        Behavior_name,
        <<<<"Requires '"/utf8, Failed_dependency/binary>>/binary,
            "' which failed"/utf8>>,
        <<<<"Fix '"/utf8, Failed_dependency/binary>>/binary,
            "' first, then this will run"/utf8>>}.
