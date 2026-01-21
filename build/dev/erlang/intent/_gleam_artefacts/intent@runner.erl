-module(intent@runner).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([default_executor/0, default_options/0, is_verbose/1, is_quiet/1, run_spec_with_executor/4, run_spec/3]).
-export_type([behavior_executor/0, output_level/0, run_options/0, behavior_result/0]).

-type behavior_executor() :: {behavior_executor,
        fun((intent@types:config(), intent@types:request(), intent@interpolate:context()) -> {ok,
                intent@http_client:execution_result()} |
            {error, intent@http_client:execution_error()})}.

-type output_level() :: quiet | normal | verbose.

-type run_options() :: {run_options,
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        output_level()}.

-type behavior_result() :: {behavior_passed,
        intent@http_client:execution_result()} |
    {behavior_failed,
        intent@output:behavior_failure(),
        intent@http_client:execution_result()} |
    {behavior_blocked, binary(), binary()} |
    {behavior_error, binary(), intent@http_client:execution_error()}.

-spec default_executor() -> behavior_executor().
default_executor() ->
    {behavior_executor, fun intent@http_client:execute_request/3}.

-spec default_options() -> run_options().
default_options() ->
    {run_options, none, none, normal}.

-spec is_verbose(run_options()) -> boolean().
is_verbose(Options) ->
    case erlang:element(4, Options) of
        verbose ->
            true;

        _ ->
            false
    end.

-spec is_quiet(run_options()) -> boolean().
is_quiet(Options) ->
    case erlang:element(4, Options) of
        quiet ->
            true;

        _ ->
            false
    end.

-spec apply_filters(list(intent@resolver:resolved_behavior()), run_options()) -> list(intent@resolver:resolved_behavior()).
apply_filters(Behaviors, Options) ->
    _pipe = Behaviors,
    gleam@list:filter(
        _pipe,
        fun(Rb) ->
            Feature_ok = case erlang:element(2, Options) of
                none ->
                    true;

                {some, F} ->
                    erlang:element(2, Rb) =:= F
            end,
            Behavior_ok = case erlang:element(3, Options) of
                none ->
                    true;

                {some, B} ->
                    erlang:element(2, erlang:element(3, Rb)) =:= B
            end,
            Feature_ok andalso Behavior_ok
        end
    ).

-spec convert_response_check_result(intent@checker:response_check_result()) -> intent@checker@types:response_check_result().
convert_response_check_result(Result) ->
    Passed = gleam@list:map(
        erlang:element(2, Result),
        fun(Check) -> case Check of
                {check_passed, Field, Rule} ->
                    {check_passed, Field, Rule};

                {check_failed, Field@1, Rule@1, Expected, Actual, Explanation} ->
                    {check_failed,
                        Field@1,
                        Rule@1,
                        Expected,
                        Actual,
                        Explanation}
            end end
    ),
    Failed = gleam@list:map(
        erlang:element(3, Result),
        fun(Check@1) -> case Check@1 of
                {check_passed, Field@2, Rule@2} ->
                    {check_passed, Field@2, Rule@2};

                {check_failed,
                    Field@3,
                    Rule@3,
                    Expected@1,
                    Actual@1,
                    Explanation@1} ->
                    {check_failed,
                        Field@3,
                        Rule@3,
                        Expected@1,
                        Actual@1,
                        Explanation@1}
            end end
    ),
    {response_check_result,
        Passed,
        Failed,
        erlang:element(4, Result),
        erlang:element(5, Result),
        erlang:element(6, Result)}.

-spec apply_captures(
    intent@interpolate:context(),
    intent@types:behavior(),
    intent@http_client:execution_result()
) -> intent@interpolate:context().
apply_captures(Ctx, Behavior, _) ->
    gleam@dict:fold(
        erlang:element(9, Behavior),
        Ctx,
        fun(Acc_ctx, Name, Path) ->
            case intent@interpolate:extract_capture(Acc_ctx, Path) of
                {ok, Value} ->
                    intent@interpolate:set_variable(Acc_ctx, Name, Value);

                {error, _} ->
                    Acc_ctx
            end
        end
    ).

-spec execute_single_behavior(
    intent@resolver:resolved_behavior(),
    intent@types:config(),
    intent@types:spec(),
    intent@interpolate:context(),
    gleam@set:set(binary()),
    behavior_executor()
) -> {behavior_result(), intent@interpolate:context(), gleam@set:set(binary())}.
execute_single_behavior(Rb, Config, _, Ctx, Failed_set, Executor) ->
    Blocked_by = gleam@list:find(
        erlang:element(5, erlang:element(3, Rb)),
        fun(Dep) -> gleam@set:contains(Failed_set, Dep) end
    ),
    case Blocked_by of
        {ok, Dep@1} ->
            Result = {behavior_blocked,
                erlang:element(2, erlang:element(3, Rb)),
                Dep@1},
            {Result,
                Ctx,
                gleam@set:insert(
                    Failed_set,
                    erlang:element(2, erlang:element(3, Rb))
                )};

        {error, _} ->
            case (erlang:element(2, Executor))(
                Config,
                erlang:element(7, erlang:element(3, Rb)),
                Ctx
            ) of
                {error, E} ->
                    Result@1 = {behavior_error,
                        erlang:element(2, erlang:element(3, Rb)),
                        E},
                    {Result@1,
                        Ctx,
                        gleam@set:insert(
                            Failed_set,
                            erlang:element(2, erlang:element(3, Rb))
                        )};

                {ok, Execution} ->
                    Ctx@1 = intent@interpolate:set_response_body(
                        Ctx,
                        erlang:element(4, Execution)
                    ),
                    Ctx@2 = intent@interpolate:set_request_body(
                        Ctx@1,
                        erlang:element(
                            6,
                            erlang:element(7, erlang:element(3, Rb))
                        )
                    ),
                    Check_result = intent@checker:check_response(
                        erlang:element(8, erlang:element(3, Rb)),
                        Execution,
                        Ctx@2
                    ),
                    Passed = erlang:element(4, Check_result) andalso gleam@list:is_empty(
                        erlang:element(3, Check_result)
                    ),
                    case Passed of
                        true ->
                            New_ctx = apply_captures(
                                Ctx@2,
                                erlang:element(3, Rb),
                                Execution
                            ),
                            Result@2 = {behavior_passed, Execution},
                            {Result@2, New_ctx, Failed_set};

                        false ->
                            Failure = intent@output:create_failure(
                                erlang:element(2, Rb),
                                erlang:element(3, Rb),
                                convert_response_check_result(Check_result),
                                Execution,
                                erlang:element(2, Config)
                            ),
                            Result@3 = {behavior_failed, Failure, Execution},
                            {Result@3,
                                Ctx@2,
                                gleam@set:insert(
                                    Failed_set,
                                    erlang:element(2, erlang:element(3, Rb))
                                )}
                    end
            end
    end.

-spec execute_behaviors_with_spinner(
    list(intent@resolver:resolved_behavior()),
    intent@types:config(),
    intent@types:spec(),
    gleam@set:set(binary()),
    spinner:spinner(),
    behavior_executor()
) -> {list(behavior_result()),
    intent@interpolate:context(),
    gleam@set:set(binary())}.
execute_behaviors_with_spinner(
    Behaviors,
    Config,
    Spec,
    Failed_set,
    Sp,
    Executor
) ->
    _pipe = gleam@list:fold(
        Behaviors,
        {[], intent@interpolate:new_context(), Failed_set},
        fun(Acc, Rb) ->
            {Results, Ctx, Failed} = Acc,
            spinner:set_text(
                Sp,
                <<"Testing: "/utf8,
                    (erlang:element(2, erlang:element(3, Rb)))/binary>>
            ),
            {Result, New_ctx, New_failed} = execute_single_behavior(
                Rb,
                Config,
                Spec,
                Ctx,
                Failed,
                Executor
            ),
            {[Result | Results], New_ctx, New_failed}
        end
    ),
    (fun(Tuple) ->
        {Results@1, Ctx@1, Failed@1} = Tuple,
        {lists:reverse(Results@1), Ctx@1, Failed@1}
    end)(_pipe).

-spec check_rules_for_execution(
    intent@http_client:execution_result(),
    list(intent@types:rule()),
    binary()
) -> list({binary(), binary(), intent@output:behavior_violation()}).
check_rules_for_execution(Execution, Rules, Behavior_name) ->
    _pipe = Rules,
    gleam@list:flat_map(
        _pipe,
        fun(Rule) ->
            Results = intent@rules_engine:check_rules(
                [Rule],
                Execution,
                Behavior_name
            ),
            gleam@list:filter_map(Results, fun(R) -> case R of
                        {rule_failed, Name, Desc, Violations} ->
                            {ok,
                                {Name,
                                    Desc,
                                    {behavior_violation,
                                        Behavior_name,
                                        gleam@list:map(
                                            Violations,
                                            fun intent@rules_engine:format_violation/1
                                        ),
                                        {some, erlang:element(4, Execution)}}}};

                        _ ->
                            {error, nil}
                    end end)
        end
    ).

-spec group_violations_by_rule(
    list({binary(), binary(), intent@output:behavior_violation()})
) -> list(intent@output:rule_violation_group()).
group_violations_by_rule(Violations) ->
    _pipe = Violations,
    _pipe@1 = gleam@list:group(_pipe, fun(V) -> erlang:element(1, V) end),
    _pipe@2 = maps:to_list(_pipe@1),
    gleam@list:map(
        _pipe@2,
        fun(Pair) ->
            {Rule_name, Items} = Pair,
            Description = case Items of
                [{_, Desc, _} | _] ->
                    Desc;

                [] ->
                    <<""/utf8>>
            end,
            Behavior_violations = gleam@list:map(
                Items,
                fun(Item) -> erlang:element(3, Item) end
            ),
            {rule_violation_group, Rule_name, Description, Behavior_violations}
        end
    ).

-spec collect_rule_violations(
    list(behavior_result()),
    list(intent@types:rule())
) -> list(intent@output:rule_violation_group()).
collect_rule_violations(Results, Rules) ->
    _pipe = Results,
    _pipe@1 = gleam@list:flat_map(_pipe, fun(Result) -> case Result of
                {behavior_passed, Execution} ->
                    check_rules_for_execution(Execution, Rules, <<""/utf8>>);

                {behavior_failed, Failure, Execution@1} ->
                    check_rules_for_execution(
                        Execution@1,
                        Rules,
                        erlang:element(3, Failure)
                    );

                _ ->
                    []
            end end),
    group_violations_by_rule(_pipe@1).

-spec collect_anti_patterns(
    list(behavior_result()),
    list(intent@types:anti_pattern())
) -> list(intent@anti_patterns:anti_pattern_result()).
collect_anti_patterns(Results, Patterns) ->
    _pipe = Results,
    gleam@list:flat_map(_pipe, fun(Result) -> case Result of
                {behavior_passed, Execution} ->
                    intent@anti_patterns:check_anti_patterns(
                        Patterns,
                        Execution,
                        <<""/utf8>>
                    );

                {behavior_failed, Failure, Execution@1} ->
                    intent@anti_patterns:check_anti_patterns(
                        Patterns,
                        Execution@1,
                        erlang:element(3, Failure)
                    );

                _ ->
                    []
            end end).

-spec run_spec_with_executor(
    intent@types:spec(),
    binary(),
    run_options(),
    behavior_executor()
) -> intent@output:spec_result().
run_spec_with_executor(Spec, Target_url, Options, Executor) ->
    Config = case gleam@string:is_empty(Target_url) of
        true ->
            erlang:element(7, Spec);

        false ->
            erlang:setelement(2, erlang:element(7, Spec), Target_url)
    end,
    case intent@resolver:resolve_execution_order(Spec) of
        {error, E} ->
            {spec_result,
                false,
                0,
                0,
                0,
                0,
                <<"Failed to resolve behavior order: "/utf8,
                    (intent@resolver:format_error(E))/binary>>,
                [],
                [],
                [],
                [],
                []};

        {ok, Resolved} ->
            Filtered = apply_filters(Resolved, Options),
            Total = erlang:length(Filtered),
            Sp = begin
                _pipe = spinner:new(
                    <<<<"Running "/utf8, (gleam@string:inspect(Total))/binary>>/binary,
                        " behaviors..."/utf8>>
                ),
                _pipe@1 = spinner:with_colour(
                    _pipe,
                    fun gleam_community@ansi:cyan/1
                ),
                spinner:start(_pipe@1)
            end,
            {Results, _, _} = execute_behaviors_with_spinner(
                Filtered,
                Config,
                Spec,
                gleam@set:new(),
                Sp,
                Executor
            ),
            spinner:stop(Sp),
            Passed = gleam@list:count(Results, fun(R) -> case R of
                        {behavior_passed, _} ->
                            true;

                        _ ->
                            false
                    end end),
            Failed = gleam@list:count(Results, fun(R@1) -> case R@1 of
                        {behavior_failed, _, _} ->
                            true;

                        {behavior_error, _, _} ->
                            true;

                        _ ->
                            false
                    end end),
            Blocked = gleam@list:count(Results, fun(R@2) -> case R@2 of
                        {behavior_blocked, _, _} ->
                            true;

                        _ ->
                            false
                    end end),
            Failures = gleam@list:filter_map(Results, fun(R@3) -> case R@3 of
                        {behavior_failed, Failure, _} ->
                            {ok, Failure};

                        _ ->
                            {error, nil}
                    end end),
            Blocked_behaviors = gleam@list:filter_map(
                Results,
                fun(R@4) -> case R@4 of
                        {behavior_blocked, Name, Dep} ->
                            {ok, intent@output:create_blocked(Name, Dep)};

                        _ ->
                            {error, nil}
                    end end
            ),
            Error_failures = gleam@list:filter_map(
                Results,
                fun(R@5) -> case R@5 of
                        {behavior_error, Name@1, Error} ->
                            {Error_type, Message} = case Error of
                                {url_parse_error, Msg} ->
                                    {<<"URL_PARSE_ERROR"/utf8>>, Msg};

                                {interpolation_error, Msg@1} ->
                                    {<<"INTERPOLATION_ERROR"/utf8>>, Msg@1};

                                {request_error, Msg@2} ->
                                    Contains_refused = gleam_stdlib:contains_string(
                                        Msg@2,
                                        <<"connection refused"/utf8>>
                                    ),
                                    Contains_timeout = gleam_stdlib:contains_string(
                                        Msg@2,
                                        <<"timeout"/utf8>>
                                    ),
                                    Contains_resolve = gleam_stdlib:contains_string(
                                        Msg@2,
                                        <<"resolve"/utf8>>
                                    ),
                                    case {Contains_refused,
                                        Contains_timeout,
                                        Contains_resolve} of
                                        {true, _, _} ->
                                            {<<"CONNECTION_REFUSED"/utf8>>,
                                                Msg@2};

                                        {_, true, _} ->
                                            {<<"TIMEOUT"/utf8>>, Msg@2};

                                        {_, _, true} ->
                                            {<<"DNS_FAILURE"/utf8>>, Msg@2};

                                        {_, _, _} ->
                                            {<<"REQUEST_ERROR"/utf8>>, Msg@2}
                                    end;

                                {response_parse_error, Msg@3} ->
                                    {<<"RESPONSE_PARSE_ERROR"/utf8>>, Msg@3};

                                {ssrf_blocked, Msg@4} ->
                                    {<<"SSRF_BLOCKED"/utf8>>, Msg@4}
                            end,
                            {ok,
                                intent@output:create_error_info(
                                    Name@1,
                                    Error_type,
                                    Message
                                )};

                        _ ->
                            {error, nil}
                    end end
            ),
            Rule_violations = collect_rule_violations(
                Results,
                erlang:element(9, Spec)
            ),
            Anti_patterns = collect_anti_patterns(
                Results,
                erlang:element(10, Spec)
            ),
            Pass = (Failed =:= 0) andalso (Blocked =:= 0),
            Summary = case Pass of
                true ->
                    <<<<"All "/utf8, (gleam@string:inspect(Passed))/binary>>/binary,
                        " behaviors passed"/utf8>>;

                false ->
                    <<<<<<<<<<(gleam@string:inspect(Failed))/binary,
                                        " failures, "/utf8>>/binary,
                                    (gleam@string:inspect(Blocked))/binary>>/binary,
                                " blocked out of "/utf8>>/binary,
                            (gleam@string:inspect(Total))/binary>>/binary,
                        " behaviors"/utf8>>
            end,
            {spec_result,
                Pass,
                Passed,
                Failed,
                Blocked,
                Total,
                Summary,
                Failures,
                Error_failures,
                Blocked_behaviors,
                Rule_violations,
                Anti_patterns}
    end.

-spec run_spec(intent@types:spec(), binary(), run_options()) -> intent@output:spec_result().
run_spec(Spec, Target_url, Options) ->
    run_spec_with_executor(Spec, Target_url, Options, default_executor()).
