-module(intent@runner).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/runner.gleam").
-export([default_options/0, run_spec/3]).
-export_type([run_options/0, behavior_result/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type run_options() :: {run_options,
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        boolean()}.

-type behavior_result() :: {behavior_passed,
        intent@http_client:execution_result()} |
    {behavior_failed,
        intent@output:behavior_failure(),
        intent@http_client:execution_result()} |
    {behavior_blocked, binary(), binary()} |
    {behavior_error, binary(), intent@http_client:execution_error()}.

-file("src/intent/runner.gleam", 29).
?DOC(" Default run options\n").
-spec default_options() -> run_options().
default_options() ->
    {run_options, none, none, false}.

-file("src/intent/runner.gleam", 163).
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

-file("src/intent/runner.gleam", 277).
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

-file("src/intent/runner.gleam", 206).
-spec execute_single_behavior(
    intent@resolver:resolved_behavior(),
    intent@types:config(),
    intent@types:spec(),
    intent@interpolate:context(),
    gleam@set:set(binary())
) -> {behavior_result(), intent@interpolate:context(), gleam@set:set(binary())}.
execute_single_behavior(Rb, Config, _, Ctx, Failed_set) ->
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
            case intent@http_client:execute_request(
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
                    Ctx@1 = case erlang:element(4, Execution) of
                        {some, Body} ->
                            intent@interpolate:set_response_body(Ctx, Body);

                        none ->
                            Ctx
                    end,
                    Ctx@2 = case erlang:element(
                        6,
                        erlang:element(7, erlang:element(3, Rb))
                    ) of
                        {some, Body@1} ->
                            intent@interpolate:set_request_body(Ctx@1, Body@1);

                        none ->
                            Ctx@1
                    end,
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
                                Check_result,
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

-file("src/intent/runner.gleam", 181).
-spec execute_behaviors_with_spinner(
    list(intent@resolver:resolved_behavior()),
    intent@types:config(),
    intent@types:spec(),
    gleam@set:set(binary()),
    spinner:spinner()
) -> {list(behavior_result()),
    intent@interpolate:context(),
    gleam@set:set(binary())}.
execute_behaviors_with_spinner(Behaviors, Config, Spec, Failed_set, Sp) ->
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
                Failed
            ),
            {[Result | Results], New_ctx, New_failed}
        end
    ),
    (fun(Tuple) ->
        {Results@1, Ctx@1, Failed@1} = Tuple,
        {lists:reverse(Results@1), Ctx@1, Failed@1}
    end)(_pipe).

-file("src/intent/runner.gleam", 307).
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
                                        erlang:element(4, Execution)}}};

                        _ ->
                            {error, nil}
                    end end)
        end
    ).

-file("src/intent/runner.gleam", 333).
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

-file("src/intent/runner.gleam", 290).
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

-file("src/intent/runner.gleam", 354).
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

-file("src/intent/runner.gleam", 34).
?DOC(" Run a spec and return the results\n").
-spec run_spec(intent@types:spec(), binary(), run_options()) -> intent@output:spec_result().
run_spec(Spec, Target_url, Options) ->
    Config = case gleam@string:is_empty(Target_url) of
        true ->
            erlang:element(7, Spec);

        false ->
            _record = erlang:element(7, Spec),
            {config,
                Target_url,
                erlang:element(3, _record),
                erlang:element(4, _record)}
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
                Sp
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
                Blocked_behaviors,
                Rule_violations,
                Anti_patterns}
    end.
