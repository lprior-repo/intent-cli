-module(intent@resolver).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/resolver.gleam").
-export([filter_by_feature/2, filter_by_name/2, get_dependents/2, resolve_execution_order/1, format_error/1]).
-export_type([resolved_behavior/0, resolve_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type resolved_behavior() :: {resolved_behavior,
        binary(),
        intent@types:behavior()}.

-type resolve_error() :: {cyclic_dependency, list(binary())} |
    {missing_dependency, binary(), binary()} |
    {duplicate_behavior_name, binary()}.

-file("src/intent/resolver.gleam", 40).
?DOC(" Filter behaviors by a specific feature name\n").
-spec filter_by_feature(list(resolved_behavior()), binary()) -> list(resolved_behavior()).
filter_by_feature(Behaviors, Feature_name) ->
    gleam@list:filter(
        Behaviors,
        fun(Rb) -> erlang:element(2, Rb) =:= Feature_name end
    ).

-file("src/intent/resolver.gleam", 48).
?DOC(" Filter behaviors by a specific behavior name\n").
-spec filter_by_name(list(resolved_behavior()), binary()) -> list(resolved_behavior()).
filter_by_name(Behaviors, Name) ->
    gleam@list:filter(
        Behaviors,
        fun(Rb) -> erlang:element(2, erlang:element(3, Rb)) =:= Name end
    ).

-file("src/intent/resolver.gleam", 56).
?DOC(" Get behaviors that depend on a given behavior\n").
-spec get_dependents(list(resolved_behavior()), binary()) -> list(resolved_behavior()).
get_dependents(Behaviors, Name) ->
    gleam@list:filter(
        Behaviors,
        fun(Rb) ->
            gleam@list:contains(erlang:element(5, erlang:element(3, Rb)), Name)
        end
    ).

-file("src/intent/resolver.gleam", 63).
-spec collect_all_behaviors(intent@types:spec()) -> list(resolved_behavior()).
collect_all_behaviors(Spec) ->
    _pipe = erlang:element(8, Spec),
    gleam@list:flat_map(
        _pipe,
        fun(Feature) -> _pipe@1 = erlang:element(4, Feature),
            gleam@list:map(
                _pipe@1,
                fun(Behavior) ->
                    {resolved_behavior, erlang:element(2, Feature), Behavior}
                end
            ) end
    ).

-file("src/intent/resolver.gleam", 80).
-spec check_duplicates_loop(list(binary()), gleam@set:set(binary())) -> {ok,
        nil} |
    {error, resolve_error()}.
check_duplicates_loop(Names, Seen) ->
    case Names of
        [] ->
            {ok, nil};

        [Name | Rest] ->
            case gleam@set:contains(Seen, Name) of
                true ->
                    {error, {duplicate_behavior_name, Name}};

                false ->
                    check_duplicates_loop(Rest, gleam@set:insert(Seen, Name))
            end
    end.

-file("src/intent/resolver.gleam", 73).
-spec check_duplicates(list(resolved_behavior())) -> {ok, nil} |
    {error, resolve_error()}.
check_duplicates(Behaviors) ->
    Names = gleam@list:map(
        Behaviors,
        fun(Rb) -> erlang:element(2, erlang:element(3, Rb)) end
    ),
    check_duplicates_loop(Names, gleam@set:new()).

-file("src/intent/resolver.gleam", 94).
-spec build_dependency_graph(list(resolved_behavior())) -> {ok,
        gleam@dict:dict(binary(), list(binary()))} |
    {error, resolve_error()}.
build_dependency_graph(Behaviors) ->
    Behavior_names = begin
        _pipe = Behaviors,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Rb) -> erlang:element(2, erlang:element(3, Rb)) end
        ),
        gleam@set:from_list(_pipe@1)
    end,
    _pipe@2 = Behaviors,
    gleam@list:try_fold(
        _pipe@2,
        gleam@dict:new(),
        fun(Graph, Rb@1) ->
            gleam@result:'try'(
                gleam@list:try_each(
                    erlang:element(5, erlang:element(3, Rb@1)),
                    fun(Dep) -> case gleam@set:contains(Behavior_names, Dep) of
                            true ->
                                {ok, nil};

                            false ->
                                {error,
                                    {missing_dependency,
                                        erlang:element(
                                            2,
                                            erlang:element(3, Rb@1)
                                        ),
                                        Dep}}
                        end end
                ),
                fun(_) ->
                    {ok,
                        gleam@dict:insert(
                            Graph,
                            erlang:element(2, erlang:element(3, Rb@1)),
                            erlang:element(5, erlang:element(3, Rb@1))
                        )}
                end
            )
        end
    ).

-file("src/intent/resolver.gleam", 141).
-spec calculate_in_degrees(
    list(resolved_behavior()),
    gleam@dict:dict(binary(), list(binary()))
) -> gleam@dict:dict(binary(), integer()).
calculate_in_degrees(Behaviors, Graph) ->
    Initial = begin
        _pipe = Behaviors,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Rb) -> {erlang:element(2, erlang:element(3, Rb)), 0} end
        ),
        maps:from_list(_pipe@1)
    end,
    gleam@dict:fold(
        Graph,
        Initial,
        fun(Degrees, Node, Deps) ->
            gleam@dict:insert(Degrees, Node, erlang:length(Deps))
        end
    ).

-file("src/intent/resolver.gleam", 207).
-spec update_in_degrees_for_dependents(
    binary(),
    gleam@dict:dict(binary(), list(binary())),
    gleam@dict:dict(binary(), integer())
) -> {gleam@dict:dict(binary(), integer()), list(binary())}.
update_in_degrees_for_dependents(Completed, Graph, In_degrees) ->
    Dependents = begin
        _pipe = Graph,
        _pipe@1 = gleam@dict:filter(
            _pipe,
            fun(_, Deps) -> gleam@list:contains(Deps, Completed) end
        ),
        gleam@dict:keys(_pipe@1)
    end,
    {New_degrees@1, Newly_ready} = gleam@list:fold(
        Dependents,
        {In_degrees, []},
        fun(Acc, Dep) ->
            {Degrees, Ready} = Acc,
            case gleam@dict:get(Degrees, Dep) of
                {ok, Count} ->
                    New_count = Count - 1,
                    New_degrees = gleam@dict:insert(Degrees, Dep, New_count),
                    case New_count =:= 0 of
                        true ->
                            {New_degrees, [Dep | Ready]};

                        false ->
                            {New_degrees, Ready}
                    end;

                {error, _} ->
                    Acc
            end
        end
    ),
    {New_degrees@1, Newly_ready}.

-file("src/intent/resolver.gleam", 159).
-spec kahn_loop(
    list(binary()),
    gleam@dict:dict(binary(), integer()),
    gleam@dict:dict(binary(), list(binary())),
    gleam@dict:dict(binary(), resolved_behavior()),
    list(resolved_behavior())
) -> {ok, list(resolved_behavior())} | {error, resolve_error()}.
kahn_loop(Queue, In_degrees, Graph, By_name, Result) ->
    case Queue of
        [] ->
            Remaining = begin
                _pipe = In_degrees,
                _pipe@1 = gleam@dict:filter(
                    _pipe,
                    fun(_, Degree) -> Degree > 0 end
                ),
                gleam@dict:keys(_pipe@1)
            end,
            case Remaining of
                [] ->
                    {ok, lists:reverse(Result)};

                _ ->
                    {error, {cyclic_dependency, Remaining}}
            end;

        [Node | Rest_queue] ->
            case gleam@dict:get(By_name, Node) of
                {ok, Rb} ->
                    {New_degrees, New_ready} = update_in_degrees_for_dependents(
                        Node,
                        Graph,
                        In_degrees
                    ),
                    Updated_queue = lists:append(Rest_queue, New_ready),
                    kahn_loop(
                        Updated_queue,
                        New_degrees,
                        Graph,
                        By_name,
                        [Rb | Result]
                    );

                {error, _} ->
                    kahn_loop(Rest_queue, In_degrees, Graph, By_name, Result)
            end
    end.

-file("src/intent/resolver.gleam", 119).
?DOC(" Topological sort using Kahn's algorithm\n").
-spec topological_sort(
    list(resolved_behavior()),
    gleam@dict:dict(binary(), list(binary()))
) -> {ok, list(resolved_behavior())} | {error, resolve_error()}.
topological_sort(Behaviors, Graph) ->
    By_name = begin
        _pipe = Behaviors,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Rb) -> {erlang:element(2, erlang:element(3, Rb)), Rb} end
        ),
        maps:from_list(_pipe@1)
    end,
    In_degrees = calculate_in_degrees(Behaviors, Graph),
    Initial_queue = begin
        _pipe@2 = In_degrees,
        _pipe@3 = gleam@dict:filter(_pipe@2, fun(_, Degree) -> Degree =:= 0 end),
        gleam@dict:keys(_pipe@3)
    end,
    kahn_loop(Initial_queue, In_degrees, Graph, By_name, []).

-file("src/intent/resolver.gleam", 23).
?DOC(" Resolve all behaviors in a spec into execution order\n").
-spec resolve_execution_order(intent@types:spec()) -> {ok,
        list(resolved_behavior())} |
    {error, resolve_error()}.
resolve_execution_order(Spec) ->
    All_behaviors = collect_all_behaviors(Spec),
    gleam@result:'try'(
        check_duplicates(All_behaviors),
        fun(_) ->
            gleam@result:'try'(
                build_dependency_graph(All_behaviors),
                fun(Graph) -> topological_sort(All_behaviors, Graph) end
            )
        end
    ).

-file("src/intent/resolver.gleam", 254).
-spec list_to_string(list(binary()), binary()) -> binary().
list_to_string(Items, Sep) ->
    case Items of
        [] ->
            <<""/utf8>>;

        [Item] ->
            Item;

        [Item@1 | Rest] ->
            <<<<Item@1/binary, Sep/binary>>/binary,
                (list_to_string(Rest, Sep))/binary>>
    end.

-file("src/intent/resolver.gleam", 239).
?DOC(" Format a resolve error as a human-readable string\n").
-spec format_error(resolve_error()) -> binary().
format_error(Error) ->
    case Error of
        {cyclic_dependency, Behaviors} ->
            <<"Cyclic dependency detected involving: "/utf8,
                (list_to_string(Behaviors, <<", "/utf8>>))/binary>>;

        {missing_dependency, Behavior, Missing} ->
            <<<<<<<<"Behavior '"/utf8, Behavior/binary>>/binary,
                        "' requires '"/utf8>>/binary,
                    Missing/binary>>/binary,
                "' which does not exist"/utf8>>;

        {duplicate_behavior_name, Name} ->
            <<"Duplicate behavior name: "/utf8, Name/binary>>
    end.
