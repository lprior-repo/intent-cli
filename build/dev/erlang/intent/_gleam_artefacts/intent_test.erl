-module(intent_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/intent_test.gleam").
-export([main/0, resolver_simple_no_deps_test/0, resolver_linear_dependency_chain_test/0, resolver_multiple_deps_on_one_test/0, resolver_missing_dependency_test/0, resolver_cyclic_dependency_test/0, resolver_duplicate_name_test/0, resolver_cross_feature_deps_test/0, interpolate_missing_variable_test/0, interpolate_no_variables_test/0, interpolate_simple_variable_test/0, interpolate_multiple_variables_test/0]).

-file("test/intent_test.gleam", 10).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("test/intent_test.gleam", 18).
-spec make_behavior(binary(), list(binary())) -> intent@types:behavior().
make_behavior(Name, Requires) ->
    {behavior,
        Name,
        <<"Test intent for "/utf8, Name/binary>>,
        <<""/utf8>>,
        Requires,
        [],
        {request,
            get,
            <<"/"/utf8, Name/binary>>,
            gleam@dict:new(),
            gleam@dict:new(),
            none},
        {response, 200, none, gleam@dict:new(), none},
        gleam@dict:new()}.

-file("test/intent_test.gleam", 42).
-spec make_feature(binary(), list(intent@types:behavior())) -> intent@types:feature().
make_feature(Name, Behaviors) ->
    {feature, Name, <<"Test feature"/utf8>>, Behaviors}.

-file("test/intent_test.gleam", 46).
-spec make_spec(list(intent@types:feature())) -> intent@types:spec().
make_spec(Features) ->
    {spec,
        <<"Test Spec"/utf8>>,
        <<"Test spec"/utf8>>,
        <<""/utf8>>,
        <<"1.0.0"/utf8>>,
        [],
        {config, <<"http://localhost"/utf8>>, 5000, gleam@dict:new()},
        Features,
        [],
        [],
        none}.

-file("test/intent_test.gleam", 65).
-spec resolver_simple_no_deps_test() -> nil.
resolver_simple_no_deps_test() ->
    B1 = make_behavior(<<"first"/utf8>>, []),
    B2 = make_behavior(<<"second"/utf8>>, []),
    B3 = make_behavior(<<"third"/utf8>>, []),
    Spec = make_spec([make_feature(<<"Feature A"/utf8>>, [B1, B2, B3])]),
    Result = intent@resolver:resolve_execution_order(Spec),
    case Result of
        {ok, Resolved} ->
            _pipe = erlang:length(Resolved),
            gleeunit_ffi:should_equal(_pipe, 3);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 84).
-spec resolver_linear_dependency_chain_test() -> nil.
resolver_linear_dependency_chain_test() ->
    B1 = make_behavior(<<"first"/utf8>>, []),
    B2 = make_behavior(<<"second"/utf8>>, [<<"first"/utf8>>]),
    B3 = make_behavior(<<"third"/utf8>>, [<<"second"/utf8>>]),
    Spec = make_spec([make_feature(<<"Feature A"/utf8>>, [B1, B2, B3])]),
    Result = intent@resolver:resolve_execution_order(Spec),
    case Result of
        {ok, Resolved} ->
            _pipe = erlang:length(Resolved),
            gleeunit_ffi:should_equal(_pipe, 3),
            Names = gleam@list:map(
                Resolved,
                fun(Rb) -> erlang:element(2, erlang:element(3, Rb)) end
            ),
            _pipe@1 = Names,
            gleeunit_ffi:should_equal(
                _pipe@1,
                [<<"first"/utf8>>, <<"second"/utf8>>, <<"third"/utf8>>]
            );

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 108).
-spec resolver_multiple_deps_on_one_test() -> nil.
resolver_multiple_deps_on_one_test() ->
    B1 = make_behavior(<<"base"/utf8>>, []),
    B2 = make_behavior(<<"child-a"/utf8>>, [<<"base"/utf8>>]),
    B3 = make_behavior(<<"child-b"/utf8>>, [<<"base"/utf8>>]),
    Spec = make_spec([make_feature(<<"Feature A"/utf8>>, [B1, B2, B3])]),
    Result = intent@resolver:resolve_execution_order(Spec),
    case Result of
        {ok, Resolved} ->
            _pipe = erlang:length(Resolved),
            gleeunit_ffi:should_equal(_pipe, 3),
            Names = gleam@list:map(
                Resolved,
                fun(Rb) -> erlang:element(2, erlang:element(3, Rb)) end
            ),
            First@1 = case Names of
                [First | _] -> First;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"intent_test"/utf8>>,
                                function => <<"resolver_multiple_deps_on_one_test"/utf8>>,
                                line => 125,
                                value => _assert_fail,
                                start => 3149,
                                'end' => 3179,
                                pattern_start => 3160,
                                pattern_end => 3171})
            end,
            _pipe@1 = First@1,
            gleeunit_ffi:should_equal(_pipe@1, <<"base"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 133).
-spec resolver_missing_dependency_test() -> nil.
resolver_missing_dependency_test() ->
    B1 = make_behavior(<<"first"/utf8>>, [<<"nonexistent"/utf8>>]),
    Spec = make_spec([make_feature(<<"Feature A"/utf8>>, [B1])]),
    Result = intent@resolver:resolve_execution_order(Spec),
    case Result of
        {ok, _} ->
            gleeunit@should:fail();

        {error, {missing_dependency, Behavior, Missing}} ->
            _pipe = Behavior,
            gleeunit_ffi:should_equal(_pipe, <<"first"/utf8>>),
            _pipe@1 = Missing,
            gleeunit_ffi:should_equal(_pipe@1, <<"nonexistent"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 153).
-spec resolver_cyclic_dependency_test() -> nil.
resolver_cyclic_dependency_test() ->
    B1 = make_behavior(<<"first"/utf8>>, [<<"second"/utf8>>]),
    B2 = make_behavior(<<"second"/utf8>>, [<<"first"/utf8>>]),
    Spec = make_spec([make_feature(<<"Feature A"/utf8>>, [B1, B2])]),
    Result = intent@resolver:resolve_execution_order(Spec),
    case Result of
        {ok, _} ->
            gleeunit@should:fail();

        {error, {cyclic_dependency, _}} ->
            gleeunit_ffi:should_be_ok({ok, nil});

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 169).
-spec resolver_duplicate_name_test() -> nil.
resolver_duplicate_name_test() ->
    B1 = make_behavior(<<"same-name"/utf8>>, []),
    B2 = make_behavior(<<"same-name"/utf8>>, []),
    Spec = make_spec([make_feature(<<"Feature A"/utf8>>, [B1, B2])]),
    Result = intent@resolver:resolve_execution_order(Spec),
    case Result of
        {ok, _} ->
            gleeunit@should:fail();

        {error, {duplicate_behavior_name, Name}} ->
            _pipe = Name,
            gleeunit_ffi:should_equal(_pipe, <<"same-name"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 188).
-spec resolver_cross_feature_deps_test() -> nil.
resolver_cross_feature_deps_test() ->
    B1 = make_behavior(<<"base"/utf8>>, []),
    B2 = make_behavior(<<"dependent"/utf8>>, [<<"base"/utf8>>]),
    Spec = make_spec(
        [make_feature(<<"Feature A"/utf8>>, [B1]),
            make_feature(<<"Feature B"/utf8>>, [B2])]
    ),
    Result = intent@resolver:resolve_execution_order(Spec),
    case Result of
        {ok, Resolved} ->
            _pipe = erlang:length(Resolved),
            gleeunit_ffi:should_equal(_pipe, 2),
            Names = gleam@list:map(
                Resolved,
                fun(Rb) -> erlang:element(2, erlang:element(3, Rb)) end
            ),
            _pipe@1 = Names,
            gleeunit_ffi:should_equal(
                _pipe@1,
                [<<"base"/utf8>>, <<"dependent"/utf8>>]
            );

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 252).
-spec interpolate_missing_variable_test() -> binary().
interpolate_missing_variable_test() ->
    Ctx = intent@interpolate:new_context(),
    Result = intent@interpolate:interpolate_string(
        Ctx,
        <<"/users/${unknown}"/utf8>>
    ),
    _pipe = Result,
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 261).
-spec interpolate_no_variables_test() -> nil.
interpolate_no_variables_test() ->
    Ctx = intent@interpolate:new_context(),
    Result = intent@interpolate:interpolate_string(
        Ctx,
        <<"/users/static"/utf8>>
    ),
    case Result of
        {ok, S} ->
            _pipe = S,
            gleeunit_ffi:should_equal(_pipe, <<"/users/static"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 277).
-spec json_string(binary()) -> gleam@json:json().
json_string(S) ->
    gleam@json:string(S).

-file("test/intent_test.gleam", 218).
-spec interpolate_simple_variable_test() -> nil.
interpolate_simple_variable_test() ->
    Ctx = begin
        _pipe = intent@interpolate:new_context(),
        intent@interpolate:set_variable(
            _pipe,
            <<"user_id"/utf8>>,
            json_string(<<"12345"/utf8>>)
        )
    end,
    Result = intent@interpolate:interpolate_string(
        Ctx,
        <<"/users/${user_id}"/utf8>>
    ),
    _pipe@1 = Result,
    gleeunit_ffi:should_be_ok(_pipe@1),
    case Result of
        {ok, S} ->
            _pipe@2 = S,
            gleeunit_ffi:should_equal(_pipe@2, <<"/users/12345"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 236).
-spec interpolate_multiple_variables_test() -> nil.
interpolate_multiple_variables_test() ->
    Ctx = begin
        _pipe = intent@interpolate:new_context(),
        _pipe@1 = intent@interpolate:set_variable(
            _pipe,
            <<"org"/utf8>>,
            json_string(<<"acme"/utf8>>)
        ),
        intent@interpolate:set_variable(
            _pipe@1,
            <<"team"/utf8>>,
            json_string(<<"dev"/utf8>>)
        )
    end,
    Result = intent@interpolate:interpolate_string(
        Ctx,
        <<"/orgs/${org}/teams/${team}"/utf8>>
    ),
    case Result of
        {ok, S} ->
            _pipe@2 = S,
            gleeunit_ffi:should_equal(_pipe@2, <<"/orgs/acme/teams/dev"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.
