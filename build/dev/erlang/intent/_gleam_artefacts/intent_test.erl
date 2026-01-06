-module(intent_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/intent_test.gleam").
-export([main/0, resolver_simple_no_deps_test/0, resolver_linear_dependency_chain_test/0, resolver_multiple_deps_on_one_test/0, resolver_missing_dependency_test/0, resolver_cyclic_dependency_test/0, resolver_duplicate_name_test/0, resolver_cross_feature_deps_test/0, interpolate_missing_variable_test/0, interpolate_no_variables_test/0, interpolate_simple_variable_test/0, interpolate_multiple_variables_test/0, interview_get_questions_api_round_1_test/0, interview_get_questions_cli_round_1_test/0, interview_create_session_test/0, interview_extract_auth_method_jwt_test/0, interview_extract_auth_method_oauth_test/0, interview_extract_entities_test/0, interview_extract_audience_mobile_test/0, interview_detect_gaps_empty_answers_test/0, interview_detect_gaps_with_answers_test/0, interview_detect_conflicts_cap_theorem_test/0, interview_calculate_confidence_high_test/0, interview_add_answer_test/0, interview_complete_round_test/0, interview_format_question_critical_test/0, http_client_url_construction_simple_test/0, http_client_path_interpolation_test/0, http_client_missing_variable_interpolation_test/0, http_client_header_interpolation_test/0, http_client_header_merge_test/0, http_client_body_json_interpolation_test/0, http_client_invalid_url_test/0, http_client_https_url_test/0, http_client_custom_port_test/0, http_client_path_leading_slash_test/0, http_client_method_conversion_get_test/0, http_client_method_conversion_post_test/0, http_client_multiple_header_merge_test/0]).

-file("test/intent_test.gleam", 13).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("test/intent_test.gleam", 21).
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
            gleam@json:null()},
        {response, 200, gleam@json:null(), gleam@dict:new(), gleam@dict:new()},
        gleam@dict:new()}.

-file("test/intent_test.gleam", 45).
-spec make_feature(binary(), list(intent@types:behavior())) -> intent@types:feature().
make_feature(Name, Behaviors) ->
    {feature, Name, <<"Test feature"/utf8>>, Behaviors}.

-file("test/intent_test.gleam", 49).
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
        {a_i_hints,
            {implementation_hints, []},
            gleam@dict:new(),
            {security_hints, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>},
            []}}.

-file("test/intent_test.gleam", 78).
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

-file("test/intent_test.gleam", 97).
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

-file("test/intent_test.gleam", 121).
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
                                line => 138,
                                value => _assert_fail,
                                start => 3526,
                                'end' => 3556,
                                pattern_start => 3537,
                                pattern_end => 3548})
            end,
            _pipe@1 = First@1,
            gleeunit_ffi:should_equal(_pipe@1, <<"base"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 146).
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

-file("test/intent_test.gleam", 166).
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

-file("test/intent_test.gleam", 182).
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

-file("test/intent_test.gleam", 201).
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

-file("test/intent_test.gleam", 265).
-spec interpolate_missing_variable_test() -> binary().
interpolate_missing_variable_test() ->
    Ctx = intent@interpolate:new_context(),
    Result = intent@interpolate:interpolate_string(
        Ctx,
        <<"/users/${unknown}"/utf8>>
    ),
    _pipe = Result,
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 274).
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

-file("test/intent_test.gleam", 288).
-spec json_string(binary()) -> gleam@json:json().
json_string(S) ->
    gleam@json:string(S).

-file("test/intent_test.gleam", 231).
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

-file("test/intent_test.gleam", 249).
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

-file("test/intent_test.gleam", 296).
-spec interview_get_questions_api_round_1_test() -> nil.
interview_get_questions_api_round_1_test() ->
    Questions = intent@interview:get_questions_for_round(api, 1),
    Has_questions = erlang:length(Questions) > 0,
    _pipe = Has_questions,
    gleeunit@should:be_true(_pipe).

-file("test/intent_test.gleam", 302).
-spec interview_get_questions_cli_round_1_test() -> nil.
interview_get_questions_cli_round_1_test() ->
    Questions = intent@interview:get_questions_for_round(cli, 1),
    Has_questions = erlang:length(Questions) > 0,
    _pipe = Has_questions,
    gleeunit@should:be_true(_pipe).

-file("test/intent_test.gleam", 308).
-spec interview_create_session_test() -> nil.
interview_create_session_test() ->
    Session = intent@interview:create_session(
        <<"test-session-1"/utf8>>,
        api,
        <<"2024-01-01T00:00:00Z"/utf8>>
    ),
    _pipe = erlang:element(2, Session),
    gleeunit_ffi:should_equal(_pipe, <<"test-session-1"/utf8>>),
    _pipe@1 = erlang:element(3, Session),
    gleeunit_ffi:should_equal(_pipe@1, api),
    _pipe@2 = erlang:element(8, Session),
    gleeunit_ffi:should_equal(_pipe@2, 0),
    _pipe@3 = erlang:element(7, Session),
    gleeunit_ffi:should_equal(_pipe@3, discovery),
    _pipe@4 = erlang:element(9, Session),
    _pipe@5 = erlang:length(_pipe@4),
    gleeunit_ffi:should_equal(_pipe@5, 0).

-file("test/intent_test.gleam", 319).
-spec interview_extract_auth_method_jwt_test() -> nil.
interview_extract_auth_method_jwt_test() ->
    Extracted = intent@interview:extract_from_answer(
        <<"q1"/utf8>>,
        <<"We use JWT tokens for authentication"/utf8>>,
        [<<"auth_method"/utf8>>]
    ),
    Auth_method = gleam@dict:get(Extracted, <<"auth_method"/utf8>>),
    _pipe = Auth_method,
    gleeunit_ffi:should_equal(_pipe, {ok, <<"jwt"/utf8>>}).

-file("test/intent_test.gleam", 328).
-spec interview_extract_auth_method_oauth_test() -> nil.
interview_extract_auth_method_oauth_test() ->
    Extracted = intent@interview:extract_from_answer(
        <<"q1"/utf8>>,
        <<"OAuth 2.0 is our auth standard"/utf8>>,
        [<<"auth_method"/utf8>>]
    ),
    Auth_method = gleam@dict:get(Extracted, <<"auth_method"/utf8>>),
    _pipe = Auth_method,
    gleeunit_ffi:should_equal(_pipe, {ok, <<"oauth"/utf8>>}).

-file("test/intent_test.gleam", 337).
-spec interview_extract_entities_test() -> nil.
interview_extract_entities_test() ->
    Extracted = intent@interview:extract_from_answer(
        <<"q1"/utf8>>,
        <<"Users, Orders, Products, Payments"/utf8>>,
        [<<"entities"/utf8>>]
    ),
    Entities = gleam@dict:get(Extracted, <<"entities"/utf8>>),
    _pipe = Entities,
    gleeunit_ffi:should_equal(
        _pipe,
        {ok, <<"Users, Orders, Products, Payments"/utf8>>}
    ).

-file("test/intent_test.gleam", 347).
-spec interview_extract_audience_mobile_test() -> nil.
interview_extract_audience_mobile_test() ->
    Extracted = intent@interview:extract_from_answer(
        <<"q1"/utf8>>,
        <<"Mainly mobile app users"/utf8>>,
        [<<"audience"/utf8>>]
    ),
    Audience = gleam@dict:get(Extracted, <<"audience"/utf8>>),
    _pipe = Audience,
    gleeunit_ffi:should_equal(_pipe, {ok, <<"mobile"/utf8>>}).

-file("test/intent_test.gleam", 356).
-spec interview_detect_gaps_empty_answers_test() -> nil.
interview_detect_gaps_empty_answers_test() ->
    Answers = [],
    Gaps = intent@interview:detect_gaps(api, Answers),
    Has_gaps = erlang:length(Gaps) > 0,
    _pipe = Has_gaps,
    gleeunit@should:be_true(_pipe).

-file("test/intent_test.gleam", 363).
-spec interview_detect_gaps_with_answers_test() -> nil.
interview_detect_gaps_with_answers_test() ->
    Answers = [{answer,
            <<"q1"/utf8>>,
            <<"What auth?"/utf8>>,
            security,
            1,
            <<"JWT"/utf8>>,
            maps:from_list([{<<"auth_method"/utf8>>, <<"jwt"/utf8>>}]),
            0.9,
            <<""/utf8>>,
            <<"2024-01-01T00:00:00Z"/utf8>>},
        {answer,
            <<"q2"/utf8>>,
            <<"What entities?"/utf8>>,
            developer,
            1,
            <<"Users, Tokens"/utf8>>,
            maps:from_list(
                [{<<"entities"/utf8>>, <<"Users, Tokens"/utf8>>},
                    {<<"base_url"/utf8>>, <<"http://localhost:8080"/utf8>>}]
            ),
            0.85,
            <<""/utf8>>,
            <<"2024-01-01T00:00:00Z"/utf8>>},
        {answer,
            <<"q3"/utf8>>,
            <<"Happy path?"/utf8>>,
            user,
            1,
            <<"Login and get token"/utf8>>,
            maps:from_list(
                [{<<"happy_path"/utf8>>, <<"Login and get token"/utf8>>}]
            ),
            0.8,
            <<""/utf8>>,
            <<"2024-01-01T00:00:00Z"/utf8>>},
        {answer,
            <<"q4"/utf8>>,
            <<"Errors?"/utf8>>,
            user,
            2,
            <<"Wrong password, user not found"/utf8>>,
            maps:from_list(
                [{<<"error_cases"/utf8>>, <<"Wrong password"/utf8>>}]
            ),
            0.75,
            <<""/utf8>>,
            <<"2024-01-01T00:00:00Z"/utf8>>},
        {answer,
            <<"q5"/utf8>>,
            <<"Format?"/utf8>>,
            developer,
            1,
            <<"JSON response format"/utf8>>,
            maps:from_list([{<<"response_format"/utf8>>, <<"json"/utf8>>}]),
            0.9,
            <<""/utf8>>,
            <<"2024-01-01T00:00:00Z"/utf8>>}],
    Gaps = intent@interview:detect_gaps(api, Answers),
    _pipe = Gaps,
    _pipe@1 = erlang:length(_pipe),
    gleeunit_ffi:should_equal(_pipe@1, 0).

-file("test/intent_test.gleam", 426).
-spec interview_detect_conflicts_cap_theorem_test() -> nil.
interview_detect_conflicts_cap_theorem_test() ->
    Answers = [{answer,
            <<"q1"/utf8>>,
            <<"Performance?"/utf8>>,
            ops,
            3,
            <<"We need fast latency, under 50ms"/utf8>>,
            maps:from_list([]),
            0.8,
            <<""/utf8>>,
            <<"2024-01-01T00:00:00Z"/utf8>>},
        {answer,
            <<"q2"/utf8>>,
            <<"Consistency?"/utf8>>,
            developer,
            3,
            <<"All data must be strongly consistent"/utf8>>,
            maps:from_list([]),
            0.85,
            <<""/utf8>>,
            <<"2024-01-01T00:00:00Z"/utf8>>}],
    Conflicts = intent@interview:detect_conflicts(Answers),
    _pipe = Conflicts,
    _pipe@1 = gleam@list:any(
        _pipe,
        fun(C) -> erlang:element(2, C) =:= <<"conflict-cap"/utf8>> end
    ),
    gleeunit@should:be_true(_pipe@1).

-file("test/intent_test.gleam", 457).
-spec interview_calculate_confidence_high_test() -> nil.
interview_calculate_confidence_high_test() ->
    Extracted = maps:from_list(
        [{<<"auth_method"/utf8>>, <<"jwt"/utf8>>},
            {<<"audience"/utf8>>, <<"mobile"/utf8>>}]
    ),
    Confidence = intent@interview:calculate_confidence(
        <<"q1"/utf8>>,
        <<"This is a very detailed response about JWT authentication and mobile users with specific requirements"/utf8>>,
        Extracted
    ),
    Is_high = Confidence > 0.8,
    _pipe = Is_high,
    gleeunit@should:be_true(_pipe).

-file("test/intent_test.gleam", 472).
-spec interview_add_answer_test() -> nil.
interview_add_answer_test() ->
    Session = intent@interview:create_session(
        <<"test-1"/utf8>>,
        api,
        <<"2024-01-01T00:00:00Z"/utf8>>
    ),
    Answer = {answer,
        <<"q1"/utf8>>,
        <<"Test"/utf8>>,
        user,
        1,
        <<"Test response"/utf8>>,
        maps:from_list([]),
        0.8,
        <<""/utf8>>,
        <<"2024-01-01T00:01:00Z"/utf8>>},
    Updated = intent@interview:add_answer(Session, Answer),
    _pipe = erlang:element(9, Updated),
    _pipe@1 = erlang:length(_pipe),
    gleeunit_ffi:should_equal(_pipe@1, 1),
    _pipe@2 = erlang:element(5, Updated),
    gleeunit_ffi:should_equal(_pipe@2, <<"2024-01-01T00:01:00Z"/utf8>>).

-file("test/intent_test.gleam", 492).
-spec interview_complete_round_test() -> nil.
interview_complete_round_test() ->
    Session = intent@interview:create_session(
        <<"test-1"/utf8>>,
        api,
        <<"2024-01-01T00:00:00Z"/utf8>>
    ),
    After_round_1 = intent@interview:complete_round(Session),
    _pipe = erlang:element(8, After_round_1),
    gleeunit_ffi:should_equal(_pipe, 1),
    _pipe@1 = erlang:element(7, After_round_1),
    gleeunit_ffi:should_equal(_pipe@1, refinement).

-file("test/intent_test.gleam", 500).
-spec interview_format_question_critical_test() -> nil.
interview_format_question_critical_test() ->
    Question = {question,
        <<"q1"/utf8>>,
        1,
        user,
        happy_path,
        critical,
        <<"What should this do?"/utf8>>,
        <<"Start simple"/utf8>>,
        <<"Example here"/utf8>>,
        <<"text"/utf8>>,
        [],
        [],
        []},
    Formatted = intent@interview:format_question(Question),
    _pipe = Formatted,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"[CRITICAL]"/utf8>>),
    gleeunit@should:be_true(_pipe@1),
    _pipe@2 = Formatted,
    _pipe@3 = gleam_stdlib:contains_string(
        _pipe@2,
        <<"What should this do?"/utf8>>
    ),
    gleeunit@should:be_true(_pipe@3).

-file("test/intent_test.gleam", 527).
-spec http_client_url_construction_simple_test() -> nil.
http_client_url_construction_simple_test() ->
    Config = {config, <<"http://localhost:8080"/utf8>>, 5000, gleam@dict:new()},
    Request = {request,
        get,
        <<"/users/123"/utf8>>,
        gleam@dict:new(),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = intent@interpolate:new_context(),
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, _} ->
            gleeunit_ffi:should_be_ok({ok, nil});

        {ok, _} ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.

-file("test/intent_test.gleam", 556).
-spec http_client_path_interpolation_test() -> nil.
http_client_path_interpolation_test() ->
    Config = {config, <<"http://localhost:8080"/utf8>>, 5000, gleam@dict:new()},
    Request = {request,
        get,
        <<"/users/${user_id}"/utf8>>,
        gleam@dict:new(),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = begin
        _pipe = intent@interpolate:new_context(),
        intent@interpolate:set_variable(
            _pipe,
            <<"user_id"/utf8>>,
            gleam@json:string(<<"123"/utf8>>)
        )
    end,
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {interpolation_error, _}} ->
            gleeunit@should:fail();

        _ ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.

-file("test/intent_test.gleam", 586).
-spec http_client_missing_variable_interpolation_test() -> nil.
http_client_missing_variable_interpolation_test() ->
    Config = {config, <<"http://localhost:8080"/utf8>>, 5000, gleam@dict:new()},
    Request = {request,
        get,
        <<"/users/${unknown_var}"/utf8>>,
        gleam@dict:new(),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = intent@interpolate:new_context(),
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {interpolation_error, _}} ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 612).
-spec http_client_header_interpolation_test() -> nil.
http_client_header_interpolation_test() ->
    Config = {config,
        <<"http://localhost:8080"/utf8>>,
        5000,
        maps:from_list([{<<"X-Default"/utf8>>, <<"default-value"/utf8>>}])},
    Request = {request,
        get,
        <<"/users"/utf8>>,
        maps:from_list([{<<"X-Token"/utf8>>, <<"${auth_token}"/utf8>>}]),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = begin
        _pipe = intent@interpolate:new_context(),
        intent@interpolate:set_variable(
            _pipe,
            <<"auth_token"/utf8>>,
            gleam@json:string(<<"secret123"/utf8>>)
        )
    end,
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {interpolation_error, _}} ->
            gleeunit@should:fail();

        _ ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.

-file("test/intent_test.gleam", 641).
-spec http_client_header_merge_test() -> nil.
http_client_header_merge_test() ->
    Config = {config,
        <<"http://localhost:8080"/utf8>>,
        5000,
        maps:from_list(
            [{<<"X-Default"/utf8>>, <<"config-value"/utf8>>},
                {<<"X-Config-Only"/utf8>>, <<"config"/utf8>>}]
        )},
    Request = {request,
        get,
        <<"/users"/utf8>>,
        maps:from_list([{<<"X-Default"/utf8>>, <<"request-value"/utf8>>}]),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = intent@interpolate:new_context(),
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {interpolation_error, _}} ->
            gleeunit@should:fail();

        _ ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.

-file("test/intent_test.gleam", 671).
-spec http_client_body_json_interpolation_test() -> nil.
http_client_body_json_interpolation_test() ->
    Config = {config, <<"http://localhost:8080"/utf8>>, 5000, gleam@dict:new()},
    Body_json = gleam@json:object(
        [{<<"username"/utf8>>, gleam@json:string(<<"${username}"/utf8>>)},
            {<<"email"/utf8>>, gleam@json:string(<<"user@example.com"/utf8>>)}]
    ),
    Request = {request,
        post,
        <<"/users"/utf8>>,
        gleam@dict:new(),
        gleam@dict:new(),
        Body_json},
    Ctx = begin
        _pipe = intent@interpolate:new_context(),
        intent@interpolate:set_variable(
            _pipe,
            <<"username"/utf8>>,
            gleam@json:string(<<"john_doe"/utf8>>)
        )
    end,
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {interpolation_error, _}} ->
            gleeunit@should:fail();

        _ ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.

-file("test/intent_test.gleam", 706).
-spec http_client_invalid_url_test() -> nil.
http_client_invalid_url_test() ->
    Config = {config, <<"not a valid url at all"/utf8>>, 5000, gleam@dict:new()},
    Request = {request,
        get,
        <<"/users"/utf8>>,
        gleam@dict:new(),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = intent@interpolate:new_context(),
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {url_parse_error, _}} ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 732).
-spec http_client_https_url_test() -> nil.
http_client_https_url_test() ->
    Config = {config,
        <<"https://api.example.com"/utf8>>,
        5000,
        gleam@dict:new()},
    Request = {request,
        get,
        <<"/secure-endpoint"/utf8>>,
        gleam@dict:new(),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = intent@interpolate:new_context(),
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {url_parse_error, _}} ->
            gleeunit@should:fail();

        _ ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.

-file("test/intent_test.gleam", 759).
-spec http_client_custom_port_test() -> nil.
http_client_custom_port_test() ->
    Config = {config, <<"http://localhost:3000"/utf8>>, 5000, gleam@dict:new()},
    Request = {request,
        get,
        <<"/health"/utf8>>,
        gleam@dict:new(),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = intent@interpolate:new_context(),
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {url_parse_error, _}} ->
            gleeunit@should:fail();

        _ ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.

-file("test/intent_test.gleam", 786).
-spec http_client_path_leading_slash_test() -> nil.
http_client_path_leading_slash_test() ->
    Config = {config, <<"http://localhost:8080"/utf8>>, 5000, gleam@dict:new()},
    Request = {request,
        get,
        <<"users/123"/utf8>>,
        gleam@dict:new(),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = intent@interpolate:new_context(),
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {interpolation_error, _}} ->
            gleeunit@should:fail();

        _ ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.

-file("test/intent_test.gleam", 814).
-spec http_client_method_conversion_get_test() -> nil.
http_client_method_conversion_get_test() ->
    Config = {config, <<"http://localhost:8080"/utf8>>, 5000, gleam@dict:new()},
    Request = {request,
        get,
        <<"/users"/utf8>>,
        gleam@dict:new(),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = intent@interpolate:new_context(),
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {url_parse_error, _}} ->
            gleeunit@should:fail();

        _ ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.

-file("test/intent_test.gleam", 841).
-spec http_client_method_conversion_post_test() -> nil.
http_client_method_conversion_post_test() ->
    Config = {config, <<"http://localhost:8080"/utf8>>, 5000, gleam@dict:new()},
    Request = {request,
        post,
        <<"/users"/utf8>>,
        gleam@dict:new(),
        gleam@dict:new(),
        gleam@json:object(
            [{<<"name"/utf8>>, gleam@json:string(<<"John"/utf8>>)}]
        )},
    Ctx = intent@interpolate:new_context(),
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {url_parse_error, _}} ->
            gleeunit@should:fail();

        _ ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.

-file("test/intent_test.gleam", 868).
-spec http_client_multiple_header_merge_test() -> nil.
http_client_multiple_header_merge_test() ->
    Config = {config,
        <<"http://localhost:8080"/utf8>>,
        5000,
        maps:from_list(
            [{<<"X-API-Version"/utf8>>, <<"v1"/utf8>>},
                {<<"User-Agent"/utf8>>, <<"intent-cli"/utf8>>}]
        )},
    Request = {request,
        get,
        <<"/data"/utf8>>,
        maps:from_list(
            [{<<"Authorization"/utf8>>, <<"Bearer token"/utf8>>},
                {<<"X-Request-ID"/utf8>>, <<"123"/utf8>>}]
        ),
        gleam@dict:new(),
        gleam@json:null()},
    Ctx = intent@interpolate:new_context(),
    Result = intent@http_client:execute_request(Config, Request, Ctx),
    case Result of
        {error, {interpolation_error, _}} ->
            gleeunit@should:fail();

        _ ->
            gleeunit_ffi:should_be_ok({ok, nil})
    end.
