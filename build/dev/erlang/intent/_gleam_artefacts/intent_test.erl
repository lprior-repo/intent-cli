-module(intent_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/intent_test.gleam").
-export([main/0, resolver_simple_no_deps_test/0, resolver_linear_dependency_chain_test/0, resolver_multiple_deps_on_one_test/0, resolver_missing_dependency_test/0, resolver_cyclic_dependency_test/0, resolver_duplicate_name_test/0, resolver_cross_feature_deps_test/0, interpolate_missing_variable_test/0, interpolate_no_variables_test/0, interpolate_simple_variable_test/0, interpolate_multiple_variables_test/0, interview_get_questions_api_round_1_test/0, interview_get_questions_cli_round_1_test/0, interview_create_session_test/0, interview_extract_auth_method_jwt_test/0, interview_extract_auth_method_oauth_test/0, interview_extract_entities_test/0, interview_extract_audience_mobile_test/0, interview_detect_gaps_empty_answers_test/0, interview_detect_gaps_with_answers_test/0, interview_detect_conflicts_cap_theorem_test/0, interview_calculate_confidence_high_test/0, interview_add_answer_test/0, interview_complete_round_test/0, interview_format_question_critical_test/0, http_client_url_construction_simple_test/0, http_client_path_interpolation_test/0, http_client_missing_variable_interpolation_test/0, http_client_header_interpolation_test/0, http_client_header_merge_test/0, http_client_body_json_interpolation_test/0, http_client_invalid_url_test/0, http_client_https_url_test/0, http_client_custom_port_test/0, http_client_path_leading_slash_test/0, http_client_method_conversion_get_test/0, http_client_method_conversion_post_test/0, http_client_multiple_header_merge_test/0, rules_engine_check_when_status_equals_test/0, rules_engine_check_when_status_greater_than_test/0, rules_engine_check_when_status_less_than_test/0, rules_engine_check_when_method_mismatch_test/0, rules_engine_check_when_path_exact_match_test/0, rules_engine_check_when_path_regex_match_test/0, rules_engine_check_body_must_contain_test/0, rules_engine_check_body_must_not_contain_test/0, rules_engine_check_body_must_not_contain_violation_test/0, rules_engine_check_body_must_contain_violation_test/0, rules_engine_check_multiple_rules_test/0, rules_engine_format_violation_body_contains_test/0, rules_engine_format_violation_body_missing_test/0, rules_engine_format_violation_field_missing_test/0, rules_engine_format_violation_header_missing_test/0, resolver_complex_diamond_dependency_test/0, resolver_multiple_branches_test/0, resolver_deep_chain_test/0, rules_engine_empty_body_test/0, rules_engine_null_json_value_test/0, rules_engine_whitespace_body_test/0, rules_engine_nested_null_field_test/0, rules_engine_empty_object_test/0]).

-file("test/intent_test.gleam", 15).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("test/intent_test.gleam", 23).
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

-file("test/intent_test.gleam", 47).
-spec make_feature(binary(), list(intent@types:behavior())) -> intent@types:feature().
make_feature(Name, Behaviors) ->
    {feature, Name, <<"Test feature"/utf8>>, Behaviors}.

-file("test/intent_test.gleam", 51).
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

-file("test/intent_test.gleam", 80).
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

-file("test/intent_test.gleam", 99).
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

-file("test/intent_test.gleam", 123).
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
                                line => 140,
                                value => _assert_fail,
                                start => 3579,
                                'end' => 3609,
                                pattern_start => 3590,
                                pattern_end => 3601})
            end,
            _pipe@1 = First@1,
            gleeunit_ffi:should_equal(_pipe@1, <<"base"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 148).
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

-file("test/intent_test.gleam", 168).
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

-file("test/intent_test.gleam", 184).
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

-file("test/intent_test.gleam", 203).
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

-file("test/intent_test.gleam", 267).
-spec interpolate_missing_variable_test() -> binary().
interpolate_missing_variable_test() ->
    Ctx = intent@interpolate:new_context(),
    Result = intent@interpolate:interpolate_string(
        Ctx,
        <<"/users/${unknown}"/utf8>>
    ),
    _pipe = Result,
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 276).
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

-file("test/intent_test.gleam", 290).
-spec json_string(binary()) -> gleam@json:json().
json_string(S) ->
    gleam@json:string(S).

-file("test/intent_test.gleam", 233).
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

-file("test/intent_test.gleam", 251).
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

-file("test/intent_test.gleam", 298).
-spec interview_get_questions_api_round_1_test() -> nil.
interview_get_questions_api_round_1_test() ->
    Questions = intent@interview:get_questions_for_round(api, 1),
    Has_questions = erlang:length(Questions) > 0,
    _pipe = Has_questions,
    gleeunit@should:be_true(_pipe).

-file("test/intent_test.gleam", 304).
-spec interview_get_questions_cli_round_1_test() -> nil.
interview_get_questions_cli_round_1_test() ->
    Questions = intent@interview:get_questions_for_round(cli, 1),
    Has_questions = erlang:length(Questions) > 0,
    _pipe = Has_questions,
    gleeunit@should:be_true(_pipe).

-file("test/intent_test.gleam", 310).
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

-file("test/intent_test.gleam", 321).
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

-file("test/intent_test.gleam", 330).
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

-file("test/intent_test.gleam", 339).
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

-file("test/intent_test.gleam", 349).
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

-file("test/intent_test.gleam", 358).
-spec interview_detect_gaps_empty_answers_test() -> nil.
interview_detect_gaps_empty_answers_test() ->
    Answers = [],
    Gaps = intent@interview:detect_gaps(api, Answers),
    Has_gaps = erlang:length(Gaps) > 0,
    _pipe = Has_gaps,
    gleeunit@should:be_true(_pipe).

-file("test/intent_test.gleam", 365).
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

-file("test/intent_test.gleam", 428).
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

-file("test/intent_test.gleam", 459).
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

-file("test/intent_test.gleam", 474).
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

-file("test/intent_test.gleam", 494).
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

-file("test/intent_test.gleam", 502).
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

-file("test/intent_test.gleam", 905).
-spec make_execution_result(
    integer(),
    binary(),
    intent@types:method(),
    binary()
) -> intent@http_client:execution_result().
make_execution_result(Status, Body_str, Method, Path) ->
    {execution_result,
        Status,
        gleam@dict:new(),
        gleam@json:object([{<<"test"/utf8>>, gleam@json:string(Body_str)}]),
        Body_str,
        100,
        Method,
        Path}.

-file("test/intent_test.gleam", 922).
-spec rules_engine_check_when_status_equals_test() -> nil.
rules_engine_check_when_status_equals_test() ->
    Rule = {rule,
        <<"Check 200 OK"/utf8>>,
        <<"Verify 200 response"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/users"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(200, <<"ok"/utf8>>, get, <<"/users"/utf8>>),
    Results = intent@rules_engine:check_rules(
        [Rule],
        Response,
        <<"test_behavior"/utf8>>
    ),
    _pipe = erlang:length(Results),
    gleeunit_ffi:should_equal(_pipe, 1),
    case Results of
        [{rule_passed, Name}] ->
            _pipe@1 = Name,
            gleeunit_ffi:should_equal(_pipe@1, <<"Check 200 OK"/utf8>>);

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 951).
-spec rules_engine_check_when_status_greater_than_test() -> nil.
rules_engine_check_when_status_greater_than_test() ->
    Rule = {rule,
        <<"Check 4xx error"/utf8>>,
        <<"Verify error status"/utf8>>,
        {'when', <<"> 399"/utf8>>, post, <<"/create"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(
        400,
        <<"bad request"/utf8>>,
        post,
        <<"/create"/utf8>>
    ),
    Results = intent@rules_engine:check_rules(
        [Rule],
        Response,
        <<"test_behavior"/utf8>>
    ),
    _pipe = erlang:length(Results),
    gleeunit_ffi:should_equal(_pipe, 1),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 980).
-spec rules_engine_check_when_status_less_than_test() -> nil.
rules_engine_check_when_status_less_than_test() ->
    Rule = {rule,
        <<"Check success range"/utf8>>,
        <<"Verify 2xx status"/utf8>>,
        {'when', <<"< 300"/utf8>>, get, <<"/data"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(
        201,
        <<"created"/utf8>>,
        get,
        <<"/data"/utf8>>
    ),
    Results = intent@rules_engine:check_rules(
        [Rule],
        Response,
        <<"test_behavior"/utf8>>
    ),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1006).
-spec rules_engine_check_when_method_mismatch_test() -> nil.
rules_engine_check_when_method_mismatch_test() ->
    Rule = {rule,
        <<"POST rule"/utf8>>,
        <<"Only for POST"/utf8>>,
        {'when', <<"== 200"/utf8>>, post, <<"/create"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(
        200,
        <<"ok"/utf8>>,
        get,
        <<"/create"/utf8>>
    ),
    Results = intent@rules_engine:check_rules(
        [Rule],
        Response,
        <<"test_behavior"/utf8>>
    ),
    _pipe = erlang:length(Results),
    gleeunit_ffi:should_equal(_pipe, 0).

-file("test/intent_test.gleam", 1031).
-spec rules_engine_check_when_path_exact_match_test() -> nil.
rules_engine_check_when_path_exact_match_test() ->
    Rule = {rule,
        <<"Exact path rule"/utf8>>,
        <<"Check exact path"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/exact/path"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(
        200,
        <<"ok"/utf8>>,
        get,
        <<"/exact/path"/utf8>>
    ),
    Results = intent@rules_engine:check_rules(
        [Rule],
        Response,
        <<"test_behavior"/utf8>>
    ),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1057).
-spec rules_engine_check_when_path_regex_match_test() -> nil.
rules_engine_check_when_path_regex_match_test() ->
    Rule = {rule,
        <<"Regex path rule"/utf8>>,
        <<"Check regex path"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"^/users/.*"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(
        200,
        <<"ok"/utf8>>,
        get,
        <<"/users/123"/utf8>>
    ),
    Results = intent@rules_engine:check_rules(
        [Rule],
        Response,
        <<"test_behavior"/utf8>>
    ),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1083).
-spec rules_engine_check_body_must_contain_test() -> nil.
rules_engine_check_body_must_contain_test() ->
    Rule = {rule,
        <<"Body content rule"/utf8>>,
        <<"Verify body contains text"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/test"/utf8>>},
        {rule_check, [], [<<"success"/utf8>>], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(
        200,
        <<"Operation was a success"/utf8>>,
        get,
        <<"/test"/utf8>>
    ),
    Results = intent@rules_engine:check_rules(
        [Rule],
        Response,
        <<"test_behavior"/utf8>>
    ),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1109).
-spec rules_engine_check_body_must_not_contain_test() -> nil.
rules_engine_check_body_must_not_contain_test() ->
    Rule = {rule,
        <<"No error rule"/utf8>>,
        <<"Verify no error in body"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/test"/utf8>>},
        {rule_check, [<<"error"/utf8>>], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(
        200,
        <<"This is clean data"/utf8>>,
        get,
        <<"/test"/utf8>>
    ),
    Results = intent@rules_engine:check_rules(
        [Rule],
        Response,
        <<"test_behavior"/utf8>>
    ),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1135).
-spec rules_engine_check_body_must_not_contain_violation_test() -> nil.
rules_engine_check_body_must_not_contain_violation_test() ->
    Rule = {rule,
        <<"No error rule"/utf8>>,
        <<"Verify no error in body"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/test"/utf8>>},
        {rule_check, [<<"error"/utf8>>], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(
        200,
        <<"This has an error in it"/utf8>>,
        get,
        <<"/test"/utf8>>
    ),
    Results = intent@rules_engine:check_rules(
        [Rule],
        Response,
        <<"test_behavior"/utf8>>
    ),
    case Results of
        [{rule_failed, Name, _, Violations}] ->
            _pipe = Name,
            gleeunit_ffi:should_equal(_pipe, <<"No error rule"/utf8>>),
            _pipe@1 = erlang:length(Violations),
            gleeunit_ffi:should_equal(_pipe@1, 1);

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1165).
-spec rules_engine_check_body_must_contain_violation_test() -> nil.
rules_engine_check_body_must_contain_violation_test() ->
    Rule = {rule,
        <<"Required text rule"/utf8>>,
        <<"Verify required text"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/test"/utf8>>},
        {rule_check,
            [],
            [<<"required"/utf8>>],
            [],
            [],
            <<""/utf8>>,
            <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(
        200,
        <<"This is missing it"/utf8>>,
        get,
        <<"/test"/utf8>>
    ),
    Results = intent@rules_engine:check_rules(
        [Rule],
        Response,
        <<"test_behavior"/utf8>>
    ),
    case Results of
        [{rule_failed, _, _, Violations}] ->
            _pipe = erlang:length(Violations),
            gleeunit_ffi:should_equal(_pipe, 1);

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1193).
-spec rules_engine_check_multiple_rules_test() -> nil.
rules_engine_check_multiple_rules_test() ->
    Rule1 = {rule,
        <<"Rule 1"/utf8>>,
        <<"First rule"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/test"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Rule2 = {rule,
        <<"Rule 2"/utf8>>,
        <<"Second rule"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/test"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = make_execution_result(200, <<"ok"/utf8>>, get, <<"/test"/utf8>>),
    Results = intent@rules_engine:check_rules(
        [Rule1, Rule2],
        Response,
        <<"test_behavior"/utf8>>
    ),
    _pipe = erlang:length(Results),
    gleeunit_ffi:should_equal(_pipe, 2).

-file("test/intent_test.gleam", 1231).
-spec rules_engine_format_violation_body_contains_test() -> nil.
rules_engine_format_violation_body_contains_test() ->
    Violation = {body_contains, <<"forbidden"/utf8>>, <<"response body"/utf8>>},
    Formatted = intent@rules_engine:format_violation(Violation),
    _pipe = Formatted,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"forbidden"/utf8>>),
    gleeunit@should:be_true(_pipe@1).

-file("test/intent_test.gleam", 1239).
-spec rules_engine_format_violation_body_missing_test() -> nil.
rules_engine_format_violation_body_missing_test() ->
    Violation = {body_missing, <<"required"/utf8>>},
    Formatted = intent@rules_engine:format_violation(Violation),
    _pipe = Formatted,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"required"/utf8>>),
    gleeunit@should:be_true(_pipe@1).

-file("test/intent_test.gleam", 1247).
-spec rules_engine_format_violation_field_missing_test() -> nil.
rules_engine_format_violation_field_missing_test() ->
    Violation = {field_missing, <<"user.id"/utf8>>},
    Formatted = intent@rules_engine:format_violation(Violation),
    _pipe = Formatted,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"user.id"/utf8>>),
    gleeunit@should:be_true(_pipe@1).

-file("test/intent_test.gleam", 1255).
-spec rules_engine_format_violation_header_missing_test() -> nil.
rules_engine_format_violation_header_missing_test() ->
    Violation = {header_missing, <<"X-Custom"/utf8>>},
    Formatted = intent@rules_engine:format_violation(Violation),
    _pipe = Formatted,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"X-Custom"/utf8>>),
    gleeunit@should:be_true(_pipe@1).

-file("test/intent_test.gleam", 1267).
-spec resolver_complex_diamond_dependency_test() -> nil.
resolver_complex_diamond_dependency_test() ->
    B1 = make_behavior(<<"base"/utf8>>, []),
    B3 = make_behavior(<<"left"/utf8>>, [<<"base"/utf8>>]),
    B4 = make_behavior(<<"right"/utf8>>, [<<"base"/utf8>>]),
    B5 = make_behavior(<<"merge"/utf8>>, [<<"left"/utf8>>, <<"right"/utf8>>]),
    Spec = make_spec([make_feature(<<"Feature A"/utf8>>, [B1, B3, B4, B5])]),
    Result = intent@resolver:resolve_execution_order(Spec),
    case Result of
        {ok, Resolved} ->
            _pipe = erlang:length(Resolved),
            gleeunit_ffi:should_equal(_pipe, 4),
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
                                function => <<"resolver_complex_diamond_dependency_test"/utf8>>,
                                line => 1282,
                                value => _assert_fail,
                                start => 35152,
                                'end' => 35182,
                                pattern_start => 35163,
                                pattern_end => 35174})
            end,
            _pipe@1 = First@1,
            gleeunit_ffi:should_equal(_pipe@1, <<"base"/utf8>>),
            case gleam@list:last(Names) of
                {ok, Last} ->
                    _pipe@2 = Last,
                    gleeunit_ffi:should_equal(_pipe@2, <<"merge"/utf8>>);

                {error, _} ->
                    gleeunit@should:fail()
            end;

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1294).
-spec resolver_multiple_branches_test() -> nil.
resolver_multiple_branches_test() ->
    B1 = make_behavior(<<"root"/utf8>>, []),
    B2 = make_behavior(<<"branch-a-1"/utf8>>, [<<"root"/utf8>>]),
    B3 = make_behavior(<<"branch-a-2"/utf8>>, [<<"branch-a-1"/utf8>>]),
    B4 = make_behavior(<<"branch-b-1"/utf8>>, [<<"root"/utf8>>]),
    B5 = make_behavior(<<"branch-b-2"/utf8>>, [<<"branch-b-1"/utf8>>]),
    Spec = make_spec([make_feature(<<"Feature"/utf8>>, [B1, B2, B3, B4, B5])]),
    Result = intent@resolver:resolve_execution_order(Spec),
    case Result of
        {ok, Resolved} ->
            _pipe = erlang:length(Resolved),
            gleeunit_ffi:should_equal(_pipe, 5),
            Names = gleam@list:map(
                Resolved,
                fun(Rb) -> erlang:element(2, erlang:element(3, Rb)) end
            ),
            _pipe@1 = gleam@list:any(Names, fun(N) -> N =:= <<"root"/utf8>> end),
            gleeunit@should:be_true(_pipe@1),
            _pipe@2 = gleam@list:any(
                Names,
                fun(N@1) -> N@1 =:= <<"branch-a-1"/utf8>> end
            ),
            gleeunit@should:be_true(_pipe@2),
            _pipe@3 = gleam@list:any(
                Names,
                fun(N@2) -> N@2 =:= <<"branch-a-2"/utf8>> end
            ),
            gleeunit@should:be_true(_pipe@3),
            _pipe@4 = gleam@list:any(
                Names,
                fun(N@3) -> N@3 =:= <<"branch-b-1"/utf8>> end
            ),
            gleeunit@should:be_true(_pipe@4),
            _pipe@5 = gleam@list:any(
                Names,
                fun(N@4) -> N@4 =:= <<"branch-b-2"/utf8>> end
            ),
            gleeunit@should:be_true(_pipe@5);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1320).
-spec resolver_deep_chain_test() -> nil.
resolver_deep_chain_test() ->
    B1 = make_behavior(<<"step1"/utf8>>, []),
    B2 = make_behavior(<<"step2"/utf8>>, [<<"step1"/utf8>>]),
    B3 = make_behavior(<<"step3"/utf8>>, [<<"step2"/utf8>>]),
    B4 = make_behavior(<<"step4"/utf8>>, [<<"step3"/utf8>>]),
    B5 = make_behavior(<<"step5"/utf8>>, [<<"step4"/utf8>>]),
    Spec = make_spec([make_feature(<<"Feature"/utf8>>, [B1, B2, B3, B4, B5])]),
    Result = intent@resolver:resolve_execution_order(Spec),
    case Result of
        {ok, Resolved} ->
            _pipe = erlang:length(Resolved),
            gleeunit_ffi:should_equal(_pipe, 5),
            Names = gleam@list:map(
                Resolved,
                fun(Rb) -> erlang:element(2, erlang:element(3, Rb)) end
            ),
            _pipe@1 = Names,
            gleeunit_ffi:should_equal(
                _pipe@1,
                [<<"step1"/utf8>>,
                    <<"step2"/utf8>>,
                    <<"step3"/utf8>>,
                    <<"step4"/utf8>>,
                    <<"step5"/utf8>>]
            );

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1345).
-spec rules_engine_empty_body_test() -> nil.
rules_engine_empty_body_test() ->
    Rule = {rule,
        <<"Empty body rule"/utf8>>,
        <<"Handle empty response"/utf8>>,
        {'when', <<"== 204"/utf8>>, delete, <<"/resource"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = {execution_result,
        204,
        gleam@dict:new(),
        gleam@json:null(),
        <<""/utf8>>,
        50,
        delete,
        <<"/resource"/utf8>>},
    Results = intent@rules_engine:check_rules([Rule], Response, <<"test"/utf8>>),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1379).
-spec rules_engine_null_json_value_test() -> nil.
rules_engine_null_json_value_test() ->
    Rule = {rule,
        <<"Null handling rule"/utf8>>,
        <<"Handle null values"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/nullable"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = {execution_result,
        200,
        gleam@dict:new(),
        gleam@json:object([{<<"value"/utf8>>, gleam@json:null()}]),
        <<"{\"value\":null}"/utf8>>,
        60,
        get,
        <<"/nullable"/utf8>>},
    Results = intent@rules_engine:check_rules([Rule], Response, <<"test"/utf8>>),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1413).
-spec rules_engine_whitespace_body_test() -> nil.
rules_engine_whitespace_body_test() ->
    Rule = {rule,
        <<"Whitespace rule"/utf8>>,
        <<"Handle whitespace body"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/test"/utf8>>},
        {rule_check, [<<"error"/utf8>>], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = {execution_result,
        200,
        gleam@dict:new(),
        gleam@json:null(),
        <<"   \n\t  "/utf8>>,
        40,
        get,
        <<"/test"/utf8>>},
    Results = intent@rules_engine:check_rules([Rule], Response, <<"test"/utf8>>),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1447).
-spec rules_engine_nested_null_field_test() -> nil.
rules_engine_nested_null_field_test() ->
    Rule = {rule,
        <<"Nested null rule"/utf8>>,
        <<"Check nested fields"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/nested"/utf8>>},
        {rule_check, [], [], [<<"user"/utf8>>], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = {execution_result,
        200,
        gleam@dict:new(),
        gleam@json:object([{<<"user"/utf8>>, gleam@json:null()}]),
        <<"{\"user\":null}"/utf8>>,
        55,
        get,
        <<"/nested"/utf8>>},
    Results = intent@rules_engine:check_rules([Rule], Response, <<"test"/utf8>>),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1481).
-spec rules_engine_empty_object_test() -> nil.
rules_engine_empty_object_test() ->
    Rule = {rule,
        <<"Empty object rule"/utf8>>,
        <<"Handle empty objects"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/data"/utf8>>},
        {rule_check, [], [], [<<"data"/utf8>>], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = {execution_result,
        200,
        gleam@dict:new(),
        gleam@json:object([{<<"data"/utf8>>, gleam@json:object([])}]),
        <<"{\"data\":{}}"/utf8>>,
        65,
        get,
        <<"/data"/utf8>>},
    Results = intent@rules_engine:check_rules([Rule], Response, <<"test"/utf8>>),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.
