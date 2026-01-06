-module(intent_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/intent_test.gleam").
-export([main/0, resolver_simple_no_deps_test/0, resolver_linear_dependency_chain_test/0, resolver_multiple_deps_on_one_test/0, resolver_missing_dependency_test/0, resolver_cyclic_dependency_test/0, resolver_duplicate_name_test/0, resolver_cross_feature_deps_test/0, interpolate_missing_variable_test/0, interpolate_no_variables_test/0, interpolate_simple_variable_test/0, interpolate_multiple_variables_test/0, interview_get_questions_api_round_1_test/0, interview_get_questions_cli_round_1_test/0, interview_create_session_test/0, interview_extract_auth_method_jwt_test/0, interview_extract_auth_method_oauth_test/0, interview_extract_entities_test/0, interview_extract_audience_mobile_test/0, interview_detect_gaps_empty_answers_test/0, interview_detect_gaps_with_answers_test/0, interview_detect_conflicts_cap_theorem_test/0, interview_calculate_confidence_high_test/0, interview_add_answer_test/0, interview_complete_round_test/0, interview_format_question_critical_test/0, http_client_url_construction_simple_test/0, http_client_path_interpolation_test/0, http_client_missing_variable_interpolation_test/0, http_client_header_interpolation_test/0, http_client_header_merge_test/0, http_client_body_json_interpolation_test/0, http_client_invalid_url_test/0, http_client_https_url_test/0, http_client_custom_port_test/0, http_client_path_leading_slash_test/0, http_client_method_conversion_get_test/0, http_client_method_conversion_post_test/0, http_client_multiple_header_merge_test/0, rules_engine_check_when_status_equals_test/0, rules_engine_check_when_status_greater_than_test/0, rules_engine_check_when_status_less_than_test/0, rules_engine_check_when_method_mismatch_test/0, rules_engine_check_when_path_exact_match_test/0, rules_engine_check_when_path_regex_match_test/0, rules_engine_check_body_must_contain_test/0, rules_engine_check_body_must_not_contain_test/0, rules_engine_check_body_must_not_contain_violation_test/0, rules_engine_check_body_must_contain_violation_test/0, rules_engine_check_multiple_rules_test/0, rules_engine_format_violation_body_contains_test/0, rules_engine_format_violation_body_missing_test/0, rules_engine_format_violation_field_missing_test/0, rules_engine_format_violation_header_missing_test/0, resolver_complex_diamond_dependency_test/0, resolver_multiple_branches_test/0, resolver_deep_chain_test/0, rules_engine_empty_body_test/0, rules_engine_null_json_value_test/0, rules_engine_whitespace_body_test/0, rules_engine_nested_null_field_test/0, rules_engine_empty_object_test/0, interpolate_unicode_variable_test/0, interpolate_unicode_in_path_test/0, rules_engine_unicode_body_content_test/0, rules_engine_emoji_in_description_test/0, interpolate_special_characters_test/0, http_client_unicode_header_test/0, json_encoding_test/0, summary_calculation_test/0, string_formatting_test/0, error_message_formatting_test/0, list_to_string_formatting_test/0, boolean_to_status_test/0, json_null_handling_test/0, bead_generation_api_profile_test/0, bead_generation_cli_profile_test/0, bead_to_jsonl_format_test/0, beads_to_jsonl_multiple_test/0, bead_stats_calculation_test/0, filter_beads_by_type_test/0, sort_beads_by_priority_test/0, add_bead_dependency_test/0, empty_session_beads_test/0, interview_session_to_json_test/0, bead_generation_event_profile_test/0, bead_generation_data_profile_test/0, bead_generation_workflow_profile_test/0, bead_generation_ui_profile_test/0, bead_record_required_fields_test/0, bead_stats_empty_list_test/0, bead_multiple_dependencies_test/0, bead_generation_preserves_answer_content_test/0, formats_validate_email_valid_simple_test/0, formats_validate_email_valid_with_subdomain_test/0, formats_validate_email_valid_with_plus_test/0, formats_validate_email_valid_with_dots_test/0, formats_validate_email_valid_with_hyphen_local_test/0, formats_validate_email_valid_with_underscore_test/0, formats_validate_email_invalid_no_at_test/0, formats_validate_email_invalid_multiple_at_test/0, formats_validate_email_invalid_empty_local_test/0, formats_validate_email_invalid_empty_domain_test/0, formats_validate_email_invalid_consecutive_dots_local_test/0, formats_validate_email_invalid_starts_with_dot_test/0, formats_validate_email_invalid_ends_with_dot_test/0, formats_validate_email_invalid_no_domain_dot_test/0, formats_validate_email_invalid_domain_starts_hyphen_test/0, formats_validate_email_invalid_domain_ends_hyphen_test/0, formats_validate_uuid_valid_v4_test/0, formats_validate_uuid_valid_v1_test/0, formats_validate_uuid_valid_uppercase_test/0, formats_validate_uuid_invalid_wrong_segment_count_test/0, formats_validate_uuid_invalid_wrong_segment_length_test/0, formats_validate_uuid_invalid_non_hex_test/0, formats_validate_uuid_invalid_version_test/0, formats_validate_uuid_invalid_variant_test/0, formats_validate_uuid_invalid_no_dashes_test/0, formats_validate_uri_valid_http_test/0, formats_validate_uri_valid_https_test/0, formats_validate_uri_valid_ftp_test/0, formats_validate_uri_valid_with_path_test/0, formats_validate_uri_valid_with_port_test/0, formats_validate_uri_valid_with_query_test/0, formats_validate_uri_invalid_empty_test/0, formats_validate_uri_invalid_no_scheme_test/0, formats_validate_uri_invalid_scheme_only_test/0, formats_validate_uri_invalid_scheme_starts_number_test/0, formats_validate_iso8601_valid_date_only_test/0, formats_validate_iso8601_valid_datetime_test/0, formats_validate_iso8601_valid_datetime_with_z_test/0, formats_validate_iso8601_valid_datetime_with_tz_plus_test/0, formats_validate_iso8601_valid_datetime_with_tz_minus_test/0, formats_validate_iso8601_valid_datetime_fractional_seconds_test/0, formats_validate_iso8601_valid_feb_28_non_leap_test/0, formats_validate_iso8601_valid_feb_29_leap_test/0, formats_validate_iso8601_invalid_too_short_test/0, formats_validate_iso8601_invalid_month_13_test/0, formats_validate_iso8601_invalid_month_00_test/0, formats_validate_iso8601_invalid_day_32_test/0, formats_validate_iso8601_invalid_day_00_test/0, formats_validate_iso8601_invalid_feb_29_non_leap_test/0, formats_validate_iso8601_invalid_april_31_test/0, formats_validate_iso8601_invalid_hour_24_test/0, formats_validate_iso8601_invalid_minute_60_test/0, formats_validate_iso8601_invalid_second_60_test/0, formats_validate_iso8601_invalid_separator_test/0, formats_validate_iso8601_valid_space_separator_test/0, checker_status_code_match_test/0, checker_status_code_mismatch_test/0, checker_field_equals_string_pass_test/0, checker_field_equals_string_fail_test/0, checker_field_equals_int_pass_test/0, checker_field_is_string_pass_test/0, checker_field_is_string_fail_test/0, checker_field_is_integer_pass_test/0, checker_field_is_boolean_pass_test/0, checker_field_is_array_pass_test/0, checker_field_is_object_pass_test/0, checker_field_present_pass_test/0, checker_field_present_fail_test/0, checker_field_absent_pass_test/0, checker_field_absent_fail_test/0, checker_field_non_empty_string_pass_test/0, checker_field_non_empty_string_fail_test/0, checker_field_is_email_pass_test/0, checker_field_is_email_fail_test/0, checker_field_is_uuid_pass_test/0, checker_field_is_uuid_fail_test/0, checker_field_is_iso8601_pass_test/0, checker_field_integer_gte_pass_test/0, checker_field_integer_gte_fail_test/0, checker_field_integer_lte_pass_test/0, checker_field_number_between_pass_test/0, checker_field_number_between_fail_test/0, checker_string_starts_with_pass_test/0, checker_string_ends_with_pass_test/0, checker_string_containing_pass_test/0, checker_non_empty_array_pass_test/0, checker_non_empty_array_fail_test/0, checker_array_of_length_pass_test/0, checker_array_min_items_pass_test/0, checker_one_of_pass_test/0, checker_one_of_fail_test/0, checker_header_present_pass_test/0, checker_header_value_mismatch_test/0, checker_header_missing_test/0, checker_header_case_insensitive_test/0, checker_nested_field_pass_test/0, checker_nested_field_missing_test/0, checker_multiple_checks_test/0, question_loader_file_not_found_test/0, question_loader_merge_empty_custom_returns_base_test/0, question_loader_merge_adds_new_questions_test/0, question_loader_merge_overrides_by_id_test/0, question_loader_merge_preserves_non_overridden_test/0, question_loader_merge_common_rounds_test/0, diff_sessions_no_changes_test/0, diff_sessions_answer_added_test/0, diff_sessions_answer_modified_test/0, diff_sessions_answer_removed_test/0, diff_sessions_stage_changed_test/0, diff_sessions_gaps_resolved_test/0, diff_sessions_conflicts_resolved_test/0, create_snapshot_test/0, format_diff_produces_output_test/0]).

-file("test/intent_test.gleam", 25).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("test/intent_test.gleam", 33).
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

-file("test/intent_test.gleam", 57).
-spec make_feature(binary(), list(intent@types:behavior())) -> intent@types:feature().
make_feature(Name, Behaviors) ->
    {feature, Name, <<"Test feature"/utf8>>, Behaviors}.

-file("test/intent_test.gleam", 61).
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

-file("test/intent_test.gleam", 90).
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

-file("test/intent_test.gleam", 109).
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

-file("test/intent_test.gleam", 133).
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
                                line => 150,
                                value => _assert_fail,
                                start => 3907,
                                'end' => 3937,
                                pattern_start => 3918,
                                pattern_end => 3929})
            end,
            _pipe@1 = First@1,
            gleeunit_ffi:should_equal(_pipe@1, <<"base"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 158).
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

-file("test/intent_test.gleam", 178).
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

-file("test/intent_test.gleam", 194).
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

-file("test/intent_test.gleam", 213).
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

-file("test/intent_test.gleam", 277).
-spec interpolate_missing_variable_test() -> binary().
interpolate_missing_variable_test() ->
    Ctx = intent@interpolate:new_context(),
    Result = intent@interpolate:interpolate_string(
        Ctx,
        <<"/users/${unknown}"/utf8>>
    ),
    _pipe = Result,
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 286).
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

-file("test/intent_test.gleam", 300).
-spec json_string(binary()) -> gleam@json:json().
json_string(S) ->
    gleam@json:string(S).

-file("test/intent_test.gleam", 243).
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

-file("test/intent_test.gleam", 261).
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

-file("test/intent_test.gleam", 308).
-spec interview_get_questions_api_round_1_test() -> nil.
interview_get_questions_api_round_1_test() ->
    Questions = intent@interview_questions:get_questions_for_round(
        <<"api"/utf8>>,
        1
    ),
    Has_questions = Questions /= [],
    _pipe = Has_questions,
    gleeunit@should:be_true(_pipe).

-file("test/intent_test.gleam", 314).
-spec interview_get_questions_cli_round_1_test() -> nil.
interview_get_questions_cli_round_1_test() ->
    Questions = intent@interview_questions:get_questions_for_round(
        <<"cli"/utf8>>,
        1
    ),
    Has_questions = Questions /= [],
    _pipe = Has_questions,
    gleeunit@should:be_true(_pipe).

-file("test/intent_test.gleam", 320).
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

-file("test/intent_test.gleam", 331).
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

-file("test/intent_test.gleam", 340).
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

-file("test/intent_test.gleam", 349).
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

-file("test/intent_test.gleam", 359).
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

-file("test/intent_test.gleam", 368).
-spec interview_detect_gaps_empty_answers_test() -> nil.
interview_detect_gaps_empty_answers_test() ->
    Answers = [],
    Gaps = intent@interview:detect_gaps(api, Answers),
    Has_gaps = Gaps /= [],
    _pipe = Has_gaps,
    gleeunit@should:be_true(_pipe).

-file("test/intent_test.gleam", 375).
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

-file("test/intent_test.gleam", 438).
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

-file("test/intent_test.gleam", 469).
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

-file("test/intent_test.gleam", 484).
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

-file("test/intent_test.gleam", 504).
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
    gleeunit_ffi:should_equal(_pipe@1, discovery).

-file("test/intent_test.gleam", 513).
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

-file("test/intent_test.gleam", 538).
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

-file("test/intent_test.gleam", 567).
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

-file("test/intent_test.gleam", 597).
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

-file("test/intent_test.gleam", 623).
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

-file("test/intent_test.gleam", 652).
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

-file("test/intent_test.gleam", 682).
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

-file("test/intent_test.gleam", 717).
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

-file("test/intent_test.gleam", 743).
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

-file("test/intent_test.gleam", 770).
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

-file("test/intent_test.gleam", 797).
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

-file("test/intent_test.gleam", 825).
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

-file("test/intent_test.gleam", 852).
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

-file("test/intent_test.gleam", 879).
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

-file("test/intent_test.gleam", 916).
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

-file("test/intent_test.gleam", 933).
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

-file("test/intent_test.gleam", 962).
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

-file("test/intent_test.gleam", 991).
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

-file("test/intent_test.gleam", 1017).
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

-file("test/intent_test.gleam", 1042).
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

-file("test/intent_test.gleam", 1068).
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

-file("test/intent_test.gleam", 1094).
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

-file("test/intent_test.gleam", 1120).
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

-file("test/intent_test.gleam", 1146).
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

-file("test/intent_test.gleam", 1176).
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

-file("test/intent_test.gleam", 1204).
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

-file("test/intent_test.gleam", 1242).
-spec rules_engine_format_violation_body_contains_test() -> nil.
rules_engine_format_violation_body_contains_test() ->
    Violation = {body_contains, <<"forbidden"/utf8>>, <<"response body"/utf8>>},
    Formatted = intent@rules_engine:format_violation(Violation),
    _pipe = Formatted,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"forbidden"/utf8>>),
    gleeunit@should:be_true(_pipe@1).

-file("test/intent_test.gleam", 1250).
-spec rules_engine_format_violation_body_missing_test() -> nil.
rules_engine_format_violation_body_missing_test() ->
    Violation = {body_missing, <<"required"/utf8>>},
    Formatted = intent@rules_engine:format_violation(Violation),
    _pipe = Formatted,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"required"/utf8>>),
    gleeunit@should:be_true(_pipe@1).

-file("test/intent_test.gleam", 1258).
-spec rules_engine_format_violation_field_missing_test() -> nil.
rules_engine_format_violation_field_missing_test() ->
    Violation = {field_missing, <<"user.id"/utf8>>},
    Formatted = intent@rules_engine:format_violation(Violation),
    _pipe = Formatted,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"user.id"/utf8>>),
    gleeunit@should:be_true(_pipe@1).

-file("test/intent_test.gleam", 1266).
-spec rules_engine_format_violation_header_missing_test() -> nil.
rules_engine_format_violation_header_missing_test() ->
    Violation = {header_missing, <<"X-Custom"/utf8>>},
    Formatted = intent@rules_engine:format_violation(Violation),
    _pipe = Formatted,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"X-Custom"/utf8>>),
    gleeunit@should:be_true(_pipe@1).

-file("test/intent_test.gleam", 1278).
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
                                line => 1293,
                                value => _assert_fail,
                                start => 35270,
                                'end' => 35300,
                                pattern_start => 35281,
                                pattern_end => 35292})
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

-file("test/intent_test.gleam", 1305).
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

-file("test/intent_test.gleam", 1331).
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

-file("test/intent_test.gleam", 1356).
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

-file("test/intent_test.gleam", 1390).
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

-file("test/intent_test.gleam", 1424).
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

-file("test/intent_test.gleam", 1458).
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

-file("test/intent_test.gleam", 1492).
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

-file("test/intent_test.gleam", 1530).
-spec interpolate_unicode_variable_test() -> nil.
interpolate_unicode_variable_test() ->
    Ctx = begin
        _pipe = intent@interpolate:new_context(),
        intent@interpolate:set_variable(
            _pipe,
            <<"emoji"/utf8>>,
            json_string(<<"🎉"/utf8>>)
        )
    end,
    Result = intent@interpolate:interpolate_string(
        Ctx,
        <<"status: ${emoji}"/utf8>>
    ),
    case Result of
        {ok, S} ->
            _pipe@1 = S,
            gleeunit_ffi:should_equal(_pipe@1, <<"status: 🎉"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1543).
-spec interpolate_unicode_in_path_test() -> nil.
interpolate_unicode_in_path_test() ->
    Ctx = begin
        _pipe = intent@interpolate:new_context(),
        intent@interpolate:set_variable(
            _pipe,
            <<"category"/utf8>>,
            json_string(<<"réclame"/utf8>>)
        )
    end,
    Result = intent@interpolate:interpolate_string(
        Ctx,
        <<"/search/${category}"/utf8>>
    ),
    case Result of
        {ok, S} ->
            _pipe@1 = S,
            gleeunit_ffi:should_equal(_pipe@1, <<"/search/réclame"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1556).
-spec rules_engine_unicode_body_content_test() -> nil.
rules_engine_unicode_body_content_test() ->
    Rule = {rule,
        <<"Unicode content rule"/utf8>>,
        <<"Check for Unicode in response"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/message"/utf8>>},
        {rule_check, [], [<<"✓"/utf8>>], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = {execution_result,
        200,
        gleam@dict:new(),
        gleam@json:null(),
        <<"Status: ✓ All systems operational"/utf8>>,
        50,
        get,
        <<"/message"/utf8>>},
    Results = intent@rules_engine:check_rules([Rule], Response, <<"test"/utf8>>),
    case Results of
        [{rule_passed, _}] ->
            gleeunit_ffi:should_be_ok({ok, nil});

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1590).
-spec rules_engine_emoji_in_description_test() -> nil.
rules_engine_emoji_in_description_test() ->
    Rule = {rule,
        <<"emoji_test"/utf8>>,
        <<"Check emoji support 🚀 in descriptions"/utf8>>,
        {'when', <<"== 200"/utf8>>, get, <<"/status"/utf8>>},
        {rule_check, [], [], [], [], <<""/utf8>>, <<""/utf8>>},
        gleam@json:null()},
    Response = {execution_result,
        200,
        gleam@dict:new(),
        gleam@json:null(),
        <<"ok"/utf8>>,
        50,
        get,
        <<"/status"/utf8>>},
    Results = intent@rules_engine:check_rules([Rule], Response, <<"test"/utf8>>),
    case Results of
        [{rule_passed, Name}] ->
            _pipe = Name,
            gleeunit_ffi:should_equal(_pipe, <<"emoji_test"/utf8>>);

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1626).
-spec interpolate_special_characters_test() -> nil.
interpolate_special_characters_test() ->
    Ctx = begin
        _pipe = intent@interpolate:new_context(),
        intent@interpolate:set_variable(
            _pipe,
            <<"special"/utf8>>,
            json_string(<<"@#$%^&*()"/utf8>>)
        )
    end,
    Result = intent@interpolate:interpolate_string(
        Ctx,
        <<"chars: ${special}"/utf8>>
    ),
    case Result of
        {ok, S} ->
            _pipe@1 = S,
            gleeunit_ffi:should_equal(_pipe@1, <<"chars: @#$%^&*()"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1639).
-spec http_client_unicode_header_test() -> nil.
http_client_unicode_header_test() ->
    Config = {config,
        <<"http://localhost:8080"/utf8>>,
        5000,
        maps:from_list([{<<"X-Custom"/utf8>>, <<"café"/utf8>>}])},
    Request = {request,
        get,
        <<"/test"/utf8>>,
        maps:from_list([{<<"X-Greeting"/utf8>>, <<"こんにちは"/utf8>>}]),
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

-file("test/intent_test.gleam", 1670).
-spec json_encoding_test() -> nil.
json_encoding_test() ->
    Value = gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(<<"Test"/utf8>>)},
            {<<"count"/utf8>>, gleam@json:int(42)},
            {<<"enabled"/utf8>>, gleam@json:bool(true)}]
    ),
    Json_str = gleam@json:to_string(Value),
    _pipe = Json_str,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"Test"/utf8>>),
    gleeunit@should:be_true(_pipe@1),
    _pipe@2 = Json_str,
    _pipe@3 = gleam_stdlib:contains_string(_pipe@2, <<"42"/utf8>>),
    gleeunit@should:be_true(_pipe@3).

-file("test/intent_test.gleam", 1690).
-spec summary_calculation_test() -> nil.
summary_calculation_test() ->
    Passed = 10,
    Failed = 3,
    Blocked = 1,
    Total = (Passed + Failed) + Blocked,
    _pipe = Total,
    gleeunit_ffi:should_equal(_pipe, 14),
    Percentage = case Total of
        0 -> 0;
        Gleam@denominator -> Passed * 100 div Gleam@denominator
    end,
    _pipe@1 = Percentage,
    gleeunit_ffi:should_equal(_pipe@1, 71).

-file("test/intent_test.gleam", 1703).
-spec string_formatting_test() -> nil.
string_formatting_test() ->
    Behavior_name = <<"get-user-by-id"/utf8>>,
    Feature_name = <<"User Management"/utf8>>,
    Formatted = <<<<Feature_name/binary, ": "/utf8>>/binary,
        Behavior_name/binary>>,
    _pipe = Formatted,
    gleeunit_ffi:should_equal(_pipe, <<"User Management: get-user-by-id"/utf8>>).

-file("test/intent_test.gleam", 1714).
-spec error_message_formatting_test() -> nil.
error_message_formatting_test() ->
    Field = <<"status"/utf8>>,
    Expected = <<"200"/utf8>>,
    Actual = <<"404"/utf8>>,
    Message = <<<<<<<<<<<<"Field '"/utf8, Field/binary>>/binary,
                        "' expected '"/utf8>>/binary,
                    Expected/binary>>/binary,
                "' but got '"/utf8>>/binary,
            Actual/binary>>/binary,
        "'"/utf8>>,
    _pipe = Message,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"status"/utf8>>),
    gleeunit@should:be_true(_pipe@1),
    _pipe@2 = Message,
    _pipe@3 = gleam_stdlib:contains_string(_pipe@2, <<"200"/utf8>>),
    gleeunit@should:be_true(_pipe@3),
    _pipe@4 = Message,
    _pipe@5 = gleam_stdlib:contains_string(_pipe@4, <<"404"/utf8>>),
    gleeunit@should:be_true(_pipe@5).

-file("test/intent_test.gleam", 1736).
-spec list_to_string_formatting_test() -> nil.
list_to_string_formatting_test() ->
    Items = [<<"first"/utf8>>, <<"second"/utf8>>, <<"third"/utf8>>],
    Formatted = gleam@string:join(Items, <<", "/utf8>>),
    _pipe = Formatted,
    gleeunit_ffi:should_equal(_pipe, <<"first, second, third"/utf8>>).

-file("test/intent_test.gleam", 1744).
-spec boolean_to_status_test() -> nil.
boolean_to_status_test() ->
    Passed = true,
    Status = case Passed of
        true ->
            <<"PASS"/utf8>>
    end,
    _pipe = Status,
    gleeunit_ffi:should_equal(_pipe, <<"PASS"/utf8>>).

-file("test/intent_test.gleam", 1754).
-spec json_null_handling_test() -> nil.
json_null_handling_test() ->
    Value = gleam@json:null(),
    Json_str = gleam@json:to_string(Value),
    _pipe = Json_str,
    gleeunit_ffi:should_equal(_pipe, <<"null"/utf8>>).

-file("test/intent_test.gleam", 1766).
-spec bead_generation_api_profile_test() -> nil.
bead_generation_api_profile_test() ->
    Session = {interview_session,
        <<"test-api-session"/utf8>>,
        api,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        complete,
        5,
        [{answer,
                <<"q1"/utf8>>,
                <<"What API endpoints do you need?"/utf8>>,
                user,
                1,
                <<"GET /users and POST /users for user management"/utf8>>,
                gleam@dict:new(),
                0.95,
                <<""/utf8>>,
                <<"2026-01-05T00:00:00Z"/utf8>>},
            {answer,
                <<"q2"/utf8>>,
                <<"What is the endpoint path?"/utf8>>,
                developer,
                1,
                <<"/users"/utf8>>,
                gleam@dict:new(),
                0.9,
                <<""/utf8>>,
                <<"2026-01-05T00:00:00Z"/utf8>>}],
        [],
        [],
        <<"API interview notes"/utf8>>},
    Beads = intent@bead_templates:generate_beads_from_session(Session),
    _pipe = gleam@list:is_empty(Beads),
    gleeunit_ffi:should_equal(_pipe, false),
    case gleam@list:first(Beads) of
        {ok, First_bead} ->
            _pipe@1 = erlang:element(4, First_bead),
            gleeunit_ffi:should_equal(_pipe@1, <<"api"/utf8>>),
            _pipe@2 = erlang:element(6, First_bead),
            gleeunit_ffi:should_equal(_pipe@2, <<"api_endpoint"/utf8>>),
            _pipe@3 = erlang:element(5, First_bead),
            gleeunit_ffi:should_equal(_pipe@3, 3);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1821).
-spec bead_generation_cli_profile_test() -> nil.
bead_generation_cli_profile_test() ->
    Session = {interview_session,
        <<"test-cli-session"/utf8>>,
        cli,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        complete,
        5,
        [{answer,
                <<"q1"/utf8>>,
                <<"What commands do you need?"/utf8>>,
                user,
                1,
                <<"list command to show all users"/utf8>>,
                gleam@dict:new(),
                0.9,
                <<""/utf8>>,
                <<"2026-01-05T00:00:00Z"/utf8>>}],
        [],
        [],
        <<"CLI interview notes"/utf8>>},
    Beads = intent@bead_templates:generate_beads_from_session(Session),
    _pipe = gleam@list:is_empty(Beads),
    gleeunit_ffi:should_equal(_pipe, false),
    case gleam@list:first(Beads) of
        {ok, First_bead} ->
            _pipe@1 = erlang:element(4, First_bead),
            gleeunit_ffi:should_equal(_pipe@1, <<"cli"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 1859).
-spec bead_to_jsonl_format_test() -> nil.
bead_to_jsonl_format_test() ->
    Bead = {bead_record,
        <<"Test Implementation"/utf8>>,
        <<"A test bead for validation"/utf8>>,
        <<"api"/utf8>>,
        2,
        <<"api_endpoint"/utf8>>,
        [<<"api"/utf8>>, <<"test"/utf8>>],
        <<"Implement according to spec"/utf8>>,
        [<<"Works correctly"/utf8>>, <<"Passes tests"/utf8>>],
        []},
    Jsonl_line = intent@bead_templates:bead_to_jsonl_line(Bead),
    _pipe = Jsonl_line,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"\"title\""/utf8>>),
    gleeunit@should:be_true(_pipe@1),
    _pipe@2 = Jsonl_line,
    _pipe@3 = gleam_stdlib:contains_string(
        _pipe@2,
        <<"\"Test Implementation\""/utf8>>
    ),
    gleeunit@should:be_true(_pipe@3),
    _pipe@4 = Jsonl_line,
    _pipe@5 = gleam_stdlib:contains_string(_pipe@4, <<"\"description\""/utf8>>),
    gleeunit@should:be_true(_pipe@5),
    _pipe@6 = Jsonl_line,
    _pipe@7 = gleam_stdlib:contains_string(_pipe@6, <<"\"profile_type\""/utf8>>),
    gleeunit@should:be_true(_pipe@7),
    _pipe@8 = Jsonl_line,
    _pipe@9 = gleam_stdlib:contains_string(_pipe@8, <<"\"api\""/utf8>>),
    gleeunit@should:be_true(_pipe@9).

-file("test/intent_test.gleam", 1883).
-spec beads_to_jsonl_multiple_test() -> nil.
beads_to_jsonl_multiple_test() ->
    Beads = [{bead_record,
            <<"First Bead"/utf8>>,
            <<"First task"/utf8>>,
            <<"api"/utf8>>,
            3,
            <<"feature"/utf8>>,
            [<<"high"/utf8>>],
            <<"Do this first"/utf8>>,
            [],
            []},
        {bead_record,
            <<"Second Bead"/utf8>>,
            <<"Second task"/utf8>>,
            <<"data"/utf8>>,
            2,
            <<"schema"/utf8>>,
            [<<"medium"/utf8>>],
            <<"Then do this"/utf8>>,
            [],
            [<<"First Bead"/utf8>>]}],
    Jsonl = intent@bead_templates:beads_to_jsonl(Beads),
    Lines = gleam@string:split(Jsonl, <<"\n"/utf8>>),
    _pipe = erlang:length(Lines),
    gleeunit_ffi:should_equal(_pipe, 2),
    _pipe@1 = Jsonl,
    _pipe@2 = gleam_stdlib:contains_string(_pipe@1, <<"First Bead"/utf8>>),
    gleeunit@should:be_true(_pipe@2),
    _pipe@3 = Jsonl,
    _pipe@4 = gleam_stdlib:contains_string(_pipe@3, <<"Second Bead"/utf8>>),
    gleeunit@should:be_true(_pipe@4),
    _pipe@5 = Jsonl,
    _pipe@6 = gleam_stdlib:contains_string(_pipe@5, <<"First task"/utf8>>),
    gleeunit@should:be_true(_pipe@6),
    _pipe@7 = Jsonl,
    _pipe@8 = gleam_stdlib:contains_string(_pipe@7, <<"Second task"/utf8>>),
    gleeunit@should:be_true(_pipe@8).

-file("test/intent_test.gleam", 1923).
-spec bead_stats_calculation_test() -> nil.
bead_stats_calculation_test() ->
    Beads = [{bead_record,
            <<"API 1"/utf8>>,
            <<"desc"/utf8>>,
            <<"api"/utf8>>,
            3,
            <<"endpoint"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []},
        {bead_record,
            <<"API 2"/utf8>>,
            <<"desc"/utf8>>,
            <<"api"/utf8>>,
            3,
            <<"endpoint"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []},
        {bead_record,
            <<"Data 1"/utf8>>,
            <<"desc"/utf8>>,
            <<"data"/utf8>>,
            2,
            <<"schema"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []}],
    Stats = intent@bead_templates:bead_stats(Beads),
    _pipe = erlang:element(2, Stats),
    gleeunit_ffi:should_equal(_pipe, 3),
    _pipe@1 = gleam@dict:get(erlang:element(3, Stats), <<"endpoint"/utf8>>),
    gleeunit_ffi:should_equal(_pipe@1, {ok, 2}),
    _pipe@2 = gleam@dict:get(erlang:element(3, Stats), <<"schema"/utf8>>),
    gleeunit_ffi:should_equal(_pipe@2, {ok, 1}),
    _pipe@3 = gleam@dict:get(erlang:element(4, Stats), 3),
    gleeunit_ffi:should_equal(_pipe@3, {ok, 2}),
    _pipe@4 = gleam@dict:get(erlang:element(4, Stats), 2),
    gleeunit_ffi:should_equal(_pipe@4, {ok, 1}).

-file("test/intent_test.gleam", 1980).
-spec filter_beads_by_type_test() -> nil.
filter_beads_by_type_test() ->
    Beads = [{bead_record,
            <<"Endpoint 1"/utf8>>,
            <<"desc"/utf8>>,
            <<"api"/utf8>>,
            1,
            <<"endpoint"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []},
        {bead_record,
            <<"Schema 1"/utf8>>,
            <<"desc"/utf8>>,
            <<"data"/utf8>>,
            1,
            <<"schema"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []},
        {bead_record,
            <<"Endpoint 2"/utf8>>,
            <<"desc"/utf8>>,
            <<"api"/utf8>>,
            1,
            <<"endpoint"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []}],
    Endpoints = intent@bead_templates:filter_beads_by_type(
        Beads,
        <<"endpoint"/utf8>>
    ),
    _pipe = erlang:length(Endpoints),
    gleeunit_ffi:should_equal(_pipe, 2),
    Schemas = intent@bead_templates:filter_beads_by_type(
        Beads,
        <<"schema"/utf8>>
    ),
    _pipe@1 = erlang:length(Schemas),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 2025).
-spec sort_beads_by_priority_test() -> nil.
sort_beads_by_priority_test() ->
    Beads = [{bead_record,
            <<"Low Priority"/utf8>>,
            <<"desc"/utf8>>,
            <<"api"/utf8>>,
            1,
            <<"task"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []},
        {bead_record,
            <<"High Priority"/utf8>>,
            <<"desc"/utf8>>,
            <<"api"/utf8>>,
            5,
            <<"task"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []},
        {bead_record,
            <<"Medium Priority"/utf8>>,
            <<"desc"/utf8>>,
            <<"api"/utf8>>,
            3,
            <<"task"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []}],
    Sorted = intent@bead_templates:sort_beads_by_priority(Beads),
    case gleam@list:first(Sorted) of
        {ok, First} ->
            _pipe = erlang:element(2, First),
            gleeunit_ffi:should_equal(_pipe, <<"High Priority"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end,
    case gleam@list:last(Sorted) of
        {ok, Last} ->
            _pipe@1 = erlang:element(2, Last),
            gleeunit_ffi:should_equal(_pipe@1, <<"Low Priority"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 2078).
-spec add_bead_dependency_test() -> nil.
add_bead_dependency_test() ->
    Beads = [{bead_record,
            <<"Schema Design"/utf8>>,
            <<"desc"/utf8>>,
            <<"data"/utf8>>,
            1,
            <<"schema"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []},
        {bead_record,
            <<"API Endpoint"/utf8>>,
            <<"desc"/utf8>>,
            <<"api"/utf8>>,
            1,
            <<"endpoint"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []}],
    Updated = intent@bead_templates:add_dependency(
        Beads,
        <<"API Endpoint"/utf8>>,
        <<"Schema Design"/utf8>>
    ),
    case gleam@list:last(Updated) of
        {ok, Endpoint_bead} ->
            _pipe = erlang:element(2, Endpoint_bead),
            gleeunit_ffi:should_equal(_pipe, <<"API Endpoint"/utf8>>),
            _pipe@1 = gleam@list:contains(
                erlang:element(10, Endpoint_bead),
                <<"Schema Design"/utf8>>
            ),
            gleeunit@should:be_true(_pipe@1);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 2118).
-spec empty_session_beads_test() -> nil.
empty_session_beads_test() ->
    Session = {interview_session,
        <<"empty-session"/utf8>>,
        api,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        complete,
        0,
        [],
        [],
        [],
        <<""/utf8>>},
    Beads = intent@bead_templates:generate_beads_from_session(Session),
    _pipe = erlang:length(Beads),
    gleeunit_ffi:should_equal(_pipe, 0).

-file("test/intent_test.gleam", 2140).
-spec interview_session_to_json_test() -> nil.
interview_session_to_json_test() ->
    Session = {interview_session,
        <<"test-session-123"/utf8>>,
        api,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        complete,
        5,
        [{answer,
                <<"q1"/utf8>>,
                <<"Test question"/utf8>>,
                user,
                1,
                <<"Test response"/utf8>>,
                gleam@dict:new(),
                0.85,
                <<""/utf8>>,
                <<"2026-01-05T00:00:00Z"/utf8>>}],
        [],
        [],
        <<"Test notes"/utf8>>},
    Json = intent@interview_storage:session_to_json(Session),
    Json_str = gleam@json:to_string(Json),
    _pipe = Json_str,
    _pipe@1 = gleam_stdlib:contains_string(_pipe, <<"test-session-123"/utf8>>),
    gleeunit@should:be_true(_pipe@1),
    _pipe@2 = Json_str,
    _pipe@3 = gleam_stdlib:contains_string(_pipe@2, <<"\"api\""/utf8>>),
    gleeunit@should:be_true(_pipe@3),
    _pipe@4 = Json_str,
    _pipe@5 = gleam_stdlib:contains_string(_pipe@4, <<"\"complete\""/utf8>>),
    gleeunit@should:be_true(_pipe@5),
    _pipe@6 = Json_str,
    _pipe@7 = gleam_stdlib:contains_string(_pipe@6, <<"Test question"/utf8>>),
    gleeunit@should:be_true(_pipe@7).

-file("test/intent_test.gleam", 2178).
-spec bead_generation_event_profile_test() -> nil.
bead_generation_event_profile_test() ->
    Session = {interview_session,
        <<"test-event-session"/utf8>>,
        event,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        complete,
        5,
        [{answer,
                <<"q1"/utf8>>,
                <<"What events should be emitted?"/utf8>>,
                developer,
                1,
                <<"user.created and user.updated events"/utf8>>,
                gleam@dict:new(),
                0.92,
                <<""/utf8>>,
                <<"2026-01-05T00:00:00Z"/utf8>>}],
        [],
        [],
        <<"Event interview notes"/utf8>>},
    Beads = intent@bead_templates:generate_beads_from_session(Session),
    _pipe = gleam@list:is_empty(Beads),
    gleeunit_ffi:should_equal(_pipe, false),
    case gleam@list:first(Beads) of
        {ok, First_bead} ->
            _pipe@1 = erlang:element(4, First_bead),
            gleeunit_ffi:should_equal(_pipe@1, <<"event"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 2216).
-spec bead_generation_data_profile_test() -> nil.
bead_generation_data_profile_test() ->
    Session = {interview_session,
        <<"test-data-session"/utf8>>,
        data,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        complete,
        5,
        [{answer,
                <<"q1"/utf8>>,
                <<"What data models are needed?"/utf8>>,
                developer,
                1,
                <<"User model with id, name, email fields"/utf8>>,
                gleam@dict:new(),
                0.88,
                <<""/utf8>>,
                <<"2026-01-05T00:00:00Z"/utf8>>}],
        [],
        [],
        <<"Data interview notes"/utf8>>},
    Beads = intent@bead_templates:generate_beads_from_session(Session),
    _pipe = gleam@list:is_empty(Beads),
    gleeunit_ffi:should_equal(_pipe, false),
    case gleam@list:first(Beads) of
        {ok, First_bead} ->
            _pipe@1 = erlang:element(4, First_bead),
            gleeunit_ffi:should_equal(_pipe@1, <<"data"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 2254).
-spec bead_generation_workflow_profile_test() -> nil.
bead_generation_workflow_profile_test() ->
    Session = {interview_session,
        <<"test-workflow-session"/utf8>>,
        workflow,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        complete,
        5,
        [{answer,
                <<"q1"/utf8>>,
                <<"What workflows exist?"/utf8>>,
                business,
                1,
                <<"User signup workflow with email verification"/utf8>>,
                gleam@dict:new(),
                0.9,
                <<""/utf8>>,
                <<"2026-01-05T00:00:00Z"/utf8>>}],
        [],
        [],
        <<"Workflow interview notes"/utf8>>},
    Beads = intent@bead_templates:generate_beads_from_session(Session),
    _pipe = gleam@list:is_empty(Beads),
    gleeunit_ffi:should_equal(_pipe, false),
    case gleam@list:first(Beads) of
        {ok, First_bead} ->
            _pipe@1 = erlang:element(4, First_bead),
            gleeunit_ffi:should_equal(_pipe@1, <<"workflow"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 2292).
-spec bead_generation_ui_profile_test() -> nil.
bead_generation_ui_profile_test() ->
    Session = {interview_session,
        <<"test-ui-session"/utf8>>,
        u_i,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        complete,
        5,
        [{answer,
                <<"q1"/utf8>>,
                <<"What UI screens do you need?"/utf8>>,
                user,
                1,
                <<"User dashboard and settings screen"/utf8>>,
                gleam@dict:new(),
                0.87,
                <<""/utf8>>,
                <<"2026-01-05T00:00:00Z"/utf8>>}],
        [],
        [],
        <<"UI interview notes"/utf8>>},
    Beads = intent@bead_templates:generate_beads_from_session(Session),
    _pipe = gleam@list:is_empty(Beads),
    gleeunit_ffi:should_equal(_pipe, false),
    case gleam@list:first(Beads) of
        {ok, First_bead} ->
            _pipe@1 = erlang:element(4, First_bead),
            gleeunit_ffi:should_equal(_pipe@1, <<"ui"/utf8>>);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 2330).
-spec bead_record_required_fields_test() -> nil.
bead_record_required_fields_test() ->
    Bead = {bead_record,
        <<"Required fields test"/utf8>>,
        <<"Testing all required fields present"/utf8>>,
        <<"api"/utf8>>,
        1,
        <<"endpoint"/utf8>>,
        [<<"test"/utf8>>],
        <<"Test hints"/utf8>>,
        [<<"Criterion 1"/utf8>>],
        [<<"dependency1"/utf8>>]},
    _pipe = gleam@string:is_empty(erlang:element(2, Bead)),
    gleeunit_ffi:should_equal(_pipe, false),
    _pipe@1 = gleam@string:is_empty(erlang:element(3, Bead)),
    gleeunit_ffi:should_equal(_pipe@1, false),
    _pipe@2 = gleam@string:is_empty(erlang:element(4, Bead)),
    gleeunit_ffi:should_equal(_pipe@2, false),
    _pipe@3 = erlang:element(5, Bead),
    gleeunit_ffi:should_equal(_pipe@3, 1),
    _pipe@4 = gleam@string:is_empty(erlang:element(6, Bead)),
    gleeunit_ffi:should_equal(_pipe@4, false),
    _pipe@5 = erlang:length(erlang:element(7, Bead)),
    gleeunit_ffi:should_equal(_pipe@5, 1),
    _pipe@6 = gleam@string:is_empty(erlang:element(8, Bead)),
    gleeunit_ffi:should_equal(_pipe@6, false),
    _pipe@7 = erlang:length(erlang:element(9, Bead)),
    gleeunit_ffi:should_equal(_pipe@7, 1),
    _pipe@8 = erlang:length(erlang:element(10, Bead)),
    gleeunit_ffi:should_equal(_pipe@8, 1).

-file("test/intent_test.gleam", 2356).
-spec bead_stats_empty_list_test() -> nil.
bead_stats_empty_list_test() ->
    Beads = [],
    Stats = intent@bead_templates:bead_stats(Beads),
    _pipe = erlang:element(2, Stats),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = gleam@dict:is_empty(erlang:element(3, Stats)),
    gleeunit@should:be_true(_pipe@1),
    _pipe@2 = gleam@dict:is_empty(erlang:element(4, Stats)),
    gleeunit@should:be_true(_pipe@2).

-file("test/intent_test.gleam", 2366).
-spec bead_multiple_dependencies_test() -> nil.
bead_multiple_dependencies_test() ->
    Beads = [{bead_record,
            <<"Implementation"/utf8>>,
            <<"desc"/utf8>>,
            <<"api"/utf8>>,
            1,
            <<"endpoint"/utf8>>,
            [],
            <<""/utf8>>,
            [],
            []}],
    Step1 = intent@bead_templates:add_dependency(
        Beads,
        <<"Implementation"/utf8>>,
        <<"Schema"/utf8>>
    ),
    Step2 = intent@bead_templates:add_dependency(
        Step1,
        <<"Implementation"/utf8>>,
        <<"Auth"/utf8>>
    ),
    case gleam@list:first(Step2) of
        {ok, Bead} ->
            _pipe = erlang:length(erlang:element(10, Bead)),
            gleeunit_ffi:should_equal(_pipe, 2),
            _pipe@1 = gleam@list:contains(
                erlang:element(10, Bead),
                <<"Schema"/utf8>>
            ),
            gleeunit@should:be_true(_pipe@1),
            _pipe@2 = gleam@list:contains(
                erlang:element(10, Bead),
                <<"Auth"/utf8>>
            ),
            gleeunit@should:be_true(_pipe@2);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 2395).
-spec bead_generation_preserves_answer_content_test() -> nil.
bead_generation_preserves_answer_content_test() ->
    Answer_text = <<"Create an API endpoint at /api/users that returns a list of all users with pagination support"/utf8>>,
    Session = {interview_session,
        <<"test-content-session"/utf8>>,
        api,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        <<"2026-01-05T00:00:00Z"/utf8>>,
        complete,
        5,
        [{answer,
                <<"q1"/utf8>>,
                <<"Describe the endpoint"/utf8>>,
                developer,
                1,
                Answer_text,
                gleam@dict:new(),
                0.95,
                <<""/utf8>>,
                <<"2026-01-05T00:00:00Z"/utf8>>}],
        [],
        [],
        <<"Content preservation test notes"/utf8>>},
    Beads = intent@bead_templates:generate_beads_from_session(Session),
    case gleam@list:first(Beads) of
        {ok, First_bead} ->
            _pipe = erlang:element(3, First_bead),
            _pipe@1 = gleam_stdlib:contains_string(_pipe, Answer_text),
            gleeunit@should:be_true(_pipe@1);

        {error, _} ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 2443).
-spec formats_validate_email_valid_simple_test() -> nil.
formats_validate_email_valid_simple_test() ->
    _pipe = intent@formats:validate_email(<<"user@example.com"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2448).
-spec formats_validate_email_valid_with_subdomain_test() -> nil.
formats_validate_email_valid_with_subdomain_test() ->
    _pipe = intent@formats:validate_email(<<"user@mail.example.com"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2453).
-spec formats_validate_email_valid_with_plus_test() -> nil.
formats_validate_email_valid_with_plus_test() ->
    _pipe = intent@formats:validate_email(<<"user+tag@example.com"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2458).
-spec formats_validate_email_valid_with_dots_test() -> nil.
formats_validate_email_valid_with_dots_test() ->
    _pipe = intent@formats:validate_email(<<"first.last@example.com"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2463).
-spec formats_validate_email_valid_with_hyphen_local_test() -> nil.
formats_validate_email_valid_with_hyphen_local_test() ->
    _pipe = intent@formats:validate_email(<<"user-name@example.com"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2468).
-spec formats_validate_email_valid_with_underscore_test() -> nil.
formats_validate_email_valid_with_underscore_test() ->
    _pipe = intent@formats:validate_email(<<"user_name@example.com"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2473).
-spec formats_validate_email_invalid_no_at_test() -> binary().
formats_validate_email_invalid_no_at_test() ->
    _pipe = intent@formats:validate_email(<<"userexample.com"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2478).
-spec formats_validate_email_invalid_multiple_at_test() -> binary().
formats_validate_email_invalid_multiple_at_test() ->
    _pipe = intent@formats:validate_email(<<"user@@example.com"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2483).
-spec formats_validate_email_invalid_empty_local_test() -> binary().
formats_validate_email_invalid_empty_local_test() ->
    _pipe = intent@formats:validate_email(<<"@example.com"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2488).
-spec formats_validate_email_invalid_empty_domain_test() -> binary().
formats_validate_email_invalid_empty_domain_test() ->
    _pipe = intent@formats:validate_email(<<"user@"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2493).
-spec formats_validate_email_invalid_consecutive_dots_local_test() -> binary().
formats_validate_email_invalid_consecutive_dots_local_test() ->
    _pipe = intent@formats:validate_email(<<"user..name@example.com"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2498).
-spec formats_validate_email_invalid_starts_with_dot_test() -> binary().
formats_validate_email_invalid_starts_with_dot_test() ->
    _pipe = intent@formats:validate_email(<<".user@example.com"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2503).
-spec formats_validate_email_invalid_ends_with_dot_test() -> binary().
formats_validate_email_invalid_ends_with_dot_test() ->
    _pipe = intent@formats:validate_email(<<"user.@example.com"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2508).
-spec formats_validate_email_invalid_no_domain_dot_test() -> binary().
formats_validate_email_invalid_no_domain_dot_test() ->
    _pipe = intent@formats:validate_email(<<"user@examplecom"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2513).
-spec formats_validate_email_invalid_domain_starts_hyphen_test() -> binary().
formats_validate_email_invalid_domain_starts_hyphen_test() ->
    _pipe = intent@formats:validate_email(<<"user@-example.com"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2518).
-spec formats_validate_email_invalid_domain_ends_hyphen_test() -> binary().
formats_validate_email_invalid_domain_ends_hyphen_test() ->
    _pipe = intent@formats:validate_email(<<"user@example-.com"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2525).
-spec formats_validate_uuid_valid_v4_test() -> nil.
formats_validate_uuid_valid_v4_test() ->
    _pipe = intent@formats:validate_uuid(
        <<"550e8400-e29b-41d4-a716-446655440000"/utf8>>
    ),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2530).
-spec formats_validate_uuid_valid_v1_test() -> nil.
formats_validate_uuid_valid_v1_test() ->
    _pipe = intent@formats:validate_uuid(
        <<"6ba7b810-9dad-11d1-80b4-00c04fd430c8"/utf8>>
    ),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2535).
-spec formats_validate_uuid_valid_uppercase_test() -> nil.
formats_validate_uuid_valid_uppercase_test() ->
    _pipe = intent@formats:validate_uuid(
        <<"550E8400-E29B-41D4-A716-446655440000"/utf8>>
    ),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2540).
-spec formats_validate_uuid_invalid_wrong_segment_count_test() -> binary().
formats_validate_uuid_invalid_wrong_segment_count_test() ->
    _pipe = intent@formats:validate_uuid(<<"550e8400-e29b-41d4-a716"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2545).
-spec formats_validate_uuid_invalid_wrong_segment_length_test() -> binary().
formats_validate_uuid_invalid_wrong_segment_length_test() ->
    _pipe = intent@formats:validate_uuid(
        <<"550e840-e29b-41d4-a716-446655440000"/utf8>>
    ),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2550).
-spec formats_validate_uuid_invalid_non_hex_test() -> binary().
formats_validate_uuid_invalid_non_hex_test() ->
    _pipe = intent@formats:validate_uuid(
        <<"550e8400-e29b-41d4-a716-44665544000g"/utf8>>
    ),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2555).
-spec formats_validate_uuid_invalid_version_test() -> binary().
formats_validate_uuid_invalid_version_test() ->
    _pipe = intent@formats:validate_uuid(
        <<"550e8400-e29b-61d4-a716-446655440000"/utf8>>
    ),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2560).
-spec formats_validate_uuid_invalid_variant_test() -> binary().
formats_validate_uuid_invalid_variant_test() ->
    _pipe = intent@formats:validate_uuid(
        <<"550e8400-e29b-41d4-0716-446655440000"/utf8>>
    ),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2565).
-spec formats_validate_uuid_invalid_no_dashes_test() -> binary().
formats_validate_uuid_invalid_no_dashes_test() ->
    _pipe = intent@formats:validate_uuid(
        <<"550e8400e29b41d4a716446655440000"/utf8>>
    ),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2572).
-spec formats_validate_uri_valid_http_test() -> nil.
formats_validate_uri_valid_http_test() ->
    _pipe = intent@formats:validate_uri(<<"http://example.com"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2577).
-spec formats_validate_uri_valid_https_test() -> nil.
formats_validate_uri_valid_https_test() ->
    _pipe = intent@formats:validate_uri(<<"https://example.com"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2582).
-spec formats_validate_uri_valid_ftp_test() -> nil.
formats_validate_uri_valid_ftp_test() ->
    _pipe = intent@formats:validate_uri(<<"ftp://files.example.com"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2587).
-spec formats_validate_uri_valid_with_path_test() -> nil.
formats_validate_uri_valid_with_path_test() ->
    _pipe = intent@formats:validate_uri(
        <<"https://example.com/path/to/resource"/utf8>>
    ),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2592).
-spec formats_validate_uri_valid_with_port_test() -> nil.
formats_validate_uri_valid_with_port_test() ->
    _pipe = intent@formats:validate_uri(<<"http://localhost:8080"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2597).
-spec formats_validate_uri_valid_with_query_test() -> nil.
formats_validate_uri_valid_with_query_test() ->
    _pipe = intent@formats:validate_uri(
        <<"https://example.com/search?q=test"/utf8>>
    ),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2602).
-spec formats_validate_uri_invalid_empty_test() -> binary().
formats_validate_uri_invalid_empty_test() ->
    _pipe = intent@formats:validate_uri(<<""/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2607).
-spec formats_validate_uri_invalid_no_scheme_test() -> binary().
formats_validate_uri_invalid_no_scheme_test() ->
    _pipe = intent@formats:validate_uri(<<"example.com"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2612).
-spec formats_validate_uri_invalid_scheme_only_test() -> binary().
formats_validate_uri_invalid_scheme_only_test() ->
    _pipe = intent@formats:validate_uri(<<"http://"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2617).
-spec formats_validate_uri_invalid_scheme_starts_number_test() -> binary().
formats_validate_uri_invalid_scheme_starts_number_test() ->
    _pipe = intent@formats:validate_uri(<<"1http://example.com"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2624).
-spec formats_validate_iso8601_valid_date_only_test() -> nil.
formats_validate_iso8601_valid_date_only_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-15"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2629).
-spec formats_validate_iso8601_valid_datetime_test() -> nil.
formats_validate_iso8601_valid_datetime_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-15T10:30:00"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2634).
-spec formats_validate_iso8601_valid_datetime_with_z_test() -> nil.
formats_validate_iso8601_valid_datetime_with_z_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-15T10:30:00Z"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2639).
-spec formats_validate_iso8601_valid_datetime_with_tz_plus_test() -> nil.
formats_validate_iso8601_valid_datetime_with_tz_plus_test() ->
    _pipe = intent@formats:validate_iso8601(
        <<"2024-01-15T10:30:00+05:30"/utf8>>
    ),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2644).
-spec formats_validate_iso8601_valid_datetime_with_tz_minus_test() -> nil.
formats_validate_iso8601_valid_datetime_with_tz_minus_test() ->
    _pipe = intent@formats:validate_iso8601(
        <<"2024-01-15T10:30:00-08:00"/utf8>>
    ),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2649).
-spec formats_validate_iso8601_valid_datetime_fractional_seconds_test() -> nil.
formats_validate_iso8601_valid_datetime_fractional_seconds_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-15T10:30:00.123"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2654).
-spec formats_validate_iso8601_valid_feb_28_non_leap_test() -> nil.
formats_validate_iso8601_valid_feb_28_non_leap_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2023-02-28"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2659).
-spec formats_validate_iso8601_valid_feb_29_leap_test() -> nil.
formats_validate_iso8601_valid_feb_29_leap_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-02-29"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2664).
-spec formats_validate_iso8601_invalid_too_short_test() -> binary().
formats_validate_iso8601_invalid_too_short_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2669).
-spec formats_validate_iso8601_invalid_month_13_test() -> binary().
formats_validate_iso8601_invalid_month_13_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-13-01"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2674).
-spec formats_validate_iso8601_invalid_month_00_test() -> binary().
formats_validate_iso8601_invalid_month_00_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-00-01"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2679).
-spec formats_validate_iso8601_invalid_day_32_test() -> binary().
formats_validate_iso8601_invalid_day_32_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-32"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2684).
-spec formats_validate_iso8601_invalid_day_00_test() -> binary().
formats_validate_iso8601_invalid_day_00_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-00"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2689).
-spec formats_validate_iso8601_invalid_feb_29_non_leap_test() -> binary().
formats_validate_iso8601_invalid_feb_29_non_leap_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2023-02-29"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2694).
-spec formats_validate_iso8601_invalid_april_31_test() -> binary().
formats_validate_iso8601_invalid_april_31_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-04-31"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2699).
-spec formats_validate_iso8601_invalid_hour_24_test() -> binary().
formats_validate_iso8601_invalid_hour_24_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-15T24:00:00"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2704).
-spec formats_validate_iso8601_invalid_minute_60_test() -> binary().
formats_validate_iso8601_invalid_minute_60_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-15T10:60:00"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2709).
-spec formats_validate_iso8601_invalid_second_60_test() -> binary().
formats_validate_iso8601_invalid_second_60_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-15T10:30:60"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2714).
-spec formats_validate_iso8601_invalid_separator_test() -> binary().
formats_validate_iso8601_invalid_separator_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-15X10:30:00"/utf8>>),
    gleeunit_ffi:should_be_error(_pipe).

-file("test/intent_test.gleam", 2719).
-spec formats_validate_iso8601_valid_space_separator_test() -> nil.
formats_validate_iso8601_valid_space_separator_test() ->
    _pipe = intent@formats:validate_iso8601(<<"2024-01-15 10:30:00"/utf8>>),
    gleeunit_ffi:should_be_ok(_pipe).

-file("test/intent_test.gleam", 2729).
-spec make_test_response(
    integer(),
    gleam@dict:dict(binary(), intent@types:check())
) -> intent@types:response().
make_test_response(Status, Checks) ->
    {response, Status, gleam@json:null(), Checks, gleam@dict:new()}.

-file("test/intent_test.gleam", 2739).
-spec make_test_execution(
    integer(),
    gleam@json:json(),
    gleam@dict:dict(binary(), binary())
) -> intent@http_client:execution_result().
make_test_execution(Status, Body_json, Headers) ->
    {execution_result,
        Status,
        Headers,
        Body_json,
        <<""/utf8>>,
        100,
        get,
        <<"/test"/utf8>>}.

-file("test/intent_test.gleam", 2752).
-spec empty_context() -> intent@interpolate:context().
empty_context() ->
    intent@interpolate:new_context().

-file("test/intent_test.gleam", 2758).
-spec checker_status_code_match_test() -> nil.
checker_status_code_match_test() ->
    Expected = make_test_response(200, gleam@dict:new()),
    Actual = make_test_execution(200, gleam@json:null(), gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:element(4, Result),
    gleeunit@should:be_true(_pipe),
    _pipe@1 = erlang:element(5, Result),
    gleeunit_ffi:should_equal(_pipe@1, 200),
    _pipe@2 = erlang:element(6, Result),
    gleeunit_ffi:should_equal(_pipe@2, 200).

-file("test/intent_test.gleam", 2768).
-spec checker_status_code_mismatch_test() -> nil.
checker_status_code_mismatch_test() ->
    Expected = make_test_response(200, gleam@dict:new()),
    Actual = make_test_execution(404, gleam@json:null(), gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:element(4, Result),
    gleeunit@should:be_false(_pipe),
    _pipe@1 = erlang:element(5, Result),
    gleeunit_ffi:should_equal(_pipe@1, 200),
    _pipe@2 = erlang:element(6, Result),
    gleeunit_ffi:should_equal(_pipe@2, 404).

-file("test/intent_test.gleam", 2780).
-spec checker_field_equals_string_pass_test() -> nil.
checker_field_equals_string_pass_test() ->
    Checks = maps:from_list(
        [{<<"name"/utf8>>,
                {check, <<"equals John"/utf8>>, <<"Name must match"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(<<"John"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 0).

-file("test/intent_test.gleam", 2793).
-spec checker_field_equals_string_fail_test() -> nil.
checker_field_equals_string_fail_test() ->
    Checks = maps:from_list(
        [{<<"name"/utf8>>,
                {check, <<"equals John"/utf8>>, <<"Name must match"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(<<"Jane"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 2806).
-spec checker_field_equals_int_pass_test() -> nil.
checker_field_equals_int_pass_test() ->
    Checks = maps:from_list(
        [{<<"age"/utf8>>,
                {check, <<"equals 25"/utf8>>, <<"Age must match"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object([{<<"age"/utf8>>, gleam@json:int(25)}]),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 0).

-file("test/intent_test.gleam", 2819).
-spec checker_field_is_string_pass_test() -> nil.
checker_field_is_string_pass_test() ->
    Checks = maps:from_list(
        [{<<"name"/utf8>>,
                {check, <<"string"/utf8>>, <<"Must be string"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(<<"test"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 0).

-file("test/intent_test.gleam", 2832).
-spec checker_field_is_string_fail_test() -> nil.
checker_field_is_string_fail_test() ->
    Checks = maps:from_list(
        [{<<"name"/utf8>>,
                {check, <<"string"/utf8>>, <<"Must be string"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object([{<<"name"/utf8>>, gleam@json:int(123)}]),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 2845).
-spec checker_field_is_integer_pass_test() -> nil.
checker_field_is_integer_pass_test() ->
    Checks = maps:from_list(
        [{<<"count"/utf8>>,
                {check, <<"integer"/utf8>>, <<"Must be integer"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object([{<<"count"/utf8>>, gleam@json:int(42)}]),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 2857).
-spec checker_field_is_boolean_pass_test() -> nil.
checker_field_is_boolean_pass_test() ->
    Checks = maps:from_list(
        [{<<"active"/utf8>>,
                {check, <<"boolean"/utf8>>, <<"Must be boolean"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object([{<<"active"/utf8>>, gleam@json:bool(true)}]),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 2869).
-spec checker_field_is_array_pass_test() -> nil.
checker_field_is_array_pass_test() ->
    Checks = maps:from_list(
        [{<<"items"/utf8>>, {check, <<"array"/utf8>>, <<"Must be array"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"items"/utf8>>,
                gleam@json:array(
                    [gleam@json:int(1), gleam@json:int(2)],
                    fun(X) -> X end
                )}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 2881).
-spec checker_field_is_object_pass_test() -> nil.
checker_field_is_object_pass_test() ->
    Checks = maps:from_list(
        [{<<"data"/utf8>>,
                {check, <<"object"/utf8>>, <<"Must be object"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"key"/utf8>>, gleam@json:string(<<"value"/utf8>>)}]
                )}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 2893).
-spec checker_field_present_pass_test() -> nil.
checker_field_present_pass_test() ->
    Checks = maps:from_list(
        [{<<"id"/utf8>>,
                {check, <<"present"/utf8>>, <<"ID must be present"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(<<"abc-123"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 0).

-file("test/intent_test.gleam", 2906).
-spec checker_field_present_fail_test() -> nil.
checker_field_present_fail_test() ->
    Checks = maps:from_list(
        [{<<"id"/utf8>>,
                {check, <<"present"/utf8>>, <<"ID must be present"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(<<"test"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 2919).
-spec checker_field_absent_pass_test() -> nil.
checker_field_absent_pass_test() ->
    Checks = maps:from_list(
        [{<<"password"/utf8>>,
                {check,
                    <<"absent"/utf8>>,
                    <<"Password should not be returned"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(<<"test"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 0).

-file("test/intent_test.gleam", 2932).
-spec checker_field_absent_fail_test() -> nil.
checker_field_absent_fail_test() ->
    Checks = maps:from_list(
        [{<<"password"/utf8>>,
                {check,
                    <<"absent"/utf8>>,
                    <<"Password should not be returned"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"password"/utf8>>, gleam@json:string(<<"secret"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 2945).
-spec checker_field_non_empty_string_pass_test() -> nil.
checker_field_non_empty_string_pass_test() ->
    Checks = maps:from_list(
        [{<<"name"/utf8>>,
                {check,
                    <<"non-empty string"/utf8>>,
                    <<"Name must not be empty"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(<<"John"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 2957).
-spec checker_field_non_empty_string_fail_test() -> nil.
checker_field_non_empty_string_fail_test() ->
    Checks = maps:from_list(
        [{<<"name"/utf8>>,
                {check,
                    <<"non-empty string"/utf8>>,
                    <<"Name must not be empty"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(<<""/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 2970).
-spec checker_field_is_email_pass_test() -> nil.
checker_field_is_email_pass_test() ->
    Checks = maps:from_list(
        [{<<"email"/utf8>>,
                {check, <<"email"/utf8>>, <<"Must be valid email"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"email"/utf8>>, gleam@json:string(<<"user@example.com"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 2982).
-spec checker_field_is_email_fail_test() -> nil.
checker_field_is_email_fail_test() ->
    Checks = maps:from_list(
        [{<<"email"/utf8>>,
                {check, <<"email"/utf8>>, <<"Must be valid email"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"email"/utf8>>, gleam@json:string(<<"not-an-email"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 2995).
-spec checker_field_is_uuid_pass_test() -> nil.
checker_field_is_uuid_pass_test() ->
    Checks = maps:from_list(
        [{<<"id"/utf8>>,
                {check, <<"uuid"/utf8>>, <<"Must be valid UUID"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"id"/utf8>>,
                gleam@json:string(
                    <<"550e8400-e29b-41d4-a716-446655440000"/utf8>>
                )}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3007).
-spec checker_field_is_uuid_fail_test() -> nil.
checker_field_is_uuid_fail_test() ->
    Checks = maps:from_list(
        [{<<"id"/utf8>>,
                {check, <<"uuid"/utf8>>, <<"Must be valid UUID"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(<<"not-a-uuid"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 3020).
-spec checker_field_is_iso8601_pass_test() -> nil.
checker_field_is_iso8601_pass_test() ->
    Checks = maps:from_list(
        [{<<"created_at"/utf8>>,
                {check,
                    <<"iso8601 datetime"/utf8>>,
                    <<"Must be valid datetime"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"created_at"/utf8>>,
                gleam@json:string(<<"2024-01-15T10:30:00Z"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3034).
-spec checker_field_integer_gte_pass_test() -> nil.
checker_field_integer_gte_pass_test() ->
    Checks = maps:from_list(
        [{<<"count"/utf8>>,
                {check, <<"integer >= 5"/utf8>>, <<"Must be at least 5"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object([{<<"count"/utf8>>, gleam@json:int(10)}]),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3046).
-spec checker_field_integer_gte_fail_test() -> nil.
checker_field_integer_gte_fail_test() ->
    Checks = maps:from_list(
        [{<<"count"/utf8>>,
                {check, <<"integer >= 5"/utf8>>, <<"Must be at least 5"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object([{<<"count"/utf8>>, gleam@json:int(3)}]),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3058).
-spec checker_field_integer_lte_pass_test() -> nil.
checker_field_integer_lte_pass_test() ->
    Checks = maps:from_list(
        [{<<"count"/utf8>>,
                {check,
                    <<"integer <= 100"/utf8>>,
                    <<"Must not exceed 100"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object([{<<"count"/utf8>>, gleam@json:int(50)}]),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3070).
-spec checker_field_number_between_pass_test() -> nil.
checker_field_number_between_pass_test() ->
    Checks = maps:from_list(
        [{<<"age"/utf8>>,
                {check,
                    <<"number between 18.0 and 65.0"/utf8>>,
                    <<"Age must be in range"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object([{<<"age"/utf8>>, gleam@json:int(30)}]),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3082).
-spec checker_field_number_between_fail_test() -> nil.
checker_field_number_between_fail_test() ->
    Checks = maps:from_list(
        [{<<"age"/utf8>>,
                {check,
                    <<"number between 18.0 and 65.0"/utf8>>,
                    <<"Age must be in range"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object([{<<"age"/utf8>>, gleam@json:int(17)}]),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3096).
-spec checker_string_starts_with_pass_test() -> nil.
checker_string_starts_with_pass_test() ->
    Checks = maps:from_list(
        [{<<"code"/utf8>>,
                {check,
                    <<"string starting with ERR-"/utf8>>,
                    <<"Error code format"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"code"/utf8>>, gleam@json:string(<<"ERR-001"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3108).
-spec checker_string_ends_with_pass_test() -> nil.
checker_string_ends_with_pass_test() ->
    Checks = maps:from_list(
        [{<<"file"/utf8>>,
                {check,
                    <<"string ending with .json"/utf8>>,
                    <<"Must be JSON file"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"file"/utf8>>, gleam@json:string(<<"config.json"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3120).
-spec checker_string_containing_pass_test() -> nil.
checker_string_containing_pass_test() ->
    Checks = maps:from_list(
        [{<<"message"/utf8>>,
                {check,
                    <<"string containing success"/utf8>>,
                    <<"Should mention success"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"message"/utf8>>,
                gleam@json:string(<<"Operation success complete"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3134).
-spec checker_non_empty_array_pass_test() -> nil.
checker_non_empty_array_pass_test() ->
    Checks = maps:from_list(
        [{<<"items"/utf8>>,
                {check, <<"non-empty array"/utf8>>, <<"Must have items"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"items"/utf8>>,
                gleam@json:array([gleam@json:int(1)], fun(X) -> X end)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3146).
-spec checker_non_empty_array_fail_test() -> nil.
checker_non_empty_array_fail_test() ->
    Checks = maps:from_list(
        [{<<"items"/utf8>>,
                {check, <<"non-empty array"/utf8>>, <<"Must have items"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"items"/utf8>>, gleam@json:array([], fun(X) -> X end)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3158).
-spec checker_array_of_length_pass_test() -> nil.
checker_array_of_length_pass_test() ->
    Checks = maps:from_list(
        [{<<"coords"/utf8>>,
                {check,
                    <<"array of length 3"/utf8>>,
                    <<"Must have 3 elements"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"coords"/utf8>>,
                gleam@json:array(
                    [gleam@json:int(1), gleam@json:int(2), gleam@json:int(3)],
                    fun(X) -> X end
                )}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3170).
-spec checker_array_min_items_pass_test() -> nil.
checker_array_min_items_pass_test() ->
    Checks = maps:from_list(
        [{<<"tags"/utf8>>,
                {check,
                    <<"array with min 2 items"/utf8>>,
                    <<"Need at least 2 tags"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"tags"/utf8>>,
                gleam@json:array(
                    [gleam@json:string(<<"a"/utf8>>),
                        gleam@json:string(<<"b"/utf8>>),
                        gleam@json:string(<<"c"/utf8>>)],
                    fun(X) -> X end
                )}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3184).
-spec checker_one_of_pass_test() -> nil.
checker_one_of_pass_test() ->
    Checks = maps:from_list(
        [{<<"status"/utf8>>,
                {check,
                    <<"one of [active, inactive, pending]"/utf8>>,
                    <<"Valid status"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"active"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3196).
-spec checker_one_of_fail_test() -> nil.
checker_one_of_fail_test() ->
    Checks = maps:from_list(
        [{<<"status"/utf8>>,
                {check,
                    <<"one of [active, inactive, pending]"/utf8>>,
                    <<"Valid status"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"unknown"/utf8>>)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3210).
-spec checker_header_present_pass_test() -> nil.
checker_header_present_pass_test() ->
    Expected = {response,
        200,
        gleam@json:null(),
        gleam@dict:new(),
        maps:from_list([{<<"Content-Type"/utf8>>, <<"application/json"/utf8>>}])},
    Actual = make_test_execution(
        200,
        gleam@json:null(),
        maps:from_list([{<<"Content-Type"/utf8>>, <<"application/json"/utf8>>}])
    ),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 0).

-file("test/intent_test.gleam", 3224).
-spec checker_header_value_mismatch_test() -> nil.
checker_header_value_mismatch_test() ->
    Expected = {response,
        200,
        gleam@json:null(),
        gleam@dict:new(),
        maps:from_list([{<<"Content-Type"/utf8>>, <<"application/json"/utf8>>}])},
    Actual = make_test_execution(
        200,
        gleam@json:null(),
        maps:from_list([{<<"Content-Type"/utf8>>, <<"text/html"/utf8>>}])
    ),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 3238).
-spec checker_header_missing_test() -> nil.
checker_header_missing_test() ->
    Expected = {response,
        200,
        gleam@json:null(),
        gleam@dict:new(),
        maps:from_list([{<<"X-Request-Id"/utf8>>, <<"abc-123"/utf8>>}])},
    Actual = make_test_execution(200, gleam@json:null(), gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 3252).
-spec checker_header_case_insensitive_test() -> nil.
checker_header_case_insensitive_test() ->
    Expected = {response,
        200,
        gleam@json:null(),
        gleam@dict:new(),
        maps:from_list([{<<"content-type"/utf8>>, <<"application/json"/utf8>>}])},
    Actual = make_test_execution(
        200,
        gleam@json:null(),
        maps:from_list([{<<"Content-Type"/utf8>>, <<"application/json"/utf8>>}])
    ),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3267).
-spec checker_nested_field_pass_test() -> nil.
checker_nested_field_pass_test() ->
    Checks = maps:from_list(
        [{<<"user.name"/utf8>>,
                {check, <<"equals John"/utf8>>, <<"User name must match"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"user"/utf8>>,
                gleam@json:object(
                    [{<<"name"/utf8>>, gleam@json:string(<<"John"/utf8>>)}]
                )}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3281).
-spec checker_nested_field_missing_test() -> nil.
checker_nested_field_missing_test() ->
    Checks = maps:from_list(
        [{<<"user.email"/utf8>>,
                {check, <<"is email"/utf8>>, <<"Must have email"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"user"/utf8>>,
                gleam@json:object(
                    [{<<"name"/utf8>>, gleam@json:string(<<"John"/utf8>>)}]
                )}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3297).
-spec checker_multiple_checks_test() -> nil.
checker_multiple_checks_test() ->
    Checks = maps:from_list(
        [{<<"id"/utf8>>, {check, <<"uuid"/utf8>>, <<"ID must be UUID"/utf8>>}},
            {<<"name"/utf8>>,
                {check, <<"non-empty string"/utf8>>, <<"Name required"/utf8>>}},
            {<<"email"/utf8>>,
                {check, <<"email"/utf8>>, <<"Email required"/utf8>>}},
            {<<"age"/utf8>>,
                {check,
                    <<"integer >= 0"/utf8>>,
                    <<"Age must be positive"/utf8>>}}]
    ),
    Expected = make_test_response(200, Checks),
    Body = gleam@json:object(
        [{<<"id"/utf8>>,
                gleam@json:string(
                    <<"550e8400-e29b-41d4-a716-446655440000"/utf8>>
                )},
            {<<"name"/utf8>>, gleam@json:string(<<"John"/utf8>>)},
            {<<"email"/utf8>>, gleam@json:string(<<"john@example.com"/utf8>>)},
            {<<"age"/utf8>>, gleam@json:int(30)}]
    ),
    Actual = make_test_execution(200, Body, gleam@dict:new()),
    Result = intent@checker:check_response(Expected, Actual, empty_context()),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 4),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 0).

-file("test/intent_test.gleam", 3323).
-spec make_test_question(binary(), integer(), binary()) -> intent@question_types:question().
make_test_question(Id, Round, Question_text) ->
    {question,
        Id,
        Round,
        user,
        happy_path,
        critical,
        Question_text,
        <<"Test context"/utf8>>,
        <<"Test example"/utf8>>,
        <<"text"/utf8>>,
        [],
        [],
        []}.

-file("test/intent_test.gleam", 3451).
-spec question_loader_file_not_found_test() -> nil.
question_loader_file_not_found_test() ->
    Result = intent@question_loader:load_custom_questions(
        <<"/nonexistent/path.cue"/utf8>>
    ),
    case Result of
        {error, {file_not_found, _}} ->
            gleeunit@should:be_true(true);

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 3490).
-spec merge_question_list_test(
    list(intent@question_types:question()),
    gleam@option:option(list(intent@question_types:question()))
) -> list(intent@question_types:question()).
merge_question_list_test(Base, Custom) ->
    case Custom of
        none ->
            Base;

        {some, Custom_questions} ->
            Custom_ids = gleam@list:map(
                Custom_questions,
                fun(Q) -> erlang:element(2, Q) end
            ),
            Filtered_base = gleam@list:filter(
                Base,
                fun(Q@1) ->
                    not gleam@list:contains(Custom_ids, erlang:element(2, Q@1))
                end
            ),
            lists:append(Filtered_base, Custom_questions)
    end.

-file("test/intent_test.gleam", 3463).
-spec merge_profile_test(
    intent@question_loader:profile_questions(),
    gleam@option:option(intent@question_loader:custom_profile_questions())
) -> intent@question_loader:profile_questions().
merge_profile_test(Base, Custom) ->
    case Custom of
        none ->
            Base;

        {some, C} ->
            {profile_questions,
                merge_question_list_test(
                    erlang:element(2, Base),
                    erlang:element(2, C)
                ),
                merge_question_list_test(
                    erlang:element(3, Base),
                    erlang:element(3, C)
                )}
    end.

-file("test/intent_test.gleam", 3340).
-spec question_loader_merge_empty_custom_returns_base_test() -> nil.
question_loader_merge_empty_custom_returns_base_test() ->
    Base = {profile_questions,
        [make_test_question(<<"q1"/utf8>>, 1, <<"Question 1"/utf8>>)],
        [make_test_question(<<"q2"/utf8>>, 2, <<"Question 2"/utf8>>)]},
    Custom = none,
    Result = merge_profile_test(Base, Custom),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1).

-file("test/intent_test.gleam", 3354).
-spec question_loader_merge_adds_new_questions_test() -> nil.
question_loader_merge_adds_new_questions_test() ->
    Base = {profile_questions,
        [make_test_question(<<"q1"/utf8>>, 1, <<"Question 1"/utf8>>)],
        []},
    Custom = {some,
        {custom_profile_questions,
            {some,
                [make_test_question(
                        <<"q-new"/utf8>>,
                        1,
                        <<"New Question"/utf8>>
                    )]},
            none}},
    Result = merge_profile_test(Base, Custom),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 2).

-file("test/intent_test.gleam", 3371).
-spec question_loader_merge_overrides_by_id_test() -> nil.
question_loader_merge_overrides_by_id_test() ->
    Base = {profile_questions,
        [make_test_question(<<"q1"/utf8>>, 1, <<"Original Question"/utf8>>)],
        []},
    Custom = {some,
        {custom_profile_questions,
            {some,
                [make_test_question(
                        <<"q1"/utf8>>,
                        1,
                        <<"Overridden Question"/utf8>>
                    )]},
            none}},
    Result = merge_profile_test(Base, Custom),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1),
    case erlang:element(2, Result) of
        [Q] ->
            _pipe@1 = erlang:element(7, Q),
            gleeunit_ffi:should_equal(_pipe@1, <<"Overridden Question"/utf8>>);

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 3394).
-spec question_loader_merge_preserves_non_overridden_test() -> nil.
question_loader_merge_preserves_non_overridden_test() ->
    Base = {profile_questions,
        [make_test_question(<<"q1"/utf8>>, 1, <<"Question 1"/utf8>>),
            make_test_question(<<"q2"/utf8>>, 1, <<"Question 2"/utf8>>),
            make_test_question(<<"q3"/utf8>>, 1, <<"Question 3"/utf8>>)],
        []},
    Custom = {some,
        {custom_profile_questions,
            {some,
                [make_test_question(<<"q2"/utf8>>, 1, <<"Overridden Q2"/utf8>>)]},
            none}},
    Result = merge_profile_test(Base, Custom),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 3),
    Overridden = gleam@list:filter(
        erlang:element(2, Result),
        fun(Q) -> erlang:element(2, Q) =:= <<"q2"/utf8>> end
    ),
    case Overridden of
        [Q@1] ->
            _pipe@1 = erlang:element(7, Q@1),
            gleeunit_ffi:should_equal(_pipe@1, <<"Overridden Q2"/utf8>>);

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 3476).
-spec merge_common_test(
    intent@question_loader:common_questions(),
    gleam@option:option(intent@question_loader:custom_common_questions())
) -> intent@question_loader:common_questions().
merge_common_test(Base, Custom) ->
    case Custom of
        none ->
            Base;

        {some, C} ->
            {common_questions,
                merge_question_list_test(
                    erlang:element(2, Base),
                    erlang:element(2, C)
                ),
                merge_question_list_test(
                    erlang:element(3, Base),
                    erlang:element(3, C)
                ),
                merge_question_list_test(
                    erlang:element(4, Base),
                    erlang:element(4, C)
                )}
    end.

-file("test/intent_test.gleam", 3422).
-spec question_loader_merge_common_rounds_test() -> nil.
question_loader_merge_common_rounds_test() ->
    Base = {common_questions,
        [make_test_question(<<"r3-q1"/utf8>>, 3, <<"Round 3 Q1"/utf8>>)],
        [make_test_question(<<"r4-q1"/utf8>>, 4, <<"Round 4 Q1"/utf8>>)],
        []},
    Custom = {some,
        {custom_common_questions,
            none,
            {some,
                [make_test_question(
                        <<"r4-q1"/utf8>>,
                        4,
                        <<"Overridden R4 Q1"/utf8>>
                    )]},
            {some,
                [make_test_question(
                        <<"r5-new"/utf8>>,
                        5,
                        <<"New Round 5 Q"/utf8>>
                    )]}}},
    Result = merge_common_test(Base, Custom),
    _pipe = erlang:length(erlang:element(2, Result)),
    gleeunit_ffi:should_equal(_pipe, 1),
    _pipe@1 = erlang:length(erlang:element(3, Result)),
    gleeunit_ffi:should_equal(_pipe@1, 1),
    case erlang:element(3, Result) of
        [Q] ->
            _pipe@2 = erlang:element(7, Q),
            gleeunit_ffi:should_equal(_pipe@2, <<"Overridden R4 Q1"/utf8>>);

        _ ->
            gleeunit@should:fail()
    end,
    _pipe@3 = erlang:length(erlang:element(4, Result)),
    gleeunit_ffi:should_equal(_pipe@3, 1).

-file("test/intent_test.gleam", 3509).
-spec make_test_interview_session(
    binary(),
    list(intent@interview:answer()),
    list(intent@interview:gap()),
    list(intent@interview:conflict()),
    intent@interview:interview_stage()
) -> intent@interview:interview_session().
make_test_interview_session(Id, Answers, Gaps, Conflicts, Stage) ->
    {interview_session,
        Id,
        api,
        <<"2024-01-01T10:00:00Z"/utf8>>,
        <<"2024-01-01T12:00:00Z"/utf8>>,
        <<""/utf8>>,
        Stage,
        1,
        Answers,
        Gaps,
        Conflicts,
        <<""/utf8>>}.

-file("test/intent_test.gleam", 3532).
-spec make_test_answer(binary(), binary()) -> intent@interview:answer().
make_test_answer(Question_id, Response) ->
    {answer,
        Question_id,
        <<"Test question for "/utf8, Question_id/binary>>,
        user,
        1,
        Response,
        gleam@dict:new(),
        0.9,
        <<""/utf8>>,
        <<"2024-01-01T10:30:00Z"/utf8>>}.

-file("test/intent_test.gleam", 3547).
-spec make_test_gap(binary(), boolean()) -> intent@interview:gap().
make_test_gap(Id, Resolved) ->
    {gap,
        Id,
        <<"test_field"/utf8>>,
        <<"Test gap"/utf8>>,
        true,
        <<""/utf8>>,
        <<"Test reason"/utf8>>,
        1,
        Resolved,
        <<""/utf8>>}.

-file("test/intent_test.gleam", 3562).
-spec make_test_conflict(binary(), integer()) -> intent@interview:conflict().
make_test_conflict(Id, Chosen) ->
    {conflict,
        Id,
        {<<"answer1"/utf8>>, <<"answer2"/utf8>>},
        <<"Test conflict"/utf8>>,
        <<"High"/utf8>>,
        [],
        Chosen}.

-file("test/intent_test.gleam", 3573).
-spec diff_sessions_no_changes_test() -> nil.
diff_sessions_no_changes_test() ->
    Session = make_test_interview_session(
        <<"session-1"/utf8>>,
        [make_test_answer(<<"q1"/utf8>>, <<"Answer 1"/utf8>>)],
        [],
        [],
        discovery
    ),
    Diff = intent@interview_storage:diff_sessions(Session, Session),
    _pipe = erlang:length(erlang:element(6, Diff)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(7, Diff)),
    gleeunit_ffi:should_equal(_pipe@1, 0),
    _pipe@2 = erlang:length(erlang:element(8, Diff)),
    gleeunit_ffi:should_equal(_pipe@2, 0),
    _pipe@3 = erlang:element(13, Diff),
    gleeunit_ffi:should_equal(_pipe@3, none).

-file("test/intent_test.gleam", 3590).
-spec diff_sessions_answer_added_test() -> nil.
diff_sessions_answer_added_test() ->
    Session1 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [make_test_answer(<<"q1"/utf8>>, <<"Answer 1"/utf8>>)],
        [],
        [],
        discovery
    ),
    Session2 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [make_test_answer(<<"q1"/utf8>>, <<"Answer 1"/utf8>>),
            make_test_answer(<<"q2"/utf8>>, <<"Answer 2"/utf8>>)],
        [],
        [],
        discovery
    ),
    Diff = intent@interview_storage:diff_sessions(Session1, Session2),
    _pipe = erlang:length(erlang:element(6, Diff)),
    gleeunit_ffi:should_equal(_pipe, 1),
    _pipe@1 = erlang:length(erlang:element(7, Diff)),
    gleeunit_ffi:should_equal(_pipe@1, 0),
    _pipe@2 = erlang:length(erlang:element(8, Diff)),
    gleeunit_ffi:should_equal(_pipe@2, 0).

-file("test/intent_test.gleam", 3617).
-spec diff_sessions_answer_modified_test() -> nil.
diff_sessions_answer_modified_test() ->
    Session1 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [make_test_answer(<<"q1"/utf8>>, <<"Original answer"/utf8>>)],
        [],
        [],
        discovery
    ),
    Session2 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [make_test_answer(<<"q1"/utf8>>, <<"Modified answer"/utf8>>)],
        [],
        [],
        discovery
    ),
    Diff = intent@interview_storage:diff_sessions(Session1, Session2),
    _pipe = erlang:length(erlang:element(6, Diff)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(7, Diff)),
    gleeunit_ffi:should_equal(_pipe@1, 1),
    _pipe@2 = erlang:length(erlang:element(8, Diff)),
    gleeunit_ffi:should_equal(_pipe@2, 0),
    case erlang:element(7, Diff) of
        [Modified] ->
            _pipe@3 = erlang:element(2, Modified),
            gleeunit_ffi:should_equal(_pipe@3, <<"q1"/utf8>>),
            _pipe@4 = erlang:element(4, Modified),
            gleeunit_ffi:should_equal(
                _pipe@4,
                {some, <<"Original answer"/utf8>>}
            ),
            _pipe@5 = erlang:element(5, Modified),
            gleeunit_ffi:should_equal(_pipe@5, <<"Modified answer"/utf8>>);

        _ ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 3651).
-spec diff_sessions_answer_removed_test() -> nil.
diff_sessions_answer_removed_test() ->
    Session1 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [make_test_answer(<<"q1"/utf8>>, <<"Answer 1"/utf8>>),
            make_test_answer(<<"q2"/utf8>>, <<"Answer 2"/utf8>>)],
        [],
        [],
        discovery
    ),
    Session2 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [make_test_answer(<<"q1"/utf8>>, <<"Answer 1"/utf8>>)],
        [],
        [],
        discovery
    ),
    Diff = intent@interview_storage:diff_sessions(Session1, Session2),
    _pipe = erlang:length(erlang:element(6, Diff)),
    gleeunit_ffi:should_equal(_pipe, 0),
    _pipe@1 = erlang:length(erlang:element(7, Diff)),
    gleeunit_ffi:should_equal(_pipe@1, 0),
    _pipe@2 = erlang:length(erlang:element(8, Diff)),
    gleeunit_ffi:should_equal(_pipe@2, 1),
    _pipe@3 = erlang:element(8, Diff),
    gleeunit_ffi:should_equal(_pipe@3, [<<"q2"/utf8>>]).

-file("test/intent_test.gleam", 3679).
-spec diff_sessions_stage_changed_test() -> nil.
diff_sessions_stage_changed_test() ->
    Session1 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [],
        [],
        [],
        discovery
    ),
    Session2 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [],
        [],
        [],
        refinement
    ),
    Diff = intent@interview_storage:diff_sessions(Session1, Session2),
    case erlang:element(13, Diff) of
        {some, {From, To}} ->
            _pipe = From,
            gleeunit_ffi:should_equal(_pipe, <<"discovery"/utf8>>),
            _pipe@1 = To,
            gleeunit_ffi:should_equal(_pipe@1, <<"refinement"/utf8>>);

        none ->
            gleeunit@should:fail()
    end.

-file("test/intent_test.gleam", 3707).
-spec diff_sessions_gaps_resolved_test() -> nil.
diff_sessions_gaps_resolved_test() ->
    Session1 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [],
        [make_test_gap(<<"gap1"/utf8>>, false),
            make_test_gap(<<"gap2"/utf8>>, false)],
        [],
        discovery
    ),
    Session2 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [],
        [make_test_gap(<<"gap1"/utf8>>, true),
            make_test_gap(<<"gap2"/utf8>>, false)],
        [],
        discovery
    ),
    Diff = intent@interview_storage:diff_sessions(Session1, Session2),
    _pipe = erlang:element(10, Diff),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3729).
-spec diff_sessions_conflicts_resolved_test() -> nil.
diff_sessions_conflicts_resolved_test() ->
    Session1 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [],
        [],
        [make_test_conflict(<<"c1"/utf8>>, -1)],
        discovery
    ),
    Session2 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [],
        [],
        [make_test_conflict(<<"c1"/utf8>>, 0)],
        discovery
    ),
    Diff = intent@interview_storage:diff_sessions(Session1, Session2),
    _pipe = erlang:element(12, Diff),
    gleeunit_ffi:should_equal(_pipe, 1).

-file("test/intent_test.gleam", 3751).
-spec create_snapshot_test() -> nil.
create_snapshot_test() ->
    Session = make_test_interview_session(
        <<"session-1"/utf8>>,
        [make_test_answer(<<"q1"/utf8>>, <<"Answer 1"/utf8>>),
            make_test_answer(<<"q2"/utf8>>, <<"Answer 2"/utf8>>)],
        [make_test_gap(<<"gap1"/utf8>>, false)],
        [],
        discovery
    ),
    Snapshot = intent@interview_storage:create_snapshot(
        Session,
        <<"Test snapshot"/utf8>>
    ),
    _pipe = erlang:element(2, Snapshot),
    gleeunit_ffi:should_equal(_pipe, <<"session-1"/utf8>>),
    _pipe@1 = erlang:element(5, Snapshot),
    gleeunit_ffi:should_equal(_pipe@1, <<"Test snapshot"/utf8>>),
    _pipe@2 = erlang:element(7, Snapshot),
    gleeunit_ffi:should_equal(_pipe@2, 1),
    _pipe@3 = maps:size(erlang:element(6, Snapshot)),
    gleeunit_ffi:should_equal(_pipe@3, 2).

-file("test/intent_test.gleam", 3771).
-spec format_diff_produces_output_test() -> nil.
format_diff_produces_output_test() ->
    Session1 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [make_test_answer(<<"q1"/utf8>>, <<"Original"/utf8>>)],
        [],
        [],
        discovery
    ),
    Session2 = make_test_interview_session(
        <<"session-1"/utf8>>,
        [make_test_answer(<<"q1"/utf8>>, <<"Modified"/utf8>>),
            make_test_answer(<<"q2"/utf8>>, <<"New answer"/utf8>>)],
        [],
        [],
        refinement
    ),
    Diff = intent@interview_storage:diff_sessions(Session1, Session2),
    Formatted = intent@interview_storage:format_diff(Diff),
    _pipe = gleam_stdlib:contains_string(Formatted, <<"Session Diff:"/utf8>>),
    gleeunit@should:be_true(_pipe),
    _pipe@1 = gleam_stdlib:contains_string(Formatted, <<"Answers Added"/utf8>>),
    gleeunit@should:be_true(_pipe@1),
    _pipe@2 = gleam_stdlib:contains_string(
        Formatted,
        <<"Answers Modified"/utf8>>
    ),
    gleeunit@should:be_true(_pipe@2),
    _pipe@3 = gleam_stdlib:contains_string(Formatted, <<"Stage:"/utf8>>),
    gleeunit@should:be_true(_pipe@3).
