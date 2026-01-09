-module(intent@spec_builder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/spec_builder.gleam").
-export([extract_features_from_answers/1, extract_behaviors_from_answers/2, extract_constraints_from_answers/1, extract_security_requirements/1, extract_non_functional_requirements/1, build_spec_from_session/1, create_test_spec/1, check_many/3]).
-export_type([generated_c_u_e/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type generated_c_u_e() :: {generated_c_u_e, binary(), list(binary()), binary()}.

-file("src/intent/spec_builder.gleam", 50).
?DOC(" Extract feature names/titles from answers\n").
-spec extract_features_from_answers(list(intent@interview:answer())) -> list(binary()).
extract_features_from_answers(Answers) ->
    _pipe = Answers,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"feature"/utf8>>, <<"capability"/utf8>>]
            )
        end
    ),
    _pipe@2 = gleam@list:map(
        _pipe@1,
        fun(Answer@1) ->
            Trimmed = gleam@string:trim(erlang:element(6, Answer@1)),
            case gleam@string:length(Trimmed) > 0 of
                true ->
                    Trimmed;

                false ->
                    <<""/utf8>>
            end
        end
    ),
    gleam@list:filter(_pipe@2, fun(S) -> S /= <<""/utf8>> end).

-file("src/intent/spec_builder.gleam", 66).
?DOC(" Extract API behaviors (methods, paths, status codes)\n").
-spec extract_behaviors_from_answers(
    list(intent@interview:answer()),
    intent@interview:profile()
) -> binary().
extract_behaviors_from_answers(Answers, _) ->
    Api_answers = gleam@list:filter(
        Answers,
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"endpoint"/utf8>>, <<"path"/utf8>>, <<"method"/utf8>>]
            )
        end
    ),
    case Api_answers of
        [] ->
            <<"// Define API behaviors here
behaviors: {
  // Add endpoint definitions
}"/utf8>>;

        Answers@1 ->
            <<<<"// API behaviors from interview
behaviors: {
"/utf8,
                    (begin
                        _pipe = gleam@list:map(
                            Answers@1,
                            fun(Answer@1) ->
                                <<<<<<"  // "/utf8,
                                            (erlang:element(3, Answer@1))/binary>>/binary,
                                        "
  // "/utf8>>/binary,
                                    (gleam@string:trim(
                                        erlang:element(6, Answer@1)
                                    ))/binary>>
                            end
                        ),
                        gleam@string:join(_pipe, <<"\n"/utf8>>)
                    end)/binary>>/binary,
                "\n}"/utf8>>
    end.

-file("src/intent/spec_builder.gleam", 96).
?DOC(" Extract constraints from answers\n").
-spec extract_constraints_from_answers(list(intent@interview:answer())) -> list(binary()).
extract_constraints_from_answers(Answers) ->
    _pipe = Answers,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"constraint"/utf8>>,
                    <<"limit"/utf8>>,
                    <<"requirement"/utf8>>]
            )
        end
    ),
    _pipe@2 = gleam@list:map(
        _pipe@1,
        fun(Answer@1) -> gleam@string:trim(erlang:element(6, Answer@1)) end
    ),
    gleam@list:filter(_pipe@2, fun(S) -> S /= <<""/utf8>> end).

-file("src/intent/spec_builder.gleam", 108).
?DOC(" Extract security requirements from answers\n").
-spec extract_security_requirements(list(intent@interview:answer())) -> binary().
extract_security_requirements(Answers) ->
    Security_answers = gleam@list:filter(
        Answers,
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"auth"/utf8>>, <<"security"/utf8>>, <<"permission"/utf8>>]
            )
        end
    ),
    case Security_answers of
        [] ->
            <<"security: {
  authentication: \"todo\"
  authorization: \"todo\"
}"/utf8>>;

        Answers@1 ->
            <<<<"security: {
"/utf8,
                    (begin
                        _pipe = gleam@list:map(
                            Answers@1,
                            fun(Answer@1) ->
                                <<<<<<<<"  // "/utf8,
                                                (erlang:element(3, Answer@1))/binary>>/binary,
                                            "
  requirement: \""/utf8>>/binary,
                                        (gleam@string:trim(
                                            erlang:element(6, Answer@1)
                                        ))/binary>>/binary,
                                    "\""/utf8>>
                            end
                        ),
                        gleam@string:join(_pipe, <<"\n"/utf8>>)
                    end)/binary>>/binary,
                "\n}"/utf8>>
    end.

-file("src/intent/spec_builder.gleam", 134).
?DOC(" Extract non-functional requirements (SLA, scale, monitoring)\n").
-spec extract_non_functional_requirements(list(intent@interview:answer())) -> list(binary()).
extract_non_functional_requirements(Answers) ->
    _pipe = Answers,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"sla"/utf8>>,
                    <<"scale"/utf8>>,
                    <<"performance"/utf8>>,
                    <<"monitoring"/utf8>>,
                    <<"latency"/utf8>>]
            )
        end
    ),
    _pipe@2 = gleam@list:map(
        _pipe@1,
        fun(Answer@1) -> gleam@string:trim(erlang:element(6, Answer@1)) end
    ),
    gleam@list:filter(_pipe@2, fun(S) -> S /= <<""/utf8>> end).

-file("src/intent/spec_builder.gleam", 146).
?DOC(" Build the main body of the spec\n").
-spec build_spec_body(
    list(binary()),
    binary(),
    list(binary()),
    binary(),
    list(binary())
) -> binary().
build_spec_body(Features, Behaviors, Constraints, Security, Non_functional) ->
    Features_section = case Features of
        [] ->
            <<"// Features
features: {
  // Add feature definitions
}"/utf8>>;

        Features@1 ->
            <<<<"// Features extracted from interview
features: {
"/utf8,
                    (begin
                        _pipe = gleam@list:map(
                            Features@1,
                            fun(Feature) ->
                                <<<<"  \""/utf8, Feature/binary>>/binary,
                                    "\": true"/utf8>>
                            end
                        ),
                        gleam@string:join(_pipe, <<"\n"/utf8>>)
                    end)/binary>>/binary,
                "\n}"/utf8>>
    end,
    Constraints_section = case Constraints of
        [] ->
            <<""/utf8>>;

        Constraints@1 ->
            <<<<"\n\n// Constraints and requirements
constraints: {
"/utf8,
                    (begin
                        _pipe@1 = gleam@list:map(
                            Constraints@1,
                            fun(Constraint) ->
                                <<"  // "/utf8, Constraint/binary>>
                            end
                        ),
                        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
                    end)/binary>>/binary,
                "\n}"/utf8>>
    end,
    Non_functional_section = case Non_functional of
        [] ->
            <<""/utf8>>;

        Nf ->
            <<<<"\n\n// Non-functional requirements
nonFunctional: {
"/utf8,
                    (begin
                        _pipe@2 = gleam@list:map(
                            Nf,
                            fun(Requirement) ->
                                <<"  // "/utf8, Requirement/binary>>
                            end
                        ),
                        gleam@string:join(_pipe@2, <<"\n"/utf8>>)
                    end)/binary>>/binary,
                "\n}"/utf8>>
    end,
    <<<<<<<<<<<<Features_section/binary, "\n\n"/utf8>>/binary,
                        Behaviors/binary>>/binary,
                    "\n\n"/utf8>>/binary,
                Security/binary>>/binary,
            Constraints_section/binary>>/binary,
        Non_functional_section/binary>>.

-file("src/intent/spec_builder.gleam", 25).
?DOC(" Build a CUE spec from a completed interview session\n").
-spec build_spec_from_session(intent@interview:interview_session()) -> binary().
build_spec_from_session(Session) ->
    Features = extract_features_from_answers(erlang:element(9, Session)),
    Behaviors = extract_behaviors_from_answers(
        erlang:element(9, Session),
        erlang:element(3, Session)
    ),
    Constraints = extract_constraints_from_answers(erlang:element(9, Session)),
    Security = extract_security_requirements(erlang:element(9, Session)),
    Non_functional = extract_non_functional_requirements(
        erlang:element(9, Session)
    ),
    Spec = {generated_c_u_e,
        <<"package api"/utf8>>,
        [],
        build_spec_body(
            Features,
            Behaviors,
            Constraints,
            Security,
            Non_functional
        )},
    <<<<(erlang:element(2, Spec))/binary, "\n\n"/utf8>>/binary,
        (erlang:element(4, Spec))/binary>>.

-file("src/intent/spec_builder.gleam", 219).
-spec make_behavior(binary()) -> intent@types:behavior().
make_behavior(Name) ->
    {behavior,
        Name,
        <<"test"/utf8>>,
        <<""/utf8>>,
        [],
        [],
        {request,
            get,
            <<"/"/utf8>>,
            gleam@dict:new(),
            gleam@dict:new(),
            gleam@json:null()},
        {response, 200, gleam@json:null(), gleam@dict:new(), gleam@dict:new()},
        gleam@dict:new()}.

-file("src/intent/spec_builder.gleam", 206).
?DOC(" Create a test spec with N behaviors - pure functional composition\n").
-spec create_test_spec(integer()) -> intent@types:spec().
create_test_spec(Behavior_count) ->
    Behaviors = begin
        _pipe = gleam@list:range(1, Behavior_count),
        gleam@list:map(
            _pipe,
            fun(I) ->
                make_behavior(<<"b"/utf8, (gleam@int:to_string(I))/binary>>)
            end
        )
    end,
    {spec,
        <<"test"/utf8>>,
        <<"test"/utf8>>,
        <<"test"/utf8>>,
        <<"1.0.0"/utf8>>,
        [],
        {config, <<"http://test"/utf8>>, 1000, gleam@dict:new()},
        [{feature, <<"test-feature"/utf8>>, <<"test"/utf8>>, Behaviors}],
        [],
        [],
        {a_i_hints,
            {implementation_hints, []},
            gleam@dict:new(),
            {security_hints, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>},
            []}}.

-file("src/intent/spec_builder.gleam", 226).
?DOC(" Batch check behaviors against results - pure map operation\n").
-spec check_many(
    list(intent@types:behavior()),
    list(intent@http_client:execution_result()),
    intent@interpolate:context()
) -> list(intent@checker:response_check_result()).
check_many(Behaviors, Results, Ctx) ->
    gleam@list:map2(
        Behaviors,
        Results,
        fun(B, R) ->
            intent@checker:check_response(erlang:element(8, B), R, Ctx)
        end
    ).
