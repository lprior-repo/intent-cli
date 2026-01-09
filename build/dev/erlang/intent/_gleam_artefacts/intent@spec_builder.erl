-module(intent@spec_builder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/spec_builder.gleam").
-export([build_light_spec_from_session/1, extract_features_from_answers/1, extract_behaviors_from_answers/2, extract_constraints_from_answers/1, extract_security_requirements/1, extract_non_functional_requirements/1, build_spec_from_session/1, build_spec_from_session_with_analysis/1, create_test_spec/1, check_many/3]).
-export_type([generated_c_u_e/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type generated_c_u_e() :: {generated_c_u_e, binary(), list(binary()), binary()}.

-file("src/intent/spec_builder.gleam", 56).
?DOC(" Extract name from answers (first answer typically contains intent/name)\n").
-spec extract_name_from_answers(list(intent@interview:answer())) -> binary().
extract_name_from_answers(Answers) ->
    _pipe = Answers,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"name"/utf8>>,
                    <<"what"/utf8>>,
                    <<"api"/utf8>>,
                    <<"command"/utf8>>,
                    <<"main"/utf8>>]
            )
        end
    ),
    _pipe@2 = gleam@list:first(_pipe@1),
    _pipe@3 = gleam@result:map(
        _pipe@2,
        fun(A) -> gleam@string:trim(erlang:element(6, A)) end
    ),
    gleam@result:unwrap(_pipe@3, <<"Untitled Specification"/utf8>>).

-file("src/intent/spec_builder.gleam", 67).
?DOC(" Extract description from answers\n").
-spec extract_description_from_answers(list(intent@interview:answer())) -> binary().
extract_description_from_answers(Answers) ->
    _pipe = Answers,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"description"/utf8>>,
                    <<"what"/utf8>>,
                    <<"accomplish"/utf8>>,
                    <<"purpose"/utf8>>]
            )
        end
    ),
    _pipe@2 = gleam@list:first(_pipe@1),
    _pipe@3 = gleam@result:map(
        _pipe@2,
        fun(A) -> gleam@string:trim(erlang:element(6, A)) end
    ),
    gleam@result:unwrap(_pipe@3, <<"Light specification from interview"/utf8>>).

-file("src/intent/spec_builder.gleam", 208).
?DOC(" Escape strings for CUE output\n").
-spec escape_cue_string(binary()) -> binary().
escape_cue_string(S) ->
    _pipe = S,
    _pipe@1 = gleam@string:replace(_pipe, <<"\\"/utf8>>, <<"\\\\"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"\""/utf8>>, <<"\\\""/utf8>>),
    gleam@string:replace(_pipe@2, <<"\n"/utf8>>, <<"\\n"/utf8>>).

-file("src/intent/spec_builder.gleam", 78).
?DOC(" Extract behaviors in LightBehavior format from answers\n").
-spec extract_light_behaviors_from_answers(list(intent@interview:answer())) -> binary().
extract_light_behaviors_from_answers(Answers) ->
    _pipe = Answers,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                [<<"flow"/utf8>>,
                    <<"step"/utf8>>,
                    <<"behavior"/utf8>>,
                    <<"happen"/utf8>>]
            )
        end
    ),
    _pipe@7 = gleam@list:flat_map(
        _pipe@1,
        fun(Answer@1) ->
            Response = gleam@string:trim(erlang:element(6, Answer@1)),
            case gleam_stdlib:contains_string(Response, <<"→"/utf8>>) of
                true ->
                    _pipe@2 = Response,
                    _pipe@3 = gleam@string:split(_pipe@2, <<"→"/utf8>>),
                    gleam@list:map(_pipe@3, fun gleam@string:trim/1);

                false ->
                    case gleam_stdlib:contains_string(Response, <<"\n"/utf8>>) of
                        true ->
                            _pipe@4 = Response,
                            _pipe@5 = gleam@string:split(_pipe@4, <<"\n"/utf8>>),
                            _pipe@6 = gleam@list:map(
                                _pipe@5,
                                fun gleam@string:trim/1
                            ),
                            gleam@list:filter(
                                _pipe@6,
                                fun(S) -> gleam@string:length(S) > 0 end
                            );

                        false ->
                            [Response]
                    end
            end
        end
    ),
    _pipe@8 = gleam@list:index_map(
        _pipe@7,
        fun(Step, Idx) ->
            Behavior_name = <<"step_"/utf8,
                (gleam@int:to_string(Idx + 1))/binary>>,
            <<<<<<<<<<<<<<<<<<<<<<<<<<<<"\t{\n"/utf8, "\t\tname: \""/utf8>>/binary,
                                                                (escape_cue_string(
                                                                    Behavior_name
                                                                ))/binary>>/binary,
                                                            "\"\n"/utf8>>/binary,
                                                        "\t\tintent: \""/utf8>>/binary,
                                                    (escape_cue_string(Step))/binary>>/binary,
                                                "\"\n"/utf8>>/binary,
                                            "\t\trequest: {\n"/utf8>>/binary,
                                        "\t\t\tmethod: \"GET\"\n"/utf8>>/binary,
                                    "\t\t\tpath: \"/\"\n"/utf8>>/binary,
                                "\t\t}\n"/utf8>>/binary,
                            "\t\tresponse: {\n"/utf8>>/binary,
                        "\t\t\tstatus: 200\n"/utf8>>/binary,
                    "\t\t}\n"/utf8>>/binary,
                "\t},\n"/utf8>>
        end
    ),
    gleam@string:concat(_pipe@8).

-file("src/intent/spec_builder.gleam", 39).
?DOC(
    " Build a LightSpec CUE from a light-mode interview session\n"
    " Used when --light flag is set (simpler output, fewer questions)\n"
).
-spec build_light_spec_from_session(intent@interview:interview_session()) -> binary().
build_light_spec_from_session(Session) ->
    Name = extract_name_from_answers(erlang:element(9, Session)),
    Description = extract_description_from_answers(erlang:element(9, Session)),
    Behaviors = extract_light_behaviors_from_answers(erlang:element(9, Session)),
    <<<<<<<<<<<<<<<<<<<<<<<<<<"// LightSpec: Minimal specification generated from light-mode interview\n"/utf8,
                                                        "// Session: "/utf8>>/binary,
                                                    (erlang:element(2, Session))/binary>>/binary,
                                                "\n"/utf8>>/binary,
                                            "package api\n\n"/utf8>>/binary,
                                        "name: \""/utf8>>/binary,
                                    (escape_cue_string(Name))/binary>>/binary,
                                "\"\n"/utf8>>/binary,
                            "description: \""/utf8>>/binary,
                        (escape_cue_string(Description))/binary>>/binary,
                    "\"\n\n"/utf8>>/binary,
                "behaviors: [\n"/utf8>>/binary,
            Behaviors/binary>>/binary,
        "]\n"/utf8>>.

-file("src/intent/spec_builder.gleam", 176).
?DOC(" Extract inversion points from error/security related answers\n").
-spec extract_inversions_from_answers(list(intent@interview:answer())) -> binary().
extract_inversions_from_answers(Answers) ->
    Inversion_keywords = [<<"wrong"/utf8>>,
        <<"fail"/utf8>>,
        <<"error"/utf8>>,
        <<"problem"/utf8>>,
        <<"security"/utf8>>,
        <<"attack"/utf8>>,
        <<"vulnerability"/utf8>>],
    _pipe = Answers,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                Inversion_keywords
            )
            orelse intent@case_insensitive:contains_any_ignore_case(
                erlang:element(6, Answer),
                Inversion_keywords
            )
        end
    ),
    _pipe@2 = gleam@list:map(
        _pipe@1,
        fun(Answer@1) ->
            Escaped = escape_cue_string(
                gleam@string:trim(erlang:element(6, Answer@1))
            ),
            <<<<"\t\t\""/utf8, Escaped/binary>>/binary, "\",\n"/utf8>>
        end
    ),
    gleam@string:concat(_pipe@2).

-file("src/intent/spec_builder.gleam", 192).
?DOC(" Extract pre-mortem scenarios from answers\n").
-spec extract_premortem_from_answers(list(intent@interview:answer())) -> binary().
extract_premortem_from_answers(Answers) ->
    Premortem_keywords = [<<"deadline"/utf8>>,
        <<"late"/utf8>>,
        <<"missing"/utf8>>,
        <<"blocked"/utf8>>,
        <<"dependency"/utf8>>,
        <<"risk"/utf8>>,
        <<"scale"/utf8>>],
    _pipe = Answers,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Answer) ->
            intent@case_insensitive:contains_any_ignore_case(
                erlang:element(3, Answer),
                Premortem_keywords
            )
            orelse intent@case_insensitive:contains_any_ignore_case(
                erlang:element(6, Answer),
                Premortem_keywords
            )
        end
    ),
    _pipe@2 = gleam@list:map(
        _pipe@1,
        fun(Answer@1) ->
            Escaped = escape_cue_string(
                gleam@string:trim(erlang:element(6, Answer@1))
            ),
            <<<<"\t\t\""/utf8, Escaped/binary>>/binary, "\",\n"/utf8>>
        end
    ),
    gleam@string:concat(_pipe@2).

-file("src/intent/spec_builder.gleam", 156).
?DOC(" Build KIRK analysis section from interview answers\n").
-spec build_kirk_analysis_section(list(intent@interview:answer())) -> binary().
build_kirk_analysis_section(Answers) ->
    Inversions = extract_inversions_from_answers(Answers),
    Pre_mortem = extract_premortem_from_answers(Answers),
    <<<<<<<<<<<<<<<<"\n// KIRK Analysis (generated with --with-analysis)\n"/utf8,
                                    "kirk: {\n"/utf8>>/binary,
                                "\tinversions: [\n"/utf8>>/binary,
                            Inversions/binary>>/binary,
                        "\t]\n"/utf8>>/binary,
                    "\tpre_mortem: [\n"/utf8>>/binary,
                Pre_mortem/binary>>/binary,
            "\t]\n"/utf8>>/binary,
        "}\n"/utf8>>.

-file("src/intent/spec_builder.gleam", 216).
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

-file("src/intent/spec_builder.gleam", 232).
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

-file("src/intent/spec_builder.gleam", 262).
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

-file("src/intent/spec_builder.gleam", 274).
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

-file("src/intent/spec_builder.gleam", 300).
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

-file("src/intent/spec_builder.gleam", 312).
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

-file("src/intent/spec_builder.gleam", 121).
-spec build_spec_from_session_impl(
    intent@interview:interview_session(),
    boolean()
) -> binary().
build_spec_from_session_impl(Session, Include_analysis) ->
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
    Body = build_spec_body(
        Features,
        Behaviors,
        Constraints,
        Security,
        Non_functional
    ),
    Full_body = case Include_analysis of
        true ->
            <<<<Body/binary, "\n"/utf8>>/binary,
                (build_kirk_analysis_section(erlang:element(9, Session)))/binary>>;

        false ->
            Body
    end,
    Spec = {generated_c_u_e, <<"package api"/utf8>>, [], Full_body},
    <<<<(erlang:element(2, Spec))/binary, "\n\n"/utf8>>/binary,
        (erlang:element(4, Spec))/binary>>.

-file("src/intent/spec_builder.gleam", 27).
?DOC(" Build a CUE spec from a completed interview session\n").
-spec build_spec_from_session(intent@interview:interview_session()) -> binary().
build_spec_from_session(Session) ->
    build_spec_from_session_impl(Session, false).

-file("src/intent/spec_builder.gleam", 33).
?DOC(
    " Build a CUE spec with KIRK analysis fields (inversions, pre_mortem)\n"
    " Used when --with-analysis flag is set\n"
).
-spec build_spec_from_session_with_analysis(
    intent@interview:interview_session()
) -> binary().
build_spec_from_session_with_analysis(Session) ->
    build_spec_from_session_impl(Session, true).

-file("src/intent/spec_builder.gleam", 385).
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

-file("src/intent/spec_builder.gleam", 372).
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
            [],
            none}}.

-file("src/intent/spec_builder.gleam", 392).
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
