-module(intent).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent.gleam").
-export([main/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/intent.gleam", 278).
-spec print_spec_summary(intent@types:spec()) -> nil.
print_spec_summary(Spec) ->
    gleam@io:println(<<"Spec: "/utf8, (erlang:element(2, Spec))/binary>>),
    gleam@io:println(<<"Version: "/utf8, (erlang:element(5, Spec))/binary>>),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"Description:"/utf8>>),
    gleam@io:println(erlang:element(3, Spec)),
    gleam@io:println(<<""/utf8>>),
    case erlang:element(4, Spec) of
        <<""/utf8>> ->
            nil;

        Audience ->
            gleam@io:println(<<"Audience: "/utf8, Audience/binary>>),
            gleam@io:println(<<""/utf8>>)
    end,
    case erlang:element(6, Spec) of
        [] ->
            nil;

        Criteria ->
            gleam@io:println(<<"Success Criteria:"/utf8>>),
            gleam@list:each(
                Criteria,
                fun(C) -> gleam@io:println(<<"  - "/utf8, C/binary>>) end
            ),
            gleam@io:println(<<""/utf8>>)
    end,
    gleam@io:println(<<"Features:"/utf8>>),
    gleam@list:each(
        erlang:element(8, Spec),
        fun(Feature) ->
            gleam@io:println(<<"  "/utf8, (erlang:element(2, Feature))/binary>>),
            gleam@io:println(
                <<"    "/utf8, (erlang:element(3, Feature))/binary>>
            ),
            gleam@io:println(
                <<"    Behaviors: "/utf8,
                    (gleam@string:inspect(
                        erlang:length(erlang:element(4, Feature))
                    ))/binary>>
            ),
            gleam@list:each(
                erlang:element(4, Feature),
                fun(B) ->
                    gleam@io:println(
                        <<<<<<"      - "/utf8, (erlang:element(2, B))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (erlang:element(3, B))/binary>>
                    )
                end
            )
        end
    ),
    case erlang:element(9, Spec) of
        [] ->
            nil;

        Rules ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"Global Rules:"/utf8>>),
            gleam@list:each(
                Rules,
                fun(Rule) ->
                    gleam@io:println(
                        <<<<<<"  - "/utf8, (erlang:element(2, Rule))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (erlang:element(3, Rule))/binary>>
                    )
                end
            )
    end,
    case erlang:element(10, Spec) of
        [] ->
            nil;

        Patterns ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"Anti-Patterns:"/utf8>>),
            gleam@list:each(
                Patterns,
                fun(P) ->
                    gleam@io:println(
                        <<<<<<"  - "/utf8, (erlang:element(2, P))/binary>>/binary,
                                ": "/utf8>>/binary,
                            (erlang:element(3, P))/binary>>
                    )
                end
            )
    end,
    nil.

-file("src/intent.gleam", 388).
?DOC(" Export spec as JSON (original behavior)\n").
-spec export_as_json(binary()) -> {ok, binary()} | {error, binary()}.
export_as_json(Spec_path) ->
    case intent@loader:export_spec_json(Spec_path) of
        {ok, Json_str} ->
            {ok, Json_str};

        {error, E} ->
            {error, intent@loader:format_error(E)}
    end.

-file("src/intent.gleam", 1272).
?DOC(" Helper: convert Profile to string for questions module\n").
-spec profile_to_string(intent@interview:profile()) -> binary().
profile_to_string(Profile) ->
    case Profile of
        api ->
            <<"api"/utf8>>;

        cli ->
            <<"cli"/utf8>>;

        event ->
            <<"event"/utf8>>;

        data ->
            <<"data"/utf8>>;

        workflow ->
            <<"workflow"/utf8>>;

        u_i ->
            <<"ui"/utf8>>
    end.

-file("src/intent.gleam", 1283).
-spec profile_to_display_string(intent@interview:profile()) -> binary().
profile_to_display_string(Profile) ->
    case Profile of
        api ->
            <<"API"/utf8>>;

        cli ->
            <<"CLI"/utf8>>;

        event ->
            <<"Event System"/utf8>>;

        data ->
            <<"Data System"/utf8>>;

        workflow ->
            <<"Workflow"/utf8>>;

        u_i ->
            <<"User Interface"/utf8>>
    end.

-file("src/intent.gleam", 1388).
-spec method_to_string(intent@types:method()) -> binary().
method_to_string(Method) ->
    case Method of
        get ->
            <<"GET"/utf8>>;

        post ->
            <<"POST"/utf8>>;

        put ->
            <<"PUT"/utf8>>;

        delete ->
            <<"DELETE"/utf8>>;

        patch ->
            <<"PATCH"/utf8>>;

        head ->
            <<"HEAD"/utf8>>;

        options ->
            <<"OPTIONS"/utf8>>
    end.

-file("src/intent.gleam", 396).
?DOC(" Export spec as YAML-like format\n").
-spec export_as_yaml(intent@types:spec()) -> {ok, binary()} | {error, binary()}.
export_as_yaml(Spec) ->
    Features = begin
        _pipe = erlang:element(8, Spec),
        _pipe@3 = gleam@list:map(
            _pipe,
            fun(F) ->
                <<<<<<<<<<<<<<"  - name: "/utf8, (erlang:element(2, F))/binary>>/binary,
                                        "\n"/utf8>>/binary,
                                    "    description: "/utf8>>/binary,
                                (erlang:element(3, F))/binary>>/binary,
                            "\n"/utf8>>/binary,
                        "    behaviors:\n"/utf8>>/binary,
                    (begin
                        _pipe@1 = erlang:element(4, F),
                        _pipe@2 = gleam@list:map(
                            _pipe@1,
                            fun(B) ->
                                <<<<<<<<<<<<<<<<<<<<<<<<<<<<"      - name: "/utf8,
                                                                                        (erlang:element(
                                                                                            2,
                                                                                            B
                                                                                        ))/binary>>/binary,
                                                                                    "\n"/utf8>>/binary,
                                                                                "        intent: "/utf8>>/binary,
                                                                            (erlang:element(
                                                                                3,
                                                                                B
                                                                            ))/binary>>/binary,
                                                                        "\n"/utf8>>/binary,
                                                                    "        method: "/utf8>>/binary,
                                                                (method_to_string(
                                                                    erlang:element(
                                                                        2,
                                                                        erlang:element(
                                                                            7,
                                                                            B
                                                                        )
                                                                    )
                                                                ))/binary>>/binary,
                                                            "\n"/utf8>>/binary,
                                                        "        path: "/utf8>>/binary,
                                                    (erlang:element(
                                                        3,
                                                        erlang:element(7, B)
                                                    ))/binary>>/binary,
                                                "\n"/utf8>>/binary,
                                            "        status: "/utf8>>/binary,
                                        (gleam@string:inspect(
                                            erlang:element(
                                                2,
                                                erlang:element(8, B)
                                            )
                                        ))/binary>>/binary,
                                    "\n"/utf8>>
                            end
                        ),
                        gleam@string:concat(_pipe@2)
                    end)/binary>>
            end
        ),
        gleam@string:concat(_pipe@3)
    end,
    {ok,
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"# "/utf8,
                                                                        (erlang:element(
                                                                            2,
                                                                            Spec
                                                                        ))/binary>>/binary,
                                                                    "\n"/utf8>>/binary,
                                                                "name: "/utf8>>/binary,
                                                            (erlang:element(
                                                                2,
                                                                Spec
                                                            ))/binary>>/binary,
                                                        "\n"/utf8>>/binary,
                                                    "description: "/utf8>>/binary,
                                                (erlang:element(3, Spec))/binary>>/binary,
                                            "\n"/utf8>>/binary,
                                        "version: "/utf8>>/binary,
                                    (erlang:element(5, Spec))/binary>>/binary,
                                "\n"/utf8>>/binary,
                            "audience: "/utf8>>/binary,
                        (erlang:element(4, Spec))/binary>>/binary,
                    "\n\n"/utf8>>/binary,
                "features:\n"/utf8>>/binary,
            Features/binary>>}.

-file("src/intent.gleam", 423).
?DOC(" Export spec as readable Markdown\n").
-spec export_as_markdown(intent@types:spec()) -> {ok, binary()} |
    {error, binary()}.
export_as_markdown(Spec) ->
    Features_md = begin
        _pipe = erlang:element(8, Spec),
        _pipe@3 = gleam@list:map(
            _pipe,
            fun(F) ->
                <<<<<<<<<<<<"## "/utf8, (erlang:element(2, F))/binary>>/binary,
                                    "\n\n"/utf8>>/binary,
                                (erlang:element(3, F))/binary>>/binary,
                            "\n\n"/utf8>>/binary,
                        "### Behaviors\n\n"/utf8>>/binary,
                    (begin
                        _pipe@1 = erlang:element(4, F),
                        _pipe@2 = gleam@list:map(
                            _pipe@1,
                            fun(B) ->
                                <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"#### "/utf8,
                                                                                            (erlang:element(
                                                                                                2,
                                                                                                B
                                                                                            ))/binary>>/binary,
                                                                                        "\n\n"/utf8>>/binary,
                                                                                    "**Intent:** "/utf8>>/binary,
                                                                                (erlang:element(
                                                                                    3,
                                                                                    B
                                                                                ))/binary>>/binary,
                                                                            "\n\n"/utf8>>/binary,
                                                                        "- **Method:** `"/utf8>>/binary,
                                                                    (method_to_string(
                                                                        erlang:element(
                                                                            2,
                                                                            erlang:element(
                                                                                7,
                                                                                B
                                                                            )
                                                                        )
                                                                    ))/binary>>/binary,
                                                                "`\n"/utf8>>/binary,
                                                            "- **Path:** `"/utf8>>/binary,
                                                        (erlang:element(
                                                            3,
                                                            erlang:element(7, B)
                                                        ))/binary>>/binary,
                                                    "`\n"/utf8>>/binary,
                                                "- **Expected Status:** "/utf8>>/binary,
                                            (gleam@string:inspect(
                                                erlang:element(
                                                    2,
                                                    erlang:element(8, B)
                                                )
                                            ))/binary>>/binary,
                                        "\n\n"/utf8>>/binary,
                                    (case erlang:element(4, B) of
                                        <<""/utf8>> ->
                                            <<""/utf8>>;

                                        Notes ->
                                            <<<<"**Notes:** "/utf8,
                                                    Notes/binary>>/binary,
                                                "\n\n"/utf8>>
                                    end)/binary>>
                            end
                        ),
                        gleam@string:concat(_pipe@2)
                    end)/binary>>
            end
        ),
        gleam@string:concat(_pipe@3)
    end,
    {ok,
        <<<<<<<<<<<<<<<<<<<<<<<<<<"# "/utf8, (erlang:element(2, Spec))/binary>>/binary,
                                                        "\n\n"/utf8>>/binary,
                                                    "> "/utf8>>/binary,
                                                (erlang:element(3, Spec))/binary>>/binary,
                                            "\n\n"/utf8>>/binary,
                                        "**Version:** "/utf8>>/binary,
                                    (erlang:element(5, Spec))/binary>>/binary,
                                "  \n"/utf8>>/binary,
                            "**Audience:** "/utf8>>/binary,
                        (erlang:element(4, Spec))/binary>>/binary,
                    "\n\n"/utf8>>/binary,
                "---\n\n"/utf8>>/binary,
            Features_md/binary>>}.

-file("src/intent.gleam", 454).
?DOC(" Export spec as AI-ready prompt\n").
-spec export_as_prompt(intent@types:spec()) -> {ok, binary()} |
    {error, binary()}.
export_as_prompt(Spec) ->
    Behaviors_text = begin
        _pipe = erlang:element(8, Spec),
        _pipe@1 = gleam@list:flat_map(_pipe, fun(F) -> erlang:element(4, F) end),
        _pipe@2 = gleam@list:map(
            _pipe@1,
            fun(B) ->
                <<<<<<<<<<<<<<<<<<<<"- "/utf8, (erlang:element(2, B))/binary>>/binary,
                                                    ": "/utf8>>/binary,
                                                (erlang:element(3, B))/binary>>/binary,
                                            " ("/utf8>>/binary,
                                        (method_to_string(
                                            erlang:element(
                                                2,
                                                erlang:element(7, B)
                                            )
                                        ))/binary>>/binary,
                                    " "/utf8>>/binary,
                                (erlang:element(3, erlang:element(7, B)))/binary>>/binary,
                            " -> "/utf8>>/binary,
                        (gleam@string:inspect(
                            erlang:element(2, erlang:element(8, B))
                        ))/binary>>/binary,
                    ")"/utf8>>
            end
        ),
        gleam@string:join(_pipe@2, <<"\n"/utf8>>)
    end,
    Success_criteria = begin
        _pipe@3 = erlang:element(6, Spec),
        _pipe@4 = gleam@list:map(_pipe@3, fun(C) -> <<"- "/utf8, C/binary>> end),
        gleam@string:join(_pipe@4, <<"\n"/utf8>>)
    end,
    {ok,
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"You are implementing: "/utf8,
                                                                                    (erlang:element(
                                                                                        2,
                                                                                        Spec
                                                                                    ))/binary>>/binary,
                                                                                "\n\n"/utf8>>/binary,
                                                                            "## Description\n"/utf8>>/binary,
                                                                        (erlang:element(
                                                                            3,
                                                                            Spec
                                                                        ))/binary>>/binary,
                                                                    "\n\n"/utf8>>/binary,
                                                                "## Target Audience\n"/utf8>>/binary,
                                                            (erlang:element(
                                                                4,
                                                                Spec
                                                            ))/binary>>/binary,
                                                        "\n\n"/utf8>>/binary,
                                                    "## Success Criteria\n"/utf8>>/binary,
                                                Success_criteria/binary>>/binary,
                                            "\n\n"/utf8>>/binary,
                                        "## Behaviors to Implement\n"/utf8>>/binary,
                                    Behaviors_text/binary>>/binary,
                                "\n\n"/utf8>>/binary,
                            "## Implementation Requirements\n"/utf8>>/binary,
                        "1. Each behavior must return the specified HTTP status\n"/utf8>>/binary,
                    "2. Follow the request/response contract exactly\n"/utf8>>/binary,
                "3. Handle edge cases gracefully\n"/utf8>>/binary,
            "4. All behaviors must pass automated validation\n"/utf8>>}.

-file("src/intent.gleam", 1359).
?DOC(" Convert a Spec to BeadRecords\n").
-spec spec_to_beads(intent@types:spec()) -> list(intent@bead_templates:bead_record()).
spec_to_beads(Spec) ->
    _pipe = erlang:element(8, Spec),
    gleam@list:flat_map(
        _pipe,
        fun(Feature) -> _pipe@1 = erlang:element(4, Feature),
            gleam@list:map(
                _pipe@1,
                fun(Behavior) ->
                    {bead_record,
                        erlang:element(2, Behavior),
                        erlang:element(3, Behavior),
                        <<"api"/utf8>>,
                        3,
                        <<"behavior"/utf8>>,
                        erlang:element(6, Behavior),
                        erlang:element(4, Behavior),
                        [<<<<"Status "/utf8,
                                    (gleam@string:inspect(
                                        erlang:element(
                                            2,
                                            erlang:element(8, Behavior)
                                        )
                                    ))/binary>>/binary,
                                " returned"/utf8>>,
                            <<"Response matches expected schema"/utf8>>],
                        erlang:element(5, Behavior),
                        <<<<(method_to_string(
                                    erlang:element(
                                        2,
                                        erlang:element(7, Behavior)
                                    )
                                ))/binary,
                                " "/utf8>>/binary,
                            (erlang:element(3, erlang:element(7, Behavior)))/binary>>,
                        <<"HTTP "/utf8,
                            (gleam@string:inspect(
                                erlang:element(2, erlang:element(8, Behavior))
                            ))/binary>>,
                        [<<"Correct status code"/utf8>>,
                            <<"Valid response body"/utf8>>],
                        [<<"Return 500 for client errors"/utf8>>,
                            <<"Expose internal errors"/utf8>>],
                        begin
                            _pipe@2 = erlang:element(5, Behavior),
                            gleam@list:map(
                                _pipe@2,
                                fun(R) -> <<"Depends on: "/utf8, R/binary>> end
                            )
                        end}
                end
            ) end
    ).

-file("src/intent.gleam", 1704).
-spec risk_level_to_string(intent@plan_mode:risk_level()) -> binary().
risk_level_to_string(Risk) ->
    case Risk of
        low ->
            <<"low"/utf8>>;

        medium ->
            <<"medium"/utf8>>;

        high ->
            <<"high"/utf8>>;

        critical ->
            <<"critical"/utf8>>
    end.

-file("src/intent.gleam", 1713).
-spec escape_cue_string(binary()) -> binary().
escape_cue_string(S) ->
    _pipe = S,
    _pipe@1 = gleam@string:replace(_pipe, <<"\\"/utf8>>, <<"\\\\"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"\""/utf8>>, <<"\\\""/utf8>>),
    _pipe@3 = gleam@string:replace(_pipe@2, <<"\n"/utf8>>, <<"\\n"/utf8>>),
    gleam@string:replace(_pipe@3, <<"\t"/utf8>>, <<"\\t"/utf8>>).

-file("src/intent.gleam", 1680).
?DOC(" Write plan approval to session CUE file\n").
-spec approve_plan(binary(), binary(), binary()) -> {ok, nil} |
    {error, binary()}.
approve_plan(Session_id, Approved_by, Notes) ->
    Session_path = <<<<".intent/session-"/utf8, Session_id/binary>>/binary,
        ".cue"/utf8>>,
    Timestamp = intent_ffi:current_iso8601_timestamp(),
    Notes_line = case gleam@string:is_empty(Notes) of
        true ->
            <<""/utf8>>;

        false ->
            <<<<"\n\tnotes: \""/utf8, (escape_cue_string(Notes))/binary>>/binary,
                "\""/utf8>>
    end,
    Approval_cue = <<<<<<<<<<<<"\n// Plan Approval\napproval: {\n\tapproved: true\n\tapproved_at: \""/utf8,
                            Timestamp/binary>>/binary,
                        "\"\n\tapproved_by: \""/utf8>>/binary,
                    Approved_by/binary>>/binary,
                "\""/utf8>>/binary,
            Notes_line/binary>>/binary,
        "\n}\n"/utf8>>,
    case simplifile:append(Session_path, Approval_cue) of
        {ok, nil} ->
            {ok, nil};

        {error, Err} ->
            {error,
                <<"Failed to write approval: "/utf8,
                    (gleam@string:inspect(Err))/binary>>}
    end.

-file("src/intent.gleam", 1844).
?DOC(" Generate regeneration entries based on failed beads and strategy\n").
-spec generate_regeneration_entries(
    list(intent@bead_feedback:bead_feedback()),
    binary()
) -> binary().
generate_regeneration_entries(Failed_beads, Strategy) ->
    Timestamp = intent_ffi:current_iso8601_timestamp(),
    Entries = begin
        _pipe = Failed_beads,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(Fb) ->
                Root_cause = case erlang:element(7, Fb) of
                    {some, Err} ->
                        erlang:element(3, Err);

                    none ->
                        erlang:element(4, Fb)
                end,
                <<<<<<<<<<<<<<<<<<<<<<<<<<"  {\n"/utf8, "    bead_id: \""/utf8>>/binary,
                                                                (erlang:element(
                                                                    2,
                                                                    Fb
                                                                ))/binary>>/binary,
                                                            "\"\n"/utf8>>/binary,
                                                        "    strategy: \""/utf8>>/binary,
                                                    Strategy/binary>>/binary,
                                                "\"\n"/utf8>>/binary,
                                            "    root_cause: \""/utf8>>/binary,
                                        (escape_cue_string(Root_cause))/binary>>/binary,
                                    "\"\n"/utf8>>/binary,
                                "    regenerated_at: \""/utf8>>/binary,
                            Timestamp/binary>>/binary,
                        "\"\n"/utf8>>/binary,
                    "  }"/utf8>>
            end
        ),
        gleam@string:join(_pipe@1, <<",\n"/utf8>>)
    end,
    Entries.

-file("src/intent.gleam", 1871).
?DOC(" Append regeneration metadata to session CUE file\n").
-spec append_regeneration_to_session(binary(), binary()) -> {ok, nil} |
    {error, binary()}.
append_regeneration_to_session(Session_path, Entries) ->
    Regen_cue = <<<<"\n// Regeneration Metadata\nregenerations: [\n"/utf8,
            Entries/binary>>/binary,
        "\n]\n"/utf8>>,
    case simplifile:append(Session_path, Regen_cue) of
        {ok, nil} ->
            {ok, nil};

        {error, Err} ->
            {error,
                <<"Failed to append: "/utf8,
                    (gleam@string:inspect(Err))/binary>>}
    end.

-file("src/intent.gleam", 1888).
-spec bead_feedback_error_to_string(intent@bead_feedback:feedback_error()) -> binary().
bead_feedback_error_to_string(Err) ->
    case Err of
        {session_not_found, Id} ->
            <<"Session not found: "/utf8, Id/binary>>;

        {write_error, Path, Msg} ->
            <<<<<<"Write error to "/utf8, Path/binary>>/binary, ": "/utf8>>/binary,
                Msg/binary>>;

        {validation_error, Msg@1} ->
            <<"Validation error: "/utf8, Msg@1/binary>>
    end.

-file("src/intent.gleam", 2106).
-spec stage_to_display_string(intent@interview:interview_stage()) -> binary().
stage_to_display_string(Stage) ->
    case Stage of
        discovery ->
            <<"Discovery"/utf8>>;

        refinement ->
            <<"Refinement"/utf8>>;

        validation ->
            <<"Validation"/utf8>>;

        complete ->
            <<"Complete"/utf8>>;

        paused ->
            <<"Paused"/utf8>>
    end.

-file("src/intent.gleam", 2223).
-spec gap_to_json(intent@kirk@inversion_checker:inversion_gap()) -> gleam@json:json().
gap_to_json(Gap) ->
    gleam@json:object(
        [{<<"category"/utf8>>, gleam@json:string(erlang:element(2, Gap))},
            {<<"description"/utf8>>, gleam@json:string(erlang:element(3, Gap))},
            {<<"severity"/utf8>>,
                gleam@json:string(
                    intent@kirk@inversion_checker:severity_to_string(
                        erlang:element(4, Gap)
                    )
                )},
            {<<"what_could_fail"/utf8>>,
                gleam@json:string(erlang:element(5, Gap))}]
    ).

-file("src/intent.gleam", 2335).
-spec detected_gap_to_json(intent@kirk@gap_detector:gap()) -> gleam@json:json().
detected_gap_to_json(Gap) ->
    gleam@json:object(
        [{<<"type"/utf8>>,
                gleam@json:string(
                    intent@kirk@gap_detector:gap_type_to_string(
                        erlang:element(2, Gap)
                    )
                )},
            {<<"description"/utf8>>, gleam@json:string(erlang:element(3, Gap))},
            {<<"severity"/utf8>>,
                gleam@json:string(
                    intent@kirk@gap_detector:severity_to_string(
                        erlang:element(4, Gap)
                    )
                )},
            {<<"suggestion"/utf8>>, gleam@json:string(erlang:element(5, Gap))},
            {<<"mental_model"/utf8>>, gleam@json:string(erlang:element(6, Gap))}]
    ).

-file("src/intent.gleam", 2587).
-spec answer_loader_error_to_string(intent@answer_loader:answer_loader_error()) -> binary().
answer_loader_error_to_string(Err) ->
    case Err of
        {file_not_found, Path} ->
            <<"File not found: "/utf8, Path/binary>>;

        {permission_denied, Path@1} ->
            <<"Permission denied reading: "/utf8, Path@1/binary>>;

        {parse_error, Path@2, Msg} ->
            <<<<<<"Parse error in "/utf8, Path@2/binary>>/binary, ": "/utf8>>/binary,
                Msg/binary>>;

        {schema_error, Msg@1} ->
            <<"Schema validation failed: "/utf8, Msg@1/binary>>;

        {io_error, Msg@2} ->
            <<"I/O error: "/utf8, Msg@2/binary>>
    end.

-file("src/intent.gleam", 1029).
?DOC(" Build initial session CUE content\n").
-spec build_session_cue(binary(), binary(), boolean()) -> binary().
build_session_cue(Session_id, Profile, Light_mode) ->
    Light_str = case Light_mode of
        true ->
            <<"true"/utf8>>;

        false ->
            <<"false"/utf8>>
    end,
    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"// Intent Interview Session\n"/utf8,
                                                                                "// Generated by intent interview --cue\n\n"/utf8>>/binary,
                                                                            "package intent\n\n"/utf8>>/binary,
                                                                        "session: {\n"/utf8>>/binary,
                                                                    "\tid: \""/utf8>>/binary,
                                                                Session_id/binary>>/binary,
                                                            "\"\n"/utf8>>/binary,
                                                        "\tprofile: \""/utf8>>/binary,
                                                    Profile/binary>>/binary,
                                                "\"\n"/utf8>>/binary,
                                            "\tlight_mode: "/utf8>>/binary,
                                        Light_str/binary>>/binary,
                                    "\n"/utf8>>/binary,
                                "\tcreated_at: \""/utf8>>/binary,
                            (intent_ffi:current_timestamp())/binary>>/binary,
                        "\"\n"/utf8>>/binary,
                    "\tanswers: []\n"/utf8>>/binary,
                "\tgaps: []\n"/utf8>>/binary,
            "\tconflicts: []\n"/utf8>>/binary,
        "}\n"/utf8>>.

-file("src/intent.gleam", 1203).
?DOC(" Ask a single question and collect answer\n").
-spec ask_single_question(
    intent@interview:interview_session(),
    intent@question_types:question(),
    integer()
) -> intent@interview:interview_session().
ask_single_question(Session, Question, Round) ->
    gleam@io:println(<<""/utf8>>),
    gleam@io:print(
        <<<<"Q"/utf8,
                (gleam@string:inspect(erlang:element(6, Question)))/binary>>/binary,
            ": "/utf8>>
    ),
    gleam@io:println(erlang:element(7, Question)),
    case gleam@string:length(erlang:element(8, Question)) > 0 of
        true ->
            gleam@io:println(
                <<"   Context: "/utf8, (erlang:element(8, Question))/binary>>
            );

        false ->
            nil
    end,
    case gleam@string:length(erlang:element(9, Question)) > 0 of
        true ->
            gleam@io:println(
                <<"   Example: "/utf8, (erlang:element(9, Question))/binary>>
            );

        false ->
            nil
    end,
    gleam@io:print(<<""/utf8>>),
    Answer_text = case intent@stdin:prompt_for_answer(<<"> "/utf8>>) of
        {ok, Text} ->
            Text;

        {error, Err} ->
            gleam@io:println_error(<<"Error reading input: "/utf8, Err/binary>>),
            gleam@io:println(<<""/utf8>>),
            <<"(input error - please try again)"/utf8>>
    end,
    Extracted = intent@interview:extract_from_answer(
        erlang:element(2, Question),
        Answer_text,
        erlang:element(11, Question)
    ),
    Confidence = intent@interview:calculate_confidence(
        erlang:element(2, Question),
        Answer_text,
        Extracted
    ),
    Answer = {answer,
        erlang:element(2, Question),
        erlang:element(7, Question),
        erlang:element(4, Question),
        Round,
        Answer_text,
        Extracted,
        Confidence,
        <<""/utf8>>,
        intent_ffi:current_timestamp()},
    Updated_session = intent@interview:add_answer(Session, Answer),
    {Sess_with_gaps, _} = intent@interview:check_for_gaps(
        Updated_session,
        Question,
        Answer
    ),
    {Sess_final, _} = intent@interview:check_for_conflicts(
        Sess_with_gaps,
        Answer
    ),
    Sess_final.

-file("src/intent.gleam", 1180).
?DOC(" Ask all unanswered questions in a round\n").
-spec ask_questions_in_round(
    intent@interview:interview_session(),
    integer(),
    intent@question_types:question()
) -> intent@interview:interview_session().
ask_questions_in_round(Session, Round, _) ->
    Profile_str = profile_to_string(erlang:element(3, Session)),
    Questions = intent@interview_questions:get_questions_for_round(
        Profile_str,
        Round
    ),
    Answered_ids = gleam@list:map(
        erlang:element(9, Session),
        fun(A) -> erlang:element(2, A) end
    ),
    Unanswered = gleam@list:filter(
        Questions,
        fun(Q) ->
            not gleam@list:contains(Answered_ids, erlang:element(2, Q))
        end
    ),
    gleam@list:fold(
        Unanswered,
        Session,
        fun(Sess, Question) -> ask_single_question(Sess, Question, Round) end
    ).

-file("src/intent.gleam", 1130).
-spec interview_loop_with_max_round(
    intent@interview:interview_session(),
    integer(),
    integer()
) -> intent@interview:interview_session().
interview_loop_with_max_round(Session, Round, Max_round) ->
    case Round > Max_round of
        true ->
            Session;

        false ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(
                <<"═══════════════════════════════════════════════════════════════════"/utf8>>
            ),
            gleam@io:println(
                <<<<<<"ROUND "/utf8, (gleam@string:inspect(Round))/binary>>/binary,
                        "/"/utf8>>/binary,
                    (gleam@string:inspect(Max_round))/binary>>
            ),
            gleam@io:println(
                <<"═══════════════════════════════════════════════════════════════════"/utf8>>
            ),
            gleam@io:println(<<""/utf8>>),
            case intent@interview:get_first_question_for_round(Session, Round) of
                {error, _} ->
                    gleam@io:println(<<"(No questions for this round)"/utf8>>),
                    interview_loop_with_max_round(Session, Round + 1, Max_round);

                {ok, First_question} ->
                    Updated_session = ask_questions_in_round(
                        Session,
                        Round,
                        First_question
                    ),
                    Beads = intent@bead_templates:generate_beads_from_session(
                        Updated_session
                    ),
                    Preview = intent@bead_templates:format_progressive_preview(
                        Beads,
                        Round
                    ),
                    case gleam@string:length(Preview) > 0 of
                        true ->
                            gleam@io:println(Preview);

                        false ->
                            nil
                    end,
                    Blocking_gaps = intent@interview:get_blocking_gaps(
                        Updated_session
                    ),
                    case Blocking_gaps of
                        [] ->
                            interview_loop_with_max_round(
                                Updated_session,
                                Round + 1,
                                Max_round
                            );

                        Gaps ->
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<"⚠️ BLOCKING GAPS DETECTED:"/utf8>>
                            ),
                            gleam@list:each(
                                Gaps,
                                fun(Gap) ->
                                    gleam@io:println(
                                        <<"  • "/utf8,
                                            (erlang:element(4, Gap))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"    "/utf8,
                                            (erlang:element(7, Gap))/binary>>
                                    )
                                end
                            ),
                            gleam@io:println(<<""/utf8>>),
                            interview_loop_with_max_round(
                                Updated_session,
                                Round + 1,
                                Max_round
                            )
                    end
            end
    end.

-file("src/intent.gleam", 1126).
?DOC(" Main interview loop - asks questions round by round\n").
-spec interview_loop(intent@interview:interview_session(), integer()) -> intent@interview:interview_session().
interview_loop(Session, Round) ->
    interview_loop_with_max_round(Session, Round, 5).

-file("src/intent.gleam", 2020).
?DOC(" The `sessions` command - list all interview sessions\n").
-spec sessions_command() -> glint:command(nil).
sessions_command() ->
    _pipe@2 = glint:command(
        fun(Input) ->
            Jsonl_path = <<".interview/sessions.jsonl"/utf8>>,
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            Profile_filter = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"profile"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            case intent@interview_storage:list_sessions_from_jsonl(Jsonl_path) of
                {error, _} ->
                    intent@cli_ui:print_warning(
                        <<"No interview sessions found"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(<<"Start a new interview with:"/utf8>>),
                    gleam@io:println(
                        <<"  intent interview --profile api"/utf8>>
                    ),
                    intent_ffi:halt(0);

                {ok, []} ->
                    intent@cli_ui:print_warning(
                        <<"No interview sessions found"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(<<"Start a new interview with:"/utf8>>),
                    gleam@io:println(
                        <<"  intent interview --profile api"/utf8>>
                    ),
                    intent_ffi:halt(0);

                {ok, Sessions} ->
                    Filtered = case Profile_filter of
                        <<""/utf8>> ->
                            Sessions;

                        P ->
                            gleam@list:filter(
                                Sessions,
                                fun(S) ->
                                    profile_to_string(erlang:element(3, S)) =:= gleam@string:lowercase(
                                        P
                                    )
                                end
                            )
                    end,
                    case Is_json of
                        true ->
                            Json_sessions = gleam@json:array(
                                Filtered,
                                fun intent@interview_storage:session_to_json/1
                            ),
                            gleam@io:println(
                                gleam@json:to_string(Json_sessions)
                            );

                        false ->
                            intent@cli_ui:print_header(
                                <<"Interview Sessions"/utf8>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            gleam@list:each(
                                Filtered,
                                fun(Session) ->
                                    Status_icon = case erlang:element(
                                        7,
                                        Session
                                    ) of
                                        complete ->
                                            <<"✓"/utf8>>;

                                        paused ->
                                            <<"⏸"/utf8>>;

                                        _ ->
                                            <<"●"/utf8>>
                                    end,
                                    gleam@io:println(
                                        <<<<Status_icon/binary, " "/utf8>>/binary,
                                            (erlang:element(2, Session))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"  Profile: "/utf8,
                                            (profile_to_display_string(
                                                erlang:element(3, Session)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"  Stage: "/utf8,
                                            (stage_to_display_string(
                                                erlang:element(7, Session)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<<<"  Rounds: "/utf8,
                                                (gleam@string:inspect(
                                                    erlang:element(8, Session)
                                                ))/binary>>/binary,
                                            "/5"/utf8>>
                                    ),
                                    gleam@io:println(
                                        <<"  Answers: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:length(
                                                    erlang:element(9, Session)
                                                )
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"  Created: "/utf8,
                                            (erlang:element(4, Session))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"  Updated: "/utf8,
                                            (erlang:element(5, Session))/binary>>
                                    ),
                                    gleam@io:println(<<""/utf8>>)
                                end
                            ),
                            gleam@io:println(
                                <<<<"Total: "/utf8,
                                        (gleam@string:inspect(
                                            erlang:length(Filtered)
                                        ))/binary>>/binary,
                                    " session(s)"/utf8>>
                            )
                    end,
                    intent_ffi:halt(0)
            end
        end
    ),
    _pipe@3 = glint:description(_pipe@2, <<"List all interview sessions"/utf8>>),
    _pipe@6 = glint:flag(
        _pipe@3,
        <<"json"/utf8>>,
        begin
            _pipe@4 = glint@flag:bool(),
            _pipe@5 = glint@flag:default(_pipe@4, false),
            glint@flag:description(_pipe@5, <<"Output as JSON"/utf8>>)
        end
    ),
    glint:flag(
        _pipe@6,
        <<"profile"/utf8>>,
        begin
            _pipe@7 = glint@flag:string(),
            _pipe@8 = glint@flag:default(_pipe@7, <<""/utf8>>),
            glint@flag:description(
                _pipe@8,
                <<"Filter by profile (api, cli, event, etc.)"/utf8>>
            )
        end
    ).

-file("src/intent.gleam", 141).
-spec run_check(
    binary(),
    binary(),
    boolean(),
    binary(),
    binary(),
    intent@runner:output_level()
) -> nil.
run_check(
    Spec_path,
    Target_url,
    Is_json,
    Feature_filter,
    Only_filter,
    Output_level
) ->
    case intent@loader:load_spec(Spec_path) of
        {error, E} ->
            intent@cli_ui:print_error(intent@loader:format_error(E)),
            intent_ffi:halt(3);

        {ok, Spec} ->
            intent@cli_ui:print_header(
                <<"Checking spec: "/utf8, (erlang:element(2, Spec))/binary>>
            ),
            Options = {run_options, case Feature_filter of
                    <<""/utf8>> ->
                        none;

                    F ->
                        {some, F}
                end, case Only_filter of
                    <<""/utf8>> ->
                        none;

                    B ->
                        {some, B}
                end, Output_level},
            Result = intent@runner:run_spec(Spec, Target_url, Options),
            case Is_json of
                true ->
                    Json_result = intent@output:spec_result_to_json(Result),
                    gleam@io:println(gleam@json:to_string(Json_result));

                false ->
                    gleam@io:println(intent@output:spec_result_to_text(Result))
            end,
            Exit_code = case Result of
                {spec_result, true, _, _, _, _, _, _, _, _, _} ->
                    intent@cli_ui:print_success(<<"All checks passed!"/utf8>>),
                    0;

                {spec_result, _, _, _, Blocked, _, _, _, _, _, _} when Blocked > 0 ->
                    intent@cli_ui:print_warning(
                        <<"Blocked behaviors detected"/utf8>>
                    ),
                    2;

                _ ->
                    intent@cli_ui:print_error(<<"Check failed"/utf8>>),
                    1
            end,
            intent_ffi:halt(Exit_code)
    end.

-file("src/intent.gleam", 88).
?DOC(" The `check` command - run spec against a target\n").
-spec check_command() -> glint:command(nil).
check_command() ->
    _pipe@4 = glint:command(
        fun(Input) ->
            Target_url = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"target"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<""/utf8>>)
            end,
            Is_json = begin
                _pipe@1 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, false)
            end,
            Feature_filter = begin
                _pipe@2 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"feature"/utf8>>
                ),
                gleam@result:unwrap(_pipe@2, <<""/utf8>>)
            end,
            Only_filter = begin
                _pipe@3 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"only"/utf8>>
                ),
                gleam@result:unwrap(_pipe@3, <<""/utf8>>)
            end,
            Output_level = case glint@flag:get_bool(
                erlang:element(3, Input),
                <<"verbose"/utf8>>
            ) of
                {ok, true} ->
                    verbose;

                _ ->
                    case glint@flag:get_bool(
                        erlang:element(3, Input),
                        <<"quiet"/utf8>>
                    ) of
                        {ok, true} ->
                            quiet;

                        _ ->
                            normal
                    end
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    run_check(
                        Spec_path,
                        Target_url,
                        Is_json,
                        Feature_filter,
                        Only_filter,
                        Output_level
                    );

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent check <spec.cue> --target <url>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@5 = glint:description(
        _pipe@4,
        <<"Run spec against a target URL and verify behaviors"/utf8>>
    ),
    _pipe@8 = glint:flag(
        _pipe@5,
        <<"target"/utf8>>,
        begin
            _pipe@6 = glint@flag:string(),
            _pipe@7 = glint@flag:default(_pipe@6, <<""/utf8>>),
            glint@flag:description(
                _pipe@7,
                <<"Target base URL to test against"/utf8>>
            )
        end
    ),
    _pipe@11 = glint:flag(
        _pipe@8,
        <<"json"/utf8>>,
        begin
            _pipe@9 = glint@flag:bool(),
            _pipe@10 = glint@flag:default(_pipe@9, false),
            glint@flag:description(_pipe@10, <<"Output results as JSON"/utf8>>)
        end
    ),
    _pipe@14 = glint:flag(
        _pipe@11,
        <<"feature"/utf8>>,
        begin
            _pipe@12 = glint@flag:string(),
            _pipe@13 = glint@flag:default(_pipe@12, <<""/utf8>>),
            glint@flag:description(
                _pipe@13,
                <<"Filter to a specific feature"/utf8>>
            )
        end
    ),
    _pipe@17 = glint:flag(
        _pipe@14,
        <<"only"/utf8>>,
        begin
            _pipe@15 = glint@flag:string(),
            _pipe@16 = glint@flag:default(_pipe@15, <<""/utf8>>),
            glint@flag:description(
                _pipe@16,
                <<"Run only a specific behavior"/utf8>>
            )
        end
    ),
    _pipe@20 = glint:flag(
        _pipe@17,
        <<"verbose"/utf8>>,
        begin
            _pipe@18 = glint@flag:bool(),
            _pipe@19 = glint@flag:default(_pipe@18, false),
            glint@flag:description(_pipe@19, <<"Verbose output"/utf8>>)
        end
    ),
    glint:flag(
        _pipe@20,
        <<"quiet"/utf8>>,
        begin
            _pipe@21 = glint@flag:bool(),
            _pipe@22 = glint@flag:default(_pipe@21, false),
            glint@flag:description(
                _pipe@22,
                <<"Quiet output (errors only)"/utf8>>
            )
        end
    ).

-file("src/intent.gleam", 207).
?DOC(" The `validate` command - validate CUE spec without running\n").
-spec validate_command() -> glint:command(nil).
validate_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:validate_cue(Spec_path) of
                        {ok, _} ->
                            intent@cli_ui:print_success(
                                <<"Valid spec: "/utf8, Spec_path/binary>>
                            ),
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                <<"Invalid spec: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent validate <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Validate a CUE spec file without running tests"/utf8>>
    ).

-file("src/intent.gleam", 233).
?DOC(" The `show` command - pretty print a parsed spec\n").
-spec show_command() -> glint:command(nil).
show_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case Is_json of
                        true ->
                            case intent@loader:export_spec_json(Spec_path) of
                                {ok, Json_str} ->
                                    gleam@io:println(Json_str),
                                    intent_ffi:halt(0);

                                {error, E} ->
                                    gleam@io:println_error(
                                        <<"Error: "/utf8,
                                            (intent@loader:format_error(E))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end;

                        false ->
                            case intent@loader:load_spec(Spec_path) of
                                {ok, Spec} ->
                                    print_spec_summary(Spec),
                                    intent_ffi:halt(0);

                                {error, E@1} ->
                                    gleam@io:println_error(
                                        <<"Error: "/utf8,
                                            (intent@loader:format_error(E@1))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent show <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(_pipe@1, <<"Pretty print a parsed spec"/utf8>>),
    glint:flag(
        _pipe@2,
        <<"json"/utf8>>,
        begin
            _pipe@3 = glint@flag:bool(),
            _pipe@4 = glint@flag:default(_pipe@3, false),
            glint@flag:description(_pipe@4, <<"Output as JSON"/utf8>>)
        end
    ).

-file("src/intent.gleam", 342).
?DOC(
    " The `export` command - export spec to various formats\n"
    " Supports: json (default), yaml, md (markdown), prompt (AI-ready)\n"
).
-spec export_command() -> glint:command(nil).
export_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Format = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"format"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<"json"/utf8>>)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Output = case Format of
                                <<"json"/utf8>> ->
                                    export_as_json(Spec_path);

                                <<"yaml"/utf8>> ->
                                    export_as_yaml(Spec);

                                <<"md"/utf8>> ->
                                    export_as_markdown(Spec);

                                <<"markdown"/utf8>> ->
                                    export_as_markdown(Spec);

                                <<"prompt"/utf8>> ->
                                    export_as_prompt(Spec);

                                _ ->
                                    {error,
                                        <<<<"Unknown format: "/utf8,
                                                Format/binary>>/binary,
                                            ". Valid: json, yaml, md, prompt"/utf8>>}
                            end,
                            case Output of
                                {ok, Str} ->
                                    gleam@io:println(Str),
                                    intent_ffi:halt(0);

                                {error, E} ->
                                    gleam@io:println_error(
                                        <<"Error: "/utf8, E/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end;

                        {error, E@1} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E@1))/binary>>
                            ),
                            intent_ffi:halt(4)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent export <spec.cue> [--format json|yaml|md|prompt]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(
        _pipe@1,
        <<"Export spec to JSON, YAML, Markdown, or AI prompt format"/utf8>>
    ),
    glint:flag(
        _pipe@2,
        <<"format"/utf8>>,
        begin
            _pipe@3 = glint@flag:string(),
            _pipe@4 = glint@flag:default(_pipe@3, <<"json"/utf8>>),
            glint@flag:description(
                _pipe@4,
                <<"Output format: json, yaml, md, prompt"/utf8>>
            )
        end
    ).

-file("src/intent.gleam", 483).
?DOC(" The `lint` command - check for specification anti-patterns\n").
-spec lint_command() -> glint:command(nil).
lint_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Lint_result = intent@spec_linter:lint_spec(Spec),
                            case Lint_result of
                                lint_valid ->
                                    gleam@io:println(
                                        <<"✓ Spec is well-formed - no linting issues found"/utf8>>
                                    ),
                                    intent_ffi:halt(0);

                                {lint_warnings, Warnings} ->
                                    gleam@io:println(
                                        intent@spec_linter:format_warnings(
                                            Warnings
                                        )
                                    ),
                                    intent_ffi:halt(1)
                            end;

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent lint <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Check spec for anti-patterns and quality issues"/utf8>>
    ).

-file("src/intent.gleam", 518).
?DOC(" The `analyze` command - analyze spec quality\n").
-spec analyze_command() -> glint:command(nil).
analyze_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@quality_analyzer:analyze_spec(Spec),
                            gleam@io:println(
                                intent@quality_analyzer:format_report(Report)
                            ),
                            intent_ffi:halt(0);

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent analyze <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Analyze spec quality and provide improvement suggestions"/utf8>>
    ).

-file("src/intent.gleam", 545).
?DOC(" The `improve` command - suggest improvements\n").
-spec improve_command() -> glint:command(nil).
improve_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Quality_report = intent@quality_analyzer:analyze_spec(
                                Spec
                            ),
                            Lint_result = intent@spec_linter:lint_spec(Spec),
                            Context = {improvement_context,
                                Quality_report,
                                Lint_result,
                                Spec},
                            Suggestions = intent@improver:suggest_improvements(
                                Context
                            ),
                            gleam@io:println(
                                intent@improver:format_improvements(Suggestions)
                            ),
                            intent_ffi:halt(0);

                        {error, E} ->
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (intent@loader:format_error(E))/binary>>
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Error: spec file path required"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Usage: intent improve <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Suggest improvements based on quality analysis and linting"/utf8>>
    ).

-file("src/intent.gleam", 746).
-spec run_interview(
    intent@interview:profile(),
    binary(),
    boolean(),
    binary(),
    boolean(),
    boolean(),
    boolean()
) -> nil.
run_interview(
    Profile,
    Answers_file,
    Strict_mode,
    Export_to,
    Light_mode,
    With_context,
    With_analysis
) ->
    Session_id = <<"interview-"/utf8, (intent_ffi:generate_uuid())/binary>>,
    Timestamp = intent_ffi:current_timestamp(),
    Session = intent@interview:create_session(Session_id, Profile, Timestamp),
    Detected_context = case With_context of
        true ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"Scanning codebase for context..."/utf8>>),
            case intent@context_scanner:detect_from_directory(<<"."/utf8>>) of
                {ok, Ctx} ->
                    gleam@io:println(
                        <<"✓ Detected: "/utf8,
                            (intent@context_scanner:language_to_string(
                                erlang:element(2, Ctx)
                            ))/binary>>
                    ),
                    case erlang:element(3, Ctx) of
                        no_framework ->
                            nil;

                        Framework ->
                            gleam@io:println(
                                <<"  Framework: "/utf8,
                                    (intent@context_scanner:framework_to_string(
                                        Framework
                                    ))/binary>>
                            )
                    end,
                    gleam@io:println(
                        <<"  Manifest: "/utf8, (erlang:element(4, Ctx))/binary>>
                    ),
                    {some, Ctx};

                {error, Err} ->
                    gleam@io:println(
                        <<"⚠ Could not detect codebase context: "/utf8,
                            Err/binary>>
                    ),
                    none
            end;

        false ->
            none
    end,
    Answers_dict = case gleam@string:is_empty(Answers_file) of
        true ->
            none;

        false ->
            case intent@answer_loader:load_from_file(Answers_file) of
                {ok, Dict} ->
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<<<<<"✓ Loaded "/utf8,
                                    (gleam@string:inspect(maps:size(Dict)))/binary>>/binary,
                                " pre-filled answers from: "/utf8>>/binary,
                            Answers_file/binary>>
                    ),
                    {some, Dict};

                {error, Err@1} ->
                    case Strict_mode of
                        true ->
                            gleam@io:println_error(
                                <<"✗ Failed to load answers file: "/utf8,
                                    (answer_loader_error_to_string(Err@1))/binary>>
                            ),
                            intent_ffi:halt(4),
                            none;

                        false ->
                            gleam@io:println(
                                <<"⚠ Failed to load answers file: "/utf8,
                                    (answer_loader_error_to_string(Err@1))/binary>>
                            ),
                            gleam@io:println(
                                <<"  Continuing in interactive mode..."/utf8>>
                            ),
                            none
                    end
            end
    end,
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
    ),
    gleam@io:println(<<"                    INTENT INTERVIEW"/utf8>>),
    gleam@io:println(
        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
    ),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"Profile: "/utf8, (profile_to_display_string(Profile))/binary>>
    ),
    gleam@io:println(<<"Session: "/utf8, Session_id/binary>>),
    case Detected_context of
        none ->
            nil;

        {some, Ctx@1} ->
            gleam@io:println(
                <<<<<<<<"Context: "/utf8,
                                (intent@context_scanner:language_to_string(
                                    erlang:element(2, Ctx@1)
                                ))/binary>>/binary,
                            " ("/utf8>>/binary,
                        (erlang:element(4, Ctx@1))/binary>>/binary,
                    ")"/utf8>>
            )
    end,
    case Answers_dict of
        none ->
            nil;

        {some, _} ->
            gleam@io:println(
                <<"Mode: Non-interactive (answers from file)"/utf8>>
            )
    end,
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"This guided interview will help us discover and refine your"/utf8>>
    ),
    gleam@io:println(<<"specification through structured questioning."/utf8>>),
    gleam@io:println(<<""/utf8>>),
    case {Light_mode, With_analysis} of
        {true, _} ->
            gleam@io:println(<<"LIGHT MODE: Quick 5-question interview"/utf8>>),
            gleam@io:println(
                <<"  • Core Intent (what are you building?)"/utf8>>
            ),
            gleam@io:println(<<""/utf8>>);

        {false, true} ->
            gleam@io:println(
                <<"ANALYSIS MODE: Deep dive with inversion/pre-mortem questions"/utf8>>
            ),
            gleam@io:println(
                <<"We'll ask questions across 5 rounds × multiple perspectives:"/utf8>>
            ),
            gleam@io:println(
                <<"  • Round 1: Core Intent (what are you building?)"/utf8>>
            ),
            gleam@io:println(
                <<"  • Round 2: Scope & Boundaries (what's in/out?)"/utf8>>
            ),
            gleam@io:println(
                <<"  • Round 3: Error Cases + Inversions (what can go wrong?)"/utf8>>
            ),
            gleam@io:println(
                <<"  • Round 4: Security + Pre-mortem (how do we keep it safe?)"/utf8>>
            ),
            gleam@io:println(
                <<"  • Round 5: Operations + Second-order effects (production?)"/utf8>>
            ),
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(
                <<"Output will include KIRK analysis fields (inversions, pre_mortem)"/utf8>>
            ),
            gleam@io:println(<<""/utf8>>);

        {false, false} ->
            gleam@io:println(
                <<"We'll ask questions across 5 rounds × multiple perspectives:"/utf8>>
            ),
            gleam@io:println(
                <<"  • Round 1: Core Intent (what are you building?)"/utf8>>
            ),
            gleam@io:println(
                <<"  • Round 2: Scope & Boundaries (what's in/out?)"/utf8>>
            ),
            gleam@io:println(
                <<"  • Round 3: Error Cases (what can go wrong?)"/utf8>>
            ),
            gleam@io:println(
                <<"  • Round 4: Security & Compliance (how do we keep it safe?)"/utf8>>
            ),
            gleam@io:println(
                <<"  • Round 5: Operations (how does it run in production?)"/utf8>>
            ),
            gleam@io:println(<<""/utf8>>)
    end,
    gleam@io:println(<<"Press Ctrl+C to save and exit at any time."/utf8>>),
    gleam@io:println(
        <<"Session will be saved to: .interview/sessions.jsonl"/utf8>>
    ),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(<<"Ready? Let's begin."/utf8>>),
    gleam@io:println(<<""/utf8>>),
    Max_round = case Light_mode of
        true ->
            1;

        false ->
            5
    end,
    Final_session = interview_loop_with_max_round(Session, 1, Max_round),
    Save_result = intent@interview_storage:append_session_to_jsonl(
        Final_session,
        <<".interview/sessions.jsonl"/utf8>>
    ),
    case Save_result of
        {ok, nil} ->
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"✓ Session saved: "/utf8, Session_id/binary>>);

        {error, Err@2} ->
            gleam@io:println_error(
                <<"✗ Failed to save session: "/utf8, Err@2/binary>>
            )
    end,
    case Export_to of
        <<""/utf8>> ->
            nil;

        Path ->
            Spec_cue = case {Light_mode, With_analysis} of
                {true, _} ->
                    intent@spec_builder:build_light_spec_from_session(
                        Final_session
                    );

                {false, true} ->
                    intent@spec_builder:build_spec_from_session_with_analysis(
                        Final_session
                    );

                {false, false} ->
                    intent@spec_builder:build_spec_from_session(Final_session)
            end,
            case simplifile:write(Path, Spec_cue) of
                {ok, nil} ->
                    gleam@io:println(
                        <<"✓ Spec exported to: "/utf8, Path/binary>>
                    ),
                    case {Light_mode, With_analysis} of
                        {true, _} ->
                            gleam@io:println(
                                <<"  (LightSpec format - minimal)"/utf8>>
                            );

                        {false, true} ->
                            gleam@io:println(
                                <<"  (includes KIRK analysis fields)"/utf8>>
                            );

                        {false, false} ->
                            nil
                    end;

                {error, Err@3} ->
                    gleam@io:println_error(
                        <<"✗ Failed to export spec: "/utf8,
                            (gleam@string:inspect(Err@3))/binary>>
                    )
            end
    end,
    intent_ffi:halt(0).

-file("src/intent.gleam", 923).
?DOC(" AI-CUE Protocol: Run interview with CUE output for AI\n").
-spec run_cue_interview(binary(), binary(), binary(), boolean()) -> nil.
run_cue_interview(Profile_str, Session_id, Answer_text, Light_mode) ->
    case intent@ai_cue_protocol:ensure_intent_directory() of
        {error, Err} ->
            gleam@io:println_error(<<"Error: "/utf8, Err/binary>>),
            intent_ffi:halt(4);

        {ok, _} ->
            nil
    end,
    case gleam@string:is_empty(Answer_text) of
        true ->
            case gleam@string:is_empty(Session_id) of
                true ->
                    New_session_id = <<"session-"/utf8,
                        (intent_ffi:generate_uuid())/binary>>,
                    Directive = intent@ai_cue_protocol:create_initial_directive(
                        Profile_str,
                        New_session_id
                    ),
                    Session_path = <<<<".intent/"/utf8, New_session_id/binary>>/binary,
                        ".cue"/utf8>>,
                    Session_cue = build_session_cue(
                        New_session_id,
                        Profile_str,
                        Light_mode
                    ),
                    case simplifile:write(Session_path, Session_cue) of
                        {ok, _} ->
                            gleam@io:println(
                                intent@ai_cue_protocol:directive_to_cue(
                                    Directive
                                )
                            ),
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<"// Session: "/utf8, New_session_id/binary>>
                            ),
                            gleam@io:println(
                                <<<<"// Use: intent interview --cue --session "/utf8,
                                        New_session_id/binary>>/binary,
                                    " --answer 'your answer'"/utf8>>
                            );

                        {error, Err@1} ->
                            gleam@io:println_error(
                                <<"Error creating session file: "/utf8,
                                    (gleam@string:inspect(Err@1))/binary>>
                            ),
                            intent_ffi:halt(4)
                    end;

                false ->
                    Session_path@1 = <<<<".intent/"/utf8, Session_id/binary>>/binary,
                        ".cue"/utf8>>,
                    case simplifile:read(Session_path@1) of
                        {ok, _} ->
                            Directive@1 = intent@ai_cue_protocol:create_initial_directive(
                                Profile_str,
                                Session_id
                            ),
                            gleam@io:println(
                                intent@ai_cue_protocol:directive_to_cue(
                                    Directive@1
                                )
                            );

                        {error, Err@2} ->
                            gleam@io:println_error(
                                <<"Session not found: "/utf8,
                                    Session_id/binary>>
                            ),
                            gleam@io:println_error(
                                <<"Error: "/utf8,
                                    (gleam@string:inspect(Err@2))/binary>>
                            ),
                            intent_ffi:halt(4)
                    end
            end;

        false ->
            case gleam@string:is_empty(Session_id) of
                true ->
                    gleam@io:println_error(
                        <<"Error: --answer requires --session"/utf8>>
                    ),
                    intent_ffi:halt(4);

                false ->
                    case intent@ai_cue_protocol:parse_answer_submission(
                        <<"current-question"/utf8>>,
                        Answer_text,
                        <<"high"/utf8>>
                    ) of
                        {error, Err@3} ->
                            gleam@io:println_error(
                                <<"Invalid answer: "/utf8, Err@3/binary>>
                            ),
                            intent_ffi:halt(4);

                        {ok, Submission} ->
                            Current = intent@ai_cue_protocol:create_initial_directive(
                                Profile_str,
                                Session_id
                            ),
                            Next = intent@ai_cue_protocol:process_answer(
                                Current,
                                Submission
                            ),
                            gleam@io:println(
                                intent@ai_cue_protocol:directive_to_cue(Next)
                            )
                    end
            end
    end,
    intent_ffi:halt(0).

-file("src/intent.gleam", 1050).
?DOC(" Resume an existing interview session\n").
-spec run_resume_interview(binary(), binary()) -> nil.
run_resume_interview(Session_id, Export_to) ->
    Jsonl_path = <<".interview/sessions.jsonl"/utf8>>,
    case intent@interview_storage:get_session_from_jsonl(Jsonl_path, Session_id) of
        {error, Err} ->
            intent@cli_ui:print_error(Err),
            intent_ffi:halt(4);

        {ok, Session} ->
            intent@cli_ui:print_header(
                <<"Resuming Interview: "/utf8,
                    (erlang:element(2, Session))/binary>>
            ),
            intent@cli_ui:print_info(
                <<"Profile: "/utf8,
                    (profile_to_display_string(erlang:element(3, Session)))/binary>>
            ),
            gleam@io:println(<<""/utf8>>),
            gleam@io:println(<<"Progress:"/utf8>>),
            gleam@io:println(
                <<"  • Answers collected: "/utf8,
                    (gleam@string:inspect(
                        erlang:length(erlang:element(9, Session))
                    ))/binary>>
            ),
            gleam@io:println(
                <<"  • Gaps detected: "/utf8,
                    (gleam@string:inspect(
                        erlang:length(erlang:element(10, Session))
                    ))/binary>>
            ),
            gleam@io:println(
                <<"  • Conflicts detected: "/utf8,
                    (gleam@string:inspect(
                        erlang:length(erlang:element(11, Session))
                    ))/binary>>
            ),
            gleam@io:println(<<""/utf8>>),
            Next_round = case erlang:element(8, Session) of
                0 ->
                    1;

                R when R < 5 ->
                    R + 1;

                _ ->
                    5
            end,
            gleam@io:println(
                <<"Resuming from Round "/utf8,
                    (gleam@string:inspect(Next_round))/binary>>
            ),
            gleam@io:println(<<""/utf8>>),
            Final_session = interview_loop(Session, Next_round),
            Save_result = intent@interview_storage:append_session_to_jsonl(
                Final_session,
                Jsonl_path
            ),
            case Save_result of
                {ok, nil} ->
                    gleam@io:println(<<""/utf8>>),
                    intent@cli_ui:print_success(
                        <<"Session updated: "/utf8,
                            (erlang:element(2, Session))/binary>>
                    );

                {error, Err@1} ->
                    intent@cli_ui:print_error(
                        <<"Failed to save session: "/utf8, Err@1/binary>>
                    )
            end,
            case Export_to of
                <<""/utf8>> ->
                    nil;

                Path ->
                    Spec_cue = intent@spec_builder:build_spec_from_session(
                        Final_session
                    ),
                    case simplifile:write(Path, Spec_cue) of
                        {ok, nil} ->
                            intent@cli_ui:print_success(
                                <<"Spec exported to: "/utf8, Path/binary>>
                            );

                        {error, Err@2} ->
                            intent@cli_ui:print_error(
                                <<"Failed to export spec: "/utf8,
                                    (gleam@string:inspect(Err@2))/binary>>
                            )
                    end
            end,
            intent_ffi:halt(0)
    end.

-file("src/intent.gleam", 580).
?DOC(" The `interview` command - guided specification discovery\n").
-spec interview_command() -> glint:command(nil).
interview_command() ->
    _pipe@11 = glint:command(
        fun(Input) ->
            Profile_str = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"profile"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<"api"/utf8>>)
            end,
            Resume_id = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"resume"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            Export_to = begin
                _pipe@2 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"export"/utf8>>
                ),
                gleam@result:unwrap(_pipe@2, <<""/utf8>>)
            end,
            Answers_file = begin
                _pipe@3 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"answers"/utf8>>
                ),
                gleam@result:unwrap(_pipe@3, <<""/utf8>>)
            end,
            Strict_mode = begin
                _pipe@4 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"strict"/utf8>>
                ),
                gleam@result:unwrap(_pipe@4, false)
            end,
            Cue_mode = begin
                _pipe@5 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"cue"/utf8>>
                ),
                gleam@result:unwrap(_pipe@5, false)
            end,
            Session_id_flag = begin
                _pipe@6 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"session"/utf8>>
                ),
                gleam@result:unwrap(_pipe@6, <<""/utf8>>)
            end,
            Answer_text = begin
                _pipe@7 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"answer"/utf8>>
                ),
                gleam@result:unwrap(_pipe@7, <<""/utf8>>)
            end,
            Light_mode = begin
                _pipe@8 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"light"/utf8>>
                ),
                gleam@result:unwrap(_pipe@8, false)
            end,
            With_context = begin
                _pipe@9 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"with-context"/utf8>>
                ),
                gleam@result:unwrap(_pipe@9, false)
            end,
            With_analysis = begin
                _pipe@10 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"with-analysis"/utf8>>
                ),
                gleam@result:unwrap(_pipe@10, false)
            end,
            case Light_mode andalso With_analysis of
                true ->
                    gleam@io:println_error(
                        <<"Error: --light and --with-analysis are mutually exclusive"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"Use --light for quick 5-question interview, or --with-analysis for deep analysis"/utf8>>
                    ),
                    intent_ffi:halt(4);

                false ->
                    nil
            end,
            case Cue_mode of
                true ->
                    run_cue_interview(
                        Profile_str,
                        Session_id_flag,
                        Answer_text,
                        Light_mode
                    );

                false ->
                    case Resume_id of
                        <<""/utf8>> ->
                            case gleam@string:lowercase(Profile_str) of
                                <<"api"/utf8>> ->
                                    run_interview(
                                        api,
                                        Answers_file,
                                        Strict_mode,
                                        Export_to,
                                        Light_mode,
                                        With_context,
                                        With_analysis
                                    );

                                <<"cli"/utf8>> ->
                                    run_interview(
                                        cli,
                                        Answers_file,
                                        Strict_mode,
                                        Export_to,
                                        Light_mode,
                                        With_context,
                                        With_analysis
                                    );

                                <<"event"/utf8>> ->
                                    run_interview(
                                        event,
                                        Answers_file,
                                        Strict_mode,
                                        Export_to,
                                        Light_mode,
                                        With_context,
                                        With_analysis
                                    );

                                <<"data"/utf8>> ->
                                    run_interview(
                                        data,
                                        Answers_file,
                                        Strict_mode,
                                        Export_to,
                                        Light_mode,
                                        With_context,
                                        With_analysis
                                    );

                                <<"workflow"/utf8>> ->
                                    run_interview(
                                        workflow,
                                        Answers_file,
                                        Strict_mode,
                                        Export_to,
                                        Light_mode,
                                        With_context,
                                        With_analysis
                                    );

                                <<"ui"/utf8>> ->
                                    run_interview(
                                        u_i,
                                        Answers_file,
                                        Strict_mode,
                                        Export_to,
                                        Light_mode,
                                        With_context,
                                        With_analysis
                                    );

                                _ ->
                                    gleam@io:println_error(
                                        <<<<"Error: unknown profile '"/utf8,
                                                Profile_str/binary>>/binary,
                                            "'"/utf8>>
                                    ),
                                    gleam@io:println_error(
                                        <<"Valid profiles: api, cli, event, data, workflow, ui"/utf8>>
                                    ),
                                    intent_ffi:halt(4)
                            end;

                        Id ->
                            run_resume_interview(Id, Export_to)
                    end
            end
        end
    ),
    _pipe@12 = glint:description(
        _pipe@11,
        <<"Guided specification discovery through structured interview"/utf8>>
    ),
    _pipe@15 = glint:flag(
        _pipe@12,
        <<"profile"/utf8>>,
        begin
            _pipe@13 = glint@flag:string(),
            _pipe@14 = glint@flag:default(_pipe@13, <<"api"/utf8>>),
            glint@flag:description(
                _pipe@14,
                <<"System profile: api, cli, event, data, workflow, or ui"/utf8>>
            )
        end
    ),
    _pipe@18 = glint:flag(
        _pipe@15,
        <<"resume"/utf8>>,
        begin
            _pipe@16 = glint@flag:string(),
            _pipe@17 = glint@flag:default(_pipe@16, <<""/utf8>>),
            glint@flag:description(
                _pipe@17,
                <<"Resume existing interview session by ID"/utf8>>
            )
        end
    ),
    _pipe@21 = glint:flag(
        _pipe@18,
        <<"answers"/utf8>>,
        begin
            _pipe@19 = glint@flag:string(),
            _pipe@20 = glint@flag:default(_pipe@19, <<""/utf8>>),
            glint@flag:description(
                _pipe@20,
                <<"Path to CUE file with pre-filled answers for non-interactive mode"/utf8>>
            )
        end
    ),
    _pipe@24 = glint:flag(
        _pipe@21,
        <<"strict"/utf8>>,
        begin
            _pipe@22 = glint@flag:bool(),
            _pipe@23 = glint@flag:default(_pipe@22, false),
            glint@flag:description(
                _pipe@23,
                <<"Strict mode: fail if answers file is missing required answers (requires --answers)"/utf8>>
            )
        end
    ),
    _pipe@27 = glint:flag(
        _pipe@24,
        <<"export"/utf8>>,
        begin
            _pipe@25 = glint@flag:string(),
            _pipe@26 = glint@flag:default(_pipe@25, <<""/utf8>>),
            glint@flag:description(
                _pipe@26,
                <<"Export completed interview to spec file"/utf8>>
            )
        end
    ),
    _pipe@30 = glint:flag(
        _pipe@27,
        <<"cue"/utf8>>,
        begin
            _pipe@28 = glint@flag:bool(),
            _pipe@29 = glint@flag:default(_pipe@28, false),
            glint@flag:description(
                _pipe@29,
                <<"Output #AIDirective CUE for AI-driven interview"/utf8>>
            )
        end
    ),
    _pipe@33 = glint:flag(
        _pipe@30,
        <<"session"/utf8>>,
        begin
            _pipe@31 = glint@flag:string(),
            _pipe@32 = glint@flag:default(_pipe@31, <<""/utf8>>),
            glint@flag:description(
                _pipe@32,
                <<"Session ID for AI-CUE protocol (use with --answer)"/utf8>>
            )
        end
    ),
    _pipe@36 = glint:flag(
        _pipe@33,
        <<"answer"/utf8>>,
        begin
            _pipe@34 = glint@flag:string(),
            _pipe@35 = glint@flag:default(_pipe@34, <<""/utf8>>),
            glint@flag:description(
                _pipe@35,
                <<"Submit answer text for AI-CUE protocol (use with --session)"/utf8>>
            )
        end
    ),
    _pipe@39 = glint:flag(
        _pipe@36,
        <<"light"/utf8>>,
        begin
            _pipe@37 = glint@flag:bool(),
            _pipe@38 = glint@flag:default(_pipe@37, false),
            glint@flag:description(
                _pipe@38,
                <<"Light mode: ask only Round 1 questions (5 instead of 25)"/utf8>>
            )
        end
    ),
    _pipe@42 = glint:flag(
        _pipe@39,
        <<"with-context"/utf8>>,
        begin
            _pipe@40 = glint@flag:bool(),
            _pipe@41 = glint@flag:default(_pipe@40, false),
            glint@flag:description(
                _pipe@41,
                <<"Scan codebase for language/framework and include context in spec"/utf8>>
            )
        end
    ),
    glint:flag(
        _pipe@42,
        <<"with-analysis"/utf8>>,
        begin
            _pipe@43 = glint@flag:bool(),
            _pipe@44 = glint@flag:default(_pipe@43, false),
            glint@flag:description(
                _pipe@44,
                <<"Deep analysis: ask inversion/pre-mortem questions, include KIRK fields in spec (mutually exclusive with --light)"/utf8>>
            )
        end
    ).

-file("src/intent.gleam", 1401).
?DOC(" Output beads to .beads/issues.jsonl\n").
-spec output_beads(list(intent@bead_templates:bead_record()), binary()) -> nil.
output_beads(Beads, Source) ->
    Bead_count = erlang:length(Beads),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
    ),
    gleam@io:println(<<"                    BEAD GENERATION"/utf8>>),
    gleam@io:println(
        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
    ),
    gleam@io:println(<<""/utf8>>),
    gleam@io:println(
        <<<<<<"Generated "/utf8, (gleam@string:inspect(Bead_count))/binary>>/binary,
                " work items from "/utf8>>/binary,
            Source/binary>>
    ),
    gleam@io:println(<<""/utf8>>),
    Jsonl_output = intent@bead_templates:beads_to_jsonl(Beads),
    case simplifile:append(
        <<".beads/issues.jsonl"/utf8>>,
        <<Jsonl_output/binary, "\n"/utf8>>
    ) of
        {ok, nil} ->
            gleam@io:println(
                <<"✓ Beads exported to: .beads/issues.jsonl"/utf8>>
            ),
            gleam@io:println(<<""/utf8>>),
            Stats = intent@bead_templates:bead_stats(Beads),
            gleam@io:println(<<"Summary:"/utf8>>),
            gleam@io:println(
                <<"  Total beads: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Stats)))/binary>>
            ),
            intent_ffi:halt(0);

        {error, Err} ->
            gleam@io:println_error(
                <<"✗ Failed to write beads: "/utf8,
                    (gleam@string:inspect(Err))/binary>>
            ),
            intent_ffi:halt(4)
    end.

-file("src/intent.gleam", 1324).
?DOC(" Generate beads from an interview session\n").
-spec generate_beads_from_session(binary()) -> nil.
generate_beads_from_session(Session_id) ->
    case intent@interview_storage:get_session_from_jsonl(
        <<".interview/sessions.jsonl"/utf8>>,
        Session_id
    ) of
        {error, Err} ->
            gleam@io:println_error(<<"Error: "/utf8, Err/binary>>),
            intent_ffi:halt(4);

        {ok, Session} ->
            Beads = intent@bead_templates:generate_beads_from_session(Session),
            output_beads(Beads, <<"session: "/utf8, Session_id/binary>>)
    end.

-file("src/intent.gleam", 1344).
?DOC(
    " Generate beads directly from a spec file (DIRECT-SPEC feature)\n"
    " Allows experienced devs to skip interview and write specs directly\n"
).
-spec generate_beads_from_spec(binary()) -> nil.
generate_beads_from_spec(Spec_path) ->
    case intent@loader:load_spec(Spec_path) of
        {error, Err} ->
            gleam@io:println_error(
                <<"Error loading spec: "/utf8,
                    (intent@loader:format_error(Err))/binary>>
            ),
            intent_ffi:halt(4);

        {ok, Spec} ->
            Beads = spec_to_beads(Spec),
            output_beads(Beads, <<"spec: "/utf8, Spec_path/binary>>)
    end.

-file("src/intent.gleam", 1298).
?DOC(
    " The `beads` command - generate work items from interview session or spec file\n"
    " Supports two modes:\n"
    " 1. intent beads <session_id> - from interview session\n"
    " 2. intent beads <spec.cue> - directly from spec file (DIRECT-SPEC)\n"
).
-spec beads_command() -> glint:command(nil).
beads_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Source | _] ->
                    case gleam@string:ends_with(Source, <<".cue"/utf8>>) of
                        true ->
                            generate_beads_from_spec(Source);

                        false ->
                            generate_beads_from_session(Source)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Usage: intent beads <session_id|spec.cue>"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(
                        <<"Generate beads from interview session:"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  intent beads interview-abc123def456"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(
                        <<"Generate beads directly from spec file (no interview needed):"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  intent beads my-api-spec.cue"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"Generate work items (beads) from session or spec file"/utf8>>
    ).

-file("src/intent.gleam", 1435).
?DOC(" Mark a bead with execution status (success/failed/blocked)\n").
-spec bead_status_command() -> glint:command(nil).
bead_status_command() ->
    _pipe@4 = glint:command(
        fun(Input) ->
            Bead_id = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"bead-id"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<""/utf8>>)
            end,
            Status = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"status"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            Reason = begin
                _pipe@2 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"reason"/utf8>>
                ),
                gleam@result:unwrap(_pipe@2, <<""/utf8>>)
            end,
            Session_id = begin
                _pipe@3 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"session"/utf8>>
                ),
                gleam@result:unwrap(_pipe@3, <<""/utf8>>)
            end,
            case gleam@string:is_empty(Bead_id) of
                true ->
                    gleam@io:println_error(
                        <<"Usage: intent bead-status --bead-id <id> --status success|failed|blocked [--reason 'text'] [--session <id>]"/utf8>>
                    ),
                    intent_ffi:halt(4);

                false ->
                    case Status of
                        <<"success"/utf8>> ->
                            case intent@bead_feedback:mark_bead_executed(
                                Session_id,
                                Bead_id,
                                success,
                                Reason,
                                0
                            ) of
                                {ok, nil} ->
                                    gleam@io:println(
                                        <<<<"✓ Bead "/utf8, Bead_id/binary>>/binary,
                                            " marked as success"/utf8>>
                                    ),
                                    intent_ffi:halt(0);

                                {error, Err} ->
                                    gleam@io:println_error(
                                        <<"✗ Failed to mark bead: "/utf8,
                                            (bead_feedback_error_to_string(Err))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end;

                        <<"failed"/utf8>> ->
                            case intent@bead_feedback:mark_bead_failed(
                                Session_id,
                                Bead_id,
                                Reason,
                                <<"execution_error"/utf8>>,
                                <<"Bead execution failed"/utf8>>,
                                none,
                                0
                            ) of
                                {ok, nil} ->
                                    gleam@io:println(
                                        <<<<"✓ Bead "/utf8, Bead_id/binary>>/binary,
                                            " marked as failed"/utf8>>
                                    ),
                                    intent_ffi:halt(0);

                                {error, Err@1} ->
                                    gleam@io:println_error(
                                        <<"✗ Failed to mark bead: "/utf8,
                                            (bead_feedback_error_to_string(
                                                Err@1
                                            ))/binary>>
                                    ),
                                    intent_ffi:halt(4)
                            end;

                        <<"blocked"/utf8>> ->
                            case gleam@string:is_empty(Reason) of
                                true ->
                                    gleam@io:println_error(
                                        <<"Error: --status blocked requires --reason"/utf8>>
                                    ),
                                    intent_ffi:halt(4);

                                false ->
                                    case intent@bead_feedback:mark_bead_blocked(
                                        Session_id,
                                        Bead_id,
                                        Reason,
                                        <<"user_action"/utf8>>,
                                        <<"User blocked this bead"/utf8>>,
                                        <<"Manual resume required"/utf8>>,
                                        0
                                    ) of
                                        {ok, nil} ->
                                            gleam@io:println(
                                                <<<<<<"✓ Bead "/utf8,
                                                            Bead_id/binary>>/binary,
                                                        " marked as blocked: "/utf8>>/binary,
                                                    Reason/binary>>
                                            ),
                                            intent_ffi:halt(0);

                                        {error, Err@2} ->
                                            gleam@io:println_error(
                                                <<"✗ Failed to mark bead: "/utf8,
                                                    (bead_feedback_error_to_string(
                                                        Err@2
                                                    ))/binary>>
                                            ),
                                            intent_ffi:halt(4)
                                    end
                            end;

                        _ ->
                            gleam@io:println_error(
                                <<<<"Error: invalid status '"/utf8,
                                        Status/binary>>/binary,
                                    "'"/utf8>>
                            ),
                            gleam@io:println_error(
                                <<"Valid statuses: success, failed, blocked"/utf8>>
                            ),
                            intent_ffi:halt(4)
                    end
            end
        end
    ),
    _pipe@5 = glint:description(
        _pipe@4,
        <<"Mark bead execution status (success/failed/blocked)"/utf8>>
    ),
    _pipe@8 = glint:flag(
        _pipe@5,
        <<"bead-id"/utf8>>,
        begin
            _pipe@6 = glint@flag:string(),
            _pipe@7 = glint@flag:default(_pipe@6, <<""/utf8>>),
            glint@flag:description(_pipe@7, <<"Bead ID (required)"/utf8>>)
        end
    ),
    _pipe@11 = glint:flag(
        _pipe@8,
        <<"status"/utf8>>,
        begin
            _pipe@9 = glint@flag:string(),
            _pipe@10 = glint@flag:default(_pipe@9, <<""/utf8>>),
            glint@flag:description(
                _pipe@10,
                <<"Status: success, failed, or blocked (required)"/utf8>>
            )
        end
    ),
    _pipe@14 = glint:flag(
        _pipe@11,
        <<"reason"/utf8>>,
        begin
            _pipe@12 = glint@flag:string(),
            _pipe@13 = glint@flag:default(_pipe@12, <<""/utf8>>),
            glint@flag:description(
                _pipe@13,
                <<"Reason for status (required for blocked)"/utf8>>
            )
        end
    ),
    glint:flag(
        _pipe@14,
        <<"session"/utf8>>,
        begin
            _pipe@15 = glint@flag:string(),
            _pipe@16 = glint@flag:default(_pipe@15, <<""/utf8>>),
            glint@flag:description(_pipe@16, <<"Session ID"/utf8>>)
        end
    ).

-file("src/intent.gleam", 1525).
?DOC(" The `plan` command - display execution plan for a session\n").
-spec plan_command() -> glint:command(nil).
plan_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Format = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"format"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<"human"/utf8>>)
            end,
            case erlang:element(2, Input) of
                [Session_id | _] ->
                    case intent@plan_mode:compute_plan(Session_id) of
                        {error, Err} ->
                            gleam@io:println_error(
                                intent@plan_mode:format_error(Err)
                            ),
                            intent_ffi:halt(4);

                        {ok, Plan} ->
                            Output = case Format of
                                <<"json"/utf8>> ->
                                    intent@plan_mode:format_plan_json(Plan);

                                _ ->
                                    intent@plan_mode:format_plan_human(Plan)
                            end,
                            gleam@io:println(Output),
                            intent_ffi:halt(0)
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Usage: intent plan <session_id> [--format human|json]"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(
                        <<"Display execution plan from session beads."/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(<<"Examples:"/utf8>>),
                    gleam@io:println_error(
                        <<"  intent plan abc123              # Human-readable output"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  intent plan abc123 --format json  # JSON output"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(
        _pipe@1,
        <<"Display execution plan from session beads"/utf8>>
    ),
    glint:flag(
        _pipe@2,
        <<"format"/utf8>>,
        begin
            _pipe@3 = glint@flag:string(),
            _pipe@4 = glint@flag:default(_pipe@3, <<"human"/utf8>>),
            glint@flag:description(
                _pipe@4,
                <<"Output format: human or json"/utf8>>
            )
        end
    ).

-file("src/intent.gleam", 1565).
?DOC(" The `plan-approve` command - approve execution plan for CI/automation\n").
-spec plan_approve_command() -> glint:command(nil).
plan_approve_command() ->
    _pipe@2 = glint:command(
        fun(Input) ->
            Auto_approve = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"yes"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            Notes = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"notes"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            case erlang:element(2, Input) of
                [Session_id | _] ->
                    case intent@plan_mode:compute_plan(Session_id) of
                        {error, Err} ->
                            gleam@io:println_error(
                                intent@plan_mode:format_error(Err)
                            ),
                            intent_ffi:halt(4);

                        {ok, Plan} ->
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<"═══════════════════════════════════════════════════════════════════"/utf8>>
                            ),
                            gleam@io:println(
                                <<"                    PLAN APPROVAL"/utf8>>
                            ),
                            gleam@io:println(
                                <<"═══════════════════════════════════════════════════════════════════"/utf8>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<"Session: "/utf8,
                                    (erlang:element(2, Plan))/binary>>
                            ),
                            gleam@io:println(
                                <<"Total Beads: "/utf8,
                                    (gleam@string:inspect(
                                        erlang:element(5, Plan)
                                    ))/binary>>
                            ),
                            gleam@io:println(
                                <<"Total Effort: "/utf8,
                                    (erlang:element(6, Plan))/binary>>
                            ),
                            gleam@io:println(
                                <<"Risk Level: "/utf8,
                                    (risk_level_to_string(
                                        erlang:element(7, Plan)
                                    ))/binary>>
                            ),
                            gleam@io:println(
                                <<"Phases: "/utf8,
                                    (gleam@string:inspect(
                                        erlang:length(erlang:element(4, Plan))
                                    ))/binary>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            case gleam@list:is_empty(erlang:element(8, Plan)) of
                                true ->
                                    nil;

                                false ->
                                    gleam@io:println(<<"⚠ BLOCKERS:"/utf8>>),
                                    gleam@list:each(
                                        erlang:element(8, Plan),
                                        fun(B) ->
                                            gleam@io:println(
                                                <<"  • "/utf8, B/binary>>
                                            )
                                        end
                                    ),
                                    gleam@io:println(<<""/utf8>>)
                            end,
                            case Auto_approve of
                                true ->
                                    case approve_plan(
                                        Session_id,
                                        <<"ci"/utf8>>,
                                        Notes
                                    ) of
                                        {ok, nil} ->
                                            gleam@io:println(
                                                <<"✓ Plan approved automatically (CI mode)"/utf8>>
                                            ),
                                            intent_ffi:halt(0);

                                        {error, Err@1} ->
                                            gleam@io:println_error(
                                                <<"✗ Failed to approve plan: "/utf8,
                                                    Err@1/binary>>
                                            ),
                                            intent_ffi:halt(4)
                                    end;

                                false ->
                                    gleam@io:println(
                                        <<"Approve this plan? (yes/no)"/utf8>>
                                    ),
                                    case intent_ffi_stdin:read_line() of
                                        {ok, Response} ->
                                            Cleaned = gleam@string:trim(
                                                gleam@string:lowercase(Response)
                                            ),
                                            case Cleaned of
                                                <<"yes"/utf8>> ->
                                                    case approve_plan(
                                                        Session_id,
                                                        <<"human"/utf8>>,
                                                        Notes
                                                    ) of
                                                        {ok, nil} ->
                                                            gleam@io:println(
                                                                <<"✓ Plan approved"/utf8>>
                                                            ),
                                                            intent_ffi:halt(0);

                                                        {error, Err@2} ->
                                                            gleam@io:println_error(
                                                                <<"✗ Failed to approve plan: "/utf8,
                                                                    Err@2/binary>>
                                                            ),
                                                            intent_ffi:halt(4)
                                                    end;

                                                <<"y"/utf8>> ->
                                                    case approve_plan(
                                                        Session_id,
                                                        <<"human"/utf8>>,
                                                        Notes
                                                    ) of
                                                        {ok, nil} ->
                                                            gleam@io:println(
                                                                <<"✓ Plan approved"/utf8>>
                                                            ),
                                                            intent_ffi:halt(0);

                                                        {error, Err@2} ->
                                                            gleam@io:println_error(
                                                                <<"✗ Failed to approve plan: "/utf8,
                                                                    Err@2/binary>>
                                                            ),
                                                            intent_ffi:halt(4)
                                                    end;

                                                <<"no"/utf8>> ->
                                                    gleam@io:println(
                                                        <<"Plan not approved"/utf8>>
                                                    ),
                                                    intent_ffi:halt(1);

                                                <<"n"/utf8>> ->
                                                    gleam@io:println(
                                                        <<"Plan not approved"/utf8>>
                                                    ),
                                                    intent_ffi:halt(1);

                                                _ ->
                                                    gleam@io:println_error(
                                                        <<"Invalid response. Please enter 'yes' or 'no'"/utf8>>
                                                    ),
                                                    intent_ffi:halt(4)
                                            end;

                                        {error, _} ->
                                            gleam@io:println_error(
                                                <<"Failed to read input"/utf8>>
                                            ),
                                            intent_ffi:halt(4)
                                    end
                            end
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Usage: intent plan-approve <session_id> [--yes] [--notes 'text']"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(
                        <<"Approve execution plan for a session."/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(<<"Flags:"/utf8>>),
                    gleam@io:println_error(
                        <<"  --yes      Auto-approve for CI pipelines (non-interactive)"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  --notes    Optional approval notes"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(<<"Examples:"/utf8>>),
                    gleam@io:println_error(
                        <<"  intent plan-approve abc123           # Interactive approval"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  intent plan-approve abc123 --yes     # CI auto-approval"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@3 = glint:description(
        _pipe@2,
        <<"Approve execution plan for session"/utf8>>
    ),
    _pipe@6 = glint:flag(
        _pipe@3,
        <<"yes"/utf8>>,
        begin
            _pipe@4 = glint@flag:bool(),
            _pipe@5 = glint@flag:default(_pipe@4, false),
            glint@flag:description(
                _pipe@5,
                <<"Auto-approve for CI (non-interactive)"/utf8>>
            )
        end
    ),
    glint:flag(
        _pipe@6,
        <<"notes"/utf8>>,
        begin
            _pipe@7 = glint@flag:string(),
            _pipe@8 = glint@flag:default(_pipe@7, <<""/utf8>>),
            glint@flag:description(_pipe@8, <<"Approval notes"/utf8>>)
        end
    ).

-file("src/intent.gleam", 1729).
?DOC(" The `beads-regenerate` command - regenerate failed/blocked beads\n").
-spec beads_regenerate_command() -> glint:command(nil).
beads_regenerate_command() ->
    _pipe@2 = glint:command(
        fun(Input) ->
            Strategy = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"strategy"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<"hybrid"/utf8>>)
            end,
            case erlang:element(2, Input) of
                [Session_id | _] ->
                    Session_path = <<<<".intent/session-"/utf8,
                            Session_id/binary>>/binary,
                        ".cue"/utf8>>,
                    case simplifile:verify_is_file(Session_path) of
                        {error, _} ->
                            gleam@io:println_error(
                                <<"Session not found: "/utf8,
                                    Session_id/binary>>
                            ),
                            gleam@io:println_error(
                                <<"Expected file: "/utf8, Session_path/binary>>
                            ),
                            intent_ffi:halt(4);

                        {ok, _} ->
                            case intent@bead_feedback:load_feedback_for_session(
                                Session_id
                            ) of
                                {error, Err} ->
                                    gleam@io:println_error(
                                        <<"Failed to load feedback: "/utf8,
                                            (bead_feedback_error_to_string(Err))/binary>>
                                    ),
                                    intent_ffi:halt(4);

                                {ok, Feedback} ->
                                    Needs_regen = begin
                                        _pipe@1 = Feedback,
                                        gleam@list:filter(
                                            _pipe@1,
                                            fun(Fb) ->
                                                case erlang:element(3, Fb) of
                                                    failed ->
                                                        true;

                                                    blocked ->
                                                        true;

                                                    _ ->
                                                        false
                                                end
                                            end
                                        )
                                    end,
                                    gleam@io:println(<<""/utf8>>),
                                    gleam@io:println(
                                        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
                                    ),
                                    gleam@io:println(
                                        <<"                    BEAD REGENERATION"/utf8>>
                                    ),
                                    gleam@io:println(
                                        <<"═══════════════════════════════════════════════════════════════════"/utf8>>
                                    ),
                                    gleam@io:println(<<""/utf8>>),
                                    gleam@io:println(
                                        <<"Session: "/utf8, Session_id/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"Strategy: "/utf8, Strategy/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"Feedback entries: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:length(Feedback)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"Beads needing regeneration: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:length(Needs_regen)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(<<""/utf8>>),
                                    case gleam@list:is_empty(Needs_regen) of
                                        true ->
                                            gleam@io:println(
                                                <<"✓ No beads need regeneration - all passed or skipped"/utf8>>
                                            ),
                                            intent_ffi:halt(0);

                                        false ->
                                            gleam@io:println(
                                                <<"Beads to regenerate:"/utf8>>
                                            ),
                                            gleam@list:each(
                                                Needs_regen,
                                                fun(Fb@1) ->
                                                    Status_icon = case erlang:element(
                                                        3,
                                                        Fb@1
                                                    ) of
                                                        failed ->
                                                            <<"✗"/utf8>>;

                                                        blocked ->
                                                            <<"⊘"/utf8>>;

                                                        _ ->
                                                            <<"?"/utf8>>
                                                    end,
                                                    gleam@io:println(
                                                        <<<<<<<<<<"  "/utf8,
                                                                            Status_icon/binary>>/binary,
                                                                        " "/utf8>>/binary,
                                                                    (erlang:element(
                                                                        2,
                                                                        Fb@1
                                                                    ))/binary>>/binary,
                                                                ": "/utf8>>/binary,
                                                            (erlang:element(
                                                                4,
                                                                Fb@1
                                                            ))/binary>>
                                                    )
                                                end
                                            ),
                                            gleam@io:println(<<""/utf8>>),
                                            Regen_entries = generate_regeneration_entries(
                                                Needs_regen,
                                                Strategy
                                            ),
                                            case append_regeneration_to_session(
                                                Session_path,
                                                Regen_entries
                                            ) of
                                                {ok, nil} ->
                                                    gleam@io:println(
                                                        <<"✓ Regeneration metadata added to session"/utf8>>
                                                    ),
                                                    gleam@io:println(
                                                        <<"  Strategy: "/utf8,
                                                            Strategy/binary>>
                                                    ),
                                                    gleam@io:println(
                                                        <<"  Beads marked for regeneration: "/utf8,
                                                            (gleam@string:inspect(
                                                                erlang:length(
                                                                    Needs_regen
                                                                )
                                                            ))/binary>>
                                                    ),
                                                    gleam@io:println(
                                                        <<""/utf8>>
                                                    ),
                                                    gleam@io:println(
                                                        <<"Next steps:"/utf8>>
                                                    ),
                                                    gleam@io:println(
                                                        <<"  1. Review regeneration suggestions in "/utf8,
                                                            Session_path/binary>>
                                                    ),
                                                    gleam@io:println(
                                                        <<<<"  2. Run 'intent plan "/utf8,
                                                                Session_id/binary>>/binary,
                                                            "' to see updated plan"/utf8>>
                                                    ),
                                                    gleam@io:println(
                                                        <<"  3. Execute regenerated beads"/utf8>>
                                                    ),
                                                    intent_ffi:halt(0);

                                                {error, Err@1} ->
                                                    gleam@io:println_error(
                                                        <<"✗ Failed to update session: "/utf8,
                                                            Err@1/binary>>
                                                    ),
                                                    intent_ffi:halt(4)
                                            end
                                    end
                            end
                    end;

                [] ->
                    gleam@io:println_error(
                        <<"Usage: intent beads-regenerate <session_id> [--strategy hybrid|inversion|premortem]"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(
                        <<"Regenerate failed/blocked beads with adjusted approach."/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(<<"Strategies:"/utf8>>),
                    gleam@io:println_error(
                        <<"  hybrid     - Use all analysis methods (default)"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  inversion  - Focus on failure mode analysis"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  premortem  - Focus on what could go wrong"/utf8>>
                    ),
                    gleam@io:println_error(<<""/utf8>>),
                    gleam@io:println_error(<<"Examples:"/utf8>>),
                    gleam@io:println_error(
                        <<"  intent beads-regenerate abc123"/utf8>>
                    ),
                    gleam@io:println_error(
                        <<"  intent beads-regenerate abc123 --strategy inversion"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@3 = glint:description(
        _pipe@2,
        <<"Regenerate failed/blocked beads with adjusted approach"/utf8>>
    ),
    glint:flag(
        _pipe@3,
        <<"strategy"/utf8>>,
        begin
            _pipe@4 = glint@flag:string(),
            _pipe@5 = glint@flag:default(_pipe@4, <<"hybrid"/utf8>>),
            glint@flag:description(
                _pipe@5,
                <<"Regeneration strategy: hybrid, inversion, or premortem"/utf8>>
            )
        end
    ).

-file("src/intent.gleam", 1897).
?DOC(" The `history` command - view session snapshot history\n").
-spec history_command() -> glint:command(nil).
history_command() ->
    _pipe = glint:command(
        fun(Input) ->
            History_path = <<".interview/history.jsonl"/utf8>>,
            case erlang:element(2, Input) of
                [Session_id | _] ->
                    case intent@interview_storage:list_session_history(
                        History_path,
                        Session_id
                    ) of
                        {error, Err} ->
                            intent@cli_ui:print_error(Err),
                            intent_ffi:halt(4);

                        {ok, []} ->
                            intent@cli_ui:print_warning(
                                <<"No history found for session: "/utf8,
                                    Session_id/binary>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            gleam@io:println(
                                <<"Tip: Session history is recorded when you save snapshots"/utf8>>
                            ),
                            gleam@io:println(
                                <<"during an interview with --snapshot flag."/utf8>>
                            ),
                            intent_ffi:halt(0);

                        {ok, Snapshots} ->
                            intent@cli_ui:print_header(
                                <<"Session History: "/utf8, Session_id/binary>>
                            ),
                            gleam@io:println(<<""/utf8>>),
                            gleam@list:each(
                                Snapshots,
                                fun(Snapshot) ->
                                    gleam@io:println(
                                        <<"┌─ "/utf8,
                                            (erlang:element(3, Snapshot))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Time: "/utf8,
                                            (erlang:element(4, Snapshot))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Stage: "/utf8,
                                            (erlang:element(9, Snapshot))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Description: "/utf8,
                                            (erlang:element(5, Snapshot))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Answers: "/utf8,
                                            (gleam@string:inspect(
                                                maps:size(
                                                    erlang:element(6, Snapshot)
                                                )
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Gaps: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:element(7, Snapshot)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(
                                        <<"│  Conflicts: "/utf8,
                                            (gleam@string:inspect(
                                                erlang:element(8, Snapshot)
                                            ))/binary>>
                                    ),
                                    gleam@io:println(<<"└─"/utf8>>),
                                    gleam@io:println(<<""/utf8>>)
                                end
                            ),
                            intent_ffi:halt(0)
                    end;

                [] ->
                    intent@cli_ui:print_error(<<"Session ID required"/utf8>>),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Usage: intent history <session-id>"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Example: intent history interview-abc123"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    glint:description(
        _pipe,
        <<"View snapshot history for an interview session"/utf8>>
    ).

-file("src/intent.gleam", 1949).
?DOC(" The `diff` command - compare two sessions\n").
-spec diff_command() -> glint:command(nil).
diff_command() ->
    _pipe = glint:command(
        fun(Input) ->
            Jsonl_path = <<".interview/sessions.jsonl"/utf8>>,
            case erlang:element(2, Input) of
                [From_id, To_id | _] ->
                    case intent@interview_storage:get_session_from_jsonl(
                        Jsonl_path,
                        From_id
                    ) of
                        {error, Err} ->
                            intent@cli_ui:print_error(
                                <<"Failed to load 'from' session: "/utf8,
                                    Err/binary>>
                            ),
                            intent_ffi:halt(4);

                        {ok, From_session} ->
                            case intent@interview_storage:get_session_from_jsonl(
                                Jsonl_path,
                                To_id
                            ) of
                                {error, Err@1} ->
                                    intent@cli_ui:print_error(
                                        <<"Failed to load 'to' session: "/utf8,
                                            Err@1/binary>>
                                    ),
                                    intent_ffi:halt(4);

                                {ok, To_session} ->
                                    Diff = intent@interview_storage:diff_sessions(
                                        From_session,
                                        To_session
                                    ),
                                    intent@cli_ui:print_header(
                                        <<"Session Comparison"/utf8>>
                                    ),
                                    gleam@io:println(<<""/utf8>>),
                                    gleam@io:println(
                                        intent@interview_storage:format_diff(
                                            Diff
                                        )
                                    ),
                                    gleam@io:println(<<""/utf8>>),
                                    Total_changes = (erlang:length(
                                        erlang:element(6, Diff)
                                    )
                                    + erlang:length(erlang:element(7, Diff)))
                                    + erlang:length(erlang:element(8, Diff)),
                                    case Total_changes of
                                        0 ->
                                            intent@cli_ui:print_info(
                                                <<"No answer changes between sessions"/utf8>>
                                            );

                                        N ->
                                            intent@cli_ui:print_info(
                                                <<(gleam@string:inspect(N))/binary,
                                                    " total answer changes"/utf8>>
                                            )
                                    end,
                                    intent_ffi:halt(0)
                            end
                    end;

                [Single_id] ->
                    intent@cli_ui:print_error(
                        <<"Two session IDs required for comparison"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Usage: intent diff <from-session> <to-session>"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Tip: Use 'intent sessions' to list available sessions"/utf8>>
                    ),
                    gleam@io:println(
                        <<"     Session provided: "/utf8, Single_id/binary>>
                    ),
                    intent_ffi:halt(4);

                [] ->
                    intent@cli_ui:print_error(<<"Session IDs required"/utf8>>),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Usage: intent diff <from-session> <to-session>"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(
                        <<"Compare two interview sessions and show differences"/utf8>>
                    ),
                    gleam@io:println(
                        <<"in answers, gaps, conflicts, and stage."/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(<<"Example:"/utf8>>),
                    gleam@io:println(
                        <<"  intent diff interview-abc123 interview-def456"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    glint:description(
        _pipe,
        <<"Compare two interview sessions and show differences"/utf8>>
    ).

-file("src/intent.gleam", 2121).
?DOC(" The `quality` command - KIRK quality analysis\n").
-spec kirk_quality_command() -> glint:command(nil).
kirk_quality_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@kirk@quality_analyzer:analyze_quality(
                                Spec
                            ),
                            case Is_json of
                                true ->
                                    Json_obj = gleam@json:object(
                                        [{<<"completeness"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(2, Report)
                                                )},
                                            {<<"consistency"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(3, Report)
                                                )},
                                            {<<"testability"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(4, Report)
                                                )},
                                            {<<"clarity"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(5, Report)
                                                )},
                                            {<<"security"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(6, Report)
                                                )},
                                            {<<"overall"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(7, Report)
                                                )},
                                            {<<"issues"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(8, Report),
                                                    fun(I) ->
                                                        gleam@json:object(
                                                            [{<<"field"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            2,
                                                                            I
                                                                        )
                                                                    )},
                                                                {<<"issue"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            3,
                                                                            I
                                                                        )
                                                                    )},
                                                                {<<"severity"/utf8>>,
                                                                    gleam@json:string(
                                                                        intent@kirk@quality_analyzer:severity_to_string(
                                                                            erlang:element(
                                                                                4,
                                                                                I
                                                                            )
                                                                        )
                                                                    )}]
                                                        )
                                                    end
                                                )}]
                                    ),
                                    gleam@io:println(
                                        gleam@json:to_string(Json_obj)
                                    );

                                false ->
                                    gleam@io:println(
                                        intent@kirk@quality_analyzer:format_report(
                                            Report
                                        )
                                    )
                            end,
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent quality <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(
        _pipe@1,
        <<"(Optional analysis) KIRK: Analyze spec quality across multiple dimensions"/utf8>>
    ),
    glint:flag(
        _pipe@2,
        <<"json"/utf8>>,
        begin
            _pipe@3 = glint@flag:bool(),
            _pipe@4 = glint@flag:default(_pipe@3, false),
            glint@flag:description(_pipe@4, <<"Output as JSON"/utf8>>)
        end
    ).

-file("src/intent.gleam", 2173).
?DOC(" The `invert` command - KIRK inversion analysis\n").
-spec kirk_invert_command() -> glint:command(nil).
kirk_invert_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@kirk@inversion_checker:analyze_inversions(
                                Spec
                            ),
                            case Is_json of
                                true ->
                                    Json_obj = gleam@json:object(
                                        [{<<"score"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(6, Report)
                                                )},
                                            {<<"security_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(2, Report),
                                                    fun gap_to_json/1
                                                )},
                                            {<<"usability_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(3, Report),
                                                    fun gap_to_json/1
                                                )},
                                            {<<"integration_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(4, Report),
                                                    fun gap_to_json/1
                                                )},
                                            {<<"suggested_behaviors"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(5, Report),
                                                    fun(S) ->
                                                        gleam@json:object(
                                                            [{<<"name"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            2,
                                                                            S
                                                                        )
                                                                    )},
                                                                {<<"intent"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            3,
                                                                            S
                                                                        )
                                                                    )},
                                                                {<<"expected_status"/utf8>>,
                                                                    gleam@json:int(
                                                                        erlang:element(
                                                                            6,
                                                                            S
                                                                        )
                                                                    )},
                                                                {<<"category"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            7,
                                                                            S
                                                                        )
                                                                    )}]
                                                        )
                                                    end
                                                )}]
                                    ),
                                    gleam@io:println(
                                        gleam@json:to_string(Json_obj)
                                    );

                                false ->
                                    gleam@io:println(
                                        intent@kirk@inversion_checker:format_report(
                                            Report
                                        )
                                    )
                            end,
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent invert <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(
        _pipe@1,
        <<"(Optional analysis) KIRK: Inversion analysis - what failure cases are missing?"/utf8>>
    ),
    glint:flag(
        _pipe@2,
        <<"json"/utf8>>,
        begin
            _pipe@3 = glint@flag:bool(),
            _pipe@4 = glint@flag:default(_pipe@3, false),
            glint@flag:description(_pipe@4, <<"Output as JSON"/utf8>>)
        end
    ).

-file("src/intent.gleam", 2233).
?DOC(" The `coverage` command - KIRK coverage analysis\n").
-spec kirk_coverage_command() -> glint:command(nil).
kirk_coverage_command() ->
    _pipe@5 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@kirk@coverage_analyzer:analyze_coverage(
                                Spec
                            ),
                            case Is_json of
                                true ->
                                    Json_obj = gleam@json:object(
                                        [{<<"overall_score"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(7, Report)
                                                )},
                                            {<<"methods"/utf8>>,
                                                gleam@json:object(
                                                    begin
                                                        _pipe@1 = erlang:element(
                                                            2,
                                                            Report
                                                        ),
                                                        _pipe@2 = maps:to_list(
                                                            _pipe@1
                                                        ),
                                                        gleam@list:map(
                                                            _pipe@2,
                                                            fun(Pair) ->
                                                                {erlang:element(
                                                                        1,
                                                                        Pair
                                                                    ),
                                                                    gleam@json:int(
                                                                        erlang:element(
                                                                            2,
                                                                            Pair
                                                                        )
                                                                    )}
                                                            end
                                                        )
                                                    end
                                                )},
                                            {<<"status_codes"/utf8>>,
                                                gleam@json:object(
                                                    begin
                                                        _pipe@3 = erlang:element(
                                                            3,
                                                            Report
                                                        ),
                                                        _pipe@4 = maps:to_list(
                                                            _pipe@3
                                                        ),
                                                        gleam@list:map(
                                                            _pipe@4,
                                                            fun(Pair@1) ->
                                                                {erlang:element(
                                                                        1,
                                                                        Pair@1
                                                                    ),
                                                                    gleam@json:int(
                                                                        erlang:element(
                                                                            2,
                                                                            Pair@1
                                                                        )
                                                                    )}
                                                            end
                                                        )
                                                    end
                                                )},
                                            {<<"owasp_score"/utf8>>,
                                                gleam@json:float(
                                                    erlang:element(
                                                        3,
                                                        erlang:element(
                                                            6,
                                                            Report
                                                        )
                                                    )
                                                )},
                                            {<<"owasp_missing"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(
                                                        4,
                                                        erlang:element(
                                                            6,
                                                            Report
                                                        )
                                                    ),
                                                    fun gleam@json:string/1
                                                )}]
                                    ),
                                    gleam@io:println(
                                        gleam@json:to_string(Json_obj)
                                    );

                                false ->
                                    gleam@io:println(
                                        intent@kirk@coverage_analyzer:format_report(
                                            Report
                                        )
                                    )
                            end,
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent coverage <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@6 = glint:description(
        _pipe@5,
        <<"(Optional analysis) KIRK: Coverage analysis including OWASP Top 10"/utf8>>
    ),
    glint:flag(
        _pipe@6,
        <<"json"/utf8>>,
        begin
            _pipe@7 = glint@flag:bool(),
            _pipe@8 = glint@flag:default(_pipe@7, false),
            glint@flag:description(_pipe@8, <<"Output as JSON"/utf8>>)
        end
    ).

-file("src/intent.gleam", 2285).
?DOC(" The `gaps` command - KIRK gap detection\n").
-spec kirk_gaps_command() -> glint:command(nil).
kirk_gaps_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Report = intent@kirk@gap_detector:detect_gaps(Spec),
                            case Is_json of
                                true ->
                                    Json_obj = gleam@json:object(
                                        [{<<"total_gaps"/utf8>>,
                                                gleam@json:int(
                                                    erlang:element(7, Report)
                                                )},
                                            {<<"severity_breakdown"/utf8>>,
                                                gleam@json:object(
                                                    [{<<"critical"/utf8>>,
                                                            gleam@json:int(
                                                                erlang:element(
                                                                    2,
                                                                    erlang:element(
                                                                        8,
                                                                        Report
                                                                    )
                                                                )
                                                            )},
                                                        {<<"high"/utf8>>,
                                                            gleam@json:int(
                                                                erlang:element(
                                                                    3,
                                                                    erlang:element(
                                                                        8,
                                                                        Report
                                                                    )
                                                                )
                                                            )},
                                                        {<<"medium"/utf8>>,
                                                            gleam@json:int(
                                                                erlang:element(
                                                                    4,
                                                                    erlang:element(
                                                                        8,
                                                                        Report
                                                                    )
                                                                )
                                                            )},
                                                        {<<"low"/utf8>>,
                                                            gleam@json:int(
                                                                erlang:element(
                                                                    5,
                                                                    erlang:element(
                                                                        8,
                                                                        Report
                                                                    )
                                                                )
                                                            )}]
                                                )},
                                            {<<"inversion_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(2, Report),
                                                    fun detected_gap_to_json/1
                                                )},
                                            {<<"second_order_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(3, Report),
                                                    fun detected_gap_to_json/1
                                                )},
                                            {<<"checklist_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(4, Report),
                                                    fun detected_gap_to_json/1
                                                )},
                                            {<<"coverage_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(5, Report),
                                                    fun detected_gap_to_json/1
                                                )},
                                            {<<"security_gaps"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(6, Report),
                                                    fun detected_gap_to_json/1
                                                )}]
                                    ),
                                    gleam@io:println(
                                        gleam@json:to_string(Json_obj)
                                    );

                                false ->
                                    gleam@io:println(
                                        intent@kirk@gap_detector:format_report(
                                            Report
                                        )
                                    )
                            end,
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent gaps <spec.cue> [--json]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(
        _pipe@1,
        <<"(Optional analysis) KIRK: Detect gaps using analysis tools"/utf8>>
    ),
    glint:flag(
        _pipe@2,
        <<"json"/utf8>>,
        begin
            _pipe@3 = glint@flag:bool(),
            _pipe@4 = glint@flag:default(_pipe@3, false),
            glint@flag:description(_pipe@4, <<"Output as JSON"/utf8>>)
        end
    ).

-file("src/intent.gleam", 2346).
?DOC(" The `compact` command - KIRK compact format (CIN)\n").
-spec kirk_compact_command() -> glint:command(nil).
kirk_compact_command() ->
    _pipe@1 = glint:command(
        fun(Input) ->
            Show_tokens = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"tokens"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Compact = intent@kirk@compact_format:spec_to_compact(
                                Spec
                            ),
                            Output = intent@kirk@compact_format:format_compact(
                                Compact
                            ),
                            gleam@io:println(Output),
                            case Show_tokens of
                                true ->
                                    {Full, Compact_tokens, Savings} = intent@kirk@compact_format:compare_token_usage(
                                        Spec
                                    ),
                                    gleam@io:println(<<""/utf8>>),
                                    gleam@io:println(
                                        <<"─────────────────────────────────────"/utf8>>
                                    ),
                                    gleam@io:println(<<"Token Analysis:"/utf8>>),
                                    gleam@io:println(
                                        <<<<"  Full JSON:    ~"/utf8,
                                                (gleam@string:inspect(Full))/binary>>/binary,
                                            " tokens"/utf8>>
                                    ),
                                    gleam@io:println(
                                        <<<<"  Compact CIN:  ~"/utf8,
                                                (gleam@string:inspect(
                                                    Compact_tokens
                                                ))/binary>>/binary,
                                            " tokens"/utf8>>
                                    ),
                                    gleam@io:println(
                                        <<<<"  Savings:      "/utf8,
                                                (gleam@string:inspect(
                                                    gleam@float:round(Savings)
                                                ))/binary>>/binary,
                                            "%"/utf8>>
                                    );

                                false ->
                                    nil
                            end,
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent compact <spec.cue> [--tokens]"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@2 = glint:description(
        _pipe@1,
        <<"(Optional analysis) KIRK: Convert to Compact Intent Notation (token-efficient)"/utf8>>
    ),
    glint:flag(
        _pipe@2,
        <<"tokens"/utf8>>,
        begin
            _pipe@3 = glint@flag:bool(),
            _pipe@4 = glint@flag:default(_pipe@3, false),
            glint@flag:description(_pipe@4, <<"Show token comparison"/utf8>>)
        end
    ).

-file("src/intent.gleam", 2393).
?DOC(" The `prototext` command - KIRK protobuf text format output\n").
-spec kirk_prototext_command() -> glint:command(nil).
kirk_prototext_command() ->
    _pipe = glint:command(fun(Input) -> case erlang:element(2, Input) of
                [Spec_path | _] ->
                    case intent@loader:load_spec(Spec_path) of
                        {ok, Spec} ->
                            Output = intent@kirk@compact_format:spec_to_prototext(
                                Spec
                            ),
                            gleam@io:println(Output),
                            intent_ffi:halt(0);

                        {error, E} ->
                            intent@cli_ui:print_error(
                                intent@loader:format_error(E)
                            ),
                            intent_ffi:halt(3)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"spec file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent prototext <spec.cue>"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end end),
    glint:description(
        _pipe,
        <<"(Optional analysis) KIRK: Export to Protobuf text format"/utf8>>
    ).

-file("src/intent.gleam", 2420).
?DOC(" The `ears` command - KIRK EARS requirements parser\n").
-spec kirk_ears_command() -> glint:command(nil).
kirk_ears_command() ->
    _pipe@2 = glint:command(
        fun(Input) ->
            Output_format = begin
                _pipe = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"output"/utf8>>
                ),
                gleam@result:unwrap(_pipe, <<"text"/utf8>>)
            end,
            Output_file = begin
                _pipe@1 = glint@flag:get_string(
                    erlang:element(3, Input),
                    <<"out"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, <<""/utf8>>)
            end,
            case erlang:element(2, Input) of
                [Requirements_path | _] ->
                    case simplifile:read(Requirements_path) of
                        {ok, Content} ->
                            Result = intent@kirk@ears_parser:parse(Content),
                            Output = case Output_format of
                                <<"cue"/utf8>> ->
                                    Spec_name = case glint@flag:get_string(
                                        erlang:element(3, Input),
                                        <<"name"/utf8>>
                                    ) of
                                        {ok, N} ->
                                            N;

                                        {error, _} ->
                                            <<"GeneratedSpec"/utf8>>
                                    end,
                                    intent@kirk@ears_parser:to_cue(
                                        Result,
                                        Spec_name
                                    );

                                <<"json"/utf8>> ->
                                    Behaviors = intent@kirk@ears_parser:to_behaviors(
                                        Result
                                    ),
                                    Json_obj = gleam@json:object(
                                        [{<<"requirements"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(2, Result),
                                                    fun(R) ->
                                                        gleam@json:object(
                                                            [{<<"id"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            2,
                                                                            R
                                                                        )
                                                                    )},
                                                                {<<"pattern"/utf8>>,
                                                                    gleam@json:string(
                                                                        intent@kirk@ears_parser:pattern_to_string(
                                                                            erlang:element(
                                                                                3,
                                                                                R
                                                                            )
                                                                        )
                                                                    )},
                                                                {<<"system_shall"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            7,
                                                                            R
                                                                        )
                                                                    )},
                                                                {<<"raw_text"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            9,
                                                                            R
                                                                        )
                                                                    )}]
                                                        )
                                                    end
                                                )},
                                            {<<"behaviors"/utf8>>,
                                                gleam@json:array(
                                                    Behaviors,
                                                    fun(B) ->
                                                        gleam@json:object(
                                                            [{<<"name"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            2,
                                                                            B
                                                                        )
                                                                    )},
                                                                {<<"intent"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            3,
                                                                            B
                                                                        )
                                                                    )},
                                                                {<<"method"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            4,
                                                                            B
                                                                        )
                                                                    )},
                                                                {<<"path"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            5,
                                                                            B
                                                                        )
                                                                    )},
                                                                {<<"status"/utf8>>,
                                                                    gleam@json:int(
                                                                        erlang:element(
                                                                            6,
                                                                            B
                                                                        )
                                                                    )}]
                                                        )
                                                    end
                                                )},
                                            {<<"errors"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(3, Result),
                                                    fun(E) ->
                                                        gleam@json:object(
                                                            [{<<"line"/utf8>>,
                                                                    gleam@json:int(
                                                                        erlang:element(
                                                                            2,
                                                                            E
                                                                        )
                                                                    )},
                                                                {<<"message"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            3,
                                                                            E
                                                                        )
                                                                    )},
                                                                {<<"suggestion"/utf8>>,
                                                                    gleam@json:string(
                                                                        erlang:element(
                                                                            4,
                                                                            E
                                                                        )
                                                                    )}]
                                                        )
                                                    end
                                                )},
                                            {<<"warnings"/utf8>>,
                                                gleam@json:array(
                                                    erlang:element(4, Result),
                                                    fun gleam@json:string/1
                                                )}]
                                    ),
                                    gleam@json:to_string(Json_obj);

                                _ ->
                                    intent@kirk@ears_parser:format_result(
                                        Result
                                    )
                            end,
                            case Output_file of
                                <<""/utf8>> ->
                                    gleam@io:println(Output);

                                Path ->
                                    case simplifile:write(Path, Output) of
                                        {ok, _} ->
                                            gleam@io:println(
                                                <<"Written to: "/utf8,
                                                    Path/binary>>
                                            );

                                        {error, _} ->
                                            intent@cli_ui:print_error(
                                                <<"Failed to write to: "/utf8,
                                                    Path/binary>>
                                            )
                                    end
                            end,
                            intent_ffi:halt(0);

                        {error, _} ->
                            intent@cli_ui:print_error(
                                <<"Failed to read: "/utf8,
                                    Requirements_path/binary>>
                            ),
                            intent_ffi:halt(4)
                    end;

                [] ->
                    intent@cli_ui:print_error(
                        <<"requirements file path required"/utf8>>
                    ),
                    gleam@io:println(
                        <<"Usage: intent ears <requirements.md> [--output text|cue|json] [--out <file>]"/utf8>>
                    ),
                    gleam@io:println(<<""/utf8>>),
                    gleam@io:println(<<"EARS Patterns:"/utf8>>),
                    gleam@io:println(
                        <<"  THE SYSTEM SHALL [behavior]                    - Ubiquitous"/utf8>>
                    ),
                    gleam@io:println(
                        <<"  WHEN [trigger] THE SYSTEM SHALL [behavior]     - Event-Driven"/utf8>>
                    ),
                    gleam@io:println(
                        <<"  WHILE [state] THE SYSTEM SHALL [behavior]      - State-Driven"/utf8>>
                    ),
                    gleam@io:println(
                        <<"  WHERE [condition] THE SYSTEM SHALL [behavior]  - Optional"/utf8>>
                    ),
                    gleam@io:println(
                        <<"  IF [condition] THEN THE SYSTEM SHALL NOT       - Unwanted"/utf8>>
                    ),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@3 = glint:description(
        _pipe@2,
        <<"(Optional analysis) KIRK: Parse EARS requirements to Intent behaviors"/utf8>>
    ),
    _pipe@6 = glint:flag(
        _pipe@3,
        <<"output"/utf8>>,
        begin
            _pipe@4 = glint@flag:string(),
            _pipe@5 = glint@flag:default(_pipe@4, <<"text"/utf8>>),
            glint@flag:description(
                _pipe@5,
                <<"Output format: text, cue, json"/utf8>>
            )
        end
    ),
    _pipe@9 = glint:flag(
        _pipe@6,
        <<"out"/utf8>>,
        begin
            _pipe@7 = glint@flag:string(),
            _pipe@8 = glint@flag:default(_pipe@7, <<""/utf8>>),
            glint@flag:description(_pipe@8, <<"Output file path"/utf8>>)
        end
    ),
    glint:flag(
        _pipe@9,
        <<"name"/utf8>>,
        begin
            _pipe@10 = glint@flag:string(),
            _pipe@11 = glint@flag:default(_pipe@10, <<"GeneratedSpec"/utf8>>),
            glint@flag:description(
                _pipe@11,
                <<"Spec name for CUE output"/utf8>>
            )
        end
    ).

-file("src/intent.gleam", 2524).
?DOC(
    " The `context-scan` command - detect language and framework\n"
    " Bead: [CTX-8] Add context-scan command to CLI\n"
).
-spec context_scan_command() -> glint:command(nil).
context_scan_command() ->
    _pipe@2 = glint:command(
        fun(Input) ->
            Is_json = begin
                _pipe = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"json"/utf8>>
                ),
                gleam@result:unwrap(_pipe, false)
            end,
            Is_cue = begin
                _pipe@1 = glint@flag:get_bool(
                    erlang:element(3, Input),
                    <<"cue"/utf8>>
                ),
                gleam@result:unwrap(_pipe@1, false)
            end,
            Scan_path = case erlang:element(2, Input) of
                [Path | _] ->
                    Path;

                [] ->
                    <<"."/utf8>>
            end,
            case intent@context_scanner:detect_from_directory(Scan_path) of
                {ok, Context} ->
                    case Is_json of
                        true ->
                            gleam@io:println(
                                intent@context_scanner:context_to_json(Context)
                            );

                        false ->
                            case Is_cue of
                                true ->
                                    gleam@io:println(
                                        intent@context_scanner:context_to_cue(
                                            Context
                                        )
                                    );

                                false ->
                                    case intent@context_scanner:format_context(
                                        Context
                                    ) of
                                        {ok, Text} ->
                                            gleam@io:println(Text);

                                        {error, Err} ->
                                            gleam@io:println_error(
                                                <<"Error: "/utf8, Err/binary>>
                                            )
                                    end
                            end
                    end,
                    intent_ffi:halt(0);

                {error, Err@1} ->
                    intent@cli_ui:print_error(Err@1),
                    intent_ffi:halt(4)
            end
        end
    ),
    _pipe@3 = glint:description(
        _pipe@2,
        <<"Scan codebase to detect language and framework"/utf8>>
    ),
    _pipe@6 = glint:flag(
        _pipe@3,
        <<"json"/utf8>>,
        begin
            _pipe@4 = glint@flag:bool(),
            _pipe@5 = glint@flag:default(_pipe@4, false),
            glint@flag:description(_pipe@5, <<"Output as JSON"/utf8>>)
        end
    ),
    glint:flag(
        _pipe@6,
        <<"cue"/utf8>>,
        begin
            _pipe@7 = glint@flag:bool(),
            _pipe@8 = glint@flag:default(_pipe@7, false),
            glint@flag:description(
                _pipe@8,
                <<"Output as CUE (for schema compatibility)"/utf8>>
            )
        end
    ).

-file("src/intent.gleam", 53).
-spec main() -> nil.
main() ->
    _pipe = glint:new(),
    _pipe@1 = glint:with_name(_pipe, <<"intent"/utf8>>),
    _pipe@2 = glint:with_pretty_help(_pipe@1, glint:default_pretty_help()),
    _pipe@3 = glint:add(_pipe@2, [<<"check"/utf8>>], check_command()),
    _pipe@4 = glint:add(_pipe@3, [<<"validate"/utf8>>], validate_command()),
    _pipe@5 = glint:add(_pipe@4, [<<"show"/utf8>>], show_command()),
    _pipe@6 = glint:add(_pipe@5, [<<"export"/utf8>>], export_command()),
    _pipe@7 = glint:add(_pipe@6, [<<"lint"/utf8>>], lint_command()),
    _pipe@8 = glint:add(_pipe@7, [<<"analyze"/utf8>>], analyze_command()),
    _pipe@9 = glint:add(_pipe@8, [<<"improve"/utf8>>], improve_command()),
    _pipe@10 = glint:add(_pipe@9, [<<"interview"/utf8>>], interview_command()),
    _pipe@11 = glint:add(_pipe@10, [<<"beads"/utf8>>], beads_command()),
    _pipe@12 = glint:add(
        _pipe@11,
        [<<"bead-status"/utf8>>],
        bead_status_command()
    ),
    _pipe@13 = glint:add(_pipe@12, [<<"history"/utf8>>], history_command()),
    _pipe@14 = glint:add(_pipe@13, [<<"diff"/utf8>>], diff_command()),
    _pipe@15 = glint:add(_pipe@14, [<<"sessions"/utf8>>], sessions_command()),
    _pipe@16 = glint:add(_pipe@15, [<<"quality"/utf8>>], kirk_quality_command()),
    _pipe@17 = glint:add(_pipe@16, [<<"invert"/utf8>>], kirk_invert_command()),
    _pipe@18 = glint:add(
        _pipe@17,
        [<<"coverage"/utf8>>],
        kirk_coverage_command()
    ),
    _pipe@19 = glint:add(_pipe@18, [<<"gaps"/utf8>>], kirk_gaps_command()),
    _pipe@20 = glint:add(_pipe@19, [<<"compact"/utf8>>], kirk_compact_command()),
    _pipe@21 = glint:add(
        _pipe@20,
        [<<"prototext"/utf8>>],
        kirk_prototext_command()
    ),
    _pipe@22 = glint:add(_pipe@21, [<<"ears"/utf8>>], kirk_ears_command()),
    _pipe@23 = glint:add(_pipe@22, [<<"plan"/utf8>>], plan_command()),
    _pipe@24 = glint:add(
        _pipe@23,
        [<<"plan-approve"/utf8>>],
        plan_approve_command()
    ),
    _pipe@25 = glint:add(
        _pipe@24,
        [<<"beads-regenerate"/utf8>>],
        beads_regenerate_command()
    ),
    _pipe@26 = glint:add(
        _pipe@25,
        [<<"context-scan"/utf8>>],
        context_scan_command()
    ),
    glint:run(_pipe@26, erlang:element(4, argv:load())).
