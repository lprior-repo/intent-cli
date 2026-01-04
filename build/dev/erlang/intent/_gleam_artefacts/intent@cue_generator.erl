-module(intent@cue_generator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/cue_generator.gleam").
-export([format_cue/1, spec_to_cue/1]).
-export_type([generated_c_u_e/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type generated_c_u_e() :: {generated_c_u_e, binary(), list(binary()), binary()}.

-file("src/intent/cue_generator.gleam", 87).
?DOC(" Check if spec has HTTP behaviors\n").
-spec has_http_behaviors(intent@types:spec()) -> boolean().
has_http_behaviors(Spec) ->
    _pipe = erlang:element(8, Spec),
    gleam@list:any(_pipe, fun(F) -> _pipe@1 = erlang:element(4, F),
            gleam@list:any(_pipe@1, fun(_) -> true end) end).

-file("src/intent/cue_generator.gleam", 96).
?DOC(" Check if spec has regex patterns in rules\n").
-spec has_regex_in_rules(intent@types:spec()) -> boolean().
has_regex_in_rules(Spec) ->
    _pipe = erlang:element(9, Spec),
    gleam@list:any(_pipe, fun(_) -> false end).

-file("src/intent/cue_generator.gleam", 104).
?DOC(" Check if spec has time-based rules\n").
-spec has_time_rules(intent@types:spec()) -> boolean().
has_time_rules(Spec) ->
    _pipe = erlang:element(9, Spec),
    gleam@list:any(_pipe, fun(_) -> false end).

-file("src/intent/cue_generator.gleam", 217).
?DOC(" Convert HTTP Method to string\n").
-spec method_to_string(intent@types:method()) -> binary().
method_to_string(Method) ->
    case Method of
        get ->
            <<"GET"/utf8>>;

        post ->
            <<"POST"/utf8>>;

        put ->
            <<"PUT"/utf8>>;

        patch ->
            <<"PATCH"/utf8>>;

        delete ->
            <<"DELETE"/utf8>>;

        head ->
            <<"HEAD"/utf8>>;

        options ->
            <<"OPTIONS"/utf8>>
    end.

-file("src/intent/cue_generator.gleam", 230).
?DOC(" Convert a Rule to CUE code\n").
-spec rule_to_cue(intent@types:rule()) -> binary().
rule_to_cue(Rule) ->
    <<<<<<<<"rule \\\""/utf8, (erlang:element(2, Rule))/binary>>/binary,
                "\\\": {
  description: \\\""/utf8>>/binary,
            (erlang:element(3, Rule))/binary>>/binary,
        "\\\"
}"/utf8>>.

-file("src/intent/cue_generator.gleam", 237).
?DOC(" Convert JSON to CUE representation\n").
-spec json_to_cue(gleam@json:json()) -> binary().
json_to_cue(Json) ->
    gleam@json:to_string(Json).

-file("src/intent/cue_generator.gleam", 125).
?DOC(" Convert a Behavior to CUE code\n").
-spec behavior_to_cue(intent@types:behavior()) -> binary().
behavior_to_cue(Behavior) ->
    Method = method_to_string(erlang:element(2, erlang:element(7, Behavior))),
    Path = erlang:element(3, erlang:element(7, Behavior)),
    Intent_line = case gleam@string:is_empty(erlang:element(3, Behavior)) of
        true ->
            <<""/utf8>>;

        false ->
            <<<<"  intent: \\\""/utf8, (erlang:element(3, Behavior))/binary>>/binary,
                "\\\"\\n"/utf8>>
    end,
    Notes_line = case gleam@string:is_empty(erlang:element(4, Behavior)) of
        true ->
            <<""/utf8>>;

        false ->
            <<<<"  notes: \\\""/utf8, (erlang:element(4, Behavior))/binary>>/binary,
                "\\\"\\n"/utf8>>
    end,
    Request_line = <<<<<<<<"  request: {\\n    method: \\\""/utf8,
                    Method/binary>>/binary,
                "\\\"\\n    path: \\\""/utf8>>/binary,
            Path/binary>>/binary,
        "\\\"\\n"/utf8>>,
    Headers_line = case gleam@dict:is_empty(
        erlang:element(4, erlang:element(7, Behavior))
    ) of
        true ->
            <<""/utf8>>;

        false ->
            <<<<"    headers: {\\n"/utf8,
                    (begin
                        _pipe = erlang:element(4, erlang:element(7, Behavior)),
                        _pipe@1 = maps:to_list(_pipe),
                        _pipe@2 = gleam@list:map(
                            _pipe@1,
                            fun(Pair) ->
                                {K, V} = Pair,
                                <<<<<<<<"      \\\""/utf8, K/binary>>/binary,
                                            "\\\": \\\""/utf8>>/binary,
                                        V/binary>>/binary,
                                    "\\\""/utf8>>
                            end
                        ),
                        gleam@string:join(_pipe@2, <<"\\n"/utf8>>)
                    end)/binary>>/binary,
                "\\n    }\\n"/utf8>>
    end,
    Request_close = <<"  }\\n"/utf8>>,
    Response_line = <<<<"  response: {\\n    status: "/utf8,
            (gleam@int:to_string(erlang:element(2, erlang:element(8, Behavior))))/binary>>/binary,
        "\\n"/utf8>>,
    Example_line = case erlang:element(3, erlang:element(8, Behavior)) of
        none ->
            <<""/utf8>>;

        {some, Ex} ->
            <<<<"    example: "/utf8, (json_to_cue(Ex))/binary>>/binary,
                "\\n"/utf8>>
    end,
    Checks_lines = case gleam@dict:is_empty(
        erlang:element(4, erlang:element(8, Behavior))
    ) of
        true ->
            <<""/utf8>>;

        false ->
            <<<<"    checks: {\\n"/utf8,
                    (begin
                        _pipe@3 = erlang:element(4, erlang:element(8, Behavior)),
                        _pipe@4 = maps:to_list(_pipe@3),
                        _pipe@5 = gleam@list:map(
                            _pipe@4,
                            fun(Pair@1) ->
                                {Field, Check} = Pair@1,
                                <<<<<<<<<<<<"      \\\""/utf8, Field/binary>>/binary,
                                                    "\\\": {\\n        rule: \\\""/utf8>>/binary,
                                                (erlang:element(2, Check))/binary>>/binary,
                                            "\\\""/utf8>>/binary,
                                        ((case gleam@string:is_empty(
                                            erlang:element(3, Check)
                                        ) of
                                            true ->
                                                <<""/utf8>>;

                                            false ->
                                                <<<<"\\n        why: \\\""/utf8,
                                                        (erlang:element(
                                                            3,
                                                            Check
                                                        ))/binary>>/binary,
                                                    "\\\""/utf8>>
                                        end))/binary>>/binary,
                                    "\\n      }"/utf8>>
                            end
                        ),
                        gleam@string:join(_pipe@5, <<"\\n"/utf8>>)
                    end)/binary>>/binary,
                "\\n    }\\n"/utf8>>
    end,
    Response_close = <<"  }\\n"/utf8>>,
    Requires_line = case gleam@list:is_empty(erlang:element(5, Behavior)) of
        true ->
            <<""/utf8>>;

        false ->
            <<<<"  requires: ["/utf8,
                    (begin
                        _pipe@6 = erlang:element(5, Behavior),
                        _pipe@7 = gleam@list:map(
                            _pipe@6,
                            fun(B) ->
                                <<<<"\\\""/utf8, B/binary>>/binary,
                                    "\\\""/utf8>>
                            end
                        ),
                        gleam@string:join(_pipe@7, <<", "/utf8>>)
                    end)/binary>>/binary,
                "]\\n"/utf8>>
    end,
    Captures_line = case gleam@dict:is_empty(erlang:element(9, Behavior)) of
        true ->
            <<""/utf8>>;

        false ->
            <<<<"  captures: {\\n"/utf8,
                    (begin
                        _pipe@8 = erlang:element(9, Behavior),
                        _pipe@9 = maps:to_list(_pipe@8),
                        _pipe@10 = gleam@list:map(
                            _pipe@9,
                            fun(Pair@2) ->
                                {Name, Path@1} = Pair@2,
                                <<<<<<<<"    \\\""/utf8, Name/binary>>/binary,
                                            "\\\": \\\""/utf8>>/binary,
                                        Path@1/binary>>/binary,
                                    "\\\""/utf8>>
                            end
                        ),
                        gleam@string:join(_pipe@10, <<"\\n"/utf8>>)
                    end)/binary>>/binary,
                "\\n  }\\n"/utf8>>
    end,
    Behaviors_name = gleam@string:replace(
        erlang:element(2, Behavior),
        <<"-"/utf8>>,
        <<"_"/utf8>>
    ),
    <<<<<<<<<<<<<<<<<<<<<<<<<<Behaviors_name/binary, ": {\\n"/utf8>>/binary,
                                                    Intent_line/binary>>/binary,
                                                Notes_line/binary>>/binary,
                                            Request_line/binary>>/binary,
                                        Headers_line/binary>>/binary,
                                    Request_close/binary>>/binary,
                                Response_line/binary>>/binary,
                            Example_line/binary>>/binary,
                        Checks_lines/binary>>/binary,
                    Response_close/binary>>/binary,
                Requires_line/binary>>/binary,
            Captures_line/binary>>/binary,
        "}"/utf8>>.

-file("src/intent/cue_generator.gleam", 112).
?DOC(" Convert a Feature to CUE code\n").
-spec feature_to_cue(intent@types:feature()) -> binary().
feature_to_cue(Feature) ->
    <<<<<<<<<<<<"feature \\\""/utf8, (erlang:element(2, Feature))/binary>>/binary,
                        "\\\": {
  description: \\\""/utf8>>/binary,
                    (erlang:element(3, Feature))/binary>>/binary,
                "\\\"

"/utf8>>/binary,
            (begin
                _pipe = erlang:element(4, Feature),
                _pipe@1 = gleam@list:map(_pipe, fun behavior_to_cue/1),
                gleam@string:join(_pipe@1, <<"\\n\\n"/utf8>>)
            end)/binary>>/binary,
        "
}"/utf8>>.

-file("src/intent/cue_generator.gleam", 242).
?DOC(" Format generated CUE for output\n").
-spec format_cue(generated_c_u_e()) -> binary().
format_cue(Generated) ->
    Import_section = case gleam@list:is_empty(erlang:element(3, Generated)) of
        true ->
            <<""/utf8>>;

        false ->
            <<(gleam@string:join(erlang:element(3, Generated), <<"\\n"/utf8>>))/binary,
                "\\n\\n"/utf8>>
    end,
    <<<<<<(erlang:element(2, Generated))/binary, "\\n\\n"/utf8>>/binary,
            Import_section/binary>>/binary,
        (erlang:element(4, Generated))/binary>>.

-file("src/intent/cue_generator.gleam", 252).
?DOC(" Helper to conditionally append items\n").
-spec append_if(list(binary()), boolean(), list(binary())) -> list(binary()).
append_if(Items, Condition, To_append) ->
    case Condition of
        true ->
            lists:append(Items, To_append);

        false ->
            Items
    end.

-file("src/intent/cue_generator.gleam", 70).
?DOC(" Generate imports needed for the spec\n").
-spec generate_imports(intent@types:spec()) -> list(binary()).
generate_imports(Spec) ->
    _pipe = [],
    _pipe@1 = append_if(
        _pipe,
        has_http_behaviors(Spec),
        [<<"import ("/utf8>>, <<"\\t\\\"net/http\\\""/utf8>>, <<")"/utf8>>]
    ),
    _pipe@2 = append_if(
        _pipe@1,
        has_regex_in_rules(Spec),
        [<<"import \\\"regexp\\\""/utf8>>]
    ),
    append_if(_pipe@2, has_time_rules(Spec), [<<"import \\\"time\\\""/utf8>>]).

-file("src/intent/cue_generator.gleam", 22).
?DOC(" Generate CUE code from a Spec\n").
-spec spec_to_cue(intent@types:spec()) -> generated_c_u_e().
spec_to_cue(Spec) ->
    Package_line = <<"package api"/utf8>>,
    Imports = generate_imports(Spec),
    Features_lines = begin
        _pipe = erlang:element(8, Spec),
        _pipe@1 = gleam@list:map(_pipe, fun feature_to_cue/1),
        gleam@string:join(_pipe@1, <<"\\n\\n"/utf8>>)
    end,
    Rules_lines = case gleam@list:is_empty(erlang:element(9, Spec)) of
        true ->
            <<""/utf8>>;

        false ->
            <<"\\n\\n// Global validation rules\\n"/utf8,
                (begin
                    _pipe@2 = erlang:element(9, Spec),
                    _pipe@3 = gleam@list:map(_pipe@2, fun rule_to_cue/1),
                    gleam@string:join(_pipe@3, <<"\\n\\n"/utf8>>)
                end)/binary>>
    end,
    Anti_patterns_lines = case gleam@list:is_empty(erlang:element(10, Spec)) of
        true ->
            <<""/utf8>>;

        false ->
            <<"\\n\\n// Anti-patterns to avoid\\n"/utf8,
                (begin
                    _pipe@4 = erlang:element(10, Spec),
                    _pipe@5 = gleam@list:map(
                        _pipe@4,
                        fun(Ap) ->
                            <<<<<<<<<<<<"anti_pattern \\\""/utf8,
                                                    (erlang:element(2, Ap))/binary>>/binary,
                                                "\\\": {
  description: \\\""/utf8>>/binary,
                                            (erlang:element(3, Ap))/binary>>/binary,
                                        "\\\"
  bad_example: "/utf8>>/binary,
                                    (json_to_cue(erlang:element(4, Ap)))/binary>>/binary,
                                "
}"/utf8>>
                        end
                    ),
                    gleam@string:join(_pipe@5, <<"\\n\\n"/utf8>>)
                end)/binary>>
    end,
    Body = <<<<Features_lines/binary, Rules_lines/binary>>/binary,
        Anti_patterns_lines/binary>>,
    {generated_c_u_e, Package_line, Imports, Body}.
