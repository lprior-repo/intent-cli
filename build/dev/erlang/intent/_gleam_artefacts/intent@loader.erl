-module(intent@loader).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/intent/loader.gleam").
-export([validate_cue/1, load_spec_quiet/1, load_spec/1, export_spec_json/1, format_error/1]).
-export_type([load_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type load_error() :: {file_not_found, binary()} |
    {cue_validation_error, binary()} |
    {cue_export_error, binary()} |
    {json_parse_error, binary()} |
    {spec_parse_error, binary()} |
    {light_spec_parse_error, binary()}.

-file("src/intent/loader.gleam", 81).
?DOC(" Validate a CUE file without exporting\n").
-spec validate_cue(binary()) -> {ok, nil} | {error, load_error()}.
validate_cue(Path) ->
    case shellout:command(
        <<"cue"/utf8>>,
        [<<"vet"/utf8>>, Path],
        <<"."/utf8>>,
        []
    ) of
        {ok, _} ->
            {ok, nil};

        {error, {_, Stderr}} ->
            {error, {cue_validation_error, Stderr}}
    end.

-file("src/intent/loader.gleam", 135).
?DOC(" Convert a LightSpec to a full Spec for unified handling\n").
-spec light_spec_to_spec(intent@types:light_spec()) -> intent@types:spec().
light_spec_to_spec(Light) ->
    Behaviors = gleam@list:map(
        erlang:element(4, Light),
        fun(Lb) ->
            {behavior,
                erlang:element(2, Lb),
                erlang:element(3, Lb),
                <<""/utf8>>,
                [],
                [],
                {request,
                    erlang:element(2, erlang:element(4, Lb)),
                    erlang:element(3, erlang:element(4, Lb)),
                    gleam@dict:new(),
                    gleam@dict:new(),
                    erlang:element(4, erlang:element(4, Lb))},
                {response,
                    erlang:element(2, erlang:element(5, Lb)),
                    gleam@json:null(),
                    erlang:element(3, erlang:element(5, Lb)),
                    gleam@dict:new()},
                gleam@dict:new()}
        end
    ),
    Feature = {feature,
        <<"default"/utf8>>,
        <<"Behaviors from light spec"/utf8>>,
        Behaviors},
    Default_ai_hints = {a_i_hints,
        {implementation_hints, []},
        gleam@dict:new(),
        {security_hints, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>, <<""/utf8>>},
        [],
        none},
    {spec,
        erlang:element(2, Light),
        erlang:element(3, Light),
        <<""/utf8>>,
        <<"1.0.0"/utf8>>,
        [],
        {config, <<""/utf8>>, 5000, gleam@dict:new()},
        [Feature],
        [],
        erlang:element(5, Light),
        case erlang:element(6, Light) of
            none ->
                Default_ai_hints;

            {some, Hints} ->
                Hints
        end}.

-file("src/intent/loader.gleam", 216).
-spec format_single_decode_error(gleam@dynamic:decode_error()) -> binary().
format_single_decode_error(Error) ->
    Path_str = case erlang:element(4, Error) of
        [] ->
            <<"at root"/utf8>>;

        Path_parts ->
            <<<<<<<<"at "/utf8,
                            (gleam@string:join(Path_parts, <<"."/utf8>>))/binary>>/binary,
                        " (path: ."/utf8>>/binary,
                    (gleam@string:join(Path_parts, <<"."/utf8>>))/binary>>/binary,
                ")"/utf8>>
    end,
    <<<<<<<<<<"Expected "/utf8, (erlang:element(2, Error))/binary>>/binary,
                    " but found "/utf8>>/binary,
                (erlang:element(3, Error))/binary>>/binary,
            " "/utf8>>/binary,
        Path_str/binary>>.

-file("src/intent/loader.gleam", 202).
-spec format_decode_errors(list(gleam@dynamic:decode_error())) -> binary().
format_decode_errors(Errors) ->
    case Errors of
        [] ->
            <<"Unknown decode error"/utf8>>;

        [Error] ->
            format_single_decode_error(Error);

        Multiple ->
            <<"Multiple decode errors:\n"/utf8,
                (gleam@string:join(
                    gleam@list:map(
                        Multiple,
                        fun(E) ->
                            <<"  • "/utf8,
                                (format_single_decode_error(E))/binary>>
                        end
                    ),
                    <<"\n"/utf8>>
                ))/binary>>
    end.

-file("src/intent/loader.gleam", 230).
-spec format_json_error(gleam@json:decode_error()) -> binary().
format_json_error(Error) ->
    case Error of
        unexpected_end_of_input ->
            <<"Unexpected end of input - JSON is incomplete or truncated.\n"/utf8,
                "  • Check that your JSON is properly closed with matching braces/brackets"/utf8>>;

        {unexpected_byte, B} ->
            <<<<<<<<"Unexpected byte: '"/utf8, B/binary>>/binary,
                        "' in JSON at this position.\n"/utf8>>/binary,
                    "  • Check for syntax errors like missing commas, quotes, or brackets\n"/utf8>>/binary,
                "  • Ensure strings are properly quoted"/utf8>>;

        {unexpected_sequence, S} ->
            <<<<<<<<"Unexpected sequence: '"/utf8, S/binary>>/binary,
                        "' in JSON.\n"/utf8>>/binary,
                    "  • This sequence is not valid JSON syntax\n"/utf8>>/binary,
                "  • Check for typos or invalid characters"/utf8>>;

        {unexpected_format, Errs} ->
            <<"JSON format error:\n"/utf8, (format_decode_errors(Errs))/binary>>
    end.

-file("src/intent/loader.gleam", 102).
-spec parse_json_spec(binary()) -> {ok, intent@types:spec()} |
    {error, load_error()}.
parse_json_spec(Json_str) ->
    case gleam@json:decode(Json_str, fun gleam@dynamic:dynamic/1) of
        {ok, Data} ->
            case intent@parser:parse_spec(Data) of
                {ok, Spec} ->
                    {ok, Spec};

                {error, Errors} ->
                    Msg = begin
                        _pipe = Errors,
                        format_decode_errors(_pipe)
                    end,
                    {error, {spec_parse_error, Msg}}
            end;

        {error, E} ->
            {error, {json_parse_error, format_json_error(E)}}
    end.

-file("src/intent/loader.gleam", 118).
-spec parse_json_light_spec(binary()) -> {ok, intent@types:spec()} |
    {error, load_error()}.
parse_json_light_spec(Json_str) ->
    case gleam@json:decode(Json_str, fun gleam@dynamic:dynamic/1) of
        {ok, Data} ->
            case intent@parser:parse_light_spec(Data) of
                {ok, Light_spec} ->
                    {ok, light_spec_to_spec(Light_spec)};

                {error, Errors} ->
                    Msg = begin
                        _pipe = Errors,
                        format_decode_errors(_pipe)
                    end,
                    {error, {light_spec_parse_error, Msg}}
            end;

        {error, E} ->
            {error, {json_parse_error, format_json_error(E)}}
    end.

-file("src/intent/loader.gleam", 88).
-spec export_and_parse(binary()) -> {ok, intent@types:spec()} |
    {error, load_error()}.
export_and_parse(Path) ->
    case shellout:command(
        <<"cue"/utf8>>,
        [<<"export"/utf8>>, Path, <<"-e"/utf8>>, <<"spec"/utf8>>],
        <<"."/utf8>>,
        []
    ) of
        {ok, Json_str} ->
            parse_json_spec(Json_str);

        {error, _} ->
            case shellout:command(
                <<"cue"/utf8>>,
                [<<"export"/utf8>>, Path, <<"-e"/utf8>>, <<"light_spec"/utf8>>],
                <<"."/utf8>>,
                []
            ) of
                {ok, Json_str@1} ->
                    parse_json_light_spec(Json_str@1);

                {error, {_, Stderr}} ->
                    {error, {cue_export_error, Stderr}}
            end
    end.

-file("src/intent/loader.gleam", 50).
?DOC(
    " Pure business logic - validates and parses without UI\n"
    " This is the testable core implementation\n"
).
-spec load_and_parse_impl(binary()) -> {ok, intent@types:spec()} |
    {error, load_error()}.
load_and_parse_impl(Path) ->
    case validate_cue(Path) of
        {ok, _} ->
            export_and_parse(Path);

        {error, E} ->
            {error, E}
    end.

-file("src/intent/loader.gleam", 40).
?DOC(
    " Load a spec from a CUE file without spinner UI\n"
    " Use this for testing and automation where no UI output is desired\n"
).
-spec load_spec_quiet(binary()) -> {ok, intent@types:spec()} |
    {error, load_error()}.
load_spec_quiet(Path) ->
    case simplifile:verify_is_file(Path) of
        {ok, true} ->
            load_and_parse_impl(Path);

        _ ->
            {error, {file_not_found, Path}}
    end.

-file("src/intent/loader.gleam", 58).
?DOC(" Load and parse with spinner UI for interactive use\n").
-spec load_and_parse_with_spinner(binary()) -> {ok, intent@types:spec()} |
    {error, load_error()}.
load_and_parse_with_spinner(Path) ->
    Sp = begin
        _pipe = spinner:new(<<"Validating CUE spec..."/utf8>>),
        _pipe@1 = spinner:with_colour(_pipe, fun gleam_community@ansi:yellow/1),
        spinner:start(_pipe@1)
    end,
    case validate_cue(Path) of
        {ok, _} ->
            spinner:set_text(Sp, <<"Exporting CUE to JSON..."/utf8>>),
            Result = export_and_parse(Path),
            spinner:stop(Sp),
            Result;

        {error, E} ->
            spinner:stop(Sp),
            {error, E}
    end.

-file("src/intent/loader.gleam", 30).
?DOC(" Load a spec from a CUE file (with spinner UI)\n").
-spec load_spec(binary()) -> {ok, intent@types:spec()} | {error, load_error()}.
load_spec(Path) ->
    case simplifile:verify_is_file(Path) of
        {ok, true} ->
            load_and_parse_with_spinner(Path);

        _ ->
            {error, {file_not_found, Path}}
    end.

-file("src/intent/loader.gleam", 249).
?DOC(" Export a spec to JSON format (for AI consumption)\n").
-spec export_spec_json(binary()) -> {ok, binary()} | {error, load_error()}.
export_spec_json(Path) ->
    case simplifile:verify_is_file(Path) of
        {ok, true} ->
            case shellout:command(
                <<"cue"/utf8>>,
                [<<"export"/utf8>>, Path, <<"-e"/utf8>>, <<"spec"/utf8>>],
                <<"."/utf8>>,
                []
            ) of
                {ok, Json_str} ->
                    {ok, Json_str};

                {error, {_, Stderr}} ->
                    {error, {cue_export_error, Stderr}}
            end;

        _ ->
            {error, {file_not_found, Path}}
    end.

-file("src/intent/loader.gleam", 261).
?DOC(" Format a LoadError as a human-readable string\n").
-spec format_error(load_error()) -> binary().
format_error(Error) ->
    case Error of
        {file_not_found, Path} ->
            <<"File not found: "/utf8, Path/binary>>;

        {cue_validation_error, Msg} ->
            <<"CUE validation failed:\n"/utf8, Msg/binary>>;

        {cue_export_error, Msg@1} ->
            <<"CUE export failed:\n"/utf8, Msg@1/binary>>;

        {json_parse_error, Msg@2} ->
            <<"JSON parse error: "/utf8, Msg@2/binary>>;

        {spec_parse_error, Msg@3} ->
            <<"Spec parse error: "/utf8, Msg@3/binary>>;

        {light_spec_parse_error, Msg@4} ->
            <<"Light spec parse error: "/utf8, Msg@4/binary>>
    end.
