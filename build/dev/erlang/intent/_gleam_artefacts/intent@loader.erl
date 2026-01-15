-module(intent@loader).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([validate_cue/1, load_spec_quiet/1, load_spec/1, export_spec_json/1, format_error/1]).
-export_type([load_error/0]).

-type load_error() :: {file_not_found, binary()} |
    {cue_validation_error, binary()} |
    {cue_export_error, binary()} |
    {json_parse_error, binary()} |
    {spec_parse_error, binary()} |
    {security_error, binary()}.

-spec validate_cue(binary()) -> {ok, nil} | {error, load_error()}.
validate_cue(Path) ->
    case intent@security:validate_file_path(Path) of
        {ok, Validated_path} ->
            case shellout:command(
                <<"cue"/utf8>>,
                [<<"vet"/utf8>>, Validated_path],
                <<"."/utf8>>,
                []
            ) of
                {ok, _} ->
                    {ok, nil};

                {error, {_, Stderr}} ->
                    {error, {cue_validation_error, Stderr}}
            end;

        {error, Security_error} ->
            {error,
                {security_error,
                    intent@security:format_security_error(Security_error)}}
    end.

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

        {error, {_, Stderr}} ->
            {error, {cue_export_error, Stderr}}
    end.

-spec load_and_parse_impl(binary()) -> {ok, intent@types:spec()} |
    {error, load_error()}.
load_and_parse_impl(Path) ->
    case validate_cue(Path) of
        {ok, _} ->
            export_and_parse(Path);

        {error, E} ->
            {error, E}
    end.

-spec load_spec_quiet(binary()) -> {ok, intent@types:spec()} |
    {error, load_error()}.
load_spec_quiet(Path) ->
    case intent@security:validate_file_path(Path) of
        {ok, Validated_path} ->
            load_and_parse_impl(Validated_path);

        {error, Security_error} ->
            {error,
                {security_error,
                    intent@security:format_security_error(Security_error)}}
    end.

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

-spec load_spec(binary()) -> {ok, intent@types:spec()} | {error, load_error()}.
load_spec(Path) ->
    case intent@security:validate_file_path(Path) of
        {ok, Validated_path} ->
            load_and_parse_with_spinner(Validated_path);

        {error, Security_error} ->
            {error,
                {security_error,
                    intent@security:format_security_error(Security_error)}}
    end.

-spec export_spec_json(binary()) -> {ok, binary()} | {error, load_error()}.
export_spec_json(Path) ->
    case intent@security:validate_file_path(Path) of
        {ok, Validated_path} ->
            case shellout:command(
                <<"cue"/utf8>>,
                [<<"export"/utf8>>,
                    Validated_path,
                    <<"-e"/utf8>>,
                    <<"spec"/utf8>>],
                <<"."/utf8>>,
                []
            ) of
                {ok, Json_str} ->
                    {ok, Json_str};

                {error, {_, Stderr}} ->
                    {error, {cue_export_error, Stderr}}
            end;

        {error, Security_error} ->
            {error,
                {security_error,
                    intent@security:format_security_error(Security_error)}}
    end.

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

        {security_error, Msg@4} ->
            Msg@4
    end.
