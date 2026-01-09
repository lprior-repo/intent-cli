-module(glint).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/glint.gleam").
-export([with_config/2, with_pretty_help/2, without_pretty_help/1, with_name/2, as_gleam_module/1, command/1, description/2, unnamed_args/2, named_args/2, flag/3, flag_tuple/2, flags/2, group_flags/3, global_flags/2, group_flag/4, global_flag/3, global_flag_tuple/2, group_flag_tuple/3, default_pretty_help/0, add/3, new/0, help_flag/0, execute/2, run_and_handle/3, run/2]).
-export_type([config/0, pretty_help/0, glint/1, args_count/0, command/1, command_input/0, command_node/1, out/1, metadata/0, flag_help/0, command_help/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type config() :: {config,
        gleam@option:option(pretty_help()),
        gleam@option:option(binary()),
        boolean()}.

-type pretty_help() :: {pretty_help,
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour()}.

-opaque glint(IZA) :: {glint, config(), command_node(IZA)}.

-type args_count() :: {eq_args, integer()} | {min_args, integer()}.

-opaque command(IZB) :: {command,
        fun((command_input()) -> IZB),
        gleam@dict:dict(binary(), glint@flag:flag()),
        binary(),
        gleam@option:option(args_count()),
        list(binary())}.

-type command_input() :: {command_input,
        list(binary()),
        gleam@dict:dict(binary(), glint@flag:flag()),
        gleam@dict:dict(binary(), binary())}.

-type command_node(IZC) :: {command_node,
        gleam@option:option(command(IZC)),
        gleam@dict:dict(binary(), command_node(IZC)),
        gleam@dict:dict(binary(), glint@flag:flag())}.

-type out(IZD) :: {out, IZD} | {help, binary()}.

-type metadata() :: {metadata, binary(), binary()}.

-type flag_help() :: {flag_help, metadata(), binary()}.

-type command_help() :: {command_help,
        metadata(),
        list(flag_help()),
        list(metadata()),
        gleam@option:option(args_count()),
        list(binary())}.

-file("src/glint.gleam", 50).
?DOC(" Add the provided config to the existing command tree\n").
-spec with_config(glint(IZJ), config()) -> glint(IZJ).
with_config(Glint, Config) ->
    {glint, Config, erlang:element(3, Glint)}.

-file("src/glint.gleam", 57).
?DOC(
    " Enable custom colours for help text headers\n"
    " For a pre-made colouring use `default_pretty_help()`\n"
).
-spec with_pretty_help(glint(IZM), pretty_help()) -> glint(IZM).
with_pretty_help(Glint, Pretty) ->
    _pipe = begin
        _record = erlang:element(2, Glint),
        {config,
            {some, Pretty},
            erlang:element(3, _record),
            erlang:element(4, _record)}
    end,
    with_config(Glint, _pipe).

-file("src/glint.gleam", 64).
?DOC(" Disable custom colours for help text headers\n").
-spec without_pretty_help(glint(IZP)) -> glint(IZP).
without_pretty_help(Glint) ->
    _pipe = begin
        _record = erlang:element(2, Glint),
        {config, none, erlang:element(3, _record), erlang:element(4, _record)}
    end,
    with_config(Glint, _pipe).

-file("src/glint.gleam", 71).
?DOC(" Give the current glint application a name\n").
-spec with_name(glint(IZS), binary()) -> glint(IZS).
with_name(Glint, Name) ->
    _pipe = begin
        _record = erlang:element(2, Glint),
        {config,
            erlang:element(2, _record),
            {some, Name},
            erlang:element(4, _record)}
    end,
    with_config(Glint, _pipe).

-file("src/glint.gleam", 78).
?DOC(
    " Adjust the generated help text to reflect that the current glint app should be run as a gleam module.\n"
    " Use in conjunction with `glint.with_name` to get usage text output like `gleam run -m <name>`\n"
).
-spec as_gleam_module(glint(IZV)) -> glint(IZV).
as_gleam_module(Glint) ->
    _pipe = begin
        _record = erlang:element(2, Glint),
        {config, erlang:element(2, _record), erlang:element(3, _record), true}
    end,
    with_config(Glint, _pipe).

-file("src/glint.gleam", 216).
?DOC(" Helper for initializing empty commands\n").
-spec empty_command() -> command_node(any()).
empty_command() ->
    {command_node, none, gleam@dict:new(), gleam@dict:new()}.

-file("src/glint.gleam", 192).
?DOC(" Recursive traversal of the command tree to find where to puth the provided command\n").
-spec do_add(command_node(JAF), list(binary()), command(JAF)) -> command_node(JAF).
do_add(Root, Path, Contents) ->
    case Path of
        [] ->
            {command_node,
                {some, Contents},
                erlang:element(3, Root),
                erlang:element(4, Root)};

        [X | Xs] ->
            {command_node,
                erlang:element(2, Root),
                begin
                    gleam@dict:update(
                        erlang:element(3, Root),
                        X,
                        fun(Node) -> _pipe = Node,
                            _pipe@1 = gleam@option:lazy_unwrap(
                                _pipe,
                                fun empty_command/0
                            ),
                            do_add(_pipe@1, Xs, Contents) end
                    )
                end,
                erlang:element(4, Root)}
    end.

-file("src/glint.gleam", 230).
?DOC(" Create a Command(a) from a Runner(a)\n").
-spec command(fun((command_input()) -> JAO)) -> command(JAO).
command(Runner) ->
    {command, Runner, gleam@dict:new(), <<""/utf8>>, none, []}.

-file("src/glint.gleam", 242).
?DOC(" Attach a description to a Command(a)\n").
-spec description(command(JAR), binary()) -> command(JAR).
description(Cmd, Description) ->
    {command,
        erlang:element(2, Cmd),
        erlang:element(3, Cmd),
        Description,
        erlang:element(5, Cmd),
        erlang:element(6, Cmd)}.

-file("src/glint.gleam", 248).
?DOC(" Specify a specific number of unnamed args that a given command expects\n").
-spec unnamed_args(command(JAU), args_count()) -> command(JAU).
unnamed_args(Cmd, Count) ->
    {command,
        erlang:element(2, Cmd),
        erlang:element(3, Cmd),
        erlang:element(4, Cmd),
        {some, Count},
        erlang:element(6, Cmd)}.

-file("src/glint.gleam", 258).
?DOC(
    " Add a list of named arguments to a Command\n"
    " These named arguments will be matched with the first N arguments passed to the command\n"
    " All named arguments must match for a command to succeed\n"
    " This works in combination with CommandInput.named_args which will contain the matched args in a Dict(String,String)\n"
    " IMPORTANT: Matched named arguments will not be present in CommandInput.args\n"
).
-spec named_args(command(JAX), list(binary())) -> command(JAX).
named_args(Cmd, Args) ->
    {command,
        erlang:element(2, Cmd),
        erlang:element(3, Cmd),
        erlang:element(4, Cmd),
        erlang:element(5, Cmd),
        Args}.

-file("src/glint.gleam", 264).
?DOC(" Add a `flag.Flag` to a `Command`\n").
-spec flag(command(JBB), binary(), glint@flag:flag_builder(any())) -> command(JBB).
flag(Cmd, Key, Flag) ->
    {command,
        erlang:element(2, Cmd),
        gleam@dict:insert(erlang:element(3, Cmd), Key, glint@flag:build(Flag)),
        erlang:element(4, Cmd),
        erlang:element(5, Cmd),
        erlang:element(6, Cmd)}.

-file("src/glint.gleam", 276).
?DOC(
    " Add a `flag.Flag to a `Command` when the flag name and builder are bundled as a #(String, flag.FlagBuilder(a)).\n"
    "\n"
    " This is merely a convenience function and calls `glint.flag` under the hood.\n"
).
-spec flag_tuple(command(JBG), {binary(), glint@flag:flag_builder(any())}) -> command(JBG).
flag_tuple(Cmd, Tup) ->
    flag(Cmd, erlang:element(1, Tup), erlang:element(2, Tup)).

-file("src/glint.gleam", 288).
?DOC(
    " Add multiple `Flag`s to a `Command`, note that this function uses `Flag` and not `FlagBuilder(_)`.\n"
    " The user will need to call `flag.build` before providing the flags here.\n"
    "\n"
    " It is recommended to call `glint.flag` instead.\n"
).
-spec flags(command(JBL), list({binary(), glint@flag:flag()})) -> command(JBL).
flags(Cmd, Flags) ->
    gleam@list:fold(
        Flags,
        Cmd,
        fun(Cmd@1, _use1) ->
            {Key, Flag} = _use1,
            {command,
                erlang:element(2, Cmd@1),
                gleam@dict:insert(erlang:element(3, Cmd@1), Key, Flag),
                erlang:element(4, Cmd@1),
                erlang:element(5, Cmd@1),
                erlang:element(6, Cmd@1)}
        end
    ).

-file("src/glint.gleam", 380).
?DOC(
    " add a group flag to a command node\n"
    " descend recursively down the command tree to find the node that the flag should be inserted at\n"
).
-spec do_group_flag(
    command_node(JCU),
    list(binary()),
    binary(),
    glint@flag:flag()
) -> command_node(JCU).
do_group_flag(Node, Path, Name, Flag) ->
    case Path of
        [] ->
            {command_node,
                erlang:element(2, Node),
                erlang:element(3, Node),
                gleam@dict:insert(erlang:element(4, Node), Name, Flag)};

        [Head | Tail] ->
            {command_node,
                erlang:element(2, Node),
                begin
                    gleam@dict:update(
                        erlang:element(3, Node),
                        Head,
                        fun(Node@1) -> _pipe = Node@1,
                            _pipe@1 = gleam@option:unwrap(
                                _pipe,
                                empty_command()
                            ),
                            do_group_flag(_pipe@1, Tail, Name, Flag) end
                    )
                end,
                erlang:element(4, Node)}
    end.

-file("src/glint.gleam", 337).
?DOC(
    " Add flags for groups of commands.\n"
    " It is recommended to use `glint.group_flag` instead if possible\n"
    "\n"
    " The provided flags will be available to all commands at or beyond the provided path\n"
    "\n"
    " Note: use of this function requires calling `flag.build` yourself on any `flag.FlagBuilder`s you wish to convert to `flag.Flag`s\n"
).
-spec group_flags(
    glint(JCD),
    list(binary()),
    list({binary(), glint@flag:flag()})
) -> glint(JCD).
group_flags(Glint, Path, Flags) ->
    gleam@list:fold(
        Flags,
        Glint,
        fun(Glint@1, Flag) ->
            {glint,
                erlang:element(2, Glint@1),
                do_group_flag(
                    erlang:element(3, Glint@1),
                    Path,
                    erlang:element(1, Flag),
                    erlang:element(2, Flag)
                )}
        end
    ).

-file("src/glint.gleam", 326).
?DOC(
    " Add global flags to the existing command tree.\n"
    "\n"
    " Like `glint.flags`, this function requires `Flag`s insead of `FlagBuilder(_)`.\n"
    " This is the equivalent to calling `glint.group_flags` with a path parameter of `[]`.\n"
    "\n"
    " Note: use of this function requires calling `flag.build` yourself on any `flag.FlagBuilder`s you wish to convert to `flag.Flag`s\n"
    " It is recommended to use `glint.global_flag` instead.\n"
).
-spec global_flags(glint(JBZ), list({binary(), glint@flag:flag()})) -> glint(JBZ).
global_flags(Glint, Flags) ->
    group_flags(Glint, [], Flags).

-file("src/glint.gleam", 352).
?DOC(
    " Add a flag for a group of commands.\n"
    " The provided flags will be available to all commands at or beyond the provided path\n"
).
-spec group_flag(
    glint(JCI),
    list(binary()),
    binary(),
    glint@flag:flag_builder(any())
) -> glint(JCI).
group_flag(Glint, Path, Name, Flag) ->
    {glint,
        erlang:element(2, Glint),
        do_group_flag(
            erlang:element(3, Glint),
            Path,
            Name,
            glint@flag:build(Flag)
        )}.

-file("src/glint.gleam", 298).
?DOC(
    " Add global flags to the existing command tree\n"
    " This is the equivalent to calling `glint.group_flag` with a path parameter of `[]`.\n"
).
-spec global_flag(glint(JBP), binary(), glint@flag:flag_builder(any())) -> glint(JBP).
global_flag(Glint, Key, Flag) ->
    group_flag(Glint, [], Key, Flag).

-file("src/glint.gleam", 310).
?DOC(
    " Add global flags to the existing command tree.\n"
    " This is the equivalent to calling `glint.group_flag_tuple` with a path parameter of `[]`.\n"
).
-spec global_flag_tuple(glint(JBU), {binary(), glint@flag:flag_builder(any())}) -> glint(JBU).
global_flag_tuple(Glint, Tup) ->
    group_flag(Glint, [], erlang:element(1, Tup), erlang:element(2, Tup)).

-file("src/glint.gleam", 369).
?DOC(
    " Add a flag for a group of commands.\n"
    " The provided flags will be available to all commands at or beyond the provided path\n"
    "\n"
    " This is a convenience function and calls `glint.group_flag` under the hood.\n"
).
-spec group_flag_tuple(
    glint(JCO),
    list(binary()),
    {binary(), glint@flag:flag_builder(any())}
) -> glint(JCO).
group_flag_tuple(Glint, Path, Flag) ->
    group_flag(Glint, Path, erlang:element(1, Flag), erlang:element(2, Flag)).

-file("src/glint.gleam", 488).
-spec args_compare(args_count(), integer()) -> {ok, nil} | {error, snag:snag()}.
args_compare(Expected, Actual) ->
    _pipe = case Expected of
        {eq_args, Expected@1} when Actual =:= Expected@1 ->
            {ok, nil};

        {min_args, Expected@2} when Actual >= Expected@2 ->
            {ok, nil};

        {eq_args, Expected@3} ->
            {error, gleam@int:to_string(Expected@3)};

        {min_args, Expected@4} ->
            {error,
                <<"at least "/utf8, (gleam@int:to_string(Expected@4))/binary>>}
    end,
    gleam@result:map_error(
        _pipe,
        fun(Err) ->
            snag:new(
                <<<<<<"expected: "/utf8, Err/binary>>/binary,
                        " argument(s), provided: "/utf8>>/binary,
                    (gleam@int:to_string(Actual))/binary>>
            )
        end
    ).

-file("src/glint.gleam", 596).
?DOC(
    " Default pretty help heading colouring\n"
    " mint (r: 182, g: 255, b: 234) colour for usage\n"
    " pink (r: 255, g: 175, b: 243) colour for flags\n"
    " buttercup (r: 252, g: 226, b: 174) colour for subcommands\n"
).
-spec default_pretty_help() -> pretty_help().
default_pretty_help() ->
    Usage_colour@1 = case gleam_community@colour:from_rgb255(182, 255, 234) of
        {ok, Usage_colour} -> Usage_colour;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 597,
                        value => _assert_fail,
                        start => 16808,
                        'end' => 16871,
                        pattern_start => 16819,
                        pattern_end => 16835})
    end,
    Flags_colour@1 = case gleam_community@colour:from_rgb255(255, 175, 243) of
        {ok, Flags_colour} -> Flags_colour;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 598,
                        value => _assert_fail@1,
                        start => 16874,
                        'end' => 16937,
                        pattern_start => 16885,
                        pattern_end => 16901})
    end,
    Subcommands_colour@1 = case gleam_community@colour:from_rgb255(
        252,
        226,
        174
    ) of
        {ok, Subcommands_colour} -> Subcommands_colour;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 599,
                        value => _assert_fail@2,
                        start => 16940,
                        'end' => 17009,
                        pattern_start => 16951,
                        pattern_end => 16973})
    end,
    {pretty_help, Usage_colour@1, Flags_colour@1, Subcommands_colour@1}.

-file("src/glint.gleam", 617).
?DOC(" Helper for filtering out empty strings\n").
-spec is_not_empty(binary()) -> boolean().
is_not_empty(S) ->
    S /= <<""/utf8>>.

-file("src/glint.gleam", 222).
?DOC(" Trim each path element and remove any resulting empty strings.\n").
-spec sanitize_path(list(binary())) -> list(binary()).
sanitize_path(Path) ->
    _pipe = Path,
    _pipe@1 = gleam@list:map(_pipe, fun gleam@string:trim/1),
    gleam@list:filter(_pipe@1, fun is_not_empty/1).

-file("src/glint.gleam", 177).
?DOC(
    " Adds a new command to be run at the specified path.\n"
    "\n"
    " If the path is `[]`, the root command is set with the provided function and\n"
    " flags.\n"
    "\n"
    " Note: all command paths are sanitized by stripping whitespace and removing any empty string elements.\n"
).
-spec add(glint(JAA), list(binary()), command(JAA)) -> glint(JAA).
add(Glint, Path, Contents) ->
    {glint,
        erlang:element(2, Glint),
        begin
            _pipe = Path,
            _pipe@1 = sanitize_path(_pipe),
            do_add(erlang:element(3, Glint), _pipe@1, Contents)
        end}.

-file("src/glint.gleam", 648).
?DOC(
    " Style heading text with the provided rgb colouring\n"
    " this is only intended for use within glint itself.\n"
).
-spec heading_style(binary(), gleam_community@colour:colour()) -> binary().
heading_style(Heading, Colour) ->
    _pipe = Heading,
    _pipe@1 = gleam_community@ansi:bold(_pipe),
    _pipe@2 = gleam_community@ansi:underline(_pipe@1),
    _pipe@3 = gleam_community@ansi:italic(_pipe@2),
    _pipe@4 = gleam_community@ansi:hex(
        _pipe@3,
        gleam_community@colour:to_rgb_hex(Colour)
    ),
    gleam_community@ansi:reset(_pipe@4).

-file("src/glint.gleam", 718).
?DOC(" generate the string representation for the type of a flag\n").
-spec flag_type_info(glint@flag:flag()) -> binary().
flag_type_info(Flag) ->
    case erlang:element(2, Flag) of
        {i, _} ->
            <<"INT"/utf8>>;

        {b, _} ->
            <<"BOOL"/utf8>>;

        {f, _} ->
            <<"FLOAT"/utf8>>;

        {l_f, _} ->
            <<"FLOAT_LIST"/utf8>>;

        {l_i, _} ->
            <<"INT_LIST"/utf8>>;

        {l_s, _} ->
            <<"STRING_LIST"/utf8>>;

        {s, _} ->
            <<"STRING"/utf8>>
    end.

-file("src/glint.gleam", 732).
?DOC(" build the help representation for a list of flags\n").
-spec build_flags_help(gleam@dict:dict(binary(), glint@flag:flag())) -> list(flag_help()).
build_flags_help(Flag) ->
    gleam@dict:fold(
        Flag,
        [],
        fun(Acc, Name, Flag@1) ->
            [{flag_help,
                    {metadata, Name, erlang:element(3, Flag@1)},
                    flag_type_info(Flag@1)} |
                Acc]
        end
    ).

-file("src/glint.gleam", 745).
?DOC(" build the help representation for a list of subcommands\n").
-spec build_subcommands_help(gleam@dict:dict(binary(), command_node(any()))) -> list(metadata()).
build_subcommands_help(Subcommands) ->
    gleam@dict:fold(
        Subcommands,
        [],
        fun(Acc, Name, Cmd) ->
            [{metadata,
                    Name,
                    begin
                        _pipe = erlang:element(2, Cmd),
                        _pipe@1 = gleam@option:map(
                            _pipe,
                            fun(Command) -> erlang:element(4, Command) end
                        ),
                        gleam@option:unwrap(_pipe@1, <<""/utf8>>)
                    end} |
                Acc]
        end
    ).

-file("src/glint.gleam", 693).
?DOC(" build the help representation for a subtree of commands\n").
-spec build_command_help_metadata(binary(), command_node(any())) -> command_help().
build_command_help_metadata(Name, Node) ->
    {Description, Flags, Unnamed_args, Named_args} = case erlang:element(
        2,
        Node
    ) of
        none ->
            {<<""/utf8>>, [], none, []};

        {some, Cmd} ->
            {erlang:element(4, Cmd),
                build_flags_help(
                    gleam@dict:merge(
                        erlang:element(4, Node),
                        erlang:element(3, Cmd)
                    )
                ),
                erlang:element(5, Cmd),
                erlang:element(6, Cmd)}
    end,
    {command_help,
        {metadata, Name, Description},
        Flags,
        build_subcommands_help(erlang:element(3, Node)),
        Unnamed_args,
        Named_args}.

-file("src/glint.gleam", 808).
?DOC(" convert an ArgsCount to a string for usage text\n").
-spec args_count_to_usage_string(args_count()) -> binary().
args_count_to_usage_string(Count) ->
    case Count of
        {eq_args, 0} ->
            <<""/utf8>>;

        {eq_args, 1} ->
            <<"[ 1 argument ]"/utf8>>;

        {eq_args, N} ->
            <<<<"[ "/utf8, (gleam@int:to_string(N))/binary>>/binary,
                " arguments ]"/utf8>>;

        {min_args, N@1} ->
            <<<<"[ "/utf8, (gleam@int:to_string(N@1))/binary>>/binary,
                " or more arguments ]"/utf8>>
    end.

-file("src/glint.gleam", 817).
-spec args_to_usage_string(gleam@option:option(args_count()), list(binary())) -> binary().
args_to_usage_string(Unnamed, Named) ->
    Named_args = begin
        _pipe = Named,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(S) -> <<<<"<"/utf8, S/binary>>/binary, ">"/utf8>> end
        ),
        gleam@string:join(_pipe@1, <<" "/utf8>>)
    end,
    Unnamed_args = begin
        _pipe@2 = gleam@option:map(Unnamed, fun args_count_to_usage_string/1),
        gleam@option:unwrap(_pipe@2, <<"[ ARGS ]"/utf8>>)
    end,
    case {Named_args, Unnamed_args} of
        {<<""/utf8>>, <<""/utf8>>} ->
            <<""/utf8>>;

        {<<""/utf8>>, _} ->
            Unnamed_args;

        {_, <<""/utf8>>} ->
            Named_args;

        {_, _} ->
            <<<<Named_args/binary, " "/utf8>>/binary, Unnamed_args/binary>>
    end.

-file("src/glint.gleam", 885).
?DOC(" generate the help text for a flag without a description\n").
-spec flag_help_to_string(flag_help()) -> binary().
flag_help_to_string(Help) ->
    <<<<<<<<(<<"--"/utf8>>)/binary,
                    (erlang:element(2, erlang:element(2, Help)))/binary>>/binary,
                "=<"/utf8>>/binary,
            (erlang:element(3, Help))/binary>>/binary,
        ">"/utf8>>.

-file("src/glint.gleam", 786).
?DOC(" convert a List(FlagHelp) to a list of strings for use in usage text\n").
-spec flags_help_to_usage_strings(list(flag_help())) -> list(binary()).
flags_help_to_usage_strings(Help) ->
    _pipe = Help,
    _pipe@1 = gleam@list:map(_pipe, fun flag_help_to_string/1),
    gleam@list:sort(_pipe@1, fun gleam@string:compare/2).

-file("src/glint.gleam", 794).
?DOC(" generate the usage help text for the flags of a command\n").
-spec flags_help_to_usage_string(list(flag_help())) -> binary().
flags_help_to_usage_string(Help) ->
    gleam@bool:guard(Help =:= [], <<""/utf8>>, fun() -> _pipe = Help,
            _pipe@1 = flags_help_to_usage_strings(_pipe),
            _pipe@2 = gleam@list:intersperse(_pipe@1, <<" "/utf8>>),
            _pipe@3 = gleam@string_builder:from_strings(_pipe@2),
            _pipe@4 = gleam@string_builder:prepend(_pipe@3, <<"[ "/utf8>>),
            _pipe@5 = gleam@string_builder:append(_pipe@4, <<" ]"/utf8>>),
            gleam@string_builder:to_string(_pipe@5) end).

-file("src/glint.gleam", 891).
?DOC(" generate the help text for a flag with a description\n").
-spec flag_help_to_string_with_description(flag_help()) -> binary().
flag_help_to_string_with_description(Help) ->
    <<<<(flag_help_to_string(Help))/binary, "\t\t"/utf8>>/binary,
        (erlang:element(3, erlang:element(2, Help)))/binary>>.

-file("src/glint.gleam", 917).
?DOC(" generate the help text for a single subcommand given its name and description\n").
-spec subcommand_help_to_string(metadata()) -> binary().
subcommand_help_to_string(Help) ->
    case erlang:element(3, Help) of
        <<""/utf8>> ->
            erlang:element(2, Help);

        _ ->
            <<<<(erlang:element(2, Help))/binary, "\t\t"/utf8>>/binary,
                (erlang:element(3, Help))/binary>>
    end.

-file("src/glint.gleam", 924).
-spec string_map(binary(), fun((binary()) -> binary())) -> binary().
string_map(S, F) ->
    case S of
        <<""/utf8>> ->
            <<""/utf8>>;

        _ ->
            F(S)
    end.

-file("src/glint.gleam", 166).
?DOC(" Creates a new command tree.\n").
-spec new() -> glint(any()).
new() ->
    {glint, {config, none, none, false}, empty_command()}.

-file("src/glint.gleam", 899).
?DOC(" generate the styled help text for a list of subcommands\n").
-spec subcommands_help_to_string(list(metadata()), config()) -> binary().
subcommands_help_to_string(Help, Config) ->
    gleam@bool:guard(
        Help =:= [],
        <<""/utf8>>,
        fun() -> <<(case erlang:element(2, Config) of
                    none ->
                        <<"SUBCOMMANDS:"/utf8>>;

                    {some, Pretty} ->
                        heading_style(
                            <<"SUBCOMMANDS:"/utf8>>,
                            erlang:element(4, Pretty)
                        )
                end)/binary, (begin
                    _pipe = Help,
                    _pipe@1 = gleam@list:map(
                        _pipe,
                        fun subcommand_help_to_string/1
                    ),
                    _pipe@2 = gleam@list:sort(
                        _pipe@1,
                        fun gleam@string:compare/2
                    ),
                    _pipe@3 = gleam@list:map(
                        _pipe@2,
                        fun(_capture) ->
                            gleam@string:append(<<"\n\t"/utf8>>, _capture)
                        end
                    ),
                    gleam@string:concat(_pipe@3)
                end)/binary>> end
    ).

-file("src/glint.gleam", 839).
?DOC(" convert a CommandHelp to a styled usage block\n").
-spec command_help_to_usage_string(command_help(), config()) -> binary().
command_help_to_usage_string(Help, Config) ->
    App_name = case erlang:element(3, Config) of
        {some, Name} when erlang:element(4, Config) ->
            <<"gleam run -m "/utf8, Name/binary>>;

        {some, Name@1} ->
            Name@1;

        none ->
            <<"gleam run"/utf8>>
    end,
    Flags = flags_help_to_usage_string(erlang:element(3, Help)),
    Args = args_to_usage_string(
        erlang:element(5, Help),
        erlang:element(6, Help)
    ),
    <<<<<<<<<<(case erlang:element(2, Config) of
                            none ->
                                <<"USAGE:"/utf8>>;

                            {some, Pretty} ->
                                heading_style(
                                    <<"USAGE:"/utf8>>,
                                    erlang:element(2, Pretty)
                                )
                        end)/binary, "\n\t"/utf8>>/binary, App_name/binary>>/binary, (string_map(
                    erlang:element(2, erlang:element(2, Help)),
                    fun(_capture) ->
                        gleam@string:append(<<" "/utf8>>, _capture)
                    end
                ))/binary>>/binary, (case Args of
                <<""/utf8>> ->
                    <<" "/utf8>>;

                _ ->
                    <<<<" "/utf8, Args/binary>>/binary, " "/utf8>>
            end)/binary>>/binary, Flags/binary>>.

-file("src/glint.gleam", 628).
?DOC(
    " Function to create the help flag string\n"
    " Exported for testing purposes only\n"
).
-spec help_flag() -> binary().
help_flag() ->
    <<(<<"--"/utf8>>)/binary, "help"/utf8>>.

-file("src/glint.gleam", 868).
?DOC(" generate the usage help string for a command\n").
-spec flags_help_to_string(list(flag_help()), config()) -> binary().
flags_help_to_string(Help, Config) ->
    gleam@bool:guard(
        Help =:= [],
        <<""/utf8>>,
        fun() -> <<(case erlang:element(2, Config) of
                    none ->
                        <<"FLAGS:"/utf8>>;

                    {some, Pretty} ->
                        heading_style(
                            <<"FLAGS:"/utf8>>,
                            erlang:element(3, Pretty)
                        )
                end)/binary, (begin
                    _pipe = [<<"--help\t\t\tPrint help information"/utf8>> |
                        gleam@list:map(
                            Help,
                            fun flag_help_to_string_with_description/1
                        )],
                    _pipe@1 = gleam@list:sort(_pipe, fun gleam@string:compare/2),
                    _pipe@2 = gleam@list:map(
                        _pipe@1,
                        fun(_capture) ->
                            gleam@string:append(<<"\n\t"/utf8>>, _capture)
                        end
                    ),
                    gleam@string:concat(_pipe@2)
                end)/binary>> end
    ).

-file("src/glint.gleam", 764).
?DOC(" convert a CommandHelp to a styled string\n").
-spec command_help_to_string(command_help(), config()) -> binary().
command_help_to_string(Help, Config) ->
    Header_items = begin
        _pipe = [erlang:element(2, erlang:element(2, Help)),
            erlang:element(3, erlang:element(2, Help))],
        _pipe@1 = gleam@list:filter(_pipe, fun is_not_empty/1),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    _pipe@2 = [Header_items,
        command_help_to_usage_string(Help, Config),
        flags_help_to_string(erlang:element(3, Help), Config),
        subcommands_help_to_string(erlang:element(4, Help), Config)],
    _pipe@3 = gleam@list:filter(_pipe@2, fun is_not_empty/1),
    gleam@string:join(_pipe@3, <<"\n\n"/utf8>>).

-file("src/glint.gleam", 635).
?DOC(" generate the help text for a command\n").
-spec cmd_help(list(binary()), command_node(any()), config()) -> binary().
cmd_help(Path, Cmd, Config) ->
    _pipe = Path,
    _pipe@1 = lists:reverse(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<" "/utf8>>),
    _pipe@3 = build_command_help_metadata(_pipe@2, Cmd),
    command_help_to_string(_pipe@3, Config).

-file("src/glint.gleam", 504).
?DOC(" Executes the current root command.\n").
-spec execute_root(
    list(binary()),
    config(),
    command_node(JDK),
    list(binary()),
    list(binary())
) -> {ok, out(JDK)} | {error, binary()}.
execute_root(Path, Config, Cmd, Args, Flag_inputs) ->
    Res = begin
        _pipe@7 = begin
            gleam@option:map(
                erlang:element(2, Cmd),
                fun(Contents) ->
                    gleam@result:'try'(
                        gleam@list:try_fold(
                            Flag_inputs,
                            gleam@dict:merge(
                                erlang:element(4, Cmd),
                                erlang:element(3, Contents)
                            ),
                            fun glint@flag:update_flags/2
                        ),
                        fun(New_flags) ->
                            gleam@result:'try'(
                                begin
                                    Named = gleam@list:zip(
                                        erlang:element(6, Contents),
                                        Args
                                    ),
                                    case erlang:length(Named) =:= erlang:length(
                                        erlang:element(6, Contents)
                                    ) of
                                        true ->
                                            {ok, maps:from_list(Named)};

                                        false ->
                                            snag:error(
                                                <<"unmatched named arguments: "/utf8,
                                                    (begin
                                                        _pipe = erlang:element(
                                                            6,
                                                            Contents
                                                        ),
                                                        _pipe@1 = gleam@list:drop(
                                                            _pipe,
                                                            erlang:length(Named)
                                                        ),
                                                        _pipe@2 = gleam@list:map(
                                                            _pipe@1,
                                                            fun(S) ->
                                                                <<<<"'"/utf8,
                                                                        S/binary>>/binary,
                                                                    "'"/utf8>>
                                                            end
                                                        ),
                                                        gleam@string:join(
                                                            _pipe@2,
                                                            <<", "/utf8>>
                                                        )
                                                    end)/binary>>
                                            )
                                    end
                                end,
                                fun(Named_args) ->
                                    Args@1 = gleam@list:drop(
                                        Args,
                                        maps:size(Named_args)
                                    ),
                                    gleam@result:map(
                                        case erlang:element(5, Contents) of
                                            {some, Count} ->
                                                _pipe@3 = Count,
                                                _pipe@4 = args_compare(
                                                    _pipe@3,
                                                    erlang:length(Args@1)
                                                ),
                                                snag:context(
                                                    _pipe@4,
                                                    <<"invalid number of arguments provided"/utf8>>
                                                );

                                            none ->
                                                {ok, nil}
                                        end,
                                        fun(_) ->
                                            _pipe@5 = {command_input,
                                                Args@1,
                                                New_flags,
                                                Named_args},
                                            _pipe@6 = (erlang:element(
                                                2,
                                                Contents
                                            ))(_pipe@5),
                                            {out, _pipe@6}
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end,
        _pipe@8 = gleam@option:unwrap(
            _pipe@7,
            snag:error(<<"command not found"/utf8>>)
        ),
        _pipe@9 = snag:context(_pipe@8, <<"failed to run command"/utf8>>),
        gleam@result:map_error(
            _pipe@9,
            fun(Err) -> {Err, cmd_help(Path, Cmd, Config)} end
        )
    end,
    case Res of
        {ok, Out} ->
            {ok, Out};

        {error, {Snag, Help}} ->
            {error,
                <<<<(snag:pretty_print(Snag))/binary,
                        "\nSee the following help text, available via the '--help' flag.\n\n"/utf8>>/binary,
                    Help/binary>>}
    end.

-file("src/glint.gleam", 437).
?DOC(" Find which command to execute and run it with computed flags and args\n").
-spec do_execute(
    command_node(JDC),
    config(),
    list(binary()),
    list(binary()),
    boolean(),
    list(binary())
) -> {ok, out(JDC)} | {error, binary()}.
do_execute(Cmd, Config, Args, Flags, Help, Command_path) ->
    case Args of
        [] when Help ->
            _pipe = Command_path,
            _pipe@1 = cmd_help(_pipe, Cmd, Config),
            _pipe@2 = {help, _pipe@1},
            {ok, _pipe@2};

        [] ->
            execute_root(Command_path, Config, Cmd, [], Flags);

        [Arg | Rest] ->
            case gleam@dict:get(erlang:element(3, Cmd), Arg) of
                {ok, Sub_command} ->
                    Sub_command@1 = {command_node,
                        erlang:element(2, Sub_command),
                        erlang:element(3, Sub_command),
                        gleam@dict:merge(
                            erlang:element(4, Cmd),
                            erlang:element(4, Sub_command)
                        )},
                    do_execute(
                        Sub_command@1,
                        Config,
                        Rest,
                        Flags,
                        Help,
                        [Arg | Command_path]
                    );

                _ when Help ->
                    _pipe@3 = Command_path,
                    _pipe@4 = cmd_help(_pipe@3, Cmd, Config),
                    _pipe@5 = {help, _pipe@4},
                    {ok, _pipe@5};

                _ ->
                    execute_root(Command_path, Config, Cmd, Args, Flags)
            end
    end.

-file("src/glint.gleam", 418).
?DOC(
    " Determines which command to run and executes it.\n"
    "\n"
    " Sets any provided flags if necessary.\n"
    "\n"
    " Each value prefixed with `--` is parsed as a flag.\n"
    "\n"
    " This function does not print its output and is mainly intended for use within `glint` itself.\n"
    " If you would like to print or handle the output of a command please see the `run_and_handle` function.\n"
).
-spec execute(glint(JCY), list(binary())) -> {ok, out(JCY)} | {error, binary()}.
execute(Glint, Args) ->
    Help_flag = help_flag(),
    {Help, Args@2} = case gleam@list:pop(Args, fun(S) -> S =:= Help_flag end) of
        {ok, {_, Args@1}} ->
            {true, Args@1};

        _ ->
            {false, Args}
    end,
    {Flags, Args@3} = gleam@list:partition(
        Args@2,
        fun(_capture) -> gleam@string:starts_with(_capture, <<"--"/utf8>>) end
    ),
    do_execute(
        erlang:element(3, Glint),
        erlang:element(2, Glint),
        Args@3,
        Flags,
        Help,
        []
    ).

-file("src/glint.gleam", 577).
?DOC(
    " A wrapper for `execute` that prints any errors enountered or the help text if requested.\n"
    " This function calls the provided handler with the value returned by the command that was run.\n"
).
-spec run_and_handle(glint(JDS), list(binary()), fun((JDS) -> any())) -> nil.
run_and_handle(Glint, Args, Handle) ->
    case execute(Glint, Args) of
        {error, S} ->
            gleam@io:println(S);

        {ok, {help, S}} ->
            gleam@io:println(S);

        {ok, {out, Out}} ->
            Handle(Out),
            nil
    end.

-file("src/glint.gleam", 570).
?DOC(
    " A wrapper for `execute` that prints any errors enountered or the help text if requested.\n"
    " This function ignores any value returned by the command that was run.\n"
    " If you would like to do something with the command output please see the run_and_handle function.\n"
).
-spec run(glint(any()), list(binary())) -> nil.
run(Glint, Args) ->
    run_and_handle(Glint, Args, fun(_) -> nil end).
