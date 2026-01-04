-module(snag).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/snag.gleam").
-export([new/1, error/1, layer/2, context/2, pretty_print/1, line_print/1]).
-export_type([snag/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type snag() :: {snag, binary(), list(binary())}.

-file("src/snag.gleam", 40).
?DOC(
    " Create a new `Snag` with the given issue text.\n"
    "\n"
    " See also the `error` function for creating a `Snag` wrapped in a `Result`.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > new(\"Not enough credit\")\n"
    " > |> line_print\n"
    " \"error: Not enough credit\"\n"
    " ```\n"
).
-spec new(binary()) -> snag().
new(Issue) ->
    {snag, Issue, []}.

-file("src/snag.gleam", 52).
?DOC(
    " Create a new `Snag` wrapped in a `Result` with the given issue text.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > error(\"Not enough credit\")\n"
    " Error(new(\"Not enough credit\"))\n"
    " ```\n"
).
-spec error(binary()) -> {ok, any()} | {error, snag()}.
error(Issue) ->
    {error, new(Issue)}.

-file("src/snag.gleam", 69).
?DOC(
    " Add additional contextual information to a `Snag`.\n"
    "\n"
    " See also the `context` function for adding contextual information to a `Snag`\n"
    " wrapped in a `Result`.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > new(\"Not enough credit\")\n"
    " > |> layer(\"Unable to make purchase\")\n"
    " > |> line_print\n"
    " \"error: Unable to make purchase <- Not enough credit\"\n"
    " ```\n"
).
-spec layer(snag(), binary()) -> snag().
layer(Snag, Issue) ->
    {snag, Issue, [erlang:element(2, Snag) | erlang:element(3, Snag)]}.

-file("src/snag.gleam", 83).
?DOC(
    " Add additional contextual information to a `Snag` wrapped in a `Result`.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > error(\"Not enough credit\")\n"
    " > |> context(\"Unable to make purchase\")\n"
    " > |> result.map_error(line_print)\n"
    " Error(\"error: Unable to make purchase <- Not enough credit\")\n"
    " ```\n"
).
-spec context({ok, IGR} | {error, snag()}, binary()) -> {ok, IGR} |
    {error, snag()}.
context(Result, Issue) ->
    case Result of
        {ok, _} ->
            Result;

        {error, Snag} ->
            {error, layer(Snag, Issue)}
    end.

-file("src/snag.gleam", 118).
-spec pretty_print_cause(list(binary())) -> gleam@string_builder:string_builder().
pretty_print_cause(Cause) ->
    _pipe = Cause,
    _pipe@1 = gleam@list:index_map(
        _pipe,
        fun(Line, Index) ->
            gleam@string:concat(
                [<<"  "/utf8>>,
                    gleam@int:to_string(Index),
                    <<": "/utf8>>,
                    Line,
                    <<"\n"/utf8>>]
            )
        end
    ),
    gleam@string_builder:from_strings(_pipe@1).

-file("src/snag.gleam", 106).
?DOC(
    " Turn a snag into a multi-line string, optimised for readability.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > new(\"Not enough credit\")\n"
    " > |> layer(\"Unable to make purchase\")\n"
    " > |> layer(\"Character creation failed\")\n"
    " > |> pretty_print\n"
    " \"error: Character creation failed\n"
    "\n"
    " cause:\n"
    "   0: Unable to make purchase\n"
    "   1: Not enough credit\n"
    " \"\n"
    " ```\n"
).
-spec pretty_print(snag()) -> binary().
pretty_print(Snag) ->
    Builder = gleam@string_builder:from_strings(
        [<<"error: "/utf8>>, erlang:element(2, Snag), <<"\n"/utf8>>]
    ),
    gleam@string_builder:to_string(case erlang:element(3, Snag) of
            [] ->
                Builder;

            Cause ->
                _pipe = Builder,
                _pipe@1 = gleam@string_builder:append(
                    _pipe,
                    <<"\ncause:\n"/utf8>>
                ),
                gleam@string_builder:append_builder(
                    _pipe@1,
                    pretty_print_cause(Cause)
                )
        end).

-file("src/snag.gleam", 138).
?DOC(
    " Turn a snag into a single-line string, optimised for compactness. This may be\n"
    " useful for logging snags.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > new(\"Not enough credit\")\n"
    " > |> layer(\"Unable to make purchase\")\n"
    " > |> layer(\"Character creation failed\")\n"
    " > |> pretty_print\n"
    " \"error: Character creation failed <- Unable to make purchase <- Not enough credit\"\n"
    " ```\n"
).
-spec line_print(snag()) -> binary().
line_print(Snag) ->
    _pipe = [gleam@string:append(<<"error: "/utf8>>, erlang:element(2, Snag)) |
        erlang:element(3, Snag)],
    gleam@string:join(_pipe, <<" <- "/utf8>>).
