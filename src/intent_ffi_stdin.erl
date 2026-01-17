-module(intent_ffi_stdin).
-export([read_line/0, read_line_trimmed/0]).

%% Read a line from standard input
%% Returns {ok, Line} on success or {error, Reason} on failure
read_line() ->
    case io:get_line("") of
        eof ->
            {error, <<"End of input">>};
        {error, Reason} ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))};
        Line when is_list(Line) ->
            %% Remove trailing newline if present
            Stripped = string:chomp(Line),
            {ok, list_to_binary(Stripped)}
    end.

%% Read a line from standard input and trim whitespace
%% Returns {ok, TrimmedLine} on success or {error, Reason} on failure
read_line_trimmed() ->
    case read_line() of
        {ok, Line} ->
            Trimmed = string:trim(Line, both),
            {ok, Trimmed};
        {error, Reason} ->
            {error, Reason}
    end.
