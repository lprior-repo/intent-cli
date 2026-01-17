-module(intent_ffi).
-export([now_ms/0, halt/1, mark_command_started/0, base64_url_decode/1, generate_uuid/0, current_timestamp/0, int_to_float/1, get_env/1]).

now_ms() ->
    erlang:system_time(millisecond).

%% Mark that a command has successfully started executing
%% This helps distinguish successful completion from Glint flag parsing errors
mark_command_started() ->
    put(command_started, true),
    nil.

%% Halt with proper exit code
%% If halt(0) is called before any command starts, it's likely a Glint flag error
%% In that case, exit with code 4 (general error) instead of 0 (success)
halt(Code) ->
    Started = get(command_started),
    io:format(standard_error, "DEBUG: halt(~p), command_started=~p~n", [Code, Started]),
    case {Code, Started} of
        {0, undefined} ->
            %% halt(0) called but no command started - likely Glint flag error
            %% Exit with code 4 (general error) instead
            io:format(standard_error, "DEBUG: Changing exit 0 to 4 (Glint flag error)~n", []),
            erlang:halt(4);
        _ ->
            %% Normal halt - use the provided code
            io:format(standard_error, "DEBUG: Using original exit code ~p~n", [Code]),
            erlang:halt(Code)
    end.

%% Base64 URL decode with padding normalization
base64_url_decode(Input) when is_binary(Input) ->
    %% Convert base64url to standard base64
    Standard = << <<(case C of
        $- -> $+;
        $_ -> $/;
        _ -> C
    end)>> || <<C>> <= Input >>,
    %% Add padding if needed
    Padded = case byte_size(Standard) rem 4 of
        0 -> Standard;
        2 -> <<Standard/binary, "==">>;
        3 -> <<Standard/binary, "=">>
    end,
    try
        {ok, base64:decode(Padded)}
    catch
        _:_ -> {error, invalid_base64}
    end.

%% Generate UUID v4 (simple implementation)
generate_uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    Parts = [to_hex(A, 8), "-", to_hex(B, 4), "-", to_hex(C, 4), "-", to_hex(D, 4), "-", to_hex(E, 12)],
    list_to_binary(Parts).

to_hex(N, Width) ->
    Hex = integer_to_list(N, 16),
    string:pad(Hex, Width, leading, $0).

%% Get current timestamp in ISO 8601 format
current_timestamp() ->
    Now = erlang:system_time(millisecond),
    calendar:system_time_to_rfc3339(Now, [{unit, millisecond}]).

%% Convert integer to float
int_to_float(I) when is_integer(I) ->
    float(I).

%% Get environment variable value
get_env(Name) when is_binary(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.
