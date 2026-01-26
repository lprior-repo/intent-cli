-module(intent_ffi).
-export([now_ms/0, halt/1, base64_url_decode/1, generate_uuid/0, current_timestamp/0, current_iso8601_timestamp/0, int_to_float/1, get_env/1, write_stderr/1, execute_command/1]).

now_ms() ->
    erlang:system_time(millisecond).

halt(Code) ->
    erlang:halt(Code).

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
    list_to_binary(calendar:system_time_to_rfc3339(Now, [{unit, millisecond}])).

%% Alias for current_timestamp (used by Gleam external declarations)
current_iso8601_timestamp() ->
    current_timestamp().

%% Convert integer to float
int_to_float(I) when is_integer(I) ->
    float(I).

%% Get environment variable value
get_env(Name) when is_binary(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.

%% Write text to stderr
write_stderr(Text) when is_binary(Text) ->
    io:format(standard_error, "~s~n", [Text]),
    nil.

%% Execute a shell command and return output
%% Returns: {stdout_binary, exit_code}
execute_command(Command) when is_binary(Command) ->
    CommandStr = binary_to_list(Command),
    %% Use open_port to properly capture exit status
    %% Execute through sh -c to support complex commands
    Port = open_port({spawn_executable, "/bin/sh"}, [exit_status, use_stdio, stderr_to_stdout, binary, {args, ["-c", CommandStr]}]),
    %% Collect output and exit status
    collect_port_output(Port, <<>>).

collect_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_output(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, Status}} ->
            %% Port closed normally
            %% Status 0 means success, non-zero means failure
            {Acc, Status};
        {'EXIT', Port, Reason} ->
            %% Port crashed
            {Acc, 1}
    after 30000 ->
        %% Timeout after 30 seconds
        {Acc, 124}  %% Timeout exit code
    end.
