-module(intent_ffi).
-export([now_ms/0, halt/1, base64_url_decode/1, generate_uuid/0, current_timestamp/0]).

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
    calendar:system_time_to_rfc3339(Now, [{unit, millisecond}]).
