-module(intent_ffi).
-export([now_ms/0, halt/1, base64_url_decode/1]).

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
