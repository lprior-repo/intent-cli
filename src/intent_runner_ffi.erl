-module(intent_runner_ffi).
-export([spinner_ref_new/0, spinner_ref_set/2, spinner_ref_get/1]).

%% Mutable reference for spinner state using process dictionary
%% This allows the UI callbacks to share spinner state across closures

spinner_ref_new() ->
    Ref = make_ref(),
    put(Ref, none),
    Ref.

spinner_ref_set(Ref, Spinner) ->
    put(Ref, {some, Spinner}),
    nil.

spinner_ref_get(Ref) ->
    case get(Ref) of
        {some, Spinner} -> {some, Spinner};
        _ -> none
    end.
