-module(gleam@regexp).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compile/2, from_string/1, check/2, split/2, scan/2, replace/3, match_map/3]).
-export_type([regexp/0, match/0, compile_error/0, options/0]).

-type regexp() :: any().

-type match() :: {match, binary(), list(gleam@option:option(binary()))}.

-type compile_error() :: {compile_error, binary(), integer()}.

-type options() :: {options, boolean(), boolean()}.

-spec compile(binary(), options()) -> {ok, regexp()} | {error, compile_error()}.
compile(Pattern, Options) ->
    gleam_regexp_ffi:compile(Pattern, Options).

-spec from_string(binary()) -> {ok, regexp()} | {error, compile_error()}.
from_string(Pattern) ->
    compile(Pattern, {options, false, false}).

-spec check(regexp(), binary()) -> boolean().
check(Regexp, String) ->
    gleam_regexp_ffi:check(Regexp, String).

-spec split(regexp(), binary()) -> list(binary()).
split(Regexp, String) ->
    gleam_regexp_ffi:split(Regexp, String).

-spec scan(regexp(), binary()) -> list(match()).
scan(Regexp, String) ->
    gleam_regexp_ffi:scan(Regexp, String).

-spec replace(regexp(), binary(), binary()) -> binary().
replace(Pattern, String, Substitute) ->
    gleam_regexp_ffi:replace(Pattern, String, Substitute).

-spec match_map(regexp(), binary(), fun((match()) -> binary())) -> binary().
match_map(Pattern, String, Substitute) ->
    gleam_regexp_ffi:match_map(Pattern, String, Substitute).
