-module(pokemon_api_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0, hello_world_test/0]).

-spec main() -> nil.
main() ->
    gleeunit:main().

-spec hello_world_test() -> nil.
hello_world_test() ->
    Name = <<"Joe"/utf8>>,
    Greeting = <<<<"Hello, "/utf8, Name/binary>>/binary, "!"/utf8>>,
    _pipe = Greeting,
    gleeunit_ffi:should_equal(_pipe, <<"Hello, Joe!"/utf8>>).
