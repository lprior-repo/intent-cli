-module(intent).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec main() -> nil.
main() ->
    Cli = begin
        _pipe = glint:new(),
        _pipe@1 = glint:with_name(_pipe, <<"intent"/utf8>>),
        _pipe@2 = glint:with_pretty_help(_pipe@1, glint:default_pretty_help()),
        _pipe@3 = glint:add(
            _pipe@2,
            [<<"check"/utf8>>],
            intent@cli@check:check_command()
        ),
        _pipe@4 = glint:add(
            _pipe@3,
            [<<"validate"/utf8>>],
            intent@cli@check:validate_command()
        ),
        _pipe@5 = glint:add(
            _pipe@4,
            [<<"show"/utf8>>],
            intent@cli@check:show_command()
        ),
        _pipe@6 = glint:add(
            _pipe@5,
            [<<"export"/utf8>>],
            intent@cli@check:export_command()
        ),
        _pipe@7 = glint:add(
            _pipe@6,
            [<<"lint"/utf8>>],
            intent@cli@check:lint_command()
        ),
        _pipe@8 = glint:add(
            _pipe@7,
            [<<"analyze"/utf8>>],
            intent@cli@check:analyze_command()
        ),
        _pipe@9 = glint:add(
            _pipe@8,
            [<<"improve"/utf8>>],
            intent@cli@check:improve_command()
        ),
        _pipe@10 = glint:add(
            _pipe@9,
            [<<"interview"/utf8>>],
            intent@cli@interview:interview_command()
        ),
        _pipe@11 = glint:add(
            _pipe@10,
            [<<"beads"/utf8>>],
            intent@cli@interview:beads_command()
        ),
        _pipe@12 = glint:add(
            _pipe@11,
            [<<"bead-status"/utf8>>],
            intent@cli@interview:bead_status_command()
        ),
        _pipe@13 = glint:add(
            _pipe@12,
            [<<"history"/utf8>>],
            intent@cli@interview:history_command()
        ),
        _pipe@14 = glint:add(
            _pipe@13,
            [<<"diff"/utf8>>],
            intent@cli@interview:diff_command()
        ),
        _pipe@15 = glint:add(
            _pipe@14,
            [<<"sessions"/utf8>>],
            intent@cli@interview:sessions_command()
        ),
        _pipe@16 = glint:add(
            _pipe@15,
            [<<"quality"/utf8>>],
            intent@cli@kirk:quality_command()
        ),
        _pipe@17 = glint:add(
            _pipe@16,
            [<<"invert"/utf8>>],
            intent@cli@kirk:invert_command()
        ),
        _pipe@18 = glint:add(
            _pipe@17,
            [<<"coverage"/utf8>>],
            intent@cli@kirk:coverage_command()
        ),
        _pipe@19 = glint:add(
            _pipe@18,
            [<<"gaps"/utf8>>],
            intent@cli@kirk:gaps_command()
        ),
        _pipe@20 = glint:add(
            _pipe@19,
            [<<"compact"/utf8>>],
            intent@cli@kirk:compact_command()
        ),
        _pipe@21 = glint:add(
            _pipe@20,
            [<<"prototext"/utf8>>],
            intent@cli@kirk:prototext_command()
        ),
        _pipe@22 = glint:add(
            _pipe@21,
            [<<"ears"/utf8>>],
            intent@cli@kirk:ears_command()
        ),
        _pipe@23 = glint:add(
            _pipe@22,
            [<<"parse"/utf8>>],
            intent@cli@kirk:parse_command()
        ),
        _pipe@24 = glint:add(
            _pipe@23,
            [<<"effects"/utf8>>],
            intent@cli@kirk:effects_command()
        ),
        _pipe@25 = glint:add(
            _pipe@24,
            [<<"plan"/utf8>>],
            intent@cli@plan:plan_command()
        ),
        _pipe@26 = glint:add(
            _pipe@25,
            [<<"plan-approve"/utf8>>],
            intent@cli@plan:plan_approve_command()
        ),
        glint:add(
            _pipe@26,
            [<<"beads-regenerate"/utf8>>],
            intent@cli@plan:beads_regenerate_command()
        )
    end,
    case glint:execute(Cli, erlang:element(4, argv:load())) of
        {error, Error_message} ->
            gleam@io:println(Error_message),
            intent_ffi:halt(1);

        {ok, {help, Help_text}} ->
            gleam@io:println(Help_text),
            nil;

        {ok, {out, _}} ->
            nil
    end.
