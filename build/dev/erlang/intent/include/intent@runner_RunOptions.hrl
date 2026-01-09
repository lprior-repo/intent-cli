-record(run_options, {
    feature_filter :: gleam@option:option(binary()),
    behavior_filter :: gleam@option:option(binary()),
    output_level :: intent@runner:output_level()
}).
