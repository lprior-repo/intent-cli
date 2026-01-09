-record(improvement_context, {
    quality_report :: intent@quality_analyzer:quality_report(),
    lint_result :: intent@spec_linter:lint_result(),
    spec :: intent@types:spec()
}).
