# PHASE 6: Automated Help Text Evaluation Script

Practical guide for implementing `evaluate-help-text.gleam` and integration with CI/CD.

---

## 1. Script Overview

### Purpose
Automate baseline assessment of help text quality across all 24 commands.

### Inputs
- Path to `src/intent.gleam` (command definitions)
- Path to `src/intent/cli_text_constants.gleam` (help text constants)

### Outputs
- JSON report: `evaluation_report.json`
- Markdown summary: `evaluation_summary.md`
- Per-command scores: `evaluation_by_command.json`

### Execution
```bash
gleam run -- evaluate-help-text \
  --src src/intent.gleam \
  --constants src/intent/cli_text_constants.gleam \
  --output evaluation_report.json
```

---

## 2. Automated Checks

### 2.1 Structural Clarity Checks

```gleam
// In automated_checks.gleam

pub fn check_sections_present(help_text: String) -> #(List(String), Int) {
  let required_sections = [
    "WHAT IT DOES",
    "WHY YOU'D USE IT",
    "WHEN TO USE IT",
    "USAGE EXAMPLES"
  ]

  let sections_found =
    list.filter(required_sections, fn(section) {
      string.contains(help_text, section)
    })

  let missing = list.length(required_sections) - list.length(sections_found)
  let score = list.length(sections_found) * 10 / 4  // 4 sections = 10 points

  #(missing_sections, score)
}

pub fn check_code_blocks(help_text: String) -> Int {
  let code_block_count =
    string.split(help_text, "```")
    |> list.length
    |> fn(x) { (x - 1) / 2 }  // ``` comes in pairs

  // Score: each code block = +2 points, max 8
  int.min(code_block_count * 2, 8)
}

pub fn check_structure_consistency(help_text: String) -> #(Int, List(String)) {
  let has_prerequisites = string.contains(help_text, "PREREQUISITES")
  let has_flags = string.contains(help_text, "FLAGS")
  let has_examples = string.contains(help_text, "USAGE EXAMPLES")
  let has_see_also = string.contains(help_text, "SEE ALSO")

  let missing_sections = []
    |> case has_prerequisites { False -> ["PREREQUISITES", ..acc], True -> acc }
    |> case has_flags { False -> ["FLAGS", ..acc], True -> acc }
    |> case has_examples { False -> ["USAGE EXAMPLES", ..acc], True -> acc }
    |> case has_see_also { False -> ["SEE ALSO", ..acc], True -> acc }

  let score = case list.length(missing_sections) {
    0 -> 10
    1 -> 8
    2 -> 6
    3 -> 4
    _ -> 2
  }

  #(score, missing_sections)
}

pub fn check_example_completeness(help_text: String) -> #(Int, Int) {
  let example_count =
    string.split(help_text, "intent ")
    |> list.length
    |> int.max(1)
    |> fn(x) { x - 1 }  // subtract 1 for the word "intent" itself

  let score = case example_count {
    0 -> 0
    1 -> 5
    2 to 3 -> 8
    _ -> 10  // 4+ examples = full score
  }

  #(score, example_count)
}

pub fn check_readability(help_text: String) -> Int {
  // Flesch-Kincaid Grade Level calculation
  let words = string.split(help_text, " ") |> list.length
  let sentences = string.split(help_text, ".") |> list.length
  let syllables = estimate_syllables(help_text)

  let fk_grade =
    (0.39 *. int.to_float(words) /. int.to_float(sentences)) +.
    (11.8 *. int.to_float(syllables) /. int.to_float(words)) -.
    15.59

  // Score: grade 8 or below = 10 points, each grade above = -1
  let grade_int = float.round(fk_grade) |> int.max(0)
  let score = int.max(0, 10 - (grade_int - 8))

  score
}

fn estimate_syllables(text: String) -> Int {
  // Simple heuristic: vowel groups = syllables
  let vowels = text |> string.lowercase |> string.split("")
  let vowel_chars = ["a", "e", "i", "o", "u"]
  list.filter(vowels, fn(c) { list.contains(vowel_chars, c) })
  |> list.length
}
```

### 2.2 Actionability Checks

```gleam
pub fn check_copy_paste_examples(help_text: String) -> #(Int, List(String)) {
  let examples =
    string.split(help_text, "```")
    |> list.filter(fn(block) { string.contains(block, "intent ") })

  let score = case list.length(examples) {
    0 -> 0
    1 -> 5
    2 to 3 -> 8
    _ -> 10
  }

  #(score, examples)
}

pub fn check_flag_documentation(help_text: String) -> #(Int, Int) {
  // Count --flag patterns
  let flag_pattern = "--[a-z-]+"
  let flags_mentioned =
    regex.scan(flag_pattern, help_text)
    |> set.from_list
    |> set.size

  // Score: each documented flag +1, max 8
  let score = int.min(flags_mentioned, 8)

  #(score, flags_mentioned)
}

pub fn check_error_scenarios(help_text: String) -> #(Int, Int) {
  // Look for error/issue documentation
  let error_patterns = [
    "COMMON ISSUES",
    "Error:",
    "Fix:",
    "FAILURE",
    "TROUBLESHOOT"
  ]

  let error_markers_found =
    list.filter(error_patterns, fn(pattern) {
      string.contains(help_text, pattern)
    })
    |> list.length

  let score = case error_markers_found {
    0 -> 0
    1 -> 2
    2 to 3 -> 5
    _ -> 7
  }

  #(score, error_markers_found)
}

pub fn check_output_format_documented(help_text: String) -> Int {
  case True {
    _ if string.contains(help_text, "OUTPUT") -> 5
    _ if string.contains(help_text, "Returns") -> 3
    _ if string.contains(help_text, "JSON") -> 2
    _ -> 0
  }
}
```

### 2.3 Training Suitability Checks

```gleam
pub fn check_mental_model(help_text: String) -> #(Int, Bool) {
  let has_model = string.contains(help_text, "MENTAL MODEL")

  let score = case has_model {
    True -> 10
    False -> 0
  }

  #(score, has_model)
}

pub fn check_workflow_integration(help_text: String) -> #(Int, List(String)) {
  // Look for related command references
  let related_commands =
    regex.scan("intent [a-z-]+", help_text)
    |> set.from_list

  let score = case set.size(related_commands) {
    0 -> 0
    1 to 2 -> 4
    3 to 5 -> 6
    _ -> 8
  }

  #(score, set.to_list(related_commands))
}

pub fn check_failure_modes(help_text: String) -> #(Int, List(String)) {
  // Extract failure scenario markers
  let failure_patterns = [
    "Error:",
    "fails",
    "failure",
    "cannot",
    "timeout"
  ]

  let failure_markers =
    list.filter(failure_patterns, fn(pattern) {
      case string.contains(help_text, pattern) {
        True -> True
        False -> False
      }
    })

  let score = case list.length(failure_markers) {
    0 -> 0
    1 -> 2
    2 to 3 -> 4
    _ -> 7
  }

  #(score, failure_markers)
}
```

### 2.4 Consistency Checks

```gleam
pub fn check_terminology_consistency(all_help_texts: List(#(String, String))) -> #(Int, Dict(String, List(String))) {
  // Build terminology map: term -> commands using it
  let term_map = dict.new()
    |> add_terminology_usage(all_help_texts, "spec")
    |> add_terminology_usage(all_help_texts, "specification")
    |> add_terminology_usage(all_help_texts, "feature")
    |> add_terminology_usage(all_help_texts, "behavior")
    |> add_terminology_usage(all_help_texts, "bead")
    |> add_terminology_usage(all_help_texts, "check")
    |> add_terminology_usage(all_help_texts, "validation")

  // Calculate consistency score
  let drift_count =
    dict.to_list(term_map)
    |> list.count(fn(pair) {
      let #(_term, commands) = pair
      list.length(commands) > 1  // Multiple variations = drift
    })

  let score = int.max(0, 30 - (drift_count * 5))

  #(score, term_map)
}

fn add_terminology_usage(
  map: Dict(String, List(String)),
  all_helps: List(#(String, String)),
  term: String,
) -> Dict(String, List(String)) {
  let using_commands =
    list.filter_map(all_helps, fn(pair) {
      let #(cmd, help) = pair
      case string.contains(help, term) {
        True -> Ok(cmd)
        False -> Error(Nil)
      }
    })

  dict.insert(map, term, using_commands)
}

pub fn check_header_consistency(all_help_texts: List(#(String, String))) -> Int {
  // Check that all use similar header patterns
  let header_patterns =
    list.map(all_help_texts, fn(pair) {
      let #(_cmd, help) = pair
      // Extract header lines (uppercase lines followed by content)
      help
      |> string.split("\n")
      |> list.filter(fn(line) { string.all_uppercase(line) })
    })

  // Count unique header patterns
  let unique_patterns =
    header_patterns
    |> set.from_list
    |> set.size

  case unique_patterns {
    1 -> 25  // Perfect consistency
    2 -> 20  // Minor variations acceptable
    3 to 5 -> 12  // Some inconsistency
    _ -> 5   // Major inconsistency
  }
}

pub fn check_tone_consistency(all_help_texts: List(#(String, String))) -> Int {
  // Check for imperative vs passive voice
  let tone_indicators =
    list.map(all_helps, fn(pair) {
      let #(_cmd, help) = pair
      let imperative_count = string.length(help) - string.length(
        string.replace(help, "execute|run|analyze|generate", "")
      )
      let passive_count = string.length(help) - string.length(
        string.replace(help, "is|are|be|been", "")
      )
      #(imperative_count, passive_count)
    })

  // Check if most commands use same voice
  let imperative_majority =
    list.count(tone_indicators, fn(pair) {
      let #(imp, pass) = pair
      imp > pass
    }) > list.length(tone_indicators) / 2

  case imperative_majority {
    True -> 25
    False -> 15
  }
}
```

### 2.5 Coverage Checks

```gleam
pub fn check_flag_coverage(help_text: String, command_name: String) -> #(Int, Int, Int) {
  // Map command to expected flags from intent.gleam
  let expected_flags = get_expected_flags(command_name)

  let documented_flags =
    regex.scan("--[a-z-]+", help_text)
    |> set.from_list
    |> set.size

  let coverage_ratio =
    int.to_float(documented_flags) /. int.to_float(list.length(expected_flags))

  let score = float.round(coverage_ratio *. 30.0)

  #(score, documented_flags, list.length(expected_flags))
}

pub fn check_scenario_coverage(help_text: String) -> #(Int, Int) {
  // Count example scenarios
  let example_count =
    string.split(help_text, "```")
    |> list.filter(fn(block) { string.contains(block, "intent ") })
    |> list.length

  let score = case example_count {
    0 -> 0
    1 -> 8
    2 -> 16
    3 to 5 -> 24
    _ -> 30
  }

  #(score, example_count)
}

pub fn check_integration_coverage(help_text: String, all_commands: List(String)) -> #(Int, List(String)) {
  // Find references to other commands
  let referenced =
    list.filter(all_commands, fn(cmd) {
      string.contains(help_text, "intent " <> cmd)
    })

  let score = case list.length(referenced) {
    0 -> 0
    1 to 2 -> 5
    3 to 5 -> 10
    _ -> 15
  }

  #(score, referenced)
}
```

---

## 3. JSON Output Format

### 3.1 Per-Command Report

```json
{
  "command": "check",
  "command_description": "Execute spec tests against target URL...",
  "scores": {
    "ai_friendliness": {
      "structural_clarity": 36,
      "actionability": 29,
      "training_suitability": 23,
      "total": 88
    },
    "usability": {
      "clarity": 28,
      "completeness": 29,
      "actionability": 19,
      "examples": 18,
      "total": 92
    },
    "consistency": {
      "terminology": 30,
      "formatting": 25,
      "tone": 20,
      "structure": 10,
      "total": 85
    },
    "coverage": {
      "flag_coverage": 10,
      "scenario_coverage": 28,
      "integration_coverage": 12,
      "edge_case_coverage": 14,
      "total": 82
    },
    "completeness": {
      "required_categories": 8,
      "optional_categories": 5,
      "percentage": 100
    }
  },
  "overall_score": 89.0,
  "tier": "★★★★☆",
  "assessment": "Good",
  "issues": [
    "Mental model not explicit (only implicit)",
    "Output format section missing",
    "SSRF protection unexplained"
  ],
  "recommendations": [
    "Add explicit MENTAL MODEL section",
    "Add OUTPUT FORMAT section showing JSON schema",
    "Add EDGE CASES subsection"
  ]
}
```

### 3.2 Summary Report

```json
{
  "evaluation_date": "2026-01-18",
  "total_commands": 24,
  "statistics": {
    "average_ai_friendliness": 76.5,
    "average_usability": 78.3,
    "average_consistency": 82.1,
    "average_coverage": 74.2,
    "average_completeness": 92.5,
    "average_overall": 78.3
  },
  "tier_distribution": {
    "excellent": 3,
    "good": 8,
    "fair": 9,
    "poor": 3,
    "critical": 1
  },
  "commands_by_tier": {
    "excellent": ["check", "validate", "plan"],
    "good": ["lint", "analyze", "improve", "doctor", "interview", "beads", "plan-approve", "effects"],
    "fair": ["show", "export", "bead-status", "history", "diff", "sessions", "quality", "coverage", "gaps"],
    "poor": ["invert", "ears", "parse"],
    "critical": ["beads-regenerate"]
  },
  "top_performers": [
    {"command": "check", "score": 89.0},
    {"command": "validate", "score": 87.5},
    {"command": "plan", "score": 86.2}
  ],
  "bottom_performers": [
    {"command": "beads-regenerate", "score": 42.1},
    {"command": "parse", "score": 48.3},
    {"command": "ears", "score": 51.7}
  ],
  "common_issues": [
    "Mental models not explicitly documented (15 commands)",
    "Edge cases not documented (12 commands)",
    "Output format section missing (14 commands)",
    "Examples insufficient (11 commands)"
  ],
  "batch_recommendations": [
    "Add MENTAL MODEL section to 15 commands",
    "Add edge case documentation to 12 commands",
    "Create output format specification template",
    "Expand examples from 2 to 4+ per command"
  ]
}
```

---

## 4. Integration with Glint

### 4.1 New Command in Intent CLI

```gleam
// In src/intent.gleam

fn evaluate_help_text_command() -> glint.Command(Nil) {
  glint.command(fn(input: glint.CommandInput) {
    let output_file =
      flag.get_string(input.flags, "output")
      |> result.unwrap("evaluation_report.json")

    case automated_checks.evaluate_all_commands() {
      Ok(metrics) -> {
        let report_json = generate_json_report(metrics)
        io.println(json.to_string(report_json))
        case file.write(output_file, json.to_string(report_json)) {
          Ok(Nil) -> Nil
          Error(_) -> io.println_error("Failed to write report")
        }
      }
      Error(msg) -> io.println_error(msg)
    }
  })
  |> glint.description("Generate help text quality evaluation report")
  |> glint.flag("output", flag.string())
}

// Add to command tree:
|> glint.add(at: ["eval-help-text"], do: evaluate_help_text_command())
```

### 4.2 Example Usage

```bash
# Generate full report
gleam run -- eval-help-text --output evaluation_report.json

# View summary
cat evaluation_report.json | jq '.statistics'

# Check specific command
cat evaluation_report.json | jq '.results[] | select(.command == "check")'

# Find all critical commands
cat evaluation_report.json | jq '.results[] | select(.tier == "★☆☆☆☆")'
```

---

## 5. CI/CD Integration

### 5.1 GitHub Actions Workflow

```yaml
name: Help Text Quality Check

on: [pull_request, push]

jobs:
  evaluate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Gleam
        uses: gleam-lang/setup-gleam@v1.2.0

      - name: Build project
        run: gleam build

      - name: Evaluate help text
        run: |
          gleam run -- eval-help-text \
            --output evaluation_report.json

      - name: Check for critical issues
        run: |
          CRITICAL=$(jq '.tier_distribution.critical' evaluation_report.json)
          if [ "$CRITICAL" -gt 0 ]; then
            echo "❌ Critical help text issues found"
            jq '.commands_by_tier.critical' evaluation_report.json
            exit 1
          fi

      - name: Comment on PR
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const report = JSON.parse(fs.readFileSync('evaluation_report.json', 'utf8'));

            const comment = `
## Help Text Quality Report

**Overall Score**: ${report.statistics.average_overall.toFixed(1)}/100

**Tier Distribution**:
- Excellent (90+): ${report.tier_distribution.excellent}
- Good (75-89): ${report.tier_distribution.good}
- Fair (60-74): ${report.tier_distribution.fair}
- Poor (45-59): ${report.tier_distribution.poor}
- Critical (<45): ${report.tier_distribution.critical}

**Common Issues**:
${report.common_issues.map(issue => `- ${issue}`).join('\n')}

[View full report](https://github.com/.../artifacts)
            `;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: comment
            });

      - name: Upload report
        uses: actions/upload-artifact@v3
        with:
          name: help-text-evaluation
          path: evaluation_report.json
```

### 5.2 Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

gleam run -- eval-help-text --output /tmp/eval.json

CRITICAL=$(jq '.tier_distribution.critical' /tmp/eval.json)

if [ "$CRITICAL" -gt 0 ]; then
  echo "❌ Pre-commit check: Critical help text issues"
  jq '.commands_by_tier.critical' /tmp/eval.json
  exit 1
fi

exit 0
```

---

## 6. Extending the Framework

### 6.1 Adding New Metrics

To add a new metric:

1. Implement check function in `automated_checks.gleam`
2. Add to scoring calculation in `evaluate_command()`
3. Update JSON schema
4. Add to documentation

```gleam
// Example: Add accessibility score

pub fn check_accessibility(help_text: String) -> Int {
  let line_length_ok =
    help_text
    |> string.split("\n")
    |> list.all(fn(line) { string.length(line) < 80 })

  let contrast_ok = string.contains(help_text, "```")  // Code blocks for readability

  case #(line_length_ok, contrast_ok) {
    #(True, True) -> 10
    #(True, False) | #(False, True) -> 5
    #(False, False) -> 0
  }
}
```

### 6.2 Custom Rubrics

Allow per-command custom rubrics:

```json
{
  "command": "custom-cmd",
  "custom_rubric": {
    "domain_specific_concept_coverage": 20,
    "integration_with_external_tools": 15,
    "migration_guide_quality": 10
  },
  "base_score": 85.0,
  "adjusted_score": 78.5
}
```

---

## 7. Manual Review Checklist

For items the script cannot automate:

```
☐ Tone consistency review (read all 24 help texts)
☐ Grammar/spelling check (automated tools like `gramma` can assist)
☐ Technical accuracy (verify examples against actual commands)
☐ Appropriateness of conceptual explanations
☐ Cultural/localization considerations
☐ Links and cross-references validity
```

---

**Script Status**: Specification Complete, Ready for Implementation
**Estimated Development Time**: 6-8 hours
**Testing Effort**: 2-3 hours
**Validation**: LLM re-evaluation on sample commands

