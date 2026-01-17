# AI Protocol Implementation Examples

Practical code examples for implementing AI agents using the Intent Interview Protocol.

---

## Table of Contents

1. [Python Implementation](#python-implementation)
2. [Node.js Implementation](#nodejs-implementation)
3. [Bash Script Implementation](#bash-script-implementation)
4. [Rust Implementation](#rust-implementation)
5. [Common Patterns](#common-patterns)

---

## Python Implementation

### Minimal Example (< 50 lines)

```python
#!/usr/bin/env python3
"""Minimal AI interview agent for Intent CLI"""

import subprocess
import json
import sys

def run_cli(*args):
    """Run intent CLI and return parsed JSON"""
    result = subprocess.run(
        ["intent", "interview", "--cue"] + list(args),
        capture_output=True,
        text=True
    )
    return json.loads(result.stdout)

def generate_answer(question):
    """Generate answer based on question (implement your logic here)"""
    # Simple example: use the pattern hint to format answer
    pattern = question["pattern"]
    if pattern == "ubiquitous":
        return "THE SYSTEM SHALL perform the required behavior"
    elif pattern == "event_driven":
        return "WHEN event occurs THE SYSTEM SHALL respond appropriately"
    return "Answer based on context"

def main():
    # Start interview
    response = run_cli("--profile", "api")
    session_id = response["session"]["id"]

    while response["action"] == "ask_question":
        question = response["question"]
        progress = response["progress"]

        print(f"Q{progress['current_step']}/{progress['total_steps']}: {question['text']}")

        # Generate answer
        answer = generate_answer(question)
        print(f"A: {answer}\n")

        # Submit answer
        response = run_cli("--session", session_id, "--answer", answer)

        # Handle errors
        if response["action"] == "validation_error":
            print(f"Error: {response['error']['message']}")
            if not response["error"]["retry_allowed"]:
                sys.exit(1)

    # Interview complete
    if response["action"] == "interview_complete":
        print(f"✓ Complete! Spec: {response['output']['spec_path']}")

if __name__ == "__main__":
    main()
```

**Run:**
```bash
python3 minimal_agent.py
```

---

### Production Example (with error handling, logging, retry)

```python
#!/usr/bin/env python3
"""Production-grade AI interview agent"""

import subprocess
import json
import sys
import logging
import time
from typing import Dict, Optional
import shlex

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class IntentAgent:
    """AI agent for conducting Intent interviews"""

    def __init__(self, profile: str, max_retries: int = 3):
        self.profile = profile
        self.max_retries = max_retries
        self.session_id: Optional[str] = None

    def run_cli(self, *args) -> Dict:
        """Execute CLI command and parse JSON response"""
        cmd = ["intent", "interview", "--cue"] + list(args)
        logger.debug(f"Executing: {' '.join(cmd)}")

        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode != 0:
                logger.error(f"CLI returned non-zero: {result.returncode}")
                logger.error(f"stderr: {result.stderr}")

            return json.loads(result.stdout)

        except subprocess.TimeoutExpired:
            logger.error("CLI command timed out")
            raise
        except json.JSONDecodeError as e:
            logger.error(f"Failed to parse JSON: {e}")
            logger.error(f"stdout: {result.stdout}")
            raise

    def start_interview(self) -> Dict:
        """Start a new interview session"""
        logger.info(f"Starting interview for profile: {self.profile}")
        response = self.run_cli("--profile", self.profile)

        if response["action"] != "ask_question":
            raise ValueError(f"Unexpected action: {response['action']}")

        self.session_id = response["session"]["id"]
        logger.info(f"Session started: {self.session_id}")

        return response

    def submit_answer(self, answer: str) -> Dict:
        """Submit answer to current question"""
        if not self.session_id:
            raise ValueError("No active session")

        # Properly escape answer for shell
        escaped_answer = shlex.quote(answer)

        logger.debug(f"Submitting answer (length: {len(answer)})")
        return self.run_cli("--session", self.session_id, "--answer", escaped_answer)

    def generate_answer(self, question: Dict) -> str:
        """Generate answer based on question context"""
        # Extract key information
        text = question["text"]
        pattern = question["pattern"]
        hint = question["hint"]
        examples = question["examples"]
        context = question["context"]
        example = question["example"]

        logger.info(f"Generating answer for: {text[:50]}...")
        logger.debug(f"Pattern: {pattern}, Hint: {hint}")

        # TODO: Implement your AI logic here
        # This is where you'd call your LLM, rules engine, or other logic

        # Simple example implementation:
        if pattern == "ubiquitous":
            return f"THE SYSTEM SHALL {self._extract_behavior(text)}"
        elif pattern == "event_driven":
            return f"WHEN {self._extract_trigger(text)} THE SYSTEM SHALL respond"
        elif pattern == "state_driven":
            return f"WHILE active THE SYSTEM SHALL {self._extract_behavior(text)}"
        elif pattern == "optional":
            return f"WHERE configured THE SYSTEM SHALL {self._extract_behavior(text)}"
        elif pattern == "unwanted":
            return f"IF error occurs THE SYSTEM SHALL NOT {self._extract_behavior(text)}"
        else:
            return example if example else "Answer based on context"

    def _extract_behavior(self, text: str) -> str:
        """Extract behavior from question text (simplified)"""
        # TODO: Implement proper NLP extraction
        return "perform the required behavior"

    def _extract_trigger(self, text: str) -> str:
        """Extract trigger from question text (simplified)"""
        # TODO: Implement proper NLP extraction
        return "event occurs"

    def handle_validation_error(self, error: Dict) -> bool:
        """Handle validation error, return True if should retry"""
        logger.warning(f"Validation error: {error['message']}")
        logger.info(f"Suggestion: {error['suggestion']}")

        if not error["retry_allowed"]:
            logger.error("Retry not allowed, aborting")
            return False

        # Log context if available
        if "context" in error:
            logger.debug(f"Error context: {error['context']}")

        return True

    def run_interview(self) -> str:
        """Run complete interview, return spec path"""
        # Start interview
        response = self.start_interview()

        retries = 0

        while True:
            action = response["action"]

            if action == "ask_question":
                question = response["question"]
                progress = response["progress"]

                # Log progress
                logger.info(
                    f"Question {progress['current_step']}/{progress['total_steps']} "
                    f"({progress['percent_complete']}%) - Round {progress['round']}"
                )
                logger.info(f"Q: {question['text']}")

                # Generate answer
                answer = self.generate_answer(question)
                logger.info(f"A: {answer[:100]}...")

                # Submit answer
                response = self.submit_answer(answer)
                retries = 0  # Reset retry counter on success

            elif action == "validation_error":
                error = response["error"]

                if not self.handle_validation_error(error):
                    raise ValueError(f"Validation failed: {error['message']}")

                # Retry with improved answer
                retries += 1
                if retries >= self.max_retries:
                    raise ValueError(f"Max retries exceeded: {error['message']}")

                # Generate improved answer (in production, use error context)
                improved_answer = self.generate_answer(question) + " (with more detail)"
                response = self.submit_answer(improved_answer)

            elif action == "interview_complete":
                output = response["output"]
                statistics = response["statistics"]

                logger.info("=" * 60)
                logger.info("Interview Complete!")
                logger.info(f"Spec: {output['spec_path']}")
                logger.info(f"Behaviors: {output['behaviors_count']}")
                logger.info(f"Rounds: {statistics['rounds_completed']}")
                logger.info(f"Confidence: {statistics['average_confidence']:.2f}")
                logger.info("=" * 60)

                return output["spec_path"]

            else:
                raise ValueError(f"Unknown action: {action}")

    def validate_spec(self, spec_path: str) -> bool:
        """Validate generated spec"""
        logger.info(f"Validating spec: {spec_path}")

        result = subprocess.run(
            ["intent", "validate", spec_path],
            capture_output=True,
            text=True
        )

        if result.returncode == 0:
            logger.info("✓ Spec validation passed")
            return True
        else:
            logger.error("✗ Spec validation failed")
            logger.error(result.stderr)
            return False


def main():
    """Main entry point"""
    import argparse

    parser = argparse.ArgumentParser(description="AI Interview Agent")
    parser.add_argument(
        "--profile",
        choices=["api", "cli", "event", "data", "workflow", "ui"],
        default="api",
        help="System profile to interview"
    )
    parser.add_argument(
        "--validate",
        action="store_true",
        help="Validate generated spec"
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug logging"
    )

    args = parser.parse_args()

    if args.debug:
        logger.setLevel(logging.DEBUG)

    try:
        # Create agent
        agent = IntentAgent(profile=args.profile)

        # Run interview
        spec_path = agent.run_interview()

        # Optionally validate
        if args.validate:
            if not agent.validate_spec(spec_path):
                sys.exit(1)

        print(f"\n✓ Success! Spec: {spec_path}")

    except Exception as e:
        logger.exception(f"Interview failed: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
```

**Run:**
```bash
# Basic
python3 production_agent.py --profile api

# With validation
python3 production_agent.py --profile cli --validate

# With debug logging
python3 production_agent.py --profile event --debug
```

---

## Node.js Implementation

### Minimal Example

```javascript
#!/usr/bin/env node
/**
 * Minimal AI interview agent for Intent CLI (Node.js)
 */

const { execSync } = require('child_process');

function runCli(...args) {
  const cmd = ['intent', 'interview', '--cue', ...args].join(' ');
  const output = execSync(cmd, { encoding: 'utf-8' });
  return JSON.parse(output);
}

function generateAnswer(question) {
  // Simple answer generation based on pattern
  const pattern = question.pattern;

  if (pattern === 'ubiquitous') {
    return 'THE SYSTEM SHALL perform the required behavior';
  } else if (pattern === 'event_driven') {
    return 'WHEN event occurs THE SYSTEM SHALL respond appropriately';
  }

  return 'Answer based on context';
}

async function main() {
  // Start interview
  let response = runCli('--profile', 'api');
  const sessionId = response.session.id;

  while (response.action === 'ask_question') {
    const question = response.question;
    const progress = response.progress;

    console.log(`Q${progress.current_step}/${progress.total_steps}: ${question.text}`);

    // Generate answer
    const answer = generateAnswer(question);
    console.log(`A: ${answer}\n`);

    // Submit answer (properly escape)
    const escapedAnswer = answer.replace(/"/g, '\\"');
    response = runCli('--session', sessionId, '--answer', escapedAnswer);

    // Handle errors
    if (response.action === 'validation_error') {
      console.error(`Error: ${response.error.message}`);
      if (!response.error.retry_allowed) {
        process.exit(1);
      }
    }
  }

  // Complete
  if (response.action === 'interview_complete') {
    console.log(`✓ Complete! Spec: ${response.output.spec_path}`);
  }
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});
```

**Run:**
```bash
node minimal_agent.js
```

---

## Bash Script Implementation

### Shell Script Example

```bash
#!/bin/bash
# Minimal AI interview agent (Bash)

set -euo pipefail

PROFILE="${1:-api}"
SESSION_ID=""

# Run CLI and parse JSON (requires jq)
run_cli() {
    intent interview --cue "$@"
}

# Extract field from JSON
extract() {
    echo "$1" | jq -r "$2"
}

# Generate answer (very simple logic)
generate_answer() {
    local question_text="$1"
    local pattern="$2"

    case "$pattern" in
        ubiquitous)
            echo "THE SYSTEM SHALL perform the required behavior"
            ;;
        event_driven)
            echo "WHEN event occurs THE SYSTEM SHALL respond"
            ;;
        *)
            echo "Answer based on context"
            ;;
    esac
}

# Main interview loop
main() {
    echo "Starting interview for profile: $PROFILE"

    # Start interview
    response=$(run_cli --profile "$PROFILE")
    action=$(extract "$response" '.action')

    if [[ "$action" != "ask_question" ]]; then
        echo "Error: unexpected action: $action" >&2
        exit 1
    fi

    SESSION_ID=$(extract "$response" '.session.id')
    echo "Session: $SESSION_ID"

    # Question loop
    while [[ "$action" == "ask_question" ]]; do
        question_text=$(extract "$response" '.question.text')
        pattern=$(extract "$response" '.question.pattern')
        current_step=$(extract "$response" '.progress.current_step')
        total_steps=$(extract "$response" '.progress.total_steps')

        echo ""
        echo "Q$current_step/$total_steps: $question_text"

        # Generate answer
        answer=$(generate_answer "$question_text" "$pattern")
        echo "A: $answer"

        # Submit answer
        response=$(run_cli --session "$SESSION_ID" --answer "$answer")
        action=$(extract "$response" '.action')

        # Handle validation errors
        if [[ "$action" == "validation_error" ]]; then
            error_msg=$(extract "$response" '.error.message')
            echo "Error: $error_msg" >&2

            retry_allowed=$(extract "$response" '.error.retry_allowed')
            if [[ "$retry_allowed" != "true" ]]; then
                exit 1
            fi
        fi
    done

    # Complete
    if [[ "$action" == "interview_complete" ]]; then
        spec_path=$(extract "$response" '.output.spec_path')
        echo ""
        echo "✓ Complete! Spec: $spec_path"
    fi
}

# Check dependencies
if ! command -v jq &> /dev/null; then
    echo "Error: jq is required but not installed" >&2
    exit 1
fi

if ! command -v intent &> /dev/null; then
    echo "Error: intent CLI is required but not found in PATH" >&2
    exit 1
fi

main
```

**Run:**
```bash
chmod +x interview_agent.sh
./interview_agent.sh api
```

---

## Rust Implementation

### Minimal Example

```rust
//! Minimal AI interview agent for Intent CLI (Rust)

use serde::{Deserialize, Serialize};
use std::process::{Command, Stdio};
use anyhow::{Context, Result};

#[derive(Debug, Deserialize)]
#[serde(tag = "action")]
enum Response {
    #[serde(rename = "ask_question")]
    AskQuestion {
        question: Question,
        progress: Progress,
        session: Session,
    },
    #[serde(rename = "interview_complete")]
    InterviewComplete {
        output: Output,
        session: Session,
        statistics: Statistics,
    },
    #[serde(rename = "validation_error")]
    ValidationError {
        error: Error,
        session: Option<Session>,
    },
}

#[derive(Debug, Deserialize)]
struct Question {
    text: String,
    pattern: String,
    hint: String,
    examples: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct Progress {
    current_step: u32,
    total_steps: u32,
    percent_complete: u32,
}

#[derive(Debug, Deserialize)]
struct Session {
    id: String,
    profile: String,
}

#[derive(Debug, Deserialize)]
struct Output {
    spec_path: String,
    behaviors_count: u32,
}

#[derive(Debug, Deserialize)]
struct Statistics {
    rounds_completed: u32,
    average_confidence: f64,
}

#[derive(Debug, Deserialize)]
struct Error {
    code: String,
    message: String,
    suggestion: String,
    retry_allowed: bool,
}

fn run_cli(args: &[&str]) -> Result<Response> {
    let mut cmd = Command::new("intent");
    cmd.args(&["interview", "--cue"]);
    cmd.args(args);

    let output = cmd
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context("Failed to execute intent CLI")?;

    let stdout = String::from_utf8(output.stdout)
        .context("Invalid UTF-8 in stdout")?;

    serde_json::from_str(&stdout)
        .context("Failed to parse JSON response")
}

fn generate_answer(question: &Question) -> String {
    match question.pattern.as_str() {
        "ubiquitous" => "THE SYSTEM SHALL perform the required behavior".to_string(),
        "event_driven" => "WHEN event occurs THE SYSTEM SHALL respond".to_string(),
        _ => "Answer based on context".to_string(),
    }
}

fn main() -> Result<()> {
    // Start interview
    let mut response = run_cli(&["--profile", "api"])?;

    let session_id = match &response {
        Response::AskQuestion { session, .. } => session.id.clone(),
        _ => anyhow::bail!("Expected ask_question response"),
    };

    // Question loop
    loop {
        match response {
            Response::AskQuestion { question, progress, .. } => {
                println!(
                    "Q{}/{}: {}",
                    progress.current_step,
                    progress.total_steps,
                    question.text
                );

                let answer = generate_answer(&question);
                println!("A: {}\n", answer);

                response = run_cli(&["--session", &session_id, "--answer", &answer])?;
            }

            Response::InterviewComplete { output, .. } => {
                println!("✓ Complete! Spec: {}", output.spec_path);
                break;
            }

            Response::ValidationError { error, .. } => {
                eprintln!("Error: {}", error.message);
                if !error.retry_allowed {
                    anyhow::bail!("Validation failed");
                }
            }
        }
    }

    Ok(())
}
```

**Cargo.toml:**
```toml
[package]
name = "intent-agent"
version = "0.1.0"
edition = "2021"

[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
anyhow = "1.0"
```

**Run:**
```bash
cargo build --release
./target/release/intent-agent
```

---

## Common Patterns

### Pattern 1: Progress Tracking

```python
def show_progress(progress: Dict):
    """Display progress bar"""
    current = progress["current_step"]
    total = progress["total_steps"]
    percent = progress["percent_complete"]

    bar_length = 50
    filled = int(bar_length * percent / 100)
    bar = "=" * filled + " " * (bar_length - filled)

    print(f"[{bar}] {percent}% ({current}/{total})")
```

### Pattern 2: Retry Logic

```python
def submit_with_retry(agent, answer, max_retries=3):
    """Submit answer with exponential backoff retry"""
    for attempt in range(max_retries):
        try:
            response = agent.submit_answer(answer)

            if response["action"] == "validation_error":
                error = response["error"]
                if not error["retry_allowed"]:
                    raise ValueError(error["message"])

                # Improve answer and retry
                answer = improve_answer(answer, error)
                time.sleep(2 ** attempt)  # Exponential backoff
                continue

            return response

        except subprocess.TimeoutExpired:
            if attempt < max_retries - 1:
                time.sleep(2 ** attempt)
                continue
            raise

    raise ValueError("Max retries exceeded")
```

### Pattern 3: Answer Caching

```python
def cached_answer_generator(question_cache_file="answers.json"):
    """Generate answers with caching"""
    try:
        with open(question_cache_file) as f:
            cache = json.load(f)
    except FileNotFoundError:
        cache = {}

    def generate(question: Dict) -> str:
        question_id = question["id"]

        if question_id in cache:
            return cache[question_id]

        answer = generate_answer(question)  # Your generation logic
        cache[question_id] = answer

        with open(question_cache_file, "w") as f:
            json.dump(cache, f, indent=2)

        return answer

    return generate
```

### Pattern 4: Session Resume

```python
def resume_or_start(profile: str, session_file=".last_session") -> Tuple[str, Dict]:
    """Resume last session or start new"""
    try:
        with open(session_file) as f:
            session_id = f.read().strip()

        # Try to resume
        response = run_cli("--session", session_id)

        if response["action"] == "ask_question":
            print(f"Resuming session {session_id}")
            return session_id, response

    except (FileNotFoundError, subprocess.CalledProcessError):
        pass

    # Start new session
    response = run_cli("--profile", profile)
    session_id = response["session"]["id"]

    with open(session_file, "w") as f:
        f.write(session_id)

    print(f"Started new session {session_id}")
    return session_id, response
```

### Pattern 5: Parallel Interviews

```python
import concurrent.futures

def run_parallel_interviews(profiles: List[str]):
    """Run multiple interviews in parallel"""
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(profiles)) as executor:
        futures = {
            executor.submit(run_interview, profile): profile
            for profile in profiles
        }

        for future in concurrent.futures.as_completed(futures):
            profile = futures[future]
            try:
                spec_path = future.result()
                print(f"✓ {profile}: {spec_path}")
            except Exception as e:
                print(f"✗ {profile}: {e}")

# Usage
run_parallel_interviews(["api", "cli", "event"])
```

---

## Testing Patterns

### Pattern: Mock CLI for Testing

```python
import unittest
from unittest.mock import patch, MagicMock

class TestAgent(unittest.TestCase):
    @patch('subprocess.run')
    def test_complete_interview(self, mock_run):
        """Test full interview flow with mocked CLI"""
        # Mock responses
        mock_run.side_effect = [
            # First response: question
            MagicMock(
                stdout=json.dumps({
                    "action": "ask_question",
                    "question": {"text": "Q1", "pattern": "ubiquitous"},
                    "session": {"id": "test-123"}
                }),
                returncode=0
            ),
            # Second response: completion
            MagicMock(
                stdout=json.dumps({
                    "action": "interview_complete",
                    "output": {"spec_path": "test.cue"}
                }),
                returncode=0
            )
        ]

        agent = IntentAgent("api")
        spec_path = agent.run_interview()

        self.assertEqual(spec_path, "test.cue")
        self.assertEqual(mock_run.call_count, 2)
```

---

## Advanced Patterns

### Pattern: LLM Integration (OpenAI)

```python
import openai

class LLMAgent(IntentAgent):
    """AI agent using OpenAI for answer generation"""

    def __init__(self, profile: str, api_key: str):
        super().__init__(profile)
        openai.api_key = api_key

    def generate_answer(self, question: Dict) -> str:
        """Generate answer using GPT-4"""
        prompt = self._build_prompt(question)

        response = openai.ChatCompletion.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are a requirements engineer answering specification questions."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.7,
            max_tokens=500
        )

        return response.choices[0].message.content.strip()

    def _build_prompt(self, question: Dict) -> str:
        """Build prompt for LLM"""
        return f"""
Question: {question['text']}

Context: {question['context']}

Example: {question['example']}

Format hint: {question['hint']}

Example answers:
{chr(10).join(f'- {ex}' for ex in question['examples'])}

Please provide a detailed answer following the format hint.
        """.strip()
```

---

**End of Examples**

See [AI_PROTOCOL.md](./AI_PROTOCOL.md) for full protocol specification.
