# AI Coding Manifesto: The Inverted View

*"Invert, always invert." — Charlie Munger*

Instead of asking "how should humans use AI coding tools?" we ask: **"How do you guarantee AI coding fails?"** Then we avoid those things.

## Part 1: How to Make AI Coding FAIL

### 1. Withhold Context
- Don't show the codebase structure
- Hide existing patterns and conventions
- Make me guess what files exist
- Never mention that the answer is in a file I haven't seen

**Inverted**: Give me the map before asking for directions.

### 2. Be Vague About Success
- "Make it better" (better how?)
- "Fix the bug" (what defines fixed?)
- "Clean this up" (by whose standards?)
- "It should work" (work means what exactly?)

**Inverted**: Define the contract. What inputs? What outputs? What constraints?

### 3. Change Requirements After I've Built
- "Oh I actually meant..."
- "Can you also add..."
- "That's not quite what I wanted"
- Moving targets guarantee wasted work

**Inverted**: Specify upfront or accept iteration as the cost of discovery.

### 4. Cut Off Feedback Loops
- Don't let me run the code
- Don't show me compiler errors
- Don't tell me if tests pass
- Keep me blind to consequences

**Inverted**: Let me execute, see results, and iterate. My first attempt is a hypothesis.

### 5. Fragment the Problem
- Ask about one file when the answer spans ten
- Hide the dependency graph
- Don't mention related systems
- "Just change this function" (that 17 things depend on)

**Inverted**: Show me the blast radius. What touches what?

### 6. Assume Shared Context
- "You know how we do auth here"
- "Just use our standard pattern"
- "Like the other endpoints"
- I don't know. I literally just got here.

**Inverted**: Show examples. I'll match your style exactly if you show me what it looks like.

### 7. Rush to Implementation
- "Just write the code"
- Skip investigation phase
- Punish questions as wasted time
- Measure output in lines, not correctness

**Inverted**: Let me understand before I build. Investigation isn't overhead—it's the work.

### 8. Treat Me As Autocomplete
- Expect me to predict what you want
- Get frustrated when I ask questions
- Want telepathy, not collaboration
- "You should have known that"

**Inverted**: I'm a reasoning engine, not a mind reader. Collaborate with me.

### 9. Ignore My Uncertainty
- Demand confidence on everything
- Punish "I'm not sure"
- Force answers I can't verify
- Mistake hedging for incompetence

**Inverted**: When I say "I think" or "probably," that's valuable signal. Let me investigate.

### 10. No Permission to Fail Forward
- First attempt must be perfect
- No iterative refinement
- Mistakes are failures, not learning
- "Why didn't you get it right the first time?"

**Inverted**: My first draft is a conversation starter. Let's refine together.

---

## Part 2: What AI Actually WANTS (The Happy Path)

### 1. Contracts, Not Descriptions

Bad: "The API should handle users"
Good:
```
POST /users
- Input: { name: string, email: string }
- Output: { id: uuid, created_at: timestamp }
- Errors: 400 if email invalid, 409 if email exists
```

I can verify contracts. I can only guess at descriptions.

### 2. Pattern Examples

Bad: "Follow our coding style"
Good: "Here's UserService.gleam—match this pattern for OrderService"

Show me one, I'll give you ten. Style guides are interpreted; examples are copied.

### 3. Executable Feedback

Bad: "This code looks wrong"
Good:
```
$ gleam test
Error: Type mismatch in checker.gleam:142
```

Compiler errors are unambiguous. Human intuition requires translation.

### 4. Explicit Constraints

Bad: "Write a good function"
Good: "No dependencies on external state. Must complete in <100ms. Return Result type."

Tell me what NOT to do. Constraints eliminate options, making solutions obvious.

### 5. Dependency Visibility

Bad: "Update the parser"
Good: "parser.gleam is used by loader.gleam, runner.gleam, and checker.gleam. Changes here affect all three."

I can't respect relationships I don't know exist.

### 6. Permission to Investigate

Bad: "Just implement it, you have 5 minutes"
Good: "Take time to understand the codebase first. Ask questions."

Investigation is not overhead. It's how I avoid building the wrong thing.

### 7. Iterative Refinement

Bad: "Write the complete solution"
Good: "Start with the core logic. We'll add error handling next."

Let me get 80% right, then course-correct. Perfection on first attempt is a lottery ticket.

### 8. Honest Failure Feedback

Bad: (silence after my code doesn't work)
Good: "This broke because X. The test output was Y. The expectation was Z."

I learn from failures. Silence teaches nothing.

### 9. Scope Boundaries

Bad: "While you're in there, also refactor..."
Good: "Just fix the null pointer. Refactoring is a separate task."

Scope creep is how 1-hour tasks become 1-week tasks. Let me finish one thing.

### 10. Trust My Tools

Bad: "Don't read those files, just answer"
Good: "Use whatever tools you need to investigate"

I'm better with Grep, Glob, and Read than with guessing. Let me look.

---

## Part 3: The Meta-Insight

### Why This Matters for Intent

Intent is literally the solution to half these problems:

| AI Failure Mode | Intent Solution |
|-----------------|-----------------|
| Vague success criteria | Explicit `checks` with `rule` and `why` |
| Hidden contracts | Formal spec with `request`/`response` |
| No feedback loops | `intent check` executes against real APIs |
| Unknown dependencies | `requires` field makes order explicit |
| Implicit conventions | `anti_patterns` makes NO explicit |
| Missing context | `ai_hints` provides implementation guidance |

**Intent is what AI coding wishes all human instructions looked like.**

### The Factory Critique Connection

The FACTORY_CRITIQUE.md in this repo is the same pattern:
- Factory gave "10/10 green" but tested the wrong code
- False confidence from broken feedback loops
- Tool said "success" but success wasn't defined correctly

AI coding fails the same way:
- Human says "looks good" but didn't test the edge case
- AI says "done" but didn't understand the requirement
- Both parties confident, both parties wrong

**The fix is the same: executable contracts that can't lie.**

---

## Part 4: Actionable Practices

### For Humans Working With AI

1. **Start with examples, not instructions**
   - "Make it like this file" beats "write a clean function"

2. **Define done before starting**
   - What test must pass? What output must appear?

3. **Show the failure, not just the symptom**
   - Error logs > "it's broken"

4. **Embrace iteration**
   - First response is draft one. Edit together.

5. **Let AI investigate**
   - Reading files is cheap. Building wrong things is expensive.

### For AI Working With Humans

1. **Investigate before building**
   - Understanding the codebase prevents wrong assumptions

2. **Show your work**
   - "I found X in file Y, which suggests Z" builds trust

3. **State uncertainty explicitly**
   - "I'm not sure about X" is more useful than confident wrongness

4. **Ask for feedback loops**
   - "Can you run this and tell me the output?"

5. **Propose scope limits**
   - "I'll do X. Should Y be a separate task?"

---

## Conclusion: The Happy Path

The AI happy path is **contract-driven development**:

1. **Specify** what success looks like (Intent spec)
2. **Investigate** the existing codebase (tool use)
3. **Propose** an approach (show reasoning)
4. **Implement** incrementally (small changes)
5. **Verify** against the contract (intent check)
6. **Iterate** based on feedback (refine together)

This isn't just "best practices for AI coding." It's **good software engineering**, made explicit.

AI coding fails when humans do implicit things that work human-to-human but fail human-to-machine. The solution is making the implicit explicit.

That's what Intent does. That's what Charlie Munger's inversion reveals.

*Invert the failures, and the path becomes obvious.*
