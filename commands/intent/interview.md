---
name: intent:interview
description: Systematic EARS interview to gather requirements
allowed-tools: [Read, Write, AskUserQuestion, Grep]
---

<objective>
Conduct systematic interview using 6 EARS patterns to gather complete requirements.

This is a PLANNING command. Do NOT implement or execute anything.
</objective>

<execution_context>
@intent/workflows/interview.md
@intent/templates/requirements.md
</execution_context>

<process>
1. Read project context if available
2. Ask questions for each EARS pattern:
   - Ubiquitous (always true)
   - Event-Driven (when X, do Y)
   - State-Driven (while X, do Y)
   - Optional (where X, do Y)
   - Unwanted (if X, shall NOT do Y)
   - Complex (combinations)
3. Format answers in EARS notation
4. Write to .intent/REQUIREMENTS.md
5. Offer review: /intent:review-requirements
</process>

<success_criteria>
- [ ] All 6 EARS patterns covered
- [ ] 18-24 requirements captured
- [ ] REQUIREMENTS.md created
- [ ] User offered review gate
</success_criteria>

<stub>
**This command is not yet implemented.**

Expected behavior:
- Systematic questioning across 6 EARS patterns
- Format requirements in EARS notation
- Create .intent/REQUIREMENTS.md
- Offer review gate

Next: Implement interview workflow in intent/workflows/interview.md
</stub>
