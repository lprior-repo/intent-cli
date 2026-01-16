---
name: intent:plan-structure
description: Group contracts into epic/feature/task hierarchy with dependencies
allowed-tools: [Read, Write]
---

<objective>
Group KIRK contracts into proper epic → feature → task hierarchy with wave-based dependencies.

This is a PLANNING command. Do NOT implement or execute anything.
</objective>

<execution_context>
@intent/workflows/structure-planner.md
@intent/templates/structure.md
</execution_context>

<process>
1. Read .intent/CONTRACTS.cue
2. Group contracts by feature/domain
3. Create epic → feature → task hierarchy
4. Identify dependencies between tasks
5. Calculate parallel execution waves
6. Write .intent/STRUCTURE.md
7. Generate .intent/DEPENDENCIES.mermaid graph
8. Offer review: /intent:review-structure
</process>

<success_criteria>
- [ ] Contracts grouped semantically
- [ ] Epic/feature/task hierarchy created
- [ ] Dependencies identified
- [ ] Wave assignments calculated
- [ ] STRUCTURE.md created
- [ ] DEPENDENCIES.mermaid created
- [ ] User offered review gate
</success_criteria>

<stub>
**This command is not yet implemented.**

Expected behavior:
- Group contracts into epic/feature/task hierarchy
- Identify dependencies
- Calculate parallel execution waves
- Create .intent/STRUCTURE.md
- Offer review gate

Next: Implement structure planner workflow
</stub>
