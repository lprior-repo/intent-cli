---
name: intent:analyze
description: Apply mental lattice analysis to discover gaps
allowed-tools: [Read, Write, Task]
---

<objective>
Apply 5 mental lattice thinking models to discover edge cases, failure modes, and gaps in requirements.

This is a PLANNING command. Do NOT implement or execute anything.
</objective>

<execution_context>
@intent/workflows/analyze.md
@intent/agents/inversion-agent.md
@intent/agents/second-order-agent.md
@intent/agents/pre-mortem-agent.md
@intent/agents/checklist-agent.md
@intent/agents/circle-agent.md
</execution_context>

<process>
1. Read .intent/REQUIREMENTS.md
2. Spawn 5 parallel agents for mental lattices:
   - Inversion (what could fail?)
   - Second-Order (what are consequences?)
   - Pre-Mortem (how could this fail?)
   - Checklist (standard practices)
   - Circle of Competence (expertise needed)
3. Aggregate discoveries
4. Add new requirements to REQUIREMENTS.md
5. Write .intent/ANALYSIS.md
6. Offer review: /intent:review-analysis
</process>

<success_criteria>
- [ ] All 5 mental lattices applied
- [ ] 40+ new requirements discovered
- [ ] ANALYSIS.md created
- [ ] REQUIREMENTS.md updated
- [ ] User offered review gate
</success_criteria>

<stub>
**This command is not yet implemented.**

Expected behavior:
- Spawn 5 parallel mental lattice agents
- Discover edge cases and failure modes
- Add requirements to REQUIREMENTS.md
- Create .intent/ANALYSIS.md
- Offer review gate

Next: Implement analyze workflow and 5 mental lattice agents
</stub>
