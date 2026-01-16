---
name: intent:quality
description: Verify quality score on 5 dimensions (target: 90%+)
allowed-tools: [Read]
---

<objective>
Score planning quality on 5 dimensions and provide recommendations.

This is a VERIFICATION command. Do NOT implement or execute anything.
</objective>

<execution_context>
@intent/workflows/quality-analyzer.md
@intent/templates/quality.json
</execution_context>

<process>
1. Read .intent/REQUIREMENTS.md
2. Read .intent/ANALYSIS.md
3. Read .intent/CONTRACTS.cue
4. Read .intent/STRUCTURE.md
5. Score on 5 dimensions:
   - Completeness (all fields present)
   - Consistency (no contradictions)
   - Testability (all beads have tests)
   - Clarity (clear descriptions)
   - Security (vulnerabilities covered)
6. Write .intent/QUALITY.json
7. Display score and recommendations
</process>

<success_criteria>
- [ ] All 5 dimensions scored
- [ ] Overall score calculated
- [ ] Recommendations provided
- [ ] QUALITY.json created
- [ ] Target: 90%+ overall
</success_criteria>

<stub>
**This command is not yet implemented.**

Expected behavior:
- Score planning on 5 dimensions
- Provide detailed recommendations
- Create .intent/QUALITY.json
- Highlight gaps and issues

Next: Implement quality analyzer workflow
</stub>
