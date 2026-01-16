---
name: intent:generate-beads
description: Create beads in bd database from structure with all metadata
allowed-tools: [Read, Bash]
---

<objective>
Generate atomic beads in bd database with complete metadata from STRUCTURE.md.

This is a PLANNING command. Do NOT implement or execute anything.
</objective>

<execution_context>
@intent/workflows/bead-generator.md
@intent/templates/bead.md
</execution_context>

<process>
1. Read .intent/STRUCTURE.md
2. Read .intent/CONTRACTS.cue for metadata
3. For each task in structure:
   - Create bead with bd create
   - Set description, priority, type
   - Add preconditions, postconditions, invariants
   - Add edge cases and test cases
   - Set context_files and output_files
   - Add estimate and wave assignment
4. Set dependencies with bd dep add
5. Write .intent/QUALITY.json with quality score
6. Offer review: /intent:review-final
</process>

<success_criteria>
- [ ] All tasks converted to beads
- [ ] Epic/feature/task hierarchy created in bd
- [ ] All dependencies set
- [ ] Complete metadata in all beads
- [ ] QUALITY.json created
- [ ] User offered final review gate
</success_criteria>

<stub>
**This command is not yet implemented.**

Expected behavior:
- Create beads in bd database
- Set epic/feature/task hierarchy
- Add complete metadata to each bead
- Set dependencies
- Calculate quality score
- Offer final review gate

Next: Implement bead generator workflow
</stub>
