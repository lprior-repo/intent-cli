---
name: intent:contract
description: Transform EARS requirements into CUE-validated KIRK contracts
allowed-tools: [Read, Write, Bash]
---

<objective>
Transform EARS requirements into formal KIRK contracts with preconditions, postconditions, and invariants.

This is a PLANNING command. Do NOT implement or execute anything.
</objective>

<execution_context>
@intent/workflows/contract.md
@intent/templates/contract.cue
</execution_context>

<process>
1. Read .intent/REQUIREMENTS.md
2. Transform each requirement into KIRK contract:
   - Preconditions (what must be true before)
   - Postconditions (what must be true after)
   - Invariants (what must always be true)
   - Edge cases
   - Test cases (given/when/then)
3. Write .intent/CONTRACTS.cue
4. Validate with CUE: cue vet schema/contract.cue .intent/CONTRACTS.cue
5. Offer review: /intent:review-contracts
</process>

<success_criteria>
- [ ] All requirements transformed
- [ ] CONTRACTS.cue created
- [ ] CUE validation passed
- [ ] User offered review gate
</success_criteria>

<stub>
**This command is not yet implemented.**

Expected behavior:
- Transform requirements into KIRK contracts
- Validate with CUE schemas
- Create .intent/CONTRACTS.cue
- Offer review gate

Next: Implement contract workflow in intent/workflows/contract.md
</stub>
