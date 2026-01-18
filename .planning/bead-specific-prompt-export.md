# Bead-Specific Prompt Export Implementation Plan

## STEP 1: RESEARCH - Complete

### Findings:

1. **Bead-to-Prompt Pipeline** (from CLAUDE.md):
   - Command: `intent prompt --bead <id> [--profile ai] [--format cin]`
   - Purpose: Generate AI-ready prompts with codebase context for individual beads
   - Profiles: ai (AI-optimized) or human (human-readable)
   - Formats: standard (text), JSON, and potentially CIN (Compact Intent Notation)

2. **Current Implementation Status**:
   - Prompt command is NOT yet registered in main CLI
   - `implementation_prompt_generator.gleam` exists with:
     - `PromptProfile` type (AiProfile, HumanProfile)
     - `build_implementation_prompt()` - generates text prompt
     - `build_json_implementation_prompt()` - generates JSON prompt
     - Stub functions that need real bead context integration
   - Beads are stored/managed in `.beads/` directory
   - Current implementation uses hard-coded example data

3. **Key Modules**:
   - `implementation_prompt_generator.gleam` - Core prompt generation
   - `bead_types.gleam` - Bead structure definition
   - `bead_templates.gleam` - Bead record generation from interviews
   - `bead_feedback.gleam` - Bead execution tracking

4. **Context Requirements for Bead Prompts**:
   - Bead metadata: id, title, description, type, priority
   - Spec context: Only relevant behaviors/features (filtered by requirements)
   - Implementation guidelines: Architecture, error handling, patterns
   - Acceptance criteria: From bead record
   - Style guide: CLAUDE.md guidelines

5. **Current Bead Structure** (from bead_types.gleam):
   ```
   Bead(
     id: String,
     title: String,
     status: BeadStatus,
     priority: Int,
     issue_type: BeadKind,
     created_at: String,
     created_by: String,
     updated_at: String,
     labels: List(String),
   )
   ```

---

## STEP 2: PLAN - Design

### Design Overview:

**Goal**: Implement bead-specific prompt export that:
1. Takes a bead ID as input
2. Loads bead metadata from `.beads/` or bead tracker
3. Filters spec context to show only relevant features/behaviors
4. Generates AI-ready prompt with focused context
5. Supports export to file with `--export <file>` flag

### Architecture:

#### Phase 1: Bead Context Loading
- Create `bead_context_loader.gleam` module
- Function: `load_bead_context(bead_id: String) -> Result(BeadContext, Error)`
- Responsibilities:
  - Query bead storage to get bead metadata and requirements
  - Parse bead dependencies/labels to determine relevant spec context
  - Return structured context for prompt generation

#### Phase 2: Spec Context Filtering
- Extend `bead_context_loader.gleam`
- Function: `filter_spec_for_bead(spec: Spec, bead: Bead) -> FilteredSpec`
- Logic:
  - Extract only features/behaviors matching bead labels/requirements
  - Include related behaviors via `requires` field
  - Exclude irrelevant implementation details
  - Preserve all checks and validation rules

#### Phase 3: Enhanced Prompt Generation
- Enhance `implementation_prompt_generator.gleam`
- New function: `generate_bead_specific_prompt(bead_context, profile) -> Result(String, Error)`
- Content:
  - Bead-specific metadata header
  - Filtered spec context (only relevant features/behaviors)
  - Acceptance criteria from bead
  - Related behaviors/dependencies
  - Standard implementation guidelines
  - Bead-specific checklist

#### Phase 4: Export Functionality
- Add to `implementation_prompt_generator.gleam`
- Function: `export_bead_prompt(bead_id, profile, output_file) -> Result(Nil, Error)`
- Functionality:
  - Generate prompt for bead
  - Write to specified file or stdout
  - Support JSON and text formats
  - Return success/error result

#### Phase 5: CLI Integration
- Register `prompt` command in `intent.gleam`
- Command signature:
  ```
  intent prompt <bead-id> [--profile ai|human] [--json] [--export <file>]
  ```
- Flags:
  - `--profile`: ai (default) or human
  - `--json`: Output as JSON instead of text
  - `--export`: Write to file instead of stdout

### Context Filtering Strategy:

1. **By Labels**: If bead has labels like "api", "auth", only include features tagged similarly
2. **By Requirements**: If bead lists dependencies, include required behaviors
3. **By Type**: If bead is "api_endpoint", include all API features
4. **Transitive Dependencies**: Include any behaviors that dependent beads require

### Data Flow:

```
bead_id
  ↓
[load_bead_context]
  ↓
BeadContext(bead_metadata, requirements, labels)
  ↓
[filter_spec_for_bead]
  ↓
FilteredSpec(relevant_features, relevant_behaviors)
  ↓
[generate_bead_specific_prompt]
  ↓
Prompt(text or JSON)
  ↓
[export or stdout]
```

### Key Files to Create/Modify:

**New Files**:
- `src/intent/bead_context_loader.gleam` - Load and filter context
- `src/intent/bead_prompt_exporter.gleam` - Export logic

**Modified Files**:
- `src/intent/implementation_prompt_generator.gleam` - Add bead-specific functions
- `src/intent.gleam` - Register prompt command

---

## Export Format Examples:

### Text Format (Default):
```
=== BEAD SPECIFIC PROMPT ===

BEAD ID: intent-cli-r91l
TITLE: Implement bead-specific prompt export
PRIORITY: 2
TYPE: feature

CONTEXT:
Relevant features from spec:
- AI-Native Features: Bead-to-Prompt Pipeline
- Commands: prompt, export
- Implementation Guidelines: FC/IS Architecture

ACCEPTANCE CRITERIA:
- Export prompts specific to beads
- Include only relevant context
- Filter spec by requirements
- Export to file with --export flag
- Support both text and JSON output

RELEVANT BEHAVIORS:
- intent prompt <spec> [--bead ID] → generates prompt with context
- intent prompt <spec> --export <file> → saves to file

[Standard implementation guidelines...]
```

### JSON Format:
```json
{
  "bead": {
    "id": "intent-cli-r91l",
    "title": "Implement bead-specific prompt export",
    "type": "feature",
    "priority": 2
  },
  "context": {
    "relevant_features": ["AI-Native Features"],
    "relevant_behaviors": [...],
    "requirements": ["context filtering", "file export"]
  },
  "acceptance_criteria": [...],
  "guidelines": {...}
}
```

---

## Summary:

**Total Implementation**:
- 2 new modules (~400-500 lines of Gleam code)
- 1 CLI command registration
- Enhanced prompt generation with context filtering
- Export functionality for text/JSON
- Support for relevant context extraction

**Value**: Developers get focused, actionable prompts for individual beads without noise from unrelated features.
