# Intent CLI JSON Schemas

Formal JSON Schema definitions for Intent CLI's JSON output format.

## Usage

### Validation with `ajv` (Node.js)

```bash
npm install ajv ajv-formats
```

```javascript
const Ajv = require('ajv');
const addFormats = require('ajv-formats');
const ajv = new Ajv();
addFormats(ajv);

const baseSchema = require('./base-response.json');
const qualitySchema = require('./quality-response.json');

// Validate quality response
const validate = ajv.compile(qualitySchema);
const valid = validate(responseData);

if (!valid) {
  console.error('Validation errors:', validate.errors);
}
```

### Validation with Python

```bash
pip install jsonschema
```

```python
import json
from jsonschema import validate, ValidationError

with open('quality-response.json') as f:
    schema = json.load(f)

with open('response.json') as f:
    data = json.load(f)

try:
    validate(instance=data, schema=schema)
    print("Response is valid")
except ValidationError as e:
    print(f"Validation error: {e.message}")
```

### CI/CD Integration

Use schemas to validate Intent CLI output in CI/CD pipelines:

```yaml
# GitHub Actions example
- name: Validate Quality Report
  run: |
    intent quality spec.cue --json=true > quality.json
    npx ajv validate -s schema/json-schema/quality-response.json -d quality.json
```

## Available Schemas

| File | Command | Description |
|------|---------|-------------|
| `base-response.json` | All | Base schema for all responses |
| `quality-response.json` | `quality` | Quality analysis output |
| `check-response.json` | `check` | HTTP test results |

## Schema Structure

All schemas extend `base-response.json` and add command-specific `data` definitions.

### Base Response

```json
{
  "success": true,
  "action": "quality_report",
  "command": "quality",
  "data": { ... },
  "errors": [],
  "next_actions": [],
  "metadata": { ... },
  "spec_path": "spec.cue"
}
```

### Metadata

All responses include metadata:

```json
{
  "timestamp": "2026-01-25T14:30:00Z",
  "version": "0.1.0",
  "exit_code": 0,
  "correlation_id": "550e8400-e29b-41d4-a716-446655440000",
  "duration_ms": 123
}
```

## Generating Schemas from CUE

The authoritative schema definitions are in `schema/ai/output/*.cue`. To convert to JSON Schema:

```bash
cue export schema/ai/output/quality.cue --out jsonschema > schema/json-schema/quality-response.json
```

## TypeScript Integration

For TypeScript projects, use the type definitions in `schema/intent-cli.d.ts`:

```typescript
import type { QualityResponse } from './schema/intent-cli';

const response: QualityResponse = JSON.parse(stdout);
console.log(`Score: ${response.data.overall_score}`);
```

## Version Compatibility

- Schemas follow semantic versioning
- Check `metadata.version` in responses for compatibility
- Breaking changes increment major version
- New optional fields may be added in minor versions

## References

- Full documentation: [docs/JSON_SCHEMA.md](../../docs/JSON_SCHEMA.md)
- TypeScript types: [schema/intent-cli.d.ts](../intent-cli.d.ts)
- CUE definitions: [schema/ai/output/](../ai/output/)
