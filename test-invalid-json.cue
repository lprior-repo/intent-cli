// Test case: Invalid JSON in examples
name: "invalid-json-spec"
description: "Spec with invalid JSON in examples"
audience: "developers"
version: "1.0.0"
success_criteria: ["API works"]
config: {
  base_url: "https://api.example.com"
  timeout_ms: 5000
  headers: {}
}
features: [
  {
    name: "feature1"
    description: "A test feature"
    behaviors: [
      {
        name: "behavior1"
        intent: "Test the API"
        request: {
          method: "GET"
          path: "/test"
          headers: {}
          query: {}
          body: null
        }
        response: {
          status: 200
          example: {"message": "ok", invalid: json}  // Invalid JSON syntax
          checks: [
            {
              rule: "status == 200"
              why: "Request should succeed"
            }
          ]
          headers: {}
        }
        notes: "This is a test behavior"
        requires: []
        tags: ["test"]
        captures: {}
      }
    ]
  }
]
rules: []
anti_patterns: []
ai_hints: []