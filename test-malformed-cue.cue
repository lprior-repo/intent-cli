// Test case: Malformed CUE syntax
name: "malformed-spec"
description: "Spec with malformed CUE syntax"
audience: "developers"
version: "1.0.0"
success_criteria: ["API works"]
config: {
  base_url: "https://api.example.com"
  timeout_ms: 5000
  headers: {}
}
features: [
  // Missing closing brace
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
          example: {"message": "ok"}
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
rules: []
anti_patterns: []
ai_hints: []