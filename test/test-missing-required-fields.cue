// Test case: Missing required fields
description: "Spec missing required fields like name"
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
        intent: "Test behavior missing required response fields"
        request: {
          method: "GET"
          path: "/test"
          headers: {}
          query: {}
          body: null
        }
        response: {
          status: 200
          // Missing 'example' and 'checks' fields
          headers: {}
        }
        notes: ""
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