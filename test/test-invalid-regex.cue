// Test case: Invalid regex patterns
name: "invalid-regex-spec"
description: "Spec with invalid regex patterns"
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
    description: "A test feature with invalid regex"
    behaviors: [
      {
        name: "behavior1"
        intent: "Test behavior with invalid regex"
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
              rule: "body.message matches [a-z"
              why: "Message should match pattern"
            }
          ]
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