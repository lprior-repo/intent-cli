// Test case: Circular dependencies in behaviors
name: "circular-dependencies-spec"
description: "Spec with circular behavior dependencies"
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
    description: "A test feature with circular deps"
    behaviors: [
      {
        name: "behavior1"
        intent: "Test behavior 1"
        request: {
          method: "GET"
          path: "/test1"
          headers: {}
          query: {}
          body: null
        }
        response: {
          status: 200
          example: {"message": "ok1"}
          checks: [
            {
              rule: "status == 200"
              why: "Request should succeed"
            }
          ]
          headers: {}
        }
        notes: "Behavior 1 depends on behavior 2"
        requires: ["behavior2"]
        tags: ["test"]
        captures: {}
      }
      {
        name: "behavior2"
        intent: "Test behavior 2"
        request: {
          method: "GET"
          path: "/test2"
          headers: {}
          query: {}
          body: null
        }
        response: {
          status: 200
          example: {"message": "ok2"}
          checks: [
            {
              rule: "status == 200"
              why: "Request should succeed"
            }
          ]
          headers: {}
        }
        notes: "Behavior 2 depends on behavior 1"
        requires: ["behavior1"]
        tags: ["test"]
        captures: {}
      }
    ]
  }
]
rules: []
anti_patterns: []
ai_hints: []