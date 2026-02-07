// Test case: Duplicate behavior names in features
name: "duplicate-names-spec"
description: "Spec with duplicate behavior names"
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
    description: "First feature"
    behaviors: [
      {
        name: "duplicate_behavior"
        intent: "Test behavior in first feature"
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
        notes: ""
        requires: []
        tags: ["test"]
        captures: {}
      }
    ]
  }
  {
    name: "feature2"
    description: "Second feature with duplicate name"
    behaviors: [
      {
        name: "duplicate_behavior"
        intent: "Test behavior in second feature"
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