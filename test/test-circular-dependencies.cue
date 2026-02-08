{
  name: "circular-dep-test"
  description: "Test spec with circular dependencies"
  audience: "developers"
  version: "1.0.0"
  success_criteria: ["Test circular dependency detection"]
  config: {
    base_url: "http://localhost:8080"
    timeout_ms: 5000
    headers: {}
  }
  features: [
    {
      name: "test-feature"
      description: "Feature with circular behaviors"
      behaviors: [
        {
          name: "behavior1"
          intent: "First behavior"
          notes: ""
          requires: ["behavior2"]
          tags: []
          request: {
            method: "GET"
            path: "/api/test1"
            headers: {}
            query: {}
            body: null
          }
          response: {
            status: 200
            example: null
            checks: {}
            headers: null
          }
          captures: {}
        }
        {
          name: "behavior2"
          intent: "Second behavior"
          notes: ""
          requires: ["behavior1"]
          tags: []
          request: {
            method: "GET"
            path: "/api/test2"
            headers: {}
            query: {}
            body: null
          }
          response: {
            status: 200
            example: null
            checks: {}
            headers: null
          }
          captures: {}
        }
      ]
    }
  ]
  rules: []
  anti_patterns: []
  ai_hints: {
    implementation: null
    entities: null
    security: null
    pitfalls: null
  }
}
