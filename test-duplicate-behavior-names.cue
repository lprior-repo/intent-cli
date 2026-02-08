{
  name: "test-duplicate-behavior-names"
  description: "Test spec with duplicate behavior names"
  audience: "developers"
  version: "1.0.0"
  success_criteria: ["Test works"]
  config: {
    base_url: "http://localhost:8080"
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
          intent: "Test intent"
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
          notes: ""
          requires: []
          tags: []
          captures: {}
        }
      ]
    }
    {
      name: "feature2"
      description: "Second feature"
      behaviors: [
        {
          name: "duplicate_behavior"
          intent: "Test intent"
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
          notes: ""
          requires: []
          tags: []
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
