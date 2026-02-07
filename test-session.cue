package test_spec

session: {
	beads: [
		{
			id: "AUTH-001"
			title: "Setup authentication system"
			requires: []
			effort: "10min"
			status: "pending"
		},
		{
			id: "DB-001"
			title: "Create database schema"
			requires: []
			effort: "15min"
			status: "pending"
		},
		{
			id: "API-001"
			title: "Implement user API endpoints"
			requires: ["AUTH-001", "DB-001"]
			effort: "20min"
			status: "pending"
		},
		{
			id: "UI-001"
			title: "Build frontend interface"
			requires: ["API-001"]
			effort: "25min"
			status: "pending"
		}
	]
}