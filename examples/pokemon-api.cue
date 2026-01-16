package pokemon_api

import "github.com/intent-cli/intent/schema:intent"

spec: intent.#Spec & {
	name: "Pokemon API"

	description: """
		A simple API for managing Pokemon and their trainers.
		Trainers can have multiple Pokemon, and each Pokemon belongs
		to exactly one trainer.
		"""

	audience: "Game clients and admin tools"

	success_criteria: [
		"Pokemon can be listed and retrieved by ID",
		"Trainers can be listed and retrieved by ID",
		"New Pokemon can be created with valid data",
		"All errors return structured error objects",
	]

	config: {
		base_url:   "http://localhost:8080"
		timeout_ms: 5000
	}

	features: [
		{
			name: "Pokemon Retrieval"

			description: """
				Users can list all Pokemon or retrieve a specific Pokemon
				by its ID. Pokemon IDs are prefixed with 'pkmn_'.
				"""

			behaviors: [
				{
					name:   "list-all-pokemon"
					intent: "Get a list of all Pokemon in the system"

					request: {
						method: "GET"
						path:   "/pokemon"
					}

					response: {
						status: 200

						example: {
							pokemon: [
								{
									id:         "pkmn_001"
									name:       "Pikachu"
									type:       "Electric"
									level:      25
									hp:         100
									trainer_id: "trainer_ash"
								},
							]
						}

						checks: {
							"pokemon": {
								rule: "non-empty array"
								why:  "Should have at least one Pokemon in the test data"
							}
						}
					}
				},
				{
					name:   "get-existing-pokemon"
					intent: "Retrieve a specific Pokemon by ID"

					request: {
						method: "GET"
						path:   "/pokemon/pkmn_001"
					}

					response: {
						status: 200

						example: {
							id:         "pkmn_001"
							name:       "Pikachu"
							type:       "Electric"
							level:      25
							hp:         100
							trainer_id: "trainer_ash"
						}

						checks: {
							"id": {
								rule: "string matching pkmn_[0-9]+"
								why:  "Pokemon IDs are prefixed for debuggability"
							}
							"name": {
								rule: "equals Pikachu"
								why:  "Pikachu is the first Pokemon in test data"
							}
							"type": {
								rule: "one of [\"Electric\", \"Fire\", \"Water\", \"Grass\"]"
								why:  "Pokemon types are from a predefined list"
							}
							"level": {
								rule: "integer >= 1"
								why:  "Pokemon level must be at least 1"
							}
							"hp": {
								rule: "integer >= 1"
								why:  "HP must be positive"
							}
							"trainer_id": {
								rule: "string matching trainer_[a-z]+"
								why:  "Trainer IDs are prefixed identifiers"
							}
						}
					}

					captures: {
						pikachu_id: "response.body.id"
					}
				},
				{
					name:   "get-nonexistent-pokemon"
					intent: "Requesting a non-existent Pokemon returns 404"

					request: {
						method: "GET"
						path:   "/pokemon/pkmn_999"
					}

					response: {
						status: 404

						example: {
							error: {
								code:    "POKEMON_NOT_FOUND"
								message: "Pokemon with id 'pkmn_999' not found"
							}
						}

						checks: {
							"error.code": {
								rule: "equals POKEMON_NOT_FOUND"
								why:  "Specific error code for client handling"
							}
							"error.message": {
								rule: "non-empty string"
								why:  "Human-readable error message"
							}
						}
					}
				},
			]
		},
		{
			name: "Trainer Retrieval"

			description: """
				Users can list all trainers or retrieve a specific trainer
				by ID. Trainer responses include a pokemon_count field.
				"""

			behaviors: [
				{
					name:   "list-all-trainers"
					intent: "Get a list of all trainers"

					request: {
						method: "GET"
						path:   "/trainers"
					}

					response: {
						status: 200

						example: {
							trainers: [
								{
									id:            "trainer_ash"
									name:          "Ash Ketchum"
									pokemon_count: 2
								},
							]
						}

						checks: {
							"trainers": {
								rule: "non-empty array"
								why:  "Should have at least one trainer in test data"
							}
						}
					}
				},
				{
					name:   "get-existing-trainer"
					intent: "Retrieve a specific trainer by ID"

					request: {
						method: "GET"
						path:   "/trainers/trainer_ash"
					}

					response: {
						status: 200

						example: {
							id:            "trainer_ash"
							name:          "Ash Ketchum"
							pokemon_count: 2
						}

						checks: {
							"id": {
								rule: "equals trainer_ash"
								why:  "Confirms we got the right trainer"
							}
							"name": {
								rule: "non-empty string"
								why:  "Trainer must have a name"
							}
							"pokemon_count": {
								rule: "integer >= 0"
								why:  "Count can be zero but not negative"
							}
						}
					}

					captures: {
						ash_id: "response.body.id"
					}
				},
				{
					name:   "get-trainer-pokemon"
					intent: "Get all Pokemon belonging to a trainer"

					requires: ["get-existing-trainer"]

					request: {
						method: "GET"
						path:   "/trainers/${ash_id}/pokemon"
					}

					response: {
						status: 200

						example: {
							pokemon: [
								{
									id:         "pkmn_001"
									name:       "Pikachu"
									type:       "Electric"
									level:      25
									hp:         100
									trainer_id: "trainer_ash"
								},
							]
						}

						checks: {
							"pokemon": {
								rule: "non-empty array"
								why:  "Ash has at least one Pokemon"
							}
						}
					}
				},
				{
					name:   "get-nonexistent-trainer"
					intent: "Requesting a non-existent trainer returns 404"

					request: {
						method: "GET"
						path:   "/trainers/trainer_unknown"
					}

					response: {
						status: 404

						checks: {
							"error.code": {
								rule: "equals TRAINER_NOT_FOUND"
								why:  "Specific error code for trainer not found"
							}
						}
					}
				},
			]
		},
		{
			name: "Pokemon Creation"

			description: """
				New Pokemon can be created with a name, type, level,
				and trainer_id. The system validates all fields.
				"""

			behaviors: [
				{
					name:   "create-pokemon-success"
					intent: "Create a new Pokemon with valid data"

					requires: ["get-existing-trainer"]

					request: {
						method: "POST"
						path:   "/pokemon"
						body: {
							name:       "Squirtle"
							type:       "Water"
							level:      10
							trainer_id: "trainer_ash"
						}
					}

					response: {
						status: 201

						example: {
							id:         "pkmn_004"
							name:       "Squirtle"
							type:       "Water"
							level:      10
							hp:         40
							trainer_id: "trainer_ash"
						}

						checks: {
							"id": {
								rule: "string matching pkmn_[0-9]+"
								why:  "New Pokemon gets a prefixed ID"
							}
							"name": {
								rule: "equals Squirtle"
								why:  "Name matches what we sent"
							}
							"type": {
								rule: "equals Water"
								why:  "Type matches what we sent"
							}
							"level": {
								rule: "equals 10"
								why:  "Level matches what we sent"
							}
							"hp": {
								rule: "equals 40"
								why:  "HP is calculated as level * 4"
							}
						}
					}

					captures: {
						new_pokemon_id: "response.body.id"
					}

					notes: """
						HP is calculated server-side as level * 4.
						The ID is auto-generated with pkmn_ prefix.
						"""
				},
				{
					name:   "create-pokemon-invalid-level"
					intent: "Creating Pokemon with invalid level fails"

					request: {
						method: "POST"
						path:   "/pokemon"
						body: {
							name:       "BadMon"
							type:       "Fire"
							level:      999
							trainer_id: "trainer_ash"
						}
					}

					response: {
						status: 400

						checks: {
							"error.code": {
								rule: "equals INVALID_LEVEL"
								why:  "Level validation returns specific error"
							}
						}
					}

					notes: "Level must be between 1 and 100"
				},
				{
					name:   "create-pokemon-invalid-trainer"
					intent: "Creating Pokemon for non-existent trainer fails"

					request: {
						method: "POST"
						path:   "/pokemon"
						body: {
							name:       "OrphanMon"
							type:       "Ghost"
							level:      15
							trainer_id: "trainer_nobody"
						}
					}

					response: {
						status: 400

						checks: {
							"error.code": {
								rule: "equals TRAINER_NOT_FOUND"
								why:  "Trainer validation returns specific error"
							}
						}
					}
				},
			]
		},
	]

	rules: [
		{
			name:        "structured-errors"
			description: "All error responses have consistent structure"

			when: {status: ">= 400"}

			check: {
				fields_must_exist: ["error.code", "error.message"]
			}

			example: {
				error: {
					code:    "ERROR_CODE"
					message: "Human readable description"
				}
			}
		},
		{
			name:        "content-type-header"
			description: "All responses declare content type"

			check: {
				header_must_exist: "Content-Type"
			}
		},
	]

	anti_patterns: [
		{
			name:        "sequential-ids"
			description: "IDs should not be plain sequential integers"

			bad_example: {
				id: 1
			}

			good_example: {
				id: "pkmn_001"
			}

			why: """
				Sequential IDs reveal business metrics and enable enumeration
				attacks. Use prefixed identifiers instead.
				"""
		},
	]

	ai_hints: {
		implementation: {
			suggested_stack: ["Python", "Flask", "SQLite"]
		}

		entities: {
			pokemon: {
				fields: {
					id:         "string, prefixed 'pkmn_', auto-generated"
					name:       "string, 1-50 chars"
					type:       "string, one of predefined types"
					level:      "integer, 1-100"
					hp:         "integer, calculated as level * 4"
					trainer_id: "string, foreign key to trainer"
				}
			}
			trainer: {
				fields: {
					id:   "string, prefixed 'trainer_'"
					name: "string, 1-100 chars"
				}
			}
		}

		pitfalls: [
			"Don't use sequential integer IDs",
			"Don't forget to validate trainer exists before creating Pokemon",
			"Don't allow levels outside 1-100 range",
		]
	}
}
