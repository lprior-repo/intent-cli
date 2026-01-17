package pagination_api

import "github.com/intent-cli/intent/schema:intent"

spec: intent.#Spec & {
	name: "Pagination API"

	description: """
		A comprehensive API demonstrating three pagination patterns:
		offset/limit (traditional), cursor-based (for infinite scroll),
		and page-based (numbered pages). Each pattern has different
		trade-offs for consistency, performance, and user experience.
		"""

	audience: "Web and mobile clients requiring paginated data access"

	version: "1.0.0"

	success_criteria: [
		"Offset/limit pagination works for small datasets",
		"Cursor-based pagination handles real-time data insertion",
		"Page-based pagination provides numbered page navigation",
		"All patterns return consistent metadata",
		"Edge cases handled gracefully (empty results, invalid params)",
	]

	config: {
		base_url:   "http://localhost:8080"
		timeout_ms: 5000
		headers: {
			"Content-Type": "application/json"
		}
	}

	features: [
		{
			name: "Offset/Limit Pagination"

			description: """
				Traditional pagination using offset and limit parameters.
				Best for small, static datasets where consistency during
				pagination is not critical. Simple to implement but can
				miss or duplicate items if data changes during pagination.
				"""

			behaviors: [
				{
					name:   "list-products-default"
					intent: "Get first page of products with default limit"

					request: {
						method: "GET"
						path:   "/products"
						query: {}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						example: {
							products: [
								{
									id:    "prod_001"
									name:  "Laptop"
									price: 999.99
								},
								{
									id:    "prod_002"
									name:  "Mouse"
									price: 29.99
								},
							]
							pagination: {
								offset:      0
								limit:       20
								total:       150
								has_more:    true
								next_offset: 20
							}
						}

						checks: {
							"products": {
								rule: "non-empty array"
								why:  "Default request returns products"
							}
							"pagination.offset": {
								rule: "equals 0"
								why:  "First page starts at offset 0"
							}
							"pagination.limit": {
								rule: "equals 20"
								why:  "Default limit is 20 items"
							}
							"pagination.total": {
								rule: "integer >= 0"
								why:  "Total count of all items"
							}
							"pagination.has_more": {
								rule: "boolean"
								why:  "Indicates if more pages exist"
							}
						}
					}

					captures: {
						total_products: "response.body.pagination.total"
					}

					tags: ["pagination", "offset-limit", "default"]
					requires: []
					notes: """
						Default limit is 20. If no offset is provided, starts at 0.
						The total field represents the total count across all pages.
						"""
				},
				{
					name:   "list-products-offset-limit"
					intent: "Get specific page using offset and limit"

					request: {
						method: "GET"
						path:   "/products"
						query: {
							offset: "20"
							limit:  "10"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						checks: {
							"products": {
								rule: "array"
								why:  "Returns products array (may be empty)"
							}
							"pagination.offset": {
								rule: "equals 20"
								why:  "Requested offset is returned"
							}
							"pagination.limit": {
								rule: "equals 10"
								why:  "Requested limit is returned"
							}
							"pagination.next_offset": {
								rule: "equals 30"
								why:  "Next offset is current offset + limit"
							}
						}
					}

					tags: ["pagination", "offset-limit"]
					requires: []
					notes: """
						Offset/limit pattern is simple but has issues:
						- Items can be skipped if inserted during pagination
						- Items can be duplicated if deleted during pagination
						- Offset becomes slow for large values (OFFSET 1000000)
						"""
				},
				{
					name:   "list-products-invalid-offset"
					intent: "Reject negative offset values"

					request: {
						method: "GET"
						path:   "/products"
						query: {
							offset: "-10"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 400
						headers: {}

						example: {
							error: {
								code:    "INVALID_OFFSET"
								message: "Offset must be a non-negative integer"
								field:   "offset"
								value:   "-10"
							}
						}

						checks: {
							"error.code": {
								rule: "equals INVALID_OFFSET"
								why:  "Specific error for invalid offset"
							}
							"error.field": {
								rule: "equals offset"
								why:  "Identifies the problematic parameter"
							}
						}
					}

					tags: ["pagination", "validation", "error"]
					requires: []
					notes: ""
				},
				{
					name:   "list-products-invalid-limit"
					intent: "Reject limit values outside allowed range"

					request: {
						method: "GET"
						path:   "/products"
						query: {
							limit: "500"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 400
						headers: {}

						example: {
							error: {
								code:    "INVALID_LIMIT"
								message: "Limit must be between 1 and 100"
								field:   "limit"
								value:   "500"
							}
						}

						checks: {
							"error.code": {
								rule: "equals INVALID_LIMIT"
								why:  "Specific error for invalid limit"
							}
							"error.message": {
								rule: "contains between 1 and 100"
								why:  "Communicates valid range"
							}
						}
					}

					tags: ["pagination", "validation", "error"]
					requires: []
					notes: "Limit is capped at 100 to prevent excessive load"
				},
				{
					name:   "list-products-beyond-end"
					intent: "Handle offset beyond total count gracefully"

					request: {
						method: "GET"
						path:   "/products"
						query: {
							offset: "999999"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						example: {
							products: []
							pagination: {
								offset:   999999
								limit:    20
								total:    150
								has_more: false
							}
						}

						checks: {
							"products": {
								rule: "empty array"
								why:  "No products beyond the end"
							}
							"pagination.has_more": {
								rule: "equals false"
								why:  "No more pages available"
							}
						}
					}

					tags: ["pagination", "edge-case"]
					requires: []
					notes: "Returning 200 with empty array is better UX than 404"
				},
			]
		},
		{
			name: "Cursor-Based Pagination"

			description: """
				Cursor-based pagination using opaque tokens that encode
				the position in the result set. Best for real-time data
				and infinite scroll. Provides consistent results even when
				data changes, but cannot jump to arbitrary pages.
				"""

			behaviors: [
				{
					name:   "list-posts-first-page"
					intent: "Get first page of posts without cursor"

					request: {
						method: "GET"
						path:   "/posts"
						query: {
							limit: "10"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						example: {
							posts: [
								{
									id:         "post_abc123"
									title:      "First Post"
									created_at: "2024-01-15T10:30:00Z"
								},
								{
									id:         "post_abc124"
									title:      "Second Post"
									created_at: "2024-01-15T10:29:00Z"
								},
							]
							pagination: {
								limit:       10
								has_more:    true
								next_cursor: "eyJpZCI6InBvc3RfYWJjMTI0IiwidCI6MTcwNTMxNjk0MH0="
							}
						}

						checks: {
							"posts": {
								rule: "non-empty array"
								why:  "First page returns posts"
							}
							"pagination.next_cursor": {
								rule: "base64 string"
								why:  "Cursor is opaque base64-encoded token"
							}
							"pagination.has_more": {
								rule: "boolean"
								why:  "Indicates if more results exist"
							}
						}
					}

					captures: {
						next_cursor: "response.body.pagination.next_cursor"
					}

					tags: ["pagination", "cursor", "first-page"]
					requires: []
					notes: """
						Cursor encodes the position in the result set (usually
						the last item's ID and timestamp). This allows consistent
						pagination even when new items are inserted.
						"""
				},
				{
					name:   "list-posts-next-page"
					intent: "Get next page using cursor from previous response"

					requires: ["list-posts-first-page"]

					request: {
						method: "GET"
						path:   "/posts"
						query: {
							cursor: "${next_cursor}"
							limit:  "10"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						checks: {
							"posts": {
								rule: "array"
								why:  "Returns next set of posts"
							}
							"pagination.has_more": {
								rule: "boolean"
								why:  "Indicates if more pages exist"
							}
						}
					}

					tags: ["pagination", "cursor", "next-page"]
					notes: """
						The cursor is opaque to clients - they should treat it
						as a black box and not try to parse or construct cursors.
						"""
				},
				{
					name:   "list-posts-invalid-cursor"
					intent: "Reject malformed or expired cursors"

					request: {
						method: "GET"
						path:   "/posts"
						query: {
							cursor: "invalid-cursor-123"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 400
						headers: {}

						example: {
							error: {
								code:    "INVALID_CURSOR"
								message: "The cursor is invalid or has expired"
								hint:    "Request the first page without a cursor"
							}
						}

						checks: {
							"error.code": {
								rule: "equals INVALID_CURSOR"
								why:  "Specific error for invalid cursor"
							}
							"error.hint": {
								rule: "non-empty string"
								why:  "Provides recovery guidance"
							}
						}
					}

					tags: ["pagination", "cursor", "validation", "error"]
					requires: []
					notes: "Cursors should have TTL (e.g., 1 hour) to prevent abuse"
				},
				{
					name:   "list-posts-last-page"
					intent: "Last page has no next_cursor and has_more is false"

					request: {
						method: "GET"
						path:   "/posts"
						query: {
							cursor: "eyJpZCI6InBvc3RfbGFzdCIsInQiOjE3MDUzMTY5NDB9"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						example: {
							posts: [
								{
									id:         "post_xyz999"
									title:      "Last Post"
									created_at: "2024-01-01T00:00:00Z"
								},
							]
							pagination: {
								limit:    10
								has_more: false
							}
						}

						checks: {
							"pagination.has_more": {
								rule: "equals false"
								why:  "No more results available"
							}
							"pagination.next_cursor": {
								rule: "absent"
								why:  "No cursor when has_more is false"
							}
						}
					}

					tags: ["pagination", "cursor", "last-page"]
					requires: []
					notes: "When has_more is false, next_cursor should be absent"
				},
			]
		},
		{
			name: "Page-Based Pagination"

			description: """
				Page-based pagination using page number and page size.
				Best for UIs with page number controls (1, 2, 3...).
				Familiar to users but can have consistency issues
				like offset/limit when data changes.
				"""

			behaviors: [
				{
					name:   "list-users-first-page"
					intent: "Get first page of users with default page size"

					request: {
						method: "GET"
						path:   "/users"
						query: {
							page: "1"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						example: {
							users: [
								{
									id:    "usr_001"
									email: "alice@example.com"
									name:  "Alice"
								},
								{
									id:    "usr_002"
									email: "bob@example.com"
									name:  "Bob"
								},
							]
							pagination: {
								page:          1
								page_size:     25
								total_pages:   10
								total_items:   250
								has_previous:  false
								has_next:      true
								previous_page: null
								next_page:     2
							}
						}

						checks: {
							"users": {
								rule: "non-empty array"
								why:  "First page returns users"
							}
							"pagination.page": {
								rule: "equals 1"
								why:  "Current page number"
							}
							"pagination.page_size": {
								rule: "equals 25"
								why:  "Default page size is 25"
							}
							"pagination.total_pages": {
								rule: "integer >= 1"
								why:  "Total number of pages"
							}
							"pagination.total_items": {
								rule: "integer >= 0"
								why:  "Total count of all users"
							}
							"pagination.has_previous": {
								rule: "equals false"
								why:  "First page has no previous"
							}
							"pagination.has_next": {
								rule: "equals true"
								why:  "More pages exist"
							}
							"pagination.next_page": {
								rule: "equals 2"
								why:  "Next page is page 2"
							}
						}
					}

					tags: ["pagination", "page-based", "first-page"]
					requires: []
					notes: """
						Page numbers start at 1 (not 0). The previous_page
						field is null on the first page.
						"""
				},
				{
					name:   "list-users-middle-page"
					intent: "Get middle page with both previous and next pages"

					request: {
						method: "GET"
						path:   "/users"
						query: {
							page:      "5"
							page_size: "10"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						checks: {
							"users": {
								rule: "array"
								why:  "Returns users for requested page"
							}
							"pagination.page": {
								rule: "equals 5"
								why:  "Requested page number"
							}
							"pagination.has_previous": {
								rule: "equals true"
								why:  "Middle page has previous page"
							}
							"pagination.has_next": {
								rule: "boolean"
								why:  "May or may not have next page"
							}
							"pagination.previous_page": {
								rule: "equals 4"
								why:  "Previous page is page 4"
							}
						}
					}

					tags: ["pagination", "page-based", "middle-page"]
					requires: []
					notes: ""
				},
				{
					name:   "list-users-last-page"
					intent: "Last page has no next page"

					request: {
						method: "GET"
						path:   "/users"
						query: {
							page: "10"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						example: {
							users: [
								{
									id:    "usr_250"
									email: "zoe@example.com"
									name:  "Zoe"
								},
							]
							pagination: {
								page:          10
								page_size:     25
								total_pages:   10
								total_items:   250
								has_previous:  true
								has_next:      false
								previous_page: 9
								next_page:     null
							}
						}

						checks: {
							"pagination.has_next": {
								rule: "equals false"
								why:  "Last page has no next"
							}
							"pagination.next_page": {
								rule: "null"
								why:  "Next page is null on last page"
							}
							"pagination.has_previous": {
								rule: "equals true"
								why:  "Last page has previous pages"
							}
						}
					}

					tags: ["pagination", "page-based", "last-page"]
					requires: []
					notes: ""
				},
				{
					name:   "list-users-invalid-page"
					intent: "Reject page number less than 1"

					request: {
						method: "GET"
						path:   "/users"
						query: {
							page: "0"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 400
						headers: {}

						example: {
							error: {
								code:    "INVALID_PAGE"
								message: "Page number must be at least 1"
								field:   "page"
								value:   "0"
							}
						}

						checks: {
							"error.code": {
								rule: "equals INVALID_PAGE"
								why:  "Specific error for invalid page"
							}
						}
					}

					tags: ["pagination", "validation", "error"]
					requires: []
					notes: "Page numbers start at 1, not 0"
				},
				{
					name:   "list-users-invalid-page-size"
					intent: "Reject page size outside allowed range"

					request: {
						method: "GET"
						path:   "/users"
						query: {
							page:      "1"
							page_size: "1000"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 400
						headers: {}

						example: {
							error: {
								code:    "INVALID_PAGE_SIZE"
								message: "Page size must be between 1 and 100"
								field:   "page_size"
								value:   "1000"
							}
						}

						checks: {
							"error.code": {
								rule: "equals INVALID_PAGE_SIZE"
								why:  "Specific error for invalid page size"
							}
						}
					}

					tags: ["pagination", "validation", "error"]
					requires: []
					notes: "Page size is capped at 100 to prevent excessive load"
				},
				{
					name:   "list-users-beyond-total-pages"
					intent: "Handle page beyond total gracefully"

					request: {
						method: "GET"
						path:   "/users"
						query: {
							page: "999"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						example: {
							users: []
							pagination: {
								page:          999
								page_size:     25
								total_pages:   10
								total_items:   250
								has_previous:  true
								has_next:      false
								previous_page: 998
								next_page:     null
							}
						}

						checks: {
							"users": {
								rule: "empty array"
								why:  "No users beyond total pages"
							}
							"pagination.has_next": {
								rule: "equals false"
								why:  "No more pages available"
							}
						}
					}

					tags: ["pagination", "edge-case"]
					requires: []
					notes: "Returning 200 with empty results is better UX than 404"
				},
			]
		},
		{
			name: "Pagination with Filtering and Sorting"

			description: """
				Demonstrates how pagination interacts with filtering
				and sorting parameters. Pagination metadata should
				reflect the filtered/sorted dataset, not the full dataset.
				"""

			behaviors: [
				{
					name:   "list-products-filtered"
					intent: "Pagination works with filtering"

					request: {
						method: "GET"
						path:   "/products"
						query: {
							category: "electronics"
							offset:   "0"
							limit:    "20"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						checks: {
							"products": {
								rule: "array"
								why:  "Returns filtered products"
							}
							"pagination.total": {
								rule: "integer >= 0"
								why:  "Total reflects filtered count, not all products"
							}
							"filters.category": {
								rule: "equals electronics"
								why:  "Echoes applied filter"
							}
						}
					}

					tags: ["pagination", "filtering"]
					requires: []
					notes: """
						The pagination.total field should reflect the count of
						items matching the filter, not the total count of all items.
						"""
				},
				{
					name:   "list-products-sorted"
					intent: "Pagination works with sorting"

					request: {
						method: "GET"
						path:   "/products"
						query: {
							sort:   "price"
							order:  "desc"
							offset: "0"
							limit:  "10"
						}
						headers: {}
						body: {}
					}

					response: {
						status: 200
						headers: {}

						checks: {
							"products": {
								rule: "array"
								why:  "Returns sorted products"
							}
							"sort.field": {
								rule: "equals price"
								why:  "Echoes sort field"
							}
							"sort.order": {
								rule: "equals desc"
								why:  "Echoes sort order"
							}
						}
					}

					tags: ["pagination", "sorting"]
					requires: []
					notes: """
						When paginating sorted results, the sort order must
						be stable across requests. Use a secondary sort key
						(like ID) to ensure deterministic ordering.
						"""
				},
			]
		},
	]

	rules: [
		{
			name:        "consistent-error-format"
			description: "All errors return structured error objects"

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
			name:        "content-type-json"
			description: "All responses must have Content-Type header"

			check: {
				header_must_exist: "Content-Type"
			}
		},
		{
			name:        "pagination-metadata-required"
			description: "All list endpoints must include pagination metadata"

			when: {
				status: "= 200"
				path:   "matches /products|/posts|/users/"
			}

			check: {
				fields_must_exist: ["pagination"]
			}
		},
	]

	anti_patterns: [
		{
			name:        "inconsistent-pagination-keys"
			description: "Don't mix pagination parameter names"

			bad_example: {
				query: {
					offset: "0"
					page:   "1"
				}
			}

			good_example: {
				query: {
					offset: "0"
					limit:  "20"
				}
			}

			why: """
				Mixing offset/limit with page/page_size is confusing.
				Choose one pagination pattern per endpoint and stick to it.
				"""
		},
		{
			name:        "1-indexed-offset"
			description: "Offset should start at 0, not 1"

			bad_example: {
				pagination: {
					offset: 1
					limit:  20
				}
			}

			good_example: {
				pagination: {
					offset: 0
					limit:  20
				}
			}

			why: "Offset is a count of items to skip, so 0 means skip nothing"
		},
		{
			name:        "missing-total-count"
			description: "Include total count for UI pagination controls"

			bad_example: {
				pagination: {
					page:      1
					page_size: 25
				}
			}

			good_example: {
				pagination: {
					page:        1
					page_size:   25
					total_pages: 10
					total_items: 250
				}
			}

			why: """
				Without total counts, clients cannot render pagination
				controls (e.g., "Showing 1-25 of 250 results").
				"""
		},
		{
			name:        "null-for-absent-page"
			description: "Use null for absent previous/next page, not 0 or -1"

			bad_example: {
				pagination: {
					previous_page: 0
					next_page:     -1
				}
			}

			good_example: {
				pagination: {
					previous_page: null
					next_page:     null
				}
			}

			why: "Null clearly indicates absence; 0 or -1 could be confused with valid pages"
		},
		{
			name:        "exposing-cursor-internals"
			description: "Cursors should be opaque tokens, not database IDs"

			bad_example: {
				pagination: {
					next_cursor: "12345"
				}
			}

			good_example: {
				pagination: {
					next_cursor: "eyJpZCI6IjEyMzQ1IiwidCI6MTcwNTMxNjk0MH0="
				}
			}

			why: """
				Exposing raw IDs allows clients to manipulate cursors,
				leading to security and data leakage issues. Use opaque
				base64-encoded tokens instead.
				"""
		},
	]

	ai_hints: {
		implementation: {
			suggested_stack: ["Go", "Chi router", "PostgreSQL", "Redis (cursor storage)"]
		}

		entities: {
			product: {
				fields: {
					id:       "string, prefixed 'prod_'"
					name:     "string"
					price:    "float64"
					category: "string"
				}
			}
			post: {
				fields: {
					id:         "string, prefixed 'post_'"
					title:      "string"
					created_at: "timestamp, for cursor-based sorting"
				}
			}
			user: {
				fields: {
					id:    "string, prefixed 'usr_'"
					email: "string, unique"
					name:  "string"
				}
			}
		}

		patterns: {
			offset_limit: {
				pros: [
					"Simple to implement",
					"Can jump to any page",
					"Easy to understand",
				]
				cons: [
					"Can miss or duplicate items if data changes",
					"Slow for large offsets (database must scan and skip rows)",
					"Not suitable for real-time data",
				]
				use_when: "Small, relatively static datasets"
			}
			cursor_based: {
				pros: [
					"Consistent results even when data changes",
					"Performant for large datasets (uses indexed WHERE clause)",
					"Great for infinite scroll",
				]
				cons: [
					"Cannot jump to arbitrary pages",
					"More complex to implement",
					"Cursors need to be stored (or signed to prevent tampering)",
				]
				use_when: "Real-time feeds, infinite scroll, large datasets"
			}
			page_based: {
				pros: [
					"Familiar to users (numbered pages)",
					"Can jump to any page",
					"Shows progress (page 3 of 10)",
				]
				cons: [
					"Same consistency issues as offset/limit",
					"Requires calculating total count (expensive)",
				]
				use_when: "Traditional web UIs with page number controls"
			}
		}

		security: {
			rate_limiting:    "100 requests per minute per IP"
			cursor_ttl:       "1 hour (prevent reuse of old cursors)"
			cursor_signature: "HMAC-SHA256 to prevent tampering"
		}

		pitfalls: [
			"Don't forget to validate pagination parameters (negative offset, zero page)",
			"Don't return 404 for page beyond total - return 200 with empty array",
			"Don't expose database IDs in cursors - use opaque tokens",
			"Don't forget to apply filters/sorting when calculating total count",
			"Don't use OFFSET for large values - switch to cursor-based for better performance",
			"Don't forget to include stable sort order (add ID as secondary sort key)",
			"Don't mix pagination patterns in the same endpoint",
		]
	}
}
