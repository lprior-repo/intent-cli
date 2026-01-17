package authentication_api

import "github.com/intent-cli/intent/schema:intent"

spec: intent.#Spec & {
	name: "Authentication & Authorization API"

	description: """
		A comprehensive authentication and authorization API demonstrating
		multiple authentication methods: JWT tokens, OAuth2 flows, API keys,
		and session-based auth. Includes user registration, login, token
		refresh, password reset, role-based access control (RBAC), and
		API key management for service-to-service authentication.
		"""

	audience: "Web applications, mobile apps, and backend services"

	success_criteria: [
		"Users can register and authenticate using email/password",
		"JWT tokens are issued and validated correctly",
		"Token refresh mechanism works without re-authentication",
		"OAuth2 authorization code flow is fully functional",
		"API keys can be created and used for service authentication",
		"Role-based access control prevents unauthorized access",
		"Password reset flow is secure and functional",
		"All authentication errors are consistent and secure",
	]

	config: {
		base_url:   "http://localhost:8080"
		timeout_ms: 5000
	}

	features: [
		{
			name: "User Registration"

			description: """
				Users register with email, password, and optional profile data.
				Email format and password strength are validated. Duplicate
				emails are rejected. Registration returns a user object without
				password or sensitive data.
				"""

			behaviors: [
				{
					name:   "register-new-user"
					intent: "New user can create an account with valid credentials"

					request: {
						method: "POST"
						path:   "/auth/register"
						body: {
							email:     "alice@example.com"
							password:  "SecurePass123!"
							full_name: "Alice Smith"
						}
					}

					response: {
						status: 201
						headers: {}

						example: {
							id:         "usr_9k2m4p7q"
							email:      "alice@example.com"
							full_name:  "Alice Smith"
							role:       "user"
							created_at: "2024-01-17T10:30:00Z"
						}

						checks: {
							"id": {
								rule: "string matching usr_[a-z0-9]+"
								why:  "User IDs are prefixed random strings for security"
							}
							"email": {
								rule: "equals alice@example.com"
								why:  "Confirms email was saved correctly"
							}
							"password": {
								rule: "absent"
								why:  "SECURITY: Passwords must never appear in responses"
							}
							"role": {
								rule: "one of [\"user\", \"admin\", \"service\"]"
								why:  "Role determines access permissions"
							}
							"created_at": {
								rule: "valid ISO8601 datetime"
								why:  "Timestamp for audit trail"
							}
						}
					}

					captures: {
						alice_user_id: "response.body.id"
					}

					notes: """
						Default role is 'user'. Administrators must be created
						through a separate admin endpoint or database migration.
						"""
				},
				{
					name:   "register-duplicate-email"
					intent: "Cannot register with an email that already exists"

					requires: ["register-new-user"]

					request: {
						method: "POST"
						path:   "/auth/register"
						body: {
							email:    "alice@example.com"
							password: "DifferentPass456!"
						}
					}

					response: {
						status: 409
						headers: {}

						example: {
							error: {
								code:    "EMAIL_EXISTS"
								message: "An account with this email already exists"
								field:   "email"
							}
						}

						checks: {
							"error.code": {
								rule: "equals EMAIL_EXISTS"
								why:  "Specific error code for duplicate email"
							}
							"error.field": {
								rule: "equals email"
								why:  "Identifies which field caused the error"
							}
						}
					}
				},
				{
					name:   "register-invalid-email"
					intent: "Email format is validated before account creation"

					request: {
						method: "POST"
						path:   "/auth/register"
						body: {
							email:    "not-an-email"
							password: "SecurePass123!"
						}
					}

					response: {
						status: 400
						headers: {}

						checks: {
							"error.code": {
								rule: "equals INVALID_EMAIL"
								why:  "Validates email format"
							}
						}
					}
				},
				{
					name:   "register-weak-password"
					intent: "Password must meet strength requirements"

					request: {
						method: "POST"
						path:   "/auth/register"
						body: {
							email:    "bob@example.com"
							password: "weak"
						}
					}

					response: {
						status: 400
						headers: {}

						example: {
							error: {
								code:    "WEAK_PASSWORD"
								message: "Password must be at least 8 characters with uppercase, lowercase, number, and special character"
								field:   "password"
							}
						}

						checks: {
							"error.code": {
								rule: "equals WEAK_PASSWORD"
								why:  "Password strength is enforced"
							}
						}
					}

					notes: """
						Password requirements:
						- At least 8 characters long
						- At least one uppercase letter
						- At least one lowercase letter
						- At least one digit
						- At least one special character (!@#$%^&*)
						"""
				},
			]
		},
		{
			name: "JWT Authentication"

			description: """
				Users authenticate with email/password to receive JWT access
				and refresh tokens. Access tokens are short-lived (1 hour),
				refresh tokens are long-lived (30 days). The Authorization
				header with Bearer token is used for authenticated requests.
				"""

			behaviors: [
				{
					name:   "login-with-valid-credentials"
					intent: "Valid credentials return JWT access and refresh tokens"

					requires: ["register-new-user"]

					request: {
						method: "POST"
						path:   "/auth/login"
						body: {
							email:    "alice@example.com"
							password: "SecurePass123!"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							access_token:  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
							refresh_token: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
							token_type:    "Bearer"
							expires_in:    3600
							user: {
								id:        "usr_9k2m4p7q"
								email:     "alice@example.com"
								full_name: "Alice Smith"
								role:      "user"
							}
						}

						checks: {
							"access_token": {
								rule: "valid JWT"
								why:  "Access token for authenticated API calls"
							}
							"refresh_token": {
								rule: "valid JWT"
								why:  "Refresh token for obtaining new access tokens"
							}
							"token_type": {
								rule: "equals Bearer"
								why:  "Standard OAuth2 token type"
							}
							"expires_in": {
								rule: "integer >= 3600"
								why:  "Token valid for at least 1 hour"
							}
							"user.id": {
								rule: "equals ${alice_user_id}"
								why:  "Returns authenticated user information"
							}
						}
					}

					captures: {
						alice_access_token:  "response.body.access_token"
						alice_refresh_token: "response.body.refresh_token"
					}

					notes: """
						JWT payload includes: user_id, email, role, iat, exp.
						Access tokens expire in 1 hour, refresh tokens in 30 days.
						"""
				},
				{
					name:   "login-with-invalid-password"
					intent: "Invalid password returns generic authentication error"

					requires: ["register-new-user"]

					request: {
						method: "POST"
						path:   "/auth/login"
						body: {
							email:    "alice@example.com"
							password: "WrongPassword!"
						}
					}

					response: {
						status: 401
						headers: {}

						example: {
							error: {
								code:    "INVALID_CREDENTIALS"
								message: "Invalid email or password"
							}
						}

						checks: {
							"error.code": {
								rule: "equals INVALID_CREDENTIALS"
								why:  "Generic error prevents email enumeration"
							}
						}
					}

					notes: """
						SECURITY: Never reveal whether the email exists.
						Always return generic INVALID_CREDENTIALS for both
						unknown email and wrong password.
						"""
				},
				{
					name:   "login-with-unknown-email"
					intent: "Unknown email returns same error as wrong password"

					request: {
						method: "POST"
						path:   "/auth/login"
						body: {
							email:    "nonexistent@example.com"
							password: "AnyPassword123!"
						}
					}

					response: {
						status: 401
						headers: {}

						checks: {
							"error.code": {
								rule: "equals INVALID_CREDENTIALS"
								why:  "Prevents email enumeration attacks"
							}
						}
					}

					notes: """
						Error message and response time should be identical
						for wrong password vs. unknown email to prevent timing
						attacks.
						"""
				},
				{
					name:   "access-protected-resource"
					intent: "Valid access token allows access to protected resources"

					requires: ["login-with-valid-credentials"]

					request: {
						method: "GET"
						path:   "/api/profile"
						headers: {
							"Authorization": "Bearer ${alice_access_token}"
						}
					}

					response: {
						status: 200
						headers: {}

						checks: {
							"id": {rule: "equals ${alice_user_id}"}
							"email": {rule: "equals alice@example.com"}
						}
					}
				},
				{
					name:   "access-without-token"
					intent: "Missing authentication token returns 401"

					request: {
						method: "GET"
						path:   "/api/profile"
					}

					response: {
						status: 401
						headers: {}

						checks: {
							"error.code": {
								rule: "equals UNAUTHORIZED"
								why:  "Authentication required"
							}
						}
					}
				},
				{
					name:   "access-with-invalid-token"
					intent: "Invalid or malformed token returns 401"

					request: {
						method: "GET"
						path:   "/api/profile"
						headers: {
							"Authorization": "Bearer invalid.token.here"
						}
					}

					response: {
						status: 401
						headers: {}

						checks: {
							"error.code": {
								rule: "equals INVALID_TOKEN"
								why:  "Token validation failed"
							}
						}
					}
				},
			]
		},
		{
			name: "Token Refresh"

			description: """
				When access tokens expire, clients use refresh tokens to
				obtain new access tokens without re-authentication.
				"""

			behaviors: [
				{
					name:   "refresh-access-token"
					intent: "Valid refresh token returns new access token"

					requires: ["login-with-valid-credentials"]

					request: {
						method: "POST"
						path:   "/auth/refresh"
						body: {
							refresh_token: "${alice_refresh_token}"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							access_token: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
							token_type:   "Bearer"
							expires_in:   3600
						}

						checks: {
							"access_token": {
								rule: "valid JWT"
								why:  "New access token issued"
							}
							"token_type": {
								rule: "equals Bearer"
								why:  "Standard token type"
							}
						}
					}

					captures: {
						alice_new_access_token: "response.body.access_token"
					}

					notes: """
						Refresh tokens are NOT rotated by default. For enhanced
						security, implement refresh token rotation where each
						refresh returns a new refresh token and invalidates the old one.
						"""
				},
				{
					name:   "refresh-with-invalid-token"
					intent: "Invalid refresh token is rejected"

					request: {
						method: "POST"
						path:   "/auth/refresh"
						body: {
							refresh_token: "invalid.refresh.token"
						}
					}

					response: {
						status: 401
						headers: {}

						checks: {
							"error.code": {
								rule: "equals INVALID_REFRESH_TOKEN"
								why:  "Refresh token validation failed"
							}
						}
					}
				},
				{
					name:   "refresh-with-expired-token"
					intent: "Expired refresh token requires re-authentication"

					request: {
						method: "POST"
						path:   "/auth/refresh"
						body: {
							refresh_token: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjF9.expired"
						}
					}

					response: {
						status: 401
						headers: {}

						checks: {
							"error.code": {
								rule: "equals REFRESH_TOKEN_EXPIRED"
								why:  "User must login again"
							}
						}
					}
				},
			]
		},
		{
			name: "Password Reset"

			description: """
				Secure password reset flow using email verification.
				Users request a reset token, receive it via email,
				and use it to set a new password.
				"""

			behaviors: [
				{
					name:   "request-password-reset"
					intent: "User can request a password reset token"

					requires: ["register-new-user"]

					request: {
						method: "POST"
						path:   "/auth/password-reset/request"
						body: {
							email: "alice@example.com"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							message: "If the email exists, a password reset link has been sent"
						}

						checks: {
							"message": {
								rule: "non-empty string"
								why:  "Generic message prevents email enumeration"
							}
						}
					}

					notes: """
						SECURITY: Always return success even if email doesn't exist
						to prevent email enumeration. Reset token is sent via email
						(not in API response) and expires in 1 hour.
						"""
				},
				{
					name:   "reset-password-with-token"
					intent: "Valid reset token allows password change"

					request: {
						method: "POST"
						path:   "/auth/password-reset/confirm"
						body: {
							token:        "reset_abc123xyz"
							new_password: "NewSecurePass456!"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							message: "Password has been reset successfully"
						}

						checks: {
							"message": {
								rule: "non-empty string"
								why:  "Confirms password was changed"
							}
						}
					}

					notes: """
						After password reset:
						- All existing refresh tokens are invalidated
						- User must login again with new password
						- Reset token is single-use and immediately invalidated
						"""
				},
				{
					name:   "reset-with-invalid-token"
					intent: "Invalid or expired reset token is rejected"

					request: {
						method: "POST"
						path:   "/auth/password-reset/confirm"
						body: {
							token:        "invalid_token"
							new_password: "NewSecurePass456!"
						}
					}

					response: {
						status: 400
						headers: {}

						checks: {
							"error.code": {
								rule: "equals INVALID_RESET_TOKEN"
								why:  "Token validation failed"
							}
						}
					}
				},
				{
					name:   "reset-with-weak-password"
					intent: "New password must meet strength requirements"

					request: {
						method: "POST"
						path:   "/auth/password-reset/confirm"
						body: {
							token:        "reset_valid123"
							new_password: "weak"
						}
					}

					response: {
						status: 400
						headers: {}

						checks: {
							"error.code": {
								rule: "equals WEAK_PASSWORD"
								why:  "Password strength enforced"
							}
						}
					}
				},
			]
		},
		{
			name: "API Key Management"

			description: """
				Service-to-service authentication using API keys.
				Users can create, list, and revoke API keys. Keys
				are prefixed and used in X-API-Key header.
				"""

			behaviors: [
				{
					name:   "create-api-key"
					intent: "Authenticated user can create an API key"

					requires: ["login-with-valid-credentials"]

					request: {
						method: "POST"
						path:   "/api/keys"
						headers: {
							"Authorization": "Bearer ${alice_access_token}"
						}
						body: {
							name: "Production Service Key"
							permissions: ["read", "write"]
						}
					}

					response: {
						status: 201
						headers: {}

						example: {
							id:   "key_7m3n9p2q"
							key:  "sk_live_4KJh3k2j4h5k6j3h4k5j6"
							name: "Production Service Key"
							permissions: ["read", "write"]
							created_at: "2024-01-17T11:00:00Z"
							last_used:  null
						}

						checks: {
							"id": {
								rule: "string matching key_[a-z0-9]+"
								why:  "Key IDs are prefixed"
							}
							"key": {
								rule: "string matching sk_(test|live)_[A-Za-z0-9]+"
								why:  "API keys are prefixed for environment identification"
							}
							"permissions": {
								rule: "non-empty array"
								why:  "Keys have explicit permissions"
							}
						}
					}

					captures: {
						alice_api_key: "response.body.key"
						alice_key_id:  "response.body.id"
					}

					notes: """
						SECURITY: API key is shown only once during creation.
						Store it securely - it cannot be retrieved later.
						Key prefix 'sk_live_' for production, 'sk_test_' for development.
						"""
				},
				{
					name:   "list-api-keys"
					intent: "User can list their API keys (keys themselves are redacted)"

					requires: ["create-api-key"]

					request: {
						method: "GET"
						path:   "/api/keys"
						headers: {
							"Authorization": "Bearer ${alice_access_token}"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							keys: [
								{
									id:       "key_7m3n9p2q"
									name:     "Production Service Key"
									key_hint: "sk_live_...5j6"
									permissions: ["read", "write"]
									created_at: "2024-01-17T11:00:00Z"
									last_used:  "2024-01-17T11:15:00Z"
								},
							]
						}

						checks: {
							"keys": {
								rule: "non-empty array"
								why:  "User has at least one API key"
							}
							"keys[0].key_hint": {
								rule: "string matching sk_(test|live)_\\.\\.\\..*"
								why:  "Full key is redacted, only hint shown"
							}
						}
					}

					notes: """
						For security, full API keys are never returned in list
						endpoints. Only a hint (first 8 + last 3 characters) is shown.
						"""
				},
				{
					name:   "use-api-key-for-authentication"
					intent: "API key can be used instead of JWT for authentication"

					requires: ["create-api-key"]

					request: {
						method: "GET"
						path:   "/api/profile"
						headers: {
							"X-API-Key": "${alice_api_key}"
						}
					}

					response: {
						status: 200
						headers: {}

						checks: {
							"id": {rule: "equals ${alice_user_id}"}
							"email": {rule: "equals alice@example.com"}
						}
					}

					notes: """
						API keys authenticate the same as JWT tokens but are
						designed for server-to-server communication, not user sessions.
						"""
				},
				{
					name:   "revoke-api-key"
					intent: "User can revoke an API key"

					requires: ["create-api-key"]

					request: {
						method: "DELETE"
						path:   "/api/keys/${alice_key_id}"
						headers: {
							"Authorization": "Bearer ${alice_access_token}"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							message: "API key revoked successfully"
						}

						checks: {
							"message": {
								rule: "non-empty string"
								why:  "Confirms revocation"
							}
						}
					}

					notes: """
						Revoked keys are immediately invalidated and cannot be
						used for any further API calls.
						"""
				},
				{
					name:   "use-revoked-api-key"
					intent: "Revoked API key is rejected"

					requires: ["revoke-api-key"]

					request: {
						method: "GET"
						path:   "/api/profile"
						headers: {
							"X-API-Key": "${alice_api_key}"
						}
					}

					response: {
						status: 401
						headers: {}

						checks: {
							"error.code": {
								rule: "equals INVALID_API_KEY"
								why:  "Key has been revoked"
							}
						}
					}
				},
			]
		},
		{
			name: "Role-Based Access Control"

			description: """
				Access to resources is controlled by user roles.
				Regular users have limited permissions, admin users
				have full access. Role is checked on protected endpoints.
				"""

			behaviors: [
				{
					name:   "user-access-public-endpoint"
					intent: "All authenticated users can access public endpoints"

					requires: ["login-with-valid-credentials"]

					request: {
						method: "GET"
						path:   "/api/public/stats"
						headers: {
							"Authorization": "Bearer ${alice_access_token}"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							total_users:    1250
							active_users:   845
							api_calls:      1500000
							uptime_percent: 99.9
						}

						checks: {
							"total_users": {
								rule: "integer >= 0"
								why:  "User count is non-negative"
							}
						}
					}
				},
				{
					name:   "user-access-admin-endpoint"
					intent: "Regular users cannot access admin endpoints"

					requires: ["login-with-valid-credentials"]

					request: {
						method: "GET"
						path:   "/api/admin/users"
						headers: {
							"Authorization": "Bearer ${alice_access_token}"
						}
					}

					response: {
						status: 403
						headers: {}

						example: {
							error: {
								code:          "FORBIDDEN"
								message:       "Insufficient permissions to access this resource"
								required_role: "admin"
								current_role:  "user"
							}
						}

						checks: {
							"error.code": {
								rule: "equals FORBIDDEN"
								why:  "User lacks required permissions"
							}
							"error.required_role": {
								rule: "equals admin"
								why:  "Shows what role is needed"
							}
						}
					}

					notes: """
						SECURITY: Return 403 Forbidden (not 404) for resources
						that exist but user cannot access. This is more transparent
						for debugging while still secure.
						"""
				},
				{
					name:   "register-admin-user"
					intent: "Admin user can be created for testing"

					request: {
						method: "POST"
						path:   "/auth/register"
						body: {
							email:    "admin@example.com"
							password: "AdminPass123!"
							role:     "admin"
						}
						headers: {
							"X-Admin-Secret": "test-admin-creation-secret"
						}
					}

					response: {
						status: 201
						headers: {}

						checks: {
							"role": {
								rule: "equals admin"
								why:  "Admin role assigned"
							}
						}
					}

					captures: {
						admin_user_id: "response.body.id"
					}

					notes: """
						In production, admin creation requires special authentication
						(X-Admin-Secret header) or is done via database migration.
						Regular registration endpoint does not allow role specification.
						"""
				},
				{
					name:   "admin-login"
					intent: "Admin user can authenticate"

					requires: ["register-admin-user"]

					request: {
						method: "POST"
						path:   "/auth/login"
						body: {
							email:    "admin@example.com"
							password: "AdminPass123!"
						}
					}

					response: {
						status: 200
						headers: {}

						checks: {
							"user.role": {
								rule: "equals admin"
								why:  "Admin role in token"
							}
						}
					}

					captures: {
						admin_access_token: "response.body.access_token"
					}
				},
				{
					name:   "admin-access-admin-endpoint"
					intent: "Admin users can access admin endpoints"

					requires: ["admin-login"]

					request: {
						method: "GET"
						path:   "/api/admin/users"
						headers: {
							"Authorization": "Bearer ${admin_access_token}"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							users: [
								{
									id:         "usr_9k2m4p7q"
									email:      "alice@example.com"
									role:       "user"
									created_at: "2024-01-17T10:30:00Z"
								},
							]
						}

						checks: {
							"users": {
								rule: "non-empty array"
								why:  "At least one user exists"
							}
						}
					}
				},
			]
		},
		{
			name: "OAuth2 Authorization Code Flow"

			description: """
				OAuth2 authorization code flow for third-party applications.
				Applications redirect users to authorization page, user grants
				permission, application receives authorization code, exchanges
				code for access token.
				"""

			behaviors: [
				{
					name:   "oauth-authorization-request"
					intent: "Start OAuth2 flow by redirecting to authorization page"

					request: {
						method: "GET"
						path:   "/oauth/authorize"
						query: {
							client_id:     "app_client_123"
							redirect_uri:  "https://app.example.com/callback"
							response_type: "code"
							scope:         "profile email"
							state:         "random_state_xyz"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							authorization_url: "https://localhost:8080/oauth/authorize/form?request_id=req_abc123"
							request_id:        "req_abc123"
						}

						checks: {
							"authorization_url": {
								rule: "non-empty string"
								why:  "URL to present to user for authorization"
							}
							"request_id": {
								rule: "string matching req_[a-z0-9]+"
								why:  "Tracks authorization request"
							}
						}
					}

					captures: {
						oauth_request_id: "response.body.request_id"
					}

					notes: """
						In a real flow, user would be redirected to a login/consent
						page. For testing, we simulate user approval.
						"""
				},
				{
					name:   "oauth-user-approves"
					intent: "User approves the OAuth2 authorization request"

					requires: ["oauth-authorization-request"]

					request: {
						method: "POST"
						path:   "/oauth/authorize/approve"
						body: {
							request_id: "${oauth_request_id}"
							approved:   true
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							authorization_code: "code_abc123xyz"
							state:              "random_state_xyz"
							redirect_uri:       "https://app.example.com/callback"
						}

						checks: {
							"authorization_code": {
								rule: "string matching code_[a-z0-9]+"
								why:  "Single-use code for token exchange"
							}
							"state": {
								rule: "equals random_state_xyz"
								why:  "State parameter prevents CSRF attacks"
							}
						}
					}

					captures: {
						oauth_auth_code: "response.body.authorization_code"
					}

					notes: """
						Authorization code is single-use and expires in 10 minutes.
						Application exchanges this code for access token.
						"""
				},
				{
					name:   "oauth-token-exchange"
					intent: "Exchange authorization code for access token"

					requires: ["oauth-user-approves"]

					request: {
						method: "POST"
						path:   "/oauth/token"
						body: {
							grant_type:    "authorization_code"
							code:          "${oauth_auth_code}"
							client_id:     "app_client_123"
							client_secret: "app_secret_456"
							redirect_uri:  "https://app.example.com/callback"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							access_token:  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
							token_type:    "Bearer"
							expires_in:    3600
							refresh_token: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
							scope:         "profile email"
						}

						checks: {
							"access_token": {
								rule: "valid JWT"
								why:  "OAuth2 access token"
							}
							"scope": {
								rule: "equals profile email"
								why:  "Granted scopes match requested"
							}
						}
					}

					captures: {
						oauth_access_token: "response.body.access_token"
					}
				},
				{
					name:   "oauth-access-user-info"
					intent: "Use OAuth2 access token to retrieve user information"

					requires: ["oauth-token-exchange"]

					request: {
						method: "GET"
						path:   "/oauth/userinfo"
						headers: {
							"Authorization": "Bearer ${oauth_access_token}"
						}
					}

					response: {
						status: 200
						headers: {}

						example: {
							sub:   "usr_9k2m4p7q"
							email: "alice@example.com"
							name:  "Alice Smith"
						}

						checks: {
							"sub": {
								rule: "string matching usr_[a-z0-9]+"
								why:  "Subject identifier (user ID)"
							}
							"email": {
								rule: "non-empty string"
								why:  "User email in granted scope"
							}
						}
					}

					notes: """
						Response includes only fields permitted by granted scopes.
						Standard OpenID Connect UserInfo endpoint format.
						"""
				},
				{
					name:   "oauth-invalid-code"
					intent: "Invalid or expired authorization code is rejected"

					request: {
						method: "POST"
						path:   "/oauth/token"
						body: {
							grant_type:    "authorization_code"
							code:          "code_invalid"
							client_id:     "app_client_123"
							client_secret: "app_secret_456"
							redirect_uri:  "https://app.example.com/callback"
						}
					}

					response: {
						status: 400
						headers: {}

						checks: {
							"error": {
								rule: "equals invalid_grant"
								why:  "OAuth2 standard error code"
							}
						}
					}
				},
			]
		},
	]

	rules: [
		{
			name:        "no-sensitive-data"
			description: "Passwords, secrets, and keys must never appear in responses"

			check: {
				body_must_not_contain: [
					"password",
					"secret",
					"private_key",
					"client_secret",
				]
			}

			example: {
				id:    "usr_123"
				email: "user@example.com"
			}
		},
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
			name:        "content-type-json"
			description: "All responses return JSON content type"

			check: {
				header_must_exist: "Content-Type"
			}
		},
		{
			name:        "bearer-token-format"
			description: "Authorization header must use Bearer scheme"

			when: {
				method: "GET"
				path:   "/api/.*"
			}

			check: {
				header_must_exist: "Authorization"
			}
		},
	]

	anti_patterns: [
		{
			name:        "password-in-response"
			description: "NEVER return password in any response, even hashed"

			bad_example: {
				id:       "usr_123"
				email:    "user@example.com"
				password: "$2b$10$abcdef..."
			}

			good_example: {
				id:    "usr_123"
				email: "user@example.com"
			}

			why: """
				Even hashed passwords should not be exposed in API responses.
				This prevents offline brute-force attacks and information leakage.
				"""
		},
		{
			name:        "email-enumeration"
			description: "Login errors must not reveal if email exists"

			bad_example: {
				error: {
					code:    "USER_NOT_FOUND"
					message: "No account with this email"
				}
			}

			good_example: {
				error: {
					code:    "INVALID_CREDENTIALS"
					message: "Invalid email or password"
				}
			}

			why: """
				Revealing whether an email exists allows attackers to enumerate
				valid user accounts. Always return generic authentication errors.
				"""
		},
		{
			name:        "sequential-user-ids"
			description: "User IDs should not be predictable sequential integers"

			bad_example: {
				id: 12345
			}

			good_example: {
				id: "usr_7k9m2p4q"
			}

			why: """
				Sequential IDs reveal business metrics (total users) and enable
				enumeration attacks. Use prefixed random strings instead.
				"""
		},
		{
			name:        "long-lived-access-tokens"
			description: "Access tokens should be short-lived"

			bad_example: {
				access_token: "token123"
				expires_in:   2592000
			}

			good_example: {
				access_token:  "token123"
				refresh_token: "refresh456"
				expires_in:    3600
			}

			why: """
				Long-lived access tokens (30 days) increase the window for
				token theft. Use short-lived access tokens (1 hour) with
				refresh tokens for better security.
				"""
		},
		{
			name:        "api-key-in-url"
			description: "API keys should never be passed in URL query parameters"

			bad_example: {
				url: "/api/profile?api_key=sk_live_abc123"
			}

			good_example: {
				url: "/api/profile"
				headers: {
					"X-API-Key": "sk_live_abc123"
				}
			}

			why: """
				URLs are often logged by servers, proxies, and browsers,
				exposing API keys. Use headers for credentials.
				"""
		},
	]

	ai_hints: {
		implementation: {
			suggested_stack: ["Node.js", "Express", "PostgreSQL", "Redis"]
		}

		entities: {
			user: {
				fields: {
					id:         "string, prefixed 'usr_', randomly generated (crypto.randomBytes)"
					email:      "string, unique index, validated with email-validator"
					password:   "string, hashed with bcrypt cost 12, NEVER returned"
					full_name:  "string, 1-200 chars"
					role:       "enum: user | admin | service, defaults to 'user'"
					created_at: "datetime, set on creation"
					updated_at: "datetime, set on every update"
				}
			}
			api_key: {
				fields: {
					id:          "string, prefixed 'key_', randomly generated"
					key:         "string, prefixed 'sk_live_' or 'sk_test_', hashed in database"
					key_hint:    "string, first 8 + last 3 chars for display"
					user_id:     "string, foreign key to users"
					name:        "string, human-readable key name"
					permissions: "array of strings: read, write, admin"
					created_at:  "datetime"
					last_used:   "datetime, updated on each use"
				}
			}
			oauth_client: {
				fields: {
					client_id:      "string, prefixed 'app_client_'"
					client_secret:  "string, hashed, for confidential clients"
					redirect_uris:  "array of allowed redirect URIs"
					allowed_scopes: "array of allowed scopes"
				}
			}
		}

		security: {
			password_hashing: "bcrypt with cost factor 12"
			jwt_algorithm:    "RS256 (asymmetric) or HS256 (symmetric)"
			jwt_expiry:       "access: 1 hour, refresh: 30 days"
			rate_limiting:    "100 login attempts per hour per IP, 1000 API calls per minute per user"
		}

		pitfalls: [
			"Don't return password field even if hashed",
			"Don't use sequential integer IDs for users or API keys",
			"Don't reveal whether email exists in login/password-reset errors",
			"Don't allow users to specify their own role during registration",
			"Don't store API keys in plain text - hash them like passwords",
			"Don't accept credentials in URL query parameters",
			"Don't make access tokens long-lived - use refresh tokens",
			"Don't forget to validate redirect_uri in OAuth2 flows (prevent open redirect)",
			"Don't skip state parameter in OAuth2 (prevents CSRF)",
			"Don't allow authorization codes to be reused (single-use only)",
		]
	}
}
