# Architecture

**Analysis Date:** 2026-03-19

## Pattern Overview

**Overall:** Multi-module Gradle project with Spring Boot 3, following layered architecture with domain-driven design principles.

**Key Characteristics:**
- **Modular Monolith** - 9 modules (common, auth-server, auth-biz, multi-tenant, auth-audit, ai-chatbi, auth-client-spring-boot-starter, ui) combined into single executable
- **Multi-tenant Architecture** - Database-per-tenant pattern with dynamic datasource switching
- **Three Security Filter Chains** - Authorization Server, Resource Server, and OAuth2 Login
- **Event-driven Audit** - AOP-based audit logging with Javers integration
- **AI-Ready** - Spring AI integration for conversational BI

## Layers

**Presentation Layer (auth-server):**
- Purpose: REST controllers, security configuration, HTTP filters
- Location: `auth-server/src/main/java/cn/opensrcdevelop/auth/`
- Contains: Controllers, SecurityFilterChain, AuthenticationProvider, Handler
- Depends on: auth-biz, common
- Used by: External clients (SPA, mobile, API consumers)

**Business Logic Layer (auth-biz):**
- Purpose: Core business logic and domain services
- Location: `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/`
- Contains: Service implementations, DTOs, Entities, Mappers, Repositories
- Depends on: common, multi-tenant, auth-audit
- Used by: auth-server controllers

**Domain Modules:**
- **multi-tenant** - Tenant context management, AOP tenant limitation
  - Location: `multi-tenant/src/main/java/cn/opensrcdevelop/tenant/`
  - Key classes: `TenantContextHolder`, `@TenantLimit` annotation

- **auth-audit** - Audit logging with AOP
  - Location: `auth-audit/src/main/java/cn/opensrcdevelop/auth/audit/`
  - Key classes: `@Audit` annotation, `AuditAspect`

- **ai-chatbi** - AI-driven data analysis
  - Location: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/`
  - Agents: ChatAgent, SqlAgent, AnalyzeAgent, ChartAgent

- **common** - Shared utilities and response structures
  - Location: `common/src/main/java/cn/opensrcdevelop/common/`
  - Key classes: `R<T>` response, `PageData`, `@CacheExpire` AOP

**Client SDK (auth-client-spring-boot-starter):**
- Purpose: SDK for business clients to integrate with Auth Server
- Location: `auth-client-spring-boot-starter/src/main/java/cn/opensrcdevelop/auth/client/`
- Key feature: `@Authorize` annotation for method-level security

## Data Flow

**Authentication Flow:**

1. Client sends credentials to `/oauth2/token` endpoint
2. `ResourceOwnerPasswordAuthenticationProvider` validates credentials
3. MFA check via `MfaValidFilter` if enabled (TOTP)
4. `DbOAuth2AuthorizationService` stores authorization
5. JWT token returned to client
6. Client includes token in Authorization header for API calls

**Multi-tenant Request Flow:**

1. Request arrives with tenant header (`X-Tenant-ID`)
2. `TenantContextFilter` extracts tenant and sets `TenantContextHolder`
3. `MultiTenantDataSource` switches to tenant database
4. Business logic executes with tenant context
5. Response returned, context cleared

**Authorization Check Flow:**

1. Resource request with token
2. `ResourceServerSecurityFilterChain` validates JWT
3. `PermissionService` checks user permissions
4. `@Authorize` annotation on method triggers authorization
5. Expression evaluation via SpringEL (IP, time, custom conditions)

## Key Abstractions

**Response Wrapper:**
- Purpose: Standard API response format
- Examples: `common/src/main/java/cn/opensrcdevelop/common/response/R.java`
- Pattern: `R.success(data)`, `R.fail(errorCode)`

**Page Data:**
- Purpose: Paginated list response
- Examples: `common/src/main/java/cn/opensrcdevelop/common/response/PageData.java`
- Pattern: `PageData.of(list, total, page, size)`

**Base Entity:**
- Purpose: Common fields for all entities
- Examples: `common/src/main/java/cn/opensrcdevelop/common/entity/BaseEntity.java`
- Fields: id, createdAt, createdBy, updatedAt, updatedBy, deleted

**Service Interface/Implementation:**
- Purpose: Business logic abstraction
- Pattern: `XxxService` (interface) + `XxxServiceImpl` (implementation)
- Location: `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/service/`

## Entry Points

**Main Application:**
- Location: `auth-server/src/main/java/cn/opensrcdevelop/auth/Application.java`
- Triggers: `java -jar auth-server.jar` or `./gradlew :auth-server:bootRun`
- Responsibilities: Spring Boot startup, component scanning, Flyway migration

**REST Controllers:**
- Location: `auth-server/src/main/java/cn/opensrcdevelop/auth/controller/`
- Key endpoints:
  - `UserController` - `/api/v1/users`
  - `RoleController` - `/api/v1/roles`
  - `ClientController` - `/api/v1/clients`
  - `PermissionController` - `/api/v1/permissions`

**OAuth2 Endpoints:**
- Location: Built-in Spring Authorization Server
- Endpoints: `/oauth2/authorize`, `/oauth2/token`, `/oauth2/jwks`

**WebSocket:**
- Location: `auth-server/src/main/java/cn/opensrcdevelop/auth/config/WebSocketConfig.java`
- Endpoint: `/ws`

## Error Handling

**Strategy:** Centralized exception handling with AOP

**Patterns:**
- `BizException` - Business logic errors (from common module)
- `ValidationException` - Input validation errors
- `ServerException` - Server errors
- Global handler: `common/src/main/java/cn/opensrcdevelop/common/aop/RestExceptionHandler.java`

## Cross-Cutting Concerns

**Logging:** SLF4J with Logback (`auth-server/src/main/resources/logback.xml`)
- Trace ID via `TraceFilter`
- Structured JSON logging

**Validation:** Bean Validation + Custom validators
- Location: `common/src/main/java/cn/opensrcdevelop/common/validation/`
- Custom annotations: `@EnumValue`, `@NotBlankStr`, `@RequiredWhen`

**Authentication:** Spring Security with multiple providers
- Resource Owner Password
- Email Code
- TOTP (MFA)
- Passkey/WebAuthn
- OAuth2 Login (Google, GitHub, etc.)

**Caching:** Redis with custom cache manager
- Custom `@CacheExpire` annotation for TTL per cache entry
- Location: `common/src/main/java/cn/opensrcdevelop/common/cache/`

**Transaction Management:** Spring @Transactional
- Tenant context propagation via `TransactionSynchronization`

---

*Architecture analysis: 2026-03-19*
