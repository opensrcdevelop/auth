# Codebase Structure

**Analysis Date:** 2026-03-19

## Directory Layout

```
auth/                                    # Project root
├── auth-server/                         # Main application (executable)
├── auth-biz/                           # Business logic layer
├── common/                             # Shared utilities
├── multi-tenant/                       # Multi-tenancy support
├── auth-audit/                         # Audit logging
├── ai-chatbi/                          # AI BI features
├── auth-client-spring-boot-starter/    # Client SDK
├── ui/                                 # Frontend (Vue 3)
├── build.gradle                        # Root build config
├── settings.gradle                     # Module definitions
└── gradle/                             # Gradle wrapper
```

## Directory Purposes

**auth-server (可执行主应用):**
- Purpose: Main Spring Boot application with REST controllers and security configuration
- Contains:
  - `controller/` - REST API endpoints (User, Role, Client, Permission, etc.)
  - `config/` - Spring configurations (Security, Redis, MyBatis, WebSocket)
  - `authentication/` - Custom authentication providers (password, email, passkey)
  - `filter/` - Security filters (MFA, tenant, captcha)
  - `handler/` - Authentication handlers (login success/failure, logout)
  - `interceptor/` - Request interceptors (trace, OAuth2 context)
  - `security/` - Security support classes
  - `support/` - Application runners, token generators
- Key files:
  - `Application.java` - Entry point
  - `application.yml` - Configuration

**auth-biz (业务逻辑层):**
- Purpose: Core business logic, domain services, data access
- Contains:
  - `service/` - Business service implementations (user, role, client, auth, etc.)
  - `mapper/` - MyBatis-Plus mappers
  - `entity/` - Domain entities
  - `dto/` - Data transfer objects (request/response)
  - `repository/` - Custom repository implementations
  - `component/` - Business components (Redis cache, auth server, scheduled tasks)
  - `constants/` - Business constants
- Business domains:
  - `user/` - User management, login, TOTP
  - `role/` - Role management
  - `client/` - OAuth2 client management
  - `resource/` - Resource/permission management
  - `identity/` - Third-party identity providers
  - `authorization/` - Authorization conditions
  - `system/` - System settings, mail, password policies
  - `audit/` - Audit logs
  - `asynctask/` - Async task execution

**common (共享工具库):**
- Purpose: Shared utilities, response structures, validation
- Contains:
  - `response/` - `R<T>`, `PageData`
  - `exception/` - `BizException`, `ValidationException`, `ServerException`
  - `util/` - `RedisUtil`, `JwtUtil`, `HttpUtil`, `MailUtil`, `WebUtil`
  - `validation/` - Custom validators and annotations
  - `security/password/` - Password rules and strength checkers
  - `cache/aop/` - `@CacheExpire` annotation with AOP
  - `filter/` - `RestFilter`, `TraceFilter`
  - `interceptor/` - `RestResponseInterceptor`
  - `aop/` - `RestResponseHandler`, `RestExceptionHandler`
- Note: Published as Maven artifact for external use

**multi-tenant (多租户支持):**
- Purpose: Database-per-tenant architecture
- Contains:
  - `service/` - Tenant management
  - `entity/` - Tenant entity
  - `mapper/` - Tenant mapper
  - `support/` - `TenantContextHolder`, `TenantHelper`, `TenantContext`
  - `aop/` - `TenantLimitAspect` for @TenantLimit annotation
  - `annoation/` - `@TenantLimit` annotation
  - `dto/` - Tenant DTOs
- Key approach: Dynamic datasource switching based on tenant prefix

**auth-audit (审计日志):**
- Purpose: AOP-based audit tracking
- Contains:
  - `annotation/` - `@Audit` annotation
  - `aop/` - `AuditAspect` for intercepting annotated methods
  - `entity/` - `AuditLog`, `ObjChangeLog`
  - `context/` - `AuditContext`
  - `compare/` - Object comparison utilities
  - `event/` - Audit events
  - `util/` - Audit utilities
- Integration: Javers for object change tracking

**ai-chatbi (AI 驱动的数据分析):**
- Purpose: Conversational BI with AI
- Contains:
  - `agent/` - ChatAgent, SqlAgent, AnalyzeAgent, ThinkAnswerAgent
  - `chat/` - Chat context, memory, advisors, tools
  - `service/` - Chat history, data source, model provider
  - `mapper/` - MyBatis mappers
  - `entity/` - Domain entities
  - `dto/` - Request/response DTOs
  - `datasource/` - Data source management
  - `enums/` - Chat content types, question types
  - `prompt/` - Prompt templates

**auth-client-spring-boot-starter (客户端 SDK):**
- Purpose: SDK for business clients to integrate with Auth Server
- Contains:
  - `authorize/` - `@Authorize` annotation and expression handler
  - `config/` - Auto-configuration
  - `service/` - Permission service
  - `support/` - OAuth2 context, attribute customizers
  - `constants/` - API constants
- Usage: Add dependency, add `@Authorize` annotation on methods

**ui (前端控制台):**
- Purpose: Vue 3 admin console
- Contains:
  - `views/` - Page components (user, role, client, system)
  - `components/` - Reusable components
  - `api/` - API client functions
  - `router/` - Vue Router configuration
  - `store/` - Pinia state management
  - `hooks/` - Vue composables
  - `util/` - Utility functions
  - `layout/` - Layout components
- Build output: Embedded in `auth-server/src/main/resources/ui/`

## Key File Locations

**Entry Points:**
- `/auth-server/src/main/java/cn/opensrcdevelop/auth/Application.java` - Main application

**Configuration:**
- `/auth-server/src/main/resources/application.yml` - Main config
- `/auth-server/src/main/resources/application-dev.properties` - Dev overrides
- `/auth-server/src/main/resources/application-prod.properties` - Prod overrides
- `/auth-server/src/main/resources/application-ai.properties` - AI config
- `/auth-server/src/main/resources/application-authorize.properties` - Authorization rules
- `/auth-server/src/main/resources/logback.xml` - Logging config

**Database Migrations:**
- `/auth-server/src/main/resources/flyway/postgre/master/` - Master database migrations
- `/auth-server/src/main/resources/flyway/postgre/tenant/` - Tenant database migrations

**Security Configuration:**
- `/auth-server/src/main/java/cn/opensrcdevelop/auth/config/AuthServerConfig.java` - Security filter chains
- `/auth-server/src/main/java/cn/opensrcdevelop/auth/authentication/` - Custom authentication

## Naming Conventions

**Files:**
- Java classes: PascalCase (e.g., `UserService`, `UserController`)
- DTOs: `{Entity}{Request|Response}Dto` (e.g., `UserRequestDto`, `UserResponseDto`)
- Entities: PascalCase, singular (e.g., `User`, `Role`)
- Mappers: `{Entity}Mapper` (e.g., `UserMapper`)
- Services: `{Domain}Service` (interface), `{Domain}ServiceImpl` (implementation)

**Directories:**
- Java packages: lowercase with dots (e.g., `cn.opensrcdevelop.auth.biz.service.user`)
- Source directories: `src/main/java`, `src/main/resources`, `src/test/java`

**Spring Components:**
- Controllers: `{Entity}Controller` (e.g., `UserController`)
- Services: `{Domain}Service`, `{Domain}ServiceImpl`
- Repositories: `{Entity}Repository`
- Config: `{Feature}Config` (e.g., `RedisCacheConfig`)

## Where to Add New Code

**New REST API:**
1. Add DTOs in `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/dto/{domain}/`
2. Add service method in `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/service/{domain}/`
3. Add controller in `auth-server/src/main/java/cn/opensrcdevelop/auth/controller/{Domain}Controller.java`
4. Add mapper if needed in `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/mapper/{domain}/`

**New Business Domain:**
1. Create package in `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/{service|dto|entity|mapper}/{domain}/`
2. Define entity and mapper
3. Implement service interface and implementation
4. Add controller in auth-server

**New Frontend Page:**
1. Add API function in `ui/src/api/{domain}.ts`
2. Add view component in `ui/src/views/{domain}/`
3. Add route in `ui/src/router/routes.ts`
4. Add menu entry in database

**New Database Migration:**
1. Add SQL file in `auth-server/src/main/resources/flyway/postgre/{master|tenant}/V{version}__{description}.sql`

**New Utility:**
1. Add to `common/src/main/java/cn/opensrcdevelop/common/util/`
2. Add tests to `common/src/test/java/`

**New AI Feature:**
1. Add agent/tool in `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/{agent|chat|tool}/`
2. Add prompt template in `ai-chatbi/src/main/resources/prompts/`

## Special Directories

**flyway:**
- Purpose: Database migrations
- Generated: No (manually written SQL)
- Committed: Yes

**ui/assets (in auth-server):**
- Purpose: Compiled frontend bundles
- Generated: Yes (by `./gradlew :ui:assembleFrontend`)
- Committed: Yes (embedded in JAR)

**build:**
- Purpose: Gradle build outputs
- Generated: Yes
- Committed: No (in .gitignore)

---

*Structure analysis: 2026-03-19*
