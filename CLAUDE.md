# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 语言约束

**所有回答必须使用简体中文输出。** 与用户的所有交互、代码注释和文档应使用简体中文。

## 项目概述

这是一个基于 Spring Authorization Server 的企业级认证授权服务器，提供 OAuth 2.0 和 OIDC 协议实现，并扩展了授权鉴权服务。

### 技术栈

- **后端**：Java 21、Spring Boot 3.5.9、Spring Authorization Server、Spring Security
- **数据库**：PostgreSQL（多租户）、Redis（会话存储和缓存）
- **ORM**：MyBatis-Plus
- **数据库迁移**：Flyway
- **前端**：Vue 3 + Vite + TypeScript + Arco Design
- **AI 集成**：Spring AI（支持 Anthropic、OpenAI、Ollama、Google Gemini）
- **构建**：Gradle 8.x 多模块项目

## 模块架构

项目采用 Gradle 多模块结构，共 9 个模块：

### 核心模块

1. **auth-server**（可执行主应用）
   - 入口类：`cn.opensrcdevelop.auth.Application`
   - 包含 REST 控制器、安全配置、过滤器
   - 依赖：auth-biz、common、multi-tenant、auth-audit、ai-chatbi、auth-client-spring-boot-starter
   - 产出：包含嵌入式前端的可执行 JAR

2. **auth-biz**（业务逻辑层）
   - 核心业务逻辑和领域服务
   - 业务域：user、role、client、resource、identity、authorization、tenant
   - 关键功能：MFA (TOTP)、验证码、身份源集成

3. **common**（共享工具库）
   - 作为 Maven 产物发布
   - 工具类：Redis、Web、HTTP、JWT、Mail、Message、密码验证
   - AOP：自定义过期时间的缓存注解 `@CacheExpire`
   - 响应结构：`R`、`PageData`

4. **multi-tenant**（多租户支持）
   - 基于租户的动态数据源切换
   - 租户上下文管理：`TenantContextHolder`
   - AOP 租户限制：`@TenantLimit`
   - 数据库：基于前缀的表命名（如 `auth_server_{tenant}`）

5. **auth-audit**（审计日志）
   - AOP 审计追踪：`@Audit` 注解
   - Javers 集成用于对象变更追踪
   - 记录：谁、什么、何时、做了什么变更

6. **ai-chatbi**（AI 驱动的数据分析）
   - Spring AI 集成，支持对话式 BI
   - 功能：SQL 生成、Python 代码生成、数据分析、图表生成
   - 自定义 Gradle 任务：`updatePromptTemplates` - 将 markdown 提示词转换为 YAML

7. **auth-client-spring-boot-starter**（客户端 SDK）
   - 发布到 Maven 仓库的 Spring Boot Starter
   - 提供 `@Authorize` 注解用于方法级安全控制
   - 支持 SpringEL 表达式进行细粒度授权

8. **ui**（前端控制台）
   - 构建输出嵌入到 auth-server 的 `/src/main/resources/ui/`
   - Vite 开发服务器运行在 4321 端口
   - Gradle 集成任务：`assembleFrontend`

## 常用命令

### 构建

```bash
# 清理并构建所有模块（跳过测试）
./gradlew clean build -x test

# 构建特定模块
./gradlew :auth-server:build

# 构建并运行 auth-server
./gradlew :auth-server:bootRun

# 更新 AI 提示词模板
./gradlew :ai-chatbi:updatePromptTemplates

# 发布到 Maven 仓库（需要凭证）
./gradlew publish
```

### 测试

```bash
# 运行所有测试
./gradlew test

# 运行特定模块的测试
./gradlew :auth-server:test
```

### Docker 部署

```bash
# 下载 docker-compose 文件
wget https://githubraw.com/opensrcdevelop/auth/main/deploy/docker/docker-compose.yml -O docker-compose.yaml

# 启动服务
docker-compose -f docker-compose.yaml up

# 访问地址：http://localhost:6543
# 默认账号：admin / 123456
```

## 架构要点

### 多租户架构

- **数据库per租户模式**，通过动态数据源切换
- 租户识别：HTTP 头或子域名
- Flyway 迁移：`/flyway/postgre/tenant/V{version}__auth-server.sql`
- 默认租户：`master`

### 安全架构

**三个安全过滤器链**：
1. Authorization Server Security（OAuth2 端点）
2. Resource Server Security（API 端点）
3. OAuth2 Login（第三方身份提供商）

**认证流程**：
- 用户名/密码 + 图形验证码
- TOTP 多因素认证 (MFA)
- OAuth2/OIDC 社交登录（可自定义身份源）
- 记住我功能

**授权模型**：
- 用户 → 角色 → 权限层次结构
- 用户组用于批量权限分配
- 细粒度资源权限，支持条件（SpringEL）
- 授权条件支持：IP 限制、时间访问控制、自定义逻辑

### 前端构建

前端使用 Vite 构建，输出直接嵌入到 auth-server 的 resources 目录，实现单产物部署。开发时可以独立运行 Vite dev server（端口 4321）。

### AI 配置

AI 功能需要配置环境变量（参考 `.env.example`）：
- 支持的模型提供商：Anthropic、OpenAI、Ollama、Google Gemini
- 提示词模板位于 `ai-chatbi/src/main/resources/prompts/`

## 重要配置文件

- `/auth-server/src/main/resources/application.yml` - 主配置文件
- `/auth-server/src/main/resources/application-dev.properties` - 开发环境配置
- `/auth-server/src/main/resources/application-prod.properties` - 生产环境配置
- `/auth-server/src/main/resources/application-authorize.properties` - 权限定义
- `/auth-server/src/main/resources/application-ai.properties` - AI 配置
- `/auth-server/src/main/resources/logback.xml` - 日志配置

## 客户端 SDK 使用

在业务客户端中使用 `auth-client-spring-boot-starter`：

1. 添加依赖：`implementation 'cn.opensrcdevelop:auth-client-spring-boot-starter:latest'`
2. 配置 Auth Server 地址和权限
3. 使用 `@Authorize` 注解保护 API 接口

详细文档参见 `/auth-client-spring-boot-starter/README.md`

## 开发规范

项目规则文件位于 `.claude/rules/` 目录，这些规则会在 Claude Code 执行任务时自动加载：

| 规则文件 | 说明 |
|---------|------|
| `development-workflow.md` | 完整开发流程规范（从创建 worktree 到 PR 合并） |
| `git-workflow.md` | Git 工作流规范（分支策略、提交规范、PR 模板） |
| `quality-gate.md` | 质量红线规则（提交前强制检查、禁止行为清单） |

所有开发任务必须严格遵循这些规则。

## 外部文档

- Notion 指南：https://zippy-fireplace-aab.notion.site/Auth-Server-Guide-2131bf1df6e180b49026e77aade2878c

## 注意事项

- 测试在 CI/CD 中默认跳过（`-x test`）
- 项目使用 Java 21 虚拟线程（`spring.threads.virtual.enabled=true`）
- Redis 用于会话存储和缓存
- PostgreSQL 使用 Flyway 进行数据库迁移

<!-- GSD:project-start source:PROJECT.md -->
## Project

**权限申请与审批模块**

为现有认证授权服务器添加**权限申请与审批**功能。普通用户可以在用户中心申请权限（需要审批），管理员可以在控制台审批权限申请。申请被批准后，权限直接写入 `t_authorize` 表生效。

**Core Value:** 让用户能够**自助申请**已定义但尚未获得的权限，通过审批流程确保权限发放的安全性和可控性。

### Constraints

- **Tech Stack**: 现有 Java 21 + Spring Boot 3.5.9 + MyBatis-Plus + PostgreSQL
- **授权写入**: 批准的权限直接写入 `t_authorize` 表，与现有授权机制一致
- **限制条件**: 仅使用 `t_permission_exp` 中已有的表达式，不使用模板
- **审计**: 使用现有 `auth-audit` 模块的 `@Audit` 注解进行审计追踪
- **权限控制**: 使用现有 `@Authorize` 注解机制，新增审批相关权限代码
<!-- GSD:project-end -->

<!-- GSD:stack-start source:codebase/STACK.md -->
## Technology Stack

## Languages
- Java 21 - Backend development, Spring Boot application
- TypeScript 5.8.3 - Frontend Vue 3 application
- HTML/CSS/SCSS - Frontend styling
- SQL - Database migrations (Flyway)
## Runtime
- JVM with Java 21
- Virtual threads enabled (`spring.threads.virtual.enabled=true`)
- Gradle 8.11.1 (wrapper-based build)
- npm 10.8.2 (frontend)
- Lockfile: Gradle dependency lock not explicitly used, Maven repositories configured
## Frameworks
- Spring Boot 3.5.9 - Main application framework
- Spring Authorization Server 3.5.9 - OAuth2/OIDC protocol implementation
- Spring Security 6.x - Authentication and authorization
- Spring Session Data Redis - Distributed session management
- Spring Web - REST API framework
- Spring WebSocket - WebSocket support
- Vue 3.5.13 - UI framework
- Vite 6.0.7 - Build tool and dev server
- TypeScript 5.8.3 - Type-safe JavaScript
- Arco Design Vue 2.57.0 - UI component library
- Ant Design Vue 4.2.6 - Additional UI components
- Pinia 3.0.2 - State management
- Vue Router 4.5.0 - Routing
- JUnit Platform - Unit testing (via Spring Boot Test)
- Playwright 1.49.1 - E2E testing (frontend)
- Spotless 7.0.2 - Code formatting (Eclipse JDT style)
- Gradle Frontend JDK21 Plugin 10.0.0 - Frontend integration in Gradle
## Key Dependencies
- MyBatis-Plus 3.5.15 - ORM layer with PostgreSQL
- Redisson 3.52.0 - Redis client with distributed collections
- Spring AI 1.1.4 - AI model integration (Anthropic, OpenAI, Ollama, Google Gemini)
- EasyExcel 4.0.3 - Excel import/export
- Apache POI 5.3.0 - Office document handling
- WebAuthn4j 0.31.0 - Passkey/FIDO2 authentication
- AWS S3 SDK 2.41.34 - Object storage
- Milvus SDK 2.6.14 - Vector database for AI
- gRPC 1.68.1 - RPC framework for Milvus
- PostgreSQL - Primary database
- Redis - Session storage and caching
- Flyway - Database migrations
- Loki4j 1.6.0-m1 - Log aggregation
- Caffeine - Local caching
- Janino - Runtime script compilation
## Configuration
- Multiple Spring profiles: `dev`, `prod`, `authorize`, `ai`
- Property files: `application.yml`, `application-dev.properties`, `application-prod.properties`, `application-authorize.properties`, `application-ai.properties`
- Frontend env files: `.env.production` (Vite)
- Root `build.gradle` - Central version management and plugin configuration
- Module-specific `build.gradle` files
- `config/spotless/eclipse-java-style.xml` - Code style configuration
## Platform Requirements
- Java 21+
- Node.js 20+ (for frontend)
- PostgreSQL 14+ (local or Docker)
- Redis 6+ (local or Docker)
- Optional: Milvus/Chroma (for AI vector store), MinIO/S3 (for file storage)
- Java 21 runtime
- PostgreSQL 14+ database
- Redis 6+ for sessions and caching
- Optional: Milvus or Chroma for AI features
- Optional: S3-compatible storage for async tasks
- Docker/Kubernetes deployment supported
<!-- GSD:stack-end -->

<!-- GSD:conventions-start source:CONVENTIONS.md -->
## Conventions

## Naming Patterns
### Files
- Classes: PascalCase (e.g., `UserServiceImpl.java`, `PermissionController.java`)
- Interfaces: PascalCase with optional `I` prefix removed (e.g., `UserService.java` - no `I` prefix)
- Test Classes: PascalCase with `Tests` suffix (e.g., `ExpressionEngineTests.java`)
- Enums: PascalCase (e.g., `CodeEnum.java`, `PrincipalTypeEnum.java`)
- Components: PascalCase (e.g., `UserDetail.vue`)
- Utilities/Hooks: camelCase (e.g., `usePagination.ts`, `http.ts`)
- API modules: camelCase (e.g., `user.ts`, `permission.ts`)
### Directories
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/` - 业务逻辑
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/service/` - 服务层
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/repository/` - 数据访问层
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/entity/` - 实体类
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/dto/` - 数据传输对象
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/mapper/` - MyBatis Mapper
### Variables and Methods
- Fields/Variables: camelCase (e.g., `userId`, `passwordEncoder`)
- Methods: camelCase (e.g., `createUser()`, `getUserInfo()`)
- Constants: UPPER_SNAKE_CASE (e.g., `ACCOUNT_NOT_EXISTS`)
- Generic type parameters: PascalCase (e.g., `<T>`, `<T, R>`)
- Variables: camelCase (e.g., `userId`, `currentPage`)
- Functions/Hooks: camelCase (e.g., `usePagination()`, `apiRequest()`)
- Types/Interfaces: PascalCase (e.g., `UserResponseDto`)
## Code Style
### Formatting
- Line split at: 120 characters (`<setting id="org.eclipse.jdt.core.formatter.lineSplit" value="120"/>`)
- Tabulation: 4 spaces (no tabs)
- Import order: Default (no specific order enforced)
- End with newline: Enabled
- Trim trailing whitespace: Enabled
### Linting
- Spotless `7.0.2` applied to all Java modules
- Auto-format: `./gradlew spotlessApply`
- Check: `./gradlew spotlessCheck`
- Excluded: `**/generated/**`, `**/generated-sources/**`, `**/build/**`
- No ESLint/Prettier config detected in `.eslintrc*` or `.prettierrc*` files
- Build uses `vue-tsc --build --force` for type checking
### Import Organization
### Language Features
- Virtual threads enabled: `spring.threads.virtual.enabled=true`
- Use `super.method()` for parent class calls
- Use `Optional.ofNullable().orElseThrow()` pattern
- Use Lombok `@RequiredArgsConstructor` for constructor injection
- Use `@Service`, `@RestController`, `@Repository` for stereotype annotations
- Use `@Transactional` explicitly on methods modifying data
- Use `@Cacheable`, `@CacheEvict` for caching
## Error Handling
### Exception Types
### Global Exception Handler
### Error Response Codes
- `RCD0` - Success
- `RCD20000` - Operation failed
- `RCD20001` - Parameter validation failed
- `RCD40001` - Unauthorized
- `RCD40003` - Forbidden
- `RCD40004` - Not found
- `RCD40005` - Duplicate data
- `RCD50000` - Internal server error
## Logging
- `log.info()` - Business operations, startup/shutdown
- `log.warn()` - Recoverable issues, degraded functionality
- `log.error()` - Failures requiring attention
- `log.debug()` - Detailed debugging information
## Comments
### Javadoc
### Inline Comments
## Function Design
### Size Guidelines
- **Max method length:** ~50 lines (as per quality rules)
- **Max class length:** ~500 lines (as per quality rules)
- **Prefer small, focused methods** over large procedures
### Parameters
- Use DTOs for multiple related parameters
- Use primitives for simple values
- Use validation annotations (`@NotBlank`, `@Valid`)
### Return Values
- Return `R<T>` for API responses
- Return `PageData<T>` for paginated results
- Return `void` for operations with no return value
- Use `Optional<T>` for nullable results
## Module Design
### Service Layer
### Controller Layer
### Layer Dependencies
## Validation
## Security
## Testing
<!-- GSD:conventions-end -->

<!-- GSD:architecture-start source:ARCHITECTURE.md -->
## Architecture

## Pattern Overview
- Multi-module Maven/Gradle structure with clear separation of concerns
- Domain-driven design in `auth-biz` module with business domains: user, role, client, resource, identity, authorization, tenant
- Three Spring Security filter chains: Authorization Server, Resource Server, OAuth2 Login
- Multi-tenancy via dynamic data source switching in `multi-tenant` module
- AOP-based audit logging in `auth-audit` module
## Layers
- Purpose: Handle HTTP requests, security configuration, and web infrastructure
- Location: `auth-server/src/main/java/cn/opensrcdevelop/auth/`
- Contains: Controllers, Config, Filters, Security, Interceptors, Handlers
- Depends on: auth-biz (services), common (utilities)
- Key sub-packages:
- Purpose: Core business logic and domain services
- Location: `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/`
- Contains: Services, Repositories, Entities, DTOs, Components, Mappers
- Depends on: common, multi-tenant, auth-audit
- Key business domains:
- Purpose: Database access with MyBatis-Plus ORM
- Location: `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/mapper/` and `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/repository/`
- Contains: MyBatis-Plus mappers and custom repositories
- Depends on: MyBatis-Plus, multi-tenant (dynamic data source)
- Multi-tenancy: Per-tenant database via dynamic data source switching
- Purpose: Cross-module utilities, response structures, caching, security
- Location: `common/src/main/java/cn/opensrcdevelop/common/`
- Contains: Response structures (`R`, `PageData`), Redis utilities, HTTP utilities, JWT utilities, Mail utilities, Password validation
- Key features:
- Purpose: Tenant context management and dynamic data source switching
- Location: `multi-tenant/src/main/java/cn/opensrcdevelop/tenant/`
- Contains: Tenant context holder, AOP tenant limits, tenant-aware components
- Database naming: `auth_server_{tenant}` per-tenant table prefix
- Purpose: AOP-based audit logging and object change tracking
- Location: `auth-audit/src/main/java/cn/opensrcdevelop/auth/audit/`
- Contains: `@Audit` annotation, Javers integration for object changes
- Records: Who, what, when, what changed
## Data Flow
```
```
```
```
## Key Abstractions
- `R<T>`: Unified API response with success flag, code, message, data
- `PageData<T>`: Paginated response wrapper
- Examples: `cn/opensrcdevelop/common/response/R.java`
- Interface in `biz/service/{domain}/`
- Implementation in `biz/service/{domain}/impl/`
- Extends `IService<Entity>` from MyBatis-Plus
- Examples: `cn/opensrcdevelop/auth/biz/service/user/UserService.java`
- MyBatis-Plus `BaseMapper` for basic CRUD
- Custom repository interfaces in `biz/repository/{domain}/`
- Examples: `cn/opensrcdevelop/auth/biz/mapper/user/`
- Request DTOs in `biz/dto/{domain}/`
- Response DTOs in `biz/dto/{domain}/`
- Validation using `ValidationGroups`
- Examples: `cn/opensrcdevelop/auth/biz/dto/user/UserRequestDto.java`
## Entry Points
- Location: `auth-server/src/main/java/cn/opensrcdevelop/auth/Application.java`
- Triggers: `java -jar auth-server.jar` or `./gradlew :auth-server:bootRun`
- Responsibilities: Spring Boot startup, component scanning (`cn.opensrcdevelop`)
- Location: `auth-server/src/main/java/cn/opensrcdevelop/auth/config/AuthServerConfig.java`
- Triggers: Automatic on application startup via `@Configuration`
- Responsibilities: OAuth2 authorization server, JWT, password encoder, client registration
## Error Handling
- `R<T>.ok()` - Success response
- `R<T>.optFail(CodeEnum, params...)` - Business error with message code
- `R<T>.internalFail()` - System error
- Exception handling via `@RestControllerAdvice` (in common module)
## Cross-Cutting Concerns
<!-- GSD:architecture-end -->

<!-- GSD:skills-start source:skills/ -->
## Project Skills

| Skill | Description | Path |
|-------|-------------|------|
| git-worktree | Git Worktree 管理命令。提供 init、list、remove 三个子命令来管理项目 worktree。 | `.claude/skills/git-worktree/SKILL.md` |
| tasks-planning | Implements Manus-style file-based planning for complex tasks with task-session tracking. Creates task_plan.md, findings.md, and progress.md. Uses tasks.json to track task-session relationships across sessions. Includes hooks for task initialization validation and session management (externalized to separate scripts). Plan requires user approval before execution. | `.claude/skills/tasks-planning/SKILL.md` |
<!-- GSD:skills-end -->

<!-- GSD:workflow-start source:GSD defaults -->
## GSD Workflow Enforcement

Before using Edit, Write, or other file-changing tools, start work through a GSD command so planning artifacts and execution context stay in sync.

Use these entry points:
- `/gsd-quick` for small fixes, doc updates, and ad-hoc tasks
- `/gsd-debug` for investigation and bug fixing
- `/gsd-execute-phase` for planned phase work

Do not make direct repo edits outside a GSD workflow unless the user explicitly asks to bypass it.
<!-- GSD:workflow-end -->

<!-- GSD:profile-start -->
## Developer Profile

> Profile not yet configured. Run `/gsd-profile-user` to generate your developer profile.
> This section is managed by `generate-claude-profile` -- do not edit manually.
<!-- GSD:profile-end -->
