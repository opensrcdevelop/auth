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

# 仅构建前端
./gradlew :ui:assembleFrontend

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

## 外部文档

- Notion 指南：https://zippy-fireplace-aab.notion.site/Auth-Server-Guide-2131bf1df6e180b49026e77aade2878c

## 注意事项

- 测试在 CI/CD 中默认跳过（`-x test`）
- 项目使用 Java 21 虚拟线程（`spring.threads.virtual.enabled=true`）
- Redis 用于会话存储和缓存
- PostgreSQL 使用 Flyway 进行数据库迁移
