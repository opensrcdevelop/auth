# External Integrations

**Analysis Date:** 2026-03-19

## APIs & External Services

**AI Model Providers:**
- Anthropic Claude - via Spring AI Anthropic starter
  - SDK: `org.springframework.ai:spring-ai-starter-model-anthropic`
  - Config: `spring.ai.anthropic.*` properties
- OpenAI - via Spring AI OpenAI starter
  - SDK: `org.springframework.ai:spring-ai-starter-model-openai`
  - Config: `spring.ai.openai.*` properties
- Ollama - via Spring AI Ollama starter (local AI)
  - SDK: `org.springframework.ai:spring-ai-starter-model-ollama`
  - Config: `spring.ai.ollama.*` properties
- Google Gemini - via Spring AI Google GenAI starter
  - SDK: `org.springframework.ai:spring-ai-starter-model-google-genai`
  - Config: `spring.ai.gemini.*` properties

## Data Storage

**Databases:**
- PostgreSQL - Primary database for multi-tenant data
  - Connection: `jdbc:postgresql://localhost:5432/auth_server_{tenant}`
  - Driver: `org.postgresql.Driver`
  - Migration: Flyway with `classpath:flyway/postgre/master`
  - Supported drivers: MySQL, SQL Server, Oracle (included but not default)

- Redis - Session and cache storage
  - Connection: `spring.data.redis.host`, `spring.data.redis.port`
  - Password: `spring.data.redis.password`
  - Client: Redisson with Lettuce pool

**File Storage:**
- S3 Compatible (MinIO in dev, AWS S3 in prod)
  - Endpoint: `S3_ENDPOINT` env var
  - Region: `S3_REGION` env var
  - Credentials: `S3_ACCESS_KEY`, `S3_SECRET_KEY`
  - SDK: `software.amazon.awssdk:s3`
  - Used for: Async task results, file uploads

**Vector Database:**
- Milvus - Vector storage for AI RAG
  - Endpoint: `milvus.endpoint` config
  - SDK: `io.milvus:milvus-sdk-java`

## Authentication & Identity

**Auth Provider:**
- Custom Spring Authorization Server
  - OAuth 2.0 Authorization Server implementation
  - OIDC support
  - Multiple grant types: Authorization Code, Refresh Token, Client Credentials

**Social Login:**
- Spring Security OAuth2 Client
  - Configurable identity providers via `spring.security.oauth2.client.provider.*`
  - Custom identity source support

**MFA:**
- TOTP (Time-based One-Time Password)
- WebAuthn/Passkey via WebAuthn4j

**Captcha:**
- AJ-Captcha 1.3.0 - Graphical captcha
  - Type: default
  - Cache: Redis

## Monitoring & Observability

**Logging:**
- Loki4j - Logback appender for Loki aggregation
  - Config: `com.github.loki4j:loki-logback-appender`
  - Output: JSON format to Loki

**Application Performance:**
- Spring Boot Actuator (implied by Spring Boot)

## CI/CD & Deployment

**Containerization:**
- Docker - Single container deployment
- Docker Compose - Local development stack

**Registry:**
- Aliyun Container Registry
  - Image: `registry.cn-hangzhou.aliyuncs.com/opensrcdevelop/auth-server-quickstart:latest`

**Build:**
- Gradle - Multi-module build
- Maven Publishing - Client SDK distribution
  - Target: Aliyun Maven repository

## Environment Configuration

**Required env vars:**
- `REDIS_HOST` - Redis server hostname
- `REDIS_PORT` - Redis port (default: 6379)
- `REDIS_PASSWORD` - Redis authentication
- `SERVER_PORT` - Application port (default: 6543)
- `multi.tenant.db.base-url` - Database connection base URL
- `multi.tenant.db.username` - Database username
- `multi.tenant.db.password` - Database password
- `multi.tenant.db-prefix` - Database name prefix
- `multi.tenant.default-tenant` - Default tenant identifier

**AI Configuration:**
- `SPRING_AI_ANTHROPIC_API_KEY` - Anthropic API key
- `SPRING_AI_OPENAI_API_KEY` - OpenAI API key
- `SPRING_AI_OLLAMA_BASE_URL` - Ollama server URL

**File Storage:**
- `ASYNC_TASK_STORAGE_TYPE` - Storage type (local/s3)
- `S3_ENDPOINT` - S3 compatible endpoint
- `S3_ACCESS_KEY` - S3 access key
- `S3_SECRET_KEY` - S3 secret key
- `S3_BUCKET` - S3 bucket name

**Mail:**
- `MAIL_HOST` - SMTP server
- `MAIL_USERNAME` - SMTP username
- `MAIL_PASSWORD` - SMTP password
- `MAIL_PORT` - SMTP port

## Webhooks & Callbacks

**Incoming:**
- OAuth2 Authorization callback: `/login/oauth2/code/*`
- WebAuthn attestation: `/api/v1/webauthn/attestation/options`
- WebAuthn assertion: `/api/v1/webauthn/authenticate/options`

**Outgoing:**
- OAuth2 token introspection endpoint
- Custom WebSocket endpoint (`/ws/**`) via STOMP

---

*Integration audit: 2026-03-19*
