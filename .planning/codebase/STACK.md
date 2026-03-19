# Technology Stack

**Analysis Date:** 2026-03-19

## Languages

**Primary:**
- Java 21 - Backend services, business logic, API controllers
- TypeScript - Frontend UI components and type safety
- Vue 3 - Frontend framework for admin console

**Secondary:**
- Python - AI prompt template processing (via custom Gradle task)
- SQL - Database migrations via Flyway

## Runtime

**Environment:**
- Java 21 with Virtual Threads (`spring.threads.virtual.enabled=true`)

**Build Tool:**
- Gradle 8.11.1 - Multi-module build management
- Node.js 22.14.0 - Frontend package management (via Gradle frontend plugin)

**Package Manager:**
- npm 10.8.2 - Frontend dependencies
- Maven repositories - Backend dependencies

## Frameworks

**Core:**
- Spring Boot 3.5.9 - Application framework
- Spring Authorization Server - OAuth 2.0/OIDC implementation
- Spring Security 6.x - Authentication and authorization
- Spring Session Data Redis - Distributed session management

**Frontend:**
- Vue 3.5.13 - UI framework
- Vite 6.0.7 - Build tool and dev server
- Arco Design Vue 2.57.0 - UI component library
- Pinia 3.0.2 - State management

**Testing:**
- JUnit 5 - Unit testing framework
- AssertJ - Fluent assertions
- Playwright 1.49.1 - E2E testing
- Spring Boot Test - Integration testing

**ORM & Database:**
- MyBatis-Plus 3.5.15 - ORM layer
- Flyway - Database migrations
- Dynamic Datasource 4.5.0 - Multi-tenant data source switching

**AI Integration:**
- Spring AI 1.1.2 - AI framework abstraction
- Spring AI Anthropic - Claude model support
- Spring AI OpenAI - GPT model support
- Spring AI Ollama - Local AI model support
- Spring AI Google GenAI - Gemini model support

## Key Dependencies

**Core Infrastructure:**
- Redisson 3.52.0 - Redis client with distributed locks
- Spring Data Redis - Redis integration
- Caffeine - Local caching
- PostgreSQL Driver - Database connectivity

**Business Logic:**
- Nimbus JWT 10.3 - JWT token handling
- EasyExcel 4.0.3 - Excel import/export
- Apache POI 5.3.0 - Office document processing
- WebAuthn4j 0.31.0 - WebAuthn/Passkey support

**Monitoring & Logging:**
- Loki4j 1.6.0 - Log aggregation (Loki compatible)
- Janino - Embedded Java compiler for dynamic evaluation

**AI & Vector:**
- Milvus SDK 2.6.14 - Vector database for RAG
- gRPC 1.68.1 - RPC communication

**External Services:**
- AWS S3 SDK 2.41.34 - Object storage (S3 compatible)
- JavaMail - Email sending

**Utilities:**
- Apache Commons Lang3 - String and date utilities
- ZXing 3.5.4 - QR code and barcode generation
- IP2Region 2.7.0 - IP address geo-location
- JExL3 3.5.0 - Expression language evaluation
- UUID Creator 6.1.1 - UUID v7 generation

## Configuration

**Environment Configuration:**
- `application.yml` - Main Spring Boot configuration
- `application-dev.properties` - Development environment settings
- `application-prod.properties` - Production environment settings
- `application-authorize.properties` - Authorization settings
- `application-ai.properties` - AI model configuration

**Key Configurations:**
- Multi-tenant database: PostgreSQL with prefix-based naming (`auth_server_{tenant}`)
- Session storage: Redis
- File storage: S3 compatible (MinIO in dev, AWS S3 in prod)

**Build Configuration:**
- Gradle root build.gradle with version management
- Per-module build.gradle files
- Frontend: Vite configuration in `ui/vite.config.js`

## Platform Requirements

**Development:**
- Java 21 JDK
- Node.js 22.14.0
- PostgreSQL (local or Docker)
- Redis (local or Docker)
- SSL certificates for local HTTPS

**Production:**
- Java 21 JRE
- PostgreSQL 15+
- Redis 7+
- S3 compatible object storage
- Mail server (SMTP)
- Optional: Milvus (vector database for RAG)

---

*Stack analysis: 2026-03-19*
