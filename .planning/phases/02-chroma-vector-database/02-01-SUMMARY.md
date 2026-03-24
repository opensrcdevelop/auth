---
phase: 02-chroma-vector-database
plan: "01"
subsystem: ai-chatbi
tags: [chroma, vectorstore, spring-ai, milvus]

# Dependency graph
requires:
  - phase: 01-planning
    provides: Research completed, interfaces defined
provides:
  - Chroma vector database support via Spring AI ChromaApi
  - Configuration-driven switch between Milvus and Chroma
affects: [ai-chatbi, vectorstore]

# Tech tracking
tech-stack:
  added: [spring-ai-chroma-store:1.0.0]
  patterns:
    - Configuration-based switching via @ConditionalOnProperty
    - Strategy pattern for vector database implementations

key-files:
  created:
    - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/vectorstore/chroma/ChromaConfigProperties.java
    - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/vectorstore/chroma/ChromaConfig.java
    - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/SampleSqlChromaVectorStoreServiceImpl.java
  modified:
    - ai-chatbi/build.gradle
    - auth-server/src/main/resources/application-dev.properties

key-decisions:
  - "使用 spring-ai-chroma-store:1.0.0 而非更高版本以避免依赖冲突"
  - "ChromaApi 包路径为 org.springframework.ai.chroma.vectorstore 而非 org.springframework.ai.chroma.api"
  - "QueryRequest 使用3参数构造函数 (float[], Integer, Map) 而非4参数版本"
  - "Chroma 相似度计算: similarity = 1 - distance (余弦距离)"

patterns-established:
  - "Pattern: 条件化 Bean 注册 - @ConditionalOnProperty(havingValue=\"chroma\") 激活 Chroma 配置"
  - "Pattern: 配置属性类 - @ConfigurationProperties(prefix=\"chroma\") 绑定配置"
  - "Pattern: 向量数据库抽象 - 实现 SampleSqlVectorStoreService 接口"

requirements-completed: [VDB-01, VDB-02, VDB-03, VDB-04]

# Metrics
duration: 562s
completed: 2026-03-24
---

# Phase 2: Chroma 向量数据库支持 - Plan 01 Summary

**Chroma 向量数据库集成实现，通过 vectorstore.type 配置在 Milvus 和 Chroma 之间切换**

## Performance

- **Duration:** 9 min 22s
- **Started:** 2026-03-24T14:26:38Z
- **Completed:** 2026-03-24T14:35:40Z
- **Tasks:** 5
- **Files modified:** 6

## Accomplishments

- 添加 spring-ai-chroma-store 依赖
- 创建 ChromaConfigProperties 配置属性类（endpoint, tenantName, databaseName, apiKey 等）
- 创建 ChromaConfig 配置类，使用 @ConditionalOnProperty 条件化激活
- 实现 SampleSqlChromaVectorStoreServiceImpl，完成 CRUD 操作
- 配置 vectorstore.type 和 chroma.* 配置项

## Task Commits

Each task was committed atomically:

1. **Task 1: 添加 spring-ai-chroma-store 依赖** - `6b6908a` (feat)
2. **Task 2: 创建 ChromaConfigProperties 配置属性类** - `9707c87` (feat)
3. **Task 3: 创建 ChromaConfig 配置类** - `52eff5b` (feat)
4. **Task 4: 创建 SampleSqlChromaVectorStoreServiceImpl 实现类** - `91816cb` (feat)
5. **Task 5: 添加 vectorstore.type 和 chroma.* 配置** - `456f075` (feat)

**Plan metadata:** `4852897` (fix: 修复 Chroma API 包路径和构造函数调用错误)

## Files Created/Modified

- `ai-chatbi/build.gradle` - 添加 spring-ai-chroma-store:1.0.0 依赖
- `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/vectorstore/chroma/ChromaConfigProperties.java` - Chroma 配置属性类
- `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/vectorstore/chroma/ChromaConfig.java` - Chroma 配置类，@ConditionalOnProperty(havingValue="chroma")
- `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/SampleSqlChromaVectorStoreServiceImpl.java` - Chroma 实现类，实现 SampleSqlVectorStoreService 接口
- `auth-server/src/main/resources/application-dev.properties` - 添加 vectorstore.type 和 chroma.* 配置

## Decisions Made

- 使用 spring-ai-chroma-store:1.0.0 而非更高版本以避免依赖冲突（spring-ai-chroma-store:1.0.0 依赖 spring-ai-vector-store:1.0.0 -> 1.1.2）
- ChromaApi 包路径为 org.springframework.ai.chroma.vectorstore 而非 org.springframework.ai.chroma.api
- QueryRequest 使用3参数构造函数 (float[], Integer, Map) 而非4参数版本
- Chroma 相似度计算: similarity = 1 - distance (余弦距离转相似度)

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] Chroma API 包路径错误**
- **Found during:** Task 3/4 (ChromaConfig and SampleSqlChromaVectorStoreServiceImpl)
- **Issue:** Research 文档中的包路径 `org.springframework.ai.chroma.api.ChromaApi` 不存在，实际为 `org.springframework.ai.chroma.vectorstore.ChromaApi`
- **Fix:** 修改所有 import 语句使用正确的包路径
- **Files modified:** ChromaConfig.java, SampleSqlChromaVectorStoreServiceImpl.java
- **Verification:** `./gradlew :ai-chatbi:compileJava` 通过
- **Committed in:** `4852897` (fix commit)

**2. [Rule 3 - Blocking] QueryRequest 构造函数参数类型不匹配**
- **Found during:** Task 4 (SampleSqlChromaVectorStoreServiceImpl)
- **Issue:** 调用 `new QueryRequest(float[], Integer, Map, List)` 失败，float[] 无法转换为 List<float[]>
- **Fix:** 改用3参数构造函数 `QueryRequest(float[], Integer, Map)`
- **Files modified:** SampleSqlChromaVectorStoreServiceImpl.java
- **Verification:** `./gradlew :ai-chatbi:compileJava` 通过
- **Committed in:** `4852897` (fix commit)

---

**Total deviations:** 2 auto-fixed (both Rule 3 - blocking)
**Impact on plan:** 所有自动修复都是正确性必需，不影响计划范围。

## Issues Encountered

- Research 文档中的 Chroma API 包路径与实际库不符（文档过期）
- Spring AI 1.0.0 和 1.1.2 版本 API 差异导致构造函数调用错误

## User Setup Required

**External services require manual configuration.** See [02-01-USER-SETUP.md](./02-01-USER-SETUP.md) for:
- Chroma 服务器启动和配置
- 环境变量配置示例
- 验证命令

## Next Phase Readiness

- Chroma 向量数据库支持已实现，配置切换机制就绪
- 需要测试环境运行完整验证 ChatBI 功能
- Phase 02 的后续 Plan 可继续实现其他向量数据库支持

---
*Phase: 02-chroma-vector-database*
*Plan: 01*
*Completed: 2026-03-24*
