# Phase 3: User Request Submission - Context

**Gathered:** 2026-04-12
**Status:** Ready for planning

<domain>
## Phase Boundary

用户提交权限申请，自动批准的权限即时生效。

核心能力：
1. POST /user-center/permissions/requests 提交权限申请（支持批量）
2. 自动批准逻辑：开启自动批准的权限提交后立即写入 t_authorize
3. 非自动批准的权限状态为 PENDING
4. 提交时记录完整审计日志

</domain>

<decisions>
## Implementation Decisions

### 申请理由
- **D-01:** 理由必填，且整个申请共用一个理由（`PermissionRequest.reason`）
- 不需要为每个权限条目单独填写理由
- `PermissionRequestItem` 不增加 reason 字段

### 自动批准逻辑
- **D-02:** 新建独立 `PermissionAutoApproveService`
  - 判断权限是否配置了自动批准（查询 `t_permission_auto_approve` 表）
  - 开启自动批准时：写入 `t_authorize` 表
  - 不新建 Service 时复用性差，AOP 过于复杂

### 事务边界
- **D-03:** 申请提交 + 授权写入 + 状态更新在同一个 `@Transactional` 中
  - 任何一步失败全部回滚，保证原子性
  - 自动批准权限的写入和 PENDING 状态记录的创建在同一事务

### 批量申请事务
- **D-04:** 全部成功或全部回滚
  - 批量申请多个权限时，任何一个权限处理失败整个申请回滚
  - 不存在同一申请单内部分权限成功部分 PENDING 的中间状态

### 重复申请检查
- **D-05:** 不允许重复申请
  - 提交申请前检查用户是否有同一权限的 PENDING 或 AUTO_APPROVED 申请记录
  - 如有则拒绝申请，返回错误提示
  - 已 APPROVED / REJECTED 的历史申请不受限制（可重新申请）

### 审计日志
- **D-06:** 使用 `@Audit` 注解记录申请操作
  - 审计内容：申请人、申请时间、申请的权限列表
  - 审计类型：USER_OPERATION 或 SYS_OPERATION（参考 AuthorizeServiceImpl）
  - 资源类型：`ResourceType.PERMISSION_REQUEST`

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### 权限申请实体
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/entity/permission/request/PermissionRequest.java` — 申请主表实体
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/entity/permission/request/PermissionRequestItem.java` — 申请权限项实体
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/entity/permission/request/PermissionAutoApprove.java` — 自动批准配置实体

### 授权服务
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/service/auth/AuthorizeService.java` — 授权服务接口
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/service/auth/impl/AuthorizeServiceImpl.java` — authorize 方法实现（含 @CacheEvict、@Transactional）
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/dto/auth/AuthorizeRequestDto.java` — 授权请求 DTO

### 数据访问
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/repository/permission/request/PermissionRequestRepository.java` — 申请 Repository
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/mapper/permission/request/PermissionAutoApproveMapper.java` — 自动批准 Mapper

### 审计
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/service/auth/impl/AuthorizeServiceImpl.java` — @Audit 注解使用参考
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/service/auth/impl/WebAuthnServiceImpl.java` — 审计上下文设置参考

### 工具类
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/util/AuthUtil.java` — `getCurrentUserId()` 获取当前用户 ID

### Phase 1-2 上下文
- `.planning/phases/01-foundation/01-CONTEXT.md` — Phase 1 决策（UUID 主键、per-tenant）
- `.planning/phases/02-user-permission-query-apis/02-CONTEXT.md` — Phase 2 决策（DTO 规范、API 路径）

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- `AuthorizeService.authorize()` — 写入 t_authorize，带 `@CacheEvict(cacheNames = CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)` 自动清除缓存
- `PermissionRequestRepository` — 已有 getById、findByUserId、findByStatus 方法
- `PermissionAutoApproveMapper` — 查询自动批准配置
- `AuthUtil.getCurrentUserId()` — 获取当前登录用户 ID

### Established Patterns
- Controller 放在 `auth-server/src/main/java/cn/opensrcdevelop/auth/controller/`
- DTO 使用 `@Schema` 注解，带 Swagger 文档
- Service 层使用 `@Transactional` 注解
- `@Audit` 注解使用参考：`AuthorizeServiceImpl.authorize()`

### Integration Points
- 新 Controller 挂载到 `/user-center/permissions/requests` 路径
- 自动批准调用 `AuthorizeService.authorize()` 写入 t_authorize
- 提交申请后清除用户权限缓存（`@CacheEvict` 已在 AuthorizeService 中处理）

</code_context>

<specifics>
## Specific Ideas

- 理由共用模式：用户一次申请多个权限，填写一个统一理由
- 不允许重复申请：同一用户对同一权限只能有一个 PENDING 或 AUTO_APPROVED 申请
- 自动批准配置在 Phase 8 才会被管理员修改，Phase 3 只读取现有配置

</specifics>

<deferred>
## Deferred Ideas

None — Phase 3 scope stayed focused

</deferred>

---

*Phase: 03-user-request-submission*
*Context gathered: 2026-04-12*
