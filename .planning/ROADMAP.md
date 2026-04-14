# Roadmap: 权限申请与审批模块

## Overview

为认证授权服务器实现权限申请与审批功能。用户可以申请未持有的权限并填写理由，管理员审批后可添加限制条件。批准的权限写入 t_authorize 表生效，自动批准的权限即时生效。整个流程保持完整的审计追踪。

## Phases

- [x] **Phase 1: Foundation** - 数据库迁移、实体类、Repository层
- [ ] **Phase 2: User Permission Query APIs** - 用户查看已有权限和可申请权限树
- [ ] **Phase 3: User Request Submission** - 用户提交权限申请、自动批准逻辑
- [ ] **Phase 4: User Request View APIs** - 用户查看申请记录和详情
- [ ] **Phase 5: Admin Pending List API** - 管理员查看待审批和全部申请
- [ ] **Phase 6: Admin Single Approve/Reject** - 单个申请审批、权限写入、审计
- [ ] **Phase 7: Batch Operations** - 批量批准/拒绝
- [ ] **Phase 8: Auto-Approve Configuration** - 激活 Permission 实体的 allowApply/autoApprove 字段
- [ ] **Phase 9: User Frontend UI** - 用户中心申请界面
- [ ] **Phase 10: Admin Frontend UI** - 管理员审批控制台

## Phase Details

### Phase 1: Foundation
**Goal**: 数据库表和基础数据访问层就绪
**Depends on**: Nothing (first phase)
**Requirements**: (database schema)
**Success Criteria** (what must be TRUE):
  1. Flyway 迁移脚本成功创建 t_permission_request、t_permission_request_item、t_permission_request_cond、t_permission_auto_approve 四张表
  2. 实体类 (Entity) 已创建且字段映射正确
  3. MyBatis-Plus Mapper 接口已创建
  4. Repository 层支持多租户隔离查询
**Plans**: 5 plans

Plans:
- [x] 01-01-PLAN.md — 创建 Flyway 迁移脚本 (t_permission_request, t_permission_request_item, t_permission_request_cond, t_permission_auto_approve)
- [x] 01-02-PLAN.md — 创建实体类 (PermissionRequest, PermissionRequestItem, PermissionRequestCond, PermissionAutoApprove)
- [x] 01-03-PLAN.md — 创建 Mapper 接口 (PermissionRequestMapper, PermissionRequestItemMapper, PermissionAutoApproveMapper)
- [x] 01-04-PLAN.md — 创建 Repository 类封装常用查询
- [x] 01-05-PLAN.md — 验证多租户隔离 (tenant_id 过滤)

### Phase 2: User Permission Query APIs
**Goal**: 用户可以查看自己已有的权限和可申请的权限树
**Depends on**: Phase 1
**Requirements**: PREQ-01, PREQ-02
**Success Criteria** (what must be TRUE):
  1. 用户调用 GET /api/v1/permissions/me 返回自己已有的权限列表
  2. 用户调用 GET /api/v1/permissions/available-tree 返回可申请权限树 (ResourceGroup → Resource → Permission)
  3. 权限树中用户已拥有的权限不显示申请按钮 (通过 alreadyGranted 字段标识)
**Plans**: 2 plans

Plans:
- [x] 02-01-PLAN.md — 实现 GET /api/v1/permissions/me 接口 (复用现有 getCurrentUserPermissions)
- [x] 02-02-PLAN.md — 实现 GET /api/v1/permissions/available-tree 接口 (可申请权限树)

### Phase 3: User Request Submission
**Goal**: 用户可以提交权限申请，自动批准的权限即时生效
**Depends on**: Phase 2
**Requirements**: PREQ-03, PAUT-01, PAUT-02, PAUT-03
**Success Criteria** (what must be TRUE):
  1. 用户调用 POST /user-center/permissions/requests 提交申请，支持批量权限和理由
  2. 开启自动批准的权限提交后状态立即变为 AUTO_APPROVED，权限写入 t_authorize
  3. 非自动批准的权限状态为 PENDING
  4. 提交时记录审计日志 (PAUD-01)
**Plans**: 2 plans

Plans:
- [x] 03-01-PLAN.md — 基础合约层：枚举 + DTO + ResourceType.PERMISSION_REQUEST + PermissionAutoApproveService + Repository.hasActivePendingRequest
- [x] 03-02-PLAN.md — 申请提交服务 + 控制器：PermissionRequestServiceImpl (@Transactional + @Audit) + UserCenterPermissionRequestController (POST /user-center/permissions/requests)

### Phase 4: User Request View APIs
**Goal**: 用户可以查看自己的申请记录
**Depends on**: Phase 3
**Requirements**: PREQ-04, PREQ-05
**Success Criteria** (what must be TRUE):
  1. 用户调用 GET /user-center/permissions/requests 返回申请记录列表 (状态、申请时间、审批结果)
  2. 用户调用 GET /user-center/permissions/requests/{id} 返回申请详情 (权限列表、审批结果、拒绝理由)
**Plans**: 2 plans

Plans:
- [x] 04-01-PLAN.md — 实现 GET /user-center/permissions/requests 列表接口
- [x] 04-02-PLAN.md — 实现 GET /user-center/permissions/requests/{id} 详情接口

### Phase 5: Admin Pending List API
**Goal**: 管理员可以查看待审批和全部申请
**Depends on**: Phase 4
**Requirements**: PAPR-01, PAPR-06
**Success Criteria** (what must be TRUE):
  1. 管理员调用 GET /admin/permissions/requests/pending 返回待审批列表
  2. 管理员调用 GET /admin/permissions/requests/all 返回全部申请 (支持按状态筛选)
  3. 管理员只能看到本租户的申请 (多租户隔离)
**Plans**: 1 plan

Plans:
- [x] 05-01-PLAN.md — 实现 GET /admin/permissions/requests/pending 待审批列表 + GET /admin/permissions/requests/all 全部申请

### Phase 6: Admin Single Approve/Reject
**Goal**: 管理员可以批准或拒绝单个申请，权限写入 t_authorize
**Depends on**: Phase 5
**Requirements**: PAPR-02, PAPR-04, PAPR-05, PAPR-07, PINT-01, PINT-02, PINT-03, PAUD-02
**Success Criteria** (what must be TRUE):
  1. 管理员调用 POST /admin/permissions/requests/{id}/approve 批准申请，可添加限制条件
  2. 管理员调用 POST /admin/permissions/requests/{id}/reject 拒绝申请，必须填写拒绝理由
  3. 审批人不能审批自己的申请 (自我审批拦截)
  4. 批准的权限写入 t_authorize 表后清除用户权限缓存
  5. 权限写入和状态更新在同一事务内完成
  6. 审批操作记录审计日志 (PAUD-02)
**Plans**: 4 plans

Plans:
- [x] 06-01: 创建 DTO 和扩展 Service 接口 (ApproveRequestDto + RejectRequestDto + PermissionRequestAdminService 方法声明)
- [x] 06-02: 实现 approveRequest 方法 (权限写入 t_authorize + 状态更新)
- [x] 06-03: 实现 rejectRequest 方法 + 添加 REST 端点 (关闭 Gap 1, 2, 3)
- [ ] 06-04: 集成缓存清除和事务优化 (如需要)

### Phase 7: Batch Operations
**Goal**: 管理员可以批量批准或拒绝多个申请
**Depends on**: Phase 6
**Requirements**: PAPR-03
**Success Criteria** (what must be TRUE):
  1. 管理员调用 POST /admin/permissions/requests/batch-approve 批量批准
  2. 管理员调用 POST /admin/permissions/requests/batch-reject 批量拒绝
  3. 批量操作全部成功或全部回滚 (原子性)
**Plans**: 2 plans

Plans:
- [ ] 07-01: 实现 POST /admin/permissions/requests/batch-approve 批量批准
- [ ] 07-02: 实现 POST /admin/permissions/requests/batch-reject 批量拒绝

### Phase 8: Auto-Approve Configuration
**Goal**: 管理员可以配置权限的自动批准开关
**Depends on**: Phase 6
**Requirements**: PAUT-01
**Success Criteria** (what must be TRUE):
  1. PermissionRequestDto 包含 allowApply 和 autoApprove 字段
  2. createPermission() 和 updatePermission() 方法处理 allowApply 和 autoApprove
  3. 权限列表页面显示 allowApply 和 autoApprove 开关
  4. 权限创建表单包含 allowApply 和 autoApprove 开关
**Plans**: 1 plan

Plans:
- [x] 08-01-PLAN.md — 实现 allowApply/autoApprove 字段支持（后端 DTO/Service + 前端列表/创建表单）

### Phase 9: User Frontend UI
**Goal**: 用户可以在用户中心提交申请和查看记录
**Depends on**: Phase 4
**Requirements**: PREQ-01, PREQ-02, PREQ-03, PREQ-04, PREQ-05
**Success Criteria** (what must be TRUE):
  1. 用户可以在用户中心查看自己已有的权限
  2. 用户可以查看可申请权限树，已拥有的权限不显示申请按钮
  3. 用户可以填写理由并提交权限申请
  4. 用户可以查看申请记录列表和单个申请详情
**Plans**: 3 plans

Plans:
- [ ] 09-01-PLAN.md — 实现用户已有权限展示页面
- [ ] 09-02-PLAN.md — 实现可申请权限树和申请提交页面
- [ ] 09-03-PLAN.md — 实现用户申请记录列表和详情页面

### Phase 10: Admin Frontend UI
**Goal**: 管理员可以在控制台审批申请
**Depends on**: Phase 8
**Requirements**: PAPR-01, PAPR-02, PAPR-03, PAPR-04, PAPR-05, PAPR-06, PAPR-07
**Success Criteria** (what must be TRUE):
  1. 管理员可以查看待审批列表和全部申请 (按状态筛选)
  2. 管理员可以批准单个申请，可选择限制条件
  3. 管理员可以拒绝单个申请，必须填写拒绝理由
  4. 管理员可以批量批准或拒绝申请
  5. 管理员不能审批自己的申请 (前端拦截)
**Plans**: 3 plans

Plans:
- [ ] 10-01: 实现管理员待审批列表和全部申请页面
- [ ] 10-02: 实现单个申请审批页面 (批准/拒绝、限制条件选择)
- [ ] 10-03: 实现批量审批功能

## Progress

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Foundation | 5/5 | Completed | 100% |
| 2. User Permission Query APIs | 2/2 | In Progress | - |
| 3. User Request Submission | 0/2 | Not started | - |
| 4. User Request View APIs | 0/2 | Not started | - |
| 5. Admin Pending List API | 1/1 | In Progress | - |
| 6. Admin Single Approve/Reject | 2/4 | In Progress | - |
| 7. Batch Operations | 0/2 | Not started | - |
| 8. Auto-Approve Configuration | 0/1 | Not started | - |
| 9. User Frontend UI | 0/3 | Not started | - |
| 10. Admin Frontend UI | 0/3 | Not started | - |

## Coverage

**Requirements mapped to phases:**

| Requirement | Phase | Description |
|-------------|-------|-------------|
| PREQ-01 | Phase 2 | 用户查看已有权限列表 |
| PREQ-02 | Phase 2 | 用户查看可申请权限树 |
| PREQ-03 | Phase 3 | 用户提交权限申请 |
| PREQ-04 | Phase 4 | 用户查看申请记录列表 |
| PREQ-05 | Phase 4 | 用户查看申请详情 |
| PAPR-01 | Phase 5 | 管理员查看待审批列表 |
| PAPR-02 | Phase 6 | 单个申请批准或拒绝 |
| PAPR-03 | Phase 7 | 批量批准或拒绝 |
| PAPR-04 | Phase 6 | 拒绝时填写理由 |
| PAPR-05 | Phase 6 | 批准时添加限制条件 |
| PAPR-06 | Phase 5 | 管理员查看所有申请 |
| PAPR-07 | Phase 6 | 自我审批拦截 |
| PAUT-01 | Phase 3+8 | 自动批准：Phase 3 读取配置，Phase 8 管理员配置开关 |
| PAUT-02 | Phase 3 | 自动批准即时生效 |
| PAUT-03 | Phase 3 | 自动批准写入 t_authorize |
| PINT-01 | Phase 6 | 批准权限写入 t_authorize |
| PINT-02 | Phase 6 | 清除用户权限缓存 |
| PINT-03 | Phase 6 | 事务内完成写入和状态更新 |
| PAUD-01 | Phase 3 | 申请操作审计日志 |
| PAUD-02 | Phase 6 | 审批操作审计日志 |

**Total: 20/20 requirements mapped**
