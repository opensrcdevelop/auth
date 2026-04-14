# Phase 10: Admin Frontend UI - Context

**Gathered:** 2026-04-14
**Status:** Ready for planning

<domain>
## Phase Boundary

管理员在控制台审批权限申请。纯前端 UI 工作，后端 API 已在 Phase 5-6 实现。

核心能力：
1. 管理员查看待审批列表和全部申请（多选状态筛选）
2. 管理员在详情弹窗中查看申请包含的权限子项列表
3. 管理员可勾选部分子项进行**部分批准**（可添加限制条件）
4. 管理员可勾选部分子项进行**部分拒绝**（必填拒绝理由）
5. 管理员不能审批自己的申请（后端已实现 PAPR-07，前端不额外处理）

**Scope: 无跨请求批量审批** — Phase 7 已跳过，跨请求的批量操作不在 v1 范围内。但同一申请单内的部分子项批准+部分拒绝是支持的。

</domain>

<decisions>
## Implementation Decisions

### 菜单位置
- **D-01:** 挂在「权限管理」菜单下作为子菜单
- **D-02:** 新增路由 `/permission/request` 对应审批管理页面
- **D-03:** 与现有的资源权限（`/permission/resource`）、限制条件（`/permission/expression`）并列

### 列表页面布局
- **D-04:** 单页面设计，通过顶部状态筛选器切换
- **D-05:** 状态筛选支持**多选**（可同时选 PENDING + AUTO_APPROVED 等组合）
- **D-06:** 默认展示全部申请，支持按状态单选/多选筛选
- **D-07:** 表格字段：申请ID、申请人、申请时间、状态、申请理由、操作（查看详情/批准/拒绝）
- **D-08:** 分页：默认 page=1, size=15（与 Phase 4-5 后端接口一致）

### 详情弹窗（核心变更）
- **D-09:** 点击「查看详情」打开弹窗
- **D-10:** 展示：申请信息、权限列表（每个子项含名称/代码/状态）、审批结果
- **D-11:** **每个子项独立审批** — 支持部分批准、部分拒绝
  - 每个 PENDING 状态的子项显示勾选框
  - 顶部有「批准选中」「拒绝选中」批量操作按钮
  - 也支持对单个子项单独操作
- **D-12:** AUTO_APPROVED 状态的子项**不显示操作按钮**（已自动批准，无需操作）

### 批准操作
- **D-13:** 选中子项后点击「批准选中」→ 打开批准弹窗
- **D-14:** 弹窗中设置**限制条件**（多选下拉框）和**优先级**（可选）
- **D-15:** 条件/优先级**适用于所有被批准的子项**（通过 `expressionIds` 字段传给后端）
- **D-16:** 不选任何子项时，「批准选中」按钮禁用
- **D-17:** 提交调用 `POST /admin/permissions/requests/{id}/approve`，后端根据 `itemIds` 只批准指定子项

### 拒绝操作
- **D-18:** 选中子项后点击「拒绝选中」→ 打开拒绝弹窗
- **D-19:** 拒绝理由：**文本输入框**，必填，有字数限制（如 200 字）
- **D-20:** 拒绝理由**适用于所有被拒绝的子项**
- **D-21:** 提交调用 `POST /admin/permissions/requests/{id}/reject`，后端根据 `itemIds` 只拒绝指定子项

### 批量操作范围
- **D-22:** **不实现**跨请求的批量审批（Phase 7 已跳过）
- **D-23:** 支持**同一申请单内**的部分子项批准 + 部分子项拒绝（一次操作）
  - 用户一次申请多个权限，管理员可以批准其中部分、拒绝其余
  - 通过勾选子项 + 一次提交实现

### 自我审批拦截
- **D-24:** 前端**不**额外处理 — 后端已实现 PAPR-07（后端会返回错误，前端依赖后端错误提示）

### API 集成
- **D-25:** 新建 `ui/src/api/adminPermissionRequest.ts` 封装 admin API 调用
- **D-26:** API 端点（Phase 5-6 已实现）：
  - `GET /admin/permissions/requests/pending` — 待审批列表
  - `GET /admin/permissions/requests/all?status=` — 全部申请
  - `POST /admin/permissions/requests/{id}/approve` — 批准（传 `itemIds` 实现部分批准）
  - `POST /admin/permissions/requests/{id}/reject` — 拒绝（传 `itemIds` 实现部分拒绝）
- **D-27:** `ApproveRequestDto` 字段：`itemIds`（可选，不传则批准所有 PENDING）、`expressionIds`、`priority`
- **D-28:** `RejectRequestDto` 字段：`itemIds`（可选，不传则拒绝所有 PENDING）、`rejectReason`

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### Backend APIs (Phase 5-6)
- `auth-server/src/main/java/cn/opensrcdevelop/auth/controller/admin/AdminPermissionRequestController.java` — admin API 端点
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/service/permission/request/PermissionRequestAdminService.java` — admin service
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/dto/permission/request/ApproveRequestDto.java` — 批准请求 DTO（expressionIds 字段）
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/dto/permission/request/RejectRequestDto.java` — 拒绝请求 DTO
- `auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/dto/permission/request/PermissionRequestListItemDto.java` — 列表项 DTO

### Frontend Patterns
- `ui/src/views/permission/authorize/index.vue` — 限制条件多选下拉框参考（a-select multiple）
- `ui/src/views/permission/authorize/index.ts` — getPermissionExpList 调用方式参考
- `ui/src/views/user/home/components/RequestRecords.vue` — 申请记录详情弹窗参考（Phase 9）
- `ui/src/api/permission.ts` — getPermissionExpList 函数
- `ui/src/router/menuRoutes.ts` — 菜单路由配置参考

### Prior Phase Context
- `.planning/phases/05-admin-pending-list-api/05-CONTEXT.md` — 待审批/全部申请列表字段规范
- `.planning/phases/06-admin-single-approve/06-CONTEXT.md` — 批准/拒绝逻辑（若存在）
- `.planning/phases/09-user-frontend-ui/09-CONTEXT.md` — 用户中心布局模式、Tabs vs 单页选择

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- `a-select multiple` — 复用授权页条件选择模式
- `getPermissionExpList` API — 已有的条件列表查询
- `RequestRecords.vue` — Phase 9 详情弹窗模式
- `permission/resource/index.vue` — 管理员表格列表参考

### Established Patterns
- 管理员页面：`a-table` + 分页 + 操作按钮
- 弹窗操作：a-modal + 表单
- 状态 Tag：使用 a-tag，颜色映射（PENDING=arcoblue, APPROVED=green, REJECTED=red, AUTO_APPROVED=purple）

### Integration Points
- 新页面路由：`/permission/request`
- 新 API 模块：`ui/src/api/adminPermissionRequest.ts`
- 菜单挂载：权限管理子菜单（与 resource/role/expression 并列）

</code_context>

<specifics>
## Specific Ideas

- 状态多选筛选：用户可同时选「待审批」+「自动批准」等组合
- 限制条件选择器：复用授权页的 a-select multiple 交互
- 拒绝理由：200 字限制，文本输入框
- **部分审批**：详情弹窗中勾选子项，批准选中的子项或拒绝选中的子项
- 一次操作可同时批准部分子项 + 拒绝其余子项（两次提交：先批准后拒绝，或先拒绝后批准）

<deferred>
## Deferred Ideas

None — Phase 10 scope stayed focused on single-approve UI only

</deferred>

---

*Phase: 10-admin-frontend-ui*
*Context gathered: 2026-04-14*
