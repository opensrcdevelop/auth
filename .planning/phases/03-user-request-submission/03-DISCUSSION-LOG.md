# Phase 3: User Request Submission - Discussion Log

> **Audit trail only.** Do not use as input to planning, research, or execution agents.
> Decisions are captured in CONTEXT.md — this log preserves the alternatives considered.

**Date:** 2026-04-12
**Phase:** 03-user-request-submission
**Areas discussed:** 申请理由、自动批准逻辑、事务边界、批量申请事务、重复申请

---

## Gray Area 1: 申请理由的必要性

| Option | Description | Selected |
|--------|-------------|----------|
| 必填理由（共用） | 每个申请单必须填写统一理由，适用于整个申请（可批量多个权限），不需单独理由字段 | ✓ |
| 必填理由（按权限） | 每个申请单必须填写理由，且每个权限条目独立记录理由（需扩展 PermissionRequestItem 表） | |
| 可选理由 | 理由字段可选，用户可填可不填 | |

**User's choice:** 必填理由（共用）
**Notes:** 用户一次申请多个权限，填写一个统一理由即可，不需要为每个权限条目单独填写理由。

---

## Gray Area 2: 自动批准逻辑的位置

### 2a. Service 组织方式

| Option | Description | Selected |
|--------|-------------|----------|
| 独立 PermissionAutoApproveService（推荐） | 新建 PermissionAutoApproveService，判断权限是否需要自动批准并写入 t_authorize。申请提交Service调用它，复用性好，事务边界清晰 | ✓ |
| 内嵌在申请Service中 | 在 PermissionRequestService 中直接判断并处理，不新建Service层。简单但复用性差 | |
| AOP切面拦截 | 通过AOP在申请提交后自动拦截，调用 AuthorizeService 写入t_authorize。解耦但调试复杂 | |

**User's choice:** 独立 PermissionAutoApproveService
**Notes:** 复用性好，事务边界清晰。

### 2b. 事务边界

| Option | Description | Selected |
|--------|-------------|----------|
| 同一事务（推荐） | 申请提交 + 授权写入 + 状态更新在同一个 @Transactional 中，任何一步失败全部回滚，原子性保证 | ✓ |
| 分开事务 | 申请提交先成功，自动批准授权写入独立事务。申请记录先落地，授权写入异步或失败不影响申请记录 | |

**User's choice:** 同一事务

---

## Gray Area 3: 批量申请的事务边界

| Option | Description | Selected |
|--------|-------------|----------|
| 全部成功或全部回滚（推荐） | 一个事务内处理全部权限申请，任何一个失败全部回滚。简单一致，但一个权限配置问题会导致整个申请失败 | ✓ |
| 部分成功部分PENDING | 自动批准的写入 t_authorize，非自动批准的记录为 PENDING。但同一申请单内部分权限成功部分失败，状态不一致问题 | |

**User's choice:** 全部成功或全部回滚

---

## Gray Area 4: 重复申请的处理

| Option | Description | Selected |
|--------|-------------|----------|
| 不允许重复申请 | 检查用户是否有同一权限的 PENDING 或 AUTO_APPROVED 申请记录，如有则拒绝（提示：已有待审批申请） | ✓ |
| 允许重复申请 | 允许用户多次申请同一权限，生成多个申请记录（正常业务场景，如催促审批） | |
| 允许但合并 | 有新申请时，将PENDING的旧申请标记为CANCELLED（用户主动撤回旧申请），再创建新申请 | |

**User's choice:** 不允许重复申请

---

## Decisions Captured

| Decision | Value |
|----------|-------|
| D-01: 申请理由 | 必填，且整个申请共用一个理由 |
| D-02: 自动批准逻辑 | 独立 PermissionAutoApproveService |
| D-03: 事务边界 | 申请提交 + 授权写入 + 状态更新在同一 @Transactional |
| D-04: 批量申请事务 | 全部成功或全部回滚 |
| D-05: 重复申请 | 不允许（检查 PENDING/AUTO_APPROVED 状态） |
| D-06: 审计日志 | @Audit 注解记录申请人、申请时间、权限列表 |

## Deferred Ideas

None — all discussion stayed within phase scope.
