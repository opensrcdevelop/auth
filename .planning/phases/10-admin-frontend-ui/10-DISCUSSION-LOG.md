# Phase 10: Admin Frontend UI - Discussion Log

> **Audit trail only.** Do not use as input to planning, research, or execution agents.
> Decisions are captured in CONTEXT.md — this log preserves the alternatives considered.

**Date:** 2026-04-14
**Phase:** 10-admin-frontend-ui
**Areas discussed:** 菜单位置, 列表布局, 限制条件选择, 批量审批, 状态筛选, 自我审批处理

---

## 菜单位置

| Option | Description | Selected |
|--------|-------------|----------|
| 独立顶级菜单「审批管理」 | 新增顶级菜单项，清晰分离用户申请与管理员审批职责 | |
| 挂在「权限管理」下作为子菜单 | 作为权限管理模块的扩展，与现有资源权限管理保持一致 | ✓ |
| 挂在「用户管理」下 | 审批作为用户权限管理的辅助功能 | |

**User's choice:** 挂在「权限管理」下作为子菜单
**Notes:** 与现有资源权限管理保持一致

---

## 列表布局

| Option | Description | Selected |
|--------|-------------|----------|
| Tabs 切换（待审批/全部） | 类似 Phase 9 用户中心的 Tab 布局模式，简洁直观 | |
| 同一页面通过状态筛选 | 全部申请列表顶部加状态筛选 Tab，一次加载两种数据 | ✓ |
| 两个独立页面 | 待审批和全部申请分开两个菜单入口 | |

**User's choice:** 同一页面通过状态筛选
**Notes:** —

---

## 限制条件选择

| Option | Description | Selected |
|--------|-------------|----------|
| 多选下拉框/复选框 | 从已有 t_permission_exp 列表中勾选，AND 关系 | |
| 条件标签选择器 | 类似权限标签的选择方式，交互更友好 | |
| 弹窗表格选择 | 打开弹窗展示所有可用条件，支持搜索 | |

**User's choice:** 参考授权时的做法（多选下拉框）
**Notes:** 参考授权时的做法（多选下拉框）

---

## 批量审批

| Option | Description | Selected |
|--------|-------------|----------|
| 不实现批量审批 | 遵循 Phase 7 跳过决策，纯单个审批 UI，简洁清晰 | ✓ |
| 实现批量审批 | 追加批量选择+批量 approve/reject 功能，需要先补后端 API | |

**User's choice:** 不批量审批。注意：可以同时批准或拒绝多个子项
**Notes:** 不批量审批。注意：可以同时批准或拒绝多个子项 — 整个请求一起审批，不是单独操作某个子项

---

## 状态筛选

| Option | Description | Selected |
|--------|-------------|----------|
| 全部 + 单状态筛选 | 默认展示全部，顶部支持按 PENDING/APPROVED/REJECTED/AUTO_APPROVED 单独筛选 | |
| 预设快捷筛选 | 提供「待审批」「已审批」「已拒绝」等快捷选项卡 | |
| 多选筛选 | 可同时选择多个状态组合筛选 | ✓ |

**User's choice:** 多选筛选
**Notes:** —

---

## 自我审批处理

| Option | Description | Selected |
|--------|-------------|----------|
| 不需要前端处理 | 后端已拦截，前端保持简洁，仅依赖后端错误提示 | ✓ |
| 前端隐藏操作按钮 | 当前用户提交的申请，前端不显示批准/拒绝按钮 | |

**User's choice:** 不需要前端处理
**Notes:** 后端已实现 PAPR-07，前端不需要额外处理

---

## Claude's Discretion

None — all decisions made by user

## Deferred Ideas

None

