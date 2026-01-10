# Git 工作流规范

## 分支策略

### 基础分支（受保护）
- **main**：生产环境分支，仅接受来自 develop 的 PR
- **develop**：开发集成分支，仅接受功能分支的 PR

### 作业分支
- 所有编码任务必须在独立的**作业分支**上完成
- 作业分支基于 **develop** 分支创建
- 分支命名建议：`feature/任务描述` 或 `bugfix/问题描述`

## Git Workflow 工作流程

### 1. 创建作业分支
```bash
# 确保本地 develop 是最新的
git checkout develop
git pull origin develop

# 创建作业分支
git checkout -b feature/your-task-name
```

### 2. 使用 Git Worktree（推荐）
对于需要并行处理的多个任务，使用 worktree：

```bash
# 为新任务创建 worktree
git worktree add ../auth-feature-task1 feature/task1

# 切换到 worktree 目录
cd ../auth-feature-task1

# 在 worktree 中正常工作
# ... 编码、提交 ...

# 完成后删除 worktree
git worktree remove ../auth-feature-task1
```

查看所有 worktree：
```bash
git worktree list
```

### 3. 提交代码
```bash
# 暂存更改
git add .

# 提交（使用规范的提交信息）
git commit -m "feat: 添加用户认证功能"
```

提交信息规范：
- `feat:` 新功能
- `fix:` 修复 bug
- `refactor:` 重构代码
- `docs:` 文档更新
- `test:` 测试相关
- `chore:` 构建/工具配置

### 4. 推送分支
```bash
git push -u origin feature/your-task-name
```

### 5. 创建 Pull Request
```bash
# 使用 gh CLI 创建 PR
gh pr create --base develop --title "功能描述" --body "详细说明"

```

PR 描述模板：
```markdown
## 变更概述
简要描述本次变更的内容

## 变更类型
- [ ] 新功能
- [ ] Bug 修复
- [ ] 重构
- [ ] 文档更新

## 测试计划
- [ ] 单元测试通过
- [ ] 手动测试完成

## 相关 Issue
Closes #issue_number
```

## 禁止操作

### ❌ 严禁直接向 main 或 develop 提交
```bash
# 禁止执行以下操作：
git checkout main
git commit -m "..."  # ❌ 禁止

git checkout develop
git commit -m "..."  # ❌ 禁止
```

### ❌ 未经审查的合并
```bash
# 禁止直接推送到 main/develop
git push origin main    # ❌ 禁止
git push origin develop # ❌ 禁止
```

## 权限保护

项目配置中已设置权限保护：
- `Bash(git push :*main:*)` - 禁止推送到 main
- `Bash(git push :*develop:*)` - 禁止推送到 develop
- `Bash(git merge :*main:*)` - 禁止合并 main
- `Bash(git merge :*develop:*)` - 禁止合并 develop

## 常用 Git Worktree 命令

| 命令 | 说明 |
|------|------|
| `git worktree add <path> <branch>` | 创建新 worktree |
| `git worktree list` | 列出所有 worktree |
| `git worktree remove <path>` | 删除 worktree |
| `git worktree prune` | 清理无效的 worktree 记录 |
