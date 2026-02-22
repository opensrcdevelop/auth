# Git 工作流规范

## 分支策略

### 基础分支（受保护）
- **main**：生产环境分支，仅接受来自 develop 的 PR
- **develop**：开发集成分支，仅接受功能分支的 PR

### 作业分支
- 所有编码任务必须在独立的**作业分支**上完成
- 作业分支基于 **develop** 分支创建
- 分支命名建议：`feature/任务描述` 或 `bugfix/问题描述`

## Git Worktree 规范

### Worktree 目录结构
所有 worktree 必须创建在项目根目录下的 `worktrees/` 文件夹中：

```
auth/
├── worktrees/
│   ├── feature-excel-import/     # 功能分支 worktree
│   ├── bugfix-login-issue/       # 修复分支 worktree
│   └── ...
├── .git/
├── auth-biz/
└── ...
```

### 创建 Worktree
```bash
# 确保本地 develop 是最新的
git checkout develop
git pull origin develop

# 创建 worktree（必须在 worktrees/ 目录下）
git worktree add worktrees/feature-excel-import feature/excel-import

# 切换到 worktree 目录
cd worktrees/feature-excel-import

# 在 worktree 中正常工作
# ... 编码、提交 ...
```

### 查看 Worktree
```bash
# 列出所有 worktree
git worktree list

# 输出示例：
# /Users/lee0407/dev/projs/auth                             abc1234 (develop)
# /Users/lee0407/dev/projs/auth/worktrees/feature-excel-import  def5678 (feature/excel-import)
```

### 删除 Worktree
```bash
# 完成后删除 worktree
cd /Users/lee0407/dev/projs/auth
git worktree remove worktrees/feature-excel-import

# 或者先切换回主目录，再删除
git worktree remove ./worktrees/feature-excel-import
```

### Worktree 最佳实践
1. **为每个独立任务创建单独的 worktree**
2. **使用描述性的目录名称**（与分支名称对应）
3. **完成任务后及时清理 worktree**
4. **不要在 worktree 中创建子 worktree**

## 提交代码规范

### 提交信息格式
```bash
git commit -m "类型: 简短描述"

# 详细提交（添加正文）
git commit -m "类型: 简短描述

详细描述本次变更的内容和原因

- 变更点1
- 变更点2
- 变更点3

Co-Authored-By: Claude <noreply@anthropic.com>"
```

### 提交类型
| 类型 | 说明 | 示例 |
|------|------|------|
| `feat:` | 新功能 | `feat: 添加用户 Excel 导入导出功能` |
| `fix:` | 修复 bug | `fix: 修复用户登录时密码验证错误` |
| `refactor:` | 重构代码 | `refactor: 优化用户查询性能` |
| `docs:` | 文档更新 | `docs: 更新 API 文档` |
| `test:` | 测试相关 | `test: 添加用户服务单元测试` |
| `chore:` | 构建/工具配置 | `chore: 添加 Spotless 代码格式化工具` |

## Pull Request 规范

### PR 标题格式
```
类型: 简短描述
```

### PR 描述模板
```markdown
## 变更概述
简要描述本次变更的内容（1-2 句话）

## 变更类型
- [ ] 新功能 (feat)
- [ ] Bug 修复 (fix)
- [ ] 重构 (refactor)
- [ ] 文档更新 (docs)
- [ ] 测试 (test)
- [ ] 配置/工具 (chore)

## 主要变更
- 变更点1
- 变更点2
- 变更点3

## 测试计划
- [ ] 单元测试通过 (`./gradlew test`)
- [ ] 代码格式化通过 (`./gradlew spotlessCheck`)
- [ ] 构建成功 (`./gradlew build`)
- [ ] 手动测试完成

## 相关 Issue
Closes #(issue_number)

## 截图/演示
（如果有 UI 变更，添加截图或 GIF）

## 检查清单
- [ ] 代码遵循项目规范
- [ ] 已添加必要的注释
- [ ] 已更新相关文档
- [ ] 无新增警告
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

### ❌ 在主分支创建 Worktree
```bash
# 禁止在主目录创建 worktree
git worktree add ../my-feature feature/xxx  # ❌ 错误

# 正确方式：在 worktrees/ 目录下创建
git worktree add worktrees/my-feature feature/xxx  # ✅ 正确
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
| `git worktree move <old> <new>` | 移动 worktree |
