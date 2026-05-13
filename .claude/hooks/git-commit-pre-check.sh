#!/bin/bash
# Git Commit Pre-Check Hook
# 在 git commit 前自动执行 spotlessApply 和编译检查

set -e

# 解析 hook 输入（JSON 格式）
read -r input
if [ -z "$input" ]; then
    exit 0
fi

# 提取 git 命令（从 JSON 中提取 command 字段）
command=$(echo "$input" | jq -r '.tool_input.command // empty')

if [ -z "$command" ]; then
    exit 0
fi

# 检查是否是 git commit 相关命令
is_git_commit=false
if echo "$command" | grep -qE 'git\s+(commit|push)'; then
    is_git_commit=true
fi

if [ "$is_git_commit" = false ]; then
    exit 0
fi

echo "🔍 检测到 git commit，正在执行质量检查..."

# 检查是否有 staged 文件
has_staged=false
if git diff --cached --quiet 2>/dev/null; then
    # 有 staged 文件才执行检查
    has_staged=true
fi

# 执行代码格式化
echo "📝 执行代码格式化 (spotlessApply)..."
if ./gradlew spotlessApply --quiet 2>&1; then
    echo "✅ 格式化完成"
else
    echo "❌ 格式化失败"
    echo '{"continue": false, "stopReason": "spotlessApply 格式化失败，请检查代码格式后重试"}'
    exit 1
fi

# 执行编译检查（跳过测试）
echo "🔨 执行编译检查..."
if ./gradlew build -x test --quiet 2>&1; then
    echo "✅ 编译检查通过"
else
    echo "❌ 编译失败"
    echo '{"continue": false, "stopReason": "编译失败，请修复编译错误后重试"}'
    exit 1
fi

echo "✅ 质量检查通过，可以提交"
exit 0