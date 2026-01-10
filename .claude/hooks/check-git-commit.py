#!/usr/bin/env python3
"""
检查 git commit 命令是否在 develop 或 main 分支上执行
"""
import sys
import json
import subprocess

def main():
    # 从 stdin 读取 JSON 输入
    try:
        input_data = json.load(sys.stdin)
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
        sys.exit(1)

    # 从 tool_input 获取命令参数
    command = input_data.get("tool_input", {}).get("command", "")

    # 只检查 git commit 命令
    if not command or "git commit" not in command:
        sys.exit(0)  # 不是 git commit 命令，直接通过

    # 获取当前分支名
    try:
        result = subprocess.run(
            ["git", "branch", "--show-current"],
            capture_output=True,
            text=True,
            check=False
        )
        branch = result.stdout.strip()
    except Exception:
        # 不在 git 仓库中，直接通过
        sys.exit(0)

    # 如果是 develop 或 main 分支，拒绝执行
    if branch in ["develop", "main"]:
        print(f"❌ 禁止在 {branch} 分支上执行 git commit", file=sys.stderr)
        print("请在作业分支上进行提交，例如：", file=sys.stderr)
        print("  git checkout -b feature/your-task-name", file=sys.stderr)
        sys.exit(2)  # exit code 2 表示阻止

    sys.exit(0)  # exit code 0 表示允许

if __name__ == "__main__":
    main()
