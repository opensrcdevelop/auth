#!/usr/bin/env python3
"""
打开 iTerm2 并启动 Claude
用于处理 AskUserQuestion 用户同意后的操作
支持检测是否已在 iTerm2 中运行
"""

import os
import sys
import subprocess

# 检测当前终端是否为 iTerm2
def is_running_in_iterm():
    """检测当前是否在 iTerm2 中运行"""
    # 检查 TERM_PROGRAM 环境变量
    term_program = os.environ.get("TERM_PROGRAM", "")
    if term_program == "iTerm.app":
        return True

    # 检查 TERM_SESSION_ID（iTerm2 会设置此变量）
    term_session_id = os.environ.get("TERM_SESSION_ID", "")
    if term_session_id:
        return True

    return False


def get_claude_path():
    """获取 Claude CLI 的路径"""
    try:
        result = subprocess.run("which claude", shell=True, capture_output=True, text=True)
        return result.stdout.strip() if result.stdout.strip() else "claude"
    except:
        return "claude"


def open_iterm_new_tab(worktree_path, branch_name, description):
    """在 iTerm2 中打开新 tab 并启动 Claude"""
    claude_path = get_claude_path()

    # 构建 AppleScript
    applescript = f'''
tell application "iTerm2"
    activate
    tell current window
        create tab with default profile
    end tell
    tell current session of current window
        write text "cd \\"{worktree_path}\\""
        write text "echo \\"Worktree: {worktree_path}\\""
        write text "echo \\"Branch: {branch_name}\\""
        write text "echo \\"Task: {description}\\""
        write text "echo \\"\\""
        write text "echo \\"正在启动 Claude...\\""
        write text "{claude_path}"
    end tell
end tell
'''

    # 执行 AppleScript
    result = subprocess.run(f"osascript -e '{applescript}'", shell=True)
    return result.returncode == 0


def open_iterm_and_new_tab(worktree_path, branch_name, description):
    """启动 iTerm2 并打开新 tab"""
    claude_path = get_claude_path()

    # 构建 AppleScript：先激活/启动 iTerm2，再打开新 tab
    applescript = f'''
tell application "iTerm2"
    activate
    delay 0.5
    tell current window
        create tab with default profile
    end tell
    tell current session of current window
        write text "cd \\"{worktree_path}\\""
        write text "echo \\"Worktree: {worktree_path}\\""
        write text "echo \\"Branch: {branch_name}\\""
        write text "echo \\"Task: {description}\\""
        write text "echo \\"\\""
        write text "echo \\"正在启动 Claude...\\""
        write text "{claude_path}"
    end tell
end tell
'''

    result = subprocess.run(f"osascript -e '{applescript}'", shell=True)
    return result.returncode == 0


def open_iterm(worktree_path, branch_name, description):
    """打开 iTerm2（如果已在 iTerm2 中则打开新 tab）"""
    in_iterm = is_running_in_iterm()

    if in_iterm:
        print("检测到当前已在 iTerm2 中，正在打开新 tab...")
        success = open_iterm_new_tab(worktree_path, branch_name, description)
    else:
        print("当前不在 iTerm2 中，正在启动 iTerm2 并打开新 tab...")
        success = open_iterm_and_new_tab(worktree_path, branch_name, description)

    return success


if __name__ == "__main__":
    if len(sys.argv) < 4:
        print("用法: python3 open_iterm.py <worktree_path> <branch_name> <description>")
        sys.exit(1)

    worktree_path = sys.argv[1]
    branch_name = sys.argv[2]
    description = " ".join(sys.argv[3:])

    success = open_iterm(worktree_path, branch_name, description)

    if success:
        print("✓ 已在 iTerm2 中打开新 tab 并启动 Claude")
    else:
        print("⚠ 打开 iTerm2 失败")
        print(f"\n手动操作：")
        print(f"  1. 打开 iTerm2")
        print(f"  2. 执行: cd {worktree_path}")
        print(f"  3. 执行: claude")

    sys.exit(0 if success else 1)
