#!/usr/bin/env python3
"""
Git Worktree 管理脚本
提供 init、list、remove 三个子命令
支持 iTerm2 集成
"""

import os
import sys
import subprocess
import re
from datetime import datetime

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))))
WORKTREES_DIR = os.path.join(PROJECT_ROOT, "worktrees")


def run_command(cmd, cwd=None, capture=True):
    """执行命令并返回结果"""
    try:
        result = subprocess.run(
            cmd, shell=True, cwd=cwd or PROJECT_ROOT,
            capture_output=capture, text=True
        )
        return result.returncode == 0, result.stdout.strip(), result.stderr.strip()
    except Exception as e:
        return False, "", str(e)


def get_current_branch():
    """获取当前分支"""
    success, stdout, _ = run_command("git branch --show-current")
    return stdout if success else ""


def get_worktree_list():
    """获取所有 worktree 列表"""
    success, stdout, _ = run_command("git worktree list")
    if not success:
        return []

    worktrees = []
    for line in stdout.split("\n"):
        if line.strip():
            parts = line.split()
            path = parts[0]
            branch = parts[1].strip("[]")
            # 检查是否有未提交的更改
            has_dirty = "*" in line or "(dirty)" in line
            worktrees.append({
                "path": path,
                "branch": branch,
                "is_main": path == PROJECT_ROOT,
                "is_dirty": has_dirty
            })
    return worktrees


def infer_branch_type(description):
    """根据描述推断分支类型"""
    desc_lower = description.lower()

    if any(kw in desc_lower for kw in ["feat", "功能", "添加", "新增", "实现"]):
        return "feature"
    elif any(kw in desc_lower for kw in ["fix", "修复", "bug", "错误", "问题"]):
        return "bugfix"
    elif any(kw in desc_lower for kw in ["refactor", "重构", "优化", "重写"]):
        return "refactor"
    elif any(kw in desc_lower for kw in ["docs", "文档", "说明", "readme"]):
        return "docs"
    elif any(kw in desc_lower for kw in ["test", "测试", "单元"]):
        return "test"
    elif any(kw in desc_lower for kw in ["chore", "配置", "构建", "依赖"]):
        return "chore"
    else:
        return "feature"  # 默认为功能分支


def sanitize_branch_name(description):
    """将描述转换为合法的英文分支名称"""
    # 转换为小写
    name = description.lower()
    # 移除非 ASCII 字符（保留英文、数字、连字符、空格）
    name = name.encode('ascii', 'ignore').decode('ascii')
    # 用连字符替换空格和下划线
    name = re.sub(r"[\s_]+", "-", name)
    # 移除非字母、数字、连字符的字符
    name = re.sub(r"[^a-z0-9-]", "", name)
    name = name.strip("-")
    # 限制长度
    if len(name) > 50:
        name = name[:50].strip("-")
    # 如果结果为空，使用带时间戳的后缀
    if not name:
        name = datetime.now().strftime("%y%m%d%H%M%S")
    return name


def cmd_init(description):
    """初始化新的 worktree"""
    if not description:
        print("错误：请提供任务描述")
        print("用法：/git-worktree:init \"任务描述\"")
        return False

    print(f"正在为任务创建 worktree: {description}")
    print("-" * 50)

    # 1. 获取当前本地分支的远程跟踪分支
    print("步骤 1/4: 获取当前分支的远程分支...")
    _, current_branch, _ = run_command("git branch --show-current")
    if not current_branch:
        print("无法获取当前分支")
        return False

    # 获取当前分支的远程跟踪分支
    success, upstream, stderr = run_command(f"git rev-parse --abbrev-ref --symbolic-full-name @{current_branch}")
    if not success or not upstream:
        # 如果获取失败，使用默认的 origin/{branch} 格式
        upstream = f"origin/{current_branch}"
        print(f"当前分支: {current_branch}")
        print(f"远程跟踪分支: {upstream} (自动推断)")
    else:
        print(f"当前分支: {current_branch}")
        print(f"远程跟踪分支: {upstream}")

    # 2. 拉取远程分支最新代码
    print("\n步骤 2/4: 拉取远程分支最新代码...")
    success, _, stderr = run_command(f"git fetch origin")
    if not success:
        print(f"拉取远程信息失败: {stderr}")
        return False

    # 验证远程分支存在
    success, stdout, _ = run_command(f"git branch -r | grep ' {upstream}$' || true")
    if not stdout:
        print(f"远程分支 '{upstream}' 不存在")
        return False
    print("✓ 远程分支已更新")

    # 3. 推断分支类型并生成新的分支名
    branch_type = infer_branch_type(description)
    branch_name_base = sanitize_branch_name(description)
    new_branch_name = f"{branch_type}/{branch_name_base}"

    print(f"\n步骤 3/4: 创建 worktree...")

    # 4. 创建 worktree（基于远程分支创建新分支）
    worktree_path = os.path.join(WORKTREES_DIR, branch_name_base)

    if os.path.exists(worktree_path):
        print(f"⚠ worktree 目录已存在: {worktree_path}")
        return False

    # 基于远程跟踪分支创建新的本地分支和 worktree
    success, _, stderr = run_command(f"git worktree add -b {new_branch_name} {worktree_path} {upstream}")
    if not success:
        print(f"创建 worktree 失败: {stderr}")
        return False
    print(f"✓ Worktree 已创建: {worktree_path}")

    # 切换回原分支
    run_command(f"git checkout {current_branch}")

    print("\n步骤 4/4: 完成")
    print("=" * 50)
    print(f"✓ Worktree 创建成功！")
    print(f"\nWorktree 路径: {worktree_path}")
    print(f"新分支名称: {new_branch_name}")
    print(f"基于分支: {upstream}")
    print("=" * 50)

    return True


def cmd_list():
    """列出所有 worktree"""
    print("Git Worktree 列表")
    print("=" * 60)

    worktrees = get_worktree_list()

    if not worktrees:
        print("未找到任何 worktree")
        return True

    # 获取主目录信息
    main_root = PROJECT_ROOT

    for wt in worktrees:
        path = wt["path"]
        branch = wt["branch"]
        is_main = wt["is_main"]
        is_dirty = wt["is_dirty"]

        # 计算相对路径
        if path == main_root:
            rel_path = "（主目录）"
        else:
            rel_path = os.path.relpath(path, main_root)

        # 状态图标
        status = "✗" if is_dirty else "✓"

        # 格式化输出
        if is_main:
            print(f"主 {rel_path:<40} [{branch}] {status}")
        else:
            print(f"└─ {rel_path:<38} [{branch}] {status}")

    print("=" * 60)
    print("✓ = 干净工作区 | ✗ = 有未提交更改")
    print()

    return True


def cmd_remove():
    """删除 worktree - 交互式"""
    worktrees = get_worktree_list()
    # 过滤掉主目录
    removable = [wt for wt in worktrees if not wt["is_main"]]

    if not removable:
        print("没有可删除的 worktree")
        return True

    print("可删除的 Worktree 列表：")
    print("-" * 40)

    for i, wt in enumerate(removable, 1):
        path = wt["path"]
        branch = wt["branch"]
        is_dirty = wt["is_dirty"]

        rel_path = os.path.relpath(path, PROJECT_ROOT)
        status = "✗ 有更改" if is_dirty else "✓ 干净"

        print(f"{i}. {rel_path:<35} [{branch}] {status}")

    print("-" * 40)
    print("0. 取消")

    # 由于无法在这里使用交互式输入，返回可用的 worktree 列表供用户选择
    # 实际删除逻辑需要外部处理
    print("\n提示：使用 /git-worktree:remove <序号> 或 /git-worktree:remove <路径> 删除")

    return True


def cmd_remove_by_index(index_str):
    """根据索引删除 worktree"""
    try:
        index = int(index_str)
    except ValueError:
        # 尝试作为路径处理
        return cmd_remove_by_path(index_str)

    worktrees = get_worktree_list()
    removable = [wt for wt in worktrees if not wt["is_main"]]

    if index < 1 or index > len(removable):
        print(f"无效的索引: {index}")
        return False

    target = removable[index - 1]
    return cmd_remove_worktree(target["path"])


def cmd_remove_by_path(path):
    """根据路径删除 worktree"""
    # 尝试多种路径格式
    possible_paths = [
        path,
        os.path.join(PROJECT_ROOT, path),
        os.path.join(PROJECT_ROOT, "worktrees", path),
    ]

    for p in possible_paths:
        if os.path.exists(p):
            return cmd_remove_worktree(p)

    print(f"未找到 worktree: {path}")
    return False


def cmd_remove_worktree(worktree_path):
    """执行删除 worktree 操作"""
    # 找到对应的 worktree 信息
    worktrees = get_worktree_list()
    target = None
    for wt in worktrees:
        if wt["path"] == worktree_path:
            target = wt
            break

    if not target:
        print(f"未找到 worktree: {worktree_path}")
        return False

    if target["is_main"]:
        print("错误：不能删除主目录的 worktree")
        return False

    # 执行删除
    success, _, stderr = run_command(f"git worktree remove {worktree_path}")
    if not success:
        print(f"删除失败: {stderr}")
        return False

    print(f"✓ 已删除 worktree: {worktree_path}")
    return True


def open_iterm_with_claude(worktree_path, branch_name, description):
    """使用 iTerm2 打开新 tab 并启动 Claude"""
    # 获取 Claude CLI 的路径
    success, claude_path, _ = run_command("which claude || which /usr/local/bin/claude")
    if not claude_path:
        claude_path = "claude"  # 默认使用 claude

    # 构建 AppleScript
    applescript = f'''
tell application "iTerm2"
    activate
    tell current window
        create tab with default profile
    end tell
    tell current session of current window
        write text "cd {worktree_path}"
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
    success, _, stderr = run_command(f"osascript -e '{applescript}'")

    if success:
        print("\n✓ 已在 iTerm2 中打开新 tab 并启动 Claude")
        return True
    else:
        print(f"\n⚠ 打开 iTerm2 失败: {stderr}")
        print(f"\n手动操作：")
        print(f"  1. 打开 iTerm2")
        print(f"  2. 执行: cd {worktree_path}")
        print(f"  3. 执行: claude")
        return False


def cmd_init_with_iterm(description, branch_name=None):
    """初始化新的 worktree，创建完成后询问用户是否打开 iTerm2"""
    if not description:
        print("错误：请提供任务描述")
        print("用法：/git-worktree:init \"任务描述\"")
        return False

    print(f"正在为任务创建 worktree: {description}")
    print("-" * 50)

    # 1. 获取当前本地分支的远程跟踪分支
    print("步骤 1/5: 获取当前分支的远程分支...")
    _, current_branch, _ = run_command("git branch --show-current")
    if not current_branch:
        print("无法获取当前分支")
        return False

    # 获取当前分支的远程跟踪分支
    success, upstream, stderr = run_command(f"git rev-parse --abbrev-ref --symbolic-full-name @{current_branch}")
    if not success or not upstream:
        # 如果获取失败，使用默认的 origin/{branch} 格式
        upstream = f"origin/{current_branch}"
        print(f"当前分支: {current_branch}")
        print(f"远程跟踪分支: {upstream} (自动推断)")
    else:
        print(f"当前分支: {current_branch}")
        print(f"远程跟踪分支: {upstream}")

    # 2. 拉取远程分支最新代码
    print("\n步骤 2/5: 拉取远程分支最新代码...")
    success, _, stderr = run_command(f"git fetch origin")
    if not success:
        print(f"拉取远程信息失败: {stderr}")
        return False

    # 验证远程分支存在
    success, stdout, _ = run_command(f"git branch -r | grep ' {upstream}$' || true")
    if not stdout:
        print(f"远程分支 '{upstream}' 不存在")
        return False
    print("✓ 远程分支已更新")

    # 3. 确定分支名
    if branch_name:
        # 使用指定的分支名（包含类型前缀）
        new_branch_name = branch_name
        # 从分支名中提取目录名
        branch_name_base = branch_name.split("/")[-1]
        print(f"\n步骤 3/5: 使用指定分支名 '{new_branch_name}'...")
    else:
        # 自动推断分支类型并生成新的分支名
        branch_type = infer_branch_type(description)
        branch_name_base = sanitize_branch_name(description)
        new_branch_name = f"{branch_type}/{branch_name_base}"
        print(f"\n步骤 3/5: 创建新分支 '{new_branch_name}'...")

    # 4. 创建 worktree（基于远程分支创建新分支）
    print("\n步骤 4/5: 创建 worktree...")
    worktree_path = os.path.join(WORKTREES_DIR, branch_name_base)

    if os.path.exists(worktree_path):
        print(f"⚠ worktree 目录已存在: {worktree_path}")
        return False

    # 基于远程跟踪分支创建新的本地分支和 worktree
    success, _, stderr = run_command(f"git worktree add -b {new_branch_name} {worktree_path} {upstream}")
    if not success:
        print(f"创建 worktree 失败: {stderr}")
        return False
    print(f"✓ Worktree 已创建: {worktree_path}")

    # 5. 切换回原分支
    print("\n步骤 5/5: 完成")
    run_command(f"git checkout {current_branch}")
    print("=" * 50)
    print(f"✓ Worktree 创建成功！")
    print(f"\nWorktree 路径: {worktree_path}")
    print(f"新分支名称: {new_branch_name}")
    print(f"基于分支: {upstream}")
    print("=" * 50)

    return worktree_path, new_branch_name, description


def main():
    """主函数"""
    if len(sys.argv) < 2:
        print("Git Worktree 管理命令")
        print("=" * 40)
        print("用法：python3 worktree_manager.py <命令> [参数]")
        print()
        print("可用命令：")
        print("  init <描述>              - 创建新的 worktree（创建后询问是否打开 iTerm2）")
        print("  init <描述> --branch <分支名>  - 创建新的 worktree，使用指定的分支名")
        print("  list                     - 列出所有 worktree")
        print("  remove                   - 交互式删除 worktree")
        print("  remove <索引>            - 删除指定索引的 worktree")
        print("  remove <路径>            - 删除指定路径的 worktree")
        return

    command = sys.argv[1]

    if command == "init":
        description = " ".join(sys.argv[2:]) if len(sys.argv) > 2 else ""
        # 解析 --branch 参数
        branch_name = None
        if "--branch" in sys.argv:
            idx = sys.argv.index("--branch")
            if idx + 1 < len(sys.argv):
                branch_name = sys.argv[idx + 1]
                # 从 description 中移除 --branch 和分支名
                args = sys.argv[2:]
                if "--branch" in args:
                    b_idx = args.index("--branch")
                    description = " ".join(args[:b_idx])

        result = cmd_init_with_iterm(description, branch_name)
        if result:
            worktree_path, branch_name_result, desc = result
            print(f"\n[ASK_USER_OPEN_ITERM]")
            print(f"{worktree_path}|{branch_name_result}|{desc}")
            print(f"[/ASK_USER_OPEN_ITERM]")

    elif command == "list":
        cmd_list()

    elif command == "remove":
        if len(sys.argv) > 2:
            cmd_remove_by_index(sys.argv[2])
        else:
            cmd_remove()

    else:
        print(f"未知命令: {command}")


if __name__ == "__main__":
    main()
