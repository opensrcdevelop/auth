#!/usr/bin/env python3
"""
检查命令是否包含危险的 rm -rf
"""
import sys
import json
import re

def main():
    # 从 stdin 读取 JSON 输入
    try:
        input_data = json.load(sys.stdin)
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
        sys.exit(1)

    # 从 tool_input 获取命令参数
    command = input_data.get("tool_input", {}).get("command", "")

    if not command:
        sys.exit(0)  # 没有命令，直接通过

    # 检查是否包含 rm -rf（但不包括已保护的 rm -rf /*）
    # 使用正则匹配 rm 后跟 -rf，并且后面不是以 / 开头（排除 /* 模式）
    pattern = r'\brm\s+-rf\s+[^/]'
    if re.search(pattern, command):
        print("❌ 禁止执行 rm -rf 命令", file=sys.stderr)
        print("为避免误删文件，请使用更精确的删除命令：", file=sys.stderr)
        print("  rm 具体文件路径", file=sys.stderr)
        print("  rm -r 具体目录路径", file=sys.stderr)
        print("或使用 file manager 工具进行文件操作", file=sys.stderr)
        sys.exit(2)  # exit code 2 表示阻止

    sys.exit(0)  # exit code 0 表示允许

if __name__ == "__main__":
    main()
