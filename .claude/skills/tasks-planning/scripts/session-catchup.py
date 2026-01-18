#!/usr/bin/env python3
"""
Session Catchup Script for planning-with-files (v2.5.0)

Analyzes previous sessions to find unsynced context for incomplete tasks.
Designed to run on SessionStart or when continuing a task.

Usage: python3 session-catchup.py [project-path] [--task <task-id>]

When --task is specified, retrieves unsynced context from ALL sessions
not yet recorded in that task's session history.
"""

import json
import sys
import os
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from datetime import datetime

PLANNING_FILES = ['task_plan.md', 'progress.md', 'findings.md']
TASKS_FILE = Path('.claude/tmp/tasks/tasks.json')


def get_project_dir(project_path: str) -> Path:
    """Convert project path to Claude's storage path format."""
    sanitized = project_path.replace('/', '-')
    if not sanitized.startswith('-'):
        sanitized = '-' + sanitized
    sanitized = sanitized.replace('_', '-')
    return Path.home() / '.claude' / 'projects' / sanitized


def get_sessions_sorted(project_dir: Path) -> List[Path]:
    """Get all session files sorted by modification time (newest first)."""
    sessions = list(project_dir.glob('*.jsonl'))
    main_sessions = [s for s in sessions if not s.name.startswith('agent-')]
    return sorted(main_sessions, key=lambda p: p.stat().st_mtime, reverse=True)


def parse_session_messages(session_file: Path) -> List[Dict]:
    """Parse all messages from a session file, preserving order."""
    messages = []
    with open(session_file, 'r') as f:
        for line_num, line in enumerate(f):
            try:
                data = json.loads(line)
                data['_line_num'] = line_num
                messages.append(data)
            except json.JSONDecodeError:
                pass
    return messages


def find_last_planning_update(messages: List[Dict]) -> Tuple[int, Optional[str]]:
    """
    Find the last time a planning file was written/edited.
    Returns (line_number, filename) or (-1, None) if not found.
    """
    last_update_line = -1
    last_update_file = None

    for msg in messages:
        msg_type = msg.get('type')

        if msg_type == 'assistant':
            content = msg.get('message', {}).get('content', [])
            if isinstance(content, list):
                for item in content:
                    if item.get('type') == 'tool_use':
                        tool_name = item.get('name', '')
                        tool_input = item.get('input', {})

                        if tool_name in ('Write', 'Edit'):
                            file_path = tool_input.get('file_path', '')
                            for pf in PLANNING_FILES:
                                if file_path.endswith(pf):
                                    last_update_line = msg['_line_num']
                                    last_update_file = pf

    return last_update_line, last_update_file


def extract_messages_after(messages: List[Dict], after_line: int) -> List[Dict]:
    """Extract conversation messages after a certain line number."""
    result = []
    for msg in messages:
        if msg['_line_num'] <= after_line:
            continue

        msg_type = msg.get('type')
        is_meta = msg.get('isMeta', False)

        if msg_type == 'user' and not is_meta:
            content = msg.get('message', {}).get('content', '')
            if isinstance(content, list):
                for item in content:
                    if isinstance(item, dict) and item.get('type') == 'text':
                        content = item.get('text', '')
                        break
                else:
                    content = ''

            if content and isinstance(content, str):
                if content.startswith(('<local-command', '<command-', '<task-notification')):
                    continue
                if len(content) > 20:
                    result.append({'role': 'user', 'content': content, 'line': msg['_line_num']})

        elif msg_type == 'assistant':
            msg_content = msg.get('message', {}).get('content', '')
            text_content = ''
            tool_uses = []

            if isinstance(msg_content, str):
                text_content = msg_content
            elif isinstance(msg_content, list):
                for item in msg_content:
                    if item.get('type') == 'text':
                        text_content = item.get('text', '')
                    elif item.get('type') == 'tool_use':
                        tool_name = item.get('name', '')
                        tool_input = tool_input = item.get('input', {})
                        if tool_name == 'Edit':
                            tool_uses.append(f"Edit: {tool_input.get('file_path', 'unknown')}")
                        elif tool_name == 'Write':
                            tool_uses.append(f"Write: {tool_input.get('file_path', 'unknown')}")
                        elif tool_name == 'Bash':
                            cmd = tool_input.get('command', '')[:80]
                            tool_uses.append(f"Bash: {cmd}")
                        else:
                            tool_uses.append(f"{tool_name}")

            if text_content or tool_uses:
                result.append({
                    'role': 'assistant',
                    'content': text_content[:600] if text_content else '',
                    'tools': tool_uses,
                    'line': msg['_line_num']
                })

    return result


def get_task_by_id(project_path: str, task_id: str) -> Optional[Dict]:
    """Get task from tasks.json by ID."""
    tasks_file = Path(project_path) / '.claude/tmp/tasks/tasks.json'
    if not tasks_file.exists():
        return None

    with open(tasks_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    for task in data.get('tasks', []):
        if task['id'] == task_id:
            return task
    return None


def get_unsynced_context_for_task(project_path: str, task_id: str) -> List[Dict]:
    """
    Get all unsynced context for a specific task.
    Returns messages from sessions NOT yet recorded in the task.
    """
    task = get_task_by_id(project_path, task_id)
    if not task:
        return []

    project_dir = get_project_dir(project_path)
    if not project_dir.exists():
        return []

    sessions = get_sessions_sorted(project_dir)
    if len(sessions) < 1:
        return []

    # Get session IDs already recorded in this task
    task_session_ids = set()
    for session in task.get('sessions', []):
        task_session_ids.add(session['sessionId'])

    unsynced_messages = []

    # Process all sessions (sorted by time, newest first)
    for session in sessions:
        session_id = session.stem

        # Skip if this session is already in the task
        if session_id in task_session_ids:
            continue

        # This is an unsynced session - get all its messages
        messages = parse_session_messages(session)
        if messages:
            unsynced_messages.extend(messages)

    return unsynced_messages


def get_all_pending_tasks(project_path: str) -> List[Dict]:
    """Get all pending/in_progress tasks from tasks.json."""
    tasks_file = Path(project_path) / '.claude/tmp/tasks/tasks.json'
    if not tasks_file.exists():
        return []

    with open(tasks_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    return [t for t in data.get('tasks', []) if t['status'] != 'completed']


def format_unsynced_messages(messages: List[Dict], max_items: int = 15) -> str:
    """Format unsynced messages for display."""
    if not messages:
        return "No unsynced context found."

    # Filter and format messages
    formatted = []
    user_msgs = [m for m in messages if m.get('role') == 'user']
    assistant_msgs = [m for m in messages if m.get('role') == 'assistant']

    # Add user messages
    for msg in user_msgs[-max_items:]:
        content = msg.get('content', '')[:300]
        formatted.append(f"USER: {content}")

    # Add assistant messages
    for msg in assistant_msgs[-max_items:]:
        content = msg.get('content', '')[:300]
        tools = msg.get('tools', [])
        formatted.append(f"CLAUDE: {content}")
        if tools:
            formatted.append(f"  Tools: {', '.join(tools[:4])}")

    return '\n'.join(formatted)


def main():
    project_path = sys.argv[1] if len(sys.argv) > 1 else os.getcwd()

    # Check for --task flag
    target_task_id = None
    if '--task' in sys.argv:
        idx = sys.argv.index('--task')
        if idx + 1 < len(sys.argv):
            target_task_id = sys.argv[idx + 1]

    project_dir = get_project_dir(project_path)

    # Check if tasks.json exists
    if not TASKS_FILE.exists() and not target_task_id:
        print("[planning-with-files] No tasks.json found. Create a task first:")
        print("  python3 scripts/tasks-manager.py create \"Task Name\"")
        return

    # If target task specified, get unsynced context for that task
    if target_task_id:
        task = get_task_by_id(project_path, target_task_id)
        if not task:
            print(f"[planning-with-files] Task not found: {target_task_id}")
            return

        print(f"\n=== Unsynced Context for Task: {task['name']} ({task['id']}) ===")
        print(f"Status: {task['status']}")
        print(f"Recorded sessions: {len(task.get('sessions', []))}")

        unsynced = get_unsynced_context_for_task(project_path, target_task_id)

        if not unsynced:
            print("\n✓ All sessions are synced. No unsynced context found.")
            return

        print(f"\nUnsynced messages: {len(unsynced)}")
        print("\n--- UNSYNCED CONTEXT ---")
        print(format_unsynced_messages(unsynced))
        return

    # Default: Check for pending tasks
    pending_tasks = get_all_pending_tasks(project_path)

    if not pending_tasks:
        print("[planning-with-files] No pending tasks.")
        print("Create a new task:")
        print("  python3 scripts/tasks-manager.py create \"Task Name\"")
        return

    print(f"[planning-with-files] Found {len(pending_tasks)} pending task(s):\n")
    for task in sorted(pending_tasks, key=lambda t: t.get('startTime', ''), reverse=True):
        session_count = len(task.get('sessions', []))
        print(f"  • {task['id']}: {task['name']} ({task['status']}, {session_count} sessions)")

    print("\nTo get unsynced context for a task, run:")
    print("  python3 session-catchup.py --task <task-id>")


if __name__ == '__main__':
    main()
