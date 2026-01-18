#!/usr/bin/env python3
"""
Tasks Manager for planning-with-files

Manages tasks.json to track task-session relationships.

Usage:
    python3 tasks-manager.py list                    # List all tasks
    python3 tasks-manager.py create <name>           # Create a new task
    python3 tasks-manager.py start <id>              # Start a task (mark in_progress)
    python3 tasks-manager.py complete <id>           # Complete a task
    python3 tasks-manager.py add-session <id> <session_id>  # Add session to task
    python3 tasks-manager.py pending                 # List pending tasks
"""

import json
import sys
import os
import subprocess
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Optional

TASKS_FILE = Path('.claude/tmp/tasks/tasks.json')
SCRIPT_DIR = Path(__file__).parent


def load_tasks() -> Dict:
    """Load tasks from JSON file."""
    if TASKS_FILE.exists():
        with open(TASKS_FILE, 'r', encoding='utf-8') as f:
            return json.load(f)
    return {"version": "1.0", "tasks": []}


def save_tasks(data: Dict):
    """Save tasks to JSON file."""
    TASKS_FILE.parent.mkdir(parents=True, exist_ok=True)
    with open(TASKS_FILE, 'w', encoding='utf-8') as f:
        json.dump(data, f, ensure_ascii=False, indent=2)


def list_tasks(data: Dict, show_all: bool = True):
    """List all tasks."""
    tasks = data.get('tasks', [])
    if not tasks:
        print("No tasks found.")
        return

    print(f"{'ID':<20} {'Name':<20} {'Status':<12} {'Sessions':<10} {'Start Time'}")
    print("-" * 80)

    for task in sorted(tasks, key=lambda x: x.get('startTime', ''), reverse=True):
        session_count = len(task.get('sessions', []))
        status = task.get('status', 'unknown')
        status_icon = {'in_progress': '🔄', 'completed': '✅', 'pending': '⏳'}.get(status, '❓')

        if not show_all and status == 'completed':
            continue

        print(f"{task['id']:<20} {task['name'][:18]:<20} {status_icon} {status:<10} {session_count:<10} {task.get('startTime', '')}")


def create_task(data: Dict, task_name: str) -> str:
    """Create a new task and return its ID."""
    task_id = task_name.lower().replace(' ', '-').replace('_', '-')
    task_id = ''.join(c for c in task_id if c.isalnum() or c == '-')

    # Check for duplicate
    for task in data.get('tasks', []):
        if task['id'] == task_id:
            print(f"Task '{task_id}' already exists.")
            return task_id

    task = {
        'id': task_id,
        'name': task_name,
        'startTime': datetime.now().isoformat(),
        'endTime': None,
        'status': 'pending',
        'sessions': []
    }

    if 'tasks' not in data:
        data['tasks'] = []
    data['tasks'].append(task)
    save_tasks(data)

    # Auto-create planning files
    init_script = SCRIPT_DIR / 'init-session.sh'
    if init_script.exists():
        result = subprocess.run(
            ['bash', str(init_script), task_id],
            capture_output=True,
            text=True
        )
        if result.returncode == 0:
            print(f"Created task: {task_id} ({task_name})")
            print(f"Planning files created in .claude/tmp/tasks/{datetime.now().strftime('%Y-%m-%d')}/{task_id}/")
        else:
            print(f"Created task: {task_id} ({task_name})")
            print(f"Warning: Failed to create planning files: {result.stderr}")
    else:
        print(f"Created task: {task_id} ({task_name})")

    return task_id


def start_task(data: Dict, task_id: str) -> bool:
    """Start a task (mark as in_progress)."""
    for task in data.get('tasks', []):
        if task['id'] == task_id:
            task['status'] = 'in_progress'
            save_tasks(data)
            print(f"Started task: {task_id}")
            return True
    print(f"Task not found: {task_id}")
    return False


def complete_task(data: Dict, task_id: str) -> bool:
    """Complete a task."""
    for task in data.get('tasks', []):
        if task['id'] == task_id:
            task['status'] = 'completed'
            task['endTime'] = datetime.now().isoformat()
            save_tasks(data)
            print(f"Completed task: {task_id}")
            return True
    print(f"Task not found: {task_id}")
    return False


def add_session(data: Dict, task_id: str, session_id: str) -> bool:
    """Add a session to a task."""
    for task in data.get('tasks', []):
        if task['id'] == task_id:
            session = {
                'sessionId': session_id,
                'startTime': datetime.now().isoformat(),
                'endTime': None
            }
            if 'sessions' not in task:
                task['sessions'] = []
            task['sessions'].append(session)
            save_tasks(data)
            print(f"Added session {session_id} to task: {task_id}")
            return True
    print(f"Task not found: {task_id}")
    return False


def end_session(data: Dict, task_id: str) -> bool:
    """End the most recent session for a task."""
    for task in data.get('tasks', []):
        if task['id'] == task_id:
            sessions = task.get('sessions', [])
            if sessions:
                sessions[-1]['endTime'] = datetime.now().isoformat()
                save_tasks(data)
                print(f"Ended session for task: {task_id}")
                return True
    print(f"No active session for task: {task_id}")
    return False


def get_pending_tasks(data: Dict) -> List[Dict]:
    """Get all pending/in_progress tasks."""
    return [t for t in data.get('tasks', []) if t['status'] != 'completed']


def get_task_context(data: Dict, task_id: str) -> Optional[Dict]:
    """Get all unsynced context for a task from its sessions."""
    task = None
    for t in data.get('tasks', []):
        if t['id'] == task_id:
            task = t
            break

    if not task:
        return None

    project_path = Path.cwd()
    project_dir = get_project_dir(str(project_path))

    if not project_dir.exists():
        return None

    sessions = get_sessions_sorted(project_dir)
    if len(sessions) < 1:
        return None

    unsynced_context = []

    for session in sorted(sessions, key=lambda s: s.stat().st_mtime, reverse=True):
        # Check if this session is part of the task
        session_id = session.stem
        task_sessions = task.get('sessions', [])
        task_session_ids = [s['sessionId'] for s in task_sessions]

        # If this session is not in the task, it's unsynced context
        if session_id not in task_session_ids:
            messages = parse_session_messages(session)
            unsynced_context.extend(messages)

    return {
        'task': task,
        'unsynced_messages': unsynced_context
    }


# Import from session-catchup.py for context retrieval
def get_project_dir(project_path: str) -> Path:
    """Convert project path to Claude's storage path format."""
    sanitized = project_path.replace('/', '-')
    if not sanitized.startswith('-'):
        sanitized = '-' + sanitized
    sanitized = sanitized.replace('_', '-')
    return Path.home() / '.claude' / 'projects' / sanitized


def get_sessions_sorted(project_dir: Path):
    """Get all session files sorted by modification time (newest first)."""
    import glob
    sessions = list(Path(project_dir).glob('*.jsonl'))
    main_sessions = [s for s in sessions if not s.name.startswith('agent-')]
    return sorted(main_sessions, key=lambda p: p.stat().st_mtime, reverse=True)


def parse_session_messages(session_file: Path) -> List[Dict]:
    """Parse all messages from a session file."""
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


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        return

    command = sys.argv[1]
    data = load_tasks()

    if command == 'list':
        list_tasks(data, show_all=True)
    elif command == 'pending':
        pending = get_pending_tasks(data)
        if not pending:
            print("No pending tasks.")
            return
        print(f"Found {len(pending)} pending task(s):")
        for task in pending:
            print(f"  - {task['id']}: {task['name']} ({task['status']})")
    elif command == 'create':
        if len(sys.argv) < 3:
            print("Usage: create <task-name>")
            return
        task_name = ' '.join(sys.argv[2:])
        create_task(data, task_name)
    elif command == 'start':
        if len(sys.argv) < 3:
            print("Usage: start <task-id>")
            return
        start_task(data, sys.argv[2])
    elif command == 'complete':
        if len(sys.argv) < 3:
            print("Usage: complete <task-id>")
            return
        complete_task(data, sys.argv[2])
    elif command == 'add-session':
        if len(sys.argv) < 4:
            print("Usage: add-session <task-id> <session-id>")
            return
        add_session(data, sys.argv[2], sys.argv[3])
    elif command == 'end-session':
        if len(sys.argv) < 3:
            print("Usage: end-session <task-id>")
            return
        end_session(data, sys.argv[2])
    else:
        print(f"Unknown command: {command}")
        print(__doc__)


if __name__ == '__main__':
    main()
