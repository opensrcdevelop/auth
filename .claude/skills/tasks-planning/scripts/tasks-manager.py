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
    python3 tasks-manager.py end-session <id>        # End the current session
    python3 tasks-manager.py delete <id>             # Delete a task
    python3 tasks-manager.py update <id> <new-name>  # Update task name
    python3 tasks-manager.py pending                 # List pending tasks
    python3 tasks-manager.py show <id>               # Show task details with sessions
    python3 tasks-manager.py stats                   # Show task statistics
"""

import json
import sys
import os
import subprocess
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Optional

# Use script directory for portable paths
SCRIPT_DIR = Path(__file__).resolve().parent
# Navigate from scripts -> tasks-planning -> skills -> .claude -> project root
PROJECT_ROOT = SCRIPT_DIR.parent.parent.parent.parent  # 4 levels up to project root
TASKS_FILE = PROJECT_ROOT / '.claude' / 'tmp' / 'tasks' / 'tasks.json'


def load_tasks() -> Dict:
    """Load tasks from JSON file."""
    if TASKS_FILE.exists():
        try:
            with open(TASKS_FILE, 'r', encoding='utf-8') as f:
                return json.load(f)
        except json.JSONDecodeError as e:
            print(f"Warning: Invalid JSON in tasks file: {e}")
            return {"version": "1.0", "tasks": []}
    return {"version": "1.0", "tasks": []}


def save_tasks(data: Dict):
    """Save tasks to JSON file."""
    TASKS_FILE.parent.mkdir(parents=True, exist_ok=True)
    try:
        with open(TASKS_FILE, 'w', encoding='utf-8') as f:
            json.dump(data, f, ensure_ascii=False, indent=2)
    except IOError as e:
        print(f"Error saving tasks file: {e}")
        sys.exit(1)


def format_duration(start_time: str, end_time: Optional[str] = None) -> str:
    """Format duration between two ISO timestamps."""
    try:
        start = datetime.fromisoformat(start_time)
        end = datetime.fromisoformat(end_time) if end_time else datetime.now()
        delta = end - start
        total_seconds = int(delta.total_seconds())

        if total_seconds < 60:
            return f"{total_seconds}s"
        elif total_seconds < 3600:
            return f"{total_seconds // 60}m {total_seconds % 60}s"
        elif total_seconds < 86400:
            hours = total_seconds // 3600
            mins = (total_seconds % 3600) // 60
            return f"{hours}h {mins}m"
        else:
            days = total_seconds // 86400
            hours = (total_seconds % 86400) // 3600
            return f"{days}d {hours}h"
    except (ValueError, TypeError):
        return "N/A"


def list_tasks(data: Dict, show_all: bool = True):
    """List all tasks."""
    tasks = data.get('tasks', [])
    if not tasks:
        print("No tasks found.")
        return

    print(f"{'ID':<30} {'Name':<24} {'Status':<12} {'Sessions':<8} {'Duration'}")
    print("-" * 90)

    for task in sorted(tasks, key=lambda x: x.get('startTime', ''), reverse=True):
        session_count = len(task.get('sessions', []))
        status = task.get('status', 'unknown')
        status_icon = {'in_progress': '[IN PROGRESS]', 'completed': '[COMPLETED]', 'pending': '[PENDING]'}.get(status, '[UNKNOWN]')

        if not show_all and status == 'completed':
            continue

        duration = format_duration(task.get('startTime', ''), task.get('endTime'))
        print(f"{task['id']:<30} {task['name'][:22]:<24} {status_icon:<12} {session_count:<8} {duration}")


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


def sync_session(data: Dict, task_id: str, session_id: str, last_line: int) -> bool:
    """Update the last synced line for a session."""
    for task in data.get('tasks', []):
        if task['id'] == task_id:
            for session in task.get('sessions', []):
                if session['sessionId'] == session_id:
                    session['lastSyncedLine'] = last_line
                    save_tasks(data)
                    print(f"Synced session {session_id} for task {task_id} (last line: {last_line})")
                    return True
    print(f"Session {session_id} not found in task: {task_id}")
    return False


def get_pending_tasks(data: Dict) -> List[Dict]:
    """Get all pending/in_progress tasks."""
    return [t for t in data.get('tasks', []) if t['status'] != 'completed']


def show_task(data: Dict, task_id: str) -> bool:
    """Show detailed task information including all sessions."""
    for task in data.get('tasks', []):
        if task['id'] == task_id:
            print(f"\n{'=' * 60}")
            print(f"Task: {task['name']}")
            print(f"ID: {task['id']}")
            print(f"Status: {task['status']}")
            print(f"Start: {task.get('startTime', 'N/A')}")
            print(f"End: {task.get('endTime', 'In Progress')}")
            print(f"Duration: {format_duration(task.get('startTime', ''), task.get('endTime'))}")

            sessions = task.get('sessions', [])
            print(f"\nSessions ({len(sessions)}):")
            print("-" * 60)

            for i, session in enumerate(sessions, 1):
                session_duration = format_duration(session.get('startTime', ''), session.get('endTime'))
                print(f"  {i}. {session['sessionId']}")
                print(f"     Started: {session.get('startTime', 'N/A')}")
                print(f"     Ended:   {session.get('endTime', 'Active')}")
                print(f"     Duration: {session_duration}")
            print(f"{'=' * 60}\n")
            return True
    print(f"Task not found: {task_id}")
    return False


def delete_task(data: Dict, task_id: str) -> bool:
    """Delete a task from the tasks list."""
    tasks = data.get('tasks', [])
    for i, task in enumerate(tasks):
        if task['id'] == task_id:
            confirm = input(f"Delete task '{task_id}'? (y/N): ")
            if confirm.lower() == 'y':
                deleted = tasks.pop(i)
                save_tasks(data)
                print(f"Deleted task: {task_id}")
                # Optionally clean up planning files
                task_date = deleted.get('startTime', '')[:10] if deleted.get('startTime') else None
                if task_date:
                    task_dir = PROJECT_ROOT / '.claude' / 'tmp' / 'tasks' / task_date / task_id
                    if task_dir.exists():
                        import shutil
                        shutil.rmtree(task_dir)
                        print(f"Also removed planning files: {task_dir}")
                return True
            else:
                print("Delete cancelled.")
                return False
    print(f"Task not found: {task_id}")
    return False


def update_task(data: Dict, task_id: str, new_name: str) -> bool:
    """Update a task's name."""
    for task in data.get('tasks', []):
        if task['id'] == task_id:
            old_name = task['name']
            task['name'] = new_name
            save_tasks(data)
            print(f"Updated task: {task_id}")
            print(f"  Old name: {old_name}")
            print(f"  New name: {new_name}")
            return True
    print(f"Task not found: {task_id}")
    return False


def show_stats(data: Dict):
    """Show task statistics."""
    tasks = data.get('tasks', [])
    if not tasks:
        print("No tasks found.")
        return

    total = len(tasks)
    completed = sum(1 for t in tasks if t['status'] == 'completed')
    in_progress = sum(1 for t in tasks if t['status'] == 'in_progress')
    pending = sum(1 for t in tasks if t['status'] == 'pending')

    total_sessions = sum(len(t.get('sessions', [])) for t in tasks)

    print("\nTask Statistics")
    print("=" * 40)
    print(f"Total tasks:     {total}")
    print(f"  Completed:     {completed}")
    print(f"  In Progress:   {in_progress}")
    print(f"  Pending:       {pending}")
    print(f"Total sessions:  {total_sessions}")
    print(f"Completion rate: {(completed / total * 100):.1f}%" if total > 0 else "N/A")
    print("=" * 40)


def get_current_session_id() -> Optional[str]:
    """Get the current Claude Code session ID from the session files."""
    import glob
    project_dir = get_project_dir(str(PROJECT_ROOT))
    if not project_dir.exists():
        return None

    sessions = list(project_dir.glob('*.jsonl'))
    # Filter out agent-* files and sort by modification time (newest first)
    main_sessions = [s for s in sessions if not s.name.startswith('agent-')]
    sorted_sessions = sorted(main_sessions, key=lambda p: p.stat().st_mtime, reverse=True)

    if sorted_sessions:
        return sorted_sessions[0].stem  # Return filename without extension
    return None


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
            status_icon = {'in_progress': '🔄', 'completed': '✅', 'pending': '⏳'}.get(task['status'], '❓')
            print(f"  {status_icon} {task['id']}: {task['name']}")
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
    elif command == 'show':
        if len(sys.argv) < 3:
            print("Usage: show <task-id>")
            return
        show_task(data, sys.argv[2])
    elif command == 'delete':
        if len(sys.argv) < 3:
            print("Usage: delete <task-id>")
            return
        delete_task(data, sys.argv[2])
    elif command == 'update':
        if len(sys.argv) < 4:
            print("Usage: update <task-id> <new-name>")
            return
        new_name = ' '.join(sys.argv[3:])
        update_task(data, sys.argv[2], new_name)
    elif command == 'stats':
        show_stats(data)
    elif command == 'current-session':
        session_id = get_current_session_id()
        if session_id:
            print(session_id)
        else:
            print("No active session found.", file=sys.stderr)
            sys.exit(1)
    elif command == 'sync':
        if len(sys.argv) < 5:
            print("Usage: sync <task-id> <session-id> <last-line>")
            return
        try:
            last_line = int(sys.argv[4])
            sync_session(data, sys.argv[2], sys.argv[3], last_line)
        except ValueError:
            print("Error: last-line must be an integer")
    else:
        print(f"Unknown command: {command}")
        print(__doc__)


if __name__ == '__main__':
    main()
