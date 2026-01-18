#!/bin/bash
# Stop Hook for tasks-planning
# Triggered when session stops
# Purpose: End session for the current task

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="${CLAUDE_PROJECT_DIR:-$(pwd)}"
TASKS_FILE="$PROJECT_DIR/.claude/tmp/tasks/tasks.json"

echo "[tasks-planning] Stop Hook"

# Check task completion
bash "${SCRIPT_DIR}/check-complete.sh"

# Get current session ID
SESSION_ID=$(python3 "${SCRIPT_DIR}/tasks-manager.py" current-session 2>/dev/null)
if [ -z "$SESSION_ID" ]; then
    exit 0
fi

# Find task for current session
TASK_ID=$(python3 -c "
import json
data = json.load(open('$TASKS_FILE'))
session_id = '$SESSION_ID'
for task in data.get('tasks', []):
    for sess in task.get('sessions', []):
        if sess.get('sessionId') == session_id:
            print(task.get('id'))
            exit(0)
" 2>/dev/null)

if [ -n "$TASK_ID" ]; then
    python3 "${SCRIPT_DIR}/tasks-manager.py" end-session "$TASK_ID" 2>/dev/null
    echo "[tasks-planning] Session ended for: $TASK_ID"
fi
