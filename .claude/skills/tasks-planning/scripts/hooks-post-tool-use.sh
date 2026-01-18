#!/bin/bash
# PostToolUse Hook for tasks-planning
# Triggered after Write|Edit operations
# Purpose: Suggest updating task_plan.md when phase is complete

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="${CLAUDE_PROJECT_DIR:-$(pwd)}"
TASKS_FILE="$PROJECT_DIR/.claude/tmp/tasks/tasks.json"
TASKS_DIR="$PROJECT_DIR/.claude/tmp/tasks"

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

if [ -z "$TASK_ID" ]; then
    exit 0
fi

# Find task directory
TASK_DIR=""
for DATE_DIR in "$TASKS_DIR"/*/; do
    if [ -d "${DATE_DIR}${TASK_ID}" ]; then
        TASK_DIR="${DATE_DIR}${TASK_ID}"
        break
    fi
done

if [ -n "$TASK_DIR" ] && [ -f "$TASK_DIR/task_plan.md" ]; then
    CURRENT_PHASE=$(grep "Phase" "$TASK_DIR/task_plan.md" 2>/dev/null | grep -c "in_progress" || echo "0")
    if [ "$CURRENT_PHASE" != "0" ]; then
        echo "[tasks-planning] 💡 Phase in progress - consider updating task_plan.md status"
    fi
fi
