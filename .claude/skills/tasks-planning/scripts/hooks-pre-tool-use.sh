#!/bin/bash
# PreToolUse Hook for tasks-planning
# Triggered before Write|Edit|Bash operations
# Purpose: Validate task initialization and show current task status

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="${CLAUDE_PROJECT_DIR:-$(pwd)}"
TASKS_FILE="$PROJECT_DIR/.claude/tmp/tasks/tasks.json"
TASKS_DIR="$PROJECT_DIR/.claude/tmp/tasks"

echo "=== [tasks-planning] PreToolUse Hook ==="

# Check 1: Is there a task created?
if [ ! -f "$TASKS_FILE" ]; then
    echo "⚠️  [tasks-planning] No task found!"
    echo "   Please create a task first:"
    echo "   python3 ${SCRIPT_DIR}/tasks-manager.py create \"Task Name\""
    exit 0
fi

# Step 1: Get current session ID
SESSION_ID=$(python3 "${SCRIPT_DIR}/tasks-manager.py" current-session 2>/dev/null)
if [ -z "$SESSION_ID" ]; then
    echo "⚠️  [tasks-planning] Cannot get current session ID!"
    exit 0
fi

# Step 2: Find task that contains this session
TASK_ID=""

# Parse tasks.json to find task with current session
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
    # No task found for current session, check for any pending task
    PENDING=$(python3 "${SCRIPT_DIR}/tasks-manager.py" pending 2>/dev/null | grep -c "🔄" || echo "0")
    if [ "$PENDING" = "0" ]; then
        echo "⚠️  [tasks-planning] No active task and current session not linked!"
        echo "   Run: python3 ${SCRIPT_DIR}/tasks-manager.py add-session <task-id> ${SESSION_ID}"
        exit 0
    fi
    TASK_ID=$(python3 "${SCRIPT_DIR}/tasks-manager.py" pending 2>/dev/null | grep "🔄" | awk '{print $1}' | head -1)
    echo "ℹ️  [tasks-planning] Current session not linked, using: $TASK_ID"
fi

# Step 3: Find task directory and check if task_plan.md exists
TASK_DIR=""
for DATE_DIR in "$TASKS_DIR"/*/; do
    if [ -d "${DATE_DIR}${TASK_ID}" ]; then
        TASK_DIR="${DATE_DIR}${TASK_ID}"
        break
    fi
done

if [ -z "$TASK_DIR" ] || [ ! -f "$TASK_DIR/task_plan.md" ]; then
    echo "⚠️  [tasks-planning] Planning files not found for: $TASK_ID"
    echo "   Create in: ${TASKS_DIR}/<date>/${TASK_ID}/"
    exit 0
fi

# Show current task status
echo "✅ [tasks-planning] Task: $TASK_ID"
echo ""
echo "=== Current Phase ==="
grep -E "Phase|Status:" "$TASK_DIR/task_plan.md" 2>/dev/null | head -30 || true
