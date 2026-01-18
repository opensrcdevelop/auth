#!/bin/bash
# Check if all phases in task_plan.md are complete
# Exit 0 if complete, exit 1 if incomplete
# Used by Stop hook to verify task completion

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../../" && pwd)"  # 4 levels up to project root

# Try to find task_plan.md in the new directory structure
if [ -z "$1" ]; then
    DATE=$(date +%Y-%m-%d)
    # Look for task_plan.md in .claude/tmp/tasks/{date}/*/
    PLAN_FILE=$(find "$PROJECT_ROOT/.claude/tmp/tasks/$DATE" -name "task_plan.md" -type f 2>/dev/null | head -1)
    if [ -z "$PLAN_FILE" ]; then
        # Fallback: try to find any task_plan.md in .claude/tmp/tasks/
        PLAN_FILE=$(find "$PROJECT_ROOT/.claude/tmp/tasks" -name "task_plan.md" -type f 2>/dev/null | head -1)
    fi
    if [ -z "$PLAN_FILE" ]; then
        # Fallback to current directory
        PLAN_FILE="task_plan.md"
    fi
else
    PLAN_FILE="$1"
fi

if [ ! -f "$PLAN_FILE" ]; then
    echo "ERROR: $PLAN_FILE not found"
    echo "Cannot verify completion without a task plan."
    exit 1
fi

echo "=== Task Completion Check ==="
echo ""

# Count phases by status (using -F for fixed string matching)
TOTAL=$(grep -c "### Phase" "$PLAN_FILE" || true)
COMPLETE=$(grep -cF "**Status:** complete" "$PLAN_FILE" || true)
IN_PROGRESS=$(grep -cF "**Status:** in_progress" "$PLAN_FILE" || true)
PENDING=$(grep -cF "**Status:** pending" "$PLAN_FILE" || true)

# Default to 0 if empty
: "${TOTAL:=0}"
: "${COMPLETE:=0}"
: "${IN_PROGRESS:=0}"
: "${PENDING:=0}"

echo "Total phases:   $TOTAL"
echo "Complete:       $COMPLETE"
echo "In progress:    $IN_PROGRESS"
echo "Pending:        $PENDING"
echo ""

# Check completion
if [ "$COMPLETE" -eq "$TOTAL" ] && [ "$TOTAL" -gt 0 ]; then
    echo "ALL PHASES COMPLETE"
    exit 0
else
    echo "TASK NOT COMPLETE"
    echo ""
    echo "Do not stop until all phases are complete."
    exit 1
fi
