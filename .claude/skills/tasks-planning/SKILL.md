---
name: tasks-planning
version: "2.7.0"
description: Implements Manus-style file-based planning for complex tasks with task-session tracking. Creates task_plan.md, findings.md, and progress.md. Uses tasks.json to track task-session relationships across sessions. Includes hooks for task initialization validation and session management.
user-invocable: true
allowed-tools:
  - Read
  - Write
  - Edit
  - Bash
  - Glob
  - Grep
  - WebFetch
  - WebSearch
hooks:
  PreToolUse:
    - matcher: "Write|Edit|Bash"
      hooks:
        - type: command
          command: |
            # [tasks-planning] PreToolUse Hook Triggered
            SCRIPT_DIR=".claude/skills/tasks-planning/scripts"
            TASKS_FILE=".claude/tmp/tasks/tasks.json"
            TASKS_DIR=".claude/tmp/tasks"

            echo "=== [tasks-planning] PreToolUse Hook ==="

            # Check 1: Is there a task created?
            if [ ! -f "$TASKS_FILE" ]; then
              echo "⚠️  [tasks-planning] No task found!"
              echo "   Please create a task first:"
              echo "   python3 ${SCRIPT_DIR}/tasks-manager.py create \"Task Name\""
              echo "   Then create task_plan.md, findings.md, progress.md"
              exit 0
            fi

            # Check 2: Is there an active task?
            TASK_OUTPUT=$(python3 "${SCRIPT_DIR}/tasks-manager.py" pending 2>/dev/null)
            PENDING=$(echo "$TASK_OUTPUT" | grep -c "🔄" || echo "0")
            if [ "$PENDING" = "0" ]; then
              echo "⚠️  [tasks-planning] No active task!"
              echo "   Run: python3 ${SCRIPT_DIR}/tasks-manager.py list"
              echo "   Start a task: python3 ${SCRIPT_DIR}/tasks-manager.py start <task-id>"
              exit 0
            fi

            # Check 3: Find task directory and check if task_plan.md exists
            TASK_ID=$(echo "$TASK_OUTPUT" | grep "🔄" | awk '{print $1}' | head -1)
            TASK_DIR=""
            # Search in all date directories for the task
            for DATE_DIR in "$TASKS_DIR"/*/; do
              if [ -d "${DATE_DIR}${TASK_ID}" ]; then
                TASK_DIR="${DATE_DIR}${TASK_ID}"
                break
              fi
            done

            if [ -z "$TASK_DIR" ] || [ ! -f "$TASK_DIR/task_plan.md" ]; then
              echo "⚠️  [tasks-planning] Planning files not found!"
              echo "   Create planning files in: ${TASKS_DIR}/<date>/${TASK_ID}/"
              echo "   Required files: task_plan.md, findings.md, progress.md"
              exit 0
            fi

            # Show current task status
            echo "✅ [tasks-planning] Task initialized, proceeding..."
            echo ""
            echo "=== Current Task Status ==="
            echo "$TASK_OUTPUT" | head -3

            # Show phase progress
            echo ""
            echo "=== Current Phase ==="
            grep -E "Phase|Status:" "$TASK_DIR/task_plan.md" 2>/dev/null | head -30 || true
  PostToolUse:
    - matcher: "Write|Edit"
      hooks:
        - type: command
          command: |
            echo "[tasks-planning] File modified"
            SCRIPT_DIR=".claude/skills/tasks-planning/scripts"
            TASKS_FILE=".claude/tmp/tasks/tasks.json"
            TASKS_DIR=".claude/tmp/tasks"

            # Get current task ID
            TASK_ID=$(python3 "${SCRIPT_DIR}/tasks-manager.py" pending 2>/dev/null | grep "🔄" | awk '{print $1}' | head -1)
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

            # Suggest updating task_plan.md if a phase is complete
            if [ -n "$TASK_DIR" ] && [ -f "$TASK_DIR/task_plan.md" ]; then
              CURRENT_PHASE=$(grep "Phase" "$TASK_DIR/task_plan.md" 2>/dev/null | grep -c "in_progress" || echo "0")
              if [ "$CURRENT_PHASE" != "0" ]; then
                echo "💡 [tasks-planning] Phase in progress - consider updating task_plan.md status when complete"
              fi
            fi
  Stop:
    - hooks:
        - type: command
          command: |
            echo "[tasks-planning] Stop Hook Triggered"
            SCRIPT_DIR=".claude/skills/tasks-planning/scripts"

            # Check task completion
            bash "${SCRIPT_DIR}/check-complete.sh"

            # End session for in-progress tasks
            TASK_ID=$(python3 "${SCRIPT_DIR}/tasks-manager.py" pending 2>/dev/null | grep "🔄" | awk '{print $1}' | head -1)
            if [ -n "$TASK_ID" ]; then
              python3 "${SCRIPT_DIR}/tasks-manager.py" end-session "$TASK_ID" 2>/dev/null
              echo "[tasks-planning] Session ended for task: $TASK_ID"
            else
              echo "[tasks-planning] No active task to end session"
            fi
---

# Tasks Planning

Work like Manus: Use persistent markdown files as your "working memory on disk."

## The Core Pattern

```
Context Window = RAM (volatile, limited)
Filesystem = Disk (persistent, unlimited)

→ Anything important gets written to disk.
→ Tasks and sessions tracked in tasks.json.
```

## Important: Where Files Go

| Location | What Goes There |
|----------|-----------------|
| Skill directory (`./`) | Templates, scripts, reference docs |
| `.claude/tmp/tasks/tasks.json` | Task registry with session history |
| `.claude/tmp/tasks/YYYY-MM-DD/task-name/` | `task_plan.md`, `findings.md`, `progress.md` |

### tasks.json Structure

```json
{
  "version": "1.0",
  "tasks": [
    {
      "id": "user-export-import",
      "name": "用户导出导入",
      "startTime": "2026-01-18T10:00:00",
      "endTime": null,
      "status": "in_progress",
      "sessions": [
        {
          "sessionId": "508e13a8-d3b7-449c-a7db-85c8e0675431",
          "startTime": "2026-01-18T10:00:00",
          "endTime": null
        }
      ]
    }
  ]
}
```

> **Note:** sessionId uses UUID format (e.g., `508e13a8-d3b7-449c-a7db-85c8e0675431`) to match Claude Code session files.

## Workflow

### Step 1: Check for Pending Tasks

**At the start of a session**, check for incomplete tasks:

```bash
python3 scripts/tasks-manager.py pending
```

### Step 2: Ask User (Use AskUserQuestion Tool)

Use the **AskUserQuestion** tool to present pending tasks and options:

```python
{
  "questions": [
    {
      "question": "您有 2 个未完成的任务。是否继续之前的任务，还是开始新的任务？",
      "header": "任务选择",
      "options": [
        {"label": "继续用户导出导入", "description": "从上次停止的地方继续，已完成 3/5 阶段"},
        {"label": "开始新任务", "description": "创建新的任务文件夹和规划文件"}
      ],
      "multiSelect": false
    }
  ]
}
```

### Step 3: Execute Selected Task

**If continuing a task:**
1. Get current session ID: `python3 tasks-manager.py current-session`
2. Add session to task: `python3 tasks-manager.py add-session <task-id> <session-id>`
3. Read existing planning files from `.claude/tmp/tasks/{date}/{task}/`
4. Run `python3 session-catchup.py --task <task-id>` to get context from previous sessions
5. Resume work

**If starting a new task:**
1. Create task: `python3 tasks-manager.py create "任务名称"`
2. Start task: `python3 tasks-manager.py start <task-id>`
3. Create planning files in `.claude/tmp/tasks/$(date +%Y-%m-%d)/<task-id>/`
4. Get current session ID: `python3 tasks-manager.py current-session`
5. Initialize session: `python3 tasks-manager.py add-session <task-id> <session-id>`

### Step 4: Complete Task

When all phases are complete:
```bash
python3 tasks-manager.py complete <task-id>
```

## File Purposes

| File | Purpose | When to Update |
|------|---------|----------------|
| `tasks.json` | Task-session registry | Session start/end |
| `task_plan.md` | Phases, progress, decisions | After each phase |
| `findings.md` | Research, discoveries | After ANY discovery |
| `progress.md` | Session log, test results | Throughout session |

## Critical Rules

### 1. Create Task First
Never start a complex task without creating a task entry in tasks.json.

### 2. Track Every Session
At session start:
1. Load context from previous sessions: `python3 session-catchup.py --task <task-id>`
2. Get current session ID: `python3 tasks-manager.py current-session`
3. Add session to task: `python3 tasks-manager.py add-session <task-id> <session-id>`

### 3. The 2-Action Rule
> "After every 2 view/browser/search operations, IMMEDIATELY save key findings to text files."

### 4. Read Before Decide
Before major decisions, read the plan file.

### 5. Update After Act
After completing a phase:
- Mark phase status: `in_progress` → `complete`
- Log errors encountered
- Note files created/modified

### 6. Log ALL Errors
Every error goes in the plan file.

### 7. Never Repeat Failures
Track what you tried. Mutate the approach.

## The 3-Strike Error Protocol

```
ATTEMPT 1: Diagnose & Fix
  → Read error carefully
  → Identify root cause
  → Apply targeted fix

ATTEMPT 2: Alternative Approach
  → Same error? Try different method
  → NEVER repeat exact same failing action

ATTEMPT 3: Broader Rethink
  → Question assumptions
  → Search for solutions
  → Consider updating the plan

AFTER 3 FAILURES: Escalate to User
```

## Read vs Write Decision Matrix

| Situation | Action | Reason |
|-----------|--------|--------|
| Just wrote a file | DON'T read | Content still in context |
| Viewed image/PDF | Write findings NOW | Multimodal → text before lost |
| Browser returned data | Write to file | Screenshots don't persist |
| Starting new phase | Read plan/findings | Re-orient if context stale |
| Error occurred | Read relevant file | Need current state to fix |
| Resuming after gap | Read all planning files | Recover state |

## The 5-Question Reboot Test

If you can answer these, your context management is solid:

| Question | Answer Source |
|----------|---------------|
| Where am I? | Current phase in task_plan.md |
| Where am I going? | Remaining phases |
| What's the goal? | Goal statement in plan |
| What have I learned? | findings.md |
| What have I done? | progress.md + tasks.json sessions |

## When to Use This Pattern

**Use for:**
- Multi-step tasks (3+ steps)
- Research tasks
- Building/creating projects
- Tasks spanning many tool calls
- Anything requiring organization

**Skip for:**
- Simple questions
- Single-file edits
- Quick lookups

## Templates

Copy these templates to start:

- [templates/task_plan.md](templates/task_plan.md) — Phase tracking
- [templates/findings.md](templates/findings.md) — Research storage
- [templates/progress.md](templates/progress.md) — Session logging

## Scripts Reference

Helper scripts for automation (in skill's scripts/ directory):

| Script | Purpose |
|--------|---------|
| `tasks-manager.py` | Task management: create, start, complete, session tracking |
| `check-complete.sh` | Verify all phases complete |
| `session-catchup.py` | Load context from previous sessions (up to 100 messages) |
| `init-session.sh` | Auto-called by `tasks-manager.py create` to initialize planning files |

### tasks-manager.py Commands

```bash
# List all tasks
python3 scripts/tasks-manager.py list

# List pending tasks
python3 scripts/tasks-manager.py pending

# Create a new task
python3 scripts/tasks-manager.py create "任务名称"

# Show task details with sessions
python3 scripts/tasks-manager.py show <task-id>

# Start a task (mark as in_progress)
python3 scripts/tasks-manager.py start <task-id>

# Complete a task
python3 scripts/tasks-manager.py complete <task-id>

# Add session to task
python3 scripts/tasks-manager.py add-session <task-id> <session-id>

# Get current session ID
python3 scripts/tasks-manager.py current-session

# End current session
python3 scripts/tasks-manager.py end-session <task-id>

# Show task statistics
python3 scripts/tasks-manager.py stats

# Update task name
python3 scripts/tasks-manager.py update <task-id> "新名称"

# Delete a task
python3 scripts/tasks-manager.py delete <task-id>
```

### session-catchup.py Commands

```bash
# Check for pending tasks (default behavior)
python3 scripts/session-catchup.py

# Get unsynced context for a specific task
python3 scripts/session-catchup.py --task <task-id>
```

**What it does:**
- Loads up to 100 key messages from previously recorded sessions
- Prioritizes user messages with content and Claude summaries with tool calls
- Helps model understand previous work when resuming a task

## Anti-Patterns

| Don't | Do Instead |
|-------|------------|
| Use TodoWrite for persistence | Create task_plan.md + tasks.json |
| Start without checking tasks | Run `tasks-manager.py pending` first |
| Forget to track sessions | Always call `add-session` at start |
| State goals once and forget | Re-read plan before decisions |
| Hide errors and retry silently | Log errors to plan file |
| Stuff everything in context | Store large content in files |
| Start executing immediately | Create task FIRST |
| Repeat failed actions | Track attempts, mutate approach |
| Create files in skill directory | Create files in `.claude/tmp/tasks/` |
