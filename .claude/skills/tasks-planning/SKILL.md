---
name: tasks-planning
version: "2.5.0"
description: Implements Manus-style file-based planning for complex tasks with task-session tracking. Creates task_plan.md, findings.md, and progress.md. Uses tasks.json to track task-session relationships across sessions.
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
    - matcher: "Write|Edit|Bash|Read|Glob|Grep"
      hooks:
        - type: command
          command: |
            # Find and display current task_plan.md
            TASK_DIR=""
            if [ -f ".claude/tmp/tasks/tasks.json" ]; then
              TASK_DIR=$(python3 .claude/skills/tasks-planning/scripts/tasks-manager.py pending 2>/dev/null | head -5 || true)
            fi
            if [ -z "$TASK_DIR" ] && [ -f ".claude/tmp/tasks/$(date +%Y-%m-%d)/*/task_plan.md" ]; then
              cat .claude/tmp/tasks/$(date +%Y-%m-%d)/*/task_plan.md 2>/dev/null | head -30
            fi || true
  PostToolUse:
    - matcher: "Write|Edit"
      hooks:
        - type: command
          command: "echo '[planning-with-files] File updated. If this completes a phase, update task_plan.md status.'"
  Stop:
    - hooks:
        - type: command
          command: |
            SKILL_DIR="$(cd "$(dirname "$0")" && pwd)"
            bash "${SKILL_DIR}/scripts/check-complete.sh"
            python3 "${SKILL_DIR}/scripts/tasks-manager.py" pending 2>/dev/null | grep "🔄" | awk '{print $1}' | head -1 | xargs -I {} bash -c 'python3 "${SKILL_DIR}/scripts/tasks-manager.py" end-session {}' 2>/dev/null || true
---

# Planning with Files (v2.5.0)

Work like Manus: Use persistent markdown files as your "working memory on disk."

## Task Management Overview (v2.5.0)

This version introduces **tasks.json** for tracking task-session relationships:

```
.claude/tmp/tasks/
├── tasks.json              # Task registry with session history
├── 2026-01-18/
│   └── user-export-import/
│       ├── task_plan.md
│       ├── findings.md
│       └── progress.md
└── 2026-01-19/
    └── feature-login/
        ├── task_plan.md
        ├── findings.md
        └── progress.md
```

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
          "sessionId": "session-abc123",
          "startTime": "2026-01-18T10:00:00",
          "endTime": "2026-01-18T11:30:00"
        }
      ]
    }
  ]
}
```

## New Session Workflow

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
1. Add current session to task: `python3 tasks-manager.py add-session <task-id> <session-id>`
2. Read existing planning files from `.claude/tmp/tasks/{date}/{task}/`
3. Run `python3 session-catchup.py` to get unsynced context
4. Resume work

**If starting a new task:**
1. Create task: `python3 tasks-manager.py create "任务名称"`
2. Start task: `python3 tasks-manager.py start <task-id>`
3. Create planning files in `.claude/tmp/tasks/$(date +%Y-%m-%d)/<task-id>/`
4. Initialize session: `python3 tasks-manager.py add-session <task-id> <session-id>`

### Step 4: Complete Task

When all phases are complete:
```bash
python3 tasks-manager.py complete <task-id>
```

## Important: Where Files Go

| Location | What Goes There |
|----------|-----------------|
| Skill directory (`./`) | Templates, scripts, reference docs |
| `.claude/tmp/tasks/tasks.json` | Task registry with session history |
| `.claude/tmp/tasks/YYYY-MM-DD/task-name/` | `task_plan.md`, `findings.md`, `progress.md` |

## Quick Start (Updated)

Before ANY complex task:

1. **Check pending tasks** — Run `python3 scripts/tasks-manager.py pending`
2. **Ask user** — Use AskUserQuestion to select or create task
3. **Create task directory** — `.claude/tmp/tasks/$(date +%Y-%m-%d)/{task-name}/`
4. **Create `task_plan.md`** — Use [templates/task_plan.md](templates/task_plan.md) as reference
5. **Create `findings.md`** — Use [templates/findings.md](templates/findings.md) as reference
6. **Create `progress.md`** — Use [templates/progress.md](templates/progress.md) as reference
7. **Re-read plan before decisions** — Refreshes goals in attention window
8. **Update after each phase** — Mark complete, log errors

> **Note:** Planning files go in `.claude/tmp/tasks/YYYY-MM-DD/task-name/`, not the skill installation folder.

## The Core Pattern

```
Context Window = RAM (volatile, limited)
Filesystem = Disk (persistent, unlimited)

→ Anything important gets written to disk.
→ Tasks and sessions tracked in tasks.json.
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
At session start: `add-session <task-id> <session-id>`
At session end: `end-session <task-id>`

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

## Scripts

Helper scripts for automation (in skill's scripts/ directory):

| Script | Purpose |
|--------|---------|
| `tasks-manager.py` | Task management: create, start, complete, session tracking (auto-creates planning files) |
| `check-complete.sh` | Verify all phases complete |
| `session-catchup.py` | Recover context from previous sessions (unsynced) |

> **Note:** `init-session.sh` is automatically called by `tasks-manager.py create`.

### tasks-manager.py Commands

```bash
# List all tasks
python3 scripts/tasks-manager.py list

# List pending tasks
python3 scripts/tasks-manager.py pending

# Create a new task
python3 scripts/tasks-manager.py create "任务名称"

# Start a task (mark as in_progress)
python3 scripts/tasks-manager.py start <task-id>

# Complete a task
python3 scripts/tasks-manager.py complete <task-id>

# Add session to task
python3 scripts/tasks-manager.py add-session <task-id> <session-id>

# End current session
python3 scripts/tasks-manager.py end-session <task-id>
```

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
