---
name: tasks-planning
version: "2.8.0"
description: Implements Manus-style file-based planning for complex tasks with task-session tracking. Creates task_plan.md, findings.md, and progress.md. Uses tasks.json to track task-session relationships across sessions. Includes hooks for task initialization validation and session management (externalized to separate scripts).
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
          command: bash ${CLAUDE_PROJECT_DIR}/.claude/skills/tasks-planning/scripts/hooks-pre-tool-use.sh
  PostToolUse:
    - matcher: "Write|Edit"
      hooks:
        - type: command
          command: bash ${CLAUDE_PROJECT_DIR}/.claude/skills/tasks-planning/scripts/hooks-post-tool-use.sh
  Stop:
    - hooks:
        - type: command
          command: bash ${CLAUDE_PROJECT_DIR}/.claude/skills/tasks-planning/scripts/hooks-stop.sh
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

## Workflow

### Step 1: Check for Pending Tasks

**At the start of a session**, check for incomplete tasks:

```bash
python3 scripts/tasks-manager.py pending
```

### Step 2: Ask User (Use AskUserQuestion Tool)

Use the **AskUserQuestion** tool to present pending tasks and options.

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

### Step 5: Create Plan
Never start a task without task_plan.md. Non-negotiable.

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
