#!/bin/bash
# Initialize planning files for a new session (v2.5.0)
# Usage: ./init-session.sh [task-name]
# Creates files in .claude/tmp/tasks/YYYY-MM-DD/{task-name}/ and updates tasks.json

set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../../" && pwd)"  # 4 levels up to project root

TASK_NAME="${1:-project}"
DATE=$(date +%Y-%m-%d)

# Create the directory structure
TASK_DIR="$PROJECT_ROOT/.claude/tmp/tasks/$DATE/$TASK_NAME"
mkdir -p "$TASK_DIR"

echo "Initializing planning files for: $TASK_NAME"
echo "Location: $TASK_DIR"

# Create task_plan.md if it doesn't exist
if [ ! -f "$TASK_DIR/task_plan.md" ]; then
    cat > "$TASK_DIR/task_plan.md" << 'EOF'
# Task Plan: [Brief Description]

## Goal
[One sentence describing the end state]

## Current Phase
Phase 1

## Phases

### Phase 1: Requirements & Discovery
- [ ] Understand user intent
- [ ] Identify constraints
- [ ] Document in findings.md
- **Status:** in_progress

### Phase 2: Planning & Structure
- [ ] Define approach
- [ ] Create project structure
- **Status:** pending

### Phase 3: Implementation
- [ ] Execute the plan
- [ ] Write to files before executing
- **Status:** pending

### Phase 4: Testing & Verification
- [ ] Verify requirements met
- [ ] Document test results
- **Status:** pending

### Phase 5: Delivery
- [ ] Review outputs
- [ ] Deliver to user
- **Status:** pending

## Decisions Made
| Decision | Rationale |
|----------|-----------|

## Errors Encountered
| Error | Resolution |
|-------|------------|
EOF
    echo "Created task_plan.md"
else
    echo "task_plan.md already exists, skipping"
fi

# Create findings.md if it doesn't exist
if [ ! -f "$TASK_DIR/findings.md" ]; then
    cat > "$TASK_DIR/findings.md" << 'EOF'
# Findings & Decisions

## Requirements
-


## Research Findings
-

## Technical Decisions
| Decision | Rationale |
|----------|-----------|

## Issues Encountered
| Issue | Resolution |
|-------|------------|

## Resources
-
EOF
    echo "Created findings.md"
else
    echo "findings.md already exists, skipping"
fi

# Create progress.md if it doesn't exist
if [ ! -f "$TASK_DIR/progress.md" ]; then
    cat > "$TASK_DIR/progress.md" << EOF
# Progress Log

## Session: $DATE

### Current Status
- **Phase:** 1 - Requirements & Discovery
- **Started:** $DATE

### Actions Taken
-

### Test Results
| Test | Expected | Actual | Status |
|------|----------|--------|--------|

### Errors
| Error | Resolution |
|-------|------------|
EOF
    echo "Created progress.md"
else
    echo "progress.md already exists, skipping"
fi

echo ""
echo "Planning files initialized!"
echo "Location: $TASK_DIR"
echo "Files: task_plan.md, findings.md, progress.md"
echo ""
echo "Next steps:"
echo "1. Update tasks.json: python3 scripts/tasks-manager.py create \"$TASK_NAME\""
echo "2. Start the task: python3 scripts/tasks-manager.py start $TASK_NAME"
echo "3. Add session: python3 scripts/tasks-manager.py add-session $TASK_NAME <session-id>"
