# Repository Guidelines

Keep modules cohesive and easy to load on every workstation.

## Project Structure & Module Organization
Modules live in `*-settings.el`. Helpers stay in `mcp-tools/`. `.beads/` is bd-managed.

## Build, Test, and Development Commands
`emacs --batch -Q -L . -l <module>.el -f batch-byte-compile <module>.el` — compile before committing. Run `emacs --batch -Q -l ert -l tests/run-tests.el -f ert-run-tests-batch-and-exit` when an ERT harness exists.

## Coding Style & Naming Conventions
Two-space indent, rely on `indent-region`. Prefix symbols with `lmoneda/`. Defer tweaks with `with-eval-after-load` inside `use-package` blocks.

## Testing Guidelines
Add ERT specs under `tests/elisp/` and reference them from `tests/run-tests.el`. Capture manual validation steps in pull request notes.

## Commit & Pull Request Guidelines
Write imperative, single-module commits. PRs should list touched files, bd issue IDs, validation evidence, and follow-ups via `bd create ... --deps discovered-from:<id>`.

## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Auto-syncs to JSONL for version control
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**
```bash
bd ready --json
```

**Create new issues:**
```bash
bd create "Issue title" -t bug|feature|task -p 0-4 --json
bd create "Issue title" -p 1 --deps discovered-from:bd-123 --json
```

**Claim and update:**
```bash
bd update bd-42 --status in_progress --json
bd update bd-42 --priority 1 --json
```

**Complete work:**
```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`

### Auto-Sync

bd automatically syncs with git:
- Exports to `.beads/issues.jsonl` after changes (5s debounce)
- Imports from JSONL when newer (e.g., after `git pull`)
- No manual export/import needed!

### MCP Server (Recommended)

If using Claude or MCP-compatible clients, install the beads MCP server:

```bash
pip install beads-mcp
```

Add to MCP config (e.g., `~/.config/claude/config.json`):
```json
{
  "beads": {
    "command": "beads-mcp",
    "args": []
  }
}
```

Then use `mcp__beads__*` functions instead of CLI commands.

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems

For more details, see README.md and QUICKSTART.md.

### Async Org Notebook Workflow (Agents)

- Always call `send-code-to-jupyter-repl-async` with a metadata map that at minimum includes `file_path`, `heading_title`, and any relevant `bd_id` so the run can be recovered later. Capture both the returned `request_id` and `kernel_msg_id` in your reasoning.
- After launching a block, back off polling via `check-async-execution`; use `output_preview` and `result_preview` to share incremental updates without reopening buffers.
- Use `list-async-executions` whenever reconnecting or handing off to surface all pending work and confirm completions; follow any `follow_up` hints by creating or closing the linked bd reminder.
- Pair polling with `get-jupyter-kernel-state` to confirm the kernel is still healthy; escalate if the kernel is disconnected or `pending_async_requests` stops changing.
- Once a block completes, persist results back into the notebook (or summarize) and close the reminder in bd before moving on.
