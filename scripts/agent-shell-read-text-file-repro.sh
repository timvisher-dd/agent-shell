#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEFAULT_REPRO_FILE="/Users/tim.visher/git.experiment.worktrees/timvisher-dd/beads/Fix-external-BEADS_DIR-repo-detection/x.txt"

REPRO_FILE="${1:-${AGENT_SHELL_LARGE_OUTPUT_FILE:-$DEFAULT_REPRO_FILE}}"
SOFT_TIMEOUT="${AGENT_SHELL_LARGE_OUTPUT_TIMEOUT:-90}"
HARD_TIMEOUT="${AGENT_SHELL_LARGE_OUTPUT_HARD_KILL_SECONDS:-$((SOFT_TIMEOUT + 2))}"
SEND_AFTER="${AGENT_SHELL_LARGE_OUTPUT_SEND_AFTER:-5}"

if [[ ! -f "$REPRO_FILE" ]]; then
  echo "repro file not found: $REPRO_FILE" >&2
  exit 1
fi

if ! command -v op >/dev/null 2>&1; then
  echo "op CLI is required for this repro script." >&2
  exit 1
fi

LOG_DIR="${AGENT_SHELL_LARGE_OUTPUT_LOG_DIR:-$ROOT/.agent-shell/transcripts}"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/read-text-file-repro-$(date +%Y%m%d-%H%M%S).log"

EMACS_COMMAND="${TIMVISHER_EMACS_COMMAND:-emacs}"

AGENT_SHELL_LARGE_OUTPUT_FILE="$REPRO_FILE" \
AGENT_SHELL_LARGE_OUTPUT_TIMEOUT="$SOFT_TIMEOUT" \
AGENT_SHELL_LARGE_OUTPUT_SEND_AFTER="$SEND_AFTER" \
op run --no-masking -- "$EMACS_COMMAND" --batch -l "$ROOT/scripts/agent-shell-large-output-repro.el" >"$LOG_FILE" 2>&1 &

EMACS_PID=$!

(
  sleep "$HARD_TIMEOUT"
  if kill -0 "$EMACS_PID" 2>/dev/null; then
    echo "Hard timeout reached; killing emacs pid $EMACS_PID" >>"$LOG_FILE"
    kill -9 "$EMACS_PID" || true
  fi
) &

wait "$EMACS_PID" || true

echo "Log: $LOG_FILE"
tail -n 5 "$LOG_FILE"
