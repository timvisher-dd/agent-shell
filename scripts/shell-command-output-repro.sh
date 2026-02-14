#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

SOFT_TIMEOUT="${SHELL_REPRO_TIMEOUT:-600}"
HARD_TIMEOUT="${SHELL_REPRO_HARD_KILL_SECONDS:-$((SOFT_TIMEOUT + 5))}"

DEFAULT_COMMAND="bash -lc 'for x in {1..35000}; do printf \"line %d\\n\" \"\$x\"; done'"

if [[ $# -gt 0 ]]; then
  COMMAND="$*"
else
  COMMAND="${SHELL_REPRO_COMMAND:-$DEFAULT_COMMAND}"
fi

if [[ -z "$COMMAND" ]]; then
  echo "shell output repro requires a command." >&2
  exit 1
fi

LOG_DIR="${SHELL_REPRO_LOG_DIR:-$ROOT/.agent-shell/transcripts}"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/shell-command-output-repro-$(date +%Y%m%d-%H%M%S).log"

EMACS_COMMAND="${TIMVISHER_EMACS_COMMAND:-emacs}"

SHELL_REPRO_COMMAND="$COMMAND" \
SHELL_REPRO_TIMEOUT="$SOFT_TIMEOUT" \
"$EMACS_COMMAND" --batch -l "$ROOT/scripts/shell-command-output-repro.el" >"$LOG_FILE" 2>&1 &

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
