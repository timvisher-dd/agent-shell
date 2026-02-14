#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

SOFT_TIMEOUT="${ACP_MINIMAL_TIMEOUT:-600}"
HARD_TIMEOUT="${ACP_MINIMAL_HARD_KILL_SECONDS:-$((SOFT_TIMEOUT + 5))}"

LOG_DIR="${ACP_MINIMAL_LOG_DIR:-$ROOT/.agent-shell/transcripts}"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/acp-minimal-stress-$(date +%Y%m%d-%H%M%S).log"

EMACS_COMMAND="${TIMVISHER_EMACS_COMMAND:-emacs}"

ACP_PROMPT_TIMEOUT="$SOFT_TIMEOUT" \
"$EMACS_COMMAND" --batch -l "$ROOT/scripts/acp-minimal-stress.el" >"$LOG_FILE" 2>&1 &

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
tail -n 10 "$LOG_FILE"
