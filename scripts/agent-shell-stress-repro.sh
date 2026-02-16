#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

SOFT_TIMEOUT="${AGENT_SHELL_STRESS_TIMEOUT:-90}"
HARD_TIMEOUT="${AGENT_SHELL_STRESS_HARD_KILL_SECONDS:-$((SOFT_TIMEOUT + 2))}"
WARMUP_MESSAGE="${AGENT_SHELL_STRESS_WARMUP_MESSAGE:-hi}"

DEFAULT_MESSAGE="$(cat <<'EOF'
!for x in {0..35000}; do printf "line %d\n" "$x"; done
EOF
)"

if [[ $# -gt 0 ]]; then
  MESSAGE="$*"
else
  MESSAGE="${AGENT_SHELL_STRESS_MESSAGE:-$DEFAULT_MESSAGE}"
fi

if [[ -z "$MESSAGE" ]]; then
  echo "stress repro requires a message or command." >&2
  exit 1
fi

if ! command -v op >/dev/null 2>&1; then
  echo "op CLI is required for this repro script." >&2
  exit 1
fi

LOG_DIR="${AGENT_SHELL_STRESS_LOG_DIR:-$ROOT/.agent-shell/transcripts}"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/stress-repro-$(date +%Y%m%d-%H%M%S).log"

EMACS_COMMAND="${TIMVISHER_EMACS_COMMAND:-emacs}"

AGENT_SHELL_STRESS_MESSAGE="$MESSAGE" \
AGENT_SHELL_STRESS_WARMUP_MESSAGE="$WARMUP_MESSAGE" \
AGENT_SHELL_STRESS_TIMEOUT="$SOFT_TIMEOUT" \
op run --no-masking -- "$EMACS_COMMAND" --batch -l "$ROOT/scripts/agent-shell-stress-repro.el" >"$LOG_FILE" 2>&1 &

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
