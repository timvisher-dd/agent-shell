#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

SOFT_TIMEOUT="${AGENT_SHELL_COMMAND_OUTPUT_TIMEOUT:-90}"
HARD_TIMEOUT="${AGENT_SHELL_COMMAND_OUTPUT_HARD_KILL_SECONDS:-$((SOFT_TIMEOUT + 2))}"
SEND_AFTER="${AGENT_SHELL_COMMAND_OUTPUT_SEND_AFTER:-5}"

DEFAULT_MESSAGE="$(cat <<'EOF'
!bash -lc 'for x in {1..35000}; do printf "line %d\n" "$x"; done'
EOF
)"

if [[ $# -gt 0 ]]; then
  MESSAGE="$*"
else
  MESSAGE="${AGENT_SHELL_COMMAND_OUTPUT_MESSAGE:-$DEFAULT_MESSAGE}"
fi

if [[ -z "$MESSAGE" ]]; then
  echo "command output repro requires a message or command." >&2
  exit 1
fi

if [[ "${MESSAGE:0:1}" != "!" ]]; then
  MESSAGE="!$MESSAGE"
fi

if ! command -v op >/dev/null 2>&1; then
  echo "op CLI is required for this repro script." >&2
  exit 1
fi

LOG_DIR="${AGENT_SHELL_COMMAND_OUTPUT_LOG_DIR:-$ROOT/.agent-shell/transcripts}"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/command-output-repro-$(date +%Y%m%d-%H%M%S).log"

EMACS_COMMAND="${TIMVISHER_EMACS_COMMAND:-emacs}"

AGENT_SHELL_LARGE_OUTPUT_MESSAGE="$MESSAGE" \
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
