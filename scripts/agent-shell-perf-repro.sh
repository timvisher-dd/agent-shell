#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

EMACS_COMMAND="${AGENT_SHELL_REPRO_EMACS:-${EMACS_COMMAND:-emacs}}"
SOFT_TIMEOUT="${AGENT_SHELL_REPRO_TIMEOUT:-90}"
LOG_DIR="${AGENT_SHELL_REPRO_LOG_DIR:-$ROOT/.agent-shell/transcripts}"
LABEL="${AGENT_SHELL_REPRO_LABEL:-}" 

AGENT_DIR="${AGENT_SHELL_REPRO_AGENT_DIR:-}"
SHELL_MAKER_DIR="${AGENT_SHELL_REPRO_SHELL_MAKER_DIR:-}"
ACP_DIR="${AGENT_SHELL_REPRO_ACP_DIR:-}"

WARMUP_MESSAGE="${AGENT_SHELL_REPRO_WARMUP_MESSAGE:-}"
MEASURE_MESSAGE="${AGENT_SHELL_REPRO_MESSAGE:-}"
POLL_INTERVAL="${AGENT_SHELL_REPRO_POLL_INTERVAL:-}"
CWD_OVERRIDE="${AGENT_SHELL_REPRO_CWD:-}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --agent-shell)
      AGENT_DIR="$2"; shift 2;;
    --shell-maker)
      SHELL_MAKER_DIR="$2"; shift 2;;
    --acp)
      ACP_DIR="$2"; shift 2;;
    --label)
      LABEL="$2"; shift 2;;
    --timeout)
      SOFT_TIMEOUT="$2"; shift 2;;
    --poll)
      POLL_INTERVAL="$2"; shift 2;;
    --warmup)
      WARMUP_MESSAGE="$2"; shift 2;;
    --message)
      MEASURE_MESSAGE="$2"; shift 2;;
    --cwd)
      CWD_OVERRIDE="$2"; shift 2;;
    --emacs)
      EMACS_COMMAND="$2"; shift 2;;
    --log-dir)
      LOG_DIR="$2"; shift 2;;
    -h|--help)
      cat <<'USAGE'
Usage: scripts/agent-shell-perf-repro.sh [options]

Options:
  --agent-shell DIR   Path to agent-shell checkout
  --shell-maker DIR   Path to shell-maker checkout
  --acp DIR           Path to acp.el checkout
  --label LABEL       Label for log file naming
  --timeout SECONDS   Soft timeout (default: 90)
  --poll SECONDS      Poll interval (default: 0.1)
  --warmup MESSAGE    Warmup message
  --message MESSAGE   Measure message
  --cwd DIR           Working directory for session
  --emacs CMD         Emacs command (default: emacs)
  --log-dir DIR       Log directory

Environment variables mirror these options with AGENT_SHELL_REPRO_* names.
Requires: op CLI (1Password) to inject OPENAI_API_KEY.
USAGE
      exit 0;;
    *)
      echo "Unknown arg: $1" >&2
      exit 1;;
  esac
done

HARD_TIMEOUT="${AGENT_SHELL_REPRO_HARD_KILL_SECONDS:-$((SOFT_TIMEOUT + 5))}"

if ! command -v op >/dev/null 2>&1; then
  echo "op CLI is required for this repro script." >&2
  exit 1
fi

if [[ -z "${OPENAI_API_KEY:-}" ]]; then
  echo "OPENAI_API_KEY must be set (op:// reference or resolved value)." >&2
  exit 1
fi

if [[ -z "${ANTHROPIC_API_KEY:-}" ]]; then
  echo "ANTHROPIC_API_KEY must be set (op:// reference or resolved value)." >&2
  exit 1
fi

mkdir -p "$LOG_DIR"

SAFE_LABEL=""
if [[ -n "$LABEL" ]]; then
  SAFE_LABEL="$(echo "$LABEL" | tr -c 'A-Za-z0-9._-' '_')_"
fi

LOG_FILE="$LOG_DIR/agent-shell-perf-repro-${SAFE_LABEL}$(date +%Y%m%d-%H%M%S).log"

export AGENT_SHELL_REPRO_TIMEOUT="$SOFT_TIMEOUT"
export AGENT_SHELL_REPRO_AGENT_DIR="$AGENT_DIR"
export AGENT_SHELL_REPRO_SHELL_MAKER_DIR="$SHELL_MAKER_DIR"
export AGENT_SHELL_REPRO_ACP_DIR="$ACP_DIR"

if [[ -n "$WARMUP_MESSAGE" ]]; then
  export AGENT_SHELL_REPRO_WARMUP_MESSAGE="$WARMUP_MESSAGE"
fi
if [[ -n "$MEASURE_MESSAGE" ]]; then
  export AGENT_SHELL_REPRO_MESSAGE="$MEASURE_MESSAGE"
fi
if [[ -n "$POLL_INTERVAL" ]]; then
  export AGENT_SHELL_REPRO_POLL_INTERVAL="$POLL_INTERVAL"
fi
if [[ -n "$CWD_OVERRIDE" ]]; then
  export AGENT_SHELL_REPRO_CWD="$CWD_OVERRIDE"
fi

op run --no-masking -- "$EMACS_COMMAND" --batch -l "$ROOT/scripts/agent-shell-perf-repro.el" >"$LOG_FILE" 2>&1 &

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
