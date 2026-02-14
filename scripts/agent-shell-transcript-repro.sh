#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PYTHON="${PYTHON:-python3}"

exec "$PYTHON" "$ROOT/scripts/agent-shell-transcript-repro.py" "$@"
