#!/usr/bin/env zsh
# generate-sample-data.zsh — one-shot J-Quants sample-data loader.
#
# Calls cl-jquants-api/scripts/generate-sample-data.lisp via SBCL,
# fetching the configured endpoints for a specific list of stock codes
# over a date range, and writing them as the same date-partitioned
# parquet layout amx-cache reads in production.
#
# Usage:
#   ./generate-sample-data.zsh CODES START_DATE [END_DATE] [DATA_DIR]
#
#   CODES       Comma-separated 4-digit JP codes, no spaces.
#               e.g. "7203,6758,9984"  (Toyota, Sony, SoftBank)
#   START_DATE  YYYY-MM-DD or YYYYMMDD. Earliest date, inclusive.
#               e.g. "2024-01-01"
#   END_DATE    Optional. Latest date, inclusive. Default: yesterday.
#   DATA_DIR    Optional. Output root. Default: $HOME/amx-data.
#
# Prerequisites:
#   - JQUANTS_API_KEY in env (you have it in ~/.zshenv).
#   - sbcl on PATH, with Quicklisp set up for the user (~/quicklisp).
#   - cl-jquants-api in ~/quicklisp/local-projects (already symlinked).
#
# Examples:
#   ./generate-sample-data.zsh "7203,6758,9984" 2024-01-01
#   ./generate-sample-data.zsh "7203,6758,9984" 2024-01-01 2024-12-31
#   ./generate-sample-data.zsh "7203" 2025-01-01 "" /Volumes/ext/amx-data

set -euo pipefail

# ── arg parsing ────────────────────────────────────────────────────────────
if [[ $# -lt 2 ]]; then
  cat >&2 <<'USAGE'
Usage: generate-sample-data.zsh CODES START_DATE [END_DATE] [DATA_DIR]

  CODES       Comma-separated 4-digit JP codes (e.g. "7203,6758,9984")
  START_DATE  YYYY-MM-DD or YYYYMMDD (e.g. "2024-01-01")
  END_DATE    Optional. Default: yesterday. Use "" to skip and set DATA_DIR.
  DATA_DIR    Optional. Default: $HOME/amx-data.

Requires JQUANTS_API_KEY in env.
USAGE
  exit 2
fi

CODES="$1"
START_DATE="$2"
END_DATE="${3:-}"
DATA_DIR="${4:-$HOME/amx-data}"

# ── sanity checks ──────────────────────────────────────────────────────────
if [[ -z "${JQUANTS_API_KEY:-}" ]]; then
  echo "ERROR: JQUANTS_API_KEY is not set in this shell (.zshenv didn't load?)." >&2
  exit 1
fi

if ! command -v sbcl >/dev/null 2>&1; then
  echo "ERROR: sbcl not found on PATH." >&2
  exit 1
fi

SCRIPT_DIR="${0:A:h}"
LISP_FILE="$SCRIPT_DIR/generate-sample-data.lisp"
if [[ ! -f "$LISP_FILE" ]]; then
  echo "ERROR: Lisp script not found: $LISP_FILE" >&2
  exit 1
fi

# Convert "7203,6758,9984" → ("7203" "6758" "9984")
CODES_LISP="("
for c in ${(s:,:)CODES}; do
  c="${c//[[:space:]]/}"
  [[ -z "$c" ]] && continue
  if ! [[ "$c" =~ ^[0-9A-Z]+$ ]]; then
    echo "ERROR: invalid code in CODES: '$c'" >&2
    exit 1
  fi
  CODES_LISP+="\"$c\" "
done
CODES_LISP="${CODES_LISP% })"

# Build optional :end-date keyword
END_ARG=""
if [[ -n "$END_DATE" ]]; then
  END_ARG=":end-date \"$END_DATE\""
fi

mkdir -p "$DATA_DIR"

# ── banner ────────────────────────────────────────────────────────────────
print -r -- "── J-Quants sample-data loader ──"
print -r -- "  codes      : $CODES"
print -r -- "  start date : $START_DATE"
print -r -- "  end date   : ${END_DATE:-(yesterday)}"
print -r -- "  data dir   : $DATA_DIR"
print -r -- "  lisp       : $LISP_FILE"
print

# ── run ────────────────────────────────────────────────────────────────────
exec sbcl --non-interactive --disable-debugger \
  --load "$LISP_FILE" \
  --eval "(amx-sample:generate-sample-data-for-stocks
            :data-dir \"$DATA_DIR\"
            :codes '$CODES_LISP
            :start-date \"$START_DATE\"
            $END_ARG)"
