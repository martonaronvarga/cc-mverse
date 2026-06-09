#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Submit model chunks as independent SLURM array tasks.

Usage:
  bash R/bin/submit_model_chunk_array.sh [--concurrency N] [--workers N] [--all] [--dry-run]

Options:
  --concurrency N   Maximum simultaneously running array tasks (default: 200)
  --workers N       Alias for --concurrency
  --all             Submit all chunk IDs, not only missing output files
  --dry-run         Print the sbatch command and exit

Run from the R project directory after processed parquet files and _config exist.
EOF
}

concurrency=200
mode="missing"
dry_run=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    --concurrency|--workers)
      concurrency="$2"
      shift 2
      ;;
    --all)
      mode="all"
      shift
      ;;
    --dry-run)
      dry_run=true
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ ! "$concurrency" =~ ^[0-9]+$ || "$concurrency" -lt 1 ]]; then
  echo "--concurrency/--workers must be a positive integer" >&2
  exit 2
fi

if [[ ! -d functions || ! -d _config ]]; then
  echo "Run this from the R project directory with functions/ and _config/ present." >&2
  exit 1
fi

RSCRIPT_BIN="${RSCRIPT:-}"
if [[ -z "$RSCRIPT_BIN" ]]; then
  if [[ -x "$HOME/local/bin/Rscript" ]]; then
    RSCRIPT_BIN="$HOME/local/bin/Rscript"
  else
    RSCRIPT_BIN="$(command -v Rscript)"
  fi
fi

helper_script="bin/model_chunk_array_spec.R"
if [[ ! -f "$helper_script" ]]; then
  helper_script="R/bin/model_chunk_array_spec.R"
fi
array_spec=$("$RSCRIPT_BIN" --vanilla "$helper_script" --mode "$mode" | tail -n 1 | tr -d '[:space:]')
if [[ -z "$array_spec" ]]; then
  echo "No model chunks to submit."
  exit 0
fi

mkdir -p logs
submit=(
  sbatch
  "--job-name=model_chunk"
  "--array=${array_spec}%${concurrency}"
  "--output=logs/model_chunk_%A_%a.out"
  "--error=logs/model_chunk_%A_%a.err"
  "${SLURM_SCRIPT:-slurm_fit_model_chunk_array.sh}"
)

printf 'Submitting %s chunks with concurrency %s\n' "$mode" "$concurrency"
printf '%q ' "${submit[@]}"
printf '\n'

if [[ "$dry_run" == true ]]; then
  exit 0
fi

"${submit[@]}"
