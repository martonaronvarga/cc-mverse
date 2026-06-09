#!/usr/bin/env bash
#SBATCH --job-name=model_chunk
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=5-00:00:00
#SBATCH --partition=hpc2019
#SBATCH --output=logs/model_chunk_%A_%a.out
#SBATCH --error=logs/model_chunk_%A_%a.err

set -euo pipefail

export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export RAYON_NUM_THREADS=1
export MALLOC_ARENA_MAX=2
export TAR_PROJECT="${SLURM_SUBMIT_DIR:-$(pwd)}"

cd "${SLURM_SUBMIT_DIR:-$(pwd)}"

if [[ -n "${BASHRC_SOURCE:-}" && -f "${BASHRC_SOURCE}" ]]; then
  # shellcheck disable=SC1090
  . "${BASHRC_SOURCE}" >/dev/null
elif [[ -f /mnt/st04pool/users/usumusu/.bashrc ]]; then
  # shellcheck disable=SC1091
  . /mnt/st04pool/users/usumusu/.bashrc >/dev/null
fi

RSCRIPT_BIN="${RSCRIPT:-}"
if [[ -z "$RSCRIPT_BIN" ]]; then
  if [[ -x "$HOME/local/bin/Rscript" ]]; then
    RSCRIPT_BIN="$HOME/local/bin/Rscript"
  else
    RSCRIPT_BIN="$(command -v Rscript)"
  fi
fi

worker_script="bin/fit_model_chunk_array.R"
if [[ ! -f "$worker_script" ]]; then
  worker_script="R/bin/fit_model_chunk_array.R"
fi

if [[ -n "${MODEL_CHUNK_ID_FILE:-}" ]]; then
  "$RSCRIPT_BIN" --vanilla "$worker_script" --chunk-id-file "${MODEL_CHUNK_ID_FILE}"
else
  "$RSCRIPT_BIN" --vanilla "$worker_script" --chunk-id "${SLURM_ARRAY_TASK_ID}"
fi
