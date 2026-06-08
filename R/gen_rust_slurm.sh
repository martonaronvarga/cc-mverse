#!/usr/bin/env bash
# gen_rust_slurm.sh — Generate the Rust processing SLURM script

generate_rust_script() {
  local output_script="$1"
  local cpus="$2"
  local mem_gb="$3"
  local partition="$4"
  local log_dir="$5"
  local r_dir="$6"
  local config_yaml="$7"
  local time_min="${8:-7200}"
  # r_dir == PROJECT_ROOT; use it directly for the data path below.
  local project_root="$r_dir"

  local node_max
  node_max=$(sinfo -p "$partition" -o "%c" --noheader 2>/dev/null \
    | grep -v -E '^(N/A)?$' | sort -n | tail -1 | tr -d '[:space:]') || true

  if [[ -n "$node_max" && "$cpus" -gt "$node_max" ]]; then
    echo "[gen_rust_slurm] WARNING: slurm.rust.cpus=${cpus} exceeds" \
         "node max ${node_max} for partition '${partition}'." \
         "SLURM will reject the job. Fix slurm.rust.cpus in pipeline.yaml." >&2
  fi

  local rust_info
  rust_info=$(cd "$r_dir" && ~/local/bin/Rscript --vanilla -e "
    invisible(lapply(list.files('functions', pattern = '\\\\.R$', full.names = TRUE), source))
    load_all_packages()
    cfg <- load_config('hpc', config_path = '${config_yaml}')
    paths <- init_project_paths('.')
    binary <- find_rust_binary(paths)
    project_root <- paths\$root %||% paths\$project_root
    input_csv <- if (grepl('^/', cfg\$raw_csv)) cfg\$raw_csv else file.path(project_root, cfg\$raw_csv)
    args <- build_rust_args(cfg, paths, input_csv)
    cat(binary, '\n')
    cat(paste(shQuote(args), collapse = ' \\\\\\\n    '), '\n')
  ")

  local binary
  binary=$(echo "$rust_info" | head -1)
  local rust_args
  rust_args=$(echo "$rust_info" | tail -n +2)

  local time_fmt
  if declare -F format_slurm_time >/dev/null 2>&1; then
    time_fmt=$(format_slurm_time "$time_min")
  else
    local days=$(( time_min / 1440 ))
    local rem=$(( time_min % 1440 ))
    local hours=$(( rem / 60 ))
    local mins=$(( rem % 60 ))
    if (( days > 0 )); then
      time_fmt=$(printf "%d-%02d:%02d:00" "$days" "$hours" "$mins")
    else
      time_fmt=$(printf "%02d:%02d:00" "$hours" "$mins")
    fi
  fi

  cat > "$output_script" << EOF_SCRIPT
#!/bin/bash
#SBATCH --job-name=mv_rust_process
#SBATCH --cpus-per-task=${cpus}
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=${mem_gb}G
#SBATCH --time=${time_fmt}
#SBATCH --partition=${partition}
#SBATCH --output=${log_dir}/rust_processing_%j.out
#SBATCH --error=${log_dir}/rust_processing_%j.err

set -Eeuo pipefail

N_CPUS="\${SLURM_CPUS_PER_TASK:-${cpus}}"

log_msg() { echo "[\$(date -Is)] \$*"; }

log_msg "Rust processor on \$(hostname), \${N_CPUS} CPUs"

export RAYON_NUM_THREADS="\${N_CPUS}"
export RUST_BACKTRACE=1
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1

set +e
/usr/bin/time -v ${binary} \\
    ${rust_args} 2>&1 | tee ${log_dir}/rust_debug_output_\${SLURM_JOB_ID}.log
status=\${PIPESTATUS[0]}
set -e

if [[ "\${status}" -ne 0 ]]; then
  log_msg "Rust processor failed with status \${status}; cache signature not written"
  exit "\${status}"
fi

log_msg "Rust processor completed; writing processed-cache signature"
cd "${project_root}"
~/local/bin/Rscript --vanilla -e "
  invisible(lapply(list.files('functions', pattern = '\\\\.R$', full.names = TRUE), source))
  load_all_packages()
  cfg <- load_config('hpc', config_path = '${config_yaml}')
  paths <- init_project_paths('.')
  project_root <- paths\$root %||% paths\$project_root
  input_csv <- if (grepl('^/', cfg\$raw_csv)) cfg\$raw_csv else file.path(project_root, cfg\$raw_csv)
  branches <- generate_all_branches(cfg)
  write_processed_cache_signature(cfg, paths, branches, input_csv)
"

log_msg "Done: \$(find ${project_root}/data/processed -name 'processed__*.parquet' | wc -l) files"
EOF_SCRIPT

  chmod +x "$output_script"
}
