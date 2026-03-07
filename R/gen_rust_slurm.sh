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
    args <- build_rust_args(cfg, paths, file.path(paths\$data_raw, basename(cfg\$raw_csv)))
    cat(binary, '\n')
    cat(paste(shQuote(args), collapse = ' \\\\\n    '), '\n')
  ")

  local binary
  binary=$(echo "$rust_info" | head -1)
  local rust_args
  rust_args=$(echo "$rust_info" | tail -n +2)

  cat > "$output_script" << EOF
#!/bin/bash
#SBATCH --job-name=mv_rust_process
#SBATCH --cpus-per-task=${cpus}
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=${mem_gb}G
#SBATCH --partition=${partition}
#SBATCH --output=${log_dir}/rust_processing_%j.out
#SBATCH --error=${log_dir}/rust_processing_%j.err

set -euo pipefail

N_CPUS="\${SLURM_CPUS_PER_TASK:-${cpus}}"

echo "[\$(date)] Rust processor on \$(hostname), \${N_CPUS} CPUs"

export RAYON_NUM_THREADS="\${N_CPUS}"
export RUST_BACKTRACE=1

${binary} \\
    ${rust_args} 2>&1 | tee ${log_dir}/rust_debug_output.log

echo "[\$(date)] Done: \$(find ${project_root}/data/processed -name 'processed__*.parquet' | wc -l) files"
EOF

  chmod +x "$output_script"
}
