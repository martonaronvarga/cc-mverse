#!/usr/bin/env bash
# multiverse.sh - Unified pipeline runner
# 
# Single source of truth: pipeline.yaml
# Bash reads it for SLURM resources. R reads it for analysis config.
#
# Usage:
#   ./multiverse.sh test
#   ./multiverse.sh local --workers 8
#   ./multiverse.sh hpc --workers 200 --rust-cpus 200
# 
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$SCRIPT_DIR"
CONFIG_YAML="${PROJECT_ROOT}/pipeline.yaml"
RESOLVED_YAML="${PROJECT_ROOT}/.pipeline.resolved.yaml"
R_DIR="${PROJECT_ROOT}"
LOG_DIR="${PROJECT_ROOT}/logs"

mkdir -p "$LOG_DIR"
source "${SCRIPT_DIR}/read_yaml.sh"

# ============================================================================
# HELPERS
# ============================================================================

ts()        { date +'%Y-%m-%d %H:%M:%S'; }
log_info()  { echo "[$(ts)] INFO:  $*"; }
log_error() { echo "[$(ts)] ERROR: $*" >&2; }

print_usage() {
  cat << 'EOF'
Usage: ./multiverse.sh [MODE] [OPTIONS]

Modes:  test | local | hpc

All options override the corresponding pipeline.yaml value for this run.

Data:
  --participants N         data.n_participants
  --trials N               data.n_trials

Execution:
  --workers N              modes.<mode>.n_workers
  --log-level LEVEL        logging.level

SLURM (hpc mode):
  --partition NAME         slurm.partition
  --controller-mem-gb N    slurm.controller.mem_gb
  --controller-time-min N  slurm.controller.time_min
  --worker-mem-gb N        slurm.worker.mem_gb
  --worker-time-min N      slurm.worker.time_min
  --rust-cpus N            slurm.rust.cpus
  --rust-mem-gb N          slurm.rust.mem_gb

Control:
  --force-rust             Re-run Rust even if output exists
  --email ADDRESS          SLURM notifications
  --dry-run                Show scripts without submitting
  --help                   This message
EOF
  exit 0
}

# ============================================================================
# PARSE CLI → YAML OVERRIDES
# ============================================================================

MODE="${1:-test}"
[[ "$MODE" == "--help" || "$MODE" == "-h" ]] && print_usage
shift || true

# Collect overrides as YAML path=value pairs
declare -A OVERRIDES
FORCE_RUST=false
EMAIL=""
DRY_RUN=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    --participants)        OVERRIDES[data.n_participants]="$2";         shift 2 ;;
    --trials)              OVERRIDES[data.n_trials]="$2";              shift 2 ;;
    --workers)             OVERRIDES[modes.${MODE}.n_workers]="$2";    shift 2 ;;
    --log-level)           OVERRIDES[logging.level]="$2";              shift 2 ;;
    --partition)           OVERRIDES[slurm.partition]="$2";            shift 2 ;;
    --controller-mem-gb)   OVERRIDES[slurm.controller.mem_gb]="$2";   shift 2 ;;
    --controller-time-min) OVERRIDES[slurm.controller.time_min]="$2"; shift 2 ;;
    --worker-mem-gb)       OVERRIDES[slurm.worker.mem_gb]="$2";       shift 2 ;;
    --worker-time-min)     OVERRIDES[slurm.worker.time_min]="$2";     shift 2 ;;
    --rust-cpus)           OVERRIDES[slurm.rust.cpus]="$2";           shift 2 ;;
    --rust-mem-gb)         OVERRIDES[slurm.rust.mem_gb]="$2";         shift 2 ;;
    --force-rust)          FORCE_RUST=true;                             shift   ;;
    --email)               EMAIL="$2";                                 shift 2 ;;
    --dry-run)             DRY_RUN=true;                                shift   ;;
    --help)                print_usage                                         ;;
    *)                     log_error "Unknown: $1"; exit 1                     ;;
  esac
done

# ============================================================================
# RESOLVE CONFIG: merge pipeline.yaml + CLI overrides → .pipeline.resolved.yaml
# ============================================================================

resolve_config() {
  # Build a Python one-liner that applies overrides to the YAML.
  # This is the ONLY place overrides are applied — everything downstream
  # reads the resolved file.
  local override_args=""
  for key in "${!OVERRIDES[@]}"; do
    override_args+="  '${key}': '${OVERRIDES[$key]}',"$'\n'
  done

  ~/local/python3.11.0/bin/python3 -c "
import yaml, copy, sys

with open('${CONFIG_YAML}') as f:
    cfg = yaml.safe_load(f)

overrides = {
${override_args}
}

def set_nested(d, dotpath, value):
    keys = dotpath.split('.')
    for k in keys[:-1]:
        d = d.setdefault(k, {})
    # Auto-convert numeric strings
    try:
        if '.' in str(value):
            value = float(value)
        else:
            value = int(value)
    except (ValueError, TypeError):
        pass
    d[keys[-1]] = value

for path, val in overrides.items():
    set_nested(cfg, path, val)

with open('${RESOLVED_YAML}', 'w') as f:
    yaml.dump(cfg, f, default_flow_style=False, sort_keys=False)
" || {
    # Fallback: if python3 not available, use R
    local override_r_list=""
    for key in "${!OVERRIDES[@]}"; do
      override_r_list+="'${key}' = '${OVERRIDES[$key]}', "
    done
    cd "$R_DIR" && ~/local/bin/Rscript --vanilla -e "
      cfg <- yaml::read_yaml('${CONFIG_YAML}')
      overrides <- list(${override_r_list} .dummy = NULL)
      overrides\$.dummy <- NULL
      for (dotpath in names(overrides)) {
        keys <- strsplit(dotpath, '[.]')[[1]]
        val <- overrides[[dotpath]]
        # Auto-convert numeric
        if (!is.na(suppressWarnings(as.numeric(val)))) val <- as.numeric(val)
        # Walk and set
        ref <- cfg
        for (k in keys[-length(keys)]) {
          if (is.null(ref[[k]])) ref[[k]] <- list()
          ref <- ref[[k]]
        }
        # Rebuild from root (R lists are copy-on-modify)
        assign_nested <- function(lst, keys, val) {
          if (length(keys) == 1) { lst[[keys]] <- val; return(lst) }
          lst[[keys[1]]] <- assign_nested(lst[[keys[1]]] %||% list(), keys[-1], val)
          lst
        }
        cfg <- assign_nested(cfg, keys, val)
      }
      yaml::write_yaml(cfg, '${RESOLVED_YAML}')
    "
  }

  log_info "Config resolved: ${RESOLVED_YAML}"
}

resolve_config

# ============================================================================
# READ RESOLVED VALUES (for bash-side use in SLURM scripts)
# ============================================================================

cfg() { yaml_get "$RESOLVED_YAML" "$1"; }

N_PARTICIPANTS=$(cfg data.n_participants)
N_TRIALS=$(cfg data.n_trials)
N_WORKERS=$(cfg "modes.${MODE}.n_workers") || N_WORKERS=$(cfg modes.local.n_workers)
LOG_LEVEL=$(cfg logging.level)
SLURM_PARTITION=$(cfg slurm.partition)
CONTROLLER_MEM_GB=$(cfg slurm.controller.mem_gb)
CONTROLLER_TIME_MIN=$(cfg slurm.controller.time_min)
WORKER_MEM_GB=$(cfg slurm.worker.mem_gb)
WORKER_TIME_MIN=$(cfg slurm.worker.time_min)
RUST_CPUS=$(cfg slurm.rust.cpus)
RUST_MEM_GB=$(cfg slurm.rust.mem_gb)

# ============================================================================
# BANNER
# ============================================================================

log_info "========================================="
log_info "Multiverse Pipeline | mode=$MODE"
log_info "  Participants: $N_PARTICIPANTS  Trials: $N_TRIALS  Workers: $N_WORKERS"
[[ "$MODE" == "hpc" ]] && {
  log_info "  Rust: ${RUST_CPUS} CPUs, ${RUST_MEM_GB}G"
  log_info "  Controller: 4 CPUs, ${CONTROLLER_MEM_GB}G"
  log_info "  Workers: ${N_WORKERS} x ${WORKER_MEM_GB}G"
}
log_info "========================================="

# ============================================================================
# LOCAL / TEST
# ============================================================================

run_local() {
  cd "$R_DIR"
  exec Rscript run.R \
    --mode "$MODE" \
    --config "$RESOLVED_YAML"
}

# ============================================================================
# HPC
# ============================================================================

run_hpc() {

  log_info "Computing expected branch count..."
  local n_expected
  n_expected=$(cd "$R_DIR" && ~/local/bin/Rscript --vanilla -e "
    invisible(lapply(list.files('functions', pattern = '\\\\.R$', full.names = TRUE), source))
    load_all_packages()
    cfg <- load_config('${MODE}', config_path = '${RESOLVED_YAML}')
    cat(length(unique(generate_all_branches(cfg)\$data_id)))
  ")

  if ! [[ "$n_expected" =~ ^[0-9]+$ ]]; then
    log_error "Failed to get branch count (got: '${n_expected}')"
    exit 1
  fi
  

  cd "$PROJECT_ROOT"

  # Step 2: Check Rust output
  local n_existing
  n_existing=$(find data/processed -name 'processed__*.parquet' 2>/dev/null | wc -l || echo 0)
  local signature_status="stale-or-missing"
  if cd "$R_DIR" && ~/local/bin/Rscript --vanilla R/bin/check_processed_cache_signature.R "$MODE" "$RESOLVED_YAML" >/dev/null 2>&1; then
    signature_status="current"
  fi
  cd "$PROJECT_ROOT"
  log_info "Processed data: ${n_existing}/${n_expected} (cache signature: ${signature_status})"

  local rust_job_id=""

  if [[ "$n_existing" -lt "$n_expected" ]] || [[ "$signature_status" != "current" ]] || [[ "$FORCE_RUST" == true ]]; then
    source "${SCRIPT_DIR}/gen_rust_slurm.sh"

    local rust_script="${PROJECT_ROOT}/slurm_rust_generated.sh"
    generate_rust_script \
      "$rust_script" \
      "$RUST_CPUS" \
      "$RUST_MEM_GB" \
      "$SLURM_PARTITION" \
      "$LOG_DIR" \
      "$R_DIR" \
      "$RESOLVED_YAML"

    if [[ "$DRY_RUN" == true ]]; then
      log_info "[DRY RUN] Rust script:"
      cat "$rust_script"
    else
      log_info "Submitting Rust job (${RUST_CPUS} CPUs, ${RUST_MEM_GB}G, partition=${SLURM_PARTITION})..."
      local out
      out=$(sbatch "$rust_script" 2>&1)
      rust_job_id=$(echo "$out" | awk '{print $NF}')
      log_info "$out"
    fi
  else
    log_info "Rust processing complete, skipping"
  fi

  # Step 3: Generate controller script
  local time_fmt
  time_fmt=$(printf "%02d:%02d:00" $(( CONTROLLER_TIME_MIN / 60 )) $(( CONTROLLER_TIME_MIN % 60 )))

  local dep=""
  [[ -n "$rust_job_id" ]] && dep="#SBATCH --dependency=afterok:${rust_job_id}"

  local mail=""
  [[ -n "$EMAIL" ]] && mail="#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=${EMAIL}"

  local ctrl_script="${PROJECT_ROOT}/slurm_controller_generated.sh"
  local ctrl_cpus
  ctrl_cpus=$(cfg slurm.controller.cpus)

  cat > "$ctrl_script" << SLURM_EOF
#!/bin/bash
#SBATCH --job-name=mv_controller
#SBATCH --time=${time_fmt}
#SBATCH --cpus-per-task=${ctrl_cpus}
#SBATCH --mem=${CONTROLLER_MEM_GB}G
#SBATCH --partition=${SLURM_PARTITION}
#SBATCH --output=${LOG_DIR}/controller_%j.out
#SBATCH --error=${LOG_DIR}/controller_%j.err
${dep}
${mail}

set -euo pipefail

log_msg() { echo "[\$(date +'%Y-%m-%d %H:%M:%S')] \$*"; }
trap 'rc=\$?; log_msg "Exit \${rc}"; scancel --name=crew -u "\$USER" 2>/dev/null || true' EXIT

log_msg "Controller starting"

cd "${R_DIR}"
~/local/bin/Rscript run.R \\
  --mode hpc \\
  --config "${RESOLVED_YAML}"

log_msg "Controller finished"
SLURM_EOF

  chmod +x "$ctrl_script"

  if [[ "$DRY_RUN" == true ]]; then
    log_info "[DRY RUN] Controller script:"
    cat "$ctrl_script"
    return 0
  fi

  log_info "Submitting controller..."
  local out
  out=$(sbatch "$ctrl_script" 2>&1)
  local ctrl_id
  ctrl_id=$(echo "$out" | awk '{print $NF}')
  log_info "$out"

  # Summary
  log_info "========================================="
  [[ -n "$rust_job_id" ]] && log_info "Rust:       job $rust_job_id"
  log_info "Controller: job $ctrl_id"
  [[ -n "$rust_job_id" ]] && log_info "Dependency: afterok:$rust_job_id"
  log_info "Monitor:    squeue -u \$USER"
  log_info "========================================="

  echo "[$(ts)] rust=${rust_job_id:-skip} ctrl=$ctrl_id workers=$N_WORKERS" >> "${LOG_DIR}/submissions.log"
}

# DISPATCH

case "$MODE" in
  test|local) run_local ;;
  hpc)        run_hpc   ;;
  *)          log_error "Unknown mode: $MODE"; exit 1 ;;
esac
