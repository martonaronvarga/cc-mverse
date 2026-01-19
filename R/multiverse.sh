#!/bin/bash
# multiverse.sh - Pipeline runner
# 
# Usage:
#   ./multiverse.sh                    # Local test (5 min)
#   ./multiverse.sh local              # Local full (60 min)
#   ./multiverse.sh hpc                # Submit to HPC cluster
#   ./multiverse.sh hpc --workers 64   # HPC with custom worker count
#
# Environment variables (optional):
#   N_PARTICIPANTS=50                  # Override default 20
#   N_TRIALS=200                       # Override default 100
#   SLURM_PARTITION=hpc2019            # Which HPC partition
#   SSH_CLUSTER_HOST=my.cluster.com    # HPC cluster hostname
#   SSH_CLUSTER_USER=$USER             # Username on cluster
#   LOG_LEVEL=debug                    # Logging level

set -e  # Exit on error

# ============================================================================
# CONFIGURATION & DEFAULTS
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$SCRIPT_DIR"

# Default mode
MODE="${1:-test}"

# Default parameters
MODE_DEFAULTS=(
  [test]="N_PARTICIPANTS=5:N_TRIALS=20:N_WORKERS=1:LOG_LEVEL=info"
  [local]="N_PARTICIPANTS=20:N_TRIALS=100:N_WORKERS=4:LOG_LEVEL=info"
  [hpc]="N_PARTICIPANTS=100:N_TRIALS=500:N_WORKERS=32:LOG_LEVEL=info"
)

# ============================================================================
# FUNCTIONS
# ============================================================================

print_usage() {
  cat << EOF
Usage: ./multiverse.sh [MODE] [OPTIONS]

MODES:
  test       Quick test run (5 min, 5 participants, 1 worker)
  local      Full local run (60 min, 20 participants, 4 workers)
  hpc        Submit to HPC cluster (64 workers, auto-submit to SLURM)

OPTIONS:
  --participants N     Number of participants (overrides mode default)
  --trials N           Trials per participant
  --workers N          Number of parallel workers
  --partition NAME     SLURM partition (for hpc mode)
  --log-level LEVEL    Logging level: debug, info, warn, error
  --help               Show this help message

EXAMPLES:
  ./multiverse.sh                          # Test locally
  ./multiverse.sh local                    # Full local run
  ./multiverse.sh hpc                      # Submit to HPC
  ./multiverse.sh local --workers 8        # Local with 8 workers
  ./multiverse.sh hpc --workers 64 --partition hpc2019  # HPC with custom partition

ENVIRONMENT:
  SSH_CLUSTER_HOST     HPC cluster hostname
  SSH_CLUSTER_USER     Username on cluster (default: \$USER)
  SLURM_PARTITION      Default SLURM partition (default: default)

EOF
  exit 0
}

log_info() {
  echo "[$(date +'%Y-%m-%d %H:%M:%S')] INFO: $@"
}

log_error() {
  echo "[$(date +'%Y-%m-%d %H:%M:%S')] ERROR: $@" >&2
}

parse_defaults() {
  local mode=$1
  local defaults="${MODE_DEFAULTS[$mode]}"
  
  if [[ -z "$defaults" ]]; then
    log_error "Unknown mode: $mode"
    print_usage
  fi
  
  # Parse colon-separated defaults
  IFS=':' read -ra parts <<< "$defaults"
  for part in "${parts[@]}"; do
    IFS='=' read -r key val <<< "$part"
    export "$key=$val"
  done
}

parse_options() {
  shift  # Skip mode argument
  
  while [[ $# -gt 0 ]]; do
    case $1 in
      --participants)
        export N_PARTICIPANTS=$2
        shift 2
        ;;
      --trials)
        export N_TRIALS=$2
        shift 2
        ;;
      --workers)
        export N_WORKERS=$2
        shift 2
        ;;
      --partition)
        export SLURM_PARTITION=$2
        shift 2
        ;;
      --log-level)
        export LOG_LEVEL=$2
        shift 2
        ;;
      --help)
        print_usage
        ;;
      *)
        log_error "Unknown option: $1"
        print_usage
        ;;
    esac
  done
}

print_config() {
  log_info "Running in $MODE mode"
  log_info "  Participants: $N_PARTICIPANTS"
  log_info "  Trials/person: $N_TRIALS"
  log_info "  Workers: $N_WORKERS"
  log_info "  Log level: $LOG_LEVEL"
  
  if [[ "$MODE" == "hpc" ]]; then
    log_info "  SLURM partition: ${SLURM_PARTITION:-default}"
    log_info "  HPC cluster: ${SSH_CLUSTER_HOST:-not configured}"
  fi
}

run_local_mode() {
  log_info "Starting local pipeline"
  
  cd "$PROJECT_ROOT/R"
  
  Rscript run.R \
    --mode "$MODE" \
    --participants "$N_PARTICIPANTS" \
    --trials "$N_TRIALS" \
    --workers "$N_WORKERS" \
    --log-level "$LOG_LEVEL"
}

run_hpc_mode() {
  log_info "Preparing HPC submission"
  
  # Check HPC configuration
  if [[ -z "$SSH_CLUSTER_HOST" ]]; then
    log_error "SSH_CLUSTER_HOST not set"
    log_error "Please set: export SSH_CLUSTER_HOST=my.cluster.edu"
    exit 1
  fi
  
  if [[ -z "$SSH_CLUSTER_USER" ]]; then
    SSH_CLUSTER_USER="$USER"
    log_info "Using SSH_CLUSTER_USER=$SSH_CLUSTER_USER"
  fi
  
  # Prepare variables for HPC submission
  export USE_HPC=TRUE
  export N_WORKERS=${N_WORKERS:-32}
  export SLURM_MEM_GB=${SLURM_MEM_GB:-8}
  export SLURM_TIME_MIN=${SLURM_TIME_MIN:-240}
  export SLURM_PARTITION=${SLURM_PARTITION:-default}
  
  log_info "HPC Configuration:"
  log_info "  Host: $SSH_CLUSTER_HOST"
  log_info "  User: $SSH_CLUSTER_USER"
  log_info "  Workers: $N_WORKERS"
  log_info "  Memory: $SLURM_MEM_GB GB/worker"
  log_info "  Time: $SLURM_TIME_MIN min"
  log_info "  Partition: $SLURM_PARTITION"
  
  # Ask for confirmation
  read -p "Submit to HPC? (y/N) " -n 1 -r
  echo
  
  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    log_info "Cancelled"
    exit 0
  fi
  
  cd "$PROJECT_ROOT/R"
  
  Rscript run.R \
    --mode hpc \
    --participants "$N_PARTICIPANTS" \
    --trials "$N_TRIALS" \
    --workers "$N_WORKERS" \
    --log-level "$LOG_LEVEL" \
    --cluster-host "$SSH_CLUSTER_HOST" \
    --cluster-user "$SSH_CLUSTER_USER"
}

# ============================================================================
# MAIN
# ============================================================================

if [[ "$MODE" == "--help" ]] || [[ "$MODE" == "-h" ]]; then
  print_usage
fi

# Parse defaults for mode
parse_defaults "$MODE"

# Parse command line options (overrides defaults)
parse_options "$@"

# Print final configuration
print_config

# Run appropriate mode
case "$MODE" in
  test|local)
    run_local_mode
    ;;
  hpc)
    run_hpc_mode
    ;;
  *)
    log_error "Unknown mode: $MODE"
    print_usage
    ;;
esac

log_info "Pipeline complete"
