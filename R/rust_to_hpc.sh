#!/usr/bin/env bash
set -euo pipefail

# ===========================================================================
# Deploy Rust binary and project to HPC
#
# Guards:
#   1. Only rebuilds Rust if source files changed (content hash)
#   2. Only uploads binary if it changed (checksum comparison)
#   3. rsync handles R/config delta efficiently
#
# Usage:
#   ./rust_to_hpc.sh user@host
#   ./rust_to_hpc.sh user@host --force    # force rebuild
# ===========================================================================

HPC_HOST="${1:?Usage: rust_to_hpc.sh user@host [--force]}"
FORCE="${2:-}"
PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
RUST_DIR="${PROJECT_DIR}/rust"
TARGET="x86_64-unknown-linux-musl"
BINARY="${PROJECT_DIR}/bin/process"
REPO_ROOT="$(cd "${PROJECT_DIR}/.." && pwd)"
FLAKE_DIR="${REPO_ROOT}/flake"
REMOTE_DIR="~/multiverse/R"
REMOTE_BINARY="${REMOTE_DIR}/rust/target/release/process"
HASH_FILE="${RUST_DIR}/target/.source_hash"

ts() { date +'%Y-%m-%d %H:%M:%S'; }
log() { echo "[$(ts)] $*"; }

# --------------------------------------------------------------------------
# Step 1: Check if Rust source changed (content hash of src/ + Cargo.*)
# --------------------------------------------------------------------------
compute_source_hash() {
  # Hash all Rust source files + Cargo manifests
  find "${RUST_DIR}/src" -type f -name '*.rs' -print0 \
    | sort -z \
    | xargs -0 sha256sum
  sha256sum "${RUST_DIR}/Cargo.toml" "${RUST_DIR}/Cargo.lock" 2>/dev/null || true
}

current_hash=$(compute_source_hash | sha256sum | awk '{print $1}')
echo "current_hash = $current_hash"

needs_rebuild=true
if [[ "$FORCE" != "--force" ]] && [[ -f "$HASH_FILE" ]]; then
  previous_hash=$(cat "$HASH_FILE")
  if [[ "$current_hash" == "$previous_hash" ]] && [[ -f "$BINARY" ]]; then
    log "Rust source unchanged (hash: ${current_hash:0:12}…), skipping rebuild"
    needs_rebuild=false
  fi
fi

# --------------------------------------------------------------------------
# Step 2: Build if needed
# --------------------------------------------------------------------------
if [[ "$needs_rebuild" == true ]]; then
  log "Building static Rust binary for ${TARGET}..."
  cd "$RUST_DIR"

  # Use nix if available (produces a fully reproducible static binary),
  # otherwise fall back to cargo with the musl target
  if command -v nix &>/dev/null && [[ -f "${FLAKE_DIR}/flake.nix" ]]; then
    log "Building via nix build ./flake#process-static ..."
    # Run nix from the repo root so result -> /nix/store/...
    ( cd "${REPO_ROOT}" && nix build ./flake#process-static )
    # At this point, ${REPO_ROOT}/result/bin/process exists
    mkdir -p "$(dirname "$BINARY")"
    cp -f "${REPO_ROOT}/result/bin/process" "$BINARY"
    chmod +x "$BINARY"
  else
    log "Nix not available, falling back to cargo build"
    cargo build --release --target "$TARGET"
  fi


  cd "$PROJECT_DIR"

  if [[ ! -f "$BINARY" ]]; then
    log "ERROR: Binary not found at $BINARY after build"
    exit 1
  fi

  # Verify it's actually static
  if command -v file &>/dev/null; then
    file_output=$(file "$BINARY")
    log "Binary type: $file_output"
    if echo "$file_output" | grep -q "dynamically linked"; then
      log "WARNING: Binary is dynamically linked — it may not run on HPC"
    fi
  fi

  # Save hash
  mkdir -p "$(dirname "$HASH_FILE")"
  echo "$current_hash" > "$HASH_FILE"
  log "Build complete, hash saved"
else
  log "Using cached binary: $BINARY"
fi

echo "DEBUG: local BINARY path = $BINARY"
if command -v file &>/dev/null; then
  echo "DEBUG: local BINARY type:"
  file "$BINARY"
fi

# --------------------------------------------------------------------------
# Step 3: Check if remote binary differs (avoid unnecessary upload)
# --------------------------------------------------------------------------
local_checksum=$(sha256sum "$BINARY" | awk '{print $1}')
remote_checksum=$(ssh -o LogLevel=ERROR "$HPC_HOST" "sha256sum ${REMOTE_BINARY} 2>/dev/null | awk '{print \$1}'" || echo "none")

if [[ "$local_checksum" == "$remote_checksum" ]] && [[ "$FORCE" != "--force" ]]; then
  log "Remote binary identical (${local_checksum:0:12}...), skipping binary upload"
  binary_changed=false
else
  binary_changed=true
fi

# --------------------------------------------------------------------------
# Step 4: Sync project files (R code, configs, scripts — NOT Rust target/)
# --------------------------------------------------------------------------
log "Syncing project to HPC..."
rsync -avzq --delete \
  --exclude='rust/target' \
  --exclude='.git' \
  --exclude='_targets' \
  --exclude='outputs' \
  --exclude='data/processed' \
  --exclude='logs/*.log' \
  --exclude='_config' \
  --exclude='docs' \
  "${PROJECT_DIR}/" "${HPC_HOST}:${REMOTE_DIR}/"

# --------------------------------------------------------------------------
# Step 5: Deploy binary if changed
# --------------------------------------------------------------------------
if [[ "$binary_changed" == true ]]; then
  log "Deploying Rust binary to HPC..."
  echo "DEBUG: uploading $BINARY -> ${HPC_HOST}:${REMOTE_BINARY}"
  ssh -o LogLevel=ERROR "$HPC_HOST" "mkdir -p ${REMOTE_DIR}/rust/target/release"
  scp "$BINARY" "${HPC_HOST}:${REMOTE_BINARY}"
  ssh -o LogLevel=ERROR "$HPC_HOST" "chmod +x ${REMOTE_BINARY}"
  log "Binary deployed (${local_checksum:0:12}…)"
else
  log "Binary already up to date on remote"
fi

# --------------------------------------------------------------------------
# Step 6: Verify
# --------------------------------------------------------------------------
log "Verifying remote binary..."
ssh "$HPC_HOST" "file ${REMOTE_BINARY} && ${REMOTE_BINARY} --help 2>&1 | head -3" || true

log "Done. SSH in and run: cd ${REMOTE_DIR} && ./multiverse.sh hpc"
