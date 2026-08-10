#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# bundle_exp.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Reads exp_dir from the yaml for data location
#   • Calls R/bundle_exp.R to collapse all sim_*/analysis outputs into
#     a single <exp_dir>/analysis/bundle.rds
#   • Always rebuilds — the bundle is cheap and must not go stale after
#     additional simulations finish analyzing
#
# Run this AFTER analyze-exp. It is what makes `make download` a
# one-file transfer instead of a ~150-file tree walk.
# ============================================================

# ===============================
# Load environment modules (HPC only)
# ===============================
if command -v module >/dev/null 2>&1; then
  module purge
  module load R/4.5.1
fi

# ===============================
# Validate CLI arguments
# ===============================
EXP_YML="${1:-}"

if [[ -z "$EXP_YML" ]]; then
  echo "Usage: $0 <path/to/exp_vX.yml>"
  exit 1
fi

if [[ ! -f "$EXP_YML" ]]; then
  echo "❌ Experiment config not found: $EXP_YML"
  exit 1
fi

# ===============================
# Resolve project root
# ===============================
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

# ===============================
# Read exp_dir from yaml
# ===============================
# `|| true` so a missing exp_dir reaches the error message below instead of
# killing the script silently via set -e on a failed command substitution.
EXP_RUN_DIR="$(grep -m1 '^[[:space:]]*exp_dir:' "$EXP_YML" | sed 's/.*exp_dir:[[:space:]]*//' | tr -d '[:space:]"' || true)"

if [[ -z "$EXP_RUN_DIR" ]]; then
  echo "❌ experiment\$exp_dir missing or unparseable in $EXP_YML"
  exit 1
fi

if [[ ! -d "$EXP_RUN_DIR" ]]; then
  echo "❌ Experiment run directory not found: $EXP_RUN_DIR"
  exit 1
fi

Rscript R/bundle_exp.R "$EXP_RUN_DIR"
