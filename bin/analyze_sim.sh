#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# analyze_sim.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Reads version to derive EXP_RUN_DIR
#   • Calls R/analyze_iter.R for each simulation
#   • Skips simulations whose analysis outputs already exist
# ============================================================

# ===============================
# Load environment modules (HPC only)
# ===============================
if command -v module >/dev/null 2>&1; then
  module purge
  module load R/4.5.1
fi

# ===============================
# Expected output filenames
# ===============================
POINT_FILE="metrics_point_iteration.rds"
INTERVAL_FILE="metrics_interval_iteration.rds"

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
# Read version and derive EXP_RUN_DIR
# ===============================
EXP_VERSION="$(grep -m1 '^\s*version:' "$EXP_YML" | sed 's/.*version:\s*//' | tr -d '[:space:]"')"

if [[ -z "$EXP_VERSION" ]]; then
  echo "❌ experiment\$version missing or unparseable in $EXP_YML"
  exit 1
fi

EXP_CFG_DIR="$(dirname "$EXP_YML")"
EXPERIMENT_REL="$(realpath --relative-to=config "$EXP_CFG_DIR")"
EXP_RUN_DIR="experiments/${EXPERIMENT_REL}/${EXP_VERSION}"

if [[ ! -d "$EXP_RUN_DIR" ]]; then
  echo "❌ Experiment run directory not found: $EXP_RUN_DIR"
  exit 1
fi

echo "📂 Analyzing: $EXP_RUN_DIR"

# ===============================
# Analyze each simulation
# ===============================
for SIM_DIR in "$EXP_RUN_DIR"/sim_*/; do
  [[ -d "$SIM_DIR" ]] || continue

  ANALYSIS_DIR="${SIM_DIR}analysis"
  SIM_ID="$(basename "$SIM_DIR")"

  if [[ -f "$ANALYSIS_DIR/$POINT_FILE" && -f "$ANALYSIS_DIR/$INTERVAL_FILE" ]]; then
    echo "✔ Skipping ${SIM_ID} (already analyzed)"
    continue
  fi

  echo "▶ Analyzing ${SIM_ID}"
  mkdir -p "$ANALYSIS_DIR"
  Rscript R/analyze_iter.R "$SIM_DIR"
done

echo "✔ Analysis complete: $EXP_RUN_DIR"