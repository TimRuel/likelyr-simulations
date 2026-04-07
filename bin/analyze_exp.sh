#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# analyze_exp.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Reads version to derive EXP_RUN_DIR
#   • Calls bin/analyze_sim.sh for each simulation
#   • Skips simulations with no iterations directory or no
#     completed iter_* folders
#   • Simulations already analyzed are skipped by analyze_sim.sh
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

echo "📂 Analyzing experiment: $EXP_RUN_DIR"

# ===============================
# Analyze each simulation
# ===============================
for SIM_DIR in "$EXP_RUN_DIR"/sim_*/; do
  [[ -d "$SIM_DIR" ]] || continue

  SIM_ID="$(basename "$SIM_DIR")"
  SIM_YML="${SIM_DIR}${SIM_ID}.yml"

  if [[ ! -f "$SIM_YML" ]]; then
    echo "❌ Simulation config not found: $SIM_YML"
    exit 1
  fi

  # Skip if no iterations directory
  ITER_DIR="${SIM_DIR}iterations"
  if [[ ! -d "$ITER_DIR" ]]; then
    echo "⏭  Skipping ${SIM_ID} — no iterations directory"
    continue
  fi

  # Skip if no completed iter_* folders containing model.rds
  N_COMPLETE=$(find "$ITER_DIR" -mindepth 2 -maxdepth 2 -name "model.rds" | wc -l)
  if [[ "$N_COMPLETE" -eq 0 ]]; then
    echo "⏭  Skipping ${SIM_ID} — no completed iterations"
    continue
  fi

  bash bin/analyze_sim.sh "$SIM_YML"
done

echo "✔ Experiment analysis complete: $EXP_RUN_DIR"