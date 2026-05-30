#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# analyze_exp.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Reads exp_dir from the yaml for data location
#   • Discovers sim_*.yml from the same config directory
#   • Calls bin/analyze_sim.sh for each simulation
#   • Skips simulations with no iterations directory or no
#     completed iter_* folders
#   • Skips simulations that appear to still be running
#     (a model.rds written within the last 2 hours)
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
# Read exp_dir from yaml
# ===============================
EXP_RUN_DIR="$(grep -m1 '^\s*exp_dir:' "$EXP_YML" | sed 's/.*exp_dir:\s*//' | tr -d '[:space:]"')"

if [[ -z "$EXP_RUN_DIR" ]]; then
  echo "❌ experiment\$exp_dir missing or unparseable in $EXP_YML"
  exit 1
fi

if [[ ! -d "$EXP_RUN_DIR" ]]; then
  echo "❌ Experiment run directory not found: $EXP_RUN_DIR"
  exit 1
fi

echo "📂 Analyzing experiment: $EXP_RUN_DIR"

# ===============================
# Discover sim yamls from config directory
# ===============================
CONFIG_SIM_DIR="$(dirname "$EXP_YML")"
SIM_YMLS=( "$CONFIG_SIM_DIR"/sim_*.yml )

if [[ ! -f "${SIM_YMLS[0]}" ]]; then
  echo "❌ No sim_*.yml files found in: $CONFIG_SIM_DIR"
  exit 1
fi

# ===============================
# Analyze each simulation
# ===============================
for SIM_YML in "${SIM_YMLS[@]}"; do
  SIM_ID="$(basename "$SIM_YML" .yml)"
  SIM_DIR="${EXP_RUN_DIR}/${SIM_ID}"

  # Skip if no iterations directory
  ITER_DIR="${SIM_DIR}/iterations"
  if [[ ! -d "$ITER_DIR" ]]; then
    echo "⏭  Skipping ${SIM_ID} — no iterations directory"
    continue
  fi

  # Skip if no completed iterations
  N_COMPLETE=$(find "$ITER_DIR" -mindepth 2 -maxdepth 2 -name "model.rds" | wc -l)
  if [[ "$N_COMPLETE" -eq 0 ]]; then
    echo "⏭  Skipping ${SIM_ID} — no completed iterations"
    continue
  fi

  # Skip if a model.rds was written within the last 2 hours,
  # indicating the simulation is likely still running
  N_RECENT=$(find "$ITER_DIR" -name "model.rds" -mmin -120 | wc -l)
  if [[ "$N_RECENT" -gt 0 ]]; then
    echo "⏭  Skipping ${SIM_ID} — ${N_RECENT} iterations written in last 2 hours (likely still running)"
    continue
  fi

  echo "▶  Analyzing ${SIM_ID} (${N_COMPLETE} completed iterations)"
  bash bin/analyze_sim.sh "$SIM_YML"
done

echo "✔ Experiment analysis complete: $EXP_RUN_DIR"