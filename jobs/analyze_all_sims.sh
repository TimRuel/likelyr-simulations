#!/usr/bin/env bash
set -euo pipefail

# ===============================
# Load environment modules (HPC only)
# ===============================
if command -v module >/dev/null 2>&1; then
  module purge
  module load R/4.5.1
fi

EXP_DIR="$1"

if [[ -z "${EXP_DIR:-}" ]]; then
  echo "Usage: ./scripts/analyze_all_sims.sh <experiment_dir>"
  exit 1
fi

if [[ ! -d "$EXP_DIR" ]]; then
  echo "Experiment directory not found: $EXP_DIR"
  exit 1
fi

for SIM_DIR in "$EXP_DIR"/sim_*; do
  [[ -d "$SIM_DIR" ]] || continue

  ANALYSIS_DIR="$SIM_DIR/analysis"
  POINT_FILE="$ANALYSIS_DIR/metrics_point_iteration.rds"
  INTERVAL_FILE="$ANALYSIS_DIR/metrics_interval_iteration.rds"

  if [[ -f "$POINT_FILE" && -f "$INTERVAL_FILE" ]]; then
    echo "✔ Skipping $(basename "$SIM_DIR") (already analyzed)"
    continue
  fi

  echo "▶ Analyzing $(basename "$SIM_DIR")"
  Rscript analysis/analyze_sim.R "$SIM_DIR"
done
