#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# analyze_sim.sh
#
# Contract:
#   • Accepts <path/to/config/.../sim_XX.yml>
#   • Reads exp_dir and sim_id from the yaml to locate data
#   • Calls R/analyze_sim.R with the sim data directory
#   • Skips if analysis outputs already exist
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
# (must match what analyze_sim.R actually writes)
# ===============================
POINT_FILE="sim_point_metrics.rds"
INTERVAL_FILE="sim_interval_metrics.rds"
INVALID_CI_FILE="invalid_ci_index.rds"

# ===============================
# Validate CLI arguments
# ===============================
SIM_YML="${1:-}"

if [[ -z "$SIM_YML" ]]; then
  echo "Usage: $0 <path/to/sim_XX.yml>"
  exit 1
fi

if [[ ! -f "$SIM_YML" ]]; then
  echo "❌ Simulation config not found: $SIM_YML"
  exit 1
fi

# ===============================
# Resolve project root
# ===============================
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

# ===============================
# Read exp_dir and sim_id from yaml
# ===============================
EXP_DIR="$(grep -m1 '^\s*exp_dir:' "$SIM_YML" | sed 's/.*exp_dir:\s*//' | tr -d '[:space:]"')"
SIM_ID="$(grep -m1 '^\s*sim_id:' "$SIM_YML" | sed 's/.*sim_id:\s*//' | tr -d '[:space:]"')"

if [[ -z "$EXP_DIR" ]]; then
  echo "❌ experiment\$exp_dir missing or unparseable in $SIM_YML"
  exit 1
fi

if [[ -z "$SIM_ID" ]]; then
  echo "❌ simulation\$sim_id missing or unparseable in $SIM_YML"
  exit 1
fi

SIM_DIR="${EXP_DIR}/${SIM_ID}"
ANALYSIS_DIR="${SIM_DIR}/analysis"

echo "📂 Analyzing: $SIM_ID"

# ===============================
# Skip if already analyzed
# ===============================
if [[ -f "$ANALYSIS_DIR/$POINT_FILE" && -f "$ANALYSIS_DIR/$INTERVAL_FILE" && -f "$ANALYSIS_DIR/$INVALID_CI_FILE" ]]; then
  echo "✔ Skipping ${SIM_ID} (already analyzed)"
  exit 0
fi

mkdir -p "$ANALYSIS_DIR"
Rscript R/analyze_sim.R "$SIM_DIR"

echo "✔ Analysis complete: $SIM_ID"