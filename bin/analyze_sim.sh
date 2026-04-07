#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# analyze_sim.sh
#
# Contract:
#   • Accepts <path/to/sim_XX/sim_XX.yml>
#   • Calls R/analyze_sim.R with the sim directory
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

# ===============================
# Validate CLI arguments
# ===============================
SIM_YML="${1:-}"

if [[ -z "$SIM_YML" ]]; then
  echo "Usage: $0 <path/to/sim_XX/sim_XX.yml>"
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
# Derive sim directory and ID
# ===============================
SIM_DIR="$(dirname "$SIM_YML")"
SIM_ID="$(basename "$SIM_DIR")"
ANALYSIS_DIR="${SIM_DIR}/analysis"

echo "📂 Analyzing: $SIM_ID"

# ===============================
# Skip if already analyzed
# ===============================
if [[ -f "$ANALYSIS_DIR/$POINT_FILE" && -f "$ANALYSIS_DIR/$INTERVAL_FILE" ]]; then
  echo "✔ Skipping ${SIM_ID} (already analyzed)"
  exit 0
fi

mkdir -p "$ANALYSIS_DIR"
Rscript R/analyze_sim.R "$SIM_DIR"

echo "✔ Analysis complete: $SIM_ID"