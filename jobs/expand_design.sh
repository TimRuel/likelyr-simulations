#!/bin/bash
set -euo pipefail

# ============================================================
# expand_design.sh
#
# Contract:
#   • Accepts <path/to/experiment.yml>
#   • Runs expand_design.R
#   • Verifies sim_*.yml files were created
#   • Does NOT inspect or re-derive design logic
# ============================================================

# ===============================
# Load environment modules (HPC only)
# ===============================
if command -v module >/dev/null 2>&1; then
  module purge
  module load R/4.5.1
fi

# --- Prevent BLAS oversubscription ---
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

# ===============================
# Validate CLI arguments
# ===============================
if [[ $# -ne 1 ]]; then
  echo "❌ ERROR: Missing arguments."
  echo "Usage: $0 <path/to/experiment.yml>"
  exit 1
fi

EXP_YML="$1"

if [[ ! -f "$EXP_YML" ]]; then
  echo "❌ ERROR: experiment.yml not found:"
  echo "    $EXP_YML"
  exit 1
fi

# ===============================
# Resolve project root
# ===============================
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "📁 PROJECT_ROOT resolved to: $PROJECT_ROOT"
echo "🧪 Using experiment config: $EXP_YML"

# ===============================
# Resolve config directory
# ===============================
EXP_CFG_DIR="$(dirname "$EXP_YML")"

# Must live under config/
EXPERIMENT_REL="$(realpath --relative-to=config "$EXP_CFG_DIR")"

if [[ -z "$EXPERIMENT_REL" || "$EXPERIMENT_REL" == "." ]]; then
  echo "❌ ERROR: experiment.yml must live under config/<experiment>/"
  exit 1
fi

echo "🧪 Experiment: ${EXPERIMENT_REL}"
echo "📂 Config directory: ${EXP_CFG_DIR}"

# ===============================
# Run R-side generation
# ===============================
RSCRIPT_PATH="scripts/expand_design.R"

if [[ ! -f "$RSCRIPT_PATH" ]]; then
  echo "❌ ERROR: Could not find $RSCRIPT_PATH"
  exit 1
fi

Rscript "$RSCRIPT_PATH" "$EXP_YML"

# ===============================
# Validate output
# ===============================
SIM_CONFIGS=( "${EXP_CFG_DIR}"/sim_*.yml )

if [[ ! -e "${SIM_CONFIGS[0]}" ]]; then
  echo "❌ ERROR: expand_design.R produced no sim_*.yml files"
  echo "Check the design block in:"
  echo "  $EXP_YML"
  exit 1
fi

echo "✔ Generated ${#SIM_CONFIGS[@]} simulation config(s)"
