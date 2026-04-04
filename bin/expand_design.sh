#!/bin/bash
set -euo pipefail

# ============================================================
# expand_design.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Runs R/expand_design.R
#   • Verifies sim_* directories were created in experiments/
#   • Does NOT inspect or re-derive design logic
#   • Config directory is never written to
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
  echo "Usage: $0 <path/to/exp_vX.yml>"
  exit 1
fi

EXP_YML="$1"

if [[ ! -f "$EXP_YML" ]]; then
  echo "❌ ERROR: Experiment config not found:"
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
# Resolve config directory and experiment path
# ===============================
EXP_CFG_DIR="$(dirname "$EXP_YML")"
EXPERIMENT_REL="$(realpath --relative-to=config "$EXP_CFG_DIR")"

if [[ -z "$EXPERIMENT_REL" || "$EXPERIMENT_REL" == "." ]]; then
  echo "❌ ERROR: experiment config must live under config/<path>/"
  exit 1
fi

echo "🧪 Experiment: ${EXPERIMENT_REL}"
echo "📂 Config directory: ${EXP_CFG_DIR}"

# ===============================
# Read version directly from YAML via grep
# ===============================
EXP_VERSION="$(grep -m1 '^\s*version:' "$EXP_YML" | sed 's/.*version:\s*//' | tr -d '[:space:]"')"

if [[ -z "$EXP_VERSION" ]]; then
  echo "❌ ERROR: experiment\$version missing or unparseable in $EXP_YML"
  exit 1
fi

# ===============================
# Run R-side generation
# ===============================
RSCRIPT_PATH="R/expand_design.R"

if [[ ! -f "$RSCRIPT_PATH" ]]; then
  echo "❌ ERROR: Could not find $RSCRIPT_PATH"
  exit 1
fi

Rscript "$RSCRIPT_PATH" "$EXP_YML"

# ===============================
# Validate output
# ===============================
EXP_RUN_DIR="experiments/${EXPERIMENT_REL}/${EXP_VERSION}"
SIM_DIRS=( "$EXP_RUN_DIR"/sim_*/ )

if [[ ! -d "${SIM_DIRS[0]}" ]]; then
  echo "❌ ERROR: expand_design.R produced no sim_* directories in:"
  echo "    $EXP_RUN_DIR"
  echo "Check the design block in:"
  echo "  $EXP_YML"
  exit 1
fi

echo "✔ Generated ${#SIM_DIRS[@]} simulation config(s) in: ${EXP_RUN_DIR}"