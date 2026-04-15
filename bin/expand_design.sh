#!/bin/bash
set -euo pipefail

# ============================================================
# expand_design.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Runs R/expand_design.R
#   • Verifies sim_* directories were created in exp_dir
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
# Read paths directly from YAML
# ===============================
EXP_VERSION="$(grep -m1 '^\s*version:' "$EXP_YML" | sed 's/.*version:\s*//' | tr -d '[:space:]"')"
EXP_RUN_DIR="$(grep -m1 '^\s*exp_dir:' "$EXP_YML" | sed 's/.*exp_dir:\s*//' | tr -d '[:space:]"')"

if [[ -z "$EXP_VERSION" ]]; then
  echo "❌ ERROR: experiment\$version missing or unparseable in $EXP_YML"
  exit 1
fi

if [[ -z "$EXP_RUN_DIR" ]]; then
  echo "❌ ERROR: experiment\$exp_dir missing or unparseable in $EXP_YML"
  exit 1
fi

echo "🔖 Version: ${EXP_VERSION}"
echo "📂 Exp dir: ${EXP_RUN_DIR}"

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
CONFIG_SIM_DIR="$(dirname "$EXP_YML")"
SIM_YMLS=( "$CONFIG_SIM_DIR"/sim_*.yml )

if [[ ! -f "${SIM_YMLS[0]}" ]]; then
  echo "❌ ERROR: expand_design.R produced no sim_*.yml files in:"
  echo "    $CONFIG_SIM_DIR"
  echo "Check the design block in:"
  echo "  $EXP_YML"
  exit 1
fi

echo "✔ Generated ${#SIM_YMLS[@]} simulation config(s) in: ${CONFIG_SIM_DIR}"