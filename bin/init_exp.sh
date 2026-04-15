#!/bin/bash
set -euo pipefail

# ============================================================
# init_exp.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Reads exp_dir from experiment$exp_dir in the YAML
#   • Discovers sim_*.yml from config/<path>/exp_vX/ (written by expand_design.R)
#   • Builds model specs for each simulation, saving to exp_dir/sim_XX/model/
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
EXP_DIR="$(grep -m1 '^\s*exp_dir:' "$EXP_YML" | sed 's/.*exp_dir:\s*//' | tr -d '[:space:]"')"

if [[ -z "$EXP_VERSION" ]]; then
  echo "❌ ERROR: experiment\$version missing or unparseable in $EXP_YML"
  exit 1
fi

if [[ -z "$EXP_DIR" ]]; then
  echo "❌ ERROR: experiment\$exp_dir missing or unparseable in $EXP_YML"
  exit 1
fi

echo "🔖 Version:     ${EXP_VERSION}"
echo "📂 Exp dir:     ${EXP_DIR}"

# ===============================
# Validate experiment directory
# ===============================
if [[ ! -d "$EXP_DIR" ]]; then
  echo "❌ ERROR: Experiment directory not found:"
  echo "    $EXP_DIR"
  echo "Did you run: make gen ?"
  exit 1
fi

mkdir -p "$EXP_DIR/analysis"

echo "✅ Experiment directory confirmed"

# ===============================
# Discover simulation yamls from config subfolder
# ===============================
CONFIG_SIM_DIR="$(dirname "$EXP_YML")"
SIM_YMLS=( "$CONFIG_SIM_DIR"/sim_*.yml )

if [[ ! -f "${SIM_YMLS[0]}" ]]; then
  echo "❌ ERROR: No sim_*.yml files found in:"
  echo "    $CONFIG_SIM_DIR"
  echo "Did you run: make gen ?"
  exit 1
fi

echo "🔢 Found ${#SIM_YMLS[@]} simulation(s)"

# ===============================
# Build model spec for each simulation
# ===============================
RSCRIPT_PATH="R/build_model_spec.R"

if [[ ! -f "$RSCRIPT_PATH" ]]; then
  echo "❌ ERROR: Could not find $RSCRIPT_PATH"
  exit 1
fi

for SIM_YML in "${SIM_YMLS[@]}"; do
  SIM_ID="$(basename "$SIM_YML" .yml)"
  SIM_DIR="${EXP_DIR}/${SIM_ID}/"
  MODEL_FILE="${SIM_DIR}model/model.rds"

  echo "────────────────────────────────────────"
  echo "🧩 Setting up simulation: ${SIM_ID}"

  if [[ -f "$MODEL_FILE" ]]; then
    echo "⏭  Skipping ${SIM_ID} — model already built"
    continue
  fi

  mkdir -p "${SIM_DIR}model"
  mkdir -p "${SIM_DIR}iterations"

  Rscript "$RSCRIPT_PATH" "$SIM_YML"

  echo "✅ Simulation initialized: ${SIM_DIR}"
done

echo "────────────────────────────────────────"
echo "✔ Experiment setup complete: ${EXP_DIR}"