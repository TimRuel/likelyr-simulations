#!/bin/bash
set -euo pipefail

# ============================================================
# init_exp.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Reads version from experiment$version in the YAML via grep
#   • Discovers sim_*/sim_*.yml already written to experiments/
#     by expand_design.R
#   • Builds model specs for each simulation
#   • Writes ONLY to experiments/
#   • Config directory is never touched
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
# Read version directly from YAML via grep
# ===============================
EXP_VERSION="$(grep -m1 '^\s*version:' "$EXP_YML" | sed 's/.*version:\s*//' | tr -d '[:space:]"')"

if [[ -z "$EXP_VERSION" ]]; then
  echo "❌ ERROR: experiment\$version missing or unparseable in $EXP_YML"
  exit 1
fi

# ===============================
# Resolve experiment paths
# ===============================
EXP_CFG_DIR="$(dirname "$EXP_YML")"
EXPERIMENT_REL="$(realpath --relative-to=config "$EXP_CFG_DIR")"

if [[ -z "$EXPERIMENT_REL" || "$EXPERIMENT_REL" == "." ]]; then
  echo "❌ ERROR: experiment config must live under config/<path>/"
  exit 1
fi

EXP_DIR="experiments/${EXPERIMENT_REL}/${EXP_VERSION}"

echo "🧪 Experiment: ${EXPERIMENT_REL}"
echo "🔖 Version:    ${EXP_VERSION}"
echo "📂 Runtime dir: ${EXP_DIR}"

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
# Discover simulation directories
# ===============================
SIM_DIRS=( "$EXP_DIR"/sim_*/ )

if [[ ! -d "${SIM_DIRS[0]}" ]]; then
  echo "❌ ERROR: No sim_* directories found in:"
  echo "    $EXP_DIR"
  echo "Did you run: make gen ?"
  exit 1
fi

echo "🔢 Found ${#SIM_DIRS[@]} simulation(s)"

# ===============================
# Build model spec for each simulation
# ===============================
RSCRIPT_PATH="R/build_model_spec.R"

if [[ ! -f "$RSCRIPT_PATH" ]]; then
  echo "❌ ERROR: Could not find $RSCRIPT_PATH"
  exit 1
fi

for SIM_DIR in "${SIM_DIRS[@]}"; do
  SIM_ID="$(basename "$SIM_DIR")"
  SIM_YML="${SIM_DIR}${SIM_ID}.yml"
  MODEL_FILE="${SIM_DIR}model/model.rds"

  echo "────────────────────────────────────────"
  echo "🧩 Setting up simulation: ${SIM_ID}"

  if [[ ! -f "$SIM_YML" ]]; then
    echo "❌ ERROR: Simulation config not found:"
    echo "    $SIM_YML"
    exit 1
  fi

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