#!/bin/bash
set -euo pipefail

# ============================================================
# init_exp.sh
#
# Contract:
#   • Accepts <path/to/experiment.yml> (from config/)
#   • Initializes experiment runtime directory
#   • Discovers sim_*.yml in the same config directory
#   • Sets up ALL simulations for the experiment
#   • Writes ONLY to experiments/
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
# Resolve experiment paths
#   config/<experiment_rel>/experiment.yml
# ===============================
EXP_CFG_DIR="$(dirname "$EXP_YML")"
EXPERIMENT_REL="$(realpath --relative-to=config "$EXP_CFG_DIR")"

if [[ -z "$EXPERIMENT_REL" || "$EXPERIMENT_REL" == "." ]]; then
  echo "❌ ERROR: experiment.yml must live under config/<experiment>/"
  exit 1
fi

EXP_DIR="experiments/${EXPERIMENT_REL}"

echo "🧪 Experiment: ${EXPERIMENT_REL}"
echo "📂 Runtime dir: ${EXP_DIR}"

# ===============================
# Initialize experiment directory
# ===============================
mkdir -p "$EXP_DIR"
mkdir -p "$EXP_DIR/analysis"

# Snapshot experiment config (authoritative)
cp "$EXP_YML" "$EXP_DIR/experiment.yml"

echo "✅ Experiment initialized"

# ===============================
# Discover simulation configs
# ===============================
SIM_CONFIGS=( "${EXP_CFG_DIR}"/sim_*.yml )

if [[ ! -e "${SIM_CONFIGS[0]}" ]]; then
  echo "❌ ERROR: No sim_*.yml files found in:"
  echo "    ${EXP_CFG_DIR}"
  echo "Did you run: make gen ?"
  exit 1
fi

echo "🔢 Found ${#SIM_CONFIGS[@]} simulation configs"

# ===============================
# Setup each simulation
# ===============================
RSCRIPT_PATH="scripts/build_model_spec.R"

if [[ ! -f "$RSCRIPT_PATH" ]]; then
  echo "❌ ERROR: Could not find $RSCRIPT_PATH"
  exit 1
fi

for CONFIG_PATH in "${SIM_CONFIGS[@]}"; do
  SIM_FILE="$(basename "$CONFIG_PATH")"
  SIM_ID="${SIM_FILE%.yml}"

  SIM_DIR="${EXP_DIR}/${SIM_ID}"
  MODEL_FILE="${SIM_DIR}/model/model.rds"
  SIM_YML="${SIM_DIR}/simulation.yml"

  echo "────────────────────────────────────────"
  echo "🧩 Setting up simulation: ${SIM_ID}"

  if [[ -f "$MODEL_FILE" ]]; then
    echo "❌ ERROR: Simulation already initialized:"
    echo "    ${MODEL_FILE} exists"
    exit 1
  fi

  # Create directory skeleton
  mkdir -p "${SIM_DIR}/model"
  mkdir -p "${SIM_DIR}/iterations"

  # Snapshot simulation config (authoritative)
  cp "$CONFIG_PATH" "$SIM_YML"

  # Run R-side setup (model construction only)
  Rscript "$RSCRIPT_PATH" "$SIM_YML"

  echo "✅ Simulation initialized:"
  echo "   ${SIM_DIR}"
done

echo "✔ Experiment setup complete:"
echo "  ${EXP_DIR}"
