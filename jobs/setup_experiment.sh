#!/bin/bash
set -euo pipefail

# ===============================
# ✅ Load environment modules
# ===============================
module purge all
module load R/4.5.1

# --- Prevent BLAS oversubscription ---
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

# ===============================
# ✅ Validate CLI arguments
# ===============================
if [[ $# -ne 1 ]]; then
  echo "❌ ERROR: Missing arguments."
  echo "Usage: $0 <path/to/config.yml>"
  exit 1
fi

CONFIG_PATH="$1"

if [[ ! -f "$CONFIG_PATH" ]]; then
  echo "❌ ERROR: Config file not found:"
  echo "    $CONFIG_PATH"
  exit 1
fi

# ===============================
# ✅ Resolve project root
# ===============================
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "📁 PROJECT_ROOT resolved to: $PROJECT_ROOT"
echo "🧪 Using config: $CONFIG_PATH"

# ===============================
# ✅ Extract experiment ID from config
# ===============================
EXP_ID="$(Rscript -e "cat(yaml::read_yaml('$CONFIG_PATH')\$experiment\$id)")"

if [[ -z "$EXP_ID" ]]; then
  echo "❌ ERROR: experiment$id not found in config."
  exit 1
fi

EXP_DIR="experiments/${EXP_ID}"
MODEL_FILE="${EXP_DIR}/model/model_ready.rds"

echo "🧪 Experiment ID: $EXP_ID"

if [[ -f "$MODEL_FILE" ]]; then
  echo "❌ ERROR: Experiment already initialized:"
  echo "    $MODEL_FILE exists"
  exit 1
fi

# ===============================
# ✅ Run setup R script
# ===============================
RSCRIPT_PATH="scripts/setup_experiment.R"

if [[ ! -f "$RSCRIPT_PATH" ]]; then
  echo "❌ ERROR: Could not find $RSCRIPT_PATH"
  exit 1
fi

Rscript "$RSCRIPT_PATH" "$CONFIG_PATH"
