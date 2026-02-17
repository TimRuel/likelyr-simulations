#!/bin/bash
#SBATCH --account=p32397
#SBATCH --partition=short
#SBATCH --time=02:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=timothyruel2024@u.northwestern.edu
#SBATCH --job-name=likelyr_iter
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null

set -euo pipefail

# ============================================================
# slurm_iter.sh
#
# Contract:
#   • Called by submit_exp.sh
#   • Runs ONE iteration of ONE simulation
#   • Declares execution mode = slurm
#   • Delegates all iteration logic to run_iter.R
# ============================================================

# ===============================
# Load environment modules (HPC only)
# ===============================
if command -v module >/dev/null 2>&1; then
  module purge
  module load R/4.5.1
  module load nlopt/2.7.1-gcc-12.3.0
fi

# --- Prevent BLAS oversubscription ---
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

# ===============================
# Validate CLI arguments
# ===============================
if [[ $# -ne 1 ]]; then
  echo "❌ ERROR: Missing arguments."
  echo "Usage: sbatch $0 <path/to/simulation.yml>"
  exit 1
fi

SIM_YML="$1"

if [[ ! -f "$SIM_YML" ]]; then
  echo "❌ ERROR: simulation.yml not found:"
  echo "    $SIM_YML"
  exit 1
fi

# ===============================
# Resolve project root
# ===============================
PROJECT_ROOT="$SLURM_SUBMIT_DIR"
cd "$PROJECT_ROOT" || {
  echo "❌ ERROR: Failed to cd into $PROJECT_ROOT"
  exit 1
}

# ===============================
# Declare execution mode
# ===============================
export LIKELYR_EXEC_MODE=slurm

# ===============================
# Iteration bookkeeping (logging only)
# ===============================
ITER_INDEX=$((SLURM_ARRAY_TASK_ID + 1))
ITER_ID=$(printf "iter_%04d" "$ITER_INDEX")

SIM_DIR="$(dirname "$SIM_YML")"
SIM_ID="$(basename "$SIM_DIR")"

LOG_DIR="${SIM_DIR}/iterations/${ITER_ID}/logs"
mkdir -p "$LOG_DIR"

LOG_FILE="${LOG_DIR}/slurm.out"

# Redirect all output
exec > "$LOG_FILE" 2>&1

# ===============================
# Begin logged output
# ===============================
echo "🚀 Starting iteration"
echo "📁 PROJECT_ROOT: ${PROJECT_ROOT}"
echo "🧩 Simulation: ${SIM_ID}"
echo "🔁 Iteration: ${ITER_ID}"
echo "🧠 Cores allocated: ${SLURM_CPUS_PER_TASK}"
echo "🧪 Simulation config: ${SIM_YML}"
echo "🕒 Start time: $(date)"

# ===============================
# Validate shared simulation model
# ===============================
MODEL_PATH="${SIM_DIR}/model/model.rds"

if [[ ! -f "$MODEL_PATH" ]]; then
  echo "❌ ERROR: Shared simulation model not found:"
  echo "    $MODEL_PATH"
  exit 1
fi

# ===============================
# Run iteration engine
# ===============================
RSCRIPT_PATH="scripts/run_iter.R"

if [[ ! -f "$RSCRIPT_PATH" ]]; then
  echo "❌ ERROR: Could not find $RSCRIPT_PATH"
  exit 1
fi

Rscript --max-connections=256 \
  "$RSCRIPT_PATH" \
  "$SIM_YML"

echo "✅ Iteration complete: ${SIM_ID} / ${ITER_ID}"
echo "🕒 End time: $(date)"
