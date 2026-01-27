#!/bin/bash
#SBATCH --account=p32397
#SBATCH --partition=short
#SBATCH --time=02:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=timothyruel2024@u.northwestern.edu
#SBATCH --job-name=likelyr_sim
#SBATCH --output=/dev/null
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem=64G
#SBATCH --array=1-1000

# ===============================
# ✅ Validate CLI arguments
# ===============================
if [[ $# -ne 1 ]]; then
  echo "❌ ERROR: Missing arguments."
  echo "Usage: sbatch $0 <experiment_id>"
  exit 1
fi

EXP_ID="$1"
SIM_ID=$(printf "sim_%04d" "$SLURM_ARRAY_TASK_ID")
REQUESTED_CORES=${SLURM_CPUS_PER_TASK:-1}

# ===============================
# ✅ Resolve project root
# ===============================
PROJECT_ROOT="$SLURM_SUBMIT_DIR"
cd "$PROJECT_ROOT" || exit 1

# ===============================
# ✅ Logging setup
# ===============================
SIM_DIR="experiments/${EXP_ID}/simulations/${SIM_ID}"
LOG_DIR="${SIM_DIR}/logs"
mkdir -p "$LOG_DIR"

LOG_FILE="${LOG_DIR}/slurm.out"
exec > "$LOG_FILE" 2>&1

echo "🚀 Running ${SIM_ID} for experiment ${EXP_ID}"
echo "🧠 Cores requested: $REQUESTED_CORES"

# ===============================
# ✅ Load environment modules
# ===============================
module purge all
module load R/4.4.0
module load nlopt/2.7.1-gcc-12.3.0
module load gsl/2.7.1-gcc-12.3.0
module load fftw/3.3.10-gcc-12.3.0

# --- Prevent BLAS oversubscription ---
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

# ===============================
# ✅ Run simulation R script
# ===============================
RSCRIPT_PATH="scripts/run_simulation.R"

if [[ ! -f "$RSCRIPT_PATH" ]]; then
  echo "❌ ERROR: Could not find $RSCRIPT_PATH"
  exit 1
fi

export EXPERIMENT_ID="$EXP_ID"

Rscript --max-connections=256 "$RSCRIPT_PATH"

echo "✅ Simulation complete: ${SIM_ID}"
