#!/bin/bash
#SBATCH --account=p32397
#SBATCH --partition=short
#SBATCH --time=02:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=timothyruel2024@u.northwestern.edu
#SBATCH --job-name=likelyr_sim
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem=64G
#SBATCH --array=0-1

set -euo pipefail

# ===============================
# Load environment modules
# ===============================
module purge
module load R/4.5.1
module load nlopt/2.7.1-gcc-12.3.0

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
  echo "Usage: sbatch $0 <config.yml>"
  exit 1
fi

CONFIG_PATH="$1"

if [[ ! -f "$CONFIG_PATH" ]]; then
  echo "❌ ERROR: Config file not found:"
  echo "    $CONFIG_PATH"
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
# Extract experiment ID (for logging only)
# ===============================
EXP_ID="$(Rscript -e "cat(yaml::read_yaml('$CONFIG_PATH')\$experiment\$id)")"

if [[ -z "$EXP_ID" ]]; then
  echo "❌ ERROR: experiment$id not found in config."
  exit 1
fi

# ===============================
# Slurm array bookkeeping
# ===============================
SIM_NUM=$((SLURM_ARRAY_TASK_ID + 1))
SIM_ID=$(printf "sim_%04d" "$SIM_NUM")

# ===============================
# Logging setup (BEST PRACTICE)
# ===============================
SIM_DIR="experiments/${EXP_ID}/simulations/${SIM_ID}"
LOG_DIR="${SIM_DIR}/logs"
mkdir -p "$LOG_DIR"

LOG_FILE="${LOG_DIR}/slurm.out"

# Redirect *all* subsequent stdout/stderr
exec > "$LOG_FILE" 2>&1

# ===============================
# Begin logged output
# ===============================
echo "🚀 Starting simulation"
echo "📁 PROJECT_ROOT: ${PROJECT_ROOT}"
echo "🧪 Experiment ID: ${EXP_ID}"
echo "🧩 Simulation ID: ${SIM_ID}"
echo "🧠 Cores allocated: ${SLURM_CPUS_PER_TASK}"
echo "🧪 Config file: ${CONFIG_PATH}"
echo "🕒 Start time: $(date)"

# ===============================
# Run simulation R script
# ===============================
RSCRIPT_PATH="scripts/run_simulation.R"

if [[ ! -f "$RSCRIPT_PATH" ]]; then
  echo "❌ ERROR: Could not find $RSCRIPT_PATH"
  exit 1
fi

Rscript --max-connections=256 "$RSCRIPT_PATH" "$CONFIG_PATH"

echo "✅ Simulation complete: ${SIM_ID}"
echo "🕒 End time: $(date)"
