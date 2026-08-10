#!/bin/bash
#SBATCH --account=p32397
#SBATCH --partition=short
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
# NOTE: --array, --output and --error are passed as CLI overrides from
#       submit_analysis.sh so logs land in the experiment's own log dir.

set -euo pipefail

# ============================================================
# slurm_analyze.sh
#
# Contract:
#   • Called by submit_analysis.sh via sbatch --array
#   • Analyzes ONE simulation (array index selects which sim yaml)
#   • Delegates to bin/analyze_sim.sh, which dispatches on
#     experiment$kind to the right analyzer
#
# Why analysis needs a job at all: a simulation experiment's analyzer
# reads every iteration's model.rds and runs infer() + compare() on each
# one — for a 48-sim x 1100-iteration experiment that is ~53,000 model
# loads, hours of serial work that must not run on a login node. One
# array task per simulation turns it into 48 concurrent ~20-minute jobs.
#
# An application experiment is 20 sims x 1 iteration and is light enough
# to analyze directly on a login node via `make results`; this path exists
# for the simulation case, but works for either kind.
# ============================================================

# ===============================
# Load environment modules (HPC only)
#
# nlopt is loaded to match slurm_iter.sh: library(likelyr) pulls in
# nloptr, which needs the shared library present even when the analyzer
# itself does no constrained solving.
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
#
# Receives the whole sim yaml list; the array index picks one. Passing the
# list rather than one path per job keeps a single sbatch submission.
# ===============================
if [[ $# -lt 1 ]]; then
  echo "❌ ERROR: Missing arguments."
  echo "Usage: sbatch --array=1-N $0 <sim_01.yml> [sim_02.yml ...]"
  exit 1
fi

SIM_YMLS=( "$@" )

if [[ -z "${SLURM_ARRAY_TASK_ID:-}" ]]; then
  echo "❌ ERROR: SLURM_ARRAY_TASK_ID not set — submit with --array."
  exit 1
fi

IDX=$(( SLURM_ARRAY_TASK_ID - 1 ))

if [[ "$IDX" -lt 0 || "$IDX" -ge "${#SIM_YMLS[@]}" ]]; then
  echo "❌ ERROR: array index ${SLURM_ARRAY_TASK_ID} out of range (1-${#SIM_YMLS[@]})"
  exit 1
fi

SIM_YML="${SIM_YMLS[$IDX]}"

# ===============================
# Resolve project root
# ===============================
PROJECT_ROOT="$SLURM_SUBMIT_DIR"
cd "$PROJECT_ROOT" || {
  echo "❌ ERROR: Failed to cd into $PROJECT_ROOT"
  exit 1
}

echo "📁 PROJECT_ROOT: ${PROJECT_ROOT}"
echo "🧩 Sim yaml:     ${SIM_YML}"
echo "🕒 Start time:   $(date)"
echo ""

bash bin/analyze_sim.sh "$SIM_YML"

echo ""
echo "🕒 End time: $(date)"
