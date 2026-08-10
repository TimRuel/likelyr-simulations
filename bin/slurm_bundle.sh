#!/bin/bash
#SBATCH --account=p32397
#SBATCH --partition=short
#SBATCH --time=00:30:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G

set -euo pipefail

# ============================================================
# slurm_bundle.sh
#
# Contract:
#   • Submitted by submit_analysis.sh with a dependency on the analysis
#     array job, so it runs once every simulation has been analyzed
#   • Collapses all sim_*/analysis into <exp_dir>/analysis/bundle.rds
#
# Submitted with --dependency=afterany (not afterok) deliberately: if a
# few simulations fail to analyze, the bundle should still be built from
# the ones that succeeded. bundle_exp.R reports how many of the total it
# actually found, so a partial bundle is visible rather than silent.
# ============================================================

if command -v module >/dev/null 2>&1; then
  module purge
  module load R/4.5.1
  module load nlopt/2.7.1-gcc-12.3.0
fi

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

if [[ $# -ne 1 ]]; then
  echo "❌ ERROR: Missing arguments."
  echo "Usage: sbatch $0 <path/to/exp_vX.yml>"
  exit 1
fi

EXP_YML="$1"

PROJECT_ROOT="$SLURM_SUBMIT_DIR"
cd "$PROJECT_ROOT" || {
  echo "❌ ERROR: Failed to cd into $PROJECT_ROOT"
  exit 1
}

echo "🕒 Start time: $(date)"
echo ""

bash bin/bundle_exp.sh "$EXP_YML"

echo ""
echo "🕒 End time: $(date)"
