#!/bin/bash
set -euo pipefail

# ============================================================
# submit_exp.sh
#
# Contract:
#   • Accepts <path/to/experiment.yml> (runtime snapshot)
#   • Submits ONE Slurm array job per simulation
#   • Array size = simulation.iterations
#   • Uses filesystem as the source of truth
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

echo "📁 PROJECT_ROOT: $PROJECT_ROOT"
echo "🧪 Experiment snapshot: $EXP_YML"

# ===============================
# Resolve experiment directory
# ===============================
EXP_RUN_DIR="$(dirname "$EXP_YML")"

if [[ ! -d "$EXP_RUN_DIR" ]]; then
  echo "❌ ERROR: Experiment directory not found:"
  echo "    $EXP_RUN_DIR"
  exit 1
fi

# ===============================
# Extract iterations per simulation
# ===============================
N_ITER="$(
  Rscript -e "
    suppressPackageStartupMessages(library(yaml))
    cfg <- read_yaml('$EXP_YML')
    it <- cfg[['simulation']][['iterations']]
    if (is.null(it) || it < 1) quit(status = 1)
    cat(it)
  "
)"

if [[ -z "$N_ITER" ]]; then
  echo "❌ ERROR: simulation.iterations missing or invalid in experiment.yml"
  exit 1
fi

echo "🔁 Iterations per simulation: $N_ITER"

# ===============================
# Discover simulations
# ===============================
SIM_DIRS=( "$EXP_RUN_DIR"/sim_* )

if [[ ! -d "${SIM_DIRS[0]}" ]]; then
  echo "❌ ERROR: No sim_* directories found in:"
  echo "    $EXP_RUN_DIR"
  echo "Did you run: make setup ?"
  exit 1
fi

# ===============================
# Submit Slurm jobs
# ===============================
SLURM_SCRIPT="jobs/slurm_iter.sh"

for sim_dir in "${SIM_DIRS[@]}"; do
  sim_id="$(basename "$sim_dir")"
  sim_yml="$sim_dir/simulation.yml"

  if [[ ! -f "$sim_yml" ]]; then
    echo "❌ ERROR: Missing simulation.yml:"
    echo "    $sim_yml"
    exit 1
  fi

  echo "🚀 Submitting ${sim_id}  (--array=0-$((N_ITER - 1)))"

  sbatch \
    --array=0-$((N_ITER - 1)) \
    "$SLURM_SCRIPT" \
    "$sim_yml"
done

echo "✔ All simulations submitted successfully"
