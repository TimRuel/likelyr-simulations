#!/bin/bash
set -euo pipefail

# ============================================================
# submit_exp.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Reads version from experiment$version to derive EXP_RUN_DIR
#   • Submits ONE Slurm array job per simulation
#   • Array size = simulation.iterations (minus completed)
#   • Creates per-simulation log directories
#   • Writes a submission.log per simulation
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

echo "📁 PROJECT_ROOT: $PROJECT_ROOT"
echo "🧪 Experiment config: $EXP_YML"

# ===============================
# Read version + iterations from YAML via grep/sed
# ===============================
EXP_VERSION="$(grep -m1 '^\s*version:' "$EXP_YML" | sed 's/.*version:\s*//' | tr -d '[:space:]"')"
N_ITER="$(grep -m1 '^\s*iterations:' "$EXP_YML" | sed 's/.*iterations:\s*//' | tr -d '[:space:]"')"

if [[ -z "$EXP_VERSION" ]]; then
  echo "❌ ERROR: experiment\$version missing or unparseable in $EXP_YML"
  exit 1
fi

if [[ -z "$N_ITER" || ! "$N_ITER" =~ ^[0-9]+$ ]]; then
  echo "❌ ERROR: simulation\$iterations missing or invalid in $EXP_YML"
  exit 1
fi

echo "🔖 Version: $EXP_VERSION"
echo "🔁 Iterations per simulation: $N_ITER"

# ===============================
# Derive experiment run directory
# ===============================
EXP_CFG_DIR="$(dirname "$EXP_YML")"
EXPERIMENT_REL="$(realpath --relative-to=config "$EXP_CFG_DIR")"
EXP_RUN_DIR="experiments/${EXPERIMENT_REL}/${EXP_VERSION}"

if [[ ! -d "$EXP_RUN_DIR" ]]; then
  echo "❌ ERROR: Experiment run directory not found:"
  echo "    $EXP_RUN_DIR"
  echo "Did you run: make setup ?"
  exit 1
fi

echo "📂 Run directory: $EXP_RUN_DIR"

# ===============================
# Discover simulations
# ===============================
SIM_DIRS=( "$EXP_RUN_DIR"/sim_*/ )

if [[ ! -d "${SIM_DIRS[0]}" ]]; then
  echo "❌ ERROR: No sim_* directories found in:"
  echo "    $EXP_RUN_DIR"
  echo "Did you run: make setup ?"
  exit 1
fi

SLURM_SCRIPT="bin/slurm_iter.sh"

# ===============================
# Submit Slurm jobs
# ===============================
for sim_dir in "${SIM_DIRS[@]}"; do
  sim_id="$(basename "$sim_dir")"
  sim_yml="${sim_dir}${sim_id}.yml"
  log_dir="${sim_dir}logs"

  if [[ ! -f "$sim_yml" ]]; then
    echo "❌ ERROR: Simulation config not found:"
    echo "    $sim_yml"
    exit 1
  fi

  # --------------------------------------------------
  # Check how many iterations are already complete
  # --------------------------------------------------
  n_complete=0
  pending_indices=()

  for ((i = 1; i <= N_ITER; i++)); do
    iter_id=$(printf "iter_%04d" "$i")
    model_file="${sim_dir}iterations/${iter_id}/model.rds"
    if [[ -f "$model_file" ]]; then
      (( n_complete++ )) || true
    else
      pending_indices+=($((i - 1)))   # 0-indexed for SLURM
    fi
  done

  n_pending="${#pending_indices[@]}"

  if [[ "$n_pending" -eq 0 ]]; then
    echo "✔ Skipping ${sim_id} — all ${N_ITER} iterations already complete"
    continue
  fi

  if [[ "$n_complete" -gt 0 ]]; then
    echo "⏭  ${sim_id}: ${n_complete}/${N_ITER} already complete, submitting ${n_pending} remaining"
  fi

  # --------------------------------------------------
  # Build SLURM array specification from pending indices
  # --------------------------------------------------
  array_spec=$(printf "%s," "${pending_indices[@]}")
  array_spec="${array_spec%,}"

  # --------------------------------------------------
  # Create log directory and submit
  # --------------------------------------------------
  mkdir -p "$log_dir"

  echo "🚀 Submitting ${sim_id}  (--array=${array_spec})"

  job_output="$(
    sbatch \
      --array="${array_spec}" \
      --output="${log_dir}/iter_%a.out" \
      --error="${log_dir}/iter_%a.err" \
      "$SLURM_SCRIPT" \
      "$sim_yml"
  )"

  job_id="$(echo "$job_output" | awk '{print $NF}')"
  echo "   Job ID: ${job_id}"

  # --------------------------------------------------
  # Write submission log
  # --------------------------------------------------
  {
    echo "========================================"
    echo "Submitted: $(date)"
    echo "Job ID:    ${job_id}"
    echo "Array:     ${array_spec}"
    echo "Pending:   ${n_pending} / ${N_ITER}"
    echo "Complete:  ${n_complete} / ${N_ITER}"
    echo "========================================"
  } >> "${log_dir}/submission.log"

done

echo "✔ All simulations submitted successfully"