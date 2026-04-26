#!/bin/bash
set -euo pipefail

# ============================================================
# submit_exp.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Reads exp_dir and logs_dir from the YAML directly
#   • Submits ONE Slurm array job per simulation
#   • Array size = simulation.iterations (minus completed)
#   • Creates per-simulation log directories under logs_dir
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
# Read paths + iterations from YAML
# ===============================
EXP_VERSION="$(grep -m1 '^\s*version:' "$EXP_YML" | sed 's/.*version:\s*//' | tr -d '[:space:]"')"
EXP_RUN_DIR="$(grep -m1 '^\s*exp_dir:' "$EXP_YML" | sed 's/.*exp_dir:\s*//' | tr -d '[:space:]"')"
LOGS_DIR="$(grep -m1 '^\s*logs_dir:' "$EXP_YML" | sed 's/.*logs_dir:\s*//' | tr -d '[:space:]"')"
N_ITER="$(grep -m1 '^\s*iterations:' "$EXP_YML" | sed 's/.*iterations:\s*//' | tr -d '[:space:]"')"

if [[ -z "$EXP_VERSION" ]]; then
  echo "❌ ERROR: experiment\$version missing or unparseable in $EXP_YML"
  exit 1
fi

if [[ -z "$EXP_RUN_DIR" ]]; then
  echo "❌ ERROR: experiment\$exp_dir missing or unparseable in $EXP_YML"
  exit 1
fi

if [[ -z "$LOGS_DIR" ]]; then
  echo "❌ ERROR: experiment\$logs_dir missing or unparseable in $EXP_YML"
  exit 1
fi

if [[ -z "$N_ITER" || ! "$N_ITER" =~ ^[0-9]+$ ]]; then
  echo "❌ ERROR: simulation\$iterations missing or invalid in $EXP_YML"
  exit 1
fi

echo "🔖 Version:    $EXP_VERSION"
echo "📂 Exp dir:    $EXP_RUN_DIR"
echo "📋 Logs dir:   $LOGS_DIR"
echo "🔁 Iterations: $N_ITER"

# ===============================
# Validate experiment directory
# ===============================
if [[ ! -d "$EXP_RUN_DIR" ]]; then
  echo "❌ ERROR: Experiment run directory not found:"
  echo "    $EXP_RUN_DIR"
  echo "Did you run: make setup ?"
  exit 1
fi

# ===============================
# Discover sim yamls from config subfolder
# ===============================
CONFIG_SIM_DIR="$(dirname "$EXP_YML")"
SIM_YMLS=( "$CONFIG_SIM_DIR"/sim_*.yml )

if [[ ! -f "${SIM_YMLS[0]}" ]]; then
  echo "❌ ERROR: No sim_*.yml files found in:"
  echo "    $CONFIG_SIM_DIR"
  echo "Did you run: make setup ?"
  exit 1
fi

SLURM_SCRIPT="bin/slurm_iter.sh"

# ===============================
# Submit Slurm jobs
# ===============================
for sim_yml in "${SIM_YMLS[@]}"; do
  sim_id="$(basename "$sim_yml" .yml)"
  sim_dir="${EXP_RUN_DIR}/${sim_id}/"
  log_dir="${LOGS_DIR}/${sim_id}"

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
      pending_indices+=("$i")   # 1-indexed to match iter folder names
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
  # Build SLURM array specification from pending indices.
  # Uses ranges (e.g. "1-500,502-1000") rather than
  # comma-separated lists to avoid sbatch pathname length limits.
  # --------------------------------------------------
  array_spec=""
  range_start="${pending_indices[0]}"
  range_end="${pending_indices[0]}"

  for ((k = 1; k < ${#pending_indices[@]}; k++)); do
    curr="${pending_indices[$k]}"
    prev="${pending_indices[$((k-1))]}"
    if [[ "$curr" -eq "$((prev + 1))" ]]; then
      range_end="$curr"
    else
      if [[ "$range_start" -eq "$range_end" ]]; then
        array_spec="${array_spec:+${array_spec},}${range_start}"
      else
        array_spec="${array_spec:+${array_spec},}${range_start}-${range_end}"
      fi
      range_start="$curr"
      range_end="$curr"
    fi
  done
  # Flush final range
  if [[ "$range_start" -eq "$range_end" ]]; then
    array_spec="${array_spec:+${array_spec},}${range_start}"
  else
    array_spec="${array_spec:+${array_spec},}${range_start}-${range_end}"
  fi

  # --------------------------------------------------
  # Create log directory and submit
  # --------------------------------------------------
  mkdir -p "$log_dir"

  echo "🚀 Submitting ${sim_id}  (--array=${array_spec})"

  job_output="$(
    sbatch \
      --array="${array_spec}" \
      --output="${log_dir}/iter_%04a.out" \
      --error="${log_dir}/iter_%04a.err" \
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