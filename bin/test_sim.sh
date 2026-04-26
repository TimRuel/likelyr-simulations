#!/bin/bash
set -euo pipefail

# ============================================================
# test_sim.sh
#
# Local test harness for a simulation — runs multiple iterations
# sequentially using the test: overrides from the sim config.
#
# Orchestrates:
#   1. R/test_sim.R         — applies test: overrides, writes test_sim.yml
#   2. R/build_model_spec.R — builds model spec from test_sim.yml
#   3. R/run_iter.R         — runs each iteration (loop)
#
# Output structure:
#   <exp_dir>/sim_XX/test_sim/
#     test_sim.yml           — sim config with test overrides applied
#     model/model.rds        — model built from test config (removed after)
#     iter_0001/model.rds
#     iter_0002/model.rds
#     ...
#
# The number of iterations is read from simulation.iterations in
# the generated test_sim.yml (after test: overrides are applied).
#
# Usage:
#   bash bin/test_sim.sh <path/to/config/.../sim_XX.yml>
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
  echo "❌ ERROR: Invalid arguments."
  echo "Usage: $0 <path/to/sim_XX.yml>"
  exit 1
fi

SIM_YML="$1"

if [[ ! -f "$SIM_YML" ]]; then
  echo "❌ ERROR: sim yaml not found:"
  echo "    $SIM_YML"
  exit 1
fi

# ===============================
# Resolve project root
# ===============================
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

# ===============================
# Read exp_dir and sim_id from yaml
# ===============================
EXP_DIR="$(grep -m1 '^\s*exp_dir:' "$SIM_YML" | sed 's/.*exp_dir:\s*//' | tr -d '[:space:]"')"
SIM_ID="$(grep -m1 '^\s*sim_id:' "$SIM_YML" | sed 's/.*sim_id:\s*//' | tr -d '[:space:]"')"

if [[ -z "$EXP_DIR" ]]; then
  echo "❌ ERROR: experiment\$exp_dir missing or unparseable in $SIM_YML"
  exit 1
fi

if [[ -z "$SIM_ID" ]]; then
  echo "❌ ERROR: simulation\$sim_id missing or unparseable in $SIM_YML"
  exit 1
fi

TEST_DIR="${EXP_DIR}/${SIM_ID}/test_sim"
TEST_YML="${TEST_DIR}/test_sim.yml"

echo "🧪 Test simulation: ${SIM_ID}"
echo "📁 PROJECT_ROOT:    ${PROJECT_ROOT}"
echo "📂 Output dir:      ${TEST_DIR}"

# ===============================
# Step 1: Create test_sim.yml
# ===============================
echo ""
echo "── Step 1: Applying test overrides ──────────────────────"
Rscript R/test_sim.R "$SIM_YML"

# ===============================
# Read number of iterations from generated test_sim.yml
# ===============================
N_ITER="$(grep -m1 '^\s*iterations:' "$TEST_YML" | sed 's/.*iterations:\s*//' | tr -d '[:space:]"')"

if [[ -z "$N_ITER" || ! "$N_ITER" =~ ^[0-9]+$ || "$N_ITER" -lt 1 ]]; then
  echo "❌ ERROR: simulation\$iterations missing or invalid in $TEST_YML"
  exit 1
fi

echo "🔁 Iterations: ${N_ITER}"

# ===============================
# Step 2: Build model spec
# ===============================
echo ""
echo "── Step 2: Building test model spec ─────────────────────"
mkdir -p "${TEST_DIR}/model"
Rscript R/build_model_spec.R "$TEST_YML"

# ===============================
# Step 3: Run iterations
# ===============================
echo ""
echo "── Step 3: Running ${N_ITER} test iteration(s) ──────────"

for ((i = 0; i < N_ITER; i++)); do
  ITER_LABEL="$(printf 'iter_%04d' $((i + 1)))"
  echo ""
  echo "  ▶ ${ITER_LABEL} ($((i + 1))/${N_ITER})"
  SLURM_ARRAY_TASK_ID="${i}" LIKELYR_EXEC_MODE=test Rscript R/run_iter.R "$TEST_YML"
done

echo ""
echo "✅ Test simulation complete: ${TEST_DIR}"

# Clean up uncalibrated model
rm -rf "${TEST_DIR}/model"
echo "✔ Removed uncalibrated model dir"