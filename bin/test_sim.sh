#!/bin/bash
set -euo pipefail

# ============================================================
# test_sim.sh
#
# Local test harness for a single simulation.
#
# Orchestrates three steps:
#   1. R/test_iter.R    — applies test: overrides, writes test_sim.yml
#   2. R/build_model_spec.R — builds model spec from test_sim.yml
#   3. R/run_iter.R     — runs one iteration using the test model
#
# Output structure:
#   sim_XX/
#     test_iteration/
#       test_sim.yml        — sim config with test overrides applied
#       model/model.rds     — model built from test config
#       model.rds           — integrated model from the test run
#
# Usage:
#   bash bin/test_sim.sh <path/to/sim_XX/sim_XX.yml>
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
  echo "Usage: $0 <path/to/sim_XX/sim_XX.yml>"
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

SIM_DIR="$(dirname "$SIM_YML")"
SIM_ID="$(basename "$SIM_DIR")"
TEST_DIR="${SIM_DIR}/test_iteration"
TEST_YML="${TEST_DIR}/test_sim.yml"

echo "🧪 Test simulation: ${SIM_ID}"
echo "📁 PROJECT_ROOT:    ${PROJECT_ROOT}"

# ===============================
# Step 1: Create test_sim.yml
# ===============================
echo ""
echo "── Step 1: Applying test overrides ──────────────────────"
Rscript R/test_iter.R "$SIM_YML"

# ===============================
# Step 2: Build model spec from test config
# ===============================
echo ""
echo "── Step 2: Building test model spec ─────────────────────"
mkdir -p "${TEST_DIR}/model"
Rscript R/build_model_spec.R "$TEST_YML"

# ===============================
# Step 3: Run iteration
# ===============================
echo ""
echo "── Step 3: Running test iteration ───────────────────────"

export LIKELYR_EXEC_MODE=test
export LIKELYR_SIM_DIR="$TEST_DIR"
export SLURM_CPUS_PER_TASK="${SLURM_CPUS_PER_TASK:-1}"

Rscript R/run_iter.R "$TEST_YML"

echo ""
echo "✅ Test simulation complete: ${TEST_DIR}"