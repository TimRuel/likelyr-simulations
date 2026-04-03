#!/bin/bash
set -euo pipefail

# ============================================================
# test_iter.sh
#
# Local test harness for a single simulation iteration.
#
# Contract:
#   • Accepts:
#       <path/to/simulation.yml> [iter_index]
#   • Declares execution mode = test
#   • Delegates execution to run_iter.R
#   • Logs to experiments/<sim>/test_runs/test_XXXX/logs/
#
# Usage:
#   bash jobs/test_iter.sh experiments/<exp>/<sim>/simulation.yml [iter_index]
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
if [[ $# -lt 1 || $# -gt 2 ]]; then
  echo "❌ ERROR: Invalid arguments."
  echo "Usage: $0 <path/to/simulation.yml> [iter_index]"
  exit 1
fi

SIM_YML="$1"
ITER_INDEX="${2:-1}"

if [[ ! -f "$SIM_YML" ]]; then
  echo "❌ ERROR: simulation.yml not found:"
  echo "    $SIM_YML"
  exit 1
fi

if ! [[ "$ITER_INDEX" =~ ^[0-9]+$ ]] || [[ "$ITER_INDEX" -lt 1 ]]; then
  echo "❌ ERROR: iter_index must be a positive integer."
  exit 1
fi

# ===============================
# Resolve project root
# ===============================
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

# ===============================
# Resolve log path
# ===============================
SIM_DIR="$(dirname "$SIM_YML")"
ITER_ID=$(printf "test_%04d" "$ITER_INDEX")
LOG_DIR="${SIM_DIR}/test_runs/${ITER_ID}/logs"
LOG_FILE="${LOG_DIR}/test.out"

mkdir -p "$LOG_DIR"

# ===============================
# Declare TEST execution mode
# ===============================
export LIKELYR_EXEC_MODE=test
export LIKELYR_TEST_ITER="$ITER_INDEX"

# ===============================
# Run test iteration (tee to console + log file)
# ===============================
{
  echo "🧪 Local test iteration"
  echo "📁 PROJECT_ROOT: ${PROJECT_ROOT}"
  echo "🧩 Simulation:   ${SIM_YML}"
  echo "🔁 Iteration:    ${ITER_ID}"
  echo "🕒 Start time:   $(date)"
  echo ""

  Rscript scripts/run_iter.R "$SIM_YML"

  echo ""
  echo "✅ Local test iteration complete"
  echo "🕒 End time: $(date)"
} 2>&1 | tee "$LOG_FILE"