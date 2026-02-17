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
#
# Usage:
#   bash jobs/test_iter.sh experiments/<exp>/<sim>/simulation.yml [iter_index]
# ============================================================

# ===============================
# Load environment modules
# ===============================
module purge all
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
# Declare TEST execution mode
# ===============================
export LIKELYR_EXEC_MODE=test
export LIKELYR_TEST_ITER="$ITER_INDEX"

# ===============================
# Run test iteration
# ===============================
echo "🧪 Local test iteration"
echo "📁 PROJECT_ROOT: ${PROJECT_ROOT}"
echo "🧩 Simulation:  ${SIM_YML}"
echo "🔁 Iteration:   test_$(printf "%04d" "$ITER_INDEX")"

Rscript scripts/run_iter.R "$SIM_YML"

echo "✅ Local test iteration complete"
