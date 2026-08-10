#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# analyze_sim.sh
#
# Contract:
#   • Accepts <path/to/config/.../sim_XX.yml>
#   • Reads exp_dir and sim_id from the yaml to locate data
#   • Reads experiment$kind to choose the analyzer:
#       simulation (default) → R/analyze_sim.R
#       application          → R/analyze_app.R
#   • Skips if that analyzer's outputs already exist
#
# Why kind is declared in config rather than inferred: applications and
# simulations share the exp_vX / sim_XX naming convention and the same
# on-disk layout, so nothing about the path distinguishes them. Reading
# it from the yaml keeps `make analyze-exp` a single entry point for
# both instead of two targets to remember.
# ============================================================

# ===============================
# Load environment modules (HPC only)
# ===============================
if command -v module >/dev/null 2>&1; then
  module purge
  module load R/4.5.1
fi

# ===============================
# Expected output filenames
# (must match what the analyzers actually write)
# ===============================
SIM_OUTPUTS=(
  "sim_point_metrics.rds"
  "sim_interval_metrics.rds"
  "invalid_ci_index.rds"
)

APP_OUTPUTS=(
  "app_estimates.rds"
  "app_curves.rds"
  "app_context.rds"
)

# ===============================
# Validate CLI arguments
# ===============================
SIM_YML="${1:-}"

if [[ -z "$SIM_YML" ]]; then
  echo "Usage: $0 <path/to/sim_XX.yml>"
  exit 1
fi

if [[ ! -f "$SIM_YML" ]]; then
  echo "❌ Simulation config not found: $SIM_YML"
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
  echo "❌ experiment\$exp_dir missing or unparseable in $SIM_YML"
  exit 1
fi

if [[ -z "$SIM_ID" ]]; then
  echo "❌ simulation\$sim_id missing or unparseable in $SIM_YML"
  exit 1
fi

SIM_DIR="${EXP_DIR}/${SIM_ID}"
ANALYSIS_DIR="${SIM_DIR}/analysis"

# ===============================
# Read experiment$kind
#
# Preferred source is the sim yaml, since expand_design.R propagates the
# whole experiment: block into every sim yaml. Sim yamls generated before
# kind: existed won't have it, so fall back to the sibling exp_*.yml in
# the same config directory, then to "simulation". That keeps every
# already-expanded experiment working without a `make gen` re-run.
# ===============================
# A missing key is the NORMAL case here, not an error — the whole point of
# the fallback is that most sim yamls predate kind:. The trailing `|| true`
# is therefore load-bearing: grep exits 1 when it finds nothing, and under
# `set -euo pipefail` a failing command substitution inside an assignment
# kills the script silently, before any of the messages below can print.
read_kind() {
  local file="$1"

  [[ -f "$file" ]] || return 0

  grep -m1 '^[[:space:]]*kind:' "$file" 2>/dev/null \
    | sed 's/.*kind:[[:space:]]*//' \
    | tr -d '[:space:]"' \
    || true
}

KIND="$(read_kind "$SIM_YML")"

if [[ -z "$KIND" ]]; then
  CONFIG_SIM_DIR="$(dirname "$SIM_YML")"
  for EXP_YML in "$CONFIG_SIM_DIR"/exp_*.yml; do
    # An unmatched glob stays literal, so the -f test is what skips it.
    # Written as an explicit if/continue rather than `[[ ... ]] && break`
    # because a trailing false test as the loop body's last command makes
    # the loop itself return non-zero under `set -e`.
    [[ -f "$EXP_YML" ]] || continue

    KIND="$(read_kind "$EXP_YML")"

    if [[ -n "$KIND" ]]; then
      break
    fi
  done
fi

KIND="${KIND:-simulation}"

case "$KIND" in
  simulation)
    ANALYZER="R/analyze_sim.R"
    EXPECTED_OUTPUTS=("${SIM_OUTPUTS[@]}")
    ;;
  application)
    ANALYZER="R/analyze_app.R"
    EXPECTED_OUTPUTS=("${APP_OUTPUTS[@]}")
    ;;
  *)
    echo "❌ Unknown experiment\$kind '${KIND}' in $SIM_YML"
    echo "   Expected 'simulation' or 'application'."
    exit 1
    ;;
esac

echo "📂 Analyzing: $SIM_ID  (kind: ${KIND})"

# ===============================
# Skip if already analyzed
# ===============================
ALL_PRESENT=1
for OUT in "${EXPECTED_OUTPUTS[@]}"; do
  if [[ ! -f "$ANALYSIS_DIR/$OUT" ]]; then
    ALL_PRESENT=0
    break
  fi
done

if [[ "$ALL_PRESENT" -eq 1 ]]; then
  echo "✔ Skipping ${SIM_ID} (already analyzed)"
  exit 0
fi

mkdir -p "$ANALYSIS_DIR"
Rscript "$ANALYZER" "$SIM_DIR"

echo "✔ Analysis complete: $SIM_ID"