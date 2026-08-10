#!/bin/bash
set -euo pipefail

# ============================================================
# submit_analysis.sh
#
# Contract:
#   • Accepts <path/to/exp_vX.yml> (from config/)
#   • Reads exp_dir from the YAML directly — the data lives on project
#     storage (/projects/p32397/...) while this repo lives in $HOME, so
#     the path always comes from the config, never from the repo location
#   • Submits ONE Slurm array job covering all simulations (one task per
#     simulation), then ONE dependent job that bundles the results
#   • Logs land in <exp_dir>/logs_analysis/
#
# This is the analysis counterpart to submit_exp.sh. Use it for
# simulation experiments, where analyzing means loading tens of thousands
# of model.rds files. Application experiments are small enough to analyze
# inline with `make results`.
#
# Usage:
#   bash bin/submit_analysis.sh config/multinom/logit_simpson/exp_v3/exp_v3.yml
# ============================================================

if command -v module >/dev/null 2>&1; then
  module purge
  module load R/4.5.1
fi

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

if ! command -v sbatch >/dev/null 2>&1; then
  echo "❌ ERROR: sbatch not found — run this on Quest, not locally."
  exit 1
fi

# ===============================
# Resolve project root (the repo, in $HOME)
# ===============================
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

# ===============================
# Read exp_dir from yaml (the data, on project storage)
# ===============================
# `|| true` so a missing exp_dir reaches the error message below instead of
# killing the script silently via set -e on a failed command substitution.
EXP_RUN_DIR="$(grep -m1 '^[[:space:]]*exp_dir:' "$EXP_YML" | sed 's/.*exp_dir:[[:space:]]*//' | tr -d '[:space:]"' || true)"

if [[ -z "$EXP_RUN_DIR" ]]; then
  echo "❌ ERROR: experiment\$exp_dir missing or unparseable in $EXP_YML"
  exit 1
fi

if [[ ! -d "$EXP_RUN_DIR" ]]; then
  echo "❌ ERROR: Experiment run directory not found:"
  echo "    $EXP_RUN_DIR"
  exit 1
fi

echo "📁 Repo:     $PROJECT_ROOT"
echo "📂 Exp dir:  $EXP_RUN_DIR"

# ===============================
# Discover sim yamls
# ===============================
CONFIG_SIM_DIR="$(dirname "$EXP_YML")"
SIM_YMLS=( "$CONFIG_SIM_DIR"/sim_*.yml )

if [[ ! -f "${SIM_YMLS[0]}" ]]; then
  echo "❌ ERROR: No sim_*.yml files found in:"
  echo "    $CONFIG_SIM_DIR"
  exit 1
fi

N_SIMS="${#SIM_YMLS[@]}"

echo "🔢 Simulations: $N_SIMS"

# ===============================
# Log directory
# ===============================
LOG_DIR="${EXP_RUN_DIR}/logs_analysis"
mkdir -p "$LOG_DIR"

# ===============================
# Submit analysis array
#
# The array covers every simulation unconditionally; analyze_sim.sh
# itself skips any simulation already analyzed, so a re-submission after
# a partial failure costs one near-instant task per finished sim rather
# than needing the pending set computed here.
# ===============================
echo "🚀 Submitting analysis array (1-${N_SIMS})"

ANALYZE_JOB="$(
  sbatch \
    --array="1-${N_SIMS}" \
    --job-name="likelyr_analyze" \
    --output="${LOG_DIR}/analyze_%04a.out" \
    --error="${LOG_DIR}/analyze_%04a.err" \
    bin/slurm_analyze.sh \
    "${SIM_YMLS[@]}" \
  | awk '{print $NF}'
)"

echo "   Job ID: ${ANALYZE_JOB}"

# ===============================
# Submit dependent bundle job
# ===============================
echo "🚀 Submitting bundle job (after analysis array)"

BUNDLE_JOB="$(
  sbatch \
    --dependency="afterany:${ANALYZE_JOB}" \
    --job-name="likelyr_bundle" \
    --output="${LOG_DIR}/bundle.out" \
    --error="${LOG_DIR}/bundle.err" \
    bin/slurm_bundle.sh \
    "$EXP_YML" \
  | awk '{print $NF}'
)"

echo "   Job ID: ${BUNDLE_JOB}"
echo ""
echo "✔ Submitted. Watch with: squeue -u \$USER"
echo "  Logs: ${LOG_DIR}"
echo ""
echo "  When the bundle job finishes, download locally with:"
echo "    make download EXP=$(echo "$EXP_RUN_DIR" | sed 's|.*/experiments/||')"
