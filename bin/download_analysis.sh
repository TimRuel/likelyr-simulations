#!/bin/bash
# ======================================================================
# download_analysis.sh
#
# Brings an experiment's analysis results down from Quest, preserving
# the local experiments/ layout.
#
# Two modes:
#
#   bundle (default)  Downloads only <exp>/analysis/bundle.rds — one
#                     file, produced by `make bundle` on Quest. This is
#                     the mode the dissertation figures consume.
#
#   tree              Downloads every sim_*/analysis/ folder, streamed as
#                     a single tar over one SSH connection. Use this when
#                     you want the per-sim files themselves (debugging a
#                     specific simulation, or a partially-analyzed
#                     experiment with no bundle yet).
#
# Usage:
#   bash download_analysis.sh <remote_exp_path> <local_exp_path> [mode]
#
# Example:
#   bash download_analysis.sh \
#     multinom/ne_entropy/exp_v13 \
#     multinom/ne_entropy/exp_v13
#
# The remote base is always:
#   tbr0780@quest.northwestern.edu:/projects/p32397/likelyr-simulations/experiments/
#
# The local base is always:
#   /c/Northwestern/likelyr-simulations/experiments/
#
# Results deliberately live here and are NOT copied into the dissertation
# repo — Dissertation/R/load-results.R reads them out of this tree via
# LIKELYR_SIMS_DIR, so there is exactly one copy of every result and no
# binary churn in the dissertation's git history.
#
# Must be run from the local machine with Northwestern VPN active.
# ======================================================================

set -euo pipefail

REMOTE_USER="tbr0780"
REMOTE_HOST="quest.northwestern.edu"
REMOTE_BASE="/projects/p32397/likelyr-simulations/experiments"
LOCAL_BASE="/c/Northwestern/likelyr-simulations/experiments"

if [[ $# -lt 2 ]]; then
  echo "Usage: bash download_analysis.sh <remote_exp_path> <local_exp_path> [bundle|tree]"
  echo "  e.g. bash download_analysis.sh multinom/ne_entropy/exp_v13 multinom/ne_entropy/exp_v13"
  exit 1
fi

REMOTE_PATH="${REMOTE_BASE}/$(echo "${1}" | sed 's|^experiments/||')"
LOCAL_PATH="${LOCAL_BASE}/$(echo "${2}" | sed 's|^experiments/||')"
MODE="${3:-bundle}"

SSH="C:/Windows/System32/OpenSSH/ssh.exe"

echo "============================================================"
echo "Downloading analysis results  (mode: ${MODE})"
echo "  From: ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}"
echo "  To:   ${LOCAL_PATH}"
echo "============================================================"

case "$MODE" in
  bundle)
    mkdir -p "${LOCAL_PATH}/analysis"

    # cat over SSH rather than scp: one connection, and it fails loudly
    # with the remote's own error message if the bundle isn't there yet.
    if ! "${SSH}" "${REMOTE_USER}@${REMOTE_HOST}" \
      "cat ${REMOTE_PATH}/analysis/bundle.rds" \
      > "${LOCAL_PATH}/analysis/bundle.rds"; then
      rm -f "${LOCAL_PATH}/analysis/bundle.rds"
      echo ""
      echo "❌ Could not fetch ${REMOTE_PATH}/analysis/bundle.rds"
      echo "   On Quest, run:"
      echo "     make analyze-exp EXP_CONFIG=<path/to/exp_vX.yml>"
      echo "     make bundle      EXP_CONFIG=<path/to/exp_vX.yml>"
      echo "   Or re-run this with mode 'tree' to pull the per-sim files."
      exit 1
    fi

    echo "Downloaded: ${LOCAL_PATH}/analysis/bundle.rds"
    ;;

  tree)
    mkdir -p "${LOCAL_PATH}"

    # Stream a tar archive of all sim_*/analysis folders over a single SSH
    # connection and extract locally — avoids repeated SSH handshakes
    "${SSH}" "${REMOTE_USER}@${REMOTE_HOST}" \
      "cd ${REMOTE_PATH} && tar czf - sim_*/analysis/" | \
      tar xzf - -C "${LOCAL_PATH}"

    echo "Extracted per-sim analysis folders into: ${LOCAL_PATH}"
    ;;

  *)
    echo "❌ Unknown mode '${MODE}'. Expected 'bundle' or 'tree'."
    exit 1
    ;;
esac

echo "Done."
