#!/bin/bash
# ======================================================================
# download_analysis.sh
#
# Downloads analysis subfolders from a Quest experiment directory to
# a local destination, preserving the sim folder structure.
#
# Usage:
#   bash download_analysis.sh <remote_exp_path> <local_exp_path>
#
# Example:
#   bash download_analysis.sh \
#     multinom/logit_simpson/exp_v1 \
#     multinom/logit_simpson/exp_v1
#
# The remote base is always:
#   tbr0780@quest.northwestern.edu:/projects/p32397/likelyr-simulations/experiments/
#
# The local base is always:
#   /c/Northwestern/likelyr-simulations/experiments/
# ======================================================================

set -euo pipefail

REMOTE_USER="tbr0780"
REMOTE_HOST="quest.northwestern.edu"
REMOTE_BASE="/projects/p32397/likelyr-simulations/experiments"
LOCAL_BASE="/c/Northwestern/likelyr-simulations/experiments"

if [[ $# -lt 2 ]]; then
  echo "Usage: bash download_analysis.sh <remote_exp_path> <local_exp_path>"
  echo "  e.g. bash download_analysis.sh multinom/logit_simpson/exp_v1 multinom/logit_simpson/exp_v1"
  exit 1
fi

REMOTE_PATH="${REMOTE_BASE}/$(echo "${1}" | sed 's|^experiments/||')"
LOCAL_PATH="${LOCAL_BASE}/$(echo "${2}" | sed 's|^experiments/||')"

SSH="C:/Windows/System32/OpenSSH/ssh.exe"

echo "============================================================"
echo "Downloading analysis folders"
echo "  From: ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}"
echo "  To:   ${LOCAL_PATH}"
echo "============================================================"

mkdir -p "${LOCAL_PATH}"

# Stream a tar archive of all sim_*/analysis folders over a single SSH
# connection and extract locally — avoids repeated SSH handshakes
"${SSH}" "${REMOTE_USER}@${REMOTE_HOST}" \
  "cd ${REMOTE_PATH} && tar czf - sim_*/analysis/" | \
  tar xzf - -C "${LOCAL_PATH}"

echo "Done."

echo "Done."