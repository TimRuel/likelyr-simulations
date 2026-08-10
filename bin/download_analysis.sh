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

# ======================================================================
# Run ssh without MSYS path mangling
#
# ssh.exe is a NATIVE Windows binary, so when this script runs under an
# MSYS2 shell — which is what `make` uses on Windows, via
# SHELL := C:/rtools45/usr/bin/bash.exe — MSYS2 rewrites any argument that
# looks like a POSIX absolute path into a Windows path before ssh sees it.
# The remote command "cat /projects/p32397/..." therefore arrived at Quest
# as "cat C:/rtools45/projects/p32397/...", and the remote cat failed while
# this script's own messages still printed the correct path. Maddening.
#
# The two variables disable that conversion (MSYS_NO_PATHCONV for Git Bash,
# MSYS2_ARG_CONV_EXCL for MSYS2/Rtools). They are set per-invocation rather
# than exported, so the local `tar` below — an MSYS binary that genuinely
# wants POSIX paths — is unaffected.
#
# On Linux and macOS both variables are simply ignored.
# ======================================================================
run_ssh() {
  MSYS_NO_PATHCONV=1 MSYS2_ARG_CONV_EXCL='*' \
    "${SSH}" "${REMOTE_USER}@${REMOTE_HOST}" "$@"
}

case "$MODE" in
  bundle)
    REMOTE_BUNDLE="${REMOTE_PATH}/analysis/bundle.rds"

    mkdir -p "${LOCAL_PATH}/analysis"

    # Check existence before writing anything, so a missing bundle can't
    # leave a truncated or error-text file behind, and so the failure names
    # the real cause instead of being inferred from a broken transfer.
    #
    # `|| STATUS=$?` rather than `if ! run_ssh ...` because inside an
    # `if !` block $? is the negation's status, not ssh's — and the
    # distinction matters: 255 is ssh itself failing (VPN, auth, host),
    # anything else is the remote `test -f` reporting the file is absent.
    SSH_STATUS=0
    run_ssh "test -f '${REMOTE_BUNDLE}'" || SSH_STATUS=$?

    if [[ "$SSH_STATUS" -ne 0 ]]; then
      echo ""
      if [[ "$SSH_STATUS" -eq 255 ]]; then
        echo "❌ Could not connect to ${REMOTE_HOST} (ssh exit 255)."
        echo "   This is a connection failure, not a missing file —"
        echo "   check that the Northwestern VPN is active."
      else
        echo "❌ Connected fine, but no bundle at:"
        echo "     ${REMOTE_BUNDLE}"
        echo "   (remote test -f exit ${SSH_STATUS})"
        echo ""
        echo "   On Quest:"
        echo "     make results EXP_CONFIG=<path/to/exp_vX.yml>"
        echo "   Or re-run this with MODE=tree to pull the per-sim files."
      fi
      exit 1
    fi

    # cat rather than scp: one connection, and stdout streams straight to
    # the destination without a temp file.
    SSH_STATUS=0
    run_ssh "cat '${REMOTE_BUNDLE}'" > "${LOCAL_PATH}/analysis/bundle.rds" \
      || SSH_STATUS=$?

    if [[ "$SSH_STATUS" -ne 0 ]]; then
      rm -f "${LOCAL_PATH}/analysis/bundle.rds"
      echo ""
      echo "❌ Transfer failed for ${REMOTE_BUNDLE} (ssh exit ${SSH_STATUS})"
      exit 1
    fi

    # A zero-byte result means the transfer "succeeded" without data —
    # worth catching here rather than as a confusing readRDS error later.
    if [[ ! -s "${LOCAL_PATH}/analysis/bundle.rds" ]]; then
      rm -f "${LOCAL_PATH}/analysis/bundle.rds"
      echo ""
      echo "❌ Downloaded 0 bytes from ${REMOTE_BUNDLE}"
      exit 1
    fi

    echo "Downloaded: ${LOCAL_PATH}/analysis/bundle.rds"
    ls -lh "${LOCAL_PATH}/analysis/bundle.rds" | awk '{print "  size: " $5}'
    ;;

  tree)
    mkdir -p "${LOCAL_PATH}"

    # Stream a tar archive of all sim_*/analysis folders over a single SSH
    # connection and extract locally — avoids repeated SSH handshakes
    run_ssh "cd '${REMOTE_PATH}' && tar czf - sim_*/analysis/" | \
      tar xzf - -C "${LOCAL_PATH}"

    echo "Extracted per-sim analysis folders into: ${LOCAL_PATH}"
    ;;

  *)
    echo "❌ Unknown mode '${MODE}'. Expected 'bundle' or 'tree'."
    exit 1
    ;;
esac

echo "Done."
