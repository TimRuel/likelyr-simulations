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

# ======================================================================
# Choose an ssh that actually runs
#
# This used to hardcode C:/Windows/System32/OpenSSH/ssh.exe. That is the
# NATIVE Windows OpenSSH, and it cannot be executed from the MSYS shell
# `make` uses on Windows (SHELL := C:/rtools45/usr/bin/bash.exe in the
# Makefile). It does not error usefully — it produces NO output at all and
# exits 255, even for `ssh -V`, which cannot legitimately fail. Other
# System32 binaries (hostname.exe, whoami.exe) run fine from that shell, so
# the breakage is specific to ssh.exe/scp.exe. Git's MSYS-built ssh works
# there. Diagnosed 2026-08-22 after `make download` failed with a "check
# the VPN" message while the network, auth and remote file were all fine.
#
# So do not trust a path: verify the candidate RUNS. `ssh -V` printing a
# version is the check that separates a working binary from the broken one
# — a `-x` file test passes on both, which is exactly how this hid.
#
# Order matters. `ssh` on PATH comes first so Linux and macOS (and Quest)
# pick the system ssh and never touch the Windows-specific fallbacks.
# Override with LIKELYR_SSH=/path/to/ssh to force a specific binary.
# ======================================================================
ssh_runs() {
  # Version goes to stderr on most builds; require non-empty output AND a
  # clean exit. `|| true` so `set -e` does not abort the probe itself.
  local out
  out="$("$1" -V 2>&1 || true)"
  [[ -n "$out" ]]
}

# The override is accepted as $4 as well as via LIKELYR_SSH, because an
# environment variable is not reliable here: C:/rtools45/usr/bin/bash.exe
# -lc — the recipe shell the Makefile selects on Windows — WIPES exported
# variables (verified: an exported var is empty inside it, while Git Bash
# -lc preserves it). An argument cannot be lost that way, so the Makefile
# passes $(LIKELYR_SSH) through positionally.
SSH_WANT="${4:-${LIKELYR_SSH:-}}"

SSH=""
SSH_TRIED=()

for cand in \
  ${SSH_WANT:-} \
  "$(command -v ssh 2>/dev/null || true)" \
  "C:/Git/usr/bin/ssh.exe" \
  "C:/Program Files/Git/usr/bin/ssh.exe" \
  "C:/Windows/System32/OpenSSH/ssh.exe"
do
  [[ -z "$cand" ]] && continue
  SSH_TRIED+=("$cand")
  if [[ -x "$cand" ]] && ssh_runs "$cand"; then
    SSH="$cand"
    break
  fi
  # An explicit request that fails the probe must not be swallowed. Falling
  # through silently would be the same silent-fallback trap this whole
  # detection block exists to remove.
  if [[ -n "${SSH_WANT:-}" && "$cand" == "${SSH_WANT}" ]]; then
    echo "⚠  Requested ssh ${cand} rejected ('ssh -V' produced no output);" \
      "falling back to autodetection." >&2
  fi
done

if [[ -z "$SSH" ]]; then
  echo "❌ No working ssh found. Tried:"
  for t in "${SSH_TRIED[@]}"; do
    if [[ -x "$t" ]]; then
      echo "     $t  (exists, but 'ssh -V' produced no output)"
    else
      echo "     $t  (not executable)"
    fi
  done
  echo ""
  echo "   Set LIKELYR_SSH=/path/to/ssh to point at one explicitly."
  exit 1
fi

echo "============================================================"
echo "Downloading analysis results  (mode: ${MODE})"
echo "  ssh:  ${SSH}"
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
        # 255 is ssh's catch-all: network, auth, host-key, or a binary that
        # will not launch. The binary was verified above, so that last one
        # is ruled out — but do NOT claim to know which of the rest it is.
        # Blaming the VPN unconditionally is what sent a real debugging
        # session (2026-08-22) chasing a network problem that did not exist
        # while the actual cause was an unrunnable ssh.exe.
        echo "❌ ssh exited 255 talking to ${REMOTE_HOST}."
        echo "   Using: ${SSH}  (verified runnable, so this is not a"
        echo "   broken ssh binary — it is the connection itself.)"
        echo ""
        echo "   255 covers several causes. To see which, run it directly:"
        echo "     ${SSH} -v ${REMOTE_USER}@${REMOTE_HOST} true"
        echo ""
        echo "   Common ones: off-campus without the Northwestern VPN, an"
        echo "   ssh key not loaded, or a changed host key."
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
