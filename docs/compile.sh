#!/usr/bin/env bash
# Compile *-cheatsheet.tex (nvim or helix) to PDF using Docker + TeX Live.
#
# Usage:
#   ./compile.sh                 — nvim, both screen (dark) + print (light)
#   ./compile.sh helix           — helix cheatsheet, both variants
#   ./compile.sh nvim            — nvim cheatsheet, both variants
#   ./compile.sh helix screen    — helix dark only
#   ./compile.sh print           — nvim light only
#   ./compile.sh helix print     — helix light only
#
# Outputs (in the same directory as this script):
#   nvim-cheatsheet-screen.pdf   — One Dark background, for screen viewing
#   nvim-cheatsheet-print.pdf    — white background, ready to print
#   helix-cheatsheet-screen.pdf
#   helix-cheatsheet-print.pdf
#
# Requirements: Docker (image pulled automatically on first run, ~5 GB)

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SHEET="nvim"
FIRST="${1:-}"
if [[ "$FIRST" == "helix" || "$FIRST" == "nvim" ]]; then
  SHEET="$FIRST"
  shift
  VARIANT="${1:-both}"
elif [[ "$FIRST" == "screen" || "$FIRST" == "print" || "$FIRST" == "both" || -z "$FIRST" ]]; then
  VARIANT="${FIRST:-both}"
else
  echo "Unknown first arg: $FIRST (use nvim|helix or screen|print|both)" >&2
  exit 1
fi
IMAGE="texlive/texlive:latest"

run_latex() {
    local input="$1"
    local jobname="$2"
    echo "  pdflatex → ${jobname}.pdf"
    docker run --rm \
        -v "${DIR}:/workspace" \
        -w /workspace \
        "${IMAGE}" \
        pdflatex \
            -interaction=nonstopmode \
            -halt-on-error \
            -jobname="${jobname}" \
            "${input}"
    # Remove auxiliary files, keep only the PDF
    rm -f "${DIR}/${jobname}.aux" \
          "${DIR}/${jobname}.log" \
          "${DIR}/${jobname}.out"
}

if [[ "${VARIANT}" != "screen" && "${VARIANT}" != "print" && "${VARIANT}" != "both" ]]; then
    echo "Usage: $0 [nvim|helix] [screen|print|both]" >&2
    exit 1
fi

INPUT="${SHEET}-cheatsheet"
SCREEN_OUT="${SHEET}-cheatsheet-screen"
PRINT_OUT="${SHEET}-cheatsheet-print"

if [[ "${VARIANT}" == "screen" || "${VARIANT}" == "both" ]]; then
    echo ">>> Building screen (dark) version for ${SHEET} <<<"
    run_latex "${INPUT}" "${SCREEN_OUT}"
fi

if [[ "${VARIANT}" == "print" || "${VARIANT}" == "both" ]]; then
    echo ">>> Building print (light) version for ${SHEET} <<<"
    # Pass \def\printmode{1} to pdflatex (input string keeps jobname control)
    run_latex '\def\printmode{1}\input{'"${INPUT}"'}' "${PRINT_OUT}"
fi

echo ""
echo "Done. Output in ${DIR}:"
ls -1 "${DIR}/${SHEET}-cheatsheet-"*.pdf 2>/dev/null | sed 's|^|  |' || echo "  (no PDFs found — check for errors above)"
echo "  (other PDFs may exist from previous builds)"
