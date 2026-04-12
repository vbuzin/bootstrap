#!/usr/bin/env bash
# Compile nvim-cheatsheet.tex to PDF using Docker + TeX Live.
#
# Usage:
#   ./compile.sh           — builds both screen (dark) and print (light) PDFs
#   ./compile.sh screen    — dark-background PDF only
#   ./compile.sh print     — white-background PDF only
#
# Outputs (in the same directory as this script):
#   nvim-cheatsheet-screen.pdf   — One Dark background, for screen viewing
#   nvim-cheatsheet-print.pdf    — white background, ready to print
#
# Requirements: Docker (image pulled automatically on first run, ~5 GB)

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VARIANT="${1:-both}"
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
    echo "Usage: $0 [screen|print|both]" >&2
    exit 1
fi

if [[ "${VARIANT}" == "screen" || "${VARIANT}" == "both" ]]; then
    echo ">>> Building screen (dark) version <<<"
    run_latex "nvim-cheatsheet" "nvim-cheatsheet-screen"
fi

if [[ "${VARIANT}" == "print" || "${VARIANT}" == "both" ]]; then
    echo ">>> Building print (light) version <<<"
    # Pass \def\printmode{1} to pdflatex via \usepretex (piped through the
    # input string so the jobname flag can still control the output filename)
    run_latex '\def\printmode{1}\input{nvim-cheatsheet}' "nvim-cheatsheet-print"
fi

echo ""
echo "Done. Output in ${DIR}:"
ls -1 "${DIR}"/*.pdf 2>/dev/null | sed 's|^|  |' || echo "  (no PDFs found — check for errors above)"
