#!/usr/bin/env bash
# Always-latest debugpy in a dedicated venv under nvim's data dir.
set -euo pipefail

NVIM_TOOLS="${NVIM_TOOLS:-${HOME}/.local/share/nvim/tools}"
VENV="${NVIM_TOOLS}/debugpy"

# Prefer Homebrew python3 when present (real builds), else PATH python3.
if [[ -x /opt/homebrew/bin/python3 ]]; then
  PYTHON=/opt/homebrew/bin/python3
elif command -v python3 >/dev/null 2>&1; then
  PYTHON="$(command -v python3)"
else
  echo "error: python3 not found (install via nvim/Brewfile)" >&2
  exit 1
fi

echo ">>> debugpy: venv at ${VENV} (python=${PYTHON}) <<<"
mkdir -p "${NVIM_TOOLS}"
if [[ ! -d "${VENV}" ]]; then
  "${PYTHON}" -m venv "${VENV}"
fi

# shellcheck disable=SC1091
source "${VENV}/bin/activate"
python -m pip install -U pip
python -m pip install -U debugpy
python -c "import debugpy; print(f'debugpy {debugpy.__version__}')"
echo ">>> debugpy: ready (${VENV}/bin/python) <<<"
