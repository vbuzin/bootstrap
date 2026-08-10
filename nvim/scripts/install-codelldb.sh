#!/usr/bin/env bash
# Always-latest CodeLLDB from GitHub releases.
# Keep the full VSIX layout under tools/codelldb/ — do NOT symlink adapter/codelldb
# into tools/bin alone (relative liblldb resolution breaks → DAP exit 101).
set -euo pipefail

NVIM_TOOLS="${NVIM_TOOLS:-${HOME}/.local/share/nvim/tools}"
INSTALL_DIR="${NVIM_TOOLS}/codelldb"
REPO="vadimcn/codelldb"

os="$(uname -s)"
arch="$(uname -m)"
case "${os}-${arch}" in
  Darwin-arm64) asset="codelldb-darwin-arm64.vsix" ;;
  Darwin-x86_64) asset="codelldb-darwin-x64.vsix" ;;
  Linux-aarch64 | Linux-arm64) asset="codelldb-linux-arm64.vsix" ;;
  Linux-x86_64) asset="codelldb-linux-x64.vsix" ;;
  *)
    echo "error: unsupported platform ${os}-${arch}" >&2
    exit 1
    ;;
esac

echo ">>> codelldb: resolving latest release (${asset}) <<<"
tag="$(
  curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" |
    python3 -c "import sys, json; print(json.load(sys.stdin)['tag_name'])"
)"
url="https://github.com/${REPO}/releases/download/${tag}/${asset}"
tmp="$(mktemp -t codelldb.XXXXXX.vsix)"

echo ">>> codelldb: downloading ${tag} <<<"
curl -fsSL -o "${tmp}" "${url}"

echo ">>> codelldb: extracting to ${INSTALL_DIR} <<<"
rm -rf "${INSTALL_DIR}"
mkdir -p "${INSTALL_DIR}"
unzip -qo "${tmp}" -d "${INSTALL_DIR}"
rm -f "${tmp}"

# Drop any old broken symlink from earlier installs
rm -f "${NVIM_TOOLS}/bin/codelldb"

adapter="${INSTALL_DIR}/extension/adapter/codelldb"
if [[ "$(uname -s)" == Darwin ]]; then
  liblldb="${INSTALL_DIR}/extension/lldb/lib/liblldb.dylib"
else
  liblldb="${INSTALL_DIR}/extension/lldb/lib/liblldb.so"
fi

if [[ ! -x "${adapter}" ]]; then
  echo "error: codelldb adapter missing at ${adapter}" >&2
  exit 1
fi
if [[ ! -e "${liblldb}" ]]; then
  echo "error: liblldb missing at ${liblldb}" >&2
  exit 1
fi
chmod +x "${adapter}"

# Smoke: must start without panic (uses --liblldb like nvim-dap / rustaceanvim)
port=$((30000 + RANDOM % 10000))
"${adapter}" --liblldb "${liblldb}" --port "${port}" >/dev/null 2>&1 &
pid=$!
sleep 0.5
if ! kill -0 "${pid}" 2>/dev/null; then
  echo "error: codelldb failed smoke start" >&2
  exit 1
fi
kill -9 "${pid}" 2>/dev/null || true

echo ">>> codelldb: ready (${adapter}, ${tag}) <<<"
echo ">>> codelldb: liblldb ${liblldb} <<<"