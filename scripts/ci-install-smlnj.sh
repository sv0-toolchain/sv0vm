#!/usr/bin/env bash
# Install SML/NJ for CI (amd64 Linux). Matches developer 110.99.x; apt smlnj is too old for sv0vm/sv0c.
set -euo pipefail

SMLNJ_VERSION="${SMLNJ_VERSION:-110.99.9}"
ROOT="${SMLNJ_INSTALL_DIR:-$HOME/smlnj-${SMLNJ_VERSION}}"

# curl exit 28 = timeout; dist downloads can be slow. Used for config.tgz and inherited by install.sh child curls.
CURL_LONG=(curl -fsSL --connect-timeout 120 --max-time 1800 --retry 5 --retry-delay 20 --retry-all-errors)

append_path() {
  if [[ -d "$ROOT/bin" ]]; then
    export PATH="$ROOT/bin:$PATH"
    if [[ -n "${GITHUB_PATH:-}" ]]; then
      echo "$ROOT/bin" >>"$GITHUB_PATH"
    fi
  fi
}

write_fetch_config_for_installer() {
  # SML/NJ config/install.sh shells out to curl/wget; these defaults apply to child downloads.
  if [[ -n "${GITHUB_ACTIONS:-}" ]]; then
    cat >"$HOME/.curlrc" <<'EOF'
connect-timeout = 120
max-time = 1800
retry = 5
retry-delay = 20
retry-all-errors
EOF
    cat >"$HOME/.wgetrc" <<'EOF'
timeout = 120
tries = 5
waitretry = 20
EOF
  fi
}

download_config_tgz() {
  local dest="$1"
  local primary="https://smlnj.org/dist/working/${SMLNJ_VERSION}/config.tgz"
  local mirror="https://smlnj.cs.uchicago.edu/dist/working/${SMLNJ_VERSION}/config.tgz"
  if "${CURL_LONG[@]}" -o "$dest" "$primary"; then
    return 0
  fi
  echo "ci-install-smlnj: primary config.tgz failed, trying mirror..." >&2
  "${CURL_LONG[@]}" -o "$dest" "$mirror"
}

append_path
if [[ -x "$ROOT/bin/sml" ]]; then
  sml '@SMLversion'
  exit 0
fi

sudo apt-get update
sudo apt-get install -y build-essential curl wget

write_fetch_config_for_installer

mkdir -p "$ROOT"
cd "$ROOT"
rm -f config.tgz
download_config_tgz config.tgz
tar -xzf config.tgz
./config/install.sh

append_path
if [[ ! -x "$ROOT/bin/sml" ]]; then
  echo "ci-install-smlnj: sml not found under $ROOT/bin" >&2
  find "$ROOT" -maxdepth 4 -name sml -type f 2>/dev/null || true
  exit 1
fi
sml '@SMLversion'
