#!/usr/bin/env bash
# Install SML/NJ for CI (amd64 Linux). Matches developer 110.99.x; apt smlnj is too old for sv0vm/sv0c.
# If smlnj.org and its mirror are unreachable, falls back to the NixOS-built SML/NJ 2026.3
# (developers run 2026.x) from cache.nixos.org, pinned by store path.
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

# Hydra build of nixpkgs smlnj for x86_64-linux; cache.nixos.org signs it.
NIX_SMLNJ="/nix/store/w681j2v1xplprr505biwy6qwz0p8g1ra-smlnj-2026.3"
NIX_INSTALLER="https://releases.nixos.org/nix/nix-2.28.4/install"

install_from_nix() {
  echo "ci-install-smlnj: smlnj.org unavailable; installing SML/NJ 2026.3 from cache.nixos.org" >&2
  cd "$HOME"
  rm -rf "$ROOT" # a partial tree would be saved under the 110.99.9 cache key and block the real install
  if ! command -v nix-store >/dev/null; then
    sh <("${CURL_LONG[@]}" "$NIX_INSTALLER") --no-daemon --yes
  fi
  if [[ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
    set +u # nix.sh reads unset variables
    # shellcheck disable=SC1091
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
    set -u
  fi
  nix-store --realise "$NIX_SMLNJ" >/dev/null
  export PATH="$NIX_SMLNJ/bin:$PATH"
  if [[ -n "${GITHUB_PATH:-}" ]]; then
    echo "$NIX_SMLNJ/bin" >>"$GITHUB_PATH"
  fi
  sml '@SMLversion'
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
if ! { download_config_tgz config.tgz && tar -xzf config.tgz && ./config/install.sh; }; then
  install_from_nix
  exit 0
fi

append_path
if [[ ! -x "$ROOT/bin/sml" ]]; then
  echo "ci-install-smlnj: sml not found under $ROOT/bin" >&2
  find "$ROOT" -maxdepth 4 -name sml -type f 2>/dev/null || true
  exit 1
fi
sml '@SMLversion'
