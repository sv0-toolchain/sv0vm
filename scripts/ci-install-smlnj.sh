#!/usr/bin/env bash
# Install SML/NJ for CI (amd64 Linux). Matches developer 110.99.x; apt smlnj is too old for sv0vm/sv0c.
set -euo pipefail

SMLNJ_VERSION="${SMLNJ_VERSION:-110.99.9}"
ROOT="${SMLNJ_INSTALL_DIR:-$HOME/smlnj-${SMLNJ_VERSION}}"

append_path() {
  if [[ -d "$ROOT/bin" ]]; then
    export PATH="$ROOT/bin:$PATH"
    if [[ -n "${GITHUB_PATH:-}" ]]; then
      echo "$ROOT/bin" >>"$GITHUB_PATH"
    fi
  fi
}

append_path
if [[ -x "$ROOT/bin/sml" ]]; then
  sml '@SMLversion'
  exit 0
fi

sudo apt-get update
sudo apt-get install -y build-essential curl

mkdir -p "$ROOT"
cd "$ROOT"
curl -fsSL -o config.tgz "https://smlnj.org/dist/working/${SMLNJ_VERSION}/config.tgz"
tar -xzf config.tgz
./config/install.sh

append_path
if [[ ! -x "$ROOT/bin/sml" ]]; then
  echo "ci-install-smlnj: sml not found under $ROOT/bin" >&2
  find "$ROOT" -maxdepth 3 -name sml -type f 2>/dev/null || true
  exit 1
fi
sml '@SMLversion'
