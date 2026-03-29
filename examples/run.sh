#!/bin/bash
set -e

DIR="$(cd "$(dirname "$0")" && pwd)"

for file in "$DIR"/*.syl; do
  echo "=== Running $file ==="
  dune exec bin/syl.exe -- run "$file"
  echo
done
