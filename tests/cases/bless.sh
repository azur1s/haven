#!/usr/bin/env bash
# Regenerate the `.out` golden files for every run/ fixture by compiling and
# running it with the current compiler. Use this after an intentional change to
# program output; review the diff before committing. Fixtures with no output
# get no `.out` file (exit code is still checked by the harness).
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"

echo "building noirc..."
cargo build --manifest-path "$root/Cargo.toml"
bin="$root/target/debug/noirc"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

for f in "$here/run"/*.nr; do
    n="$(basename "$f" .nr)"
    if ! "$bin" "$f" -o "$tmp/$n" >"$tmp/$n.log" 2>&1; then
        echo "SKIP $n (does not compile):"
        sed 's/\x1b\[[0-9;]*m//g' "$tmp/$n.log" | head -3
        continue
    fi
    "$tmp/$n" > "$tmp/$n.out" 2>/dev/null || true
    if [ -s "$tmp/$n.out" ]; then
        cp "$tmp/$n.out" "$here/run/$n.out"
        echo "blessed $n.out"
    else
        rm -f "$here/run/$n.out"
        echo "no output: $n (removed stale .out if any)"
    fi
done
