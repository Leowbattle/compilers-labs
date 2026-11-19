#!/usr/bin/env bash
# Regression checks for whole-input parsing.
#
# By default this exercises the checked-in .mt files and the new rejection
# behaviour. Set CORPUS_DIR to the generated benchmark directory to also
# compile its 41 programs:
#
#   CORPUS_DIR=/path/to/perf-compare/bench/programs tests/whole_input_test.sh
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
lean_dir="$root/lean"
export PATH="${HOME}/.elan/bin:${PATH}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

fail() {
  echo "FAIL: $*" >&2
  exit 1
}

ok() {
  echo "ok - $*"
}

expect_ok() {
  local name="$1"
  shift
  if "$@" >"$tmp/stdout" 2>"$tmp/stderr"; then
    ok "$name"
  else
    echo "--- stdout" >&2
    cat "$tmp/stdout" >&2
    echo "--- stderr" >&2
    cat "$tmp/stderr" >&2
    fail "$name"
  fi
}

expect_fail() {
  local name="$1"
  shift
  if "$@" >"$tmp/stdout" 2>"$tmp/stderr"; then
    echo "--- stdout" >&2
    cat "$tmp/stdout" >&2
    fail "$name (unexpected success)"
  else
    ok "$name"
  fi
}

cd "$lean_dir"
lake build >/dev/null

mtc="$lean_dir/.lake/build/bin/mtc"
[[ -x "$mtc" ]] || fail "missing executable: $mtc"

# Valid .mt sources from the repository and the cw2 tests must still compile.
for src in "$root"/tests/*.mt "$root"/tests/cw2_test/*.mt; do
  base="$(basename "$src" .mt)"
  cp "$src" "$tmp/$base.mt"
  expect_ok "compile $base.mt" "$mtc" "$tmp/$base.mt"
  [[ -f "$tmp/$base.tam" ]] || fail "$base.tam was not generated"
done

# The generated TAM must still parse and run.
expect_ok "run generated program.tam" "$mtc" "$tmp/program.tam"
grep -q "Final stack: \[3,7\]" "$tmp/stdout" ||
  fail "unexpected interpreter output for program.tam"

# Trailing whitespace is still accepted: it is consumed by the token parsers.
cp "$root/tests/program.mt" "$tmp/whitespace.mt"
printf '\n \t\n' >>"$tmp/whitespace.mt"
expect_ok "accept trailing whitespace after .mt" "$mtc" "$tmp/whitespace.mt"

# Trailing non-whitespace input is a parse error for both languages.
cp "$root/tests/program.mt" "$tmp/trailing_garbage.mt"
printf '\n%%%%%% trailing garbage %%%%%%\n' >>"$tmp/trailing_garbage.mt"
expect_fail "reject trailing garbage after .mt" "$mtc" "$tmp/trailing_garbage.mt"

cp "$tmp/program.tam" "$tmp/trailing_garbage.tam"
printf '\n%%%%%% trailing garbage %%%%%%\n' >>"$tmp/trailing_garbage.tam"
expect_fail "reject trailing garbage after .tam" "$mtc" "$tmp/trailing_garbage.tam"

# Optionally compile the generated benchmark corpus. It is kept outside this
# repository, so opt in with CORPUS_DIR.
corpus="${CORPUS_DIR:-}"
if [[ -n "$corpus" ]]; then
  [[ -d "$corpus" ]] || fail "CORPUS_DIR is not a directory: $corpus"
  mkdir -p "$tmp/corpus"
  cp "$corpus"/*.mt "$tmp/corpus/"
  count=0
  for src in "$tmp/corpus"/*.mt; do
    base="$(basename "$src" .mt)"
    expect_ok "compile corpus/$base.mt" "$mtc" "$src"
    count=$((count + 1))
  done
  ok "generated benchmark corpus: $count programs"
else
  echo "skip - generated benchmark corpus (set CORPUS_DIR=... to enable)"
fi

echo "all whole-input parsing checks passed"
