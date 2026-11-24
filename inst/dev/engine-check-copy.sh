#!/usr/bin/env bash
################################################################################
#
# Check 1a in the shell, without R: each file listed in inst/inlavanilla.lock
# exists and has the listed MD5 of its text with LF line endings (that is,
# tr -d '\r' < file | md5sum), and every R/vanilla-*.R file is listed. Run it
# from the package root, or pass the root as the first argument. The exit
# status is 1 when the check fails.
#
################################################################################

set -u
cd "${1:-.}" || exit 2
lock=inst/inlavanilla.lock
status=0

fail() {
  if [ -n "${GITHUB_ACTIONS:-}" ]; then
    echo "::error::$1"
  else
    echo "FAIL: $1"
  fi
  status=1
}

if command -v md5sum > /dev/null 2>&1; then
  md5_lf() { tr -d '\r' < "$1" | md5sum | cut -d ' ' -f 1; }
else
  md5_lf() { tr -d '\r' < "$1" | md5 -q; }
fi

if [ ! -f "$lock" ]; then
  fail "there is no lock file at $lock"
  exit 1
fi

# One line "file md5" per file record of the lock
records=$(tr -d '\r' < "$lock" | awk -F ': *' '
  /^File:/ { file = $2 }
  /^MD5:/ { md5 = $2 }
  /^[[:space:]]*$/ { if (file != "") print file, md5; file = ""; md5 = "" }
  END { if (file != "") print file, md5 }
')

n=0
while read -r file md5; do
  [ -n "$file" ] || continue
  n=$((n + 1))
  if [ ! -f "$file" ]; then
    fail "$file is in the lock but missing"
    continue
  fi
  got=$(md5_lf "$file")
  if [ "$got" != "$md5" ]; then
    fail "$file differs from the lock (MD5 $got, lock $md5)"
  fi
done << EOF
$records
EOF

for file in R/vanilla-*.R; do
  [ -e "$file" ] || continue
  if ! printf '%s\n' "$records" |
    awk -v f="$file" '$1 == f { found = 1 } END { exit !found }'; then
    fail "$file is not in the lock"
  fi
done

if [ "$status" -eq 0 ]; then
  echo "ok: the $n file(s) in $lock match their copies, and no copy is unlisted"
fi
exit "$status"
