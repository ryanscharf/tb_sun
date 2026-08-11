#!/bin/bash
# `export $(cat file | xargs)` word-splits the whole file on ALL whitespace,
# not just newlines -- any value containing a space (e.g. USL_SEASON=2026
# fall, or USL_SEASON='2026 fall' if Dockge passes the quotes through
# literally rather than stripping them itself) gets silently truncated or
# mangled instead of exported whole. Reading line-by-line, splitting only on
# the first `=`, and stripping one layer of surrounding matching quotes from
# the value (if present) handles both a quoted and unquoted value in
# /tmp/docker.env correctly either way.
if [ -f /tmp/docker.env ]; then
  while IFS= read -r line; do
    [ -z "$line" ] && continue
    key="${line%%=*}"
    value="${line#*=}"
    if [[ "$value" == \'*\' ]] || [[ "$value" == \"*\" ]]; then
      value="${value:1:-1}"
    fi
    export "$key=$value"
  done < /tmp/docker.env
fi

cd /home/r-environment
echo "==========================================="
echo "Playoff Simulation - $(date)"
echo "==========================================="

rm -rf temp_repo
git clone https://github.com/ryanscharf/tb_sun.git temp_repo || { echo "ERROR: git clone failed"; exit 1; }
cp temp_repo/playoff_runner.R .
cp temp_repo/playoff_modeling.R .
cp temp_repo/functions.R .
rm -rf temp_repo

/usr/local/bin/Rscript /home/r-environment/playoff_runner.R
EXIT_CODE=$?

[ $EXIT_CODE -eq 0 ] && echo "Simulation complete." || echo "ERROR: exited $EXIT_CODE"
echo "==========================================="
exit $EXIT_CODE
