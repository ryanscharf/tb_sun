#!/bin/bash
if [ -f /tmp/docker.env ]; then
  export $(cat /tmp/docker.env | xargs)
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

Rscript /home/r-environment/playoff_runner.R
EXIT_CODE=$?

[ $EXIT_CODE -eq 0 ] && echo "Simulation complete." || echo "ERROR: exited $EXIT_CODE"
echo "==========================================="
exit $EXIT_CODE
