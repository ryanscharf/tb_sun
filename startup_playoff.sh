#!/bin/bash
/docker-entrypoint.sh

if [ "${RUN_ON_STARTUP}" = "true" ] || [ "${RUN_ON_STARTUP}" = "TRUE" ]; then
  echo "RUN_ON_STARTUP enabled — running simulation now..."
  /home/r-environment/run_playoff_simulation.sh
fi

cron
tail -f /var/log/playoff_cron.log
