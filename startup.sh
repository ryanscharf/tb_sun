#!/bin/bash

# Export environment variables
/docker-entrypoint.sh

# Check if scraper should run on startup
if [ "${RUN_ON_STARTUP}" = "true" ] || [ "${RUN_ON_STARTUP}" = "TRUE" ]; then
  echo "RUN_ON_STARTUP is enabled - running scraper on container startup..."
  /home/r-environment/run_scraper.sh
else
  echo "RUN_ON_STARTUP is disabled - skipping startup run (will run on cron schedule)"
fi

# Start cron for scheduled runs
cron

# Keep container running by tailing logs
tail -f /var/log/cron.log
