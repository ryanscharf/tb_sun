#!/bin/bash

# Load environment variables
if [ -f /tmp/docker.env ]; then
  export $(cat /tmp/docker.env | xargs)
fi

cd /home/r-environment

echo "==========================================="
echo "Tampa Bay Sun FC Roster Scraper - $(date)"
echo "==========================================="

# Pull latest code from GitHub
echo "Pulling latest code from GitHub..."
rm -rf temp_repo
git clone https://github.com/ryanscharf/tb_sun.git temp_repo

if [ $? -ne 0 ]; then
  echo "ERROR: Failed to clone repository"
  exit 1
fi

# Copy the scraper script
cp temp_repo/tampa_bay_sun_roster_scraper.R .

if [ $? -ne 0 ]; then
  echo "ERROR: Failed to copy scraper script"
  exit 1
fi

# Clean up temp directory
rm -rf temp_repo

echo "Running Tampa Bay Sun roster scraper..."

# Run the scraper
Rscript /home/r-environment/tampa_bay_sun_roster_scraper.R
SCRIPT_EXIT_CODE=$?

if [ $SCRIPT_EXIT_CODE -eq 0 ]; then
  echo "Scraper completed successfully"
else
  echo "ERROR: Scraper exited with code $SCRIPT_EXIT_CODE"
fi

echo "==========================================="
echo ""

exit $SCRIPT_EXIT_CODE
