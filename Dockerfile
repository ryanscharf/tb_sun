# Base R image
FROM rocker/r-base

ENV TZ=America/New_York

# Install system dependencies
RUN apt-get update && apt-get install -y \
    cron \
    git \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install pak using the simpler method
RUN R -e "install.packages('pak', repos='https://cloud.r-project.org/')"

# Use pak to install R dependencies for Tampa Bay Sun roster scraper
RUN R -e "pak::pkg_install(c('rvest', 'dplyr', 'tibble', 'stringr', 'lubridate', 'DBI', 'RPostgres', 'blastula'))"

WORKDIR /home/r-environment

# Copy script files
COPY docker-entrypoint.sh /docker-entrypoint.sh
COPY run_scraper.sh /home/r-environment/run_scraper.sh
COPY startup.sh /startup.sh

# Make scripts executable
RUN chmod +x /docker-entrypoint.sh && \
    chmod +x /home/r-environment/run_scraper.sh && \
    chmod +x /startup.sh

# Create cron job to run at 3 AM Eastern Time daily
RUN echo "0 * * * * /home/r-environment/run_scraper.sh >> /var/log/cron.log 2>&1" > /etc/cron.d/scraper-cron && \
    chmod 0644 /etc/cron.d/scraper-cron && \
    crontab /etc/cron.d/scraper-cron

# Create log file
RUN touch /var/log/cron.log

# Run entrypoint, start cron, and tail logs
# CMD ["sh", "-c", "/docker-entrypoint.sh && cron && tail -f /var/log/cron.log"]
CMD ["/startup.sh"]