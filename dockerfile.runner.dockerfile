FROM rocker/r-base

ENV TZ=America/New_York

RUN apt-get update -qq && apt-get install -y \
    cron \
    git \
    libpq-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('pak', repos='https://cloud.r-project.org/')"
RUN R -e "pak::pkg_install(c('tidyverse','itscalledsoccer','mirai','data.table','dtplyr','httr2','jsonlite','DBI','RPostgres'))"

WORKDIR /home/r-environment

COPY docker-entrypoint.sh /docker-entrypoint.sh
COPY run_playoff_simulation.sh /home/r-environment/run_playoff_simulation.sh
COPY startup_playoff.sh /startup_playoff.sh

RUN chmod +x /docker-entrypoint.sh \
             /home/r-environment/run_playoff_simulation.sh \
             /startup_playoff.sh

# Run Monday and Thursday at 3 AM Eastern (TZ set above)
RUN echo "0 3 * * 1,4 /home/r-environment/run_playoff_simulation.sh >> /var/log/playoff_cron.log 2>&1" \
    > /etc/cron.d/playoff-cron && \
    chmod 0644 /etc/cron.d/playoff-cron && \
    crontab /etc/cron.d/playoff-cron

RUN touch /var/log/playoff_cron.log

CMD ["/startup_playoff.sh"]