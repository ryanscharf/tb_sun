# Playoff Modeling: Docker Runner + Postgres Setup Guide

## Architecture Overview

One new Docker container runs alongside the existing roster scraper:

- **`playoff-runner`** — Runs the Monte Carlo simulation on a schedule, writes results to Postgres at `192.168.2.66`

The Shiny app (shinyapps.io or Posit Connect Cloud) connects via your Cloudflare tunnel at `pg-tn.ryanscharf.com`.

---

## Step 1: Create Postgres Tables

Connect to `192.168.2.66` and run:

```bash
psql -h 192.168.2.66 -U tb_sun_writer -d tb_sun
```

```sql
CREATE TABLE simulation_runs (
  run_id          SERIAL PRIMARY KEY,
  run_at          TIMESTAMPTZ DEFAULT NOW(),
  n_sims          INTEGER,
  games_played    INTEGER,
  games_remaining INTEGER
);

CREATE TABLE playoff_odds (
  run_id            INTEGER REFERENCES simulation_runs(run_id) ON DELETE CASCADE,
  team_id           VARCHAR(20) NOT NULL,
  team_name         VARCHAR(100),
  team_abbreviation VARCHAR(10),
  playoff_pct       NUMERIC(5,2),
  avg_pts           NUMERIC(6,2),
  current_points    INTEGER,
  games_played      INTEGER,
  PRIMARY KEY (run_id, team_id)
);

CREATE TABLE match_probabilities (
  run_id          INTEGER REFERENCES simulation_runs(run_id) ON DELETE CASCADE,
  match_id        INTEGER NOT NULL,
  home_team_abbr  VARCHAR(10),
  away_team_abbr  VARCHAR(10),
  match_date      DATE,
  home_xg         NUMERIC(5,3),
  away_xg         NUMERIC(5,3),
  home_win_pct    NUMERIC(5,2),
  draw_pct        NUMERIC(5,2),
  away_win_pct    NUMERIC(5,2),
  avg_home_goals  NUMERIC(5,3),
  avg_away_goals  NUMERIC(5,3),
  PRIMARY KEY (run_id, match_id)
);

GRANT ALL ON simulation_runs, playoff_odds, match_probabilities TO tb_sun_writer;
GRANT USAGE, SELECT ON SEQUENCE simulation_runs_run_id_seq TO tb_sun_writer;
```

---

## Step 2: Create `playoff_runner.R`

Save as `playoff_runner.R` in the repo root. Paste the full functions block from `playoff_modeling.R` (lines 12–686 — all function definitions and the `team_name_mapping` tribble) into the marked section.

```r
library(tidyverse)
library(itscalledsoccer)
library(mirai)
library(data.table)
library(dtplyr)
library(httr2)
library(jsonlite)
library(DBI)
library(RPostgres)

# ── DB connection ──────────────────────────────────────────────────────────────
get_db_conn <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("DB_HOST"),
    port     = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname   = Sys.getenv("DB_NAME"),
    user     = Sys.getenv("DB_USERNAME"),
    password = Sys.getenv("DB_PASSWORD")
  )
}

# ── PASTE FUNCTIONS FROM playoff_modeling.R HERE ──────────────────────────────
#    get_schedule()
#    team_name_mapping tribble
#    calculate_team_strengths()
#    simulate_matches_vectorized()
#    simulate_season_vectorized()
#    calculate_playoff_odds_fast()
#    get_match_probabilities()

# ── Config ─────────────────────────────────────────────────────────────────────
N_SIMS  <- as.integer(Sys.getenv("N_SIMS",  "100000"))
N_CORES <- as.integer(Sys.getenv("N_CORES", "4"))

message(sprintf("[%s] Starting playoff simulation (%s sims, %d cores)",
                Sys.time(), format(N_SIMS, big.mark = ","), N_CORES))

asa_client <- AmericanSoccerAnalysis$new()
teams      <- asa_client$get_teams(leagues = 'usls')
schedule   <- get_schedule()

output       <- calculate_playoff_odds_fast(schedule, n_sims = N_SIMS, n_cores = N_CORES)
playoff_odds <- output$summary

# Rebuild remaining_games + team_strengths for match probabilities
asa_games <- asa_client$get_games(leagues = 'usls', season = '2025-26') %>%
  mutate(date_only = as.Date(date_time_utc))

schedule_mapped <- schedule %>%
  left_join(team_name_mapping, by = c("home_team" = "fotmob_name")) %>%
  rename(home_team_id = team_id) %>%
  mutate(home_team = schedule_name) %>%
  select(-team_abbreviation, -schedule_name) %>%
  left_join(team_name_mapping, by = c("away_team" = "fotmob_name")) %>%
  rename(away_team_id = team_id) %>%
  mutate(away_team = schedule_name) %>%
  select(-team_abbreviation) %>%
  mutate(date = as.Date(date_utc))

played_games <- asa_games %>%
  filter(status == "FullTime") %>%
  select(home_team_id, away_team_id,
         home_goals = home_score, away_goals = away_score,
         date = date_only)

remaining_games <- schedule_mapped %>%
  anti_join(played_games, by = c("home_team_id", "away_team_id", "date")) %>%
  mutate(match_id = row_number())

all_team_ids <- unique(c(schedule_mapped$home_team_id, schedule_mapped$away_team_id))

team_strengths_complete <- tibble(team = all_team_ids) %>%
  left_join(calculate_team_strengths(played_games), by = "team") %>%
  mutate(
    l_avg = mean(attack_strength, na.rm = TRUE),
    attack_strength  = if_else(is.na(attack_strength),  l_avg, attack_strength),
    defense_strength = if_else(is.na(defense_strength), l_avg, defense_strength)
  ) %>%
  select(-l_avg)

match_probs <- get_match_probabilities(
  remaining_games,
  team_strengths_complete,
  teams,
  n_sims = min(N_SIMS, 50000)
) %>%
  left_join(
    remaining_games %>% select(match_id, match_date = date),
    by = "match_id"
  )

# ── Write to Postgres ──────────────────────────────────────────────────────────
message(sprintf("[%s] Writing results to Postgres...", Sys.time()))

con <- get_db_conn()
on.exit(dbDisconnect(con), add = TRUE)

run_id <- dbGetQuery(con, sprintf(
  "INSERT INTO simulation_runs (n_sims, games_played, games_remaining)
   VALUES (%d, %d, %d) RETURNING run_id",
  N_SIMS, nrow(played_games), nrow(remaining_games)
))$run_id

odds_rows <- playoff_odds %>%
  select(team_id = team, team_name, team_abbreviation,
         playoff_pct, avg_pts, current_points, games_played) %>%
  mutate(run_id = run_id)

dbWriteTable(con, "playoff_odds", odds_rows, append = TRUE, row.names = FALSE)

if (nrow(match_probs) > 0) {
  prob_rows <- match_probs %>%
    mutate(run_id = run_id) %>%
    select(run_id, match_id,
           home_team_abbr = home_team, away_team_abbr = away_team,
           match_date, home_xg, away_xg, home_win_pct, draw_pct, away_win_pct,
           avg_home_goals, avg_away_goals)

  dbWriteTable(con, "match_probabilities", prob_rows, append = TRUE, row.names = FALSE)
}

message(sprintf("[%s] Done. run_id = %d", Sys.time(), run_id))
```

---

## Step 3: Create `Dockerfile.runner`

```dockerfile
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
```

---

## Step 4: Create `run_playoff_simulation.sh`

```bash
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
rm -rf temp_repo

Rscript /home/r-environment/playoff_runner.R
EXIT_CODE=$?

[ $EXIT_CODE -eq 0 ] && echo "Simulation complete." || echo "ERROR: exited $EXIT_CODE"
echo "==========================================="
exit $EXIT_CODE
```

---

## Step 5: Create `startup_playoff.sh`

```bash
#!/bin/bash
/docker-entrypoint.sh

if [ "${RUN_ON_STARTUP}" = "true" ] || [ "${RUN_ON_STARTUP}" = "TRUE" ]; then
  echo "RUN_ON_STARTUP enabled — running simulation now..."
  /home/r-environment/run_playoff_simulation.sh
fi

cron
tail -f /var/log/playoff_cron.log
```

---

## Step 6: Update `docker-compose.yml`

Add the new service. The `playoff-runner` reuses the same DB credentials already in `.env`.

```yaml
services:

  tb_sun:
    image: ghcr.io/ryanscharf/tb_sun:latest
    container_name: tb_sun
    restart: unless-stopped
    environment:
      - TZ=America/New_York
      - DB_HOST=${DB_HOST}
      - DB_PORT=${DB_PORT}
      - DB_USERNAME=${DB_USERNAME}
      - DB_PASSWORD=${DB_PASSWORD}
      - DB_NAME=${DB_NAME}
      - DB_MIN_SIZE=${DB_MIN_SIZE}
      - DB_IDLE_TIMEOUT=${DB_IDLE_TIMEOUT}
      - EMAIL_FROM=${EMAIL_FROM}
      - EMAIL_TO=${EMAIL_TO}
      - EMAIL_SMTP_HOST=${EMAIL_SMTP_HOST}
      - EMAIL_SMTP_PORT=${EMAIL_SMTP_PORT}
      - EMAIL_USERNAME=${EMAIL_USERNAME}
      - EMAIL_PASSWORD=${EMAIL_PASSWORD}
      - EMAIL_USE_SSL=${EMAIL_USE_SSL}
      - EMAIL_SEND_SUCCESS=${EMAIL_SEND_SUCCESS}
      - RUN_ON_STARTUP=${RUN_ON_STARTUP:-false}
    networks:
      - tb_sun-network

  playoff-runner:
    build:
      context: .
      dockerfile: Dockerfile.runner
    container_name: playoff-runner
    restart: unless-stopped
    environment:
      - TZ=America/New_York
      - DB_HOST=${DB_HOST}
      - DB_PORT=${DB_PORT}
      - DB_USERNAME=${DB_USERNAME}
      - DB_PASSWORD=${DB_PASSWORD}
      - DB_NAME=${DB_NAME}
      - N_SIMS=${N_SIMS:-100000}
      - N_CORES=${N_CORES:-4}
      - RUN_ON_STARTUP=${PLAYOFF_RUN_ON_STARTUP:-false}
    networks:
      - tb_sun-network

networks:
  tb_sun-network:
    driver: bridge
```

Add to `.env`:

```bash
N_SIMS=100000
N_CORES=4
PLAYOFF_RUN_ON_STARTUP=true   # flip to false after first successful run
```

---

## Step 7: Build and Deploy on TrueNAS

```bash
docker compose build playoff-runner
docker compose up -d playoff-runner

# Watch the first run
docker logs -f playoff-runner

# Verify data landed
psql -h 192.168.2.66 -U tb_sun_writer -d tb_sun \
  -c "SELECT run_id, run_at, games_played, games_remaining FROM simulation_runs;"
```

---

## Step 8: Connect Your Shiny App

In your Shiny app, read from the DB using the same credentials. Set these as environment variables in your shinyapps.io or Posit Connect dashboard:

```r
library(DBI)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(),
  host     = "pg-tn.ryanscharf.com",
  port     = 5432,
  dbname   = Sys.getenv("DB_NAME"),       # tb_sun
  user     = Sys.getenv("DB_USERNAME"),
  password = Sys.getenv("DB_PASSWORD")
)

# Latest playoff odds
playoff_odds <- dbGetQuery(con,
  "SELECT p.*
   FROM playoff_odds p
   WHERE run_id = (SELECT MAX(run_id) FROM simulation_runs)
   ORDER BY playoff_pct DESC"
)

# Latest match probabilities
match_probs <- dbGetQuery(con,
  "SELECT m.*
   FROM match_probabilities m
   WHERE run_id = (SELECT MAX(run_id) FROM simulation_runs)
   ORDER BY match_date, match_id"
)
```

---

## Summary of Files to Create/Change

| File | Action |
|------|--------|
| `playoff_runner.R` | Create — simulation script that writes to Postgres |
| `Dockerfile.runner` | Create — Docker image for the runner |
| `run_playoff_simulation.sh` | Create — shell script invoked by cron |
| `startup_playoff.sh` | Create — container entrypoint |
| `docker-compose.yml` | Update — add `playoff-runner` service |
| `.env` | Update — add `N_SIMS`, `N_CORES`, `PLAYOFF_RUN_ON_STARTUP` |
