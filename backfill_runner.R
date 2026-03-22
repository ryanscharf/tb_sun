library(tidyverse)
library(itscalledsoccer)
library(mirai)
library(data.table)
library(dtplyr)
library(httr2)
library(jsonlite)
library(DBI)
library(RPostgres)

source("functions.R")

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

N_SIMS  <- as.integer(Sys.getenv("N_SIMS",  "100000"))
N_CORES <- as.integer(Sys.getenv("N_CORES", "4"))

# ── Fetch all data once ────────────────────────────────────────────────────────
message("Fetching data from ASA and FotMob...")

init_con <- get_db_conn()
on.exit(try(dbDisconnect(init_con), silent = TRUE), add = TRUE)

asa_client <- AmericanSoccerAnalysis$new()
teams      <- asa_client$get_teams(leagues = 'usls')
schedule   <- get_schedule(con = init_con)

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

all_played_games <- asa_games %>%
  filter(status == "FullTime") %>%
  select(
    home_team_id, away_team_id,
    home_goals = home_score, away_goals = away_score,
    date = date_only
  )

all_team_ids <- unique(c(schedule_mapped$home_team_id, schedule_mapped$away_team_id))

# ── Define gameweek cutoffs ────────────────────────────────────────────────────
# Group match dates into gameweeks by ISO week
gameweeks_df <- all_played_games %>%
  mutate(iso_week = paste(lubridate::isoyear(date), lubridate::isoweek(date), sep = "-W")) %>%
  group_by(iso_week) %>%
  summarize(start_date = min(date), end_date = max(date), .groups = "drop") %>%
  arrange(start_date) %>%
  mutate(gameweek_number = row_number())

message(sprintf("Found %d gameweeks to backfill.", nrow(gameweeks_df)))

# ── Insert gameweeks into DB ───────────────────────────────────────────────────
dbWriteTable(init_con, "gameweeks",
  gameweeks_df %>% select(gameweek_number, start_date, end_date),
  append = TRUE, row.names = FALSE
)
# Fetch back with DB-assigned gameweek_ids
gameweeks_with_ids <- dbGetQuery(init_con, "SELECT * FROM gameweeks ORDER BY gameweek_number")
dbDisconnect(init_con)

# ── Loop through each gameweek ─────────────────────────────────────────────────
for (gw_row in seq_len(nrow(gameweeks_with_ids))) {
  gw            <- gameweeks_with_ids[gw_row, ]
  cutoff_date   <- as.Date(gw$end_date)
  gameweek_id   <- gw$gameweek_id
  gw_number     <- gw$gameweek_number

  message(sprintf("\n── Gameweek %d (%s to %s) ──────────────────────────────",
                  gw_number, gw$start_date, cutoff_date))

  played_games <- all_played_games %>% filter(date <= cutoff_date)
  remaining_games <- schedule_mapped %>%
    anti_join(played_games, by = c("home_team_id", "away_team_id", "date")) %>%
    mutate(match_id = row_number())

  message(sprintf("   %d played, %d remaining", nrow(played_games), nrow(remaining_games)))

  # Build current standings
  home_standings <- played_games %>%
    group_by(team = home_team_id) %>%
    summarize(pts = sum(if_else(home_goals > away_goals, 3, if_else(home_goals == away_goals, 1, 0))),
              games = n(), .groups = "drop")

  away_standings <- played_games %>%
    group_by(team = away_team_id) %>%
    summarize(pts = sum(if_else(away_goals > home_goals, 3, if_else(home_goals == away_goals, 1, 0))),
              games = n(), .groups = "drop")

  standings_raw <- bind_rows(home_standings, away_standings) %>%
    group_by(team) %>%
    summarize(current_points = sum(pts), games_played = sum(games), .groups = "drop")

  current_standings <- tibble(team = all_team_ids) %>%
    left_join(standings_raw, by = "team") %>%
    mutate(
      current_points = if_else(is.na(current_points), 0, current_points),
      games_played   = if_else(is.na(games_played), 0L, games_played)
    )

  team_strengths_complete <- tibble(team = all_team_ids) %>%
    left_join(calculate_team_strengths(played_games), by = "team") %>%
    mutate(
      l_avg = mean(attack_strength, na.rm = TRUE),
      l_avg = if_else(is.na(l_avg) | l_avg < 0.1, 1.3, l_avg),
      attack_strength  = if_else(is.na(attack_strength),  l_avg, attack_strength),
      defense_strength = if_else(is.na(defense_strength), l_avg, defense_strength)
    ) %>%
    select(-l_avg)

  # Run simulation
  daemons(N_CORES)
  everywhere({ library(data.table) })

  sims_per_worker <- ceiling(N_SIMS / N_CORES)

  sim_results <- map(
    1:N_CORES,
    in_parallel(
      function(i) {
        set.seed(as.integer(Sys.time()) + i)
        this_n <- if (i == N_CORES) N_SIMS - (sims_per_worker * (N_CORES - 1)) else sims_per_worker
        simulate_season_vectorized(current_standings, remaining_games, team_strengths_complete, this_n)
      },
      current_standings = current_standings,
      remaining_games = remaining_games,
      team_strengths_complete = team_strengths_complete,
      sims_per_worker = sims_per_worker,
      N_SIMS = N_SIMS,
      N_CORES = N_CORES,
      simulate_season_vectorized = simulate_season_vectorized
    )
  ) %>% bind_rows()

  daemons(0)

  playoff_summary <- sim_results %>%
    lazy_dt() %>%
    group_by(team) %>%
    summarize(playoff_pct = mean(made_playoffs) * 100, avg_pts = mean(points), .groups = "drop") %>%
    as_tibble() %>%
    left_join(current_standings, by = "team") %>%
    left_join(teams %>% select(team_id, team_name, team_abbreviation), by = c("team" = "team_id"))

  match_probs <- get_match_probabilities(
    remaining_games,
    team_strengths_complete,
    teams,
    n_sims = min(N_SIMS, 50000)
  ) %>%
    left_join(remaining_games %>% select(match_id, match_date = date), by = "match_id")

  # Write to DB with historical timestamp — connect fresh after simulation
  con <- get_db_conn()

  model_version_id <- dbGetQuery(con,
    "SELECT model_version_id FROM model_versions WHERE version = '1.0'"
  )$model_version_id

  run_id <- dbGetQuery(con, sprintf(
    "INSERT INTO simulation_runs (run_at, n_sims, games_played, games_remaining, gameweek_id, model_version_id)
     VALUES ('%s'::timestamptz, %d, %d, %d, %d, %d) RETURNING run_id",
    paste0(cutoff_date, " 23:59:59 America/New_York"),
    N_SIMS, nrow(played_games), nrow(remaining_games), gameweek_id, model_version_id
  ))$run_id

  odds_rows <- playoff_summary %>%
    select(team_id = team, team_name, team_abbreviation,
           playoff_pct, avg_pts, current_points, games_played) %>%
    mutate(run_id = run_id, gameweek_id = gameweek_id)
  dbWriteTable(con, "playoff_odds", odds_rows, append = TRUE, row.names = FALSE)

  if (nrow(match_probs) > 0) {
    prob_rows <- match_probs %>%
      mutate(run_id = run_id, gameweek_id = gameweek_id) %>%
      select(run_id, gameweek_id, match_id,
             home_team_abbr = home_team, away_team_abbr = away_team,
             match_date, home_xg, away_xg, home_win_pct, draw_pct, away_win_pct,
             avg_home_goals, avg_away_goals)
    dbWriteTable(con, "match_probabilities", prob_rows, append = TRUE, row.names = FALSE)
  }

  message(sprintf("   Written run_id = %d", run_id))
  dbDisconnect(con)
}

message("\nBackfill complete.")
