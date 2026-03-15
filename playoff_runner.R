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
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USERNAME"),
    password = Sys.getenv("DB_PASSWORD")
  )
}

# Source functions and team_name_mapping from the modeling script.
# eval() stops execution before the bottom section that runs analysis and plots.
local({
  lines <- readLines("playoff_modeling.R")
  # Lines 12-621: all function definitions (stops before the main execution block)
  eval(parse(text = lines[12:621]), envir = globalenv())
  # Lines 638-686: get_match_probabilities (skips the tictoc/run block at 623-635)
  eval(parse(text = lines[638:686]), envir = globalenv())
})

# ── Config ─────────────────────────────────────────────────────────────────────
N_SIMS <- as.integer(Sys.getenv("N_SIMS", "100000"))
N_CORES <- as.integer(Sys.getenv("N_CORES", "4"))

message(sprintf(
  "[%s] Starting playoff simulation (%s sims, %d cores)",
  Sys.time(),
  format(N_SIMS, big.mark = ","),
  N_CORES
))

asa_client <- AmericanSoccerAnalysis$new()
teams <- asa_client$get_teams(leagues = 'usls')
schedule <- get_schedule()

output <- calculate_playoff_odds_fast(
  schedule,
  n_sims = N_SIMS,
  n_cores = N_CORES
)
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
  select(
    home_team_id,
    away_team_id,
    home_goals = home_score,
    away_goals = away_score,
    date = date_only
  )

remaining_games <- schedule_mapped %>%
  anti_join(played_games, by = c("home_team_id", "away_team_id", "date")) %>%
  mutate(match_id = row_number())

all_team_ids <- unique(c(
  schedule_mapped$home_team_id,
  schedule_mapped$away_team_id
))

team_strengths_complete <- tibble(team = all_team_ids) %>%
  left_join(calculate_team_strengths(played_games), by = "team") %>%
  mutate(
    l_avg = mean(attack_strength, na.rm = TRUE),
    attack_strength = if_else(is.na(attack_strength), l_avg, attack_strength),
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

# Upsert current gameweek based on the most recent played game's ISO week
current_week_start <- floor_date(max(played_games$date), unit = "week", week_start = 1)
current_week_end   <- max(played_games$date)
current_gw_number  <- dbGetQuery(con, sprintf(
  "SELECT COUNT(*) + 1 AS gw FROM gameweeks WHERE end_date < '%s'", current_week_start
))$gw

gameweek_id <- dbGetQuery(con, sprintf(
  "INSERT INTO gameweeks (gameweek_number, start_date, end_date)
   VALUES (%d, '%s', '%s')
   ON CONFLICT (gameweek_number) DO UPDATE SET end_date = EXCLUDED.end_date
   RETURNING gameweek_id",
  current_gw_number, current_week_start, current_week_end
))$gameweek_id

run_id <- dbGetQuery(
  con,
  sprintf(
    "INSERT INTO simulation_runs (n_sims, games_played, games_remaining, gameweek_id)
   VALUES (%d, %d, %d, %d) RETURNING run_id",
    N_SIMS,
    nrow(played_games),
    nrow(remaining_games),
    gameweek_id
  )
)$run_id

odds_rows <- playoff_odds %>%
  select(
    team_id = team,
    team_name,
    team_abbreviation,
    playoff_pct,
    avg_pts,
    current_points,
    games_played
  ) %>%
  mutate(run_id = run_id, gameweek_id = gameweek_id)

dbWriteTable(con, "playoff_odds", odds_rows, append = TRUE, row.names = FALSE)

if (nrow(match_probs) > 0) {
  prob_rows <- match_probs %>%
    mutate(run_id = run_id, gameweek_id = gameweek_id) %>%
    select(
      run_id,
      gameweek_id,
      match_id,
      home_team_abbr = home_team,
      away_team_abbr = away_team,
      match_date,
      home_xg,
      away_xg,
      home_win_pct,
      draw_pct,
      away_win_pct,
      avg_home_goals,
      avg_away_goals
    )

  dbWriteTable(
    con,
    "match_probabilities",
    prob_rows,
    append = TRUE,
    row.names = FALSE
  )
}

message(sprintf("[%s] Done. run_id = %d", Sys.time(), run_id))
