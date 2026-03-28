suppressPackageStartupMessages({
  library(tidyverse)
  library(itscalledsoccer)
  library(mirai)
  library(data.table)
  library(dtplyr)
  library(httr2)
  library(jsonlite)
  library(DBI)
  library(RPostgres)
})

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

source("functions.R")

# ── Config ─────────────────────────────────────────────────────────────────────
N_SIMS <- as.integer(Sys.getenv("N_SIMS", "100000"))
N_CORES <- as.integer(Sys.getenv("N_CORES", "4"))

message(sprintf(
  "[%s] Starting playoff simulation (%s sims, %d cores)",
  Sys.time(),
  format(N_SIMS, big.mark = ","),
  N_CORES
))

con <- get_db_conn()
on.exit(dbDisconnect(con), add = TRUE)

asa_client <- AmericanSoccerAnalysis$new()
teams <- suppressMessages(asa_client$get_teams(leagues = 'usls'))
schedule <- get_schedule(con = con)

output <- calculate_playoff_odds_fast(
  schedule,
  n_sims = N_SIMS,
  n_cores = N_CORES
)
playoff_odds     <- output$summary
played_games     <- output$played_games
remaining_games  <- output$remaining_games
match_probs      <- output$match_probs %>%
  left_join(remaining_games %>% select(match_id, match_date = date), by = "match_id")
scoreline_dist   <- output$scoreline_dist
rank_dist        <- output$rank_dist
cutoff_dist      <- output$cutoff_dist

# ── Write to Postgres ──────────────────────────────────────────────────────────
message(sprintf("[%s] Writing results to Postgres...", Sys.time()))

# Upsert current gameweek based on the most recent played game's ISO week
current_week_start <- floor_date(max(played_games$date), unit = "week", week_start = 1)
current_week_end   <- max(played_games$date)
current_gw_number <- as.integer(dbGetQuery(con, sprintf(
  "SELECT COUNT(*) + 1 AS gw FROM gameweeks WHERE end_date < '%s'", current_week_start
))$gw)

gameweek_id <- as.integer(dbGetQuery(con, sprintf(
  "INSERT INTO gameweeks (gameweek_number, start_date, end_date)
   VALUES (%d, '%s', '%s')
   ON CONFLICT (gameweek_number) DO UPDATE SET end_date = EXCLUDED.end_date
   RETURNING gameweek_id",
  current_gw_number, current_week_start, current_week_end
))$gameweek_id)

model_version_id <- as.integer(dbGetQuery(con,
  "SELECT model_version_id FROM model_versions WHERE version = '1.0'"
)$model_version_id)

run_id <- as.integer(dbGetQuery(
  con,
  sprintf(
    "INSERT INTO simulation_runs (n_sims, games_played, games_remaining, gameweek_id, model_version_id)
   VALUES (%d, %d, %d, %d, %d) RETURNING run_id",
    N_SIMS,
    nrow(played_games),
    nrow(remaining_games),
    gameweek_id,
    model_version_id
  )
)$run_id)

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

  dbWriteTable(con, "match_probabilities", prob_rows, append = TRUE, row.names = FALSE)
}

if (nrow(scoreline_dist) > 0) {
  scoreline_rows <- scoreline_dist %>%
    left_join(remaining_games %>% select(match_id, match_date = date), by = "match_id") %>%
    mutate(run_id = run_id, gameweek_id = gameweek_id) %>%
    select(run_id, gameweek_id, match_id,
           home_team_abbr = home_team, away_team_abbr = away_team,
           match_date, home_goals, away_goals, scoreline, prob)

  dbWriteTable(con, "scoreline_distributions", scoreline_rows, append = TRUE, row.names = FALSE)
}

rank_rows <- rank_dist %>%
  mutate(run_id = run_id, gameweek_id = gameweek_id) %>%
  select(run_id, gameweek_id, team_id = team, team_abbreviation, rank, count, pct)
dbWriteTable(con, "rank_distributions", rank_rows, append = TRUE, row.names = FALSE)

cutoff_rows <- cutoff_dist %>%
  mutate(run_id = run_id, gameweek_id = gameweek_id) %>%
  select(run_id, gameweek_id, points, count, pct)
dbWriteTable(con, "cutoff_distributions", cutoff_rows, append = TRUE, row.names = FALSE)

message(sprintf("[%s] Done. run_id = %d", Sys.time(), run_id))
