# gpu_backfill_reconcile.R
#
# Sanity check before resuming the full backfill with USE_GPU=TRUE: picks one
# (season, model_version, cutoff_date) combination that was already written
# by the earlier CPU-only backfill run, re-simulates that exact cutoff with
# use_gpu=TRUE, and compares the new playoff_pct per team against what's
# already sitting in the DB. Doesn't write anything -- read-only comparison.
#
# Run interactively; expects DB_HOST/DB_PORT/DB_NAME/DB_USERNAME/DB_PASSWORD
# in the environment (.env loaded below if present).

suppressPackageStartupMessages({
  library(tidyverse)
  library(itscalledsoccer)
  library(mirai)
  library(data.table)
  library(dtplyr)
  library(DBI)
  library(RPostgres)
})

source("functions.R")

if (file.exists(".env")) {
  lines <- readLines(".env")
  lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]
  for (line in lines) {
    kv <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(kv) >= 2) {
      do.call(Sys.setenv, setNames(list(trimws(paste(kv[-1], collapse = "="))), trimws(kv[1])))
    }
  }
}

library(torch)
if (!cuda_is_available()) stop("CUDA not available in this session.")

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

# ── Pick one already-backfilled (season, model_version, cutoff_date) ───────────
# Prefer a mid-season cutoff (not the very first or last day) so there's a
# meaningful number of remaining games to actually exercise the simulator.
con <- get_db_conn()
candidate <- dbGetQuery(con, "
  SELECT sr.run_id, sr.season, sr.model_version_id, mv.label, mv.feature_flags,
         (sr.run_at AT TIME ZONE 'America/New_York')::date AS cutoff_date,
         sr.games_remaining
  FROM simulation_runs sr
  JOIN model_versions mv ON mv.model_version_id = sr.model_version_id
  WHERE sr.games_remaining > 0
  ORDER BY ABS(sr.games_remaining - 50)
  LIMIT 1
")

if (nrow(candidate) == 0) stop("No existing simulation_runs rows found to reconcile against -- has any backfill run written data yet?")

season <- candidate$season[1]
model_version_id <- candidate$model_version_id[1]
mv_label <- candidate$label[1]
mv_flags <- jsonlite::fromJSON(candidate$feature_flags[1])
cutoff_date <- as.Date(candidate$cutoff_date[1])
run_id <- candidate$run_id[1]

message(sprintf(
  "Reconciling: season=%s, preset=%s (model_version_id=%d), cutoff=%s (existing run_id=%d, %d games remaining)",
  season, mv_label, model_version_id, cutoff_date, run_id, candidate$games_remaining[1]
))

existing_odds <- dbGetQuery(con, sprintf(
  "SELECT team_id, team_abbreviation, playoff_pct, avg_pts FROM playoff_odds WHERE run_id = %d",
  run_id
))

# ── League params: needs the same point-in-time-correct pooled fit the
# original backfill used if this preset relies on it. ──────────────────────────
needs_league_params <- isTRUE(mv_flags$dixon_coles_tau) || isTRUE(mv_flags$fitted_home_advantage)

asa_client <- AmericanSoccerAnalysis$new()
teams <- suppressMessages(asa_client$get_teams(leagues = "usls"))

all_results <- suppressMessages(asa_client$get_games(leagues = "usls", season = season)) %>%
  as_tibble() %>%
  transmute(
    home_team_id, away_team_id,
    home_goals = home_score, away_goals = away_score,
    date = as.Date(date_time_utc), status
  )
all_team_ids <- unique(c(all_results$home_team_id, all_results$away_team_id))

league_params <- if (needs_league_params) {
  fit_pooled_league_params(season, current_season = season, cutoff_date = cutoff_date)
} else {
  NULL
}

message("Re-simulating this exact cutoff with use_gpu = TRUE (1M sims, matching production N_SIMS)...")
gpu_output <- calculate_playoff_odds_at_cutoff(
  all_results, cutoff_date, all_team_ids,
  n_sims = 1000000, n_cores = 4, qualify_top_n = 4L,
  feature_flags = mv_flags, league_params = league_params, season = season,
  use_gpu = TRUE
)

gpu_odds <- gpu_output$summary %>%
  select(team_id = team, gpu_playoff_pct = playoff_pct, gpu_avg_pts = avg_pts)

comparison <- existing_odds %>%
  rename(cpu_playoff_pct = playoff_pct, cpu_avg_pts = avg_pts) %>%
  inner_join(gpu_odds, by = "team_id") %>%
  mutate(
    playoff_pct_diff = abs(cpu_playoff_pct - gpu_playoff_pct),
    avg_pts_diff = abs(cpu_avg_pts - gpu_avg_pts)
  )

cat("\n── CPU (already in DB) vs fresh GPU rerun, same cutoff ──\n")
print(comparison %>% select(team_abbreviation, cpu_playoff_pct, gpu_playoff_pct, playoff_pct_diff,
                             cpu_avg_pts, gpu_avg_pts, avg_pts_diff))

cat(sprintf(
  "\nMax playoff_pct diff: %.3f%% | Max avg_pts diff: %.3f\n(expected: small, from independent RNG streams -- NOT necessarily near-zero, since N_SIMS=1M each side is a fresh Monte Carlo draw)\n",
  max(comparison$playoff_pct_diff), max(comparison$avg_pts_diff)
))

dbDisconnect(con)
