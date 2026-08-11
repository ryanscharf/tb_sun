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

source("functions.R")

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

# ── Config ─────────────────────────────────────────────────────────────────────
# NA -> SQL NULL (rho is NA for non-Dixon-Coles presets, not a fitting failure).
sql_num <- function(x) if (is.null(x) || is.na(x)) "NULL" else as.character(x)

N_SIMS <- as.integer(Sys.getenv("N_SIMS", "1000000"))
N_CORES <- as.integer(Sys.getenv("N_CORES", "4"))

# GPU path (simulate_season_gpu/simulate_season_gpu_dc) validated against a
# corrected CPU reference via validate_gpu_dc_vs_cpu() -- max playoff_pct
# diff 0.481%, max scoreline-cell diff 0.494% at n_sims=100K. Falls back to
# CPU automatically if torch/CUDA aren't available in this environment.
USE_GPU <- as.logical(Sys.getenv("USE_GPU", "FALSE")) &&
  requireNamespace("torch", quietly = TRUE) &&
  isTRUE(tryCatch(torch::cuda_is_available(), error = function(e) FALSE))

# USL Super League's season labels aren't sequential (2024-25, 2025-26, Fall
# 2026, 2027, ...) so this can't be safely auto-incremented in code -- set it
# explicitly each season via the USL_SEASON env var.
CURRENT_SEASON <- Sys.getenv("USL_SEASON", "2025-26")

message(sprintf(
  "[%s] Starting playoff simulation (season %s, %s sims, %d cores, GPU: %s)",
  Sys.time(),
  CURRENT_SEASON,
  format(N_SIMS, big.mark = ","),
  N_CORES,
  USE_GPU
))

con <- get_db_conn()
on.exit(dbDisconnect(con), add = TRUE)

asa_client <- AmericanSoccerAnalysis$new()
teams <- suppressMessages(asa_client$get_teams(leagues = 'usls'))

# Sync schedule from FotMob first so is_completed is current
schedule <- get_schedule(con = con, season = CURRENT_SEASON)

# ── Skip if no new games since last run ────────────────────────────────────────
# Scoped to the current season -- otherwise a season transition could compare
# against the prior season's last-recorded games_played and skip the first
# real run of the new one if the counts happen to coincide.
last_games_played <- dbGetQuery(
  con,
  sprintf(
    "SELECT games_played FROM simulation_runs WHERE season = '%s' ORDER BY run_id DESC LIMIT 1",
    CURRENT_SEASON
  )
)$games_played

current_games_completed <- sum(schedule$is_completed, na.rm = TRUE)

if (
  length(last_games_played) > 0 && current_games_completed == last_games_played
) {
  message(sprintf(
    "[%s] No new games since last run (%d completed). Skipping.",
    Sys.time(),
    as.integer(current_games_completed)
  ))
  quit(status = 0)
}

# ── Active model version presets ────────────────────────────────────────────────
# Activation lives in the DB (model_versions.is_active), not code, so which
# presets run nightly can change without a redeploy. See
# model_versions_migration.sql for the curated preset rows and their
# feature_flags: time_decay_xi, dixon_coles_tau, shrinkage_k,
# fitted_home_advantage, and xg_blend_weight are all wired into
# calculate_playoff_odds_fast() now.
model_versions_df <- dbGetQuery(
  con,
  "SELECT model_version_id, version, label, feature_flags
   FROM model_versions WHERE is_active = TRUE ORDER BY model_version_id"
)

if (nrow(model_versions_df) == 0) {
  stop(
    "No active model_versions rows -- run model_versions_migration.sql and mark at least one is_active."
  )
}

# ── League-level structural parameters (home advantage, Dixon-Coles rho) ───────
# Computed once per run (not once per preset) by pooling across every
# completed USL SL season, rather than fit fresh from a single thin season
# each time -- see fit_pooled_league_params() for why these are treated
# differently from attack/defense given the league's heavy YoY roster
# turnover. Add each newly completed season's label as it becomes available;
# this degrades gracefully to a single-season fit until then.
#
# POOLED_SEASONS must be in chronological order, ending with CURRENT_SEASON
# -- fit_pooled_league_params() truncates CURRENT_SEASON's own games to
# "as of today" (point-in-time correctness) and pools any earlier season in
# full, so this run never has visibility into results that haven't happened
# yet (matters more for backfill_runner.R's historical cutoffs than here,
# since "today" naturally has no future games to leak in live -- but it also
# guards against POOLED_SEASONS accidentally listing a season that hasn't
# started yet).
POOLED_SEASONS <- trimws(strsplit(
  Sys.getenv("POOLED_SEASONS", CURRENT_SEASON),
  ","
)[[1]])

needs_league_params <- any(vapply(
  seq_len(nrow(model_versions_df)),
  function(i) {
    flags <- jsonlite::fromJSON(model_versions_df$feature_flags[i])
    isTRUE(flags$dixon_coles_tau) || isTRUE(flags$fitted_home_advantage)
  },
  logical(1)
))

league_params <- if (needs_league_params) {
  message(sprintf(
    "[%s] Fitting pooled league parameters across season(s): %s",
    Sys.time(),
    paste(POOLED_SEASONS, collapse = ", ")
  ))
  fit_pooled_league_params(
    POOLED_SEASONS,
    current_season = CURRENT_SEASON,
    cutoff_date = Sys.Date()
  )
} else {
  NULL
}

# gameweek_id is resolved from the first preset's played_games (below) since
# it's shared across presets -- schedule/gameweek don't vary by model version,
# and this avoids an extra throwaway ASA API call just to get played_games.
gameweek_id <- NULL

for (mv_row in seq_len(nrow(model_versions_df))) {
  model_version_id <- as.integer(model_versions_df$model_version_id[mv_row])
  mv_label <- model_versions_df$label[mv_row]
  mv_flags <- jsonlite::fromJSON(model_versions_df$feature_flags[mv_row])

  message(sprintf(
    "[%s] Running preset %s (model_version_id = %d)...",
    Sys.time(),
    mv_label,
    model_version_id
  ))

  output <- calculate_playoff_odds_fast(
    schedule,
    n_sims = N_SIMS,
    n_cores = N_CORES,
    qualify_top_n = 4,
    feature_flags = mv_flags,
    league_params = league_params,
    season = CURRENT_SEASON,
    use_gpu = USE_GPU
  )
  playoff_odds <- output$summary
  played_games <- output$played_games
  remaining_games <- output$remaining_games
  match_probs <- output$match_probs %>%
    left_join(
      remaining_games %>% select(match_id, match_date = date),
      by = "match_id"
    )
  scoreline_dist <- output$scoreline_dist
  rank_dist <- output$rank_dist
  cutoff_dist <- output$cutoff_dist

  if (is.null(gameweek_id)) {
    # max(played_games$date) on 0 rows returns -Inf (with a warning), which
    # then fails as a literal SQL date string below -- a season with no
    # games played yet (e.g. before its first game, or ASA not having data
    # for it yet) has no "most recent played date" to anchor gameweek 1 to,
    # so fall back to today as the baseline snapshot date instead.
    current_week_end <- if (nrow(played_games) > 0) {
      max(played_games$date)
    } else {
      Sys.Date()
    }
    current_week_start <- floor_date(
      current_week_end,
      unit = "week",
      week_start = 1
    )
    # Scoped to the current season -- gameweek_number resets to 1 each new
    # season rather than counting up forever across all of them.
    current_gw_number <- as.integer(
      dbGetQuery(
        con,
        sprintf(
          "SELECT COUNT(*) + 1 AS gw FROM gameweeks WHERE season = '%s' AND end_date < '%s'",
          CURRENT_SEASON,
          current_week_start
        )
      )$gw
    )

    gameweek_id <- as.integer(
      dbGetQuery(
        con,
        sprintf(
          "INSERT INTO gameweeks (season, gameweek_number, start_date, end_date)
       VALUES ('%s', %d, '%s', '%s')
       ON CONFLICT (season, gameweek_number) DO UPDATE SET end_date = EXCLUDED.end_date
       RETURNING gameweek_id",
          CURRENT_SEASON,
          current_gw_number,
          current_week_start,
          current_week_end
        )
      )$gameweek_id
    )
  }

  message(sprintf(
    "[%s] Writing %s results to Postgres...",
    Sys.time(),
    mv_label
  ))

  run_id <- as.integer(
    dbGetQuery(
      con,
      sprintf(
        "INSERT INTO simulation_runs (season, n_sims, games_played, games_remaining, gameweek_id, model_version_id, home_advantage, rho)
     VALUES ('%s', %d, %d, %d, %d, %d, %s, %s) RETURNING run_id",
        CURRENT_SEASON,
        N_SIMS,
        nrow(played_games),
        nrow(remaining_games),
        gameweek_id,
        model_version_id,
        sql_num(output$home_advantage_used),
        sql_num(output$rho_used)
      )
    )$run_id
  )

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

  if (nrow(scoreline_dist) > 0) {
    scoreline_rows <- scoreline_dist %>%
      left_join(
        remaining_games %>% select(match_id, match_date = date),
        by = "match_id"
      ) %>%
      mutate(run_id = run_id, gameweek_id = gameweek_id) %>%
      select(
        run_id,
        gameweek_id,
        match_id,
        home_team_abbr = home_team,
        away_team_abbr = away_team,
        match_date,
        home_goals,
        away_goals,
        scoreline,
        prob
      )

    dbWriteTable(
      con,
      "scoreline_distributions",
      scoreline_rows,
      append = TRUE,
      row.names = FALSE
    )
  }

  rank_rows <- rank_dist %>%
    mutate(run_id = run_id, gameweek_id = gameweek_id) %>%
    select(
      run_id,
      gameweek_id,
      team_id = team,
      team_abbreviation,
      rank,
      count,
      pct
    )
  dbWriteTable(
    con,
    "rank_distributions",
    rank_rows,
    append = TRUE,
    row.names = FALSE
  )

  cutoff_rows <- cutoff_dist %>%
    mutate(run_id = run_id, gameweek_id = gameweek_id) %>%
    select(run_id, gameweek_id, points, count, pct)
  dbWriteTable(
    con,
    "cutoff_distributions",
    cutoff_rows,
    append = TRUE,
    row.names = FALSE
  )

  message(sprintf(
    "[%s] Done with %s. run_id = %d",
    Sys.time(),
    mv_label,
    run_id
  ))
}

# ── Elo ratings (independent of model_versions/presets) ────────────────────────
# Computed once per run over the full current season in a single
# chronological pass (fast, no simulation) -- see compute_elo_ratings() in
# functions.R. Seeds from the season immediately prior to CURRENT_SEASON in
# POOLED_SEASONS (same chronological list used for league_params above),
# regressed toward the mean, if that prior season has any recorded ratings.
message(sprintf("[%s] Computing Elo ratings...", Sys.time()))

elo_all_results <- suppressMessages(asa_client$get_games(
  leagues = 'usls',
  season = CURRENT_SEASON
)) %>%
  as_tibble() %>%
  transmute(
    home_team_id,
    away_team_id,
    home_goals = home_score,
    away_goals = away_score,
    date = as.Date(date_time_utc),
    status
  )

elo_all_team_ids <- unique(c(
  elo_all_results$home_team_id,
  elo_all_results$away_team_id
))

season_pos <- match(CURRENT_SEASON, POOLED_SEASONS)
prior_season <- if (!is.na(season_pos) && season_pos > 1) {
  POOLED_SEASONS[season_pos - 1]
} else {
  NA
}

elo_starting_ratings <- if (!is.na(prior_season)) {
  prior_elo <- dbGetQuery(
    con,
    sprintf(
      "SELECT DISTINCT ON (team_id) team_id, elo_rating
       FROM elo_ratings WHERE season = '%s'
       ORDER BY team_id, computed_at DESC",
      prior_season
    )
  )
  if (nrow(prior_elo) > 0) {
    regress_elo_to_mean(setNames(prior_elo$elo_rating, prior_elo$team_id))
  } else {
    NULL
  }
} else {
  NULL
}

elo_history <- compute_elo_ratings(
  elo_all_results,
  elo_all_team_ids,
  starting_ratings = elo_starting_ratings
)

write_elo_snapshot(
  con,
  CURRENT_SEASON,
  gameweek_id,
  elo_snapshot_at(
    elo_history,
    max(elo_all_results$date[elo_all_results$status == "FullTime"]),
    elo_all_team_ids
  ) %>%
    left_join(
      teams %>% select(team_id, team_name, team_abbreviation),
      by = "team_id"
    )
)

message(sprintf("[%s] All presets complete.", Sys.time()))
