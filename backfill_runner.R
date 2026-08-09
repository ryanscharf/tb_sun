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

N_SIMS <- as.integer(Sys.getenv("N_SIMS", "1000000"))
N_CORES <- as.integer(Sys.getenv("N_CORES", "4"))

# Seasons to backfill. USL Super League's season labels aren't sequential
# (2024-25, 2025-26, Fall 2026, 2027, ...) so this is an explicit list, not
# something inferred from the current season.
BACKFILL_SEASONS <- trimws(strsplit(
  Sys.getenv("BACKFILL_SEASONS", "2024-25,2025-26"),
  ","
)[[1]])

message(sprintf(
  "[%s] Starting backfill (seasons: %s, %s sims, %d cores)",
  Sys.time(),
  paste(BACKFILL_SEASONS, collapse = ", "),
  format(N_SIMS, big.mark = ","),
  N_CORES
))

asa_client <- AmericanSoccerAnalysis$new()
teams <- suppressMessages(asa_client$get_teams(leagues = 'usls'))

# ── Active model version presets ────────────────────────────────────────────────
# Same presets playoff_runner.R runs nightly -- see model_versions_migration.sql.
init_con <- get_db_conn()
model_versions_df <- dbGetQuery(
  init_con,
  "SELECT model_version_id, version, label, feature_flags
   FROM model_versions WHERE is_active = TRUE ORDER BY model_version_id"
)

if (nrow(model_versions_df) == 0) {
  dbDisconnect(init_con)
  stop(
    "No active model_versions rows -- run model_versions_migration.sql and mark at least one is_active."
  )
}
dbDisconnect(init_con)

# ── League-level structural parameters (home advantage, Dixon-Coles rho) ───────
# Computed once, pooled across every season being backfilled (not per
# season/gameday/preset) -- see fit_pooled_league_params() in functions.R for
# why these are treated as league-structural rather than roster-specific
# given USL SL's heavy YoY roster turnover.
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
    "[%s] Fitting pooled league parameters across: %s",
    Sys.time(),
    paste(BACKFILL_SEASONS, collapse = ", ")
  ))
  fit_pooled_league_params(BACKFILL_SEASONS)
} else {
  NULL
}

# ── Backfill each season ────────────────────────────────────────────────────────
for (season in BACKFILL_SEASONS) {
  message(sprintf(
    "\n══ Season %s ══════════════════════════════════════════════════════",
    season
  ))

  # Pulled directly from ASA -- includes both played AND future/scheduled
  # games for the season, so this is self-contained and doesn't depend on
  # FotMob's live schedule (which only reflects whichever season is
  # currently underway, not a historical one like 2024-25).
  all_results <- suppressMessages(asa_client$get_games(
    leagues = 'usls',
    season = season
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

  all_team_ids <- unique(c(all_results$home_team_id, all_results$away_team_id))

  played_dates <- all_results %>%
    filter(status == "FullTime") %>%
    distinct(date) %>%
    arrange(date) %>%
    pull(date)

  if (length(played_dates) == 0) {
    message(sprintf(
      "No completed games found for season %s -- skipping.",
      season
    ))
    next
  }

  # gameweek_number groups game days by ISO week (same convention as
  # playoff_runner.R, for chart continuity), but a simulation run is written
  # for EVERY distinct game day below, not just once per week -- matching
  # the end-of-game-day cadence the live runner uses.
  gameweeks_df <- tibble(date = played_dates) %>%
    mutate(
      iso_week = paste(
        lubridate::isoyear(date),
        lubridate::isoweek(date),
        sep = "-W"
      )
    ) %>%
    group_by(iso_week) %>%
    mutate(start_date = min(date), end_date = max(date)) %>%
    ungroup() %>%
    distinct(iso_week, start_date, end_date) %>%
    arrange(start_date) %>%
    mutate(gameweek_number = row_number())

  message(sprintf(
    "Season %s: %d game day(s) across %d gameweek(s) to backfill x %d model version(s).",
    season,
    length(played_dates),
    nrow(gameweeks_df),
    nrow(model_versions_df)
  ))

  con <- get_db_conn()
  dbWriteTable(
    con,
    "gameweeks",
    gameweeks_df %>%
      mutate(season = season) %>%
      select(season, gameweek_number, start_date, end_date),
    append = TRUE,
    row.names = FALSE
  )
  gameweeks_with_ids <- dbGetQuery(
    con,
    sprintf(
      "SELECT gameweek_id, gameweek_number FROM gameweeks WHERE season = '%s' ORDER BY gameweek_number",
      season
    )
  )
  dbDisconnect(con)

  # Map each played game-day to its gameweek_id.
  day_to_gw <- tibble(date = played_dates) %>%
    mutate(
      iso_week = paste(
        lubridate::isoyear(date),
        lubridate::isoweek(date),
        sep = "-W"
      )
    ) %>%
    left_join(
      gameweeks_df %>% select(iso_week, gameweek_number),
      by = "iso_week"
    ) %>%
    left_join(gameweeks_with_ids, by = "gameweek_number")

  # ── Loop through each game day x each active model version preset ────────────
  for (day_idx in seq_along(played_dates)) {
    cutoff_date <- played_dates[day_idx]
    gw_row <- day_to_gw %>% filter(date == cutoff_date)
    gameweek_id <- as.integer(gw_row$gameweek_id[1])
    gw_number <- gw_row$gameweek_number[1]

    for (mv_row in seq_len(nrow(model_versions_df))) {
      model_version_id <- as.integer(model_versions_df$model_version_id[mv_row])
      mv_label <- model_versions_df$label[mv_row]
      mv_flags <- jsonlite::fromJSON(model_versions_df$feature_flags[mv_row])

      message(sprintf(
        "[%s] %s | gameweek %d (%s) | %s",
        Sys.time(),
        season,
        gw_number,
        cutoff_date,
        mv_label
      ))

      output <- calculate_playoff_odds_at_cutoff(
        all_results,
        cutoff_date,
        all_team_ids,
        n_sims = N_SIMS,
        n_cores = N_CORES,
        qualify_top_n = 4L,
        feature_flags = mv_flags,
        league_params = league_params,
        season = season
      )

      played_games <- output$played_games
      remaining_games <- output$remaining_games
      playoff_odds <- output$summary
      match_probs <- output$match_probs %>%
        left_join(
          remaining_games %>% select(match_id, match_date = date),
          by = "match_id"
        )
      scoreline_dist <- output$scoreline_dist %>%
        left_join(
          remaining_games %>% select(match_id, match_date = date),
          by = "match_id"
        )
      rank_dist <- output$rank_dist
      cutoff_dist <- output$cutoff_dist

      # ── Write to DB ────────────────────────────────────────────────────────
      con <- get_db_conn()

      run_id <- as.integer(
        dbGetQuery(
          con,
          sprintf(
            "INSERT INTO simulation_runs (run_at, season, n_sims, games_played, games_remaining, gameweek_id, model_version_id)
         VALUES ('%s'::timestamptz, '%s', %d, %d, %d, %d, %d) RETURNING run_id",
            paste0(cutoff_date, " 23:59:59 America/New_York"),
            season,
            N_SIMS,
            nrow(played_games),
            nrow(remaining_games),
            gameweek_id,
            model_version_id
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
      dbWriteTable(
        con,
        "playoff_odds",
        odds_rows,
        append = TRUE,
        row.names = FALSE
      )

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

      dbDisconnect(con)
      message(sprintf("   Written run_id = %d", run_id))
    }
  }
}

message(sprintf("\n[%s] Backfill complete.", Sys.time()))
