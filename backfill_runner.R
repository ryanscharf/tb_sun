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
# Recomputed per game-day below (NOT once globally) -- each day's backfilled
# snapshot must only see results known as of that day. Seasons strictly
# before the one being backfilled are pooled in full (fully known already);
# the current season is truncated to date <= cutoff_date; any season listed
# after the current one in BACKFILL_SEASONS is excluded entirely. See
# fit_pooled_league_params() in functions.R.
needs_league_params <- any(vapply(
  seq_len(nrow(model_versions_df)),
  function(i) {
    flags <- jsonlite::fromJSON(model_versions_df$feature_flags[i])
    isTRUE(flags$dixon_coles_tau) || isTRUE(flags$fitted_home_advantage)
  },
  logical(1)
))

# Elo carries forward across seasons (regressed toward the mean -- see
# regress_elo_to_mean()), unlike attack/defense which resets fully each
# season -- see functions.R's Elo section for the reasoning. NULL for the
# first season backfilled (nothing to carry forward from).
carried_elo_ratings <- NULL

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

  # Resumable: only insert gameweeks that aren't already there (a prior
  # crashed/interrupted run may have already written some of this season's
  # gameweeks) -- everything below is keyed off the DB's existing state
  # rather than assuming a clean slate.
  con <- get_db_conn()
  existing_gw_numbers <- dbGetQuery(
    con,
    sprintf("SELECT gameweek_number FROM gameweeks WHERE season = '%s'", season)
  )$gameweek_number

  new_gameweeks <- gameweeks_df %>% filter(!(gameweek_number %in% existing_gw_numbers))
  if (nrow(new_gameweeks) > 0) {
    dbWriteTable(
      con,
      "gameweeks",
      new_gameweeks %>%
        mutate(season = season) %>%
        select(season, gameweek_number, start_date, end_date),
      append = TRUE,
      row.names = FALSE
    )
  }

  gameweeks_with_ids <- dbGetQuery(
    con,
    sprintf(
      "SELECT gameweek_id, gameweek_number FROM gameweeks WHERE season = '%s' ORDER BY gameweek_number",
      season
    )
  )

  # (season, model_version_id, run_at::date) already written -- run_at is
  # set to cutoff_date below via '{cutoff_date} 23:59:59 America/New_York'.
  # Casting straight to ::date uses Postgres's SESSION timezone, not
  # necessarily America/New_York -- if the session defaults to UTC, 23:59:59
  # America/New_York lands after midnight UTC, so run_at::date silently
  # comes back as cutoff_date + 1 and never matches, breaking the skip
  # check entirely. `AT TIME ZONE 'America/New_York'` reconstructs the
  # original wall-clock date regardless of session timezone.
  existing_runs <- dbGetQuery(
    con,
    sprintf(
      "SELECT model_version_id, (run_at AT TIME ZONE 'America/New_York')::date AS run_date FROM simulation_runs WHERE season = '%s'",
      season
    )
  )
  existing_run_keys <- if (nrow(existing_runs) > 0) {
    paste(existing_runs$model_version_id, existing_runs$run_date)
  } else {
    character(0)
  }
  if (length(existing_run_keys) > 0) {
    message(sprintf(
      "Resuming: %d (game-day, preset) run(s) already written for season %s -- skipping those.",
      length(existing_run_keys), season
    ))
  }

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

  # ── Elo ratings (independent of model_versions/presets) ──────────────────────
  # Computed once for the whole season in a single chronological pass (fast,
  # no simulation) -- see compute_elo_ratings() in functions.R. Carries
  # forward the prior season's final ratings, regressed toward the mean,
  # rather than a flat 1500 start.
  elo_history <- compute_elo_ratings(
    all_results,
    all_team_ids,
    starting_ratings = carried_elo_ratings
  )

  # ── Loop through each game day x each active model version preset ────────────
  for (day_idx in seq_along(played_dates)) {
    cutoff_date <- played_dates[day_idx]
    gw_row <- day_to_gw %>% filter(date == cutoff_date)
    gameweek_id <- as.integer(gw_row$gameweek_id[1])
    gw_number <- gw_row$gameweek_number[1]

    # Written for every game day regardless of whether the simulation work
    # below is skipped by resumability -- cheap, and a resumed run that
    # skips most/all simulation presets for a day should still fully
    # populate elo_ratings for it.
    elo_con <- get_db_conn()
    write_elo_snapshot(
      elo_con, season, gameweek_id,
      elo_snapshot_at(elo_history, cutoff_date, all_team_ids) %>%
        left_join(teams %>% select(team_id, team_name, team_abbreviation), by = "team_id")
    )
    dbDisconnect(elo_con)

    # Skip this day entirely (before any expensive computation, including
    # the league-params fit below) if every active preset is already
    # written for it -- resumability.
    day_pending <- vapply(seq_len(nrow(model_versions_df)), function(mv_row) {
      mv_id <- as.integer(model_versions_df$model_version_id[mv_row])
      !(paste(mv_id, as.character(cutoff_date)) %in% existing_run_keys)
    }, logical(1))

    if (!any(day_pending)) {
      next
    }

    # Point-in-time correctness: refit fresh for THIS cutoff_date so no
    # day's backfilled snapshot has visibility into results that hadn't
    # happened yet as of that day (see fit_pooled_league_params()).
    day_league_params <- if (needs_league_params) {
      fit_pooled_league_params(
        BACKFILL_SEASONS,
        current_season = season,
        cutoff_date = cutoff_date
      )
    } else {
      NULL
    }

    for (mv_row in seq_len(nrow(model_versions_df))) {
      model_version_id <- as.integer(model_versions_df$model_version_id[mv_row])
      mv_label <- model_versions_df$label[mv_row]
      mv_flags <- jsonlite::fromJSON(model_versions_df$feature_flags[mv_row])

      if (paste(model_version_id, as.character(cutoff_date)) %in% existing_run_keys) {
        next
      }

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
        league_params = day_league_params,
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

  # Carry this season's final Elo ratings into the next season, regressed
  # toward the mean (see regress_elo_to_mean()).
  final_elo_snapshot <- elo_snapshot_at(elo_history, max(played_dates), all_team_ids)
  carried_elo_ratings <- regress_elo_to_mean(
    setNames(final_elo_snapshot$elo_rating, final_elo_snapshot$team_id)
  )
}

message(sprintf("\n[%s] Backfill complete.", Sys.time()))
