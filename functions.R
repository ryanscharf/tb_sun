suppressPackageStartupMessages({
  library(tidyverse)
  library(itscalledsoccer)
  library(mirai)
  library(data.table)
  library(dtplyr)
  library(httr2)
  library(jsonlite)
})

# Load .env if present (local dev on Windows)
if (file.exists(".env")) {
  lines <- readLines(".env")
  lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]
  for (line in lines) {
    kv <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(kv) >= 2) {
      key <- trimws(kv[1])
      val <- trimws(paste(kv[-1], collapse = "="))
      do.call(Sys.setenv, setNames(list(val), key))
    }
  }
}


get_schedule <- function(league_id = "10699", season = Sys.getenv("USL_SEASON", "2025-26"), con = NULL) {
  url <- paste0("https://www.fotmob.com/api/data/leagues?id=", league_id)

  fotmob_schedule <- tryCatch(
    {
      data <- request(url) %>%
        req_perform() %>%
        resp_body_string() %>%
        fromJSON()

      data$fixtures$allMatches %>%
        unnest_wider(everything(), names_sep = "_") %>%
        unnest_wider(any_of(c("home", "away")), names_sep = "_") %>%
        unnest_wider(starts_with("status"), names_sep = "_") %>%
        select(
          home_team = home_name,
          away_team = away_name,
          is_completed = status_finished_1,
          date_utc = status_utcTime_1
        ) %>%
        mutate(
          date_utc = as.POSIXct(
            date_utc,
            format = "%Y-%m-%dT%H:%M:%S",
            tz = "UTC"
          ),
          season = season
        )
    },
    error = function(e) {
      message(sprintf("FotMob fetch failed: %s", conditionMessage(e)))
      NULL
    }
  )

  if (!is.null(fotmob_schedule) && !is.null(con)) {
    # Diff against this season's DB rows only to flag rescheduled games
    db_schedule <- dbGetQuery(
      con,
      sprintf(
        "SELECT home_team, away_team, date_utc FROM schedule WHERE season = '%s'",
        season
      )
    ) %>%
      mutate(date_utc = as.POSIXct(date_utc, tz = "UTC"))

    # A game is rescheduled if its (home, away, old_date) is no longer in FotMob
    # but a new date exists for the same matchup. Anti-join avoids many-to-many
    # issues when teams meet more than once in a season.
    fotmob_keys <- fotmob_schedule %>% select(home_team, away_team, date_utc)
    db_keys <- db_schedule %>% select(home_team, away_team, date_utc)

    removed_games <- anti_join(
      db_keys,
      fotmob_keys,
      by = c("home_team", "away_team", "date_utc")
    )
    added_games <- anti_join(
      fotmob_keys,
      db_keys,
      by = c("home_team", "away_team", "date_utc")
    )

    rescheduled_keys <- inner_join(
      added_games,
      removed_games %>% select(home_team, away_team),
      by = c("home_team", "away_team")
    ) %>%
      distinct(home_team, away_team, date_utc)

    fotmob_schedule <- fotmob_schedule %>%
      left_join(
        rescheduled_keys %>% mutate(is_rescheduled = TRUE),
        by = c("home_team", "away_team", "date_utc")
      ) %>%
      mutate(is_rescheduled = coalesce(is_rescheduled, FALSE))

    n_rescheduled <- sum(fotmob_schedule$is_rescheduled)
    if (n_rescheduled > 0) {
      message(sprintf("%d game(s) flagged as rescheduled.", n_rescheduled))
    }

    dbWriteTable(
      con,
      "schedule_staging",
      fotmob_schedule,
      temporary = TRUE,
      overwrite = TRUE,
      row.names = FALSE
    )

    dbExecute(
      con,
      "
      INSERT INTO schedule (season, home_team, away_team, is_completed, is_rescheduled, date_utc)
      SELECT season, home_team, away_team, is_completed, is_rescheduled, date_utc FROM schedule_staging
      ON CONFLICT (season, home_team, away_team, date_utc)
      DO UPDATE SET
        is_completed   = EXCLUDED.is_completed,
        is_rescheduled = EXCLUDED.is_rescheduled,
        updated_at     = NOW()
    "
    )

    deleted <- dbExecute(
      con,
      sprintf(
        "
      DELETE FROM schedule
      WHERE season = '%s'
        AND NOT EXISTS (
          SELECT 1 FROM schedule_staging s
          WHERE s.home_team = schedule.home_team
            AND s.away_team = schedule.away_team
            AND s.date_utc  = schedule.date_utc
        )
    ",
        season
      )
    )
    if (deleted > 0) {
      message(sprintf("Removed %d stale schedule rows.", deleted))
    }

    message("Schedule synced to DB.")
    return(fotmob_schedule)
  }

  if (!is.null(fotmob_schedule)) {
    return(fotmob_schedule)
  }

  if (!is.null(con)) {
    message("FotMob unavailable — falling back to DB schedule.")
    schedule <- dbGetQuery(
      con,
      sprintf(
        "SELECT home_team, away_team, is_completed, date_utc FROM schedule WHERE season = '%s' ORDER BY date_utc",
        season
      )
    ) %>%
      mutate(date_utc = as.POSIXct(date_utc, tz = "UTC"))
    if (nrow(schedule) == 0) {
      stop("DB schedule fallback is empty.")
    }
    return(schedule)
  }

  stop("FotMob fetch failed and no DB connection provided for fallback.")
}

team_name_mapping <- tribble(
  ~schedule_name    , ~team_abbreviation , ~team_id     , ~fotmob_name                    ,
  "Brooklyn"        , "BKN"              , "7vQ7dBYMD1" , "Brooklyn FC (W)"               ,
  "Tampa Bay"       , "TB"               , "9Yqdgo95vJ" , "Tampa Bay Sun FC (W)"          ,
  "Dallas"          , "DAL"              , "2vQ1y44QrA" , "Dallas Trinity FC (W)"         ,
  "Fort Lauderdale" , "FTL"              , "a35r7yBqL6" , "Fort Lauderdale United FC (W)" ,
  "Lexington"       , "LEX"              , "OlMlPegMLz" , "Lexington SC (W)"              ,
  "Spokane"         , "SPK"              , "Vj58dW358n" , "Spokane Zephyr FC (W)"         ,
  "DC"              , "DC"               , "KXMeG8xq64" , "DC Power FC (W)"               ,
  "Jacksonville"    , "JAX"              , "raMyb1d5d2" , "Sporting JAX (W)"              ,
  "Carolina"        , "CAR"              , "eV5D7zaMKn" , "Carolina Ascent FC (W)"
)

# usl_sl_team_brands <- tribble(
#   ~team_abbreviation , ~primary  , ~secondary , ~crest_url                                                                                                                                        ,
#   "BKN"              , "#C5BFAE" , "#0B0B0C"  , "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0c/Brooklyn_FC_logo.svg/250px-Brooklyn_FC_logo.svg.png"                                   ,
#   "TB"               , "#141B4D" , "#FBDE40"  , "https://upload.wikimedia.org/wikipedia/en/thumb/4/43/Tampa_Bay_Sun_FC_logo.png/250px-Tampa_Bay_Sun_FC_logo.png"                                  ,
#   "DAL"              , "#54052B" , "#B39262"  , "https://upload.wikimedia.org/wikipedia/en/thumb/4/43/Dallas_Trinity_FC_logo.png/250px-Dallas_Trinity_FC_logo.png"                                ,
#   "FTL"              , "#A7E6D7" , "#003087"  , "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/Fort_Lauderdale_United_FC_logo_2024.png/250px-Fort_Lauderdale_United_FC_logo_2024.png" ,
#   "LEX"              , "#003A0E" , "#004205"  , "https://upload.wikimedia.org/wikipedia/en/thumb/1/18/Lexington_SC_logo.png/120px-Lexington_SC_logo.png"                                          ,
#   "SPK"              , "#0095B3" , "#000000"  , "https://upload.wikimedia.org/wikipedia/commons/e/e5/Spokane_Zephyr_FC_logo.png"                                                                  ,
#   "DC"               , "#000000" , "#147BD1"  , "https://upload.wikimedia.org/wikipedia/en/e/e6/DC_Power_FC_logo.png"                                                                             ,
#   "JAX"              , "#00ADEE" , "#FFAC3E"  , "https://upload.wikimedia.org/wikipedia/commons/3/36/Sporting_Club_Jacksonville_primary_crest.png"                                                ,
#   "CAR"              , "#002256" , "#445CC7"  , "https://upload.wikimedia.org/wikipedia/en/7/7b/Carolina_Ascent_logo.png"
# )

usl_sl_team_brands <- tribble(
  ~team_abbreviation , ~primary  , ~secondary , ~crest_url                                                                                                                                        ,
  "BKN"              , "#C5BFAE" , "#0B0B0C"  , "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0c/Brooklyn_FC_logo.svg/250px-Brooklyn_FC_logo.svg.png"                                   ,
  "TB"               , "#FBDE40" , "#141B4D"  , "https://upload.wikimedia.org/wikipedia/en/thumb/4/43/Tampa_Bay_Sun_FC_logo.png/250px-Tampa_Bay_Sun_FC_logo.png"                                  ,
  "DAL"              , "#54052B" , "#B39262"  , "https://upload.wikimedia.org/wikipedia/en/thumb/4/43/Dallas_Trinity_FC_logo.png/250px-Dallas_Trinity_FC_logo.png"                                ,
  "FTL"              , "#A7E6D7" , "#003087"  , "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/Fort_Lauderdale_United_FC_logo_2024.png/250px-Fort_Lauderdale_United_FC_logo_2024.png" ,
  "LEX"              , "#00FF1E" , "#004205"  , "https://upload.wikimedia.org/wikipedia/en/thumb/1/18/Lexington_SC_logo.png/120px-Lexington_SC_logo.png"                                          ,
  "SPK"              , "#dba149" , "#0092af"  , "https://upload.wikimedia.org/wikipedia/commons/e/e5/Spokane_Zephyr_FC_logo.png"                                                                  ,
  "DC"               , "#147bd1" , "#000000"  , "https://upload.wikimedia.org/wikipedia/en/e/e6/DC_Power_FC_logo.png"                                                                             ,
  "JAX"              , "#00ADEE" , "#FFAC3E"  , "https://upload.wikimedia.org/wikipedia/commons/3/36/Sporting_Club_Jacksonville_primary_crest.png"                                                ,
  "CAR"              , "#F79400" , "#445CC7"  , "https://upload.wikimedia.org/wikipedia/en/7/7b/Carolina_Ascent_logo.png"
)

calculate_team_strengths <- function(completed_games) {
  dt <- lazy_dt(completed_games)

  home <- dt %>%
    group_by(team = home_team_id) %>%
    summarize(
      goals_for = sum(home_goals),
      goals_against = sum(away_goals),
      games = n(),
      .groups = "drop"
    ) %>%
    as_tibble()

  away <- dt %>%
    group_by(team = away_team_id) %>%
    summarize(
      goals_for = sum(away_goals),
      goals_against = sum(home_goals),
      games = n(),
      .groups = "drop"
    ) %>%
    as_tibble()

  team_stats <- bind_rows(home, away) %>%
    group_by(team) %>%
    summarize(
      goals_for = sum(goals_for),
      goals_against = sum(goals_against),
      games = sum(games),
      .groups = "drop"
    ) %>%
    mutate(
      attack_strength = goals_for / games,
      defense_strength = goals_against / games
    )

  return(team_stats)
}

# Time-weighted, opponent-adjusted attack/defense strength (Maher 1982 /
# Dixon & Coles 1997), as an alternative to calculate_team_strengths()'s raw
# per-game averages. Fits a weighted Poisson log-linear model
# (goals ~ team + opponent + home) with sum-to-zero contrasts, so a team's
# rating reflects the opponents it actually faced, and -- when xi > 0 --
# recent results count more than early-season ones
# (weight = exp(-xi * days_since_match)).
#
# Returns attack_strength/defense_strength on the SAME scale
# calculate_team_strengths() uses (an "expected goals for/against per game
# against a league-average opponent" reading), so it's a drop-in replacement
# anywhere team_strengths is consumed -- the existing multiplicative
# home_xg/away_xg formulas in simulate_matches_vectorized() /
# simulate_season_vectorized() don't need to change.
#
# Falls back to calculate_team_strengths() when there isn't enough data to
# fit (early season) or the GLM fails to converge -- this matters a lot for a
# 9-team, ~26-game league where the first few gameweeks are extremely thin.
# Also clamps its output (see MAX_STRENGTH below) as defense-in-depth: with
# too little data a team can hit near-perfect separation in the Poisson GLM,
# sending its fitted coefficient -- and therefore attack_strength =
# exp(intercept + attack_eff) -- toward astronomical values (observed in
# practice: exp() overflow on day-1-of-season backfill cutoffs with only a
# handful of games played).
fit_team_strengths_dc <- function(completed_games, xi = 0, as_of_date = NULL) {
  # Total match count alone isn't enough per-team coverage for 8-9 teams --
  # require roughly 2 appearances/team on average (nrow is match count, each
  # match is 2 team-appearances, so nrow >= n_teams_seen is ~2 apps/team).
  n_teams_seen <- length(unique(c(completed_games$home_team_id, completed_games$away_team_id)))
  if (nrow(completed_games) < 4 || n_teams_seen < 2 || nrow(completed_games) < n_teams_seen) {
    return(calculate_team_strengths(completed_games))
  }

  as_of_date <- if (is.null(as_of_date)) {
    max(completed_games$date)
  } else {
    as_of_date
  }

  long <- bind_rows(
    completed_games %>%
      transmute(
        team = home_team_id,
        opponent = away_team_id,
        goals = home_goals,
        home = 1,
        date = date
      ),
    completed_games %>%
      transmute(
        team = away_team_id,
        opponent = home_team_id,
        goals = away_goals,
        home = 0,
        date = date
      )
  ) %>%
    mutate(
      w = if (xi > 0) exp(-xi * as.numeric(as_of_date - date)) else 1,
      team = factor(team)
    )
  long$opponent <- factor(long$opponent, levels = levels(long$team))

  n_teams <- nlevels(long$team)
  if (n_teams < 2) {
    return(calculate_team_strengths(completed_games))
  }

  contrasts(long$team) <- contr.sum(n_teams)
  contrasts(long$opponent) <- contr.sum(n_teams)

  fit <- tryCatch(
    # goals can be non-integer here when xG blending (feature_flags$xg_blend_weight)
    # is active -- suppress glm's "non-integer counts" warning for that
    # expected case; convergence is still checked explicitly below.
    suppressWarnings(glm(
      goals ~ team + opponent + home,
      family = poisson(),
      weights = w,
      data = long
    )),
    error = function(e) NULL
  )

  if (is.null(fit) || !isTRUE(fit$converged)) {
    return(calculate_team_strengths(completed_games))
  }

  team_levels <- levels(long$team)
  cf <- coef(fit)
  intercept <- unname(cf["(Intercept)"])

  extract_sum_contrast_effects <- function(prefix) {
    effs <- numeric(n_teams)
    for (i in seq_len(n_teams - 1)) {
      nm <- paste0(prefix, i)
      effs[i] <- if (nm %in% names(cf)) unname(cf[[nm]]) else 0
    }
    effs[n_teams] <- -sum(effs[seq_len(n_teams - 1)])
    effs
  }

  attack_eff <- extract_sum_contrast_effects("team")
  defense_eff <- extract_sum_contrast_effects("opponent")

  games_played <- long %>%
    mutate(team = as.character(team)) %>%
    count(team, name = "games")

  # Convert the GLM's log-linear home coefficient into an additive constant
  # on the same units as the legacy flat +0.3, so it can be dropped straight
  # into the existing home_xg = attack*defense/league_avg + home_advantage
  # formula: league_avg * (exp(home_coef) - 1) is the additive shift that
  # approximates the same multiplicative boost at league-average scale.
  home_coef <- if ("home" %in% names(cf)) unname(cf[["home"]]) else 0
  league_avg <- exp(intercept)
  fitted_home_advantage <- league_avg * (exp(home_coef) - 1)

  # Safety clamp against GLM separation blowup (see function-level comment
  # above) -- not a modeling choice, just bounds generously above/below
  # anything a real match could produce so a numerically pathological fit
  # can't propagate into downstream Postgres NUMERIC(5,3) overflow or
  # nonsensical simulated scorelines.
  MAX_STRENGTH <- 10
  MIN_STRENGTH <- 0.05
  MAX_HOME_ADVANTAGE <- 2

  fitted_home_advantage <- min(max(fitted_home_advantage, -MAX_HOME_ADVANTAGE), MAX_HOME_ADVANTAGE)

  result <- tibble(
    team = team_levels,
    attack_strength = pmin(pmax(exp(intercept + attack_eff), MIN_STRENGTH), MAX_STRENGTH),
    defense_strength = pmin(pmax(exp(intercept + defense_eff), MIN_STRENGTH), MAX_STRENGTH)
  ) %>%
    left_join(games_played, by = "team") %>%
    mutate(games = coalesce(games, 0L))

  attr(result, "home_advantage") <- fitted_home_advantage
  result
}

# Empirical-Bayes shrinkage: blends each team's attack/defense rating toward
# the league-average prior, weighted by games played (games / (games + k)),
# instead of the hard "0 games -> exactly league average" cutover used
# elsewhere. Smooths small-sample swings early in a 9-team, ~26-game season
# (e.g. an opening 4-0 win no longer swings a team's rating to the extreme).
# No-ops if `team_strengths` has no `games` column (the zero-games-played
# fallback tibble in calculate_playoff_odds_fast()) or k isn't set.
apply_shrinkage <- function(team_strengths, k) {
  if (is.null(k) || is.na(k) || k <= 0 || !("games" %in% names(team_strengths))) {
    return(team_strengths)
  }

  league_attack <- mean(team_strengths$attack_strength, na.rm = TRUE)
  league_defense <- mean(team_strengths$defense_strength, na.rm = TRUE)

  team_strengths %>%
    mutate(
      shrink_w = games / (games + k),
      attack_strength = shrink_w * attack_strength + (1 - shrink_w) * league_attack,
      defense_strength = shrink_w * defense_strength + (1 - shrink_w) * league_defense
    ) %>%
    select(-shrink_w)
}

# Best-effort match-level xG blend: blends actual goals with ASA's match xG
# when available for USL Super League, since goals are a noisy low-count
# signal and this league's samples stay thin most of the season. ASA's
# get_game_xgoals() column names aren't publicly documented, so this matches
# common naming patterns defensively and falls back to goals-only (no
# blending) if the API call fails, returns nothing, or the schema doesn't
# match what's expected -- coverage for USL SL specifically isn't guaranteed
# the way it is for MLS/NWSL.
blend_actual_and_xg <- function(played_games, xg_weight, season = Sys.getenv("USL_SEASON", "2025-26")) {
  if (is.null(xg_weight) || is.na(xg_weight) || xg_weight <= 0) {
    return(played_games)
  }

  tryCatch(
    {
      xg <- suppressMessages(
        asa_client$get_game_xgoals(leagues = "usls", season_name = season)
      ) %>%
        as_tibble()

      if (nrow(xg) == 0) stop("empty xG response")

      home_xg_col <- intersect(c("home_team_xgoals", "home_xgoals", "home_xg"), names(xg))[1]
      away_xg_col <- intersect(c("away_team_xgoals", "away_xgoals", "away_xg"), names(xg))[1]
      date_col <- intersect(c("date_time_utc", "date"), names(xg))[1]

      if (is.na(home_xg_col) || is.na(away_xg_col) || is.na(date_col)) {
        stop("unrecognized get_game_xgoals() schema")
      }

      xg_slim <- xg %>%
        transmute(
          home_team_id,
          away_team_id,
          date = as.Date(.data[[date_col]]),
          home_xg_actual = .data[[home_xg_col]],
          away_xg_actual = .data[[away_xg_col]]
        )

      blended <- played_games %>%
        left_join(xg_slim, by = c("home_team_id", "away_team_id", "date")) %>%
        mutate(
          matched = !is.na(home_xg_actual) & !is.na(away_xg_actual),
          home_goals = if_else(
            is.na(home_xg_actual), home_goals,
            (1 - xg_weight) * home_goals + xg_weight * home_xg_actual
          ),
          away_goals = if_else(
            is.na(away_xg_actual), away_goals,
            (1 - xg_weight) * away_goals + xg_weight * away_xg_actual
          )
        )

      n_matched <- sum(blended$matched)
      # A join that silently matches 0 rows (e.g. a date-format/timezone
      # mismatch between ASA's xG endpoint and get_games()) would otherwise
      # succeed without error while blending nothing -- make that visible.
      message(sprintf(
        "xG blend: matched %d/%d played games (weight = %.2f).",
        n_matched, nrow(played_games), xg_weight
      ))
      if (n_matched == 0) stop("xG data returned but 0 rows matched played_games on team/date")

      blended %>% select(-home_xg_actual, -away_xg_actual, -matched)
    },
    error = function(e) {
      message(sprintf("xG blend unavailable (%s) -- using goals only.", conditionMessage(e)))
      played_games
    }
  )
}

# Dixon & Coles (1997) low-score correlation correction. Independent Poisson
# systematically misses the empirical correlation between home/away goals at
# low scores -- this reweights the 4 affected cells (0-0, 1-1 overrepresented
# in real data; 1-0, 0-1 underrepresented) by a small correlation parameter
# rho, typically negative when fit.
dc_tau <- function(x, y, lambda, mu, rho) {
  ifelse(
    x == 0 & y == 0, 1 - lambda * mu * rho,
    ifelse(
      x == 0 & y == 1, 1 + lambda * rho,
      ifelse(
        x == 1 & y == 0, 1 + mu * rho,
        ifelse(x == 1 & y == 1, 1 - rho, 1)
      )
    )
  )
}

# Fits rho by maximum likelihood against completed_games, holding the
# already-fitted per-match lambda/mu (home/away expected goals, from
# team_strengths -- typically fit_team_strengths_dc()'s output) fixed. This
# is the standard two-step Dixon-Coles procedure: fit goal rates first, then
# fit the correlation term conditional on them (only the tau factor depends
# on rho, so this reduces to maximizing sum(w * log(tau)) over historical
# scorelines).
estimate_dc_rho <- function(completed_games, team_strengths, home_advantage = 0.3, xi = 0, as_of_date = NULL) {
  if (nrow(completed_games) < 10) {
    return(0)
  }

  as_of_date <- if (is.null(as_of_date)) max(completed_games$date) else as_of_date

  raw_avg <- mean(team_strengths$attack_strength, na.rm = TRUE)
  league_avg <- if (is.na(raw_avg) || raw_avg < 0.1) 1.3 else raw_avg

  home_attack <- team_strengths$attack_strength[match(completed_games$home_team_id, team_strengths$team)]
  home_defense <- team_strengths$defense_strength[match(completed_games$home_team_id, team_strengths$team)]
  away_attack <- team_strengths$attack_strength[match(completed_games$away_team_id, team_strengths$team)]
  away_defense <- team_strengths$defense_strength[match(completed_games$away_team_id, team_strengths$team)]

  # Same overflow safety cap as simulate_matches_dc() -- not a modeling choice.
  lambda <- pmin(pmax(0.01, (home_attack * away_defense / league_avg) + home_advantage), 15)
  mu <- pmin(pmax(0.01, (away_attack * home_defense / league_avg)), 15)

  w <- if (xi > 0) exp(-xi * as.numeric(as_of_date - completed_games$date)) else rep(1, nrow(completed_games))

  x <- completed_games$home_goals
  y <- completed_games$away_goals

  ok <- !is.na(lambda) & !is.na(mu)
  x <- x[ok]
  y <- y[ok]
  lambda <- lambda[ok]
  mu <- mu[ok]
  w <- w[ok]

  if (length(x) < 10) {
    return(0)
  }

  neg_ll <- function(rho) {
    tau <- pmax(dc_tau(x, y, lambda, mu, rho), 1e-6)
    -sum(w * log(tau))
  }

  # [-0.5, 0.5] comfortably covers the range rho takes in the published
  # Dixon-Coles literature; optimize() will stay away from values that make
  # tau go negative for the observed lambda/mu since neg_ll blows up there.
  optimize(neg_ll, interval = c(-0.5, 0.5))$minimum
}

# Ad-hoc diagnostic (not called from calculate_playoff_odds_fast() / the
# nightly pipeline) -- compares the ACTUAL frequency of the 4 tau-adjusted
# scorelines (0-0, 1-0, 0-1, 1-1) against what plain independent Poisson
# would predict from the fitted team strengths, with no tau correction
# applied. Published Dixon-Coles fits get a negative rho because real
# football has MORE 0-0/1-1 and FEWER 1-0/0-1 than independent Poisson
# predicts; a positive fitted rho (as happened on USL SL's first
# estimate_dc_rho() run) means this dataset showed the opposite pattern.
# This tells you whether that's genuine signal in the data or worth
# distrusting as a small-sample artifact -- run manually, e.g.:
#   diagnose_dc_tau(played_games, team_strengths_complete, home_advantage)
diagnose_dc_tau <- function(completed_games, team_strengths, home_advantage = 0.3) {
  raw_avg <- mean(team_strengths$attack_strength, na.rm = TRUE)
  league_avg <- if (is.na(raw_avg) || raw_avg < 0.1) 1.3 else raw_avg

  home_attack <- team_strengths$attack_strength[match(completed_games$home_team_id, team_strengths$team)]
  home_defense <- team_strengths$defense_strength[match(completed_games$home_team_id, team_strengths$team)]
  away_attack <- team_strengths$attack_strength[match(completed_games$away_team_id, team_strengths$team)]
  away_defense <- team_strengths$defense_strength[match(completed_games$away_team_id, team_strengths$team)]

  # Same overflow safety cap as simulate_matches_dc() -- not a modeling choice.
  lambda <- pmin(pmax(0.01, (home_attack * away_defense / league_avg) + home_advantage), 15)
  mu <- pmin(pmax(0.01, (away_attack * home_defense / league_avg)), 15)

  ok <- !is.na(lambda) & !is.na(mu)
  x <- completed_games$home_goals[ok]
  y <- completed_games$away_goals[ok]
  lambda <- lambda[ok]
  mu <- mu[ok]
  n <- length(x)

  scorelines <- list(
    "0-0" = c(0, 0),
    "1-0" = c(1, 0),
    "0-1" = c(0, 1),
    "1-1" = c(1, 1)
  )

  rows <- purrr::map_dfr(names(scorelines), function(nm) {
    sx <- scorelines[[nm]][1]
    sy <- scorelines[[nm]][2]
    actual_pct <- mean(x == sx & y == sy) * 100
    # Mean, across games, of each game's own Poisson pmf at this scoreline
    # (each game has its own lambda/mu) -- no tau correction applied here,
    # this is the plain independent-Poisson baseline being compared against.
    poisson_expected_pct <- mean(dpois(sx, lambda) * dpois(sy, mu)) * 100
    tibble(
      scoreline = nm,
      actual_pct = actual_pct,
      poisson_expected_pct = poisson_expected_pct,
      diff_pct_pts = actual_pct - poisson_expected_pct
    )
  })

  message(sprintf(
    "n = %d games. Textbook Dixon-Coles pattern (implies negative rho): POSITIVE diff_pct_pts on 0-0/1-1 (actual > Poisson-predicted) and NEGATIVE on 1-0/0-1 (actual < predicted). The reverse pattern is what a positive fitted rho is responding to here.",
    n
  ))
  rows
}

# League-level structural parameters -- home advantage and the Dixon-Coles
# rho correlation term -- describe the competition itself (how home fields
# behave, how correlated low scores are in this league), not any one
# season's specific roster. USL Super League sees near-total roster churn
# year over year, so unlike attack/defense (which stays a clean per-season
# fit -- last season's rating is not a trustworthy prior for this season's
# different players), these two are estimated by pooling every available
# completed season and weight-averaging by games played, rather than
# refitting from a single ~56-126 game season each run. (We showed via
# diagnose_dc_tau() that a single season this size isn't enough to reliably
# pin down rho on its own -- pooling multiple seasons is the fix, and this
# degrades gracefully to a single-season fit when only one season's data
# exists yet, e.g. right now.)
#
# Each season is fit independently first (its own team_strengths, its own
# home_advantage/rho estimate) so a season's own relative team quality is
# correctly accounted for before pooling -- this avoids conflating
# season-to-season roster/competitiveness differences with the structural
# parameters being estimated.
fit_pooled_league_params <- function(seasons, xi = 0) {
  per_season <- purrr::map_dfr(seasons, function(season) {
    games <- tryCatch(
      suppressMessages(asa_client$get_games(leagues = "usls", season = season)) %>%
        as_tibble() %>%
        filter(status == "FullTime") %>%
        transmute(
          home_team_id, away_team_id,
          home_goals = home_score, away_goals = away_score,
          date = as.Date(date_time_utc)
        ),
      error = function(e) {
        message(sprintf("fit_pooled_league_params: couldn't fetch season %s (%s) -- skipping.", season, conditionMessage(e)))
        tibble()
      }
    )

    if (nrow(games) < 10) {
      return(NULL)
    }

    strengths <- fit_team_strengths_dc(games, xi = xi)
    home_adv <- attr(strengths, "home_advantage")
    if (is.null(home_adv) || !is.finite(home_adv)) home_adv <- 0.3

    rho <- estimate_dc_rho(games, strengths, home_advantage = home_adv, xi = xi)

    tibble(season = season, n_games = nrow(games), home_advantage = home_adv, rho = rho)
  })

  if (is.null(per_season) || nrow(per_season) == 0) {
    message("fit_pooled_league_params: no usable seasons -- falling back to defaults (home_advantage = 0.3, rho = 0).")
    return(list(home_advantage = 0.3, rho = 0, per_season = tibble()))
  }

  message(sprintf(
    "fit_pooled_league_params: pooled %d season(s), %d total games -- home_advantage = %.3f, rho = %.4f",
    nrow(per_season), sum(per_season$n_games),
    weighted.mean(per_season$home_advantage, per_season$n_games),
    weighted.mean(per_season$rho, per_season$n_games)
  ))

  list(
    home_advantage = weighted.mean(per_season$home_advantage, per_season$n_games),
    rho = weighted.mean(per_season$rho, per_season$n_games),
    per_season = per_season
  )
}

# Samples goals for a single match from the Dixon-Coles tau-corrected joint
# distribution (rather than independent rpois draws), via categorical
# sampling over a capped scoreline grid. Used by simulate_matches_dc(); the
# parallel-simulation counterpart (simulate_season_dc()) inlines the same
# logic directly so it has no external function dependencies to ship to
# mirai workers.
sample_dc_goals <- function(home_xg, away_xg, rho, n_sims, max_goals = 10) {
  h_range <- 0:max_goals
  a_range <- 0:max_goals

  p <- outer(h_range, a_range, function(h, a) dpois(h, home_xg) * dpois(a, away_xg))
  tau_adj <- outer(h_range, a_range, dc_tau, lambda = home_xg, mu = away_xg, rho = rho)
  p <- pmax(p * tau_adj, 0)
  p <- p / sum(p)

  idx <- sample.int(length(p), size = n_sims, replace = TRUE, prob = as.vector(p))
  list(
    h = h_range[((idx - 1) %% (max_goals + 1)) + 1],
    a = a_range[((idx - 1) %/% (max_goals + 1)) + 1]
  )
}

# Dixon-Coles tau-corrected analog of simulate_matches_vectorized() -- same
# input/output shape (drop-in for get_match_probabilities() /
# get_scoreline_distributions()), but draws goals from the tau-adjusted
# joint distribution per match instead of independent Poisson.
simulate_matches_dc <- function(remaining_games, team_strengths, n_sims, home_advantage = 0.3, rho = 0, max_goals = 10) {
  n_games <- nrow(remaining_games)

  raw_avg <- mean(team_strengths$attack_strength, na.rm = TRUE)
  league_avg <- if (is.na(raw_avg) || raw_avg < 0.1) 1.3 else raw_avg

  home_attack <- team_strengths$attack_strength[match(remaining_games$home_team_id, team_strengths$team)]
  home_defense <- team_strengths$defense_strength[match(remaining_games$home_team_id, team_strengths$team)]
  away_attack <- team_strengths$attack_strength[match(remaining_games$away_team_id, team_strengths$team)]
  away_defense <- team_strengths$defense_strength[match(remaining_games$away_team_id, team_strengths$team)]

  # Upper cap is a safety net, not a modeling choice -- pathological inputs
  # (e.g. an extreme fitted strength value) could otherwise produce an xG
  # that overflows the match_probabilities table's NUMERIC(5,3) column;
  # nothing a real match produces should ever approach this.
  home_xg <- pmin(pmax(0.01, (home_attack * away_defense / league_avg) + home_advantage), 15)
  away_xg <- pmin(pmax(0.01, (away_attack * home_defense / league_avg)), 15)

  h_goals <- matrix(0L, n_sims, n_games)
  a_goals <- matrix(0L, n_sims, n_games)
  for (g in seq_len(n_games)) {
    draw <- sample_dc_goals(home_xg[g], away_xg[g], rho, n_sims, max_goals)
    h_goals[, g] <- draw$h
    a_goals[, g] <- draw$a
  }

  h_pts <- ifelse(h_goals > a_goals, 3, ifelse(h_goals == a_goals, 1, 0))
  a_pts <- ifelse(a_goals > h_goals, 3, ifelse(h_goals == a_goals, 1, 0))
  h_gd <- h_goals - a_goals
  a_gd <- a_goals - h_goals

  data.table(
    sim_id = rep(1:n_sims, each = n_games),
    match_id = rep(1:n_games, times = n_sims),
    home_team_id = rep(remaining_games$home_team_id, times = n_sims),
    away_team_id = rep(remaining_games$away_team_id, times = n_sims),
    home_goals = as.vector(t(h_goals)),
    away_goals = as.vector(t(a_goals)),
    home_points = as.vector(t(h_pts)),
    away_points = as.vector(t(a_pts)),
    home_gd = as.vector(t(h_gd)),
    away_gd = as.vector(t(a_gd)),
    home_xg = rep(home_xg, times = n_sims),
    away_xg = rep(away_xg, times = n_sims)
  )
}

# Dixon-Coles tau-corrected analog of simulate_season_vectorized(). Draws
# goals from the tau-adjusted joint distribution instead of independent
# Poisson, then aggregates final standings identically to the legacy
# function (points -> GD -> GS -> random tiebreak, same as everywhere else
# in this file -- see the plan notes on the standings tiebreaker for why a
# full head-to-head resolver isn't done at this per-sim scale).
#
# Deliberately self-contained (no calls to sample_dc_goals()/dc_tau(), which
# are only for the non-parallel match-level path) so mirai workers can run
# it without needing extra helper functions shipped to them.
simulate_season_dc <- function(
  current_standings,
  remaining_games,
  team_strengths,
  n_sims,
  home_advantage = 0.3,
  rho = 0,
  qualify_top_n = 4L,
  max_goals = 10
) {
  n_games <- nrow(remaining_games)
  all_teams <- current_standings$team
  n_teams <- length(all_teams)

  raw_avg <- mean(team_strengths$attack_strength, na.rm = TRUE)
  league_avg <- if (is.na(raw_avg) || raw_avg < 0.1) 1.3 else raw_avg

  home_attack <- team_strengths$attack_strength[match(remaining_games$home_team_id, all_teams)]
  home_defense <- team_strengths$defense_strength[match(remaining_games$home_team_id, all_teams)]
  away_attack <- team_strengths$attack_strength[match(remaining_games$away_team_id, all_teams)]
  away_defense <- team_strengths$defense_strength[match(remaining_games$away_team_id, all_teams)]

  # See simulate_matches_dc() -- same overflow safety cap, not a modeling choice.
  home_xg <- pmin(pmax(0.01, (home_attack * away_defense / league_avg) + home_advantage), 15)
  away_xg <- pmin(pmax(0.01, (away_attack * home_defense / league_avg)), 15)

  h_goals <- matrix(0L, n_sims, n_games)
  a_goals <- matrix(0L, n_sims, n_games)

  h_range <- 0:max_goals
  a_range <- 0:max_goals

  for (g in seq_len(n_games)) {
    lambda <- home_xg[g]
    mu <- away_xg[g]

    p <- outer(h_range, a_range, function(h, a) dpois(h, lambda) * dpois(a, mu))
    tau_adj <- outer(h_range, a_range, function(x, y) {
      ifelse(
        x == 0 & y == 0, 1 - lambda * mu * rho,
        ifelse(
          x == 0 & y == 1, 1 + lambda * rho,
          ifelse(
            x == 1 & y == 0, 1 + mu * rho,
            ifelse(x == 1 & y == 1, 1 - rho, 1)
          )
        )
      )
    })
    p <- pmax(p * tau_adj, 0)
    p <- p / sum(p)

    idx <- sample.int(length(p), size = n_sims, replace = TRUE, prob = as.vector(p))
    h_goals[, g] <- h_range[((idx - 1) %% (max_goals + 1)) + 1]
    a_goals[, g] <- a_range[((idx - 1) %/% (max_goals + 1)) + 1]
  }

  h_pts <- ifelse(h_goals > a_goals, 3, ifelse(h_goals == a_goals, 1, 0))
  a_pts <- ifelse(a_goals > h_goals, 3, ifelse(h_goals == a_goals, 1, 0))
  h_gd <- h_goals - a_goals

  final_pts <- matrix(current_standings$current_points, n_sims, n_teams, byrow = TRUE)
  final_gd <- matrix(0, n_sims, n_teams)
  final_gs <- matrix(0, n_sims, n_teams)

  home_cols <- match(remaining_games$home_team_id, all_teams)
  away_cols <- match(remaining_games$away_team_id, all_teams)

  for (g in seq_len(n_games)) {
    final_pts[, home_cols[g]] <- final_pts[, home_cols[g]] + h_pts[, g]
    final_pts[, away_cols[g]] <- final_pts[, away_cols[g]] + a_pts[, g]
    final_gd[, home_cols[g]] <- final_gd[, home_cols[g]] + h_gd[, g]
    final_gd[, away_cols[g]] <- final_gd[, away_cols[g]] - h_gd[, g]
    final_gs[, home_cols[g]] <- final_gs[, home_cols[g]] + h_goals[, g]
    final_gs[, away_cols[g]] <- final_gs[, away_cols[g]] + a_goals[, g]
  }

  tb_score <- (final_pts * 1e6) +
    ((final_gd + 500) * 1e3) +
    final_gs +
    matrix(runif(n_sims * n_teams), n_sims, n_teams)

  rank_matrix <- t(apply(tb_score, 1, function(x) rank(-x, ties.method = "random")))

  data.table(
    sim_id = rep(1:n_sims, times = n_teams),
    team = rep(all_teams, each = n_sims),
    points = as.vector(final_pts),
    rank = as.integer(as.vector(rank_matrix)),
    made_playoffs = as.vector(rank_matrix) <= as.integer(qualify_top_n)
  )
}

simulate_matches_vectorized <- function(
  remaining_games,
  team_strengths,
  n_sims,
  home_advantage = 0.3
) {
  n_games <- nrow(remaining_games)

  raw_avg <- mean(team_strengths$attack_strength, na.rm = TRUE)
  league_avg <- if (is.na(raw_avg) || raw_avg < 0.1) 1.3 else raw_avg

  home_attack <- team_strengths$attack_strength[match(
    remaining_games$home_team_id,
    team_strengths$team
  )]
  home_defense <- team_strengths$defense_strength[match(
    remaining_games$home_team_id,
    team_strengths$team
  )]
  away_attack <- team_strengths$attack_strength[match(
    remaining_games$away_team_id,
    team_strengths$team
  )]
  away_defense <- team_strengths$defense_strength[match(
    remaining_games$away_team_id,
    team_strengths$team
  )]

  # See simulate_matches_dc() -- same overflow safety cap, not a modeling choice.
  home_xg <- pmin(pmax(
    0.01,
    (home_attack * away_defense / league_avg) + home_advantage
  ), 15)
  away_xg <- pmin(pmax(0.01, (away_attack * home_defense / league_avg)), 15)

  h_goals <- matrix(
    rpois(n_sims * n_games, rep(home_xg, each = n_sims)),
    n_sims,
    n_games
  )
  a_goals <- matrix(
    rpois(n_sims * n_games, rep(away_xg, each = n_sims)),
    n_sims,
    n_games
  )

  h_pts <- ifelse(h_goals > a_goals, 3, ifelse(h_goals == a_goals, 1, 0))
  a_pts <- ifelse(a_goals > h_goals, 3, ifelse(h_goals == a_goals, 1, 0))

  h_gd <- h_goals - a_goals
  a_gd <- a_goals - h_goals

  match_results <- data.table(
    sim_id = rep(1:n_sims, each = n_games),
    match_id = rep(1:n_games, times = n_sims),
    home_team_id = rep(remaining_games$home_team_id, times = n_sims),
    away_team_id = rep(remaining_games$away_team_id, times = n_sims),
    home_goals = as.vector(t(h_goals)),
    away_goals = as.vector(t(a_goals)),
    home_points = as.vector(t(h_pts)),
    away_points = as.vector(t(a_pts)),
    home_gd = as.vector(t(h_gd)),
    away_gd = as.vector(t(a_gd)),
    home_xg = rep(home_xg, times = n_sims),
    away_xg = rep(away_xg, times = n_sims)
  )

  return(match_results)
}

aggregate_season_results <- function(
  match_results,
  current_standings,
  n_sims,
  qualify_top_n = 4
) {
  all_teams <- current_standings$team

  home_agg <- match_results %>%
    lazy_dt() %>%
    group_by(sim_id, team = home_team_id) %>%
    summarize(
      points = sum(home_points),
      gd = sum(home_gd),
      gs = sum(home_goals),
      .groups = "drop"
    ) %>%
    as_tibble()

  away_agg <- match_results %>%
    lazy_dt() %>%
    group_by(sim_id, team = away_team_id) %>%
    summarize(
      points = sum(away_points),
      gd = sum(away_gd),
      gs = sum(away_goals),
      .groups = "drop"
    ) %>%
    as_tibble()

  season_results <- bind_rows(home_agg, away_agg) %>%
    group_by(sim_id, team) %>%
    summarize(
      sim_points = sum(points),
      sim_gd = sum(gd),
      sim_gs = sum(gs),
      .groups = "drop"
    ) %>%
    left_join(
      current_standings %>% select(team, current_points),
      by = "team"
    ) %>%
    mutate(
      current_points = replace_na(current_points, 0),
      final_points = current_points + sim_points,
      final_gd = sim_gd,
      final_gs = sim_gs
    )

  season_results <- season_results %>%
    group_by(sim_id) %>%
    mutate(
      tb_score = (final_points * 1e6) +
        ((final_gd + 500) * 1e3) +
        final_gs +
        runif(n()),
      rank = rank(-tb_score, ties.method = "random"),
      made_playoffs = rank <= qualify_top_n
    ) %>%
    ungroup() %>%
    select(sim_id, team, final_points, final_gd, final_gs, rank, made_playoffs)

  return(season_results)
}

simulate_season_vectorized <- function(
  current_standings,
  remaining_games,
  team_strengths,
  n_sims,
  home_advantage = 0.3,
  qualify_top_n = 4
) {
  n_games <- nrow(remaining_games)
  all_teams <- current_standings$team
  n_teams <- length(all_teams)

  raw_avg <- mean(team_strengths$attack_strength, na.rm = TRUE)
  league_avg <- if (is.na(raw_avg) || raw_avg < 0.1) 1.3 else raw_avg

  home_attack <- team_strengths$attack_strength[match(
    remaining_games$home_team_id,
    all_teams
  )]
  home_defense <- team_strengths$defense_strength[match(
    remaining_games$home_team_id,
    all_teams
  )]
  away_attack <- team_strengths$attack_strength[match(
    remaining_games$away_team_id,
    all_teams
  )]
  away_defense <- team_strengths$defense_strength[match(
    remaining_games$away_team_id,
    all_teams
  )]

  # See simulate_matches_dc() -- same overflow safety cap, not a modeling choice.
  home_xg <- pmin(pmax(
    0.01,
    (home_attack * away_defense / league_avg) + home_advantage
  ), 15)
  away_xg <- pmin(pmax(0.01, (away_attack * home_defense / league_avg)), 15)

  h_goals <- matrix(
    rpois(n_sims * n_games, rep(home_xg, each = n_sims)),
    n_sims,
    n_games
  )
  a_goals <- matrix(
    rpois(n_sims * n_games, rep(away_xg, each = n_sims)),
    n_sims,
    n_games
  )

  h_pts <- ifelse(h_goals > a_goals, 3, ifelse(h_goals == a_goals, 1, 0))
  a_pts <- ifelse(a_goals > h_goals, 3, ifelse(h_goals == a_goals, 1, 0))

  h_gd <- h_goals - a_goals
  a_gd <- a_goals - h_goals

  final_pts <- matrix(
    current_standings$current_points,
    n_sims,
    n_teams,
    byrow = TRUE
  )
  final_gd <- matrix(0, n_sims, n_teams)
  final_gs <- matrix(0, n_sims, n_teams)

  home_cols <- match(remaining_games$home_team_id, all_teams)
  away_cols <- match(remaining_games$away_team_id, all_teams)

  for (g in 1:n_games) {
    final_pts[, home_cols[g]] <- final_pts[, home_cols[g]] + h_pts[, g]
    final_pts[, away_cols[g]] <- final_pts[, away_cols[g]] + a_pts[, g]
    final_gd[, home_cols[g]] <- final_gd[, home_cols[g]] + h_gd[, g]
    final_gd[, away_cols[g]] <- final_gd[, away_cols[g]] + a_gd[, g]
    final_gs[, home_cols[g]] <- final_gs[, home_cols[g]] + h_goals[, g]
    final_gs[, away_cols[g]] <- final_gs[, away_cols[g]] + a_goals[, g]
  }

  tb_score <- (final_pts * 1e6) +
    ((final_gd + 500) * 1e3) +
    (final_gs) +
    matrix(runif(n_sims * n_teams), n_sims, n_teams)

  rank_matrix <- t(apply(tb_score, 1, function(x) {
    rank(-x, ties.method = "random")
  }))

  results <- data.table(
    sim_id = rep(1:n_sims, times = n_teams),
    team = rep(all_teams, each = n_sims),
    points = as.vector(final_pts),
    rank = as.integer(as.vector(rank_matrix)),
    made_playoffs = as.vector(rank_matrix) <= as.integer(qualify_top_n)
  )

  return(results)
}

calculate_playoff_odds_fast <- function(
  schedule_obj,
  n_sims = 10000,
  n_cores = 6,
  qualify_top_n = 4,
  feature_flags = list(),
  league_params = NULL,
  season = Sys.getenv("USL_SEASON", "2025-26")
) {
  message(sprintf("Fetching official scores from ASA API for season %s...", season))
  asa_games <- suppressMessages(asa_client$get_games(
    leagues = 'usls',
    season = season
  )) %>%
    lazy_dt() %>%
    mutate(date_only = as.Date(date_time_utc)) %>%
    as_tibble()

  schedule_mapped <- schedule_obj %>%
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
    lazy_dt() %>%
    filter(status == "FullTime") %>%
    select(
      home_team_id,
      away_team_id,
      home_goals = home_score,
      away_goals = away_score,
      date = date_only
    ) %>%
    as_tibble()

  remaining_games <- schedule_mapped %>%
    filter(is_completed == FALSE) %>%
    anti_join(played_games, by = c("home_team_id", "away_team_id", "date")) %>%
    mutate(match_id = row_number())

  all_team_ids <- unique(c(
    schedule_mapped$home_team_id,
    schedule_mapped$away_team_id
  ))

  run_simulation_pipeline(
    played_games, remaining_games, all_team_ids,
    n_sims, n_cores, qualify_top_n, feature_flags, league_params, season
  )
}

# Backfill/historical variant of calculate_playoff_odds_fast(): instead of
# live-fetching "now" from ASA + FotMob's current schedule (which only
# exists for whichever season is presently underway -- meaningless for a
# fully completed past season like 2024-25), this takes an already-fetched
# full-season match list (both played AND future/scheduled games, e.g. from
# asa_client$get_games(season = X) directly, no FotMob involved) plus an
# explicit historical cutoff_date, and simulates "as of" that date. Shares
# run_simulation_pipeline() with the live function so backfilled and live
# numbers come from identical simulation logic -- only how played/remaining
# games are determined differs.
calculate_playoff_odds_at_cutoff <- function(
  all_results,
  cutoff_date,
  all_team_ids,
  n_sims = 10000,
  n_cores = 6,
  qualify_top_n = 4,
  feature_flags = list(),
  league_params = NULL,
  season = NULL
) {
  played_games <- all_results %>%
    filter(status == "FullTime", date <= cutoff_date) %>%
    select(home_team_id, away_team_id, home_goals, away_goals, date)

  # "Remaining" = not already resolved as of cutoff_date -- covers both
  # future-dated games and any game dated on/before cutoff_date that wasn't
  # actually completed (postponement, etc.), not just a simple date filter.
  remaining_games <- all_results %>%
    filter(!(status == "FullTime" & date <= cutoff_date)) %>%
    select(home_team_id, away_team_id, date) %>%
    mutate(match_id = row_number())

  message(sprintf(
    "Cutoff %s: %d games played, %d remaining.",
    cutoff_date, nrow(played_games), nrow(remaining_games)
  ))

  run_simulation_pipeline(
    played_games, remaining_games, all_team_ids,
    n_sims, n_cores, qualify_top_n, feature_flags, league_params, season
  )
}

# Shared simulation core for calculate_playoff_odds_fast() (live) and
# calculate_playoff_odds_at_cutoff() (backfill) -- everything from strength
# fitting through match-level simulation and output shaping, independent of
# how played_games/remaining_games were determined.
run_simulation_pipeline <- function(
  played_games,
  remaining_games,
  all_team_ids,
  n_sims,
  n_cores,
  qualify_top_n,
  feature_flags,
  league_params,
  season
) {
  fitted_home_advantage <- NULL

  if (nrow(played_games) > 0) {
    # xG blending only feeds the strength fit -- standings below always use
    # the real played_games so points/current_points reflect actual results.
    played_games_for_strength <- if (!is.null(feature_flags$xg_blend_weight)) {
      blend_actual_and_xg(played_games, feature_flags$xg_blend_weight, season = season)
    } else {
      played_games
    }

    team_strengths <- if (!is.null(feature_flags$time_decay_xi)) {
      fit_team_strengths_dc(played_games_for_strength, xi = feature_flags$time_decay_xi)
    } else {
      calculate_team_strengths(played_games_for_strength)
    }

    # Capture before any further dplyr piping, which won't preserve this
    # custom attribute.
    fitted_home_advantage <- attr(team_strengths, "home_advantage")

    if (!is.null(feature_flags$shrinkage_k)) {
      team_strengths <- apply_shrinkage(team_strengths, feature_flags$shrinkage_k)
    }

    home_standings <- played_games %>%
      lazy_dt() %>%
      group_by(team = home_team_id) %>%
      summarize(
        pts = sum(if_else(
          home_goals > away_goals,
          3,
          if_else(home_goals == away_goals, 1, 0)
        )),
        games = n(),
        .groups = "drop"
      ) %>%
      as_tibble()

    away_standings <- played_games %>%
      lazy_dt() %>%
      group_by(team = away_team_id) %>%
      summarize(
        pts = sum(if_else(
          away_goals > home_goals,
          3,
          if_else(home_goals == away_goals, 1, 0)
        )),
        games = n(),
        .groups = "drop"
      ) %>%
      as_tibble()

    standings_raw <- bind_rows(home_standings, away_standings) %>%
      group_by(team) %>%
      summarize(
        current_points = sum(pts),
        games_played = sum(games),
        .groups = "drop"
      )
  } else {
    standings_raw <- tibble(
      team = all_team_ids,
      current_points = 0,
      games_played = 0
    )
    team_strengths <- tibble(
      team = all_team_ids,
      attack_strength = 1.3,
      defense_strength = 1.3
    )
  }

  current_standings <- tibble(team = all_team_ids) %>%
    left_join(standings_raw, by = "team") %>%
    mutate(
      current_points = if_else(is.na(current_points), 0, current_points),
      games_played = if_else(is.na(games_played), 0L, games_played)
    )

  team_strengths_complete <- tibble(team = all_team_ids) %>%
    left_join(team_strengths, by = "team") %>%
    mutate(
      l_avg = mean(attack_strength, na.rm = TRUE),
      attack_strength = if_else(is.na(attack_strength), l_avg, attack_strength),
      defense_strength = if_else(
        is.na(defense_strength),
        l_avg,
        defense_strength
      )
    ) %>%
    select(-l_avg)

  message(sprintf(
    "%d games played, %d remaining.",
    nrow(played_games),
    nrow(remaining_games)
  ))

  # ── Home advantage & Dixon-Coles rho ────────────────────────────────────────
  # These are league-structural parameters, not roster-specific ones (see
  # fit_pooled_league_params()) -- prefer a pooled multi-season estimate when
  # the caller supplies one (playoff_runner.R computes this once per run,
  # not per preset). Fall back to fitting from the current season alone only
  # when no pooled estimate is available, e.g. ad hoc calls or backfill_runner.R.
  home_advantage <- 0.3
  if (isTRUE(feature_flags$fitted_home_advantage)) {
    if (!is.null(league_params$home_advantage) && is.finite(league_params$home_advantage)) {
      home_advantage <- league_params$home_advantage
    } else if (!is.null(fitted_home_advantage) && is.finite(fitted_home_advantage)) {
      home_advantage <- fitted_home_advantage
    }
  }

  rho <- 0
  use_dc_sim <- isTRUE(feature_flags$dixon_coles_tau)
  if (use_dc_sim) {
    if (!is.null(league_params$rho) && is.finite(league_params$rho)) {
      rho <- league_params$rho
      message(sprintf("Dixon-Coles rho (pooled across seasons): %.4f", rho))
    } else if (nrow(played_games) > 0) {
      rho <- estimate_dc_rho(
        played_games,
        team_strengths_complete,
        home_advantage = home_advantage,
        xi = if (!is.null(feature_flags$time_decay_xi)) feature_flags$time_decay_xi else 0
      )
      message(sprintf("Dixon-Coles rho (single-season fit, no pooled estimate supplied): %.4f", rho))
    }
  }

  daemons(n_cores)
  everywhere({
    library(data.table)
  })

  sims_per_worker <- ceiling(n_sims / n_cores)

  playoff_results <- map(
    1:n_cores,
    in_parallel(
      function(i) {
        set.seed(as.integer(Sys.time()) + i)
        this_n <- if (i == n_cores) {
          n_sims - (sims_per_worker * (n_cores - 1))
        } else {
          sims_per_worker
        }
        if (use_dc_sim) {
          simulate_season_dc(
            current_standings,
            remaining_games,
            team_strengths_complete,
            this_n,
            home_advantage = home_advantage,
            rho = rho,
            qualify_top_n = qualify_top_n
          )
        } else {
          simulate_season_vectorized(
            current_standings,
            remaining_games,
            team_strengths_complete,
            this_n,
            home_advantage = home_advantage,
            qualify_top_n = qualify_top_n
          )
        }
      },
      current_standings = current_standings,
      remaining_games = remaining_games,
      team_strengths_complete = team_strengths_complete,
      sims_per_worker = sims_per_worker,
      n_sims = n_sims,
      n_cores = n_cores,
      qualify_top_n = qualify_top_n,
      home_advantage = home_advantage,
      rho = rho,
      use_dc_sim = use_dc_sim,
      simulate_season_vectorized = simulate_season_vectorized,
      simulate_season_dc = simulate_season_dc
    ),
    .progress = TRUE
  ) %>%
    bind_rows()

  daemons(0)

  summary <- playoff_results %>%
    lazy_dt() %>%
    group_by(team) %>%
    summarize(
      playoff_pct = mean(made_playoffs) * 100,
      avg_pts = mean(points),
      .groups = "drop"
    ) %>%
    as_tibble()

  final_tab <- summary %>%
    left_join(current_standings, by = "team") %>%
    left_join(
      teams %>% select(team_id, team_name, team_abbreviation),
      by = c("team" = "team_id")
    ) %>%
    arrange(desc(playoff_pct))

  rank_dist <- playoff_results %>%
    lazy_dt() %>%
    group_by(team, rank) %>%
    summarize(count = n(), .groups = "drop") %>%
    as_tibble() %>%
    mutate(pct = count / n_sims) %>%
    left_join(
      teams %>% select(team_id, team_abbreviation),
      by = c("team" = "team_id")
    )

  cutoff_dist <- playoff_results %>%
    lazy_dt() %>%
    filter(rank == as.integer(qualify_top_n)) %>%
    group_by(points) %>%
    summarize(count = n(), .groups = "drop") %>%
    as_tibble() %>%
    mutate(pct = count / n_sims)

  # Single match-level simulation — shared by match probs and scoreline distributions
  message("Running match-level simulation...")
  match_results <- if (use_dc_sim) {
    simulate_matches_dc(
      remaining_games,
      team_strengths_complete,
      n_sims,
      home_advantage = home_advantage,
      rho = rho
    )
  } else {
    simulate_matches_vectorized(
      remaining_games,
      team_strengths_complete,
      n_sims,
      home_advantage = home_advantage
    )
  }

  match_probs <- get_match_probabilities(match_results, remaining_games, teams)
  scoreline_dist <- get_scoreline_distributions(
    match_results,
    remaining_games,
    teams,
    n_sims
  )

  return(list(
    summary = final_tab,
    raw = as.data.table(playoff_results),
    match_probs = match_probs,
    scoreline_dist = scoreline_dist,
    rank_dist = rank_dist,
    cutoff_dist = cutoff_dist,
    played_games = played_games,
    remaining_games = remaining_games
  ))
}

get_match_probabilities <- function(
  match_results,
  remaining_games,
  teams_info
) {
  match_probs <- match_results %>%
    lazy_dt() %>%
    group_by(match_id, home_team_id, away_team_id, home_xg, away_xg) %>%
    summarize(
      home_win_pct = mean(home_points == 3) * 100,
      draw_pct = mean(home_points == 1) * 100,
      away_win_pct = mean(away_points == 3) * 100,
      avg_home_goals = mean(home_goals),
      avg_away_goals = mean(away_goals),
      .groups = "drop"
    ) %>%
    as_tibble() %>%
    left_join(
      teams_info %>% select(team_id, home_team = team_abbreviation),
      by = c("home_team_id" = "team_id")
    ) %>%
    left_join(
      teams_info %>% select(team_id, away_team = team_abbreviation),
      by = c("away_team_id" = "team_id")
    ) %>%
    select(
      match_id,
      home_team,
      away_team,
      home_xg,
      away_xg,
      home_win_pct,
      draw_pct,
      away_win_pct,
      avg_home_goals,
      avg_away_goals
    )

  return(match_probs)
}

get_team_path_to_playoffs <- function(
  team_abbr,
  remaining_games,
  team_strengths,
  teams_info,
  n_sims = 10000
) {
  team_id <- teams_info %>%
    filter(team_abbreviation == team_abbr) %>%
    pull(team_id)

  team_matches <- remaining_games %>%
    mutate(match_id = row_number()) %>%
    filter(home_team_id == team_id | away_team_id == team_id)

  if (nrow(team_matches) == 0) {
    return(tibble(message = "No remaining matches"))
  }

  match_results <- simulate_matches_vectorized(
    team_matches,
    team_strengths,
    n_sims
  )

  team_schedule <- match_results %>%
    lazy_dt() %>%
    mutate(
      is_home = home_team_id == team_id,
      opponent_id = if_else(is_home, away_team_id, home_team_id),
      team_goals = if_else(is_home, home_goals, away_goals),
      opp_goals = if_else(is_home, away_goals, home_goals),
      team_points = if_else(is_home, home_points, away_points),
      result = case_when(
        team_points == 3 ~ "Win",
        team_points == 1 ~ "Draw",
        TRUE ~ "Loss"
      )
    ) %>%
    group_by(match_id, is_home, opponent_id) %>%
    summarize(
      win_pct = mean(result == "Win") * 100,
      draw_pct = mean(result == "Draw") * 100,
      loss_pct = mean(result == "Loss") * 100,
      avg_team_goals = mean(team_goals),
      avg_opp_goals = mean(opp_goals),
      expected_points = mean(team_points),
      .groups = "drop"
    ) %>%
    as_tibble() %>%
    left_join(
      teams_info %>% select(team_id, opponent = team_abbreviation),
      by = c("opponent_id" = "team_id")
    ) %>%
    mutate(
      location = if_else(is_home, "vs", "@"),
      matchup = paste(location, opponent)
    ) %>%
    arrange(match_id) %>%
    select(
      match_id,
      matchup,
      win_pct,
      draw_pct,
      loss_pct,
      expected_points,
      avg_team_goals,
      avg_opp_goals
    )

  return(team_schedule)
}

get_scoreline_distributions <- function(
  match_results,
  remaining_games,
  teams_info,
  n_sims,
  max_goals = 5
) {
  match_results %>%
    lazy_dt() %>%
    mutate(
      home_goals_capped = pmin(home_goals, max_goals),
      away_goals_capped = pmin(away_goals, max_goals)
    ) %>%
    group_by(
      match_id,
      home_team_id,
      away_team_id,
      home_goals_capped,
      away_goals_capped
    ) %>%
    summarize(prob = n() / n_sims, .groups = "drop") %>%
    as_tibble() %>%
    left_join(
      teams_info %>% select(team_id, home_team = team_abbreviation),
      by = c("home_team_id" = "team_id")
    ) %>%
    left_join(
      teams_info %>% select(team_id, away_team = team_abbreviation),
      by = c("away_team_id" = "team_id")
    ) %>%
    mutate(
      matchup = paste0(home_team, " vs ", away_team),
      scoreline = paste0(home_goals_capped, "-", away_goals_capped)
    ) %>%
    select(
      match_id,
      matchup,
      home_team,
      away_team,
      home_goals = home_goals_capped,
      away_goals = away_goals_capped,
      scoreline,
      prob
    )
}

plot_playoff_odds <- function(odds, run) {
  odds %>%
    mutate(team_abbreviation = fct_reorder(team_abbreviation, playoff_pct)) %>%
    ggplot(aes(x = team_abbreviation, y = playoff_pct)) +
    geom_col(aes(fill = playoff_pct > 50), show.legend = FALSE) +
    geom_text(
      aes(label = sprintf("%.1f%%", playoff_pct)),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    scale_fill_manual(values = c("FALSE" = "gray70", "TRUE" = "darkgreen")) +
    scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
    labs(
      title = "USL Super League Playoff Probabilities",
      subtitle = sprintf(
        "Based on %s Monte Carlo simulations | Gameweek %s",
        format(run$n_sims, big.mark = ","),
        run$gameweek_number
      ),
      x = NULL,
      y = "Playoff Probability (%)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )
}

table_uncertainty <- function(odds, n_sims) {
  odds %>%
    mutate(
      p = playoff_pct / 100,
      se = sqrt(p * (1 - p) / n_sims),
      lower_ci = pmax(0, p - 1.96 * se),
      upper_ci = pmin(1, p + 1.96 * se)
    ) %>%
    transmute(
      Team = team_abbreviation,
      Chance = sprintf("%.2f%%", p * 100),
      `Std. Error` = sprintf("%.2f%%", se * 100),
      `95% Low` = sprintf("%.2f%%", lower_ci * 100),
      `95% High` = sprintf("%.2f%%", upper_ci * 100)
    )
}

table_match_probs <- function(match_probs) {
  match_probs %>%
    mutate(
      match_date = format(as.Date(match_date), "%b %d"),
      home_win_pct = sprintf("%.1f%%", home_win_pct),
      draw_pct = sprintf("%.1f%%", draw_pct),
      away_win_pct = sprintf("%.1f%%", away_win_pct),
      home_xg = sprintf("%.2f", home_xg),
      away_xg = sprintf("%.2f", away_xg)
    ) %>%
    select(
      Date = match_date,
      Home = home_team_abbr,
      Away = away_team_abbr,
      `Home Win` = home_win_pct,
      Draw = draw_pct,
      `Away Win` = away_win_pct,
      `Home xG` = home_xg,
      `Away xG` = away_xg
    )
}


plot_trends <- function(history, seed = 42) {
  team_levels <- history %>%
    distinct(team_abbreviation) %>%
    slice_sample(prop = 1, replace = FALSE) %>%
    pull(team_abbreviation)

  color_map <- deframe(select(usl_sl_team_brands, team_abbreviation, primary))

  gw_breaks <- history %>%
    distinct(gameweek_number, run_date) %>%
    group_by(gameweek_number) %>%
    slice_min(run_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(gameweek_number)

  history %>%
    mutate(
      team_abbreviation = factor(team_abbreviation, levels = team_levels)
    ) %>%
    ggplot(aes(
      x = run_date,
      y = playoff_pct,
      color = team_abbreviation,
      group = team_abbreviation
    )) +
    geom_line(linewidth = 1) +
    geom_point(aes(shape = played_today), size = 2) +
    geom_hline(
      yintercept = 50,
      linetype = "dashed",
      color = "gray50",
      alpha = 0.7
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      labels = scales::percent_format(scale = 1)
    ) +
    scale_x_date(
      breaks = gw_breaks$run_date,
      labels = gw_breaks$gameweek_number
    ) +
    scale_color_manual(values = color_map) +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1), guide = "none") +
    labs(
      title = "Playoff Probability Over Time",
      x = "Gameweek",
      y = "Playoff Probability (%)",
      color = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}


plot_rank_distributions <- function(
  rank_dist,
  n_ranks = 9,
  title = "Final Table Position Distributions",
  subtitle = "1,000,000 Simulated Seasons | USL Super League"
) {
  rank_dist_complete <- rank_dist |>
    tidyr::complete(
      team_abbreviation,
      rank = seq_len(n_ranks),
      fill = list(count = 0, pct = 0)
    )

  team_order <- rank_dist_complete |>
    summarise(mean_rank = weighted.mean(rank, pct), .by = team_abbreviation) |>
    arrange(desc(mean_rank)) |>
    pull(team_abbreviation)

  color_map <- deframe(select(usl_sl_team_brands, team_abbreviation, primary))

  rank_dist_complete |>
    mutate(
      team_abbreviation = factor(team_abbreviation, levels = rev(team_order))
    ) |>
    ggplot(aes(x = rank, y = pct, fill = team_abbreviation)) +
    geom_col(width = 1, alpha = 1) +
    geom_segment(
      aes(x = rank - 0.5, xend = rank + 0.5, y = pct, yend = pct),
      linewidth = 0.4
    ) +
    annotate(
      "rect",
      xmin = 0.5,
      xmax = 4.5,
      ymin = -Inf,
      ymax = Inf,
      fill = "green",
      alpha = 0.05
    ) +
    scale_fill_manual(values = color_map, guide = "none") +
    scale_x_continuous(breaks = seq_len(n_ranks)) +
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(~team_abbreviation, ncol = 1, strip.position = "left") +
    theme_ridges(grid = TRUE) +
    theme(
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.spacing = unit(0, "lines")
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = glue::glue("Final Rank (1st to {n_ranks}th)"),
      y = NULL
    )
}


plot_cutoff_distribution <- function(cutoff_dist, current_standings = NULL) {
  p <- cutoff_dist %>%
    rename(playoff_line_pts = points) %>%
    mutate(playoff_line_pts = as.integer(playoff_line_pts)) %>%
    ggplot() +
    geom_col(aes(x = playoff_line_pts, y = count)) +
    labs(
      title = "Distribution of 4th Place Point Totals",
      x = "Points Required for 4th Place",
      y = "Frequency",
      subtitle = "Sampled Over 1,000,000 Simulations"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"))

  if (!is.null(current_standings) && nrow(current_standings) >= 4) {
    fourth_pts <- sort(current_standings$current_points, decreasing = TRUE)[4]
    p <- p +
      geom_vline(
        xintercept = fourth_pts,
        color = "darkred",
        linetype = "dashed",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = fourth_pts,
        y = max(cutoff_dist$count) * 0.95,
        label = sprintf("Current 4th: %d pts", fourth_pts),
        hjust = -0.05,
        color = "darkred",
        size = 4
      )
  }
  p
}

# ── Cross-league backfill helpers ─────────────────────────────────────────────

get_wfr_results <- function(country, gender, tier, season_end_year) {
  worldfootballR::load_match_results(
    country = country,
    gender = gender,
    season_end_year = season_end_year,
    tier = tier
  ) |>
    transmute(
      home_team_id = Home,
      away_team_id = Away,
      home_goals = HomeGoals,
      away_goals = AwayGoals,
      date = as.Date(Date)
    ) |>
    filter(!is.na(home_goals))
}

get_played_at_date <- function(all_results, cutoff_date) {
  all_results |> filter(date <= cutoff_date)
}

get_remaining_at_date <- function(all_results, cutoff_date) {
  all_results |> filter(date > cutoff_date) |> mutate(match_id = row_number())
}

simulate_season_at_gameweek <- function(
  all_results,
  cutoff_date,
  qualify_top_n,
  n_sims = 1000000,
  home_advantage = 0.3,
  use_gpu = FALSE
) {
  played <- get_played_at_date(all_results, cutoff_date)
  remaining <- get_remaining_at_date(all_results, cutoff_date)

  if (nrow(played) < 1 || nrow(remaining) < 1) {
    return(NULL)
  }

  strengths <- calculate_team_strengths(played)

  current_standings <- bind_rows(
    played |>
      transmute(
        team = home_team_id,
        pts = if_else(
          home_goals > away_goals,
          3L,
          if_else(home_goals == away_goals, 1L, 0L)
        ),
        gd = home_goals - away_goals,
        gs = home_goals
      ),
    played |>
      transmute(
        team = away_team_id,
        pts = if_else(
          away_goals > home_goals,
          3L,
          if_else(home_goals == away_goals, 1L, 0L)
        ),
        gd = away_goals - home_goals,
        gs = away_goals
      )
  ) |>
    group_by(team) |>
    summarise(
      current_points = sum(pts),
      current_gd = sum(gd),
      current_gs = sum(gs),
      games_played = n(),
      .groups = "drop"
    )

  sim_fn <- if (use_gpu) simulate_season_gpu else simulate_season_vectorized

  sim_fn(
    current_standings = current_standings,
    remaining_games = remaining,
    team_strengths = strengths,
    n_sims = n_sims,
    home_advantage = home_advantage,
    qualify_top_n = qualify_top_n
  )
}

simulate_season_gpu <- function(
  current_standings,
  remaining_games,
  team_strengths,
  n_sims,
  home_advantage = 0.3,
  qualify_top_n = 4L
) {
  if (!torch::cuda_is_available()) {
    stop(
      "CUDA not available — use simulate_season_vectorized() for CPU execution."
    )
  }

  n_games <- nrow(remaining_games)
  all_teams <- current_standings$team
  n_teams <- length(all_teams)

  # ── xG rates (CPU, length n_games) ────────────────────────────────────────────
  raw_avg <- mean(team_strengths$attack_strength, na.rm = TRUE)
  league_avg <- if (is.na(raw_avg) || raw_avg < 0.1) 1.3 else raw_avg

  str_teams <- team_strengths$team
  home_attack <- team_strengths$attack_strength[match(
    remaining_games$home_team_id,
    str_teams
  )]
  home_defense <- team_strengths$defense_strength[match(
    remaining_games$home_team_id,
    str_teams
  )]
  away_attack <- team_strengths$attack_strength[match(
    remaining_games$away_team_id,
    str_teams
  )]
  away_defense <- team_strengths$defense_strength[match(
    remaining_games$away_team_id,
    str_teams
  )]

  unmatched <- unique(c(
    remaining_games$home_team_id[is.na(home_attack)],
    remaining_games$away_team_id[is.na(away_attack)]
  ))
  if (length(unmatched) > 0) {
    message(sprintf(
      "    %d team(s) with no match history — assuming 0 strength: %s",
      length(unmatched),
      paste(unmatched, collapse = ", ")
    ))
    home_attack[is.na(home_attack)] <- 0
    home_defense[is.na(home_defense)] <- 0
    away_attack[is.na(away_attack)] <- 0
    away_defense[is.na(away_defense)] <- 0
  }

  home_xg <- pmax(
    0.01,
    (home_attack * away_defense / league_avg) + home_advantage
  )
  away_xg <- pmax(0.01, (away_attack * home_defense / league_avg))

  # ── One-hot assignment matrices (n_games × n_teams) — built on CPU, tiny ──────
  home_cols <- match(remaining_games$home_team_id, all_teams)
  away_cols <- match(remaining_games$away_team_id, all_teams)

  home_onehot_r <- matrix(0, n_games, n_teams)
  away_onehot_r <- matrix(0, n_games, n_teams)
  home_onehot_r[cbind(seq_len(n_games), home_cols)] <- 1
  away_onehot_r[cbind(seq_len(n_games), away_cols)] <- 1

  home_onehot <- torch::torch_tensor(
    home_onehot_r,
    dtype = torch::torch_float32(),
    device = "cuda"
  )
  away_onehot <- torch::torch_tensor(
    away_onehot_r,
    dtype = torch::torch_float32(),
    device = "cuda"
  )

  # ── Rate tensors: broadcast (n_games,) → (n_sims, n_games) on GPU ─────────────
  # expand() is zero-copy; contiguous() materialises the full matrix on GPU.
  home_rates_t <- torch::torch_tensor(
    home_xg,
    dtype = torch::torch_float32(),
    device = "cuda"
  )
  away_rates_t <- torch::torch_tensor(
    away_xg,
    dtype = torch::torch_float32(),
    device = "cuda"
  )
  home_rates_2d <- home_rates_t$unsqueeze(1L)$expand(c(
    n_sims,
    n_games
  ))$contiguous()
  away_rates_2d <- away_rates_t$unsqueeze(1L)$expand(c(
    n_sims,
    n_games
  ))$contiguous()
  rm(home_rates_t, away_rates_t)

  # ── Poisson draws on GPU — (n_sims, n_games) ──────────────────────────────────
  # NOTE: PyTorch issue #136750 — validate against rpois via validate_gpu_vs_cpu()
  # before trusting for production.
  h_goals_t <- torch::torch_poisson(home_rates_2d)
  a_goals_t <- torch::torch_poisson(away_rates_2d)
  rm(home_rates_2d, away_rates_2d)

  # ── Points tensors (float32 for matmul) ───────────────────────────────────────
  h_win <- (h_goals_t > a_goals_t)$to(dtype = torch::torch_float32())
  h_draw <- (h_goals_t == a_goals_t)$to(dtype = torch::torch_float32())
  a_win <- (a_goals_t > h_goals_t)$to(dtype = torch::torch_float32())

  h_pts_t <- h_win * 3 + h_draw # (n_sims, n_games)
  a_pts_t <- a_win * 3 + h_draw # (n_sims, n_games)
  h_gd_t <- h_goals_t - a_goals_t # (n_sims, n_games) home GD per game
  rm(h_win, h_draw, a_win)

  # ── Standings accumulation via matmul — fully on GPU ──────────────────────────
  # (n_sims, n_games) @ (n_games, n_teams) = (n_sims, n_teams)
  start_pts_t <- torch::torch_tensor(
    as.numeric(current_standings$current_points),
    dtype = torch::torch_float32(),
    device = "cuda"
  ) # (n_teams,) — broadcasts to (n_sims, n_teams) on addition

  final_pts_t <- torch::torch_matmul(h_pts_t, home_onehot) +
    torch::torch_matmul(a_pts_t, away_onehot) +
    start_pts_t

  final_gd_t <- torch::torch_matmul(h_gd_t, home_onehot) +
    torch::torch_matmul(-h_gd_t, away_onehot)

  h_goals_f <- h_goals_t$to(dtype = torch::torch_float32())
  a_goals_f <- a_goals_t$to(dtype = torch::torch_float32())
  final_gs_t <- torch::torch_matmul(h_goals_f, home_onehot) +
    torch::torch_matmul(a_goals_f, away_onehot)
  rm(h_pts_t, a_pts_t, h_gd_t, h_goals_t, a_goals_t, h_goals_f, a_goals_f)
  rm(home_onehot, away_onehot, start_pts_t)

  # ── Tiebreaker score — fully on GPU ───────────────────────────────────────────
  rand_t <- torch::torch_rand(
    c(n_sims, n_teams),
    dtype = torch::torch_float32(),
    device = "cuda"
  )
  tb_score_t <- (final_pts_t * 1e6) +
    ((final_gd_t + 500) * 1e3) +
    final_gs_t +
    rand_t
  rm(rand_t, final_gd_t, final_gs_t)

  # ── Ranking via double argsort — fully on GPU ──────────────────────────────────
  # R torch argsort returns 1-based indices (R convention).
  # argsort(-x, dim=2) → 1-based positions sorted high-to-low
  # argsort(that, dim=2) → 1-based rank for each team (1 = best)
  rank_t <- torch::torch_argsort(
    torch::torch_argsort(-tb_score_t, dim = 2L),
    dim = 2L
  )
  rm(tb_score_t)

  # ── Pull back only (n_sims × n_teams) result matrices ─────────────────────────
  made_playoffs_cpu <- as.array((rank_t <= as.integer(qualify_top_n))$cpu())
  points_cpu <- as.array(final_pts_t$cpu())
  ranks_cpu <- as.array(rank_t$cpu())
  rm(rank_t, final_pts_t)

  data.table(
    sim_id = rep(seq_len(n_sims), times = n_teams),
    team = rep(all_teams, each = n_sims),
    points = as.vector(points_cpu),
    rank = as.integer(as.vector(ranks_cpu)),
    made_playoffs = as.vector(made_playoffs_cpu)
  )
}

# Validate GPU Poisson RNG output against CPU rpois.
# Run once after installing torch before using GPU for production.
# Max diff should be well under 1% at n_sims = 100K if RNG is healthy.
validate_gpu_vs_cpu <- function(
  all_results,
  cutoff_date,
  qualify_top_n,
  n_sims = 100000,
  home_advantage = 0.3
) {
  played <- get_played_at_date(all_results, cutoff_date)
  remaining <- get_remaining_at_date(all_results, cutoff_date)
  strengths <- calculate_team_strengths(played)

  current_standings <- bind_rows(
    played |>
      transmute(
        team = home_team_id,
        pts = if_else(
          home_goals > away_goals,
          3L,
          if_else(home_goals == away_goals, 1L, 0L)
        )
      ),
    played |>
      transmute(
        team = away_team_id,
        pts = if_else(
          away_goals > home_goals,
          3L,
          if_else(home_goals == away_goals, 1L, 0L)
        )
      )
  ) |>
    group_by(team) |>
    summarise(current_points = sum(pts), .groups = "drop")

  cpu_res <- simulate_season_vectorized(
    current_standings,
    remaining,
    strengths,
    n_sims,
    home_advantage,
    qualify_top_n
  )
  gpu_res <- simulate_season_gpu(
    current_standings,
    remaining,
    strengths,
    n_sims,
    home_advantage,
    qualify_top_n
  )

  cpu_odds <- as_tibble(cpu_res) |>
    group_by(team) |>
    summarize(cpu_pct = mean(made_playoffs) * 100, .groups = "drop")
  gpu_odds <- as_tibble(gpu_res) |>
    group_by(team) |>
    summarize(gpu_pct = mean(made_playoffs) * 100, .groups = "drop")

  comparison <- inner_join(cpu_odds, gpu_odds, by = "team") |>
    mutate(diff = abs(cpu_pct - gpu_pct))

  message(sprintf(
    "Max absolute diff in playoff_pct: %.3f%%",
    max(comparison$diff)
  ))
  message(sprintf(
    "Mean absolute diff:                %.3f%%",
    mean(comparison$diff)
  ))
  message("PASS threshold: max diff < 1.0%% at n_sims = 100K")

  comparison
}

plot_scoreline_distributions <- function(scoreline_dist, ncol = 3) {
  scoreline_dist %>%
    mutate(home_loss = away_goals > home_goals) %>%
    ggplot(aes(x = away_goals, y = home_goals, fill = prob)) +
    geom_tile(aes(linewidth = home_loss), color = "gray30") +
    scale_linewidth_manual(
      values = c("TRUE" = 1.2, "FALSE" = 0.3),
      guide = "none"
    ) +
    geom_shadowtext(
      aes(label = scales::percent(prob, accuracy = 0.1)),
      color = "white",
      bg.color = "black",
      bg.r = 0.1,
      size = 4,
      fontface = "bold"
    ) +
    facet_wrap(~matchup, ncol = ncol) +
    scale_fill_viridis_c(
      option = "inferno",
      labels = scales::percent,
      name = "Probability"
    ) +
    scale_x_continuous(breaks = 0:5, expand = c(0, 0)) +
    scale_y_continuous(breaks = 0:5, expand = c(0, 0)) +
    labs(
      title = "Scoreline Probability Distributions",
      subtitle = "Home team on Y-axis | Based on Poisson simulation",
      x = "Away Goals",
      y = "Home Goals"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )
}
