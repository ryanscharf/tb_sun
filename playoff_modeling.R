library(tidyverse)
library(itscalledsoccer)
library(mirai) # Native parallelization backend
library(data.table)
library(dtplyr) # Enables data.table backend for dplyr
library(tictoc)
library(gt)
library(ggridges)
library(httr2)
library(jsonlite)

get_schedule <- function(league_id = "10699", season = "2025-26", con = NULL) {
  url <- paste0("https://www.fotmob.com/api/data/leagues?id=", league_id)

  fotmob_schedule <- tryCatch({
    data <- request(url) %>%
      req_perform() %>%
      resp_body_string() %>%
      fromJSON()

    data$fixtures$allMatches %>%
      unnest_wider(everything(), names_sep = "_") %>%
      unnest_wider(any_of(c("home", "away")), names_sep = "_") %>%
      unnest_wider(starts_with("status"), names_sep = "_") %>%
      select(
        home_team    = home_name,
        away_team    = away_name,
        is_completed = status_finished_1,
        date_utc     = status_utcTime_1
      ) %>%
      mutate(
        date_utc = as.POSIXct(date_utc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        season   = season
      )
  }, error = function(e) {
    message(sprintf("FotMob fetch failed: %s", conditionMessage(e)))
    NULL
  })

  if (!is.null(fotmob_schedule) && !is.null(con)) {
    # Diff against this season's DB rows only to flag rescheduled games
    db_schedule <- dbGetQuery(con,
      sprintf("SELECT home_team, away_team, date_utc FROM schedule WHERE season = '%s'", season)
    ) %>%
      mutate(date_utc = as.POSIXct(date_utc, tz = "UTC"))

    rescheduled_keys <- fotmob_schedule %>%
      inner_join(db_schedule, by = c("home_team", "away_team"), suffix = c("_new", "_old")) %>%
      filter(date_utc_new != date_utc_old) %>%
      select(home_team, away_team, date_utc = date_utc_new)

    fotmob_schedule <- fotmob_schedule %>%
      left_join(rescheduled_keys %>% mutate(is_rescheduled = TRUE),
                by = c("home_team", "away_team", "date_utc")) %>%
      mutate(is_rescheduled = coalesce(is_rescheduled, FALSE))

    n_rescheduled <- sum(fotmob_schedule$is_rescheduled)
    if (n_rescheduled > 0) message(sprintf("%d game(s) flagged as rescheduled.", n_rescheduled))

    dbWriteTable(con, "schedule_staging", fotmob_schedule,
                 temporary = TRUE, overwrite = TRUE, row.names = FALSE)

    dbExecute(con, "
      INSERT INTO schedule (season, home_team, away_team, is_completed, is_rescheduled, date_utc)
      SELECT season, home_team, away_team, is_completed, is_rescheduled, date_utc FROM schedule_staging
      ON CONFLICT (season, home_team, away_team, date_utc)
      DO UPDATE SET
        is_completed   = EXCLUDED.is_completed,
        is_rescheduled = EXCLUDED.is_rescheduled,
        updated_at     = NOW()
    ")

    deleted <- dbExecute(con, sprintf("
      DELETE FROM schedule
      WHERE season = '%s'
        AND NOT EXISTS (
          SELECT 1 FROM schedule_staging s
          WHERE s.home_team = schedule.home_team
            AND s.away_team = schedule.away_team
            AND s.date_utc  = schedule.date_utc
        )
    ", season))
    if (deleted > 0) message(sprintf("Removed %d stale schedule rows.", deleted))

    message("Schedule synced to DB.")
    return(fotmob_schedule)
  }

  if (!is.null(fotmob_schedule)) return(fotmob_schedule)

  if (!is.null(con)) {
    message("FotMob unavailable — falling back to DB schedule.")
    schedule <- dbGetQuery(con, sprintf(
      "SELECT home_team, away_team, is_completed, date_utc FROM schedule WHERE season = '%s' ORDER BY date_utc",
      season
    )) %>%
      mutate(date_utc = as.POSIXct(date_utc, tz = "UTC"))
    if (nrow(schedule) == 0) stop("DB schedule fallback is empty.")
    return(schedule)
  }

  stop("FotMob fetch failed and no DB connection provided for fallback.")
}


# Initialize ASA client and get teams
asa_client <- AmericanSoccerAnalysis$new()
teams <- asa_client$get_teams(leagues = 'usls')

# Load schedule from uploaded CSV
# schedule <- read_csv(
#   "Gainbridge USL Super League 2025-26 data - Game Records.csv"
# ) %>%
#   select(
#     match_id = `Match ID`,
#     date = Date,
#     home_team = `Home Team`,
#     home_goals = `Home Goals`,
#     away_goals = `Away Goals`,
#     away_team = `Away Team`
#   ) %>%
#   mutate(
#     date = mdy(date),
#     home_goals = as.numeric(home_goals),
#     away_goals = as.numeric(away_goals),
#     is_completed = !is.na(home_goals) & !is.na(away_goals)
#   )

# Team name mapping
# team_name_mapping <- tribble(
#   ~schedule_name    , ~team_abbreviation ,
#   "Brooklyn"        , "BKN"              ,
#   "Tampa Bay"       , "TB"               ,
#   "Dallas"          , "DAL"              ,
#   "Fort Lauderdale" , "FTL"              ,
#   "Lexington"       , "LEX"              ,
#   "Spokane"         , "SPK"              ,
#   "DC"              , "DC"               ,
#   "Jacksonville"    , "JAX"              ,
#   "Carolina"        , "CAR"
# ) %>%
#   left_join(teams %>% distinct(team_id, team_abbreviation)) %>%
#    mutate(fotmob_name = case_when(
#     schedule_name == "Brooklyn" ~ "Brooklyn FC (W)",
#     schedule_name == "Carolina" ~ "Carolina Ascent FC (W)",
#     schedule_name == "Dallas" ~ "Dallas Trinity FC (W)",
#     schedule_name == "DC" ~ "DC Power FC (W)",
#     schedule_name == "Fort Lauderdale" ~ "Fort Lauderdale United FC (W)",
#     schedule_name == "Jacksonville" ~ "Sporting JAX (W)",
#     schedule_name == "Lexington" ~ "Lexington SC (W)",
#     schedule_name == "Spokane" ~ "Spokane Zephyr FC (W)",
#     schedule_name == "Tampa Bay" ~ "Tampa Bay Sun FC (W)"
#   ))

schedule <- get_schedule()

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


# Function to calculate team strengths (using dtplyr)
calculate_team_strengths <- function(completed_games) {
  # Convert to lazy dt for dtplyr
  dt <- lazy_dt(completed_games)

  home <- dt %>%
    group_by(team = home_team_id) %>%
    summarize(
      goals_for = sum(home_goals),
      goals_against = sum(away_goals),
      games = n(),
      .groups = "drop"
    ) %>%
    as_tibble() # Collect BEFORE bind_rows

  away <- dt %>%
    group_by(team = away_team_id) %>%
    summarize(
      goals_for = sum(away_goals),
      goals_against = sum(home_goals),
      games = n(),
      .groups = "drop"
    ) %>%
    as_tibble() # Collect BEFORE bind_rows

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

# Function 1: Simulate match-level results for all sims
simulate_matches_vectorized <- function(
  remaining_games,
  team_strengths,
  n_sims,
  home_advantage = 0.3
) {
  n_games <- nrow(remaining_games)

  # 1. Calculate xG for each match ------------------------------------------------------
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

  home_xg <- pmax(
    0.01,
    (home_attack * away_defense / league_avg) + home_advantage
  )
  away_xg <- pmax(0.01, (away_attack * home_defense / league_avg))

  # 2. Simulate Goals for ALL sims x ALL games -------------------------------------------------------
  # Matrices are [n_sims x n_games]
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

  # 3. Calculate match outcomes --------------------------------------------------
  h_pts <- ifelse(h_goals > a_goals, 3, ifelse(h_goals == a_goals, 1, 0))
  a_pts <- ifelse(a_goals > h_goals, 3, ifelse(h_goals == a_goals, 1, 0))

  h_gd <- h_goals - a_goals
  a_gd <- a_goals - h_goals

  # 4. Create match results data.table --------------------------------------------
  # Each row is one match in one simulation
  match_results <- data.table(
    sim_id = rep(1:n_sims, each = n_games),
    match_id = rep(1:n_games, times = n_sims),
    home_team_id = rep(remaining_games$home_team_id, times = n_sims),
    away_team_id = rep(remaining_games$away_team_id, times = n_sims),
    home_goals = as.vector(t(h_goals)), # Transpose to get correct order
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

# Function 2: Aggregate match results to season outcomes
aggregate_season_results <- function(
  match_results,
  current_standings,
  n_sims
) {
  all_teams <- current_standings$team
  n_teams <- length(all_teams)

  # Aggregate home results
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

  # Aggregate away results
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

  # Combine and add current standings
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
      final_gd = sim_gd, # Assuming current GD starts at 0 for simplicity
      final_gs = sim_gs
    )

  # Apply tie-breakers and determine playoffs
  season_results <- season_results %>%
    group_by(sim_id) %>%
    mutate(
      # Tie-break score: Points (millions) + GD (thousands) + GS (units) + random
      tb_score = (final_points * 1e6) +
        ((final_gd + 500) * 1e3) +
        final_gs +
        runif(n()),
      rank = rank(-tb_score, ties.method = "random"),
      made_playoffs = rank <= 4
    ) %>%
    ungroup() %>%
    select(sim_id, team, final_points, final_gd, final_gs, rank, made_playoffs)

  return(season_results)
}

# Updated main simulation function that uses both
simulate_season_vectorized <- function(
  current_standings,
  remaining_games,
  team_strengths,
  n_sims,
  home_advantage = 0.3,
  return_matches = FALSE # NEW: option to return match-level data
) {
  # Simulate all matches
  match_results <- simulate_matches_vectorized(
    remaining_games,
    team_strengths,
    n_sims,
    home_advantage
  )

  # If user wants match-level results, return them
  if (return_matches) {
    return(match_results)
  }

  # Otherwise, aggregate to season results
  season_results <- aggregate_season_results(
    match_results,
    current_standings,
    n_sims
  )

  return(season_results)
}

# VECTORIZED simulation - simulate ALL games for ALL sims at once
simulate_season_vectorized <- function(
  current_standings,
  remaining_games,
  team_strengths,
  n_sims,
  home_advantage = 0.3
) {
  n_games <- nrow(remaining_games)
  all_teams <- current_standings$team
  n_teams <- length(all_teams)

  # 1. Setup xG Clamps ------------------------------------------------------
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

  home_xg <- pmax(
    0.01,
    (home_attack * away_defense / league_avg) + home_advantage
  )
  away_xg <- pmax(0.01, (away_attack * home_defense / league_avg))

  # 2. Simulate Goals -------------------------------------------------------
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

  # 3. Points and GD Logic --------------------------------------------------
  h_pts <- ifelse(h_goals > a_goals, 3, ifelse(h_goals == a_goals, 1, 0))
  a_pts <- ifelse(a_goals > h_goals, 3, ifelse(h_goals == a_goals, 1, 0))

  h_gd <- h_goals - a_goals
  a_gd <- a_goals - h_goals

  # 4. Aggregate to Seasonal Totals -----------------------------------------
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

  # 5. Apply Tie-Breakers ---------------------------------------------------
  tb_score <- (final_pts * 1e6) +
    ((final_gd + 500) * 1e3) +
    (final_gs) +
    matrix(runif(n_sims * n_teams), n_sims, n_teams)

  playoffs <- t(apply(tb_score, 1, function(x) {
    rank(-x, ties.method = "random") <= 4
  }))

  # 6. Formatting Results - return as data.table for dtplyr
  results <- data.table(
    team = rep(all_teams, each = n_sims),
    points = as.vector(final_pts),
    made_playoffs = as.vector(playoffs)
  )

  return(results)
}

# Main function to calculate playoff odds (dtplyr optimized)
calculate_playoff_odds_fast <- function(
  schedule_obj,
  n_sims = 10000,
  n_cores = 6
) {
  # 1. Fetch Official Results from ASA --------------------------------------
  message("Fetching official scores from ASA API...")
  asa_games <- asa_client$get_games(leagues = 'usls', season = '2025-26') %>%
    lazy_dt() %>%
    mutate(date_only = as.Date(date_time_utc)) %>%
    as_tibble()

  # 2. Map IDs to your Schedule Blueprint -----------------------------------
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

  # 3. Separate Played vs. Remaining ----------------------------------------
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
    anti_join(played_games, by = c("home_team_id", "away_team_id", "date"))

  all_team_ids <- unique(c(
    schedule_mapped$home_team_id,
    schedule_mapped$away_team_id
  ))

  # 4. Current Standings (from ASA results only) ----------------------------
  if (nrow(played_games) > 0) {
    team_strengths <- calculate_team_strengths(played_games)

    # Calculate Current Points using dtplyr
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
      as_tibble() # Collect BEFORE bind_rows

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
      as_tibble() # Collect BEFORE bind_rows

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

  # Fill missing teams
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

  # 5. Simulation using purrr + mirai -----------------------------------------------------------
  message(sprintf(
    "ASA: %d games played. Schedule: %d games remaining.",
    nrow(played_games),
    nrow(remaining_games)
  ))

  # Set up mirai daemons for parallel processing
  daemons(n_cores)

  # Load required packages on all workers
  everywhere({
    library(data.table)
  })

  sims_per_worker <- ceiling(n_sims / n_cores)

  # Use map() with in_parallel() for native purrr parallelization
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
        simulate_season_vectorized(
          current_standings,
          remaining_games,
          team_strengths_complete,
          this_n
        )
      },
      # Pass required objects to the parallel workers
      current_standings = current_standings,
      remaining_games = remaining_games,
      team_strengths_complete = team_strengths_complete,
      sims_per_worker = sims_per_worker,
      n_sims = n_sims,
      n_cores = n_cores,
      simulate_season_vectorized = simulate_season_vectorized
    ),
    .progress = TRUE # Built-in progress bar
  ) %>%
    bind_rows()

  # Clean up daemons
  daemons(0)

  # 6. Output Table using dtplyr ---------------------------------------------------------
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

  return(list(
    summary = final_tab,
    raw = as.data.table(playoff_results) # Keep as data.table for downstream
  ))
}

gc()
# RUN THE ANALYSIS
tictoc::tic()
playoff_odds <- calculate_playoff_odds_fast(
  schedule,
  n_sims = 1e6,
  n_cores = 18
)
tictoc::toc()

output <- playoff_odds
playoff_odds <- output$summary
playoff_results <- output$raw

# NEW: Get match-level predictions for remaining games
get_match_probabilities <- function(
  remaining_games,
  team_strengths,
  teams_info,
  n_sims = 10000
) {
  # Simulate matches
  match_results <- simulate_matches_vectorized(
    remaining_games,
    team_strengths,
    n_sims
  )

  # Calculate probabilities for each match
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

# NEW: Get team's remaining schedule with win probabilities
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

  # Get all matches involving this team
  team_matches <- remaining_games %>%
    mutate(match_id = row_number()) %>%
    filter(home_team_id == team_id | away_team_id == team_id)

  if (nrow(team_matches) == 0) {
    return(tibble(message = "No remaining matches"))
  }

  # Simulate those matches
  match_results <- simulate_matches_vectorized(
    team_matches,
    team_strengths,
    n_sims
  )

  # Calculate probabilities from team's perspective
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

# Example usage after running main simulation:
# Get match probabilities for all remaining games
# match_probs <- get_match_probabilities(
#   remaining_games,
#   team_strengths,
#   teams,
#   n_sims = 50000
# )
#
# # Get specific team's path to playoffs
# ftl_schedule <- get_team_path_to_playoffs(
#   "FTL",
#   remaining_games,
#   team_strengths,
#   teams,
#   n_sims = 50000
# )

# Visualization
playoff_odds %>%
  ggplot(aes(x = reorder(team_abbreviation, playoff_pct), y = playoff_pct)) +
  geom_col(aes(fill = playoff_pct > 50), show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.2f%%", playoff_pct)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_fill_manual(values = c("gray70", "darkgreen")) +
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
  labs(
    title = "USL Super League Playoff Probabilities",
    subtitle = sprintf(
      "Based on %s Monte Carlo simulations",
      format(1e6, big.mark = ",")
    ),
    x = NULL,
    y = "Playoff Probability (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  )

# Calculate uncertainty using dtplyr
uncertainty_table <- playoff_results %>%
  lazy_dt() %>%
  group_by(team) %>%
  summarize(
    playoff_prob = mean(made_playoffs),
    avg_pts = mean(points),
    sd_pts = sd(points),
    n = n(),
    .groups = "drop"
  ) %>%
  as_tibble() %>%
  mutate(
    se = sqrt((playoff_prob * (1 - playoff_prob)) / n),
    lower_ci = playoff_prob - (1.96 * se),
    upper_ci = playoff_prob + (1.96 * se)
  ) %>%
  left_join(
    teams %>% select(team_id, team_abbreviation),
    by = c("team" = "team_id")
  ) %>%
  arrange(desc(playoff_prob))

# Render the uncertainty table
uncertainty_table %>%
  select(team_abbreviation, playoff_prob, se, lower_ci, upper_ci) %>%
  gt() %>%
  tab_header(title = "Simulation Reliability Metrics") %>%
  fmt_percent(columns = -team_abbreviation, decimals = 2) %>%
  cols_label(
    team_abbreviation = "Team",
    playoff_prob = "Chance",
    se = "Std. Error",
    lower_ci = "95% Low",
    upper_ci = "95% High"
  )

# Calculate ranks using dtplyr
playoff_results_with_ranks <- playoff_results %>%
  lazy_dt() %>%
  group_by(team) %>%
  mutate(sim_id = row_number()) %>%
  ungroup() %>%
  group_by(sim_id) %>%
  mutate(rank = rank(-points, ties.method = "random")) %>%
  ungroup() %>%
  as_tibble()

# Verify average rank
playoff_results_with_ranks %>%
  lazy_dt() %>%
  group_by(team) %>%
  summarize(mean_rank = mean(rank), .groups = "drop") %>%
  as_tibble() %>%
  left_join(
    teams %>% select(team_id, team_abbreviation),
    by = c("team" = "team_id")
  ) %>%
  arrange(mean_rank)

# Create ridge plot
plot_data <- playoff_results_with_ranks %>%
  left_join(
    teams %>% select(team_id, team_abbreviation),
    by = c("team" = "team_id")
  )

ggplot(
  plot_data,
  aes(
    x = rank,
    y = reorder(team_abbreviation, -rank, FUN = mean),
    fill = team_abbreviation
  )
) +
  geom_density_ridges(
    stat = "binline",
    bins = 9,
    scale = 0.9,
    draw_baseline = FALSE,
    alpha = 0.8
  ) +
  scale_fill_viridis_d(option = "mako", guide = "none") +
  scale_x_continuous(breaks = 1:9, limits = c(0, 10)) +
  theme_ridges(grid = TRUE) +
  annotate(
    "rect",
    xmin = 0.5,
    xmax = 4.5,
    ymin = 0,
    ymax = Inf,
    fill = "green",
    alpha = 0.05
  ) +
  annotate(
    "text",
    x = 2.5,
    y = 9.8,
    label = "PLAYOFF ZONE",
    color = "green4",
    fontface = "bold",
    alpha = 0.3
  ) +
  coord_cartesian(xlim = c(0.2, 9.8), clip = "off") +
  labs(
    title = "Final Table Position Distributions",
    subtitle = "1,000,000 Simulated Seasons | USL Super League",
    x = "Final Rank (1st to 9th)",
    y = NULL
  )

# Chaos metrics using dtplyr
bubble_teams <- teams %>%
  filter(team_abbreviation %in% c("DAL", "DC", "SPK", 'CAR')) %>%
  select(team_id, team_abbreviation)

# Pivot to wide format
sim_wide <- playoff_results_with_ranks %>%
  filter(team %in% bubble_teams$team_id) %>%
  pivot_wider(
    id_cols = sim_id,
    names_from = team,
    values_from = c(points, rank),
    names_sep = "_"
  )

# Calculate cutoffs
cutoffs <- playoff_results_with_ranks %>%
  lazy_dt() %>%
  filter(rank %in% c(4, 5, 6)) %>%
  group_by(sim_id, rank) %>%
  slice(1) %>%
  ungroup() %>%
  select(sim_id, rank, points) %>%
  pivot_wider(
    names_from = rank,
    values_from = points,
    names_prefix = "pts_"
  ) %>%
  as_tibble()

sim_wide <- sim_wide %>%
  left_join(cutoffs, by = "sim_id")

# Calculate chaos metrics
total_sims <- 1e6

ftl_team_id <- bubble_teams %>%
  filter(team_abbreviation == "FTL") %>%
  pull(team_id)
tb_team_id <- bubble_teams %>%
  filter(team_abbreviation == "TB") %>%
  pull(team_id)

# chaos_metrics <- tibble(
#   Scenario = c(
#     "The Photo Finish (4th & 5th separated by ≤ 1 pt)",
#     "The Three-Way Tie (4th, 5th, and 6th all on same points)",
#     # "The Heartbreak Finish (FTL finishes 5th on Tie-breakers)",
#     "The TB Miracle (TB overcomes odds to take 4th)"
#   ),
#   Frequency = c(
#     sim_wide %>% filter(pts_4 - pts_5 <= 1) %>% nrow(),
#     sim_wide %>% filter(pts_4 == pts_5 & pts_5 == pts_6) %>% nrow(),
#     # sim_wide %>%
#     #   filter(
#     #     .data[[paste0("rank_", ftl_team_id)]] == 5 &
#     #       .data[[paste0("points_", ftl_team_id)]] == pts_4
#     #   ) %>%
#     #   nrow(),
#     sim_wide %>% filter(.data[[paste0("rank_", tb_team_id)]] == 4) %>% nrow()
#   )
# ) %>%
#   mutate(
#     Probability = paste0(round((Frequency / total_sims) * 100, 2), "%"),
#     Frequency = format(Frequency, big.mark = ",")
#   )

# print("--- THE CHAOS TIER: PROBABILITY TABLE ---")
# print(chaos_metrics)

# Playoff line histogram
cutoffs %>%
  rename(playoff_line_pts = pts_4) %>%
  mutate(playoff_line_pts = as.integer(playoff_line_pts)) %>%
  ggplot() +
  geom_histogram(aes(x = playoff_line_pts), binwidth = 1) +
  labs(
    title = "Distribution of 4th Place Point Totals",
    x = "Points Required for 4th Place",
    y = "Frequency",
    subtitle = "Sampled Over 1,000,000 Simulations"
  ) +
  theme_minimal()

# ##### testing
# asa_games <- asa_client$get_games(leagues = 'usls', season = '2025-26') %>%
#   lazy_dt() %>%
#   mutate(date_only = as.Date(date_time_utc)) %>%
#   as_tibble()

# # 2. Map IDs to your Schedule Blueprint -----------------------------------
# schedule_mapped <- schedule %>%
#   left_join(team_name_mapping, by = c("home_team" = "schedule_name")) %>%
#   rename(home_team_id = team_id) %>%
#   select(-team_abbreviation) %>%
#   left_join(team_name_mapping, by = c("away_team" = "schedule_name")) %>%
#   rename(away_team_id = team_id) %>%
#   mutate(date = as.Date(date))

# # 3. Separate Played vs. Remaining ----------------------------------------
# played_games <- asa_games %>%
#   lazy_dt() %>%
#   filter(status == "FullTime") %>%
#   select(
#     home_team_id,
#     away_team_id,
#     home_goals = home_score,
#     away_goals = away_score,
#     date = date_only
#   ) %>%
#   as_tibble()

# # Create a match key to ensure proper matching
# schedule_with_key <- schedule_mapped %>%
#   mutate(match_key = paste(home_team_id, away_team_id, date, sep = "_"))

# played_with_key <- played_games %>%
#   mutate(match_key = paste(home_team_id, away_team_id, date, sep = "_"))
# # Mark games as played based on the match key

# remaining_games <- schedule_with_key %>%
#   filter(!match_key %in% played_with_key$match_key) %>%
#   select(-match_key)

# team_strengths <- calculate_team_strengths(played_games)

# tb_path <- get_team_path_to_playoffs(
#   "TB",
#   remaining_games,
#   team_strengths,
#   teams,
#   n_sims = 50000
# )

# print(tb_path)

# asa_mapped <- asa_games %>%
#   left_join(team_name_mapping, by = c("home_team_id" = "team_id")) %>%
#   rename(home_team = schedule_name) %>%
#   select(-team_abbreviation) %>%
#   left_join(team_name_mapping, by = c("away_team_id" = "team_id")) %>%
#   rename(away_team = schedule_name) %>%
#   select(-team_abbreviation) %>%
#   mutate(
#     date_time_utc = lubridate::ymd_hms(date_time_utc),
#     date_time_est = with_tz(date_time_utc, tzone = "America/New_York"),
#     date_only = as_date(date_time_est)
#   )

# asa_mapped %>%
#   select(
#     game_id,
#     date_time_utc,
#     date_time_est,
#     home_team,
#     home_score,
#     away_score,
#     away_team,
#     everything()
#   ) %>%
#   View()

# played_games <- asa_mapped %>%
#   lazy_dt() %>%
#   filter(status == "FullTime") %>%
#   select(
#     home_team_id,
#     away_team_id,
#     home_goals = home_score,
#     away_goals = away_score,
#     date = date_only
#   ) %>%
#   as_tibble()

# remaining_games <- schedule_mapped %>%
#   anti_join(played_games, by = c("home_team_id", "away_team_id", "date"))

# ── Scoreline distributions ────────────────────────────────────────────────────
get_scoreline_distributions <- function(
  remaining_games,
  team_strengths,
  teams_info,
  n_sims = 50000,
  max_goals = 5  # cap scoreline grid at this many goals
) {
  match_results <- simulate_matches_vectorized(
    remaining_games %>% mutate(match_id = row_number()),
    team_strengths,
    n_sims
  )

  match_results %>%
    lazy_dt() %>%
    mutate(
      home_goals_capped = pmin(home_goals, max_goals),
      away_goals_capped = pmin(away_goals, max_goals)
    ) %>%
    group_by(match_id, home_team_id, away_team_id, home_goals_capped, away_goals_capped) %>%
    summarize(prob = n() / n_sims, .groups = "drop") %>%
    as_tibble() %>%
    left_join(teams_info %>% select(team_id, home_team = team_abbreviation),
              by = c("home_team_id" = "team_id")) %>%
    left_join(teams_info %>% select(team_id, away_team = team_abbreviation),
              by = c("away_team_id" = "team_id")) %>%
    mutate(
      matchup = paste0(home_team, " vs ", away_team),
      scoreline = paste0(home_goals_capped, "-", away_goals_capped)
    ) %>%
    select(match_id, matchup, home_team, away_team,
           home_goals = home_goals_capped, away_goals = away_goals_capped,
           scoreline, prob)
}

plot_scoreline_distributions <- function(scoreline_dist, ncol = 3) {
  scoreline_dist %>%
    mutate(home_loss = away_goals > home_goals) %>%
    ggplot(aes(x = away_goals, y = home_goals, fill = prob)) +
    geom_tile(aes(color = home_loss, linewidth = home_loss)) +
    geom_text(aes(label = scales::percent(prob, accuracy = 0.1)),
              size = 2.8, color = "white", fontface = "bold") +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "white"), guide = "none") +
    scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.3), guide = "none") +
    facet_wrap(~ matchup, ncol = ncol) +
    scale_fill_gradient(low = "#1a1a2e", high = "#e94560",
                        labels = scales::percent, name = "Probability") +
    scale_x_continuous(breaks = 0:5, expand = c(0, 0)) +
    scale_y_continuous(breaks = 0:5, expand = c(0, 0)) +
    labs(
      title = "Scoreline Probability Distributions",
      subtitle = paste0("Home team on Y-axis | Based on Poisson simulation"),
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

# Example usage:
# scoreline_dist <- get_scoreline_distributions(
#   remaining_games,
#   team_strengths_complete,
#   teams,
#   n_sims = 50000
# )
# plot_scoreline_distributions(scoreline_dist)
