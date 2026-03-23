suppressPackageStartupMessages({
  library(tidyverse)
  library(itscalledsoccer)
  library(mirai)
  library(data.table)
  library(dtplyr)
  library(httr2)
  library(jsonlite)
})

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

  home_xg <- pmax(
    0.01,
    (home_attack * away_defense / league_avg) + home_advantage
  )
  away_xg <- pmax(0.01, (away_attack * home_defense / league_avg))

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
  n_sims
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
      made_playoffs = rank <= 4
    ) %>%
    ungroup() %>%
    select(sim_id, team, final_points, final_gd, final_gs, rank, made_playoffs)

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

  playoffs <- t(apply(tb_score, 1, function(x) {
    rank(-x, ties.method = "random") <= 4
  }))

  results <- data.table(
    team = rep(all_teams, each = n_sims),
    points = as.vector(final_pts),
    made_playoffs = as.vector(playoffs)
  )

  return(results)
}

calculate_playoff_odds_fast <- function(
  schedule_obj,
  n_sims = 10000,
  n_cores = 6
) {
  message("Fetching official scores from ASA API...")
  asa_games <- asa_client$get_games(leagues = 'usls', season = '2025-26') %>%
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
    anti_join(played_games, by = c("home_team_id", "away_team_id", "date"))

  all_team_ids <- unique(c(
    schedule_mapped$home_team_id,
    schedule_mapped$away_team_id
  ))

  if (nrow(played_games) > 0) {
    team_strengths <- calculate_team_strengths(played_games)

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
    "ASA: %d games played. Schedule: %d games remaining.",
    nrow(played_games),
    nrow(remaining_games)
  ))

  daemons(n_cores)
  everywhere({ library(data.table) })

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
        simulate_season_vectorized(
          current_standings,
          remaining_games,
          team_strengths_complete,
          this_n
        )
      },
      current_standings = current_standings,
      remaining_games = remaining_games,
      team_strengths_complete = team_strengths_complete,
      sims_per_worker = sims_per_worker,
      n_sims = n_sims,
      n_cores = n_cores,
      simulate_season_vectorized = simulate_season_vectorized
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

  return(list(
    summary = final_tab,
    raw = as.data.table(playoff_results)
  ))
}

get_match_probabilities <- function(
  remaining_games,
  team_strengths,
  teams_info,
  n_sims = 10000
) {
  match_results <- simulate_matches_vectorized(
    remaining_games,
    team_strengths,
    n_sims
  )

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
  remaining_games,
  team_strengths,
  teams_info,
  n_sims = 50000,
  max_goals = 5
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
