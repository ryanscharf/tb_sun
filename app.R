library(shiny)
library(bslib)
library(tidyverse)
library(DBI)
library(RPostgres)
library(jsonlite)
library(shadowtext)
library(ggridges)
source("functions.R")

# ── DB ─────────────────────────────────────────────────────────────────────────
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

get_model_versions <- function() {
  con <- get_db_conn()
  on.exit(dbDisconnect(con))
  dbGetQuery(
    con,
    "
    SELECT model_version_id, version, label, description, is_default, citations
    FROM model_versions
    WHERE is_active = TRUE
    ORDER BY model_version_id
  "
  )
}

# Seasons with data for this model version, most-recently-active first (so
# the season dropdown defaults to whatever season is actually current).
get_seasons_for_version <- function(model_version_id) {
  con <- get_db_conn()
  on.exit(dbDisconnect(con))
  dbGetQuery(
    con,
    sprintf(
      "
    SELECT season, MAX(run_at) AS latest_run_at
    FROM simulation_runs
    WHERE model_version_id = %d
    GROUP BY season
    ORDER BY latest_run_at DESC
  ",
      model_version_id
    )
  )
}

# The runner runs at the end of each game day, not once per gameweek, so a
# gameweek can have several runs -- this is only the highest gameweek_number
# reached, used to size the slider; load_db_data() below picks the latest
# run *within* whichever gameweek is selected.
get_max_gameweek <- function(model_version_id, season) {
  con <- get_db_conn()
  on.exit(dbDisconnect(con))
  dbGetQuery(
    con,
    sprintf(
      "
    SELECT MAX(gw.gameweek_number) AS max_gw
    FROM simulation_runs sr
    JOIN gameweeks gw ON sr.gameweek_id = gw.gameweek_id
    WHERE sr.model_version_id = %d AND sr.season = '%s'
  ",
      model_version_id,
      season
    )
  )$max_gw
}

# Elo has no model_version dimension -- loaded independently of the
# model_version selector, only by season/gameweek (capped at the selected
# gameweek, same "scrub back in time" behavior as Historical Trends).
get_elo_data <- function(season, gameweek_number) {
  con <- get_db_conn()
  on.exit(dbDisconnect(con))

  history <- dbGetQuery(
    con,
    sprintf(
      "
    SELECT er.team_id, er.team_name, er.team_abbreviation,
           er.elo_rating, er.games_played,
           gw.gameweek_number, gw.end_date AS gameweek_date
    FROM elo_ratings er
    JOIN gameweeks gw ON er.gameweek_id = gw.gameweek_id
    WHERE er.season = '%s' AND gw.gameweek_number <= %d
    ORDER BY gw.gameweek_number, er.team_abbreviation
  ",
      season,
      gameweek_number
    )
  )

  current <- history %>%
    group_by(team_id) %>%
    filter(gameweek_number == max(gameweek_number)) %>%
    ungroup()

  list(history = history, current = current)
}

load_db_data <- function(model_version_id, season, gameweek_number) {
  con <- get_db_conn()
  on.exit(dbDisconnect(con))

  # Multiple runs can share a gameweek_number (one per game day within that
  # calendar week) -- take the latest one, i.e. the state as of that
  # gameweek's most recently processed game day.
  run <- dbGetQuery(
    con,
    sprintf(
      "
    SELECT sr.run_id, sr.run_at, sr.season, sr.n_sims, sr.games_played, sr.games_remaining,
           gw.gameweek_number
    FROM simulation_runs sr
    JOIN gameweeks gw ON sr.gameweek_id = gw.gameweek_id
    WHERE sr.model_version_id = %d AND sr.season = '%s' AND gw.gameweek_number = %d
    ORDER BY sr.run_at DESC
    LIMIT 1
  ",
      model_version_id,
      season,
      gameweek_number
    )
  )

  if (nrow(run) == 0) {
    return(list(
      run = run,
      odds = run,
      match_probs = run,
      history = run,
      scoreline_dist = run,
      rank_dist = run,
      cutoff_dist = run
    ))
  }

  odds <- dbGetQuery(
    con,
    sprintf(
      "
    SELECT team_name, team_abbreviation, playoff_pct, avg_pts,
           current_points, games_played
    FROM playoff_odds
    WHERE run_id = %d
    ORDER BY playoff_pct DESC
  ",
      run$run_id
    )
  )

  match_probs <- dbGetQuery(
    con,
    sprintf(
      "
    SELECT home_team_abbr, away_team_abbr, match_date,
           home_win_pct, draw_pct, away_win_pct,
           home_xg, away_xg
    FROM match_probabilities
    WHERE run_id = %d
    ORDER BY match_date, match_id
  ",
      run$run_id
    )
  )

  # Scoped to the currently selected season AND capped at the selected
  # gameweek -- gameweek_number resets to 1 each new season (so without the
  # season filter, different seasons' early gameweeks would overlay at the
  # same x-axis position), and capping at gameweek_number keeps the "scrub
  # back in time" slider from revealing gameweeks after the one selected.
  history <- dbGetQuery(
    con,
    sprintf(
      "
    SELECT
      po.team_abbreviation,
      po.playoff_pct,
      gw.gameweek_number,
      sr.run_at::date AS run_date,
      COALESCE(
        po.games_played > LAG(po.games_played) OVER (
          PARTITION BY po.team_abbreviation ORDER BY sr.run_at
        ),
        po.games_played > 0
      ) AS played_today
    FROM playoff_odds po
    JOIN simulation_runs sr ON po.run_id = sr.run_id
    JOIN gameweeks gw ON sr.gameweek_id = gw.gameweek_id
    WHERE sr.model_version_id = %d AND sr.season = '%s' AND gw.gameweek_number <= %d
    ORDER BY sr.run_at, po.team_abbreviation
  ",
      model_version_id,
      run$season[1],
      gameweek_number
    )
  )

  scoreline_dist <- dbGetQuery(
    con,
    sprintf(
      "
    SELECT home_team_abbr, away_team_abbr, match_date,
           home_goals, away_goals, prob
    FROM scoreline_distributions
    WHERE run_id = %d
    ORDER BY match_date, match_id, home_goals, away_goals
  ",
      run$run_id
    )
  )

  rank_dist <- dbGetQuery(
    con,
    sprintf(
      "
    SELECT team_id, team_abbreviation, rank, count, pct
    FROM rank_distributions
    WHERE run_id = %d
    ORDER BY team_abbreviation, rank
  ",
      run$run_id
    )
  )

  cutoff_dist <- dbGetQuery(
    con,
    sprintf(
      "
    SELECT points, count, pct
    FROM cutoff_distributions
    WHERE run_id = %d
    ORDER BY points
  ",
      run$run_id
    )
  )

  list(
    run = run,
    odds = odds,
    match_probs = match_probs,
    history = history,
    scoreline_dist = scoreline_dist,
    rank_dist = rank_dist,
    cutoff_dist = cutoff_dist
  )
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "USL Super League Playoff Odds",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    open = FALSE,
    selectInput("season", "Season", choices = NULL),
    sliderInput(
      "gameweek",
      "Gameweek",
      min = 1,
      max = 1,
      value = 1,
      step = 1
    ),
    selectInput("model_version", "Model Version", choices = NULL),
    hr(),
    uiOutput("model_version_description"),
    hr(),
    accordion(
      open = FALSE,
      accordion_panel(
        "Latest Run",
        uiOutput("run_info"),
        hr(),
        actionButton(
          "refresh",
          "Refresh",
          icon = icon("rotate"),
          class = "btn-secondary w-100"
        )
      ),
      accordion_panel(
        "About the Model",
        div(
          class = "small",
          p(
            "This tool uses a ",
            strong("Monte Carlo simulation"),
            " —
            a technique that runs the remaining season thousands of times
            using probability and randomness — to estimate each team's
            chances of reaching the playoffs."
          ),
          h6("Rating Teams", class = "mt-3 mb-1 fw-bold"),
          p(
            "Each team is rated on offensive and defensive strength based
            on goals scored and conceded per game so far this season.
            A team that scores often and defends well receives a higher
            rating."
          ),
          h6("Predicting Matches", class = "mt-3 mb-1 fw-bold"),
          p(
            "For every remaining match, the model calculates an expected
            goal tally for each side — factoring in the opponent's
            strength and a home-field advantage. Goals are then drawn
            from a ",
            strong("Poisson distribution"),
            ", a statistical
            tool well-suited for rare, independent events like goals in
            soccer."
          ),
          h6("Simulating the Season", class = "mt-3 mb-1 fw-bold"),
          p(
            "This process repeats up to 1,000,000 times, each producing
            a complete final standings table with all tiebreakers
            (points → goal difference → goals scored) applied."
          ),
          h6("Calculating Odds", class = "mt-3 mb-1 fw-bold"),
          p(
            "A team's playoff probability is the share of simulations in
            which they finish in a qualifying position. If a team makes
            the playoffs in 73,000 of 100,000 simulations, their odds
            are ",
            strong("73%"),
            "."
          ),
          h6("Limitations", class = "mt-3 mb-1 fw-bold"),
          p(
            class = "text-muted",
            "The model treats each game independently and relies solely
            on current-season goal data. It does not account for
            injuries, suspensions, recent form, or roster changes."
          )
        )
      )
    )
  ),

  navset_card_underline(
    nav_panel("Playoff Odds", plotOutput("odds_plot", height = "500px")),
    nav_panel("Uncertainty", tableOutput("uncertainty_table")),
    nav_panel(
      "Match Probabilities",
      fluidRow(
        column(
          4,
          selectInput(
            "match_probs_team",
            "Filter by Team",
            choices = c("All Teams" = ""),
            selected = ""
          )
        )
      ),
      tableOutput("match_probs_table")
    ),
    nav_panel("Historical Trends", plotOutput("trends_plot", height = "500px")),
    nav_panel(
      "Score Matrix",
      fluidRow(
        column(4, selectInput("scoreline_match", "Match", choices = NULL))
      ),
      plotOutput("scoreline_plot", height = "450px")
    ),
    nav_panel(
      "Rankings Distribution",
      plotOutput("rank_dist_plot", height = "500px")
    ),
    nav_panel("Playoff Line", plotOutput("cutoff_dist_plot", height = "500px")),
    nav_panel(
      "Team Ratings",
      plotOutput("elo_trends_plot", height = "500px"),
      hr(),
      tableOutput("elo_leaderboard_table")
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  model_versions <- get_model_versions()
  data <- reactiveVal(NULL)

  observe({
    req(nrow(model_versions) > 0)
    # Default to the highest model_version_id (get_model_versions() already
    # filters to is_active and orders ascending, so this is simply the most
    # advanced active preset -- e.g. v5.0 Combined today) rather than trusting
    # the DB's is_default flag, which requires manual upkeep every time a new
    # preset is added and would otherwise silently keep pointing at a stale
    # older version.
    default_idx <- nrow(model_versions)
    default_id <- model_versions$model_version_id[default_idx]
    updateSelectInput(
      session,
      "model_version",
      choices = setNames(model_versions$model_version_id, model_versions$label),
      selected = default_id
    )
  })

  # Cascade: model_version -> season choices -> gameweek slider bounds.
  # Data loading itself is handled separately below by a plain observe() so
  # it reacts to any of the three inputs changing, not just the last one in
  # this chain (see comment there for why that distinction matters).
  observeEvent(input$model_version, {
    req(input$model_version)
    seasons <- get_seasons_for_version(as.integer(input$model_version))
    req(nrow(seasons) > 0)
    # seasons is ordered most-recently-active first, so this defaults to
    # whichever season is actually current.
    updateSelectInput(
      session,
      "season",
      choices = seasons$season,
      selected = seasons$season[1]
    )
  })

  # Depends on BOTH model_version and season, not just season -- different
  # model versions can have different gameweek coverage for the same season
  # (e.g. a preset activated partway through the season has fewer gameweeks
  # of history than one that's been running since day one), so switching
  # model_version while the season string happens to stay the same still
  # needs to re-check the gameweek bounds, not just when season itself changes.
  observeEvent(list(input$model_version, input$season), {
    req(input$model_version, input$season)
    max_gw <- get_max_gameweek(as.integer(input$model_version), input$season)
    req(!is.na(max_gw))
    updateSliderInput(
      session,
      "gameweek",
      min = 1,
      max = max_gw,
      value = max_gw
    )
  })

  # Plain observe(), not observeEvent() -- reacts to a change in ANY of the
  # three inputs (not just the last one in the cascade), so switching
  # model_version reloads data immediately even in the edge case above where
  # season/gameweek don't end up changing.
  observe({
    req(input$model_version, input$season, input$gameweek)
    data(load_db_data(
      as.integer(input$model_version),
      input$season,
      input$gameweek
    ))
  })

  observeEvent(input$refresh, {
    req(input$model_version, input$season, input$gameweek)
    data(load_db_data(
      as.integer(input$model_version),
      input$season,
      input$gameweek
    ))
  })

  # Elo has no model_version dimension -- loaded independently of the
  # model_version-driven cascade above, keyed only on season/gameweek.
  elo_data <- reactiveVal(NULL)

  observe({
    req(input$season, input$gameweek)
    elo_data(get_elo_data(input$season, input$gameweek))
  })

  observeEvent(input$refresh, {
    req(input$season, input$gameweek)
    elo_data(get_elo_data(input$season, input$gameweek))
  })

  output$elo_trends_plot <- renderPlot({
    d <- elo_data()
    req(nrow(d$history) > 0)
    plot_elo_trends(d$history)
  })

  output$elo_leaderboard_table <- renderTable(
    {
      d <- elo_data()
      req(nrow(d$current) > 0)
      table_elo_leaderboard(d$current)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  output$model_version_description <- renderUI({
    req(input$model_version)
    mv <- model_versions[
      model_versions$model_version_id == as.integer(input$model_version),
    ]
    req(nrow(mv) > 0)

    citations <- tryCatch(
      jsonlite::fromJSON(mv$citations[1]),
      error = function(e) NULL
    )

    citation_tags <- if (
      !is.null(citations) && is.data.frame(citations) && nrow(citations) > 0
    ) {
      tagList(
        p(class = "small text-muted mb-1 mt-2", strong("References:")),
        tags$ul(
          class = "small text-muted ps-3 mb-0",
          lapply(seq_len(nrow(citations)), function(i) {
            tags$li(
              tags$a(
                href = citations$url[i],
                target = "_blank",
                rel = "noopener",
                sprintf("%s (%d)", citations$authors[i], citations$year[i])
              ),
              sprintf(" — %s", citations$title[i])
            )
          })
        )
      )
    } else {
      NULL
    }

    tagList(
      p(class = "small text-muted", mv$description[1]),
      citation_tags
    )
  })

  output$run_info <- renderUI({
    d <- data()
    req(nrow(d$run) > 0)
    r <- d$run
    tagList(
      p(class = "small mb-1", strong("Season: "), r$season),
      p(class = "small mb-1", strong("Gameweek: "), r$gameweek_number),
      p(
        class = "small mb-1",
        strong("Sims: "),
        format(r$n_sims, big.mark = ",")
      ),
      p(
        class = "small mb-1",
        strong("Played: "),
        r$games_played,
        " | Remaining: ",
        r$games_remaining
      ),
      p(
        class = "small text-muted",
        format(as.POSIXct(r$run_at), "%b %d %Y %H:%M")
      )
    )
  })

  output$odds_plot <- renderPlot({
    d <- data()
    req(nrow(d$odds) > 0)
    plot_playoff_odds(d$odds, d$run)
  })

  output$uncertainty_table <- renderTable(
    {
      d <- data()
      req(nrow(d$odds) > 0, nrow(d$run) > 0)
      table_uncertainty(d$odds, d$run$n_sims)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  observeEvent(data(), {
    d <- data()
    if (nrow(d$match_probs) > 0) {
      teams <- sort(unique(c(
        d$match_probs$home_team_abbr,
        d$match_probs$away_team_abbr
      )))
      updateSelectInput(
        session,
        "match_probs_team",
        choices = c("All Teams" = "", teams)
      )
    }
  })

  output$match_probs_table <- renderTable(
    {
      d <- data()
      req(nrow(d$match_probs) > 0)
      mp <- d$match_probs
      if (nzchar(input$match_probs_team)) {
        mp <- mp %>%
          filter(
            home_team_abbr == input$match_probs_team |
              away_team_abbr == input$match_probs_team
          )
      }
      table_match_probs(mp)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  observeEvent(data(), {
    d <- data()
    if (nrow(d$scoreline_dist) > 0) {
      matches <- d$scoreline_dist %>%
        distinct(home_team_abbr, away_team_abbr, match_date) %>%
        mutate(
          label = sprintf(
            "%s  %s vs %s",
            format(as.Date(match_date), "%b %d"),
            home_team_abbr,
            away_team_abbr
          ),
          value = paste(home_team_abbr, away_team_abbr, match_date, sep = "||")
        ) %>%
        arrange(match_date)
      updateSelectInput(
        session,
        "scoreline_match",
        choices = setNames(matches$value, matches$label)
      )
    }
  })

  output$scoreline_plot <- renderPlot({
    d <- data()
    req(nrow(d$scoreline_dist) > 0, input$scoreline_match)

    parts <- strsplit(input$scoreline_match, "\\|\\|")[[1]]
    home_abbr <- parts[1]
    away_abbr <- parts[2]
    mdate <- parts[3]

    df <- d$scoreline_dist %>%
      filter(
        home_team_abbr == home_abbr,
        away_team_abbr == away_abbr,
        as.character(as.Date(match_date)) == mdate
      ) %>%
      rename(home_team = home_team_abbr, away_team = away_team_abbr) %>%
      mutate(matchup = paste0(home_team, " vs ", away_team))

    req(nrow(df) > 0)

    plot_scoreline_distributions(df, ncol = 1)
  })

  output$rank_dist_plot <- renderPlot({
    d <- data()
    req(nrow(d$rank_dist) > 0)
    plot_rank_distributions(d$rank_dist)
  })

  output$cutoff_dist_plot <- renderPlot({
    d <- data()
    req(nrow(d$cutoff_dist) > 0)
    plot_cutoff_distribution(d$cutoff_dist, d$odds)
  })

  output$trends_plot <- renderPlot({
    d <- data()
    req(nrow(d$history) > 0)
    plot_trends(d$history)
  })
}

shinyApp(ui, server)
