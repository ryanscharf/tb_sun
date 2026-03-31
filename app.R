library(shiny)
library(bslib)

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
library(tidyverse)
library(DBI)
library(RPostgres)
library(shadowtext)
library(ggridges)
source("functions.R")

# ── DB ─────────────────────────────────────────────────────────────────────────
get_db_conn <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST", "pg-tn.ryanscharf.com"),
    port = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USERNAME"),
    password = Sys.getenv("DB_PASSWORD")
  )
}

load_db_data <- function() {
  con <- get_db_conn()
  on.exit(dbDisconnect(con))

  run <- dbGetQuery(
    con,
    "
    SELECT sr.run_id, sr.run_at, sr.n_sims, sr.games_played, sr.games_remaining,
           gw.gameweek_number
    FROM simulation_runs sr
    LEFT JOIN gameweeks gw ON sr.gameweek_id = gw.gameweek_id
    ORDER BY sr.run_at DESC
    LIMIT 1
  "
  )

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

  history <- dbGetQuery(
    con,
    "
    SELECT po.team_abbreviation, po.playoff_pct, gw.gameweek_number
    FROM playoff_odds po
    JOIN simulation_runs sr ON po.run_id = sr.run_id
    JOIN gameweeks gw ON sr.gameweek_id = gw.gameweek_id
    ORDER BY gw.gameweek_number
  "
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
    accordion(
      open = FALSE,
      accordion_panel(
        "Latest Run",
        icon = bsicons::bs_icon("clock-history"),
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
        icon = bsicons::bs_icon("info-circle"),
        div(
          class = "small",
          p(
            "This tool uses a ", strong("Monte Carlo simulation"), " —
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
            from a ", strong("Poisson distribution"), ", a statistical
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
            are ", strong("73%"), "."
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
    nav_panel("Match Probabilities", tableOutput("match_probs_table")),
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
    nav_panel("Playoff Line", plotOutput("cutoff_dist_plot", height = "500px"))
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  data <- reactiveVal(load_db_data())

  observeEvent(input$refresh, {
    data(load_db_data())
  })

  output$run_info <- renderUI({
    d <- data()
    req(nrow(d$run) > 0)
    r <- d$run
    tagList(
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

  output$match_probs_table <- renderTable(
    {
      d <- data()
      req(nrow(d$match_probs) > 0)
      table_match_probs(d$match_probs)
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
