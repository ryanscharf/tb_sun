# Load required packages
library(itscalledsoccer)
library(tidyverse)

# Initialize the ASA client
asa_client <- AmericanSoccerAnalysis$new()

teams <- asa_client$get_teams(leagues = 'usls')

# Get USL Super League games for 2024-25 season
games <- asa_client$get_games(
  leagues = "usls",
  seasons = "2024-25"
)

# Filter for completed games only and calculate points
results <- games %>%
  filter(!is.na(home_score) & !is.na(away_score)) %>%
  mutate(
    # Determine points for each team
    home_points = case_when(
      home_score > away_score ~ 3,
      home_score == away_score ~ 1,
      TRUE ~ 0
    ),
    away_points = case_when(
      away_score > home_score ~ 3,
      away_score == home_score ~ 1,
      TRUE ~ 0
    )
  ) %>%
  arrange(date_time_utc)

# Create home team records
home_records <- results %>%
  select(
    gameweek = matchday,
    team = home_team_id,
    points = home_points,
    knockout_game,
    date_time_utc
  )

# Create away team records
away_records <- results %>%
  select(
    gameweek = matchday,
    team = away_team_id,
    points = away_points,
    knockout_game,
    date_time_utc
  )


playoff_teams <- bind_rows(home_records, away_records) %>%
  filter(knockout_game == T) %>%
  distinct(team)

# Combine and calculate cumulative points
points_by_gameweek <- bind_rows(home_records, away_records) %>%
  left_join(teams, by = c('team' = 'team_id')) %>%
  # filter(knockout_game == F) %>%
  arrange(date_time_utc) %>%
  group_by(team) %>%
  mutate(gn = row_number()) %>%
  arrange(team, gn) %>%
  group_by(team) %>%
  mutate(cumulative_points = cumsum(points)) %>%
  ungroup() %>%
  arrange(gameweek, desc(cumulative_points))

playoff_line <- points_by_gameweek %>%
  filter(knockout_game == T) %>%
  summarize(line = min(cumulative_points)) %>%
  pull(line)

points_by_gameweek %>%
  filter(knockout_game == F) %>%
  mutate(made_playoffs = case_when(team %in% playoff_teams$team ~ T, T ~ F)) %>%
  ggplot() +
  geom_line(aes(
    x = gn,
    y = cumulative_points,
    color = team_name,
    linetype = made_playoffs
  )) +
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed")) +
  geom_hline(yintercept = playoff_line) +
  labs(
    title = "USL Super League 2024-25: Cumulative Points by Game Number",
    x = "Game Number",
    y = "Cumulative Points",
    color = "Team",
    linetype = "Made Playoffs"
  ) +
  theme_minimal()


games_2025 <- asa_client$get_games(
  leagues = "usls",
  seasons = "2025-26"
)
res_25 <- games_2025 %>%
  filter(!is.na(home_score) & !is.na(away_score)) %>%
  mutate(
    # Determine points for each team
    home_points = case_when(
      home_score > away_score ~ 3,
      home_score == away_score ~ 1,
      TRUE ~ 0
    ),
    away_points = case_when(
      away_score > home_score ~ 3,
      away_score == home_score ~ 1,
      TRUE ~ 0
    )
  ) %>%
  arrange(date_time_utc)

home_records_25 <- res_25 %>%
  select(
    gameweek = matchday,
    team = home_team_id,
    points = home_points,
    knockout_game,
    date_time_utc
  )

# Create away team records
away_records_25 <- res_25 %>%
  select(
    gameweek = matchday,
    team = away_team_id,
    points = away_points,
    knockout_game,
    date_time_utc
  )

games_2025 <- bind_rows(home_records_25, away_records_25) %>%
  left_join(teams, by = c('team' = 'team_id')) %>%
  filter(team_name == 'Tampa Bay Sun FC') %>%
  arrange(date_time_utc) %>%
  group_by(team_name) %>%
  mutate(gn = row_number()) %>%
  arrange(team, gn) %>%
  group_by(team) %>%
  mutate(cumulative_points = cumsum(points)) %>%
  ungroup() %>%
  arrange(gameweek, desc(cumulative_points))

points_by_gameweek %>%
  filter(knockout_game == F) %>%
  mutate(made_playoffs = case_when(team %in% playoff_teams$team ~ T, T ~ F)) %>%
  ggplot() +
  geom_line(aes(
    x = gn,
    y = cumulative_points,
    color = team_name,
    linetype = made_playoffs
  )) +
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed")) +
  geom_hline(yintercept = playoff_line) +
  labs(
    title = "USL Super League 2024-25: Cumulative Points by Game Number",
    subtitle = '25-26 Sun Season in Red',
    x = "Game Number",
    y = "Cumulative Points",
    color = "Team",
    linetype = "Made Playoffs"
  ) +
  theme_minimal() +
  geom_line(
    data = games_2025,
    aes(x = gn, y = cumulative_points),
    color = 'red'
  )
