source("functions.R")
library(tictoc)
library(gt)
library(ggridges)

# ── Setup ──────────────────────────────────────────────────────────────────────
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

schedule <- get_schedule()

# ── Run Analysis ───────────────────────────────────────────────────────────────
gc()
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

# Example usage:
# match_probs <- get_match_probabilities(
#   remaining_games,
#   team_strengths,
#   teams,
#   n_sims = 50000
# )
#
# ftl_schedule <- get_team_path_to_playoffs(
#   "FTL",
#   remaining_games,
#   team_strengths,
#   teams,
#   n_sims = 50000
# )

# ── Visualization ──────────────────────────────────────────────────────────────
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

# ── Chaos Metrics ──────────────────────────────────────────────────────────────
bubble_teams <- teams %>%
  filter(team_abbreviation %in% c("DAL", "DC", "SPK", 'CAR')) %>%
  select(team_id, team_abbreviation)

sim_wide <- playoff_results_with_ranks %>%
  filter(team %in% bubble_teams$team_id) %>%
  pivot_wider(
    id_cols = sim_id,
    names_from = team,
    values_from = c(points, rank),
    names_sep = "_"
  )

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
#     "The TB Miracle (TB overcomes odds to take 4th)"
#   ),
#   Frequency = c(
#     sim_wide %>% filter(pts_4 - pts_5 <= 1) %>% nrow(),
#     sim_wide %>% filter(pts_4 == pts_5 & pts_5 == pts_6) %>% nrow(),
#     sim_wide %>% filter(.data[[paste0("rank_", tb_team_id)]] == 4) %>% nrow()
#   )
# ) %>%
#   mutate(
#     Probability = paste0(round((Frequency / total_sims) * 100, 2), "%"),
#     Frequency = format(Frequency, big.mark = ",")
#   )
# print("--- THE CHAOS TIER: PROBABILITY TABLE ---")
# print(chaos_metrics)

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

# ── Scoreline distributions ────────────────────────────────────────────────────
# scoreline_dist <- get_scoreline_distributions(
#   remaining_games,
#   team_strengths_complete,
#   teams,
#   n_sims = 50000
# )
# plot_scoreline_distributions(scoreline_dist)
