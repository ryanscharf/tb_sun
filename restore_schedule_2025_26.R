# restore_schedule_2025_26.R
#
# get_schedule()'s upsert-then-delete-stale logic (functions.R) purges any
# `schedule` row for the passed-in season that isn't in the freshly-fetched
# FotMob payload. FotMob has since rolled over to the next season's fixtures
# entirely (2025-26 concluded), so every real 2025-26 row got deleted as
# "stale" and the new season's fixtures got inserted under the still-
# configured "2025-26" label in their place.
#
# Fix, in order:
#   1. Relabel the currently-mislabeled next-season rows from "2025-26" to
#      "2026 fall" (real fixtures, just tagged wrong).
#   2. Rebuild the real 2025-26 schedule rows from ASA -- unlike `schedule`,
#      ASA still has the complete historical schedule+results for that
#      season (this is exactly what backfill_runner.R already used, so
#      nothing in simulation_runs/playoff_odds/etc. was ever affected by
#      this -- only the live `schedule` table was).
#
# Run interactively; expects DB_HOST/DB_PORT/DB_NAME/DB_USERNAME/DB_PASSWORD
# in the environment (.env loaded below if present).

suppressPackageStartupMessages({
  library(tidyverse)
  library(itscalledsoccer)
  library(DBI)
  library(RPostgres)
  library(glue)
})

source("functions.R")

if (file.exists(".env")) {
  lines <- readLines(".env")
  lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]
  for (line in lines) {
    kv <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(kv) >= 2) {
      do.call(Sys.setenv, setNames(list(trimws(paste(kv[-1], collapse = "="))), trimws(kv[1])))
    }
  }
}

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

MISLABELED_SEASON <- "2025-26"
CORRECT_NEW_LABEL <- "2026 fall"
REAL_SEASON <- "2025-26"

con <- get_db_conn()

# ── Step 1: relabel the mislabeled next-season rows ────────────────────────────
before <- dbGetQuery(con, glue(
  "SELECT COUNT(*) AS n FROM schedule WHERE season = '{MISLABELED_SEASON}'"
))$n
message(glue("Rows currently tagged '{MISLABELED_SEASON}': {before}"))

relabeled <- dbExecute(con, glue(
  "UPDATE schedule SET season = '{CORRECT_NEW_LABEL}', updated_at = NOW() WHERE season = '{MISLABELED_SEASON}'"
))
message(glue("Relabeled {relabeled} row(s) from '{MISLABELED_SEASON}' to '{CORRECT_NEW_LABEL}'."))

# ── Step 2: rebuild the real 2025-26 schedule from ASA ──────────────────────────
asa_client <- AmericanSoccerAnalysis$new()

asa_games <- suppressMessages(asa_client$get_games(leagues = "usls", season = REAL_SEASON)) %>%
  as_tibble() %>%
  transmute(
    home_team_id, away_team_id,
    is_completed = status == "FullTime",
    # date_time_utc comes back as a plain ISO8601-ish character string, not
    # POSIXct -- assigning it straight through left dbWriteTable() inferring
    # a TEXT staging column, which then failed to cast against schedule's
    # real TIMESTAMPTZ column on insert. A rigid as.POSIXct(format=...) then
    # silently produced NA for rows not matching that exact format (e.g.
    # fractional seconds), which is worse -- it violates the NOT NULL
    # constraint instead of erroring loudly. as_datetime() tolerates format
    # variation and handles the input already being POSIXct too.
    date_utc = lubridate::as_datetime(date_time_utc, tz = "UTC")
  )

message(glue("Fetched {nrow(asa_games)} game(s) for season {REAL_SEASON} from ASA."))

rebuilt_schedule <- asa_games %>%
  left_join(team_name_mapping %>% select(team_id, fotmob_name), by = c("home_team_id" = "team_id")) %>%
  rename(home_team = fotmob_name) %>%
  left_join(team_name_mapping %>% select(team_id, fotmob_name), by = c("away_team_id" = "team_id")) %>%
  rename(away_team = fotmob_name) %>%
  select(home_team, away_team, is_completed, date_utc) %>%
  mutate(season = REAL_SEASON, is_rescheduled = FALSE)

unmapped <- rebuilt_schedule %>% filter(is.na(home_team) | is.na(away_team))
if (nrow(unmapped) > 0) {
  message(glue("WARNING: {nrow(unmapped)} game(s) couldn't be mapped to a fotmob_name -- team_name_mapping may be missing a team. Not writing these rows."))
  print(unmapped)
}

bad_dates <- rebuilt_schedule %>% filter(is.na(date_utc))
if (nrow(bad_dates) > 0) {
  message(glue("WARNING: {nrow(bad_dates)} game(s) had a date_utc that failed to parse -- not writing these rows."))
  print(bad_dates)
}

rebuilt_schedule <- rebuilt_schedule %>%
  filter(!is.na(home_team), !is.na(away_team), !is.na(date_utc))

dbWriteTable(con, "schedule_restore_staging", rebuilt_schedule, temporary = TRUE, overwrite = TRUE, row.names = FALSE)

inserted <- dbExecute(con, "
  INSERT INTO schedule (season, home_team, away_team, is_completed, is_rescheduled, date_utc)
  SELECT season, home_team, away_team, is_completed, is_rescheduled, date_utc FROM schedule_restore_staging
  ON CONFLICT (season, home_team, away_team, date_utc)
  DO UPDATE SET
    is_completed   = EXCLUDED.is_completed,
    is_rescheduled = EXCLUDED.is_rescheduled,
    updated_at     = NOW()
")
message(glue("Wrote/updated {inserted} row(s) into schedule for season {REAL_SEASON}."))

# ── Verify ───────────────────────────────────────────────────────────────────
verify <- dbGetQuery(con, glue("
  SELECT season, COUNT(*) AS n_games, SUM(CASE WHEN is_completed THEN 1 ELSE 0 END) AS n_completed,
         MIN(date_utc) AS earliest, MAX(date_utc) AS latest
  FROM schedule
  WHERE season IN ('{REAL_SEASON}', '{CORRECT_NEW_LABEL}')
  GROUP BY season
"))
print(verify)

dbDisconnect(con)
