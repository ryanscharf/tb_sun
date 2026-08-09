-- ═══════════════════════════════════════════════════════════════════════════
-- USL Super League playoff-odds model -- canonical schema
-- ═══════════════════════════════════════════════════════════════════════════
-- Single source of truth for every table this project (functions.R,
-- playoff_runner.R, backfill_runner.R, app.R) reads or writes. Reconstructed
-- directly from a live schema dump (information_schema.columns,
-- table_constraints, pg_constraint, pg_indexes against the actual DB) --
-- not guessed from code usage.
--
-- Deliberately excludes tables belonging to other processes that happen to
-- share this database: game_notes_documents, roster, roster_events,
-- shop_known_products, survey_info.
--
-- Safe to run twice: every CREATE TABLE uses IF NOT EXISTS and every column/
-- constraint/index that might already exist on a partially-set-up DB uses
-- ADD COLUMN IF NOT EXISTS / a guarded DO block. Works both as a from-scratch
-- rebuild (DB blown away) and as an idempotent "make sure everything matches"
-- pass against the current live DB.
--
-- Supersedes (and replaces -- do not keep running) the old incremental
-- migration files: model_versions_migration.sql, season_scoping_migration.sql,
-- elo_ratings_migration.sql. Operational one-off scripts (wipe_calculated_data.sql,
-- cleanup_duplicate_runs.sql, cleanup_stale_league_param_runs.sql) are a
-- different category -- data maintenance, not schema -- and are unaffected.
-- ═══════════════════════════════════════════════════════════════════════════

-- ── model_versions ────────────────────────────────────────────────────────────
-- Curated Monte Carlo model presets (v1.0 Baseline .. v5.0 Combined). No
-- foreign-key dependencies -- created first.
CREATE TABLE IF NOT EXISTS model_versions (
  model_version_id SERIAL PRIMARY KEY,
  version           VARCHAR(20) NOT NULL,
  description       TEXT,
  created_at        TIMESTAMPTZ DEFAULT NOW(),
  label             TEXT,
  feature_flags     JSONB NOT NULL DEFAULT '{}'::jsonb,
  is_active         BOOLEAN NOT NULL DEFAULT FALSE,
  is_default        BOOLEAN NOT NULL DEFAULT FALSE,
  citations         JSONB NOT NULL DEFAULT '[]'::jsonb
);

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'model_versions_version_key') THEN
    ALTER TABLE model_versions ADD CONSTRAINT model_versions_version_key UNIQUE (version);
  END IF;
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS model_versions_one_default
  ON model_versions (is_default)
  WHERE is_default;

-- ── gameweeks ────────────────────────────────────────────────────────────────
-- Season-scoped; gameweek_number resets to 1 each new season rather than
-- counting up forever across all of them.
CREATE TABLE IF NOT EXISTS gameweeks (
  gameweek_id     SERIAL PRIMARY KEY,
  gameweek_number INTEGER NOT NULL,
  start_date      DATE NOT NULL,
  end_date        DATE NOT NULL,
  season          TEXT NOT NULL
);

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint WHERE conname = 'gameweeks_season_gameweek_number_key'
  ) THEN
    ALTER TABLE gameweeks
      ADD CONSTRAINT gameweeks_season_gameweek_number_key UNIQUE (season, gameweek_number);
  END IF;
END $$;

-- ── schedule ─────────────────────────────────────────────────────────────────
-- Synced from FotMob by get_schedule() in functions.R -- only meaningful for
-- whichever season is currently underway.
CREATE TABLE IF NOT EXISTS schedule (
  schedule_id     SERIAL PRIMARY KEY,
  season          TEXT NOT NULL,
  home_team       TEXT NOT NULL,
  away_team       TEXT NOT NULL,
  is_completed    BOOLEAN NOT NULL DEFAULT FALSE,
  is_rescheduled  BOOLEAN NOT NULL DEFAULT FALSE,
  date_utc        TIMESTAMPTZ NOT NULL,
  updated_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'uq_schedule_match') THEN
    ALTER TABLE schedule
      ADD CONSTRAINT uq_schedule_match UNIQUE (season, home_team, away_team, date_utc);
  END IF;
END $$;

-- ── simulation_runs ──────────────────────────────────────────────────────────
-- One row per (season, game-day, model_version_id) combination -- the
-- runner writes at the end of each game day, not once per gameweek.
CREATE TABLE IF NOT EXISTS simulation_runs (
  run_id            SERIAL PRIMARY KEY,
  run_at            TIMESTAMPTZ DEFAULT NOW(),
  n_sims            INTEGER,
  games_played      INTEGER,
  games_remaining   INTEGER,
  gameweek_id       INTEGER REFERENCES gameweeks(gameweek_id),
  model_version_id  INTEGER REFERENCES model_versions(model_version_id),
  season            TEXT NOT NULL,
  -- Nullable: home_advantage is always used (0.3 default or fitted), but
  -- rho only applies to Dixon-Coles presets (v3.0+) -- NULL means the
  -- preset didn't use the tau correction, not that fitting failed.
  home_advantage    NUMERIC(6,4),
  rho               NUMERIC(6,4)
);

ALTER TABLE simulation_runs ADD COLUMN IF NOT EXISTS home_advantage NUMERIC(6,4);
ALTER TABLE simulation_runs ADD COLUMN IF NOT EXISTS rho NUMERIC(6,4);

-- ── playoff_odds ─────────────────────────────────────────────────────────────
CREATE TABLE IF NOT EXISTS playoff_odds (
  run_id              INTEGER NOT NULL REFERENCES simulation_runs(run_id) ON DELETE CASCADE,
  team_id             VARCHAR(20) NOT NULL,
  team_name           VARCHAR(100),
  team_abbreviation   VARCHAR(10),
  playoff_pct         NUMERIC(5,2),
  avg_pts             NUMERIC(6,2),
  current_points      INTEGER,
  games_played        INTEGER,
  gameweek_id         INTEGER,
  PRIMARY KEY (run_id, team_id)
);

CREATE INDEX IF NOT EXISTS idx_playoff_odds_pct ON playoff_odds (playoff_pct);

-- ── match_probabilities ──────────────────────────────────────────────────────
CREATE TABLE IF NOT EXISTS match_probabilities (
  run_id            INTEGER NOT NULL REFERENCES simulation_runs(run_id) ON DELETE CASCADE,
  match_id          INTEGER NOT NULL,
  home_team_abbr    VARCHAR(10),
  away_team_abbr    VARCHAR(10),
  match_date        DATE,
  home_xg           NUMERIC(5,3),
  away_xg           NUMERIC(5,3),
  home_win_pct      NUMERIC(5,2),
  draw_pct          NUMERIC(5,2),
  away_win_pct      NUMERIC(5,2),
  avg_home_goals    NUMERIC(5,3),
  avg_away_goals    NUMERIC(5,3),
  gameweek_id       INTEGER REFERENCES gameweeks(gameweek_id),
  PRIMARY KEY (run_id, match_id)
);

-- ── scoreline_distributions ──────────────────────────────────────────────────
CREATE TABLE IF NOT EXISTS scoreline_distributions (
  run_id            INTEGER NOT NULL REFERENCES simulation_runs(run_id) ON DELETE CASCADE,
  gameweek_id       INTEGER REFERENCES gameweeks(gameweek_id),
  match_id          INTEGER NOT NULL,
  home_team_abbr    VARCHAR(10),
  away_team_abbr    VARCHAR(10),
  match_date        DATE,
  home_goals        INTEGER NOT NULL,
  away_goals        INTEGER NOT NULL,
  scoreline         VARCHAR(5),
  prob              NUMERIC(6,4) NOT NULL,
  PRIMARY KEY (run_id, match_id, home_goals, away_goals)
);

-- ── rank_distributions ───────────────────────────────────────────────────────
CREATE TABLE IF NOT EXISTS rank_distributions (
  rank_dist_id      SERIAL PRIMARY KEY,
  run_id            INTEGER NOT NULL REFERENCES simulation_runs(run_id),
  gameweek_id       INTEGER NOT NULL REFERENCES gameweeks(gameweek_id),
  team_id           VARCHAR NOT NULL,
  team_abbreviation VARCHAR NOT NULL,
  rank              INTEGER NOT NULL,
  count             INTEGER NOT NULL,
  pct               NUMERIC NOT NULL
);

-- ── cutoff_distributions ─────────────────────────────────────────────────────
CREATE TABLE IF NOT EXISTS cutoff_distributions (
  cutoff_dist_id    SERIAL PRIMARY KEY,
  run_id            INTEGER NOT NULL REFERENCES simulation_runs(run_id),
  gameweek_id       INTEGER NOT NULL REFERENCES gameweeks(gameweek_id),
  points            INTEGER NOT NULL,
  count             INTEGER NOT NULL,
  pct               NUMERIC NOT NULL
);

-- ── elo_ratings ──────────────────────────────────────────────────────────────
-- Standalone Elo tracker -- not tied to model_versions/simulation_runs, no
-- presets to multiply out. One row per team per gameweek.
CREATE TABLE IF NOT EXISTS elo_ratings (
  season            TEXT NOT NULL,
  gameweek_id       INTEGER NOT NULL REFERENCES gameweeks(gameweek_id),
  team_id           VARCHAR(20) NOT NULL,
  elo_rating        NUMERIC(7,2) NOT NULL,
  games_played      INTEGER NOT NULL,
  computed_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (season, gameweek_id, team_id)
);

-- Patches the live table up to the shape write_elo_snapshot() (functions.R)
-- actually inserts into -- team_name/team_abbreviation were added to the
-- write path after the table was first created and were never backfilled
-- onto the live schema (confirmed via live dump: only 6 of 8 columns exist).
ALTER TABLE elo_ratings ADD COLUMN IF NOT EXISTS team_name TEXT;
ALTER TABLE elo_ratings ADD COLUMN IF NOT EXISTS team_abbreviation TEXT;

-- ═══════════════════════════════════════════════════════════════════════════
-- Seed data: curated model_versions presets
-- ═══════════════════════════════════════════════════════════════════════════
-- Each preset cites only the papers for techniques it actually implements
-- (additive across presets -- v5.0 accumulates all of them). Maher (1982) is
-- the origin of the multiplicative attack/defense Poisson model every preset
-- uses underneath its own additions; Dixon & Coles (1997) extends it with
-- time-decay + the tau correction; Efron & Morris (1977) is the empirical-
-- Bayes shrinkage citation; Mead/O'Hare/McMenemy (2023) is the xG citation.
INSERT INTO model_versions (version, label, description, feature_flags, is_active, is_default, citations)
VALUES
  (
    '1.0',
    'v1.0 Baseline',
    'Season-1 model: raw per-game goal averages (no time weighting or '
    'opponent adjustment), flat +0.3 home-goal advantage, independent '
    'Poisson match simulation, points/GD/GS/random tiebreak.',
    '{}'::jsonb,
    TRUE,
    TRUE,
    '[
      {
        "authors": "Maher, M.J.",
        "year": 1982,
        "title": "Modelling association football scores",
        "venue": "Statistica Neerlandica, 36(3), 109-118",
        "url": "https://doi.org/10.1111/j.1467-9574.1982.tb00782.x"
      }
    ]'::jsonb
  ),
  (
    '2.0',
    'v2.0 Time-Decay',
    'Adds exponential time-weighting (Dixon & Coles 1997) so recent '
    'results count more than early-season ones when fitting attack/'
    'defense strength.',
    '{"time_decay_xi": 0.0021}'::jsonb,
    FALSE,
    FALSE,
    '[
      {
        "authors": "Maher, M.J.",
        "year": 1982,
        "title": "Modelling association football scores",
        "venue": "Statistica Neerlandica, 36(3), 109-118",
        "url": "https://doi.org/10.1111/j.1467-9574.1982.tb00782.x"
      },
      {
        "authors": "Dixon, M.J. & Coles, S.G.",
        "year": 1997,
        "title": "Modelling Association Football Scores and Inefficiencies in the Football Betting Market",
        "venue": "Journal of the Royal Statistical Society: Series C (Applied Statistics), 46(2), 265-280",
        "url": "https://doi.org/10.1111/1467-9876.00065"
      }
    ]'::jsonb
  ),
  (
    '3.0',
    'v3.0 + Dixon-Coles',
    'Adds the Dixon-Coles low-score correlation correction (tau) so '
    '0-0/1-0/0-1/1-1 draw/scoreline probabilities aren''t systematically '
    'underestimated by independent Poisson.',
    '{"time_decay_xi": 0.0021, "dixon_coles_tau": true}'::jsonb,
    FALSE,
    FALSE,
    '[
      {
        "authors": "Maher, M.J.",
        "year": 1982,
        "title": "Modelling association football scores",
        "venue": "Statistica Neerlandica, 36(3), 109-118",
        "url": "https://doi.org/10.1111/j.1467-9574.1982.tb00782.x"
      },
      {
        "authors": "Dixon, M.J. & Coles, S.G.",
        "year": 1997,
        "title": "Modelling Association Football Scores and Inefficiencies in the Football Betting Market",
        "venue": "Journal of the Royal Statistical Society: Series C (Applied Statistics), 46(2), 265-280",
        "url": "https://doi.org/10.1111/1467-9876.00065"
      }
    ]'::jsonb
  ),
  (
    '4.0',
    'v4.0 + Shrinkage',
    'Adds empirical-Bayes shrinkage of each team''s attack/defense rating '
    'toward the league-average prior, weighted by games played -- smooths '
    'small-sample swings early in a 9-team season.',
    '{"time_decay_xi": 0.0021, "dixon_coles_tau": true, "shrinkage_k": 7}'::jsonb,
    FALSE,
    FALSE,
    '[
      {
        "authors": "Maher, M.J.",
        "year": 1982,
        "title": "Modelling association football scores",
        "venue": "Statistica Neerlandica, 36(3), 109-118",
        "url": "https://doi.org/10.1111/j.1467-9574.1982.tb00782.x"
      },
      {
        "authors": "Dixon, M.J. & Coles, S.G.",
        "year": 1997,
        "title": "Modelling Association Football Scores and Inefficiencies in the Football Betting Market",
        "venue": "Journal of the Royal Statistical Society: Series C (Applied Statistics), 46(2), 265-280",
        "url": "https://doi.org/10.1111/1467-9876.00065"
      },
      {
        "authors": "Efron, B. & Morris, C.",
        "year": 1977,
        "title": "Stein''s Paradox in Statistics",
        "venue": "Scientific American, 236(5), 119-127",
        "url": "https://doi.org/10.1038/scientificamerican0577-119"
      }
    ]'::jsonb
  ),
  (
    '5.0',
    'v5.0 Combined',
    'All of the above, plus a data-fitted (rather than hardcoded) home-'
    'advantage parameter and, where ASA match-level xG data is available '
    'for USL Super League, an xG-blended strength rating to reduce '
    'small-sample goal-count noise.',
    '{"time_decay_xi": 0.0021, "dixon_coles_tau": true, "shrinkage_k": 7, "fitted_home_advantage": true, "xg_blend_weight": 0.5}'::jsonb,
    FALSE,
    FALSE,
    '[
      {
        "authors": "Maher, M.J.",
        "year": 1982,
        "title": "Modelling association football scores",
        "venue": "Statistica Neerlandica, 36(3), 109-118",
        "url": "https://doi.org/10.1111/j.1467-9574.1982.tb00782.x"
      },
      {
        "authors": "Dixon, M.J. & Coles, S.G.",
        "year": 1997,
        "title": "Modelling Association Football Scores and Inefficiencies in the Football Betting Market",
        "venue": "Journal of the Royal Statistical Society: Series C (Applied Statistics), 46(2), 265-280",
        "url": "https://doi.org/10.1111/1467-9876.00065"
      },
      {
        "authors": "Efron, B. & Morris, C.",
        "year": 1977,
        "title": "Stein''s Paradox in Statistics",
        "venue": "Scientific American, 236(5), 119-127",
        "url": "https://doi.org/10.1038/scientificamerican0577-119"
      },
      {
        "authors": "Mead, J., O''Hare, A. & McMenemy, P.",
        "year": 2023,
        "title": "Expected goals in football: Improving model performance and demonstrating value",
        "venue": "PLOS ONE, 18(4), e0282295",
        "url": "https://doi.org/10.1371/journal.pone.0282295"
      }
    ]'::jsonb
  )
ON CONFLICT (version) DO UPDATE SET
  label = EXCLUDED.label,
  description = EXCLUDED.description,
  feature_flags = EXCLUDED.feature_flags,
  citations = EXCLUDED.citations;
-- Note: is_active/is_default are intentionally NOT in the ON CONFLICT SET
-- list -- re-running this script must not silently reactivate/deactivate
-- presets you've since changed via the app or manually in the DB.
