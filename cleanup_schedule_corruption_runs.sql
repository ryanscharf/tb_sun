-- Removes simulation_runs (and children) written for season = '2025-26'
-- AFTER the real season had already concluded, using the `schedule` table
-- while it was corrupted by FotMob's rollover to the next season (see
-- restore_schedule_2025_26.R for the root cause and schedule-table fix).
--
-- Signature: the real 2025-26 season legitimately finished at
-- games_played = 126, games_remaining = 0 (run_at = 2026-05-18). Any run
-- after that showing games_remaining > 0 is not a real continuation of the
-- season -- it's calculate_playoff_odds_fast() treating the mislabeled
-- next-season fixtures (56 of them) as "remaining" games for an already-
-- finished season. Confirmed via:
--   SELECT run_id, run_at, model_version_id, games_played, games_remaining
--   FROM simulation_runs WHERE season = '2025-26' ORDER BY run_at DESC;
-- which showed run_ids 1273-1277 (run_at = 2026-08-10) at
-- games_played=126, games_remaining=56 -- 56 matching exactly the
-- mislabeled-row count, immediately after the legitimate 2026-05-18
-- games_remaining=0 finish.

CREATE TEMP TABLE corrupted_run_ids AS
SELECT run_id
FROM simulation_runs
WHERE season = '2025-26'
  AND run_at > '2026-05-18'::timestamptz
  AND games_remaining > 0;

-- Sanity check before the deletes run -- eyeball this before proceeding.
-- Expect exactly the 5 runs identified above (run_ids 1273-1277).
SELECT * FROM corrupted_run_ids ORDER BY run_id;

DELETE FROM playoff_odds WHERE run_id IN (SELECT run_id FROM corrupted_run_ids);
DELETE FROM match_probabilities WHERE run_id IN (SELECT run_id FROM corrupted_run_ids);
DELETE FROM scoreline_distributions WHERE run_id IN (SELECT run_id FROM corrupted_run_ids);
DELETE FROM rank_distributions WHERE run_id IN (SELECT run_id FROM corrupted_run_ids);
DELETE FROM cutoff_distributions WHERE run_id IN (SELECT run_id FROM corrupted_run_ids);
DELETE FROM simulation_runs WHERE run_id IN (SELECT run_id FROM corrupted_run_ids);

DROP TABLE corrupted_run_ids;
