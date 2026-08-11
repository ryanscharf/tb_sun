-- Removes backfilled simulation_runs (and children) for presets that use
-- pooled league_params (dixon_coles_tau and/or fitted_home_advantage --
-- currently v3.0, v4.0, v5.0 Combined), scoped to season = '2025-26' only.
--
-- fit_pooled_league_params() was changed so that once a qualified PRIOR
-- season already anchors the pool, the current (in-progress) season no
-- longer needs to independently clear the full ~54-game sufficiency gate
-- before contributing -- previously it sat frozen on 2024-25's numbers
-- alone for roughly the first half of the season. This only changes
-- behavior for a season that HAS a qualified prior season available, which
-- as of this cleanup means 2025-26 (2024-25 was the first-ever backfilled
-- season, has no prior season to lean on, and is unaffected -- its rows are
-- deliberately left alone).
--
-- v1.0 Baseline and v2.0 Time-Decay don't use league_params at all and are
-- untouched by this either way.

CREATE TEMP TABLE stale_run_ids AS
SELECT sr.run_id
FROM simulation_runs sr
JOIN model_versions mv ON sr.model_version_id = mv.model_version_id
WHERE sr.season = '2025-26'
  AND (
    (mv.feature_flags->>'dixon_coles_tau')::boolean IS TRUE
    OR (mv.feature_flags->>'fitted_home_advantage')::boolean IS TRUE
  );

-- Sanity check before the deletes run -- eyeball this count.
SELECT COUNT(*) AS stale_runs_to_remove FROM stale_run_ids;

DELETE FROM playoff_odds WHERE run_id IN (SELECT run_id FROM stale_run_ids);
DELETE FROM match_probabilities WHERE run_id IN (SELECT run_id FROM stale_run_ids);
DELETE FROM scoreline_distributions WHERE run_id IN (SELECT run_id FROM stale_run_ids);
DELETE FROM rank_distributions WHERE run_id IN (SELECT run_id FROM stale_run_ids);
DELETE FROM cutoff_distributions WHERE run_id IN (SELECT run_id FROM stale_run_ids);
DELETE FROM simulation_runs WHERE run_id IN (SELECT run_id FROM stale_run_ids);

DROP TABLE stale_run_ids;
