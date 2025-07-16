DELETE FROM sequencer_upgrades
WHERE rowid NOT IN (
    SELECT MIN(rowid)
    FROM sequencer_upgrades
    GROUP BY injected_before, sequencer, pool_address, activation_timestamp
);

-- SQLite treats `NULL` in a particular manner, in that `NULL != NULL`. In
-- order to have a real uniqueness constraint for `applied_before`, we need to
-- replace `NULL` by a dummy value. Considering `applied_before` is a level,
-- then `-1` is a valid dummy value.
CREATE UNIQUE INDEX unapplied_sequencer_upgrade
ON sequencer_upgrades (
  COALESCE(applied_before, -1)
);
