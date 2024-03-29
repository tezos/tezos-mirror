-- We need to drop the previous contents because the contents has not been kept
-- up to date
DELETE FROM kernel_upgrades;

ALTER TABLE kernel_upgrades
RENAME COLUMN applied_before TO injected_before;

ALTER TABLE kernel_upgrades
RENAME COLUMN applied TO applied_before;

-- We add a constraint that we can only have one kernel upgrade not applied at
-- a given time 
CREATE UNIQUE INDEX unapplied_upgrade
ON kernel_upgrades (applied_before)
WHERE applied_before IS NULL;
