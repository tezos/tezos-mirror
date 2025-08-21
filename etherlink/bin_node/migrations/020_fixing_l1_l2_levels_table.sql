CREATE TABLE l1_l2_finalized_levels (
  l1_level INTEGER PRIMARY KEY,
  start_l2_level INTEGER NOT NULL, -- L2 level at start of L1 block
  end_l2_level INTEGER NOT NULL -- L2 level at end of L1 block
);

CREATE INDEX idx_start_l2_level
ON l1_l2_finalized_levels(start_l2_level);

CREATE INDEX idx_end_l2_level
ON l1_l2_finalized_levels(end_l2_level);

WITH RECURSIVE
-- Identify min and max L1 levels
bounds AS (
  SELECT MIN(l1_level) AS min_l1, MAX(l1_level) AS max_l1
  FROM l1_l2_levels_relationships
),
-- Generate all L1 levels between min and max
-- Limit to last 700k blocks (2 weeks on mainnet) to keep migration quick
all_levels(l1_level) AS (
  SELECT MAX(min_l1, max_l1 - 700000) FROM bounds
  UNION ALL
  SELECT l1_level + 1 FROM all_levels
  JOIN bounds
  WHERE l1_level + 1 <= max_l1
),
-- For each L1 level compute the finalized start and end levels, using
-- values from l1_l2_levels_relationships
levels AS (
  SELECT a.l1_level as l1_level,
  MAX(r.finalized_l2_level) OVER (
    ORDER BY a.l1_level
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
  ) as end_l2_level,
  MAX(r.finalized_l2_level) OVER (
    ORDER BY a.l1_level
    ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
  ) as start_l2_level
  FROM all_levels a
  LEFT OUTER JOIN l1_l2_levels_relationships r
  ON r.l1_level = a.l1_level
  ORDER BY a.l1_level
)
-- Put these into the new table
INSERT INTO l1_l2_finalized_levels (l1_level, start_l2_level, end_l2_level)
SELECT
  l1_level,
  (CASE WHEN start_l2_level IS NULL THEN end_l2_level
   -- we don't know what first L1 level contains
   ELSE start_l2_level END),
  end_l2_level
FROM levels
WHERE end_l2_level IS NOT NULL;

ALTER TABLE l1_l2_levels_relationships
DROP COLUMN finalized_l2_level;
