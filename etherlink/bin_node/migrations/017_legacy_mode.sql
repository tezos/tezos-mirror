CREATE TABLE block_storage_mode (
  legacy INTEGER NOT NULL,
  CONSTRAINT valid_legacy CHECK (legacy IN (0, 1))
);

-- For existing data directories, we determine whether they were using the
-- legacy block storage or the experimental features by comparing the number of
-- blocks and context hashes. Same number means the new block storage was used.
-- Different numbers means legacy.
-- 
-- On new data directories, the result will be 0 in base cases, meaning the
-- legacy mode will be disabled.
INSERT INTO block_storage_mode (legacy)
SELECT
  CASE
    WHEN (blocks_count = context_hashes_count) THEN 0
    WHEN (blocks_count = 0) THEN 1
    ELSE 2 -- this will raise an error, as 2 is an invalid value, preventing
           -- the node to migrate
  END
FROM (
  SELECT
    (SELECT COUNT(*) FROM blocks) AS blocks_count,
    (SELECT COUNT(*) FROM context_hashes WHERE id >= 0) AS context_hashes_count
);
