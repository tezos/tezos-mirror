CREATE TABLE l1_latest_level_with_l2_level (
  l2_level PRIMARY KEY ON CONFLICT REPLACE,
  l1_level INT NOT NULL UNIQUE ON CONFLICT ABORT
);

INSERT INTO l1_latest_level_with_l2_level (l2_level, l1_level)
SELECT context_hashes.id, level
  FROM l1_latest_level
       CROSS JOIN context_hashes
 ORDER BY id DESC limit 1 ;

DROP TABLE l1_latest_level;
